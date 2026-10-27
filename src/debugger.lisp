(in-package #:lambda-debugger)

;;;; -- Live Sessions and Outcomes --

(defstruct (outcome (:constructor debugger--make-outcome))
  "The result of one operation, including recovery metadata and all raw values."
  (values nil :type list :read-only t)
  (status ':ok :type (member :ok :aborted) :read-only t)
  (condition-report nil :type (or null string) :read-only t)
  (restart-names nil :type list :read-only t)
  (selected-restart-name nil :type (or null string) :read-only t))

(define-condition session-error (error)
  ((session :initarg :session :reader session-error-session
            :documentation "The session whose live state cannot be accessed.")
   (reason :initarg :reason :reader session-error-reason
           :documentation "Either :EXPIRED or :WRONG-THREAD."))
  (:report (lambda (condition stream)
             (format stream "Debugger session is ~A."
                     (session-error-reason condition))))
  (:documentation "Live debugger state was accessed outside its owner or extent."))

(defclass debug-session ()
  ((condition :initarg :condition :reader debugger--session-condition
              :documentation "The original condition, retained only during suspension.")
   (restarts :initarg :restarts :reader debugger--session-restarts
             :documentation "The selectable live restart objects.")
   (snapshot :initarg :snapshot :reader debugger--session-snapshot
             :documentation "Detached portable diagnostic data.")
   (owner :initform sb-thread:*current-thread* :reader debugger--session-owner
          :documentation "The only thread allowed to access live objects.")
   (live-p :initform t :accessor debugger--session-live-p
           :documentation "Whether the failure still has its dynamic extent."))
  (:documentation "A thread-owned live failure and its portable snapshot."))

(defun debugger--require-live-session (session)
  "Require SESSION's owner thread and dynamic failure extent."
  (unless (eq (debugger--session-owner session) sb-thread:*current-thread*)
    (error 'session-error :session session :reason ':wrong-thread))
  (unless (debugger--session-live-p session)
    (error 'session-error :session session :reason ':expired))
  session)

(defun debug-session-condition (session)
  "Return SESSION's original condition on its owner thread while suspended."
  (debugger--require-live-session session)
  (debugger--session-condition session))

(defun debug-session-restarts (session)
  "Return a copied list of SESSION's live restart objects on its owner thread."
  (debugger--require-live-session session)
  (copy-list (debugger--session-restarts session)))

(defun debugger--copy-data (data)
  "Copy a library-owned portable tree, including its string buffers."
  (typecase data
    (string
     (copy-seq data))
    (cons
     (cons (debugger--copy-data (first data))
           (debugger--copy-data (rest data))))
    (t
     data)))

(defun debug-session-snapshot (session)
  "Return a detached portable snapshot, including after SESSION has expired."
  (debugger--copy-data (debugger--session-snapshot session)))

(defun debugger--make-session (condition restarts &key source operation-kind
                                                     retry-p return-values-p)
  "Describe one failure without putting live objects into its snapshot."
  (let ((identifier (format nil "~36R" (random (ash 1 128) (make-random-state t)))))
    (make-instance
     'debug-session :condition condition :restarts restarts
     :snapshot
     (list :condition-type     (string-downcase (diagnostic--text (type-of condition)))
           :condition-report   (diagnostic--text condition)
           :source             (diagnostic--bounded-string source 16384)
           :operation-kind     operation-kind
           :restarts           (loop for restart in restarts for index from 1
                                     collect (list :id     (format nil "~A/~D" identifier index)
                                                   :report (diagnostic--text restart)))
           :capabilities       (list :invoke-restart t :repair-and-invoke t
                                     :retry-operation retry-p :repair-and-retry retry-p
                                     :return-values return-values-p :abort-operation t)
           :backtrace          (safe-backtrace)
           :condition-metadata (diagnostic--metadata condition)))))


;;;; -- SBCL Hook and Restart Adapters --

(defun call-with-debugger-hook (thunk handler)
  "Call THUNK with both Common Lisp and SBCL debugger hooks bound to HANDLER.

HANDLER receives (condition previous-hook) and must transfer control to resolve
a debugger entry. Both bindings are restored on every exit."
  (let ((*debugger-hook*               handler)
        (sb-ext:*invoke-debugger-hook* handler))
    (funcall thunk)))

(defun debugger--call-with-abort-restart (thunk &key name report function)
  "Call THUNK with a runtime-named SBCL restart for this operation boundary.

This is RESTART-BIND's binding protocol from SBCL src/code/restart.lisp."
  (let* ((restart (sb-kernel:make-restart
                   name function (lambda (stream) (write-string report stream))))
         (sb-kernel:*restart-clusters*
           (cons (list restart) sb-kernel:*restart-clusters*)))
    (funcall thunk)))

(defun debugger--evaluate-source (source)
  "Evaluate exactly one form in the caller's package and return all values as a list."
  (let ((end         (gensym "END-OF-SOURCE"))
        (*read-eval* nil))
    (multiple-value-bind (form position) (read-from-string source nil end)
      (when (eq form end)
        (error 'recovery-error :reason "source contains no form"))
      (unless (eq (read-from-string source nil end :start position) end)
        (error 'recovery-error :reason "source contains trailing forms"))
      (multiple-value-list (eval form)))))


;;;; -- Operation Boundary --

(defun call-with-debugger
    (thunk &key selector (condition-p (constantly t))
           (passthrough-p (constantly nil))
           (restart-p (lambda (restart) (not (null (restart-name restart)))))
           (evaluate-source #'debugger--evaluate-source) failure-handler observer
           (source "") (operation-kind ':lisp) (retry-p t) (return-values-p nil)
           (abort-restart-name 'abort-operation) (abort-report "Abort this operation."))
  "Run THUNK with live failure selection and return one OUTCOME.

SELECTOR receives a DEBUG-SESSION. Return a live restart and optional argument
source, a RECOVERY, or NIL to abort this boundary. Serious conditions accepted by
CONDITION-P and explicit debugger entries are selected while their stack is live.
PASSTHROUGH-P excludes control conditions. Recovery sources execute on the owner
thread; retry reruns THUNK and does not roll back prior effects.

Ordinary recovery failures call FAILURE-HANDLER with the condition and allow
another selection. Failures in selectors, policy predicates, observers, or the
failure handler propagate outside the boundary without recursive debugger entry.
OBSERVER receives an event and detached portable data; its return is ignored.
Events are :CONDITION, :RECOVERY-FAILED, :RETRY, :RETURN, :ABORT, and :COMPLETE."
  (check-type source string)
  (check-type operation-kind keyword)
  (check-type abort-restart-name (and symbol (not null)))
  (let ((escape-tag         (gensym "DEBUGGER-FAILURE"))
        (retry-tag          (gensym "DEBUGGER-RETRY"))
        (return-tag         (gensym "DEBUGGER-RETURN"))
        (action-failure-tag (gensym "DEBUGGER-ACTION-FAILURE"))
        (handling-p         nil)
        (raw-values         nil)
        (status             ':ok)
        (condition-report   nil)
        (restart-names      nil)
        (selected-name      nil)
        (active-session     nil)
        (outer-hook         (or sb-ext:*invoke-debugger-hook* *debugger-hook*)))
    (labels ((escape (condition)
               "Transfer a callback or control failure outside our hook bindings."
               (throw escape-tag condition))

             (callback (function &rest arguments)
               "Run an integration callback with a nonrecursive failure boundary."
               (call-with-debugger-hook
                (lambda ()
                  (handler-bind ((serious-condition #'escape))
                    (apply function arguments)))
                (lambda (condition hook)
                  (declare (ignore hook))
                  (escape condition))))

             (notify (event &optional extra)
               "Publish only detached snapshot and outcome metadata."
               (when observer
                 (callback observer event
                           (append (when active-session
                                     (debug-session-snapshot active-session))
                                   (debugger--copy-data extra)))))

             (abort-operation ()
               "Abort exactly this operation, rather than finding an outer restart."
               (setf status ':aborted raw-values nil
                     selected-name (symbol-name abort-restart-name))
               (notify ':abort)
               (throw return-tag ':return))

             (evaluate (text)
               "Apply caller evaluation policy and require an argument value list."
               (let ((values (funcall evaluate-source text)))
                (unless (handler-case
                            (and (listp values)
                                 (or (null values) (list-length values)))
                          (type-error ()
                            nil))
                  (error 'recovery-error :reason "source evaluator must return a finite proper value list"))
                values))

             (live-target (session identifier)
               "Resolve an opaque identifier only in the current failure session."
               (loop for description in (getf (debugger--session-snapshot session) :restarts)
                     for restart in (debugger--session-restarts session)
                     when (equal identifier (getf description :id))
                       return restart))

             (invoke (restart arguments)
               "Require a selectable, still-live restart before invoking it."
               (unless (and (member restart (debugger--session-restarts active-session))
                            (member restart (compute-restarts)))
                 (error 'recovery-error :reason "target restart is no longer live"))
               (setf selected-name (let ((name (restart-name restart)))
                                     (and name (symbol-name name))))
               (apply #'invoke-restart restart (if arguments (evaluate arguments) nil)))

             (execute (proposal)
               "Validate and execute one portable recovery on this thread."
               (validate-recovery (debugger--session-snapshot active-session) proposal)
               (let ((kind (recovery-kind proposal)))
                 (setf selected-name (symbol-name kind))
                (when (member kind '(:invoke-restart :repair-and-invoke))
                  (unless (member (live-target active-session (recovery-restart-id proposal))
                                  (compute-restarts))
                    (recovery--reject proposal "target restart is no longer live")))
                (when (recovery-preparation-source proposal)
                  (evaluate (recovery-preparation-source proposal)))
                (case kind
                  ((:invoke-restart :repair-and-invoke)
                   (invoke (live-target active-session (recovery-restart-id proposal))
                           (recovery-argument-source proposal)))
                  ((:retry-operation :repair-and-retry)
                   (notify ':retry)
                   (throw retry-tag ':retry))
                  (:return-values
                   (setf raw-values (evaluate (recovery-return-source proposal)))
                   (notify ':return (list :value-count (length raw-values)))
                   (throw return-tag ':return))
                  (:abort-operation
                   (abort-operation)))))

             (action-failure (condition)
               "Pass control conditions through; return other failures to selection."
               (if (callback passthrough-p condition)
                   (escape condition)
                   (throw action-failure-tag condition)))

             (handle-condition (condition)
               "Select and execute recovery without unwinding the failing stack."
               (when (and (not handling-p)
                          (not (callback passthrough-p condition))
                          (callback condition-p condition))
                (let* ((restarts
                          (callback
                           (lambda ()
                              (loop for restart in (compute-restarts condition)
                                    when (callback restart-p restart)
                                      collect restart
                                      and count t into selected-count
                                    when (eql selected-count 256)
                                      do (loop-finish)))))
                       (session (callback #'debugger--make-session condition restarts
                                          :source source :operation-kind operation-kind
                                          :retry-p retry-p :return-values-p return-values-p)))
                   (setf condition-report (getf (debugger--session-snapshot session) :condition-report)
                         restart-names (mapcar (lambda (restart)
                                                 (let ((name (restart-name restart)))
                                                   (and name (symbol-name name))))
                                               restarts))
                  (let ((previous-session active-session))
                    (setf handling-p t active-session session)
                    (unwind-protect
                         (progn
                           (notify ':condition)
                           (loop
                             (multiple-value-bind (selected arguments)
                                 (when selector (callback selector session))
                               (unless selected (abort-operation))
                               (let ((failure
                                       (catch action-failure-tag
                                         (call-with-debugger-hook
                                          (lambda ()
                                            (handler-bind ((serious-condition #'action-failure))
                                              (if (typep selected 'recovery)
                                                  (execute selected)
                                                  (invoke selected arguments))))
                                          (lambda (condition hook)
                                            (declare (ignore hook))
                                            (action-failure condition)))
                                         nil)))
                                 (unless failure (return))
                                 (setf condition-report (diagnostic--text failure)
                                       selected-name nil)
                                 (notify ':recovery-failed
                                         (list :recovery-condition-report condition-report))
                                 (when failure-handler (callback failure-handler failure))))))
                     (setf (debugger--session-live-p session) nil
                           (slot-value session 'condition) nil
                           (slot-value session 'restarts) nil
                           handling-p nil active-session previous-session))))))

             (debugger-entry (condition hook)
               "Capture explicit debugger entry or forward an excluded condition."
               (when handling-p (escape condition))
               (if (or (callback passthrough-p condition)
                       (not (callback condition-p condition)))
                   (progn
                     (when outer-hook (callback outer-hook condition hook))
                     (escape condition))
                   (progn
                     (handle-condition condition)
                     (abort-operation)))))
      (let ((failure
              (catch escape-tag
                (loop
                  (let ((result
                          (catch retry-tag
                            (catch return-tag
                              (debugger--call-with-abort-restart
                               (lambda ()
                                 (call-with-debugger-hook
                                  (lambda ()
                                    (handler-bind ((serious-condition #'handle-condition))
                                      (setf raw-values (multiple-value-list (funcall thunk)))))
                                  #'debugger-entry))
                               :name abort-restart-name :report abort-report
                               :function #'abort-operation)
                              ':done))))
                    (unless (eq result ':retry) (return))))
                (notify ':complete (list :status status :condition-report condition-report
                                        :restart-names         restart-names
                                        :selected-restart-name selected-name))
                nil)))
        (when failure (error failure)))
      (debugger--make-outcome :values raw-values :status status
                             :condition-report condition-report :restart-names restart-names
                             :selected-restart-name selected-name))))
