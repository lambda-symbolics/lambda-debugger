(in-package #:lambda-debugger)

;;;; -- Behavioral Test Support --

(defparameter *tests* nil "Registered behavior checks in load order.")
(defparameter *test-count* 0 "Assertions performed in the current run.")
(defparameter *test-repaired* nil "Owner-thread source evaluation fixture.")

(define-condition assertion-failure (error)
  ((description :initarg :description :reader assertion-failure-description))
  (:report (lambda (condition stream)
             (write-string (assertion-failure-description condition) stream)))
  (:documentation "A failed test assertion, distinct from a deliberately signaled error."))

(defun check (value &optional (description "assertion failed"))
  "Record one assertion or fail the current behavior check."
  (incf *test-count*)
  (unless value (error 'assertion-failure :description description)))

(defmacro check-equal (expected form)
  "Evaluate each operand once and compare its value."
  (let ((expected-value (gensym "EXPECTED")) (actual-value (gensym "ACTUAL")))
    `(let ((,expected-value ,expected) (,actual-value ,form))
       (check (equal ,expected-value ,actual-value)
              (format nil "Expected ~S, got ~S" ,expected-value ,actual-value)))))

(defmacro deftest (name &body body)
  "Define and register one behavior owner."
  `(progn
     (defun ,name () ,@body)
     (pushnew ',name *tests*)))

(defun captured-condition (thunk)
  "Return the condition escaping THUNK, or NIL."
  (handler-case (progn (funcall thunk) nil)
    (condition (condition) condition)))

(defun restart-with-name (session name)
  "Find NAME among this session's live restart objects."
  (find name (debug-session-restarts session) :key #'restart-name))

(defun restart-id-for-name (session name)
  "Resolve NAME using the core's one ordered restart description set."
  (loop for restart in (debug-session-restarts session)
        for description in (getf (debug-session-snapshot session) :restarts)
        when (eq name (restart-name restart)) return (getf description :id)))

(defun portable-p (value)
  "Recognize the library's portable diagnostic data vocabulary."
  (typecase value
    ((or boolean string integer keyword) t)
    (cons (and (portable-p (first value)) (portable-p (rest value))))
    (t nil)))

(define-condition custom-test-condition (error) ()
  (:report "custom failure")
  (:documentation "Fixture for extensible diagnostic metadata."))

(defmethod condition-metadata ((condition custom-test-condition))
  "Provide intentionally overlong and invalid custom diagnostic fields."
  (declare (ignore condition))
  (list :category ':custom
        :fields (list (list :label "Long value" :value (make-string 900 :initial-element #\x))
                      (list :label "Live object" :value #'identity))
        :location (list :pathname "test.lisp" :line 2 :column 3 :position 7)))


;;;; -- Live Selection and Operation Boundaries --

(deftest success-values
  (dolist (values '(() (nil) (1 nil :three)))
    (let ((outcome (call-with-debugger (lambda () (values-list values)))))
      (check-equal values (outcome-values outcome))
      (check (eq (outcome-status outcome) ':ok)))))

(deftest restart-identity
  (let ((outcome
          (call-with-debugger
           (lambda ()
             (restart-case
                 (restart-case (error "duplicate names")
                   (use-value (value) value))
               (use-value (left right) (+ left right))))
           :selector
           (lambda (session)
             (let* ((live (remove-if-not
                           (lambda (restart) (eq (restart-name restart) 'use-value))
                           (debug-session-restarts session)))
                    (ids (mapcar (lambda (item) (getf item :id))
                                 (getf (debug-session-snapshot session) :restarts))))
               (check (= 2 (length live)))
               (check (= (length ids) (length (remove-duplicates ids :test #'equal))))
               (values (second live) "(values 40 2)"))))))
    (check-equal '(42) (outcome-values outcome))
    (check-equal "USE-VALUE" (outcome-selected-restart-name outcome))))

(deftest anonymous-filter-and-local-abort
  (let ((outer-aborted-p nil))
    (let ((outcome
            (restart-case
                (call-with-debugger
                 (lambda ()
                   (restart-bind ((nil (lambda () nil)
                                       :report-function (lambda (stream) (write-string "anonymous" stream))))
                     (error "abort")))
                 :abort-restart-name 'back-to-repl
                 :selector (lambda (session)
                             (check (every #'restart-name (debug-session-restarts session)))
                             (restart-with-name session 'back-to-repl)))
              (abort-operation () (setf outer-aborted-p t)))))
      (check (not outer-aborted-p))
      (check (eq (outcome-status outcome) ':aborted))
      (check-equal "BACK-TO-REPL" (outcome-selected-restart-name outcome)))))

(deftest recovery-actions
  (dolist (kind '(:invoke-restart :repair-and-invoke :retry-operation
                 :repair-and-retry :return-values :abort-operation))
    (let ((attempts 0) (effects nil))
      (setf *test-repaired* nil)
      (let ((outcome
              (call-with-debugger
               (lambda ()
                 (push (incf attempts) effects)
                 (if (> attempts 1)
                     42
                     (restart-case (error "recover")
                       (use-value (left right) (+ left right)))))
               :return-values-p t
               :selector
               (lambda (session)
                 (make-recovery
                  :kind kind :report "Recover this operation"
                  :restart-id (when (member kind '(:invoke-restart :repair-and-invoke))
                                (restart-id-for-name session 'use-value))
                  :preparation-source
                  (when (member kind '(:repair-and-invoke :repair-and-retry))
                    "(setf lambda-debugger::*test-repaired* :ready)")
                  :argument-source (when (member kind '(:invoke-restart :repair-and-invoke))
                                     "(values 40 2)")
                  :return-source (when (eq kind ':return-values) "(values 42 nil)"))))))
        (check (eq (outcome-status outcome) (if (eq kind ':abort-operation) ':aborted ':ok)))
        (check-equal (case kind (:abort-operation nil) (:return-values '(42 nil)) (t '(42)))
                     (outcome-values outcome))
        (check-equal (if (member kind '(:retry-operation :repair-and-retry)) '(2 1) '(1)) effects)
        (when (member kind '(:repair-and-invoke :repair-and-retry))
          (check (eq *test-repaired* ':ready)))))))

(deftest proposal-validation
  (let ((snapshot '(:restarts ((:id "target" :report "use value"))
                    :capabilities (:invoke-restart t :repair-and-invoke t
                                   :retry-operation t :repair-and-retry t
                                   :return-values t :abort-operation t))))
    (dolist (fields '((:kind :invoke-restart :restart-id "target")
                      (:kind :repair-and-invoke :restart-id "target" :preparation-source "nil")
                      (:kind :retry-operation)
                      (:kind :repair-and-retry :preparation-source "nil")
                      (:kind :return-values :return-source "nil")
                      (:kind :abort-operation)))
      (let ((proposal (apply #'make-recovery :report "valid" fields)))
        (check (eq proposal (validate-recovery snapshot proposal)))))
    (dolist (fields '((:kind :invoke-restart :restart-id "other")
                      (:kind :invoke-restart :restart-id "target" :preparation-source "nil")
                      (:kind :repair-and-invoke :restart-id "target")
                      (:kind :retry-operation :argument-source "1")
                      (:kind :repair-and-retry :restart-id "target" :preparation-source "nil")
                      (:kind :return-values)
                      (:kind :abort-operation :return-source "1")))
      (check (typep (captured-condition
                     (lambda ()
                       (validate-recovery snapshot (apply #'make-recovery :report "invalid" fields))))
                    'recovery-error)))
    (dolist (fields '((:kind :unsupported :report "x")
                      (:kind :abort-operation :report nil)
                      (:kind :abort-operation :report t)
                      (:kind :abort-operation :report "")
                      (:kind :return-values :report "x" :return-source 3)))
      (let ((condition (captured-condition (lambda () (apply #'make-recovery fields)))))
        (check (typep condition 'recovery-error))
        (check (stringp (princ-to-string condition)))))
    (check (typep (captured-condition
                   (lambda () (validate-recovery '(:capabilities nil)
                                                 (make-recovery :kind ':retry-operation :report "x"))))
                  'recovery-error))))

(deftest action-failure-and-source-policy
  (dolist (source '("1 2" " " "#.(error \"reader-time\")" "(error \"action\")"))
    (let ((calls 0) (failure nil) (first-id nil))
      (let ((outcome
              (call-with-debugger
               (lambda () (error "original"))
               :return-values-p t
               :failure-handler (lambda (condition) (setf failure condition))
               :selector (lambda (session)
                           (let ((id (getf (first (getf (debug-session-snapshot session) :restarts)) :id)))
                             (if first-id (check-equal first-id id) (setf first-id id)))
                           (when (= (incf calls) 1)
                             (make-recovery :kind ':return-values :report "bad" :return-source source))))))
        (check (eq (outcome-status outcome) ':aborted))
        (check (= calls 2))
        (check (typep failure 'error))))))

(deftest stale-proposal
  (let (old-proposal (calls 0) (failures 0))
    (call-with-debugger
     (lambda () (restart-case (error "old") (use-value (value) value)))
     :selector (lambda (session)
                 (setf old-proposal (make-recovery :kind ':invoke-restart :report "old"
                                                  :restart-id (restart-id-for-name session 'use-value)
                                                  :argument-source "1"))
                 nil))
    (let ((outcome
            (call-with-debugger
             (lambda () (restart-case (error "new") (use-value (value) value)))
             :failure-handler (lambda (condition)
                                (check (typep condition 'recovery-error)) (incf failures))
             :selector (lambda (session)
                         (if (= (incf calls) 1)
                             old-proposal
                             (values (restart-with-name session 'use-value) "42"))))))
      (check-equal '(42) (outcome-values outcome))
      (check (= failures 1)))))

(deftest callback-failure-boundaries
  (dolist (role '(:selector :observer :condition-p :passthrough-p :restart-p :failure-handler))
    (let ((fault (make-condition 'simple-error :format-control "callback fault")) (calls 0))
      (let* ((callback (lambda (&rest ignored) (declare (ignore ignored)) (incf calls) (error fault)))
             (options (if (eq role ':failure-handler)
                          (list :return-values-p t
                                :selector (lambda (session) (declare (ignore session))
                                            (make-recovery :kind ':return-values :report "bad"
                                                           :return-source "(error \"action\")"))
                                role callback)
                          (list role callback))))
        (check (eq fault (captured-condition
                          (lambda () (apply #'call-with-debugger (lambda () (error "operation")) options)))))
        (check (= calls 1))))))

(deftest condition-policy-and-passthrough
  (let ((fault (make-condition 'simple-error :format-control "control")))
    (check (eq fault (captured-condition
                      (lambda () (call-with-debugger (lambda () (error fault))
                                                    :passthrough-p (constantly t)))))))
  (let ((outer-calls 0) (selector-calls 0))
    (handler-bind ((simple-error (lambda (condition) (declare (ignore condition)) (incf outer-calls))))
      (check-equal '(:done)
                   (outcome-values
                    (call-with-debugger
                     (lambda () (signal (make-condition 'simple-error :format-control "filtered")) :done)
                     :condition-p (constantly nil)
                     :selector (lambda (session) (declare (ignore session)) (incf selector-calls))))))
    (check (= outer-calls 1))
    (check (zerop selector-calls)))
  (let ((fault (make-condition 'simple-error :format-control "control in action")))
    (check (eq fault (captured-condition
                      (lambda ()
                        (call-with-debugger
                         (lambda () (error "operation")) :return-values-p t
                         :passthrough-p (lambda (condition) (eq condition fault))
                         :evaluate-source (lambda (source) (declare (ignore source)) (error fault))
                         :selector (lambda (session) (declare (ignore session))
                                     (make-recovery :kind ':return-values :report "control" :return-source "nil")))))))))

(deftest explicit-debugger-and-hook-restoration
  (check-equal '(:continued)
               (outcome-values
                (call-with-debugger
                 (lambda () (restart-case (invoke-debugger (make-condition 'condition))
                              (continue () :continued)))
                 :selector (lambda (session) (restart-with-name session 'continue)))))
  (let ((outer-cl *debugger-hook*) (outer-sbcl sb-ext:*invoke-debugger-hook*) (calls 0))
    (catch 'resolved
      (call-with-debugger-hook
       (lambda () (invoke-debugger (make-condition 'condition)))
       (lambda (condition hook) (declare (ignore condition hook))
         (incf calls) (throw 'resolved t))))
    (check (= calls 1))
    (check (eq outer-cl *debugger-hook*))
    (check (eq outer-sbcl sb-ext:*invoke-debugger-hook*))))

(deftest session-lifetime-and-detachment
  (let (saved snapshot)
    (call-with-debugger
     (lambda () (error "session"))
     :selector
     (lambda (session)
       (setf saved session snapshot (debug-session-snapshot session))
       (check (portable-p snapshot))
       (check (every #'stringp (getf snapshot :backtrace)))
       (let ((thread (sb-thread:make-thread
                      (lambda ()
                        (and (typep (captured-condition
                                     (lambda () (debug-session-condition session))) 'session-error)
                             (portable-p (debug-session-snapshot session)))))))
         (check (sb-thread:join-thread thread)))
       (setf (char (getf snapshot :condition-report) 0) #\x)
       (check-equal "session" (getf (debug-session-snapshot session) :condition-report))
       nil))
    (check (typep (captured-condition (lambda () (debug-session-restarts saved))) 'session-error))
    (check (portable-p (debug-session-snapshot saved)))))

(deftest typed-metadata-and-bounds
  (dolist (condition (list (make-condition 'type-error :datum 1 :expected-type 'string)
                           (make-condition 'undefined-function :name 'missing-function)
                           (make-condition 'unbound-variable :name 'missing-variable)
                           (make-condition 'division-by-zero :operation '/ :operands '(1 0))
                           (make-condition 'file-error :pathname #P"missing.lisp")
                           (make-condition 'package-error :package (find-package '#:cl))))
    (let ((metadata (condition-metadata condition)))
      (check (portable-p metadata))
      (check (getf metadata :fields))))
  (let ((stream (make-string-input-stream (format nil "(a~% . .)"))) metadata)
    (let ((condition
            (captured-condition
             (lambda ()
               (handler-bind ((reader-error
                                (lambda (condition)
                                  (setf metadata (condition-metadata condition)))))
                 (read stream))))))
      (check (typep condition 'reader-error))
      (let ((location (getf metadata :location)))
        (check-equal 2 (getf location :line))
        (check (plusp (getf location :column)))
        (check-equal (file-position stream) (getf location :position))))
    (close stream))
  (let ((cycle (list :cycle)))
    (setf (rest cycle) cycle)
    (dolist (datum (list cycle (make-string 50000 :initial-element #\x)))
      (let ((value (getf (first (getf (condition-metadata
                                     (make-condition 'type-error :datum datum :expected-type 'integer))
                                    :fields)) :value)))
        (check (<= (length value) 512)))))
  (let (snapshot)
    (call-with-debugger (lambda () (error 'custom-test-condition))
                       :selector (lambda (session) (setf snapshot (debug-session-snapshot session)) nil))
    (let ((metadata (getf snapshot :condition-metadata)))
      (check (portable-p metadata))
      (check (= 1 (length (getf metadata :fields))))
      (check (= 512 (length (getf (first (getf metadata :fields)) :value))))
      (check-equal 2 (getf (getf metadata :location) :line)))))

(deftest observable-operation
  (let ((attempts 0) (events nil))
    (call-with-debugger
     (lambda () (if (= (incf attempts) 1) (error "retry") #'identity))
     :selector (lambda (session) (declare (ignore session))
                 (make-recovery :kind ':retry-operation :report "retry"))
     :observer (lambda (event payload) (check (portable-p payload)) (push event events)))
    (check-equal '(:condition :retry :complete) (reverse events))))

(defun run-tests ()
  "Run all registered behavior owners, returning failures and assertion count."
  (let ((*test-count* 0) (failures 0))
    (dolist (test (reverse *tests*))
      (let* ((failure-tag (gensym "TEST-FAILURE"))
             (failure
               (catch failure-tag
                 (call-with-debugger-hook
                  (lambda ()
                    (handler-case (funcall test)
                      (assertion-failure (condition)
                        (throw failure-tag condition))))
                  (lambda (condition hook)
                    (declare (ignore hook))
                    (throw failure-tag condition)))
                 nil)))
        (if failure
            (progn
              (incf failures)
              (format t "FAIL ~A: ~A~%" test failure))
            (format t "PASS ~A~%" test))))
    (format t "lambda-debugger: ~D cases, ~D assertions, ~D failures~%"
            (length *tests*) *test-count* failures)
    (values failures *test-count*)))
