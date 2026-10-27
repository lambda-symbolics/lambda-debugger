(in-package #:lambda-debugger)

;;;; -- Portable Recovery Proposals --

(define-condition recovery-error (error)
  ((proposal :initarg :proposal :initform nil :reader recovery-error-proposal
             :documentation "The rejected proposal, when available.")
   (kind :initarg :kind :initform nil :reader recovery-error-kind
         :documentation "The proposed recovery operation.")
   (reason :initarg :reason :reader recovery-error-reason
           :documentation "The reason validation or execution failed."))
  (:report (lambda (condition stream)
             (format stream "Invalid debugger recovery (~A): ~A."
                     (diagnostic--text (recovery-error-kind condition))
                     (recovery-error-reason condition))))
  (:documentation "A typed rejection of a recovery proposal or its live target."))

(defclass recovery ()
  ((kind :initarg :kind :initform nil :reader recovery-kind
         :documentation "One of the six supported recovery kinds.")
   (report :initarg :report :initform nil :reader recovery-report
           :documentation "A nonempty explanation for the person selecting recovery.")
   (restart-id :initarg :restart-id :initform nil :reader recovery-restart-id
               :documentation "An opaque identifier from the target session snapshot.")
   (preparation-source :initarg :preparation-source :initform nil
                       :reader        recovery-preparation-source
                       :documentation "One source form evaluated before repair recovery.")
   (argument-source :initarg :argument-source :initform nil
                    :reader        recovery-argument-source
                    :documentation "One source form producing all restart arguments.")
   (return-source :initarg :return-source :initform nil :reader recovery-return-source
                  :documentation "One source form producing replacement return values."))
  (:documentation "A source-only recovery description, usable across threads.

Readers expose copied strings. Validation and execution never accept arbitrary
values or functions supplied by a diagnosis worker."))

(defun recovery--reject (proposal reason)
  "Signal a typed rejection without printing the proposal or its source."
  (error 'recovery-error :proposal proposal
         :kind                     (when (typep proposal 'recovery) (recovery-kind proposal))
         :reason                   reason))

(defmethod initialize-instance :after ((proposal recovery) &key)
  "Validate and detach the proposal's basic fields at construction."
  (unless (member (recovery-kind proposal)
                  '(:invoke-restart :repair-and-invoke :retry-operation
                    :repair-and-retry :return-values :abort-operation))
    (recovery--reject proposal "unsupported recovery kind"))
  (let ((report (slot-value proposal 'report)))
    (unless (and (stringp report) (< 0 (length report) 8193))
      (recovery--reject proposal "report must be a nonempty string of at most 8192 characters")))
  (dolist (slot '(restart-id preparation-source argument-source return-source))
    (let ((value (slot-value proposal slot))
          (limit (if (eq slot 'restart-id) 256 16384)))
      (when value
        (unless (and (stringp value) (< 0 (length value) (1+ limit)))
          (recovery--reject proposal "target and source fields must be nonempty bounded strings"))
        (setf (slot-value proposal slot) (copy-seq value)))))
  (setf (slot-value proposal 'report) (copy-seq (recovery-report proposal))))

;; Strings are mutable in Common Lisp; do not expose proposal-owned buffers.
(defmethod recovery-report :around ((proposal recovery))
  "Return a detached report string."
  (let ((value (call-next-method)))
    (and value (copy-seq value))))

(defmethod recovery-restart-id :around ((proposal recovery))
  "Return a detached restart identifier."
  (let ((value (call-next-method)))
    (and value (copy-seq value))))

(defmethod recovery-preparation-source :around ((proposal recovery))
  "Return detached preparation source."
  (let ((value (call-next-method)))
    (and value (copy-seq value))))

(defmethod recovery-argument-source :around ((proposal recovery))
  "Return detached restart argument source."
  (let ((value (call-next-method)))
    (and value (copy-seq value))))

(defmethod recovery-return-source :around ((proposal recovery))
  "Return detached replacement value source."
  (let ((value (call-next-method)))
    (and value (copy-seq value))))

(defun make-recovery (&key kind report restart-id preparation-source
                          argument-source return-source)
  "Construct a typed source-only proposal; session validation occurs before execution."
  (make-instance 'recovery :kind kind :report report :restart-id restart-id
                 :preparation-source preparation-source
                 :argument-source argument-source :return-source return-source))

(defun validate-recovery (snapshot proposal)
  "Validate PROPOSAL against SNAPSHOT before any source evaluation.

Return the same proposal. Signal RECOVERY-ERROR for invalid fields, unsupported
capabilities, or an identifier from a different failure session."
  (unless (typep proposal 'recovery)
    (recovery--reject proposal "proposal is not a recovery"))
  (let* ((kind         (recovery-kind proposal))
         (target       (recovery-restart-id proposal))
         (preparation  (recovery-preparation-source proposal))
         (arguments    (recovery-argument-source proposal))
         (returned     (recovery-return-source proposal))
         (capabilities (diagnostic--plist-value snapshot :capabilities))
         (restarts     (diagnostic--plist-value snapshot :restarts)))
    (unless (diagnostic--plist-value capabilities kind)
      (recovery--reject proposal "operation does not support this recovery"))
    (when (member kind '(:invoke-restart :repair-and-invoke))
      (unless (and target
                   (loop for tail = restarts then (rest tail)
                         repeat 256 while (consp tail)
                         thereis (equal target
                                        (diagnostic--plist-value (first tail) :id))))
        (recovery--reject proposal "target is not a restart in this failure session")))
    (when (and (member kind '(:repair-and-invoke :repair-and-retry))
               (null preparation))
      (recovery--reject proposal "repair requires preparation source"))
    (when (and (eq kind ':return-values) (null returned))
      (recovery--reject proposal "replacement values require return source"))
    (let ((allowed
            (case kind
              (:invoke-restart
               '(restart-id argument-source))
              (:repair-and-invoke
               '(restart-id preparation-source argument-source))
              (:repair-and-retry
               '(preparation-source))
              (:return-values
               '(return-source)))))
      (loop for slot in '(restart-id preparation-source argument-source return-source)
            when (and (slot-value proposal slot) (not (member slot allowed)))
              do (recovery--reject proposal "fields are not allowed for this recovery kind")))
    proposal))
