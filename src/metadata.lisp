(in-package #:lambda-debugger)

;;;; -- Bounded Diagnostic Text --

(defclass diagnostic-output-stream (fundamental-character-output-stream)
  ((buffer :initarg :buffer :reader diagnostic-output-buffer
           :documentation "Fixed-capacity character buffer.")
   (stop-tag :initarg :stop-tag :reader diagnostic-output-stop-tag
             :documentation "Catch tag used when the buffer is full."))
  (:documentation "A bounded printer sink that stops printing at its limit."))

(defmethod stream-write-char ((stream diagnostic-output-stream) character)
  "Append CHARACTER or stop the current diagnostic printer."
  (let ((buffer (diagnostic-output-buffer stream)))
    (unless (vector-push character buffer)
      (throw (diagnostic-output-stop-tag stream) t)))
  character)

(defmethod stream-write-string
    ((stream diagnostic-output-stream) string &optional (start 0) end)
  "Append only the part of STRING that fits in the diagnostic buffer."
  (loop for index from start below (or end (length string))
        do (stream-write-char stream (char string index)))
  string)

(defun diagnostic--text (object &key (readable-p nil) (limit 512))
  "Print OBJECT into at most LIMIT characters, tolerating printer failures."
  (let* ((buffer         (make-array limit :element-type 'character :fill-pointer 0))
         (stop-tag       (gensym "DIAGNOSTIC-LIMIT"))
         (stream         (make-instance 'diagnostic-output-stream
                                        :buffer buffer :stop-tag stop-tag))
         (*print-circle* t)
         (*print-level*  4)
         (*print-length* 16)
         (*print-pretty* nil))
    (let ((truncated-p
            (catch stop-tag
              (handler-case
                  (write object :stream stream :escape readable-p)
                (error ()
                  (setf (fill-pointer buffer) 0)
                  (write-string "<unprintable>" stream)))
              nil)))
      (when (and truncated-p (>= limit 3))
        (replace buffer "..." :start1 (- limit 3))))
    (coerce buffer 'simple-string)))

(defun diagnostic--bounded-string (string limit)
  "Copy at most LIMIT characters from STRING."
  (subseq string 0 (min limit (length string))))

(defun diagnostic--plist-value (object key)
  "Read a short possibly malformed property list without following cycles."
  (loop repeat 32
        for tail = object then (cddr tail)
        while (and (consp tail) (consp (rest tail)))
        when (eq (first tail) key)
          return (second tail)))


;;;; -- Typed Condition Metadata --

(defgeneric condition-metadata (condition)
  (:documentation
   "Describe CONDITION as :CATEGORY, :FIELDS, and optional :LOCATION.

Each field is (:LABEL string :VALUE string). Locations use :PATHNAME, :LINE,
:COLUMN, and :POSITION. Methods should return diagnostic text, not live objects.
Snapshot construction bounds and copies these descriptions."))

(defmethod condition-metadata ((condition condition))
  "Provide an empty detail set for an otherwise unknown condition type."
  (declare (ignore condition))
  (list :category ':condition :fields nil :location nil))

(defmethod condition-metadata ((condition type-error))
  "Describe the offending datum and the expected Common Lisp type."
  (list :category ':type-error
        :fields   (list (list :label "Value"
                              :value (diagnostic--text (type-error-datum condition)
                                                      :readable-p t))
                        (list :label "Expected type"
                              :value (diagnostic--text (type-error-expected-type condition)
                                                      :readable-p t)))))

(defmethod condition-metadata ((condition cell-error))
  "Describe the missing or unbound cell by its name."
  (list :category (cond
                    ((typep condition 'undefined-function)
                     ':undefined-function)
                    ((typep condition 'unbound-variable)
                     ':unbound-variable)
                    (t
                     ':cell-error))
        :fields   (list (list :label "Name"
                              :value (diagnostic--text (cell-error-name condition)
                                                      :readable-p t)))))

(defmethod condition-metadata ((condition arithmetic-error))
  "Describe the arithmetic operation and its operands."
  (list :category ':arithmetic-error
        :fields   (list (list :label "Operation"
                              :value (diagnostic--text
                                      (arithmetic-error-operation condition)
                                      :readable-p t))
                        (list :label "Operands"
                              :value (diagnostic--text
                                      (arithmetic-error-operands condition)
                                      :readable-p t)))))

(defmethod condition-metadata ((condition file-error))
  "Describe the file involved in a file operation failure."
  (let ((path (file-error-pathname condition)))
    (list :category ':file-error
          :fields   (list (list :label "File" :value (diagnostic--text path)))
          :location (when path
                      (list :pathname (diagnostic--text path))))))

(defmethod condition-metadata ((condition package-error))
  "Describe the package involved in a package operation failure."
  (let ((package (package-error-package condition)))
    (list :category ':package-error
          :fields   (list (list :label "Package"
                                :value (diagnostic--text
                                        (if (packagep package)
                                            (package-name package)
                                          package)))))))

(defun diagnostic--reader-location (stream)
  "Describe the current stream cursor, with one-based string line and column."
  (let ((position (ignore-errors (file-position stream)))
        (path     (ignore-errors (namestring (pathname stream)))))
    (if (and (typep stream 'sb-impl::string-input-stream) (integerp position))
        (let* ((string       (sb-impl::string-input-stream-string stream))
               (start        (sb-impl::string-input-stream-start stream))
               (end          (min (length string) (+ start position)))
               (last-newline (position #\Newline string :start start :end end :from-end t)))
          (list :position position
                :line     (1+ (count #\Newline string :start start :end end))
                :column   (1+ (- end (if last-newline (1+ last-newline) start)))))
        (when (or path (integerp position))
          (list :pathname path :position position)))))

(defmethod condition-metadata ((condition reader-error))
  "Describe a reader stream's available pathname and cursor location.

Capture while signaling: SBCL may discard a string stream when closing it.
POSITION is a stream offset, not a guessed token location. String stream LINE
and COLUMN are one-based, relative to the stream's starting offset."
  (list :category ':reader-error :fields nil
        :location (diagnostic--reader-location (stream-error-stream condition))))

(defun diagnostic--metadata (condition)
  "Normalize extensible metadata into a bounded detached portable description."
  (let* ((raw      (condition-metadata condition))
         (category (diagnostic--plist-value raw :category))
         (fields   (diagnostic--plist-value raw :fields))
         (location (diagnostic--plist-value raw :location))
         (normalized-fields
           (loop repeat 16
                 for tail = fields then (rest tail)
                 while (consp tail)
                 for field = (first tail)
                 for label = (diagnostic--plist-value field :label)
                 for value = (diagnostic--plist-value field :value)
                 when (and (stringp label) (stringp value))
                   collect (list :label (diagnostic--bounded-string label 80)
                                 :value (diagnostic--bounded-string value 512))))
         (normalized-location
           (loop for key in '(:pathname :line :column :position)
                 for value = (diagnostic--plist-value location key)
                 when (or (and (eq key ':pathname) (stringp value))
                          (and (not (eq key ':pathname))
                               (integerp value) (>= value 0)))
                   append (list key (if (stringp value)
                                        (diagnostic--bounded-string value 512)
                                        value)))))
    (list :category (if (keywordp category) category ':condition)
          :fields   normalized-fields
          :location normalized-location)))

(defun safe-backtrace (&key (count 30))
  "Return at most COUNT argument-free, bounded call-name strings on SBCL."
  (check-type count (integer 0 256))
  (handler-case
      (mapcar (lambda (frame)
                (diagnostic--text (first frame) :readable-p t :limit 160))
              (sb-debug:list-backtrace :count count :argument-limit 0
                                      :from ':current-frame))
    (error ()
      nil)))
