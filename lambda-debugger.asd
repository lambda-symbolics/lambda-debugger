(asdf:defsystem #:lambda-debugger
  :description "Live restart selection, portable recoveries, and typed diagnostics."
  :author      "Lambda Symbolics OÜ"
  :version     "0.1.0"
  :license     "COLL-Attribution"
  :depends-on  (#:trivial-gray-streams)
  :serial      t
  :components  ((:file "src/package")
                (:file "src/metadata")
                (:file "src/recovery")
                (:file "src/debugger"))
  :in-order-to ((asdf:test-op (asdf:test-op #:lambda-debugger/tests))))

(asdf:defsystem #:lambda-debugger/tests
  :depends-on (#:lambda-debugger)
  :serial     t
  :components ((:file "tests/core-tests"))
  :perform    (asdf:test-op (operation system)
                (declare (ignore operation system))
                (let ((failures (uiop:symbol-call '#:lambda-debugger '#:run-tests)))
                  (unless (zerop failures)
                    (error "~D lambda-debugger cases failed." failures)))))
