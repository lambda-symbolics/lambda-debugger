(defpackage #:lambda-debugger
  (:documentation "Run code with a restart debugger that lets a selector pick a restart,
retry, return values, or abort, and describe conditions as portable data.")
  (:use #:cl)
  (:import-from #:trivial-gray-streams
                #:fundamental-character-output-stream
                #:stream-write-char #:stream-write-string)
  (:export #:call-with-debugger #:call-with-debugger-hook
           #:outcome #:outcome-values #:outcome-status
           #:outcome-condition-report #:outcome-restart-names
           #:outcome-selected-restart-name
           #:debug-session #:debug-session-condition #:debug-session-restarts
           #:debug-session-snapshot
           #:session-error #:session-error-session #:session-error-reason
           #:recovery #:make-recovery #:validate-recovery
           #:recovery-kind #:recovery-report #:recovery-restart-id
           #:recovery-preparation-source #:recovery-argument-source
           #:recovery-return-source
           #:recovery-error #:recovery-error-proposal #:recovery-error-kind
           #:recovery-error-reason
           #:condition-metadata #:safe-backtrace))
