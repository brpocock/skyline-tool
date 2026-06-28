(cl:defpackage :skyline-tool
  (:use :cl :alexandria :serapeum :split-sequence
        :local-time :cl-change-case :parse-number :bordeaux-threads)
  (:import-from :uiop
                #:run-program
                #:split-string)
  (:shadow #:range)
  (:shadowing-import-from :serapeum #:partition)
  ;; Export ONLY external call points. Unit tests will have to use ::
  (:export #:command
           #:c
           #:about-skyline-tool
           #:run-for-port
           #:run-gui
           #:run-repl))
