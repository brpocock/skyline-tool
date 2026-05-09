;;; Phantasia SkylineTool/tests/near-term-makefile-parse-tests.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC.

(in-package :skyline-tool/test)

(def-suite near-term-makefile-parse-tests
  :description "Near-term port Makefile parse smoke tests"
  :in skyline-tool/test)

(in-suite near-term-makefile-parse-tests)

(defun run-near-term-makefile-parse-tests ()
  "Runs the near-term port Makefile parse tests.

@table @asis
@item Inputs
Uses the test definitions already provided by @file{interface-tests.lisp}.
@item Outputs
Returns the aggregate FiveAM result.
@end table"
  (every #'identity
         (list (fiveam:run! 'write-master-makefile-lynx-make-n-parses)
               (fiveam:run! 'write-master-makefile-intv-make-n-parses)
               (fiveam:run! 'write-master-makefile-5200-make-n-parses))))
