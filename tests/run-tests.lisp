;; Comprehensive Skyline-Tool Converter Test Runner
;; Tests all converter functionality suites

(defparameter *converter-test-results* nil)

(format t "~%=== SKYLINE-TOOL CONVERTER TEST SUITE ===~%")
(format t "Running comprehensive converter functionality tests...~%~%")

;; Test 1: Infrastructure verification
(format t "1. Infrastructure verification... ")
(let* ((makefile (or (probe-file "Makefile")
                     (some (lambda (f) (probe-file f))
                           (directory "Makefile.*"))))
       (source-dir (probe-file "Source/"))
       (skyline-dir (probe-file "SkylineTool/src/"))
       ;; Platform-specific tools (optional - check if Makefiles reference them)
       (intv-tools (and (probe-file "Makefile.Intv")
                        (probe-file "Tools/jzIntv/bin/as1600")
                        (probe-file "Tools/jzIntv/bin/jzintv")))
       (common-ok (and makefile source-dir skyline-dir))
       (required-makefiles '("Makefile.Intv" "Makefile.7800" "Makefile.Lynx" "Makefile.NES"
                             "Makefile.SNES" "Makefile.DMG" "Makefile.CGB" "Makefile.SMS"
                             "Makefile.GG" "Makefile.TG16" "Makefile.CBM" "Makefile.C16"
                             "Makefile.A2" "Makefile.A3" "Makefile.2GS" "Makefile.5200"
                             "Makefile.1000" "Makefile.BBC"))
       (missing-makefiles (loop for file in required-makefiles
                                unless (probe-file file)
                                collect file)))
    (unless common-ok
      (format *error-output* "ERROR: Infrastructure check failed: missing required files/directories~%")
      (unless makefile
        (format *error-output* "  Missing: Makefile or Makefile.*~%"))
      (unless source-dir
        (format *error-output* "  Missing: Source/ directory~%"))
      (unless skyline-dir
        (format *error-output* "  Missing: SkylineTool/src/ directory~%"))
      (sb-ext:exit :code 1))
    (when missing-makefiles
      (format *error-output* "ERROR: Missing required Makefiles:~%")
      (dolist (file missing-makefiles)
        (format *error-output* "  Missing: ~A~%" file))
      (sb-ext:exit :code 1))
    (when (and (probe-file "Makefile.Intv") (not intv-tools))
      (format t "   ⚠️  Intellivision tools not found (as1600/jzintv)~%"))
    (format t "✅ PASSED~%")
    (push (cons :infrastructure t) *converter-test-results*))

;; Load all test files - runtime will signal error if any are missing
(load "SkylineTool/tests/action-tests.lisp")
(load "SkylineTool/tests/graphics-tests.lisp")
(load "SkylineTool/tests/music-tests.lisp")
(load "SkylineTool/tests/compiler-tests.lisp")
(load "SkylineTool/tests/intv-gram-tests.lisp")

;; Run all test suites
(funcall (intern "RUN-ACTION-TESTS" :skyline-tool/test))
(funcall (intern "RUN-GRAPHICS-TESTS" :skyline-tool/test))
(funcall (intern "RUN-MUSIC-TESTS" :skyline-tool/test))
(funcall (intern "RUN-CONVERTER-TESTS" :skyline-tool/tests/converters))
(funcall (intern "RUN-INTV-GRAM-TESTS" :skyline-tool/test))

(format t "~%done~%")
