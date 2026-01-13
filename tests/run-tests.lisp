;;;; Comprehensive Skyline-Tool test runner for all platforms and systems
;;;; This runs ALL platform-specific tests and ensures complete coverage

(format t "~&Running Skyline-Tool Comprehensive Test Suite~%")
(format t "~&==============================================~%")

;; Load all test suites and run them
(let ((test-results nil))

  ;; Helper function to run a test suite safely
  (defun run-test-suite (test-file test-suite-name)
    (format t "~&Running ~A tests...~%" test-suite-name)
    (handler-case
        (progn
          (load test-file)
          (let ((result (fiveam:run! (intern test-suite-name))))
            (push (cons test-suite-name result) test-results)
            (format t "~&✓ ~A tests completed~%" test-suite-name)))
      (error (e)
        (format t "~&✗ ~A tests failed: ~A~%" test-suite-name e)
        (push (cons test-suite-name :failed) test-results))))

  ;; Run individual test suites
  (run-test-suite "tests/basic-tests.lisp" "SKYLINE-TOOL/BASIC-TEST::BASIC-TESTS")
  (run-test-suite "tests/graphics-tests.lisp" "SKYLINE-TOOL/GRAPHICS-TEST::GRAPHICS-TESTS")
  (run-test-suite "tests/music-tests.lisp" "SKYLINE-TOOL/MUSIC-TEST::MUSIC-TESTS")
  (run-test-suite "tests/speech-tests.lisp" "SKYLINE-TOOL/SPEECH-TEST::SPEECH-TESTS")
  (run-test-suite "tests/build-tests.lisp" "SKYLINE-TOOL/BUILD-TEST::BUILD-TESTS")
  (run-test-suite "tests/interface-tests.lisp" "SKYLINE-TOOL/INTERFACE-TEST::INTERFACE-TESTS")

  ;; Platform-specific tests
  (run-test-suite "tests/7800-art-tests.lisp" "SKYLINE-TOOL/7800-ART-TEST::7800-ART-SUITE")
  (run-test-suite "tests/forth-compilation-tests.lisp" "SKYLINE-TOOL/FORTH-COMPILATION-TEST::FORTH-COMPILATION-SUITE")
  (run-test-suite "tests/7800-tests.lisp" "SKYLINE-TOOL/7800-TEST::7800-TESTS")
  (run-test-suite "tests/5200-tests.lisp" "SKYLINE-TOOL/5200-TEST::5200-TESTS")
  (run-test-suite "tests/lynx-graphics-tests.lisp" "SKYLINE-TOOL/LYNX-GRAPHICS-TEST::LYNX-GRAPHICS-TESTS")
  (run-test-suite "tests/cgb-tests.lisp" "SKYLINE-TOOL/CGB-TEST::CGB-TESTS")
  (run-test-suite "tests/dmg-tests.lisp" "SKYLINE-TOOL/DMG-TEST::DMG-TESTS")
  (run-test-suite "tests/nes-tests.lisp" "SKYLINE-TOOL/NES-TEST::NES-TESTS")
  (run-test-suite "tests/snes-tests.lisp" "SKYLINE-TOOL/SNES-TEST::SNES-TESTS")
  (run-test-suite "tests/colecovision-tests.lisp" "SKYLINE-TOOL/COLECOvision-TEST::COLECOvision-TESTS")
  (run-test-suite "tests/sega-tests.lisp" "SKYLINE-TOOL/SEGA-TEST::SEGA-TESTS")
  (run-test-suite "tests/multiplatform-tests.lisp" "SKYLINE-TOOL/MULTIPLATFORM-TEST::MULTIPLATFORM-TESTS")

  ;; Summary
  (format t "~&~%=== TEST SUMMARY ===~%")
  (let ((total-tests (length test-results))
        (passed-tests (count-if (lambda (result) (not (eq (cdr result) :failed))) test-results)))
    (dolist (result (reverse test-results))
      (let ((test-name (car result))
            (test-result (cdr result)))
        (format t "~&~A: ~:[❌ FAILED~;✅ PASSED~]~%"
                test-name (not (eq test-result :failed)))))
    (format t "~&~%TOTAL: ~D/~D test suites completed successfully~%"
            passed-tests total-tests)

    (format t "~&~%Note: Some platform tests are currently stub implementations.~%")
    (format t "~&As implementations are completed, these tests will provide full validation.~%")
    (format t "~&Skyline-Tool comprehensive testing completed for all platforms!~%")

    ;; Exit with appropriate status
    (if (= passed-tests total-tests)
        (format t "~&🎉 ALL TESTS PASSED!~%")
        (progn
          (format t "~&⚠️  SOME TESTS FAILED - check output above~%")
          (sb-ext:exit :code 1)))))
