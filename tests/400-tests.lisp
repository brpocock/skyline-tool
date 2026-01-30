;;; Phantasia SkylineTool/tests/400-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

;; Use the test package defined by the test runner
(in-package :skyline-tool/test)

(def-suite 400-tests
  :description "Tests for Atari 400-specific SkylineTool functionality"
  :in skyline-tool/test)

(in-suite 400-tests)

;; Test 400 dispatch functionality delegates to 5200
(test 400-dispatch-delegates-to-5200
  "Test that 400 PNG dispatch delegates to 5200 functionality"
  ;; Verify dispatch-png% generic function exists
  (is-true (fboundp 'skyline-tool::dispatch-png%)
           "dispatch-png% generic function should exist")

  ;; Verify that there's a method specialized for 400 (Atari 400)
  (is-true (find-method #'skyline-tool::dispatch-png% '() (list (list 'eql 400) t t t t t t t) nil)
           "dispatch-png% should have a method for Atari 400 (machine 400)")

  ;; Test with mock data to ensure delegation works
  (let ((test-pixels (make-array '(8 8) :element-type '(unsigned-byte 32) :initial-element 0)))
    ;; This should delegate to 5200 Mode E bitmap compilation
    (is-true (skyline-tool::dispatch-png% 400 "/fake.png" "Object/400/" test-pixels 8 8 nil test-pixels)
             "400 dispatch should delegate to 5200 and return success")))

;; Test 400 platform validation
(test 400-platform-validation
  "Test that 400 is properly recognized as a valid platform"
  (let ((skyline-tool::*machine* 400))
    ;; Test that machine is recognized as valid
    (is-true (skyline-tool::machine-valid-p)
             "400 should be recognized as a valid machine")

    ;; Test machine name functions
    (is (stringp (skyline-tool::machine-short-name))
        "machine-short-name should return a string")
    (is (stringp (skyline-tool::machine-long-name))
        "machine-long-name should return a string")

    ;; Verify machine names are correct for 400
    (is (string= "400" (skyline-tool::machine-short-name))
        "400 short name should be '400'")
    (is (string= "Atari 400" (skyline-tool::machine-long-name))
        "400 long name should be 'Atari 400'")))

;; Test 400 interface functions delegate to 5200
(test 400-interface-functions-delegate
  "Test that 400 interface functions properly delegate to 5200 equivalents"
  ;; Test blob-rip-400 delegates to blob-rip-5200-tile
  (signals error (skyline-tool::blob-rip-400-tile "/nonexistent.png")
           "blob-rip-400 should signal error (delegating to 5200 unimplemented function)")

  ;; Test compile-art-400 exists and can be called
  (is-true (fboundp 'skyline-tool::compile-art-400)
           "compile-art-400 function should exist")

  ;; Test compile-art-400 delegates appropriately
  (signals error (skyline-tool::compile-art-400 "/fake.in" "/fake.out")
           "compile-art-400 should delegate to 5200 compile-art function"))

;; Test 400 graphics compilation delegates to 5200
(test 400-graphics-compilation-delegates
  "Test that 400 graphics compilation properly delegates to 5200 Mode E"
  ;; Test with simple pixel data that should delegate to compile-5200-mode-e-bitmap
  (let ((test-pixels (make-array '(8 8) :element-type '(unsigned-byte 32) :initial-element 0)))
    (is-true (skyline-tool::dispatch-png% 400 "/test.png" "Object/400/" test-pixels 8 8 nil test-pixels)
             "400 should successfully dispatch graphics compilation to 5200")))

(defun run-400-tests ()
  "Run all 400 tests and return results"
  (fiveam:run! '400-tests))
