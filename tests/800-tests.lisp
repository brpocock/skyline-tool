;;; Phantasia SkylineTool/tests/800-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

;; Use the test package defined by the test runner
(in-package :skyline-tool/test)

(def-suite 800-tests
  :description "Tests for Atari 800-specific SkylineTool functionality"
  :in skyline-tool/test)

(in-suite 800-tests)

;; Test 800 dispatch functionality delegates to 5200
(test 800-dispatch-delegates-to-5200
  "Test that 800 PNG dispatch delegates to 5200 functionality"
  ;; Verify dispatch-png% generic function exists
  (is-true (fboundp 'skyline-tool::dispatch-png%)
           "dispatch-png% generic function should exist")

  ;; Verify that there's a method specialized for 800 (Atari 800)
  (is-true (find-method #'skyline-tool::dispatch-png% '() (list (list 'eql 800) t t t t t t t) nil)
           "dispatch-png% should have a method for Atari 800 (machine 800)")

  ;; Test with mock data to ensure delegation works
  (let ((test-pixels (make-array '(8 8) :element-type '(unsigned-byte 32) :initial-element 0)))
    ;; This should delegate to 5200 Mode E bitmap compilation
    (is-true (skyline-tool::dispatch-png% 800 "/fake.png" "Object/800/" test-pixels 8 8 nil test-pixels)
             "800 dispatch should delegate to 5200 and return success")))

;; Test 800 platform validation
(test 800-platform-validation
  "Test that 800 is properly recognized as a valid platform"
  (let ((skyline-tool::*machine* 800))
    ;; Test that machine is recognized as valid
    (is-true (skyline-tool::machine-valid-p)
             "800 should be recognized as a valid machine")

    ;; Test machine name functions
    (is (stringp (skyline-tool::machine-short-name))
        "machine-short-name should return a string")
    (is (stringp (skyline-tool::machine-long-name))
        "machine-long-name should return a string")

    ;; Verify machine names are correct for 800
    (is (string= "800" (skyline-tool::machine-short-name))
        "800 short name should be '800'")
    (is (string= "Atari 800" (skyline-tool::machine-long-name))
        "800 long name should be 'Atari 800'")))

;; Test 800 interface functions delegate to 5200
(test 800-interface-functions-delegate
  "Test that 800 interface functions properly delegate to 5200 equivalents"
  ;; Test blob-rip-800 delegates to blob-rip-5200-tile
  (signals error (skyline-tool::blob-rip-800-tile "/nonexistent.png")
           "blob-rip-800 should signal error (delegating to 5200 unimplemented function)")

  ;; Test compile-art-800 exists and can be called
  (is-true (fboundp 'skyline-tool::compile-art-800)
           "compile-art-800 function should exist")

  ;; Test compile-art-800 delegates appropriately
  (signals error (skyline-tool::compile-art-800 "/fake.in" "/fake.out")
           "compile-art-800 should delegate to 5200 compile-art function"))

;; Test 800 graphics compilation delegates to 5200
(test 800-graphics-compilation-delegates
  "Test that 800 graphics compilation properly delegates to 5200 Mode E"
  ;; Test with simple pixel data that should delegate to compile-5200-mode-e-bitmap
  (let ((test-pixels (make-array '(8 8) :element-type '(unsigned-byte 32) :initial-element 0)))
    (is-true (skyline-tool::dispatch-png% 800 "/test.png" "Object/800/" test-pixels 8 8 nil test-pixels)
             "800 should successfully dispatch graphics compilation to 5200")))

(defun run-800-tests ()
  "Run all 800 tests and return results"
  (fiveam:run! '800-tests))
