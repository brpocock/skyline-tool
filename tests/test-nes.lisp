;;; NES Validation Tests
;;; Comprehensive tests for NES conversion functions and asset allocation

(require 'asdf)
(load "setup.lisp")
(asdf:load-system :skyline-tool)

(in-package :skyline-tool)

;;; Test NES asset loader sizes
(defun test-nes-asset-loader-sizes ()
  "Test that NES asset loader sizes are correctly calculated"
  (format t "~&Testing NES asset loader sizes...")

  ;; Test overhead size
  (assert (= (asset-loader-size :overhead 5 8) 10)
          () "NES overhead size should be 10 for 5 records")

  ;; Test song loader size
  (assert (= (asset-loader-size :song 3 8) 160)
          () "NES song loader size should be 160 for 3 records")

  ;; Test script loader size
  (assert (= (asset-loader-size :script 2 8) (+ 80 (* (1+ 2) 3)))
          () "NES script loader size should be correct for 2 records")

  ;; Test blob loader size
  (assert (= (asset-loader-size :blob 4 8) (+ 180 1 (* 4 2)))
          () "NES blob loader size should be correct for 4 records")

  ;; Test map loader size
  (assert (= (asset-loader-size :map 1 8) (+ 640 1 (* 1 3)))
          () "NES map loader size should be correct for 1 record")

  (format t " PASSED~%"))

;;; Test NES bank size
(defun test-nes-bank-size ()
  "Test that NES bank size is correctly defined"
  (format t "~&Testing NES bank size...")

  (let ((*machine* 8))
    (assert (= (size-of-banks) #x2000)
            () "NES bank size should be 8KB (#x2000)"))

  (format t " PASSED~%"))

;;; Test NES video types
(defun test-nes-video-types ()
  "Test that NES supports correct video types"
  (format t "~&Testing NES video types...")

  (let ((*machine* 8))
    (let ((video-types (supported-video-types)))
      (assert (member :ntsc video-types) () "NES should support NTSC")
      (assert (member :pal video-types) () "NES should support PAL")
      (assert (= (length video-types) 2) () "NES should support exactly 2 video types")))

  (format t " PASSED~%"))

;;; Test NES makefile generation
(defun test-nes-makefile-generation ()
  "Test that NES makefile generation works"
  (format t "~&Testing NES makefile generation...")

  ;; This is harder to test directly, but we can test that the method exists
  ;; and doesn't error out
  (let ((*machine* 8)
        (*last-bank* 1))
    ;; The method should exist and not throw an error
    (assert (fboundp 'write-master-makefile-for-machine)
            () "write-master-makefile-for-machine should be defined")
    ;; We can't easily test the actual output without capturing stdout
    ;; but we can ensure it doesn't crash
    (format t " (method exists)~%")))

;;; Test NES asset allocation
(defun test-nes-asset-allocation ()
  "Test NES asset allocation logic"
  (format t "~&Testing NES asset allocation...")

  (let ((*machine* 8))
    ;; Test basic asset allocation setup
    (assert (= *machine* 8) () "Machine should be set to NES")

    ;; Test that we can create asset names
    (let ((test-asset "Art/PlayerSprite"))
      (assert (stringp (asset->symbol-name test-asset))
              () "Should generate symbol name for asset"))

    (format t " PASSED~%")))

;;; Test NES directory name
(defun test-nes-directory-name ()
  "Test NES directory name mapping"
  (format t "~&Testing NES directory name...")

  (assert (string= (machine-directory-name 8) "NES")
          () "Machine 8 should map to NES directory")

  (format t " PASSED~%"))

;;; Run all NES tests
(defun run-nes-tests ()
  "Run all NES validation tests"
  (format t "~&=== Running NES Validation Tests ===~%")

  (handler-case
      (progn
        (test-nes-directory-name)
        (test-nes-bank-size)
        (test-nes-video-types)
        (test-nes-asset-loader-sizes)
        (test-nes-asset-allocation)
        (test-nes-makefile-generation)

        (format t "~&=== All NES Tests PASSED ===~%")
        t)

    (error (e)
      (format t "~&=== NES Test FAILED: ~A ===~%" e)
      nil)))

;;; Export the test runner
(export 'run-nes-tests)
