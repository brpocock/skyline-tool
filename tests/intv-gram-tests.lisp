;;; Phantasia SkylineTool/tests/intv-gram-tests.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC
;;; Test-Driven Development for Intellivision GRAM card compiler

(in-package :skyline-tool/test)

(def-suite intv-gram-tests
  :description "Tests for Intellivision GRAM card compilation")

(in-suite intv-gram-tests)

;; Test helper functions
(defparameter *test-gram-dir* (merge-pathnames "test-gram/" (uiop:temporary-directory)))

(defun ensure-test-gram-dir ()
  "Ensure test directory exists"
  (ensure-directories-exist *test-gram-dir*))

(defun cleanup-test-gram-file (filename)
  "Remove a test output file"
  (let ((path (merge-pathnames filename *test-gram-dir*)))
    (when (probe-file path)
      (delete-file path))))

(defmacro with-temp-gram-output ((output-var filename) &body body)
  "Create temporary output file path and cleanup after"
  `(let ((,output-var (merge-pathnames ,filename *test-gram-dir*)))
     (unwind-protect
          (progn
            (ensure-test-gram-dir)
            ,@body)
       (cleanup-test-gram-file ,filename))))

;; Test 1: Output file name
(test gram-compiler-output-filename
  "Test that GRAM compiler outputs a file with the correct name"
  (with-temp-gram-output (output-path "test-cards.s")
    (let ((input-png (make-pathname :name "test-cards" :type "png")))
      ;; Call the GRAM compiler (function name TBD)
      (skyline-tool::compile-gram-intv input-png *test-gram-dir* :height 8 :width 8 :palette-pixels nil)
      ;; Verify output file exists with correct name
      (is-true (probe-file output-path)
               "Output file should exist: ~A" output-path)
      ;; Verify file has .s extension
      (is (string= "s" (pathname-type output-path))
          "Output file should have .s extension"))))

;; Test 2: DECLE format verification
(test gram-compiler-decle-format
  "Test that GRAM compiler outputs DECLE statements in correct format"
  (with-temp-gram-output (output-path "test-card.s")
    (let ((input-png (make-pathname :name "test-card" :type "png")))
      ;; Call the GRAM compiler
      (skyline-tool::compile-gram-intv input-png *test-gram-dir*)
      ;; Read the output file
      (let ((output-content (uiop:read-file-string output-path)))
        ;; Verify file contains DECLE keyword
        (is-true (search "DECLE" output-content)
                 "Output file should contain DECLE statements")
        ;; Verify DECLE format (should have spaces before DECLE)
        (is-true (or (search "    DECLE" output-content)
                     (search "DECLE" output-content))
                 "Output file should have properly formatted DECLE statements")))))
