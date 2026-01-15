;;; Phantasia SkylineTool/tests/package.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

(defpackage :skyline-tool/test
  (:use :cl :skyline-tool :fiveam)
  (:export #:action-tests
           #:animation-preview-tests
           #:graphics-tests
           #:build-tests
           #:interface-tests
           #:makefile-tests
           #:music-tests
           #:intv-asset-converters
           #:intv-card-layouts
           #:intv-gram-tests
           #:run-intv-gram-tests
           #:zx81-tests
           #:spectrum-tests)

  ;; Define variables that were referenced in removed test files
  (:export #:*hex-start*
           #:*test-file*
           #:make-test-stamp))

;; Define the variables with default values
(defparameter *hex-start* #x1000
  "Default starting address for hex output in tests")

(defparameter *test-file*
  (let* ((platform-dir (if (and (boundp 'skyline-tool:*machine*)
                                skyline-tool:*machine*)
                           (skyline-tool::machine-directory-name)
                           "test"))
         (path (format nil "Object/~a/tmpnam-~x.bin" platform-dir (sxhash (get-universal-time)))))
    (ensure-directories-exist path)
    path)
  "Default test file path for file I/O tests")

(defun make-test-stamp (prefix)
  "Create a test timestamp string for test identification"
  (format nil "~a-~a" prefix (get-universal-time)))
