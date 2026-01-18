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
  (:export #:*test-file*
           #:make-test-stamp))

(defparameter *test-file*
  (let* ((platform-dir (if (and (boundp 'skyline-tool:*machine*)
                                skyline-tool:*machine*)
                           (skyline-tool::machine-directory-name)
                           "test"))
        ;; Use cryptographically secure random identifier
        (random-id (format nil "~16,'0x"
                          (or (ignore-errors
                                (with-open-file (urandom "/dev/urandom" :element-type '(unsigned-byte 8))
                                  (let ((bytes (make-array 8 :element-type '(unsigned-byte 8))))
                                    (read-sequence bytes urandom)
                                    (loop for i from 0 below 8
                                          sum (ash (aref bytes i) (* i 8))))))
                              (random (expt 2 64)))))
         (path (format nil "Object/~a/tmpnam-~a.bin" platform-dir random-id)))
    (ensure-directories-exist path)
    path)
  "Default test file path for file I/O tests")

(defun make-test-stamp (width height pattern)
  "Create a test graphics stamp (2D array) with the specified pattern.
Width and height specify dimensions, pattern can be:
:solid-0 - all zeros
:solid-1 - all ones
:checkerboard - alternating 0s and 1s
:horizontal-bars - alternating rows of 0s and 1s"
  (let ((stamp (make-array (list width height) :element-type '(unsigned-byte 8) :initial-element 0)))
    (ecase pattern
      (:solid-0
       ;; Already initialized to 0
       )
      (:solid-1
       (dotimes (x width)
         (dotimes (y height)
           (setf (aref stamp x y) 1))))
      (:checkerboard
       (dotimes (x width)
         (dotimes (y height)
           (setf (aref stamp x y) (if (evenp (+ x y)) 0 1)))))
      (:horizontal-bars
       (dotimes (x width)
         (dotimes (y height)
           (setf (aref stamp x y) (if (evenp y) 0 1))))))
    stamp))
