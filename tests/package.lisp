;;; Phantasia SkylineTool/tests/package.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC.

(defpackage :skyline-tool/test
  (:use :cl :skyline-tool :fiveam)
  (:export #:run-all-tests
           ;; Test suite names
           #:action-tests
           #:display-list-tests
           #:text-transcription-tests
           #:animation-preview-tests
           #:graphics-tests
           #:music-compilation-tests
           #:build-tests
           #:interface-tests
           #:5200-tests
           #:7800-tests
           #:colecovision-tests
           #:intv-gram-tests
           #:nes-tests
           #:snes-tests))

(in-package :skyline-tool/test)
