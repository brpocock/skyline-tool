;;; Simple NES Tests
;;; Test just the NES-specific functions we added

(defpackage :skyline-tool
  (:use :cl))

(in-package :skyline-tool)

;; Load just the asset-allocator file
(load "src/asset-allocator.lisp")

;; Test NES asset loader sizes
(format t "~&Testing NES asset loader sizes...~%")

(assert (= (asset-loader-size :overhead 5 8) 10))
(format t "  Overhead size: PASS~%")

(assert (= (asset-loader-size :song 3 8) 160))
(format t "  Song loader size: PASS~%")

(assert (= (asset-loader-size :script 2 8) (+ 80 (* 3 3))))
(format t "  Script loader size: PASS~%")

(assert (= (asset-loader-size :blob 4 8) (+ 180 1 (* 4 2))))
(format t "  Blob loader size: PASS~%")

(assert (= (asset-loader-size :map 1 8) (+ 640 1 (* 1 3))))
(format t "  Map loader size: PASS~%")

;; Test NES bank size
(format t "~&Testing NES bank size...~%")
(let ((*machine* 8))
  (assert (= (size-of-banks) #x2000)))
(format t "  Bank size: PASS~%")

;; Test NES video types
(format t "~&Testing NES video types...~%")
(let ((*machine* 8))
  (let ((video-types (supported-video-types)))
    (assert (member :ntsc video-types))
    (assert (member :pal video-types))
    (assert (= (length video-types) 2))))
(format t "  Video types: PASS~%")

;; Test NES directory name
(format t "~&Testing NES directory name...~%")
(assert (string= (machine-directory-name 8) "NES"))
(format t "  Directory name: PASS~%")

(format t "~&All NES tests PASSED!~%")
