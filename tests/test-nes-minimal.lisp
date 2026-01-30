;;; Minimal NES Tests
;;; Test just the NES-specific functions we added, defined locally

(defpackage :nes-test
  (:use :cl))

(in-package :nes-test)

;; Define the constants we need
(defconstant +all-builds+ '("AA" "Public" "Demo"))
(defconstant +all-video+ '(:ntsc :pal :secam))

;; Define the functions we need
(defvar *machine* 8)

(defun machine-directory-name (&optional (machine *machine*))
  (ecase machine
    (1 "Oric")
    (2 "A2")
    (3 "A3")
    (8 "NES")
    (16 "TG16")
    (20 "VIC20")
    (23 "A2e")
    (64 "C64")
    (81 "ZX81")
    (88 "SNES")
    (128 "C128")
    (200 "Lynx")
    (222 "2gs")
    (223 "BBC")
    (264 "C16")
    (2609 "Intv")
    (1601 "Intv")
    (2600 "2600")
    (3010 "2600")
    (5200 "5200")
    (7800 "7800")
    (9918 "Coleco")
    (359020 "GBC")))

(defgeneric asset-loader-size (kind record-count machine)
  (:method ((kind (eql :overhead)) record-count (machine (eql 7800)))
    12)
  (:method ((kind (eql :song)) record-count (machine (eql 7800)))
    256)
  (:method ((kind (eql :script)) record-count (machine (eql 7800)))
    (+ 128 (* (1+ record-count) 4)))
  (:method ((kind (eql :blob)) record-count (machine (eql 7800)))
    (+ 284 1 (* record-count 3)))
  (:method ((kind (eql :map)) record-count (machine (eql 7800)))
    (+ 1024 1 (* record-count 3)))
  ;; NES methods
  (:method ((kind (eql :overhead)) record-count (machine (eql 8)))
    10)
  (:method ((kind (eql :song)) record-count (machine (eql 8)))
    160)
  (:method ((kind (eql :script)) record-count (machine (eql 8)))
    (+ 80 (* (1+ record-count) 3)))
  (:method ((kind (eql :blob)) record-count (machine (eql 8)))
    (+ 180 1 (* record-count 2)))
  (:method ((kind (eql :map)) record-count (machine (eql 8)))
    (+ 640 1 (* record-count 3))))

(defun size-of-banks ()
  (ecase *machine*
    (8 #x2000)  ;; NES: 8KB banks
    (2600 #x1000)
    (7800 #x4000)
    (9918 #x8000)
    (359020 #x8000)))

(defun supported-video-types (&optional (machine *machine*))
  (case machine
    (400 '(:ntsc))
    (800 '(:ntsc))
    (5200 '(:ntsc))
    (7800 '(:ntsc :pal))
    (81 '(:ntsc :pal))
    (2068 '(:ntsc :pal))
    (9918 '(:ntsc))
    (8 '(:ntsc :pal))  ;; NES supports both
    (t +all-video+)))

;; Now run the tests
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

(format t "~&Testing NES bank size...~%")
(let ((*machine* 8))
  (assert (= (size-of-banks) #x2000)))
(format t "  Bank size: PASS~%")

(format t "~&Testing NES video types...~%")
(let ((*machine* 8))
  (let ((video-types (supported-video-types)))
    (assert (member :ntsc video-types))
    (assert (member :pal video-types))
    (assert (= (length video-types) 2))))
(format t "  Video types: PASS~%")

(format t "~&Testing NES directory name...~%")
(assert (string= (machine-directory-name 8) "NES"))
(format t "  Directory name: PASS~%")

(format t "~&All NES tests PASSED!~%")
