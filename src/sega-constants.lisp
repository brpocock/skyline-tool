(in-package :skyline-tool)

;;; Master System / SG-1000 hardware and palette constants used by tooling and
;;; sega-tests.lisp regression checks.

(defparameter *rom-start* #x0000)
(defparameter *rom-end* #xBFFF)
(defparameter *ram-start* #xC000)
(defparameter *ram-end* #xDFFF)
(defparameter *vram-start* #xE000)
(defparameter *vram-end* #xFFFF)

(defparameter *vdp-data* #xBE)
(defparameter *vdp-ctrl* #xBF)
(defparameter *psg-port* #x7F)
(defparameter *joy1-port* #xDC)
(defparameter *joy2-port* #xDD)

(defparameter *vdp-reg0* #x80)
(defparameter *vdp-reg1* #x81)
(defparameter *vdp-reg7* #x87)
(defparameter *vdp-vram-write* #x4000)
(defparameter *vdp-cram-write* #xC000)

(defparameter *psg-ch0-tone-l* #x80)
(defparameter *psg-ch0-vol* #x90)
(defparameter *psg-ch1-tone-l* #xA0)
(defparameter *psg-ch1-vol* #xB0)
(defparameter *psg-ch3-noise* #xE0)
(defparameter *psg-ch3-vol* #xF0)

(defparameter *black* #x00)
(defparameter *blue* #x02)
(defparameter *red* #x20)
(defparameter *white* #x2A)

(defparameter *joy-up* #x01)
(defparameter *joy-down* #x02)
(defparameter *joy-left* #x04)
(defparameter *joy-right* #x08)
(defparameter *joy-button1* #x10)
(defparameter *joy-button2* #x20)

(defparameter *screen-width* 256)
(defparameter *screen-height* 192)
(defparameter *tile-width* 8)
(defparameter *tile-height* 8)
(defparameter *tiles-per-row* 32)
(defparameter *tiles-per-col* 24)

(defparameter *rst-00h* #x0000)
(defparameter *rst-08h* #x0008)
(defparameter *rst-38h* #x0038)
(defparameter *nmi-vector* #x0066)
