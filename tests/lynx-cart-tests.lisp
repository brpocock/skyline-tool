;;;; Phantasia SkylineTool/tests/lynx-cart-tests.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC.
;;;;
;;;; Focused regression tests for Atari Lynx (machine 200) cartridge
;;;; pipeline support added against Phantasia issue #1323.  These cover
;;;; the three concrete gaps that block Phantasia/Lynx Phase 1 builds:
;;;;
;;;;   1. ASSET->OBJECT-NAME must accept @code{*MACHINE*} = 200 (Lynx).
;;;;   2. WRITE-MAKEFILE-TOP-LINE must accept @code{*MACHINE*} = 200.
;;;;   3. WRITE-CART-HEADER for @code{*MACHINE*} = 200 must emit a
;;;;      structurally valid 64-byte LNX header (K. Wilkins / Handy
;;;;      format, also accepted by mednafen and No-Intro), with proper
;;;;      little-endian @var{page_size_bank0} chosen from raw ROM size.
;;;;
;;;; Tests are written in TDD style: they fail on origin/main and pass
;;;; only once the implementation in @file{src/asset-allocator.lisp} and
;;;; @file{src/misc.lisp} has been corrected.

(in-package :skyline-tool/test)

(def-suite lynx-cart-tests
  :description "Phantasia issue #1323 — Lynx (machine 200) Makefile + LNX header support."
  :in skyline-tool/test)

(in-suite lynx-cart-tests)

;;; ---------------------------------------------------------------------------
;;; Test helpers
;;; ---------------------------------------------------------------------------

(defun %lynx-tmp-binary (size-bytes &key (fill #x42))
  "Create a temp file of SIZE-BYTES filled with FILL, return its pathname.

@table @asis
@item SIZE-BYTES
Total size in bytes of the synthetic ROM image to create.
@item FILL
Byte value (default @code{#x42}) used to fill the entire file.
@item Return
@code{pathname} of the freshly created file.  Caller is responsible for
deleting it.
@end table"
  (let* ((tmp (uiop:temporary-directory))
         (path (merge-pathnames (format nil "phantasia-lynx-~16,'0x.bin"
                                        (random (expt 2 60)))
                                tmp)))
    (with-open-file (out path :element-type '(unsigned-byte 8)
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
      (loop repeat size-bytes do (write-byte fill out)))
    path))

(defun %lynx-read-bytes (pathname count &key (offset 0))
  "Read COUNT bytes starting at OFFSET from PATHNAME.

@table @asis
@item PATHNAME
File to read from.
@item COUNT
Number of bytes to read after seeking to @var{OFFSET}.
@item OFFSET
Byte offset from start of file (default 0).
@item Return
@code{simple-array (unsigned-byte 8)} of length @var{COUNT}.
@end table"
  (with-open-file (in pathname :element-type '(unsigned-byte 8))
    (file-position in offset)
    (let ((buf (make-array count :element-type '(unsigned-byte 8) :initial-element 0)))
      (read-sequence buf in)
      buf)))

(defmacro with-lynx-machine (&body body)
  "Bind @code{skyline-tool::*MACHINE*} to 200 (Lynx) for BODY.

Also binds @code{skyline-tool::*GAME-TITLE*} and
@code{skyline-tool::*STUDIO*} to test-friendly defaults so the
@samp{cart_name} / @samp{manuf_name} sections of the LNX header are
deterministic."
  `(let ((skyline-tool::*machine* 200)
         (skyline-tool::*game-title* "TestCart")
         (skyline-tool::*studio* "TestStudio"))
     ,@body))

;;; ---------------------------------------------------------------------------
;;; ASSET->OBJECT-NAME — must accept *MACHINE* = 200 (Lynx).
;;; ---------------------------------------------------------------------------

(test lynx-asset->object-name-blobs
  "ASSET->OBJECT-NAME with *MACHINE*=200 must succeed for a Blobs asset.

Regression target: ECASE failure observed in
@file{src/asset-allocator.lisp:asset->object-name} when bin/skyline-tool
processes Blobs/Lynx/AtariAge for the Lynx port (issue #1323)."
  (with-lynx-machine
    (let ((result (skyline-tool::asset->object-name "Blobs/TitleCard")))
      (is (stringp result)
          "asset->object-name should return a string for Blobs on machine 200")
      (is (search "Blob" result)
          "Blobs/TitleCard target path should mention 'Blob' (got ~s)" result))))

(test lynx-asset->object-name-scripts
  "ASSET->OBJECT-NAME with *MACHINE*=200 must succeed for a Scripts asset."
  (with-lynx-machine
    (let ((result (skyline-tool::asset->object-name "Scripts/Title")))
      (is (stringp result)
          "asset->object-name should return a string for Scripts on machine 200")
      (is (search "Script" result)
          "Scripts/Title target path should mention 'Script' (got ~s)" result))))

(test lynx-asset->object-name-songs
  "ASSET->OBJECT-NAME with *MACHINE*=200 must succeed for a Songs asset.

Lynx audio is supplied by Mikey rather than POKEY/TIA, but the asset
indexer still produces Songs/* entries for portable index files."
  (with-lynx-machine
    (let ((result (skyline-tool::asset->object-name "Songs/Title")))
      (is (stringp result)
          "asset->object-name should return a string for Songs on machine 200"))))

;;; ---------------------------------------------------------------------------
;;; WRITE-MAKEFILE-TOP-LINE — must accept *MACHINE* = 200.
;;; ---------------------------------------------------------------------------

(test lynx-write-makefile-top-line-no-error
  "WRITE-MAKEFILE-TOP-LINE must not signal ECASE failure for *MACHINE*=200.

Per issue #1323, write-master-makefile-for-machine for Lynx calls
write-makefile-top-line, which lacks a (200 ...) clause on origin/main."
  (with-lynx-machine
    (let ((output (with-output-to-string (*standard-output*)
                    (skyline-tool::write-makefile-top-line :build "Public"))))
      (is (stringp output)
          "write-makefile-top-line should produce string output for Lynx"))))

;;; ---------------------------------------------------------------------------
;;; WRITE-CART-HEADER — LNX format conformance.
;;; ---------------------------------------------------------------------------
;;;
;;; LNX header layout (K. Wilkins, Handy / No-Intro), 64 bytes total:
;;;
;;;   Offset  Size  Field
;;;   ------  ----  -------------------------------
;;;   0x00    4     Magic bytes "LYNX"
;;;   0x04    2     page_size_bank0 (LE)
;;;   0x06    2     page_size_bank1 (LE)
;;;   0x08    2     version (LE, must be 1)
;;;   0x0A    32    cart_name (NUL-padded)
;;;   0x2A    16    manuf_name (NUL-padded)
;;;   0x3A    1     rotation (0 = normal, 2 = vertical)
;;;   0x3B    5     spare (zero)
;;;
;;; Page-size mapping for single-bank carts (bank0 only): page = size / 256.
;;;
;;;   ROM size (KiB)   page_size_bank0 (decimal / hex)
;;;   --------------   --------------------------------
;;;        64           256  / 0x0100
;;;       128           512  / 0x0200
;;;       256          1024  / 0x0400
;;;       512          2048  / 0x0800
;;;      1024          4096  / 0x1000

(defparameter +lynx-header-bytes+ 64
  "Number of bytes in the LNX cartridge header (Handy/No-Intro format).")

(defparameter +lynx-rom-size-bytes-for-tests+ (* 512 1024)
  "ROM size used by the LNX header tests below: 512 KiB, the Phantasia
target cartridge size for the Lynx port (#1321 / Phase 1 #1322).")

(test lynx-write-cart-header-magic-lynx
  "First 4 bytes of the LNX header must be ASCII \"LYNX\"."
  (with-lynx-machine
    (let* ((bin (%lynx-tmp-binary +lynx-rom-size-bytes-for-tests+))
           (lnx (uiop:make-pathname* :defaults bin :type "lnx")))
      (unwind-protect
           (progn
             (skyline-tool::write-cart-header lnx bin)
             (let ((magic (%lynx-read-bytes lnx 4 :offset 0)))
               (is (equalp magic #(#x4C #x59 #x4E #x58)) ; "LYNX"
                   "LNX magic at offset 0..3 should be ASCII LYNX, got ~s"
                   (map 'string #'code-char magic))))
        (when (probe-file bin) (delete-file bin))
        (when (probe-file lnx) (delete-file lnx))))))

(test lynx-write-cart-header-bank0-page-size-512k
  "page_size_bank0 (offsets 4-5, LE word) must be 2048 for a 512 KiB ROM.

Computed as ROM-size-bytes / 256.  Without this, mednafen/Handy may
silently load with the wrong banking and produce mis-paged code."
  (with-lynx-machine
    (let* ((bin (%lynx-tmp-binary +lynx-rom-size-bytes-for-tests+))
           (lnx (uiop:make-pathname* :defaults bin :type "lnx")))
      (unwind-protect
           (progn
             (skyline-tool::write-cart-header lnx bin)
             (let ((page (%lynx-read-bytes lnx 2 :offset 4)))
               (is (= (aref page 0) #x00)
                   "page_size_bank0 LE low byte (offset 4) should be #x00 for 2048, got #x~2,'0X"
                   (aref page 0))
               (is (= (aref page 1) #x08)
                   "page_size_bank0 LE high byte (offset 5) should be #x08 for 2048, got #x~2,'0X"
                   (aref page 1))))
        (when (probe-file bin) (delete-file bin))
        (when (probe-file lnx) (delete-file lnx))))))

(test lynx-write-cart-header-bank1-page-size-zero
  "page_size_bank1 (offsets 6-7, LE word) must be 0 for a single-bank cart."
  (with-lynx-machine
    (let* ((bin (%lynx-tmp-binary +lynx-rom-size-bytes-for-tests+))
           (lnx (uiop:make-pathname* :defaults bin :type "lnx")))
      (unwind-protect
           (progn
             (skyline-tool::write-cart-header lnx bin)
             (let ((page (%lynx-read-bytes lnx 2 :offset 6)))
               (is (zerop (aref page 0))
                   "page_size_bank1 LE low byte (offset 6) should be 0 for single-bank cart, got #x~2,'0X"
                   (aref page 0))
               (is (zerop (aref page 1))
                   "page_size_bank1 LE high byte (offset 7) should be 0, got #x~2,'0X"
                   (aref page 1))))
        (when (probe-file bin) (delete-file bin))
        (when (probe-file lnx) (delete-file lnx))))))

(test lynx-write-cart-header-version
  "version (offsets 8-9, LE word) must be 1."
  (with-lynx-machine
    (let* ((bin (%lynx-tmp-binary +lynx-rom-size-bytes-for-tests+))
           (lnx (uiop:make-pathname* :defaults bin :type "lnx")))
      (unwind-protect
           (progn
             (skyline-tool::write-cart-header lnx bin)
             (let ((ver (%lynx-read-bytes lnx 2 :offset 8)))
               (is (= 1 (aref ver 0))
                   "version LE low byte (offset 8) should be #x01, got #x~2,'0X"
                   (aref ver 0))
               (is (zerop (aref ver 1))
                   "version LE high byte (offset 9) should be #x00, got #x~2,'0X"
                   (aref ver 1))))
        (when (probe-file bin) (delete-file bin))
        (when (probe-file lnx) (delete-file lnx))))))

(test lynx-write-cart-header-cart-name
  "cart_name (offsets 10..41, 32 bytes) must encode *GAME-TITLE* as ASCII."
  (with-lynx-machine
    (let* ((bin (%lynx-tmp-binary +lynx-rom-size-bytes-for-tests+))
           (lnx (uiop:make-pathname* :defaults bin :type "lnx")))
      (unwind-protect
           (progn
             (skyline-tool::write-cart-header lnx bin)
             (let* ((bytes (%lynx-read-bytes lnx 32 :offset 10))
                    (decoded (with-output-to-string (out)
                               (loop for b across bytes
                                     while (plusp b)
                                     do (write-char (code-char b) out)))))
               (is (equal decoded "TestCart")
                   "cart_name should decode to *GAME-TITLE*; got ~s" decoded)))
        (when (probe-file bin) (delete-file bin))
        (when (probe-file lnx) (delete-file lnx))))))

(test lynx-write-cart-header-rotation
  "rotation (offset 0x3A) must be 0 (normal) for the Phantasia Lynx port."
  (with-lynx-machine
    (let* ((bin (%lynx-tmp-binary +lynx-rom-size-bytes-for-tests+))
           (lnx (uiop:make-pathname* :defaults bin :type "lnx")))
      (unwind-protect
           (progn
             (skyline-tool::write-cart-header lnx bin)
             (let ((rot (aref (%lynx-read-bytes lnx 1 :offset #x3A) 0)))
               (is (zerop rot)
                   "rotation byte (offset 0x3A) should be 0 (no rotation), got #x~2,'0X"
                   rot)))
        (when (probe-file bin) (delete-file bin))
        (when (probe-file lnx) (delete-file lnx))))))

(test lynx-write-cart-header-total-size
  "Output @file{.lnx} size must equal 64-byte header + raw ROM size."
  (with-lynx-machine
    (let* ((bin (%lynx-tmp-binary +lynx-rom-size-bytes-for-tests+))
           (lnx (uiop:make-pathname* :defaults bin :type "lnx")))
      (unwind-protect
           (progn
             (skyline-tool::write-cart-header lnx bin)
             (with-open-file (s lnx :element-type '(unsigned-byte 8))
               (is (= (file-length s)
                      (+ +lynx-header-bytes+ +lynx-rom-size-bytes-for-tests+))
                   "LNX file should be header + ROM bytes; got ~D, expected ~D"
                   (file-length s)
                   (+ +lynx-header-bytes+ +lynx-rom-size-bytes-for-tests+))))
        (when (probe-file bin) (delete-file bin))
        (when (probe-file lnx) (delete-file lnx))))))

(test lynx-write-cart-header-page-size-table
  "page_size_bank0 must scale with ROM size: page = ROM-size / 256.

Validates the full small/medium/large cart matrix used by Handy.  Only
power-of-two cart sizes between 64 KiB and 1 MiB are exercised; these
match the supported homebrew cart capacities documented at
https://atarilynxvault.com/pages/atari-lynx-cartridge-reader-writer-board-software."
  (with-lynx-machine
    (dolist (size-and-expected
             '(( #x10000 . #x0100)   ;  64 KiB → 256
               ( #x20000 . #x0200)   ; 128 KiB → 512
               ( #x40000 . #x0400)   ; 256 KiB → 1024
               ( #x80000 . #x0800))) ; 512 KiB → 2048
      (destructuring-bind (size . expected-page) size-and-expected
        (let* ((bin (%lynx-tmp-binary size))
               (lnx (uiop:make-pathname* :defaults bin :type "lnx")))
          (unwind-protect
               (progn
                 (skyline-tool::write-cart-header lnx bin)
                 (let* ((bytes (%lynx-read-bytes lnx 2 :offset 4))
                        (got (logior (aref bytes 0) (ash (aref bytes 1) 8))))
                   (is (= got expected-page)
                       "ROM size #x~6,'0X: page_size_bank0 should be #x~4,'0X, got #x~4,'0X"
                       size expected-page got)))
            (when (probe-file bin) (delete-file bin))
            (when (probe-file lnx) (delete-file lnx))))))))
