(defpackage :7800gd-debug (:use :common-lisp :alexandria)
            (:shadowing-import-from :sb-ext :octets-to-string)
            (:export
             #:cmd-break
             #:cmd-execute
             #:cmd-init
             #:cmd-return
             #:cmd-simple
             #:cmd-status
             #:cmd-term
             #:cmd-write-cart
             #:cmd-write-data
             #:cmd-write-data-complete
             #:cmd-status
             #:upload
             #:with-open-7800gd-port))

(in-package :7800gd-debug)

(defconstant +cmd-status+ 0)
(defconstant +cmd-break+ 1)
(defconstant +cmd-return+ 2)
(defconstant +cmd-write-cart+ 3)
(defconstant +cmd-execute+ 4)

(define-constant +command-names+ '(status break return write-cart execute) :test 'equalp)

(defconstant +a78-type-v3-pokey-800+ (ash 1 15))
(defconstant +a78-type-v3-exram-m2+ (ash 1 14))
(defconstant +a78-type-v3-bankset+ (ash 1 13))

(defconstant +a78-type-v3-irq-pokey-800+ (ash 1 4))
(defconstant +a78-type-v3-irq-ym2151-460+ (ash 1 3))
(defconstant +a78-type-v3-irq-pokey-440+ (ash 1 2))
(defconstant +a78-type-v3-irq-pokey-450+ (ash 1 1))
(defconstant +a78-type-v3-irq-pokey-4000+ (ash 1 0))

(defconstant +a78-type-pokey-4000+ (ash 1 0))
(defconstant +a78-type-supergame-banking+ (ash 1 1))
(defconstant +a78-type-exram-4000+ (ash 1 2))
(defconstant +a78-type-rom-4000+ (ash 1 3))
(defconstant +a78-type-bank6-4000+ (ash 1 4))
(defconstant +a78-type-exram-x2+ (ash 1 5))
(defconstant +a78-type-pokey-450+ (ash 1 6))
(defconstant +a78-type-exram-a8+ (ash 1 7))
(defconstant +a78-type-activision-banking+ (ash 1 8))
(defconstant +a78-type-absolute-banking+ (ash 1 9))
(defconstant +a78-type-pokey-440+ (ash 1 10))
(defconstant +a78-type-ym2151-460+ (ash 1 11))
(defconstant +a78-type-souper+ (ash 1 12))

(defconstant +a78-tv-region+ (ash 1 0))
(defconstant +a78-tv-region-ntsc+ 0)
(defconstant +a78-tv-region-pal+ 1)
(defconstant +a78-tv-composite+ (ash 1 1))

(defconstant +a78-save-hsc+ (ash 1 0))
(defconstant +a78-save-savekey+ (ash 1 1))
(defconstant +a78-video-hint-composite+ (ash 1 1))

(defconstant +a78-controller-atarivox-savekey 10)
(defconstant +a78-controller-mega7800 12)

(defconstant +ea78-v4-mapper-linear+ 0)
(defconstant +ea78-v4-mapper-supergame+ 1)
(defconstant +ea78-v4-mapper-activision+ 2)
(defconstant +ea78-v4-mapper-absolute+ 3)
(defconstant +ea78-v4-mapper-souper+ 4)

(defconstant +ea78-v4-mapper-linear-bankset+ (ash 1 7))

(defconstant +ea78-v4-mapper-linear-4kopt-none+ 0)
(defconstant +ea78-v4-mapper-linear-4kopt-ram+ 1)
(defconstant +ea78-v4-mapper-linear-4kopt-exram-a8+ 2)
(defconstant +ea78-v4-mapper-linear-4kopt-exram-m2+ 3)
(defconstant +ea78-v4-mapper-linear-4kopt-mask+ 3)

(defconstant +ea78-v4-mapper-supergame-bankset+ (ash 1 7))

(defconstant +ea78-v4-mapper-supergame-4kopt-none+ 0)
(defconstant +ea78-v4-mapper-supergame-4kopt-ram+ 1)
(defconstant +ea78-v4-mapper-supergame-4kopt-exram-a8+ 2)
(defconstant +ea78-v4-mapper-supergame-4kopt-exram-m2+ 3)
(defconstant +ea78-v4-mapper-supergame-4kopt-exrom+ 4)
(defconstant +ea78-v4-mapper-supergame-4kopt-exfix+ 5)
(defconstant +ea78-v4-mapper-supergame-4kopt-exram-x2+ 6)
(defconstant +ea78-v4-mapper-supergame-4kopt-mask+ 7)

(defconstant +ea78-v4-audio-stream+ (ash 1 5))
(defconstant +ea78-v4-audio-covox+ (ash 1 4))
(defconstant +ea78-v4-audio-ym2151+ (ash 1 3))

(defconstant +ea78-v4-audio-pokey-none+ 0)
(defconstant +ea78-v4-audio-pokey-440+ 1)
(defconstant +ea78-v4-audio-pokey-450+ 2)
(defconstant +ea78-v4-audio-pokey-440-450+ 3)
(defconstant +ea78-v4-audio-pokey-800+ 4)
(defconstant +ea78-v4-audio-pokey-4000+ 5)
(defconstant +ea78-v4-audio-pokey-mask+ 7)

(defconstant +ea78-v4-irq-enable-pokey-1+ (ash 1 0))
(defconstant +ea78-v4-irq-enable-pokey-2+ (ash 1 1))
(defconstant +ea78-v4-irq-enable-ym2151+ (ash 1 2))

(defun cmd-send (port command)
  "Start command and send command byte"
  (check-type port stream)
  (check-type command (unsigned-byte 8))
  (and (com-break port)
       (com-write port (vector command) 1)))

(defun cmd-simple (port command &optional (data nil) (size 0))
  "Send command packet including parameters if required"
  (check-type port stream)
  (check-type command (unsigned-byte 8))
  (check-type data (or null vector))
  (check-type size (integer 0 *))
  (assert (or (zerop size) (and data (<= size (length data)))))
  (and (cmd-send port command)
       (if (plusp size)
           (com-write port data size)
           t)
       (let ((e (com-read port 1)))
         (if (equalp (vector 0) e)
             t
             (error "Error code from port ~a: ~s" port e)))))

(defun cmd-init (port-pathname)
  "Initialize 7800GD serial interface"
  (check-type port-pathname (or pathname string))
  (com-open port-pathname 500000))

(defun cmd-term (port)
  "Terminate connection"
  (check-type port stream)
  (com-close port))

(defun cmd-status (port)
  "Request status"
  (check-type port stream)
  (format *trace-output* "~&~5t• Requesting status")
  (unless (cmd-send port +cmd-status+)
    (error "Can't send request for status"))
  (let ((n (elt (com-read port 1) 0)))
    (format *trace-output* " … status is “~a”"
            (case n
              (0 "Running")
              (1 "Stopped")
              (2 "In Menu")
              (otherwise (error "Undefined status code $~x" n))))
    n))

(defun cmd-break (port)
  "Break currently executing program"
  (check-type port stream)
  (cmd-simple port +cmd-break+))

(defun cmd-return (port)
  "Return (resume) from break"
  (check-type port stream)
  (cmd-simple port +cmd-return+))

(defun write-param (&rest words)
  "Writes a series of 32-bit WORDS to a new vector"
  (assert (every (rcurry #'typep '(unsigned-byte 32)) words))
  (loop with param = (make-array (list (* 4 (length words))) :element-type '(unsigned-byte 8))
        for word in words
        for i from 0 by 4
        do (setf (aref param (+ i)) (ldb (byte 8 0) word)
                 (aref param (+ 1 i)) (ldb (byte 8 8) word)
                 (aref param (+ 2 i)) (ldb (byte 8 16) word)
                 (aref param (+ 3 i)) (ldb (byte 8 24) word))
        finally (return (values param (array-dimension param 0)))))

(defun cmd-write-cart (port address size)
  "Write to cartridge memory space"
  (check-type address (integer 0 (#.(expt 2 32))))
  (check-type size (integer 0 #.(expt 2 32)))
  (assert (<= (+ address size) (expt 2 32)))
  (multiple-value-bind (param length) (write-param address size)
    (cmd-simple port +cmd-write-cart+ param length)))

(defun cmd-write-data (port data size)
  "Write raw data to cart, used for data upload"
  (check-type data vector)
  (check-type size (integer 0 (#.(expt 2 32))))
  (assert (<= size (length data)))
  (com-write port data size))

(defun cmd-write-data-complete (port)
  "Check for write data completion"
  (or (equalp #(0) (com-read port 1))
      (error "Write data not completed")))

(defun cmd-execute (port &key
                           (mapper +ea78-v4-mapper-supergame+)
                           (mapper-options 0)
                           (mapper-audio +ea78-v4-audio-pokey-450+)
                           (mapper-irq-enable 0)
                           (size 0)
                           (extra-flags 0))
  "Execute ROM (reboot) with given mapper"
  (check-type mapper (integer 0 (#.(expt 2 8))))
  (check-type mapper-options (integer 0 (#.(expt 2 8))))
  (check-type mapper-audio (integer 0 (#.(expt 2 16))))
  (check-type mapper-irq-enable (integer 0 (#.(expt 2 16))))
  (check-type size (integer 0 (#.(expt 2 32))))
  (check-type extra-flags (integer 0 (#.(expt 2 16))))
  (let ((param (make-array '(12) :element-type '(unsigned-byte))))
    (setf (aref param 0) mapper
          (aref param 1) mapper-options
          (aref param 2) (ldb (byte 8 0) mapper-audio)
          (aref param 3) (ldb (byte 8 8) mapper-audio)
          (aref param 4) (ldb (byte 8 0) mapper-irq-enable)
          (aref param 5) (ldb (byte 8 8) mapper-irq-enable)
          (aref param 6) (ldb (byte 8 0) size)
          (aref param 7) (ldb (byte 8 8) size)
          (aref param 8) (ldb (byte 8 16) size)
          (aref param 9) (ldb (byte 8 24) size)
          (aref param 10) (ldb (byte 8 0) extra-flags)
          (aref param 11) (ldb (byte 8 8) extra-flags))
    (cmd-simple port +cmd-execute+ param 12)))

(defun upload (port file offset load-address size)
  (check-type port stream)
  (check-type file stream)
  (check-type offset (integer 0 (#.(expt 2 32))))
  (check-type load-address (integer 0 (#.(expt 2 32))))
  (check-type size (integer 0 (#.(expt 2 32))))
  (unless (cmd-write-cart port load-address size)
    (error "Unable to write data"))
  (file-position file offset)
  (format t "Writing ~:dkiB to $~5,'0x: " (round (/ size 1024)) load-address)
  (loop with left = size
        for blocks-to-read = (min 4096 left)
        with blocks-sent = 0
        while (plusp left)
        do (let ((buf (read-stream-content-into-byte-vector
                       file
                       'alexandria::%length blocks-to-read)))
             (princ "·")
             (unless (cmd-write-data port buf blocks-to-read)
               (error "Error while sending data from file +$~x to buffer $~x"
                      (+ offset blocks-sent) (+ load-address blocks-sent)))
             (incf blocks-sent blocks-to-read)
             (decf left blocks-to-read)
             (princ #\Backspace)
             (princ "•"))
        finally (progn
                  (cmd-write-data-complete port)
                  (format t " OK")
                  (return t))))

(defstruct mapper-info
  mapper options)

(defun banksetp (mapper-info)
  (check-type mapper-info mapper-info)
  (and (member (mapper-info-mapper mapper-info)
               '(+EA78-V4-mapper-linear+ +EA74-v4-mapper-supergame+))
       (plusp (logand (mapper-info-options mapper-info)
                      +EA78-v4-mapper-linear-bankset+))))

(defstruct a78-header
  (magic "" :type string)
  (title "" :type string)
  (size 0 :type #.(list 'integer 0 (list (expt 2 32))))
  (type 0 :type (integer 0 (65536)))
  (controller-1 0 :type (unsigned-byte 8))
  (controller-2 0 :type (unsigned-byte 8))
  (video-hint 0 :type (unsigned-byte 8))
  (save-device 0 :type (unsigned-byte 8))
  (v3-irq-enable 0 :type (unsigned-byte 8))
  (expansion-module 0 :type (unsigned-byte 8))
  (v4-mapper 0 :type (unsigned-byte 8))
  (v4-mapper-options 0 :type (unsigned-byte 8))
  (v4-mapper-audio 0 :type (integer 0 (65536)))
  (v4-mapper-irq-enable 0 :type (integer 0 (65536)))
  (version 0 :type (unsigned-byte 8)))

(defun big-endian-number (bytes)
  (check-type bytes vector)
  (loop for byte across bytes
        with number = 0
        do (setf number (logior (ash number 8) byte))
        finally (return number)))

(defun make-a78-header-from-bytes (header-bytes)
  (check-type header-bytes vector)
  (make-a78-header
   :magic (octets-to-string (subseq header-bytes 1 17) :external-format :utf-8)
   :title (octets-to-string (subseq header-bytes 17 49) :external-format :utf-8)
   :size (big-endian-number (subseq header-bytes 49 53))
   :type (big-endian-number (subseq header-bytes 53 55))
   :controller-1 (elt header-bytes 55)
   :controller-2 (elt header-bytes 56)
   :video-hint (elt header-bytes 57)
   :save-device (elt header-bytes 58)
   ;; skip 3 bytes
   :v3-irq-enable (elt header-bytes 62)
   :expansion-module (elt header-bytes 63)
   :v4-mapper (elt header-bytes 64)
   :v4-mapper-options (elt header-bytes 65)
   :v4-mapper-audio (big-endian-number (subseq header-bytes 66 68))
   :v4-mapper-irq-enable (big-endian-number (subseq header-bytes 68 70))
   :version (elt header-bytes 0)))

(defun get-7800-mapper (mapper-info file)
  (check-type mapper-info mapper-info)
  (check-type file stream)
  (file-position file 0)
  (let ((header-version (read-byte file))
        (header (let ((header-bytes (make-array '(208) :element-type '(unsigned-byte 8))))
                  (read-sequence header-bytes file)
                  (make-a78-header-from-bytes header-bytes))))
    (error "unimplemented")))

(defun com-open (pathname &optional (speed 500000))
  "Opens the specified serial port, configures its timeouts, and sets its speed."
  (check-type pathname (or pathname string))
  (check-type speed (integer 300 *))
  (cserial-port:make-serial-stream
   (cserial-port:open-serial pathname
                             :baud-rate speed
                             :data-bits 8
                             :parity :none
                             :stop-bits 1)))

(defun com-close (port)
  (check-type port stream)
  (finish-output port)
  (handler-case
      (cserial-port:close-serial port)
    (simple-error (c)
      (warn "~a" c))))

(defun set-comm-break (port &optional (enablep t))
  (cserial-port::%set-serial-state (slot-value port 'cserial-port::serial)
                                   :break enablep))
(defun com-break (port)
  (check-type port stream)
  (sb-posix:tcsendbreak (cserial-port::serial-fd (cserial-port::stream-serial port)) 1)
  t)

(defun com-write (port data size)
  (check-type port stream)
  (check-type data vector)
  (check-type size (integer 0 *))
  (assert (<= size (array-dimension data 0)))
  (assert (every (lambda (n) (typep n '(unsigned-byte 8)))
                 (subseq data 0 (min (length data) size))))
  (dotimes (i size)
    (write-byte (aref data i) port))
  t)

(defun com-read (port size)
  (check-type port stream)
  (check-type size (integer 1 *))
  (let ((buf (make-array (list size) :element-type '(unsigned-byte 8))))
    (dotimes (i size)
      (setf (aref buf i) (read-byte port t 0)))
    buf))

(defun cmd-close (port)
  (com-close port))

(defmacro with-open-7800gd-port ((port port-pathname) &body body)
  `(let (( ,port (cmd-init ,port-pathname)))
     (unwind-protect
          (progn ,@body)
       (when ,port
         (cmd-close ,port)))))
