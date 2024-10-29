(in-package :skyline-tool)

(defun serial-port-has-7800gd-p (pathname)
  (let ((port (7800gd-debug:cmd-init pathname)))
    (unless (cserial-port::%valid-fd-p (cserial-port::stream-serial port))
      (error "Invalid port? ~a ⇒ ~a" pathname port))
    (if (member (7800gd-debug:cmd-status port) '(0 1 2))
        (list pathname port)
        nil)))

(defun spawn-thread-to-look-for-7800gd-on-port (pathname)
  (check-type pathname (or pathname string))
  (make-thread (lambda () (ignore-errors (serial-port-has-7800gd-p pathname)))
               :name (format nil "Looking for 7800GD on port ~a" pathname)))

(defun find-7800gd-serial-port ()
  (if (tty-xterm-p)
      (format t "~2%~10t[1;3;4mConnect to 7800GD Serial Port~%[0m")
      (format t "~2%~10t*** Connect to 7800GD Serial Port ***"))
  (interactive-wait "

Make certain that  the USB serial interface device is  connected to both
the computer and  the 7800GD cartridge's debug port. Do  NOT continue if
any dangerous  or sensitive  devices are connected  to ANY  serial port,
instead, find the  device port pathname (e.g. /dev/ttyUSB0)  and pass it
in yourself. It is possible that this probe process could cause issues.

When ready, hit Return, and I'll try to locate the path to the interface.")
  (force-output)
  (let ((ports (click.adventuring.skyline.eprom:enumerate-real-serial-ports)))
    (format t "~&Searching ~:d serial ports…" (length ports))
    (let ((thread-pool (mapcar #'spawn-thread-to-look-for-7800gd-on-port ports)))
      (labels ((kill-threads () (dolist (th thread-pool)
				       (when (and th (thread-alive-p th))
                                                    (destroy-thread th)))))
        (loop
           (dolist (thread thread-pool)
             (unless (thread-alive-p thread)
               (let ((return (join-thread thread)))
                 (removef thread-pool thread)
                 (when return
                   (destructuring-bind (port stream) return
                     (when (y-or-n-p "~2&Found an 7800GD on ~a. Proceed?" port)
                       (kill-threads)
                       (return-from find-7800gd-serial-port (list port stream))))))))
           (unless thread-pool
             (error "Searched ~:d serial port~:p and could not find a 7800GD.
 ~{~a~^, ~:_~}"
                    (length ports)
                    ports))
           (when (zerop (random 5))
             (format t "~&… still waiting for search of ~:d serial port~:p ~
(~d% of ~:d) to respond or time out …"
                     (length thread-pool)
                     (round (* 100.0 (/ (length thread-pool) (length ports))))
                     (length ports))
             (when (< (length thread-pool) 5)
               (format t "~&… remaining tasks: ~{~a~^, ~}"
                       (mapcar #'thread-name thread-pool))))
           (sleep 3/2))))))

(defun push-7800gd (binary-pathname &key (serial-pathname (uiop:getenv "TTY"))
                                         (executep t) (skip-bank-62-p nil))
  "Push the BINARY-PATHNAME file in BIN format to the 7800GD on serial port SERIAL-PATHNAME.

If SERIAL-PATHNAME is not supplied, search for the 7800GD on any serial port.

If EXECUTEP is T then boot the uploaded code freshly."
  (check-type binary-pathname (or string pathname))
  (check-type serial-pathname (or null string pathname))
  (assert (probe-file binary-pathname) (binary-pathname)
          "Could not find the binary file “~a”" binary-pathname)
  (let ((pn (etypecase binary-pathname
              (pathname binary-pathname)
              (string (make-pathname :defaults binary-pathname)))))
    (when (string-equal "a78" (pathname-type pn))
      (cerror "Try it anyway"
              "This pathname “~a” seems to refer to an A78, not BIN, file. This probably won't work."
              (enough-namestring pn))))
  (7800gd-debug:with-open-7800gd-port (port (or serial-pathname
                                                (first (find-7800gd-serial-port))))
    (with-open-file (binary binary-pathname :element-type '(unsigned-byte 8))
      (ignore-errors (7800gd-debug:cmd-break port))
      (if (and skip-bank-62-p (= (file-length binary) #x100000))
          (progn (7800gd-debug:upload port binary 0 0 (* #x4000 62))
                 (7800gd-debug:upload port binary (* #x4000 63) (* #x4000 63) #x4000))
          (7800gd-debug:upload port binary 0 0 (file-length binary)))
      (when executep
        (7800gd-debug:cmd-execute
         port
         :mapper 7800gd-debug::+ea78-v4-mapper-supergame+
         :mapper-options 7800gd-debug::+ea78-v4-mapper-supergame-4kopt-ram+
         :mapper-audio 7800gd-debug::+ea78-v4-audio-pokey-450+
         :size (file-length binary))))))

(defun push-7800gd-bin (binary-pathname &optional serial-pathname)
  (push-7800gd binary-pathname :serial-pathname serial-pathname))

(defun push-7800gd-bin-no-execute (binary-pathname &optional serial-pathname
                                                             bump-version-p)
  (let ((serial (or serial-pathname
                    (first (find-7800gd-serial-port)))))
    (push-7800gd binary-pathname :serial-pathname serial :executep nil :skip-bank-62-p t)
    (when bump-version-p
      (7800gd-bump-version serial))))

(defvar *7800gd-version-cookie* 0)

(defun 7800gd-bump-version (serial-pathname)
  "Shove a new version number into the 7800GD over serial"
  (setf *7800gd-version-cookie* (mod (1+ *7800gd-version-cookie*) #x100))
  (7800gd-debug:with-open-7800gd-port (port serial-pathname)
    (ignore-errors (7800gd-debug:cmd-break port))
    (format *trace-output* "~&Pushing over version cookie $~2,'0x" *7800gd-version-cookie*)
    (7800gd-debug:upload port (vector *7800gd-version-cookie*)
                         (find-label-from-files "CurrentRunningVersion") 0 1)))

