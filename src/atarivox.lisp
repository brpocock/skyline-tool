(in-package :skyline-tool)

(defun find-atarivox-serial-port ()
  (if (tty-xterm-p)
      (format t "~2%~10t[1;3;4mConnect to AtariVox (via Stelladapter) Serial Port~%[0m")
      (format t "~2%~10t*** Connect to AtariVox (via Stelladapter) Serial Port ***"))
  (interactive-wait "

Make certain that the Stelladapter is connected to both the computer and
the AtariVox. Do NOT continue if  any dangerous or sensitive devices are
connected  to   ANY  serial   port,  instead,   find  the   device  port
pathname (e.g.  /dev/ttyUSB0) and  pass it in  yourself. It  is possible
that this probe process could cause issues.

When ready, hit Return, and I'll try to locate the path to the burner.")
  (force-output)
  (let ((ports (click.adventuring.skyline.eprom:enumerate-real-serial-ports)))
    (format t "~&Searching ~:d serial ports…" (length ports))
    (let ((thread-pool (mapcar #'spawn-thread-to-look-for-atarivox-on-port ports)))
      (labels ((kill-threads () (map nil
                                     (lambda (th) (when (and th (thread-alive-p th))
                                                    (destroy-thread th)))
                                     thread-pool)))
        (loop
           (dolist (thread thread-pool)
             (unless (thread-alive-p thread)
               (let ((return (join-thread thread)))
                 (removef thread-pool thread)
                 (when return
                   (destructuring-bind (port stream) return
                     (when (y-or-n-p "~2&Found an AtariVox (via Stelladapter) on ~a. Proceed?" port)
                       (kill-threads)
                       (return-from find-atarivox-serial-port (list port stream))))))))
           (unless thread-pool
             (error "Searched ~:d serial port~:p and could not find a AtariVox (via Stelladapter).
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


(defun serial-port-has-atarivox-p (pathname)
  (check-type pathname (or pathname string))
  (let ((port (cserial-port:make-serial-stream
               (cserial-port:open-serial pathname
                                         :baud-rate 19200
                                         :data-bits 8
                                         :parity :none
                                         :stop-bits 1))))
    (unless (cserial-port::%valid-fd-p (cserial-port::stream-serial port))
      (error "Invalid port? ~a ⇒ ~a" pathname port))
    (list pathname port)))

(defun spawn-thread-to-look-for-atarivox-on-port (pathname)
  (check-type pathname (or pathname string))
  (make-thread (lambda () (ignore-errors (serial-port-has-atarivox-p pathname)))
               :name (format nil "Looking for AtariVox on port ~a" pathname)))

(defvar *atarivox-port* nil)

(defmacro with-atarivox ((&optional port-name) &body body)
  `(let ((*atarivox-port* (second ,(if port-name
                                       `(serial-port-has-atarivox-p ,port-name )
                                       `(find-atarivox-serial-port)))))
     ,@body))

(defun read-speakjet-tokens ()
  (let ((hash (make-hash-table :test 'equal)))
    (with-input-from-file (speakjet.s
                           (asdf:system-relative-pathname :skyline-tool
                                                          #p"../Source/Common/SpeakJet.s"))
      (loop for line = (read-line speakjet.s nil nil)
            while line
            do (destructuring-bind (&optional key$ value$) (split-sequence #\: line)
                 (when (and key$ value$ (char= #\= (char value$ 0)))
                   (let ((key (string-trim " " key$))
                         (value (or (parse-integer value$ :start 1 :junk-allowed t)
                                    (when-let ($ (position #\$ value$))
                                      (parse-integer value$ :start (1+ $) :junk-allowed t
                                                            :radix 16)))))
                     (when value
                       (setf (gethash key hash) value)))))))
    hash))

(defun convert-atarivox-bytes (tokens)
  (let ((token-values (read-speakjet-tokens)))
    (mapcar (lambda (token$)
              (when (string= "SpeakJet." token$ :end2 (min 9 (length token$)))
                (gethash (subseq token$ 9) token-values)))
            tokens)))

(defun atarivox-speak (phrase)
  (dolist (byte (convert-atarivox-bytes (convert-for-atarivox phrase)))
    (when byte
      (write-byte byte *atarivox-port*)
      (sleep 1/50)))
  (interactive-wait "~& (Next)…"))

(defvar *read-script-frame* nil)

(clim:define-application-frame read-script-frame ()
  ((%decal-index :initform 0 :accessor decal-index :initarg :index))
  (:panes (script-list-pane :application :height 700 :width 450
                                         :display-function 'display-script-list)
          (interactor :interactor :height 125 :width 450))
  (:layouts (default (clim:vertically () script-list-pane interactor))))

(define-read-script-frame-command (com-read-script :menu t :name t)
    ((script-full-name 'script-name :gesture :select))
  (read-script-out-loud script-full-name)
  (when *read-script-frame*
    (clim:frame-exit *read-script-frame*)))

(defun read-script-out-loud (script-pathname)
  (with-atarivox ()
    (with-input-from-file (script (merge-pathnames
                                   (make-pathname :defaults script-pathname
                                                  :type "fountain")
                                   (make-pathname :directory '(:relative "Source"))))
      (loop for line = (read-line script nil nil)
            with mode = nil
            while line
            do (ecase mode
                 ((nil) (cond
                          ((or (string= line "INT " :end1 (min (length line) 4))
                               (string= line "EXT " :end1 (min (length line) 4)))
                           (format *trace-output* "~& Scene: ~a" line))
                          ((and (not (emptyp (remove-if-not #'alpha-char-p line)))
                                (every #'upper-case-p (remove-if-not #'alpha-char-p line)))
                           (destructuring-bind (char-name &rest _)
                               (split-sequence #\Space (string-trim " " line))
                             (format t "~& Speaker: ~a" char-name)
                             (let* ((stats (cond
                                             ((char= #\> (char char-name 0)) nil)
                                             ((string-equal "NARRATOR" char-name)
                                              (list :speed 96
                                                    :pitch 114
                                                    :bend 88))
                                             (t (load-actor char-name))))
                                    (speed (getf stats :speed))
                                    (pitch (getf stats :pitch))
                                    (bend (getf stats :bend)))
                               (format t "   (Speed: ~d Pitch: ~d Bend: ~d)"
                                       speed pitch bend)
                               (when speed
                                 (write-byte 21 *atarivox-port*)
                                 (write-byte speed *atarivox-port*))
                               (when pitch
                                 (write-byte 22 *atarivox-port*)
                                 (write-byte pitch *atarivox-port*))
                               (when bend
                                 (write-byte 23 *atarivox-port*)
                                 (write-byte bend *atarivox-port*))))
                           (setf mode :speaker))
                          (t nil)))
                 (:speaker
                  (cond ((emptyp (string-trim " " line))
                         (setf mode nil))
                        (t (format t "~& « ~a »" line)
                           (atarivox-speak line)))))))))

(defun play-script-on-atarivox (&optional script-to-read)
  "Choose a script from a menu, and read it out loud on AtariVox (via Stelladaptor over USB)"
  (if script-to-read
      (progn
        (when *read-script-frame*
          (clim:frame-exit *read-script-frame*))
        (read-script-out-loud
         (if (search "Scripts/" script-to-read)
             script-to-read
             (format nil "Scripts/~a" script-to-read))))
      
      (let* ((frame (clim:make-application-frame 'read-script-frame))
             (*read-script-frame* frame))
        (setf (clim:frame-pretty-name frame)
              (format nil "~a: Read Script" (cl-change-case:title-case *game-title*)))
        (make-thread (lambda () (clim:run-frame-top-level frame))
                     :name "Script Reader (launcher)"))))
