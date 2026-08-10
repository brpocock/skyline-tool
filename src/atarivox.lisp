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
    (write-bytes #(31 28 0) port)
    (list pathname port)))

(defun spawn-thread-to-look-for-atarivox-on-port (pathname)
  (check-type pathname (or pathname string))
  (clim-sys:make-process (lambda () (ignore-errors (serial-port-has-atarivox-p pathname)))
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

(defun read-intellivoice-tokens ()
  (let ((hash (make-hash-table :test 'equal)))
    (with-input-from-file (intellivoice.s
                           (asdf:system-relative-pathname :skyline-tool
                                                          #p"../Source/Common/IntelliVoice.s"))
      (loop for line = (read-line intellivoice.s nil nil)
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

(defun convert-speech-bytes (tokens &optional (speech-system :atarivox))
  "Convert speech tokens to byte values for the specified speech system."
  (let ((token-values (ecase speech-system
                        (:atarivox (read-speakjet-tokens))
                        (:intellivoice (read-intellivoice-tokens)))))
    (mapcar (lambda (token$)
              (cond
                ((string= "SpeakJet." token$ :end2 (min 9 (length token$)))
                 (gethash (subseq token$ 9) token-values))
                ((string= "IntelliVoice." token$ :end2 (min 13 (length token$)))
                 (gethash (subseq token$ 13) token-values))
                (t (gethash token$ token-values))))
            tokens)))

(defun convert-atarivox-bytes (tokens)
  (convert-speech-bytes tokens :atarivox))

(defun convert-intellivoice-bytes (tokens)
  (convert-speech-bytes tokens :intellivoice))

(defun speech-speak (phrase &optional (speech-system :atarivox))
  "Speak a phrase using the specified speech system."
  (ecase speech-system
    (:atarivox
     (dolist (byte (convert-speech-bytes (convert-for-speech phrase speech-system) speech-system))
       (when byte
         (write-byte byte *atarivox-port*)
         (sleep 1/50)))
     (interactive-wait "~& (AtariVox: Next)…"))
    (:intellivoice
     ;; IntelliVoice speech synthesis is handled by the jzIntv emulator
     ;; We prepare the phoneme data here for the Forth interpreter to use
     (let ((phonemes (convert-for-speech phrase :intellivoice)))
       (format t "~& (IntelliVoice phonemes prepared for '~a': ~{~a~^ ~})~%" phrase phonemes)
       ;; Return phoneme data for Forth bytecode generation
       phonemes))))

(defun current-speech-system ()
  "Determine the appropriate speech system based on the current machine."
  (case *machine*
    (2609 :intellivoice)  ; Intellivision with IntelliVoice (CP1610)
    (2600 :atarivox)      ; Atari 2600 with AtariVox
    (7800 :atarivox)))    ; Atari 7800 with AtariVox

(defun atarivox-speak (phrase)
  (speech-speak phrase :atarivox))

(defun intellivoice-speak (phrase)
  "Generate IntelliVoice phoneme data for emulator synthesis.
   Since there's no Linux hardware interface for IntelliVoice,
   this prepares phoneme data that jzIntv can synthesize."
  (let ((phonemes (convert-for-speech phrase :intellivoice)))
    (format t "~&IntelliVoice phonemes for '~a': ~{~a~^ ~}~%" phrase phonemes)
    ;; Return the phoneme data for use by the Forth interpreter
    phonemes))

(defun convert-for-speech (string speech-system)
  "Convert STRING into a list of tokens for the specified speech system."
  (ecase speech-system
    (:atarivox (convert-for-atarivox string))
    (:intellivoice (convert-for-intellivoice string))))

(defun convert-for-intellivoice (string)
  "Convert STRING into a list of IntelliVoice phoneme tokens"
  (ensure-intellivoice-dictionary)
  (when (emptyp string) (return-from convert-for-intellivoice nil))
  ;; Parse the string using the IntelliVoice dictionary
  (let ((words nil))
    (cl-ppcre:do-scans (start end reg-starts reg-ends
                        "(\\s+|-|\\\\\\d+|[~\\\\]\\p{L}+|[\\p{L}\\p{N}']+|[^\\s\\p{L}\\p{N}'-]+)" string)
      (let ((word (string-trim #(#\Space #\Tab #\Newline)
                               (subseq string start end))))
        (push word words)))
    (let ((output (list)))
      (dolist (word words)
        (if (and (not (emptyp word))
                 (some #'digit-char-p word)
                 (every #'char-digit-or-comma-p word))
            (dolist (num (reverse (split-sequence-if-not #'alpha-char-p
                                                         (format nil "~r" (parse-number (remove #\, word))))))
              (push num output))
            (push word output)))
      (setf words output))
    (let ((bytes (loop
                   for word in words
                   append (cond
                            ((emptyp word) (list "PA1"))
                            ((string= word ",") (list "PA2"))
                            ((string= word ".") (list "PA3"))
                            ((string= word "!") (list "PA3"))
                            ((string= word "?") (list "PA4"))
                            ((string= word ";") (list "PA3"))
                            ((string= word ":") (list "PA4"))
                            (t (let ((phonemes (gethash (string-upcase word) *intellivoice-dictionary*)))
                                 (if phonemes
                                     (split-sequence #\Space phonemes)
                                     ;; Fallback: try to pronounce unknown words using basic rules
                                     (intellivoice-basic-pronunciation word))))))))
      bytes)))

(defun intellivoice-basic-pronunciation (word)
  "Fallback phoneme sequence for unknown IntelliVoice WORD.

Returns a conservative pause token so script compilation succeeds even when
the dictionary lacks an entry."
  (declare (ignore word))
  (list "PA1"))

(defvar *intellivoice-dictionary* nil)

(defun ensure-intellivoice-dictionary ()
  (unless *intellivoice-dictionary*
    (reload-intellivoice-dictionary)))

(defun reload-intellivoice-dictionary ()
  "Load the IntelliVoice phonetic dictionary"
  (setf *intellivoice-dictionary* (make-hash-table :test 'equal))
  (with-input-from-file (dic #p"Source/Tables/IntelliVoice.dic")
    (assert (equalp "[words]" (read-line dic nil nil)) ()
            "Expected [words] section in IntelliVoice.dic")
    (loop for line = (read-line dic nil nil)
          while line
          do (when (and (> (length line) 0)
                        (not (char= #\# (char line 0)))
                        (not (char= #\[ (char line 0))))
               (let ((equals-pos (position #\= line)))
                 (when equals-pos
                   (let ((word (string-trim " " (subseq line 0 equals-pos)))
                         (phonemes (string-trim " " (subseq line (1+ equals-pos)))))
                     (setf (gethash word *intellivoice-dictionary*) phonemes)))))))
  (format *trace-output* "~&Loaded ~:d words into IntelliVoice dictionary~%"
          (hash-table-count *intellivoice-dictionary*)))

(defvar *read-script-frame* nil)

(clim:define-command-table read-script-menu
  :menu (("Run Script..." :command com-run-script)
         ("Read Aloud..." :command com-read-script)
         ("Edit Script..." :command com-edit-script)
         (nil :divider :line)
         ("Close Script" :command com-close-frame)))

(clim:define-command-table script-help-menu
  :menu (("How to Read Scripts" :command com-help-for-window)
         ("Skyline-Tool Developers' Guide" :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide" :command com-open-scripting-guide)
         (nil :divider :line)
         ("About Skyline-Tool" :command com-about-skyline-tool)))

(clim:define-command-table read-menu-bar
  :menu (("Script" :menu read-script-menu) ("Edit" :menu edit-menu) ("Help" :menu script-help-menu)))

(clim:define-application-frame read-script-frame ()
  ((%decal-index :initform 0 :accessor decal-index :initarg :index))
  (:panes (script-list-pane :application :height 700 :width 750
                                         :display-function 'display-script-list)
          (interactor :interactor :height 125 :width 750))
  (:menu-bar read-menu-bar)
  (:icon (skyline-tool-icon :resource :script))
  (:layouts (default (clim:vertically () script-list-pane interactor))))

;; Commands defined after frame so define-read-script-frame-command is available
(define-read-script-frame-command (com-read-script :menu nil :name t)
    ((script-full-name 'script-name :gesture :select))
  (read-script-out-loud script-full-name)
  (when *read-script-frame*
    (clim:frame-exit *read-script-frame*)))

(define-read-script-frame-command (com-run-script :menu nil :name t)
    ((script-full-name 'script-name :gesture :select))
  (run-script script-full-name)
  (when *read-script-frame*
    (clim:frame-exit *read-script-frame*)))

(define-read-script-frame-command (com-edit-script :menu nil :name t)
    ((script-full-name 'script-name :gesture :edit))
  (if swank::*emacs-connection*
      (swank:ed-in-emacs (format nil "Source/~a.fountain" script-full-name))
      (clim-sys:make-process (lambda ()
                               (uiop:run-program
                                (list "emacsclient" "-n"
                                      (format nil "Source/~a.fountain" script-full-name))))
                             :name (format nil "Editing ~a" script-full-name))))

(defun read-script-out-loud (script-pathname)
  "Non-interactive script reading — speaks straight through without pauses."
  (ecase (current-speech-system)
    (:atarivox
     (with-atarivox ()
       (%read-fountain-script script-pathname nil nil)))
    (:intellivoice
     (error "~&IntelliVoice script reading not yet implemented for: ~a" script-pathname))))

(defun %read-fountain-script (script-pathname stream interactive &optional (atarivox-stream *atarivox-port*))
  "Parse and speak a fountain SCRIPT-PATHNAME.
   If STREAM is non-NIL, display output there.
   If INTERACTIVE, wait for Return after each spoken line."
  (with-input-from-file (script (merge-pathnames
                                 (make-pathname :defaults script-pathname :type "fountain")
                                 (make-pathname :directory '(:relative "Source"))))
    (flet (($ (fmt &rest args)
             (when stream (apply #'format stream fmt args))
             (force-output stream)))
      (loop for line = (read-line script nil nil)
            with mode = nil
            while line
            do (ecase mode
                 ((nil)
                  (cond
                    ((or (string= line "INT " :end1 (min (length line) 4))
                         (string= line "EXT " :end1 (min (length line) 4)))
                     ($ "~2%Scene: ~a" line))
                    ((and (not (emptyp (remove-if-not #'alpha-char-p line)))
                          (every #'upper-case-p (remove-if-not #'alpha-char-p line)))
                     (destructuring-bind (char-name &rest _)
                         (split-sequence #\Space (string-trim " " line))
                       (declare (ignore _))
                       ($ "~%~%Speaker: ~a~%" char-name)
                       (let* ((stats (cond
                                       ((char= #\> (char char-name 0)) nil)
                                       ((or (string-equal "NARRATOR" char-name)
                                            (string-equal "PLAYER" char-name))
                                        (list :speed 96 :pitch 114 :bend 88))
                                       (t (load-actor char-name))))
                              (speed (getf stats :speed))
                              (pitch (getf stats :pitch))
                              (bend (getf stats :bend)))
                         ($ "   (Speed: ~d Pitch: ~d Bend: ~d)" speed pitch bend)
                         (when speed (write-byte 21 atarivox-stream) (write-byte speed atarivox-stream))
                         (when pitch (write-byte 22 atarivox-stream) (write-byte pitch atarivox-stream))
                         (when bend (write-byte 23 atarivox-stream) (write-byte bend atarivox-stream))))
                     (setf mode :speaker))
                    (t nil)))
                 (:speaker
                  (cond ((emptyp (string-trim " " line))
                         (setf mode nil)
                         ($ "~&"))
                        (t
                         ($ "~&  « ~a »" line)
                         (speech-speak line :atarivox)
                         (when interactive
                           ($ "~%  — Press Return to continue —")
                            (read-char stream))))))))))


(defun %setup-voice-params (char-name)
  "Load and apply voice parameters for CHAR-NAME to the AtariVox."
  (let* ((stats (cond
                  ((char= #\> (char char-name 0)) nil)
                  ((or (string-equal "NARRATOR" char-name)
                       (string-equal "PLAYER" char-name))
                   (list :speed 96 :pitch 114 :bend 88))
                  (t (load-actor char-name))))
         (speed (getf stats :speed))
         (pitch (getf stats :pitch))
         (bend (getf stats :bend)))
    (when speed (write-byte 21 *atarivox-port*) (write-byte speed *atarivox-port*))
    (when pitch (write-byte 22 *atarivox-port*) (write-byte pitch *atarivox-port*))
    (when bend (write-byte 23 *atarivox-port*) (write-byte bend *atarivox-port*))))

(defvar *script-reader-state* nil
  "Current state for the interactive script reader: (line mode scene-heading character-name).")

(defvar *script-reader-lines* nil
  "All parsed (type text) pairs for the current script being read.")

(defvar *script-reader-index* 0
  "Current position in *script-reader-lines*.")

(clim:define-application-frame script-reader-frame ()
  ((current-line :initform "" :accessor reader-current-line)
   (current-speaker :initform "" :accessor reader-current-speaker)
   (current-scene :initform "" :accessor reader-current-scene)
   (elements :initform nil :accessor reader-elements)
   (index :initform 0 :accessor reader-index))
  (:panes (display-pane :application :height 500 :width 700
                                    :display-function 'display-reader-line)
          (interactor :interactor :height 80 :width 700))
  (:layouts (default (clim:vertically () display-pane interactor)))
  (:command-table (script-reader-frame)))

(defun display-reader-line (frame pane)
  (clim:window-clear pane)
  (let ((scene (reader-current-scene frame))
        (speaker (reader-current-speaker frame))
        (line (reader-current-line frame)))
    (when (plusp (length scene))
      (clim:with-text-face (pane :bold)
        (format pane "Scene: ~a~%~%" scene)))
    (when (plusp (length speaker))
      (clim:with-text-face (pane :italic)
        (format pane "~a~%~%" speaker)))
    (format pane "~a~%" line)
    (format pane "~%~10t— Press Return in the input area below to advance —")))

(defun reader-loop (frame)
  "Main loop: advance through parsed elements, updating the display
   and speaking dialogue via AtariVox."
  (let* ((elements (reader-elements frame))
         (idx 0)
         (interactor (clim:find-pane-named frame 'interactor))
         (stream (or interactor *standard-output*)))
    (flet ((show (scene speaker line)
             (setf (reader-current-scene frame) (or scene "")
                   (reader-current-speaker frame) (or speaker "")
                   (reader-current-line frame) (or line ""))
             (clim:redisplay-frame-panes frame)))
      (show nil nil "(Ready — press Return to begin)")
      (loop while (< idx (length elements))
            for elem = (nth idx elements)
            for type = (first elem)
            for text = (second elem)
            do (incf idx)
            do (case type
                 (:scene-heading
                  (show text nil nil))
                 (:character
                  (show nil text "")
                  (%setup-voice-params text)
                  ;; Wait for Return before showing dialogue
                  (handler-case
                      (progn
                        (format stream "~&Press Return to hear ~a..." text)
                        (force-output stream)
                        (read-line stream))
                    (error ())))
                 (:parenthetical
                  (show nil nil text))
                 (:dialogue
                  (show nil nil text)
                  (handler-case
                      (progn
                        (speech-speak text :atarivox)
                        (format stream "~&Press Return for next line...")
                        (force-output stream)
                        (read-line stream))
                    (error ()))))
            finally (show nil nil "(End of script)")))))

(defun parse-fountain-for-reader (script-full-name)
  "Parse a fountain file into a list of (type text) elements for the reader."
  (let ((elements nil) (mode nil))
    (with-input-from-file (script (merge-pathnames
                                   (make-pathname :defaults script-full-name :type "fountain")
                                   (make-pathname :directory '(:relative "Source"))))
      (loop for line = (read-line script nil nil)
            while line
            do (cond
                 ((or (string= line "INT " :end1 (min (length line) 4))
                      (string= line "EXT " :end1 (min (length line) 4)))
                  (push (list :scene-heading (string-trim " " line)) elements))
                 ((and (not (emptyp (remove-if-not #'alpha-char-p line)))
                       (every #'upper-case-p (remove-if-not #'alpha-char-p line)))
                  (push (list :character (string-trim " " line)) elements)
                  (setf mode :speaker))
                 ((eq mode :speaker)
                  (cond ((emptyp (string-trim " " line))
                         (setf mode nil))
                        ((char= (char line 0) #\()
                         (push (list :parenthetical (string-trim " " line)) elements))
                        (t
                         (push (list :dialogue (string-trim " " line)) elements)))))))
    (nreverse elements)))

(defun read-script-interactive (script-full-name)
  "Interactive script reading — shows each line in a CLIM window,
   speaks it via AtariVox, and waits for Return to advance."
  (ecase (current-speech-system)
    (:atarivox
     (let* ((elements (parse-fountain-for-reader script-full-name))
            (fm (or (clim:find-frame-manager) (return-from read-script-interactive)))
            (frame (clim:make-application-frame 'script-reader-frame
                                                 :pretty-name (format nil "Read: ~a" script-full-name)
                                                 :frame-manager fm
                                                 :elements elements
                                                 :width 720 :height 500)))
       ;; Start the frame event loop and reader in parallel processes
       (clim-sys:make-process
        (lambda ()
          (clim:run-frame-top-level frame))
        :name (format nil "Script Reader Frame"))
       ;; Short delay to let the frame start, then run the reader with AtariVox
       (sleep 0.5)
       (clim-sys:make-process
        (lambda ()
          (with-atarivox ()
            (reader-loop frame)))
        :name (format nil "Reader: ~a" script-full-name))))
    (:intellivoice
     (error "IntelliVoice interactive reading not implemented."))))

(defun play-script-with-speech (&optional script-to-read)
  "Choose a script from a menu, and read it out loud using the appropriate speech system for the current platform"
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
              (window-title "Read Script"))
        (clim-sys:make-process (lambda () (clim:run-frame-top-level frame))
                               :name "Script Reader (launcher)"))))
