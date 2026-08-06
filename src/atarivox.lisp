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
                           (asdf:system-relative-pathname
                            :skyline-tool
                            (make-pathname :directory (list :relative :up "Source" "Code"
                                                            (if *machine*
                                                                (machine-directory-name)
                                                                "7800")
                                                            "Common")
                                           :name "SpeakJet" :type "s")))
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

(defmethod convert-speech-bytes (tokens (speech-system (eql :atarivox)))
  "Convert speech tokens to byte values for the specified speech system."
  (let ((token-values (read-speakjet-tokens)))
    (mapcar (lambda (token$)
              (cond
                ((string= "SpeakJet." token$ :end2 (min 9 (length token$)))
                 (gethash (subseq token$ 9) token-values))
                ((char= #\$ (char token$ 0))
                 (parse-integer token$ :start 1 :radix 16))
                (t (gethash token$ token-values))))
            tokens)))

(defmethod convert-speech-bytes (tokens (speech-system (eql :intellivoice)))
  "Convert speech tokens to byte values for the specified speech system."
  (let ((token-values (read-intellivoice-tokens)))
    (mapcar (lambda (token$)
              (cond
                ((string= "IntelliVoice." token$ :end2 (min 13 (length token$)))
                 (gethash (subseq token$ 13) token-values))
                (t (gethash token$ token-values))))
            tokens)))

(defun convert-atarivox-bytes (tokens)
  (convert-speech-bytes tokens :atarivox))

(defun convert-intellivoice-bytes (tokens)
  (convert-speech-bytes tokens :intellivoice))

(defmethod speech-speak (phrase (speech-system (eql :atarivox)) &key
                                                                  (voice (default-voice-profile)))
  "Speak a phrase using the specified speech system.
VOICE supplies the speaking character's VOICE-PROFILE, which drives the
intonation envelope; when omitted the default profile is used."
  (let ((buffer (convert-for-speech phrase speech-system :voice voice))
        (speakjet-queue 0))
    (dolist (byte  buffer)
      (loop with bytes = (convert-speech-bytes buffer speech-system)
            for byte = (pop bytes)
            when byte
              do (progn (write-byte byte *atarivox-port*)
                        (sleep 1/16)
                        (when (< 4 (incf speakjet-queue
                                         (cond ((< byte 6) (/ (rest (elt +speakjet-pause-durations+ byte)) 1000))
                                               ((<= 20 byte 30) (write-byte (pop bytes) *atarivox-port*)
                                                                1/30)
                                               ((< byte #x20) 1/60)
                                               (t 1/30))))
                          (loop while (plusp (decf speakjet-queue)) do (sleep 1/30))))))))

(defun current-speech-system ()
  "Determine the appropriate speech system based on the current machine."
  (case *machine*
    (2609 :intellivoice)      ; Intellivision with IntelliVoice (CP1610)
    (64 :magic-voice)
    ((2600 7800 3000 20 64 128) :atarivox)))    ; AtariVox/VecVox

(defun atarivox-speak (phrase)
  (speech-speak phrase :atarivox))

(defun intellivoice-speak (phrase)
  (declare (ignore phrase))
  (error "IntelliVoice does not have a PC interface, so I cannot speak thisnnnn"))

(defun voice-profile-for-name (char-name)
  "Return the VOICE-PROFILE for the speaker CHAR-NAME, or the default profile.
NARRATOR and PLAYER use the fixed values applied by %SETUP-VOICE-PARAMS."
  (let ((name (string-trim " " (or char-name ""))))
    (cond
      ((or (emptyp name) (char= #\> (char name 0)))
       (default-voice-profile))
      ((string-equal "NARRATOR" name)
       (voice-profile-from-actor (list :speed 96 :pitch 88 :bend 4)))
      ((string-equal "PLAYER" name)
       (voice-profile-from-actor (list :speed 96 :pitch 80 :bend 5)))
      (t (voice-profile-from-actor (load-actor name))))))

(defmethod convert-for-speech (string (speech-system (eql :atarivox))
                               &key (voice (default-voice-profile)))
  "Convert STRING into a list of tokens for the specified speech system."
  (convert-for-atarivox string :voice voice))

(defmethod convert-for-speech (string (speech-system (eql :intellivoice))
                               &key (voice (default-voice-profile)))
  "Convert STRING into a list of tokens for the specified speech system."
  (declare (ignore voice))
  (convert-for-intellivoice string))

(defun intellivoice-basic-pronunciation (word)
  (let ((s (string-upcase word))
        (phonemes nil)
        (i 0)
        (len (length word)))
    (labels ((peek (n) (if (< (+ i n) len) (char s (+ i n)) (code-char 0)))
             (emit (p) (push p phonemes) t)
             (match (pat)
               (let ((plen (length pat)))
                 (when (string-prefix-p pat s :start2 i)
                   (incf i plen)
                   t))))
      (loop while (< i len)
            do (let ((c (peek 0)))
                 (cond
                   ((and (char= c #\Q) (char= (peek 1) #\U))
                    (emit "KW") (incf i 2))
                   ((match "YOU") (emit "YY1") (emit "UW1"))
                   ((match "THR") (emit "TH") (emit "RR1"))
                   ((match "TH") (emit "TH"))
                   ((match "SH") (emit "SH"))
                   ((match "CH") (emit "CH"))
                   ((match "WH") (emit "WH"))
                   ((match "PH") (emit "FF"))
                   ((match "GH") (emit "KK1"))
                   ((match "KN"))
                   ((match "GN"))
                   ((match "PS"))
                   ((match "WR") (emit "RR1"))
                   ((match "MB") (emit "MM"))
                   ((match "NG") (emit "NG"))
                   ((match "CK") (emit "KK1"))
                   ((match "TCH") (emit "CH"))
                   ((match "DGE") (emit "JH"))
                   ((match "EA") (emit "IY"))
                   ((match "EE") (emit "IY"))
                   ((match "EY") (emit "EY"))
                   ((match "AY") (emit "EY"))
                   ((match "AI") (emit "EY"))
                   ((match "OA") (emit "OW"))
                   ((match "OO") (emit "UW1"))
                   ((match "OI") (emit "OY"))
                   ((match "OY") (emit "OY"))
                   ((match "OU") (emit "OW"))
                   ((match "OW") (emit "OW"))
                   ((match "AU") (emit "AW"))
                   ((match "AW") (emit "AW"))
                   ((match "IE") (emit "IY"))
                   ((match "UE") (emit "UW1"))
                   ((match "UI") (emit "UW1"))
                   ((match "EI") (emit "IY"))
                   ((match "EU") (emit "UW1"))
                   ((match "ER") (emit "ER1"))
                   ((match "AR") (emit "AA") (emit "RR1"))
                   ((match "OR") (emit "AO") (emit "RR1"))
                   ((match "IR") (emit "ER1"))
                   ((match "UR") (emit "ER1"))
                   ((char= c #\B) (emit "BB1") (incf i))
                   ((char= c #\C) (emit (if (member (peek 1) '(#\E #\I #\Y)) "SS" "KK1")) (incf i))
                   ((char= c #\D) (emit "DD1") (incf i))
                   ((char= c #\F) (emit "FF") (incf i))
                   ((char= c #\G) (emit (if (member (peek 1) '(#\E #\I #\Y)) "JH" "GG1")) (incf i))
                   ((char= c #\H) (emit "HH1") (incf i))
                   ((char= c #\J) (emit "JH") (incf i))
                   ((char= c #\K) (emit "KK1") (incf i))
                   ((char= c #\L) (emit "LL") (incf i))
                   ((char= c #\M) (emit "MM") (incf i))
                   ((char= c #\N) (emit "NN1") (incf i))
                   ((char= c #\P) (emit "PP") (incf i))
                   ((char= c #\Q) (emit "KK1") (incf i))
                   ((char= c #\R) (emit "RR1") (incf i))
                   ((char= c #\S)
                    (cond ((char= (peek 1) #\S) (incf i))
                          ((char= (peek 1) #\C) (emit "SH") (incf i 2))
                          ((and (char= (peek 1) #\I) (char= (peek 2) #\O)) (emit "ZH") (incf i 3))
                          ((and (char= (peek 1) #\U) (char= (peek 2) #\R)) (emit "ZH") (incf i 3))
                          (t (emit "SS") (incf i))))
                   ((char= c #\T)
                    (cond ((and (char= (peek 1) #\I) (char= (peek 2) #\O)) (emit "SH") (incf i 3))
                          ((and (char= (peek 1) #\S) (char= (peek 2) #\S)) (incf i 2))
                          ((char= (peek 1) #\T) (incf i))
                          (t (emit "TT1") (incf i))))
                   ((char= c #\V) (emit "VV") (incf i))
                   ((char= c #\W) (emit "WW") (incf i))
                   ((char= c #\X) (emit "KK1") (emit "SS") (incf i))
                   ((char= c #\Y) (emit "YY1") (incf i))
                   ((char= c #\Z) (emit "ZZ") (incf i))
                   ((char= c #\A)
                    (cond ((char= (peek 1) #\E) (emit "AA") (incf i 2))
                          ((char= (peek 1) #\R) (emit "AA") (emit "RR1") (incf i 2))
                          ((and (char= (peek 1) #\I) (char= (peek 2) #\R)) (emit "EH") (incf i 3))
                          ((and (char= (peek 1) #\L) (char= (peek 2) #\L)) (emit "AO") (incf i 3))
                          (t (emit "AE") (incf i))))
                   ((char= c #\E)
                    (cond ((and (char= (peek 1) #\A) (char= (peek 2) #\R)) (emit "IY") (emit "AX") (incf i 3))
                          ((and (char= (peek 1) #\U) (char= (peek 2) #\R)) (emit "IY") (incf i 3))
                          ((and (char= (peek 1) #\I) (char= (peek 2) #\R)) (emit "IY") (incf i 3))
                          ((char= (peek 1) #\W) (emit "UW1") (incf i 2))
                          ((and (char= (peek 2) #\E) (not (alpha-char-p (peek 3))))
                           (emit "IY") (incf i 3))
                          ((and (char= (peek 1) #\A) (char= (peek 2) #\D)) (emit "EH") (incf i 3))
                          (t (emit "EH") (incf i))))
                   ((char= c #\I)
                    (cond ((char= (peek 1) #\R) (emit "ER1") (incf i 2))
                          ((and (char= (peek 2) #\E) (not (alpha-char-p (peek 3))))
                           (emit "IY") (incf i 3))
                          (t (emit "IH") (incf i))))
                   ((char= c #\O)
                    (cond ((and (char= (peek 1) #\O) (char= (peek 2) #\R)) (emit "AO") (incf i 3))
                          ((char= (peek 1) #\R) (emit "AO") (incf i 2))
                          ((and (char= (peek 2) #\E) (not (alpha-char-p (peek 3))))
                           (emit "OW") (incf i 3))
                          (t (emit "AA") (incf i))))
                   ((char= c #\U)
                    (cond ((char= (peek 1) #\R) (emit "ER1") (incf i 2))
                          ((and (char= (peek 2) #\E) (not (alpha-char-p (peek 3))))
                           (emit "UW1") (incf i 3))
                          (t (emit "UH") (incf i))))
                   (t (incf i))))))
    (nreverse phonemes)))

(defun atarivox-basic-pronunciation (word)
  "Generate a fallback pronunciation for WORD using SpeakJet phonemes.
   This is a basic heuristic implementation for words not found in the dictionary."
  (let ((s (string-upcase word))
        (phonemes nil)
        (i 0)
        (len (length word)))
    (labels ((peek (n) (if (< (+ i n) len) (char s (+ i n)) (code-char 0)))
             (emit (p) (push p phonemes) t)
             (match (pat)
               (let ((plen (length pat)))
                 (when (string-prefix-p pat s :start2 i)
                   (incf i plen)
                   t))))
      (loop while (< i len)
            do (let ((c (peek 0)))
                 (cond
                   ((and (char= c #\Q) (char= (peek 1) #\U))
                    (emit "KW") (incf i 2))
                   ((match "THR") (emit "TH") (emit "RR"))
                   ((match "SH") (emit "SH"))
                   ((match "CH") (emit "CH"))
                   ((match "WH") (emit "WH"))
                   ((match "PH") (emit "FF"))
                   ((match "GH") (emit "KK"))
                   ((match "KN"))
                   ((match "GN"))
                   ((match "PS"))
                   ((match "WR") (emit "RR"))
                   ((match "MB") (emit "MM"))
                   ((match "NG") (emit "NG"))
                   ((match "CK") (emit "KK"))
                   ((match "TCH") (emit "CH"))
                   ((match "DGE") (emit "JH"))
                   ((match "EA") (emit "IY"))
                   ((match "EE") (emit "IY"))
                   ((match "EY") (emit "EY"))
                   ((match "AY") (emit "EY"))
                   ((match "AI") (emit "EY"))
                   ((match "OA") (emit "OW"))
                   ((match "OO") (emit "UW"))
                   ((match "OI") (emit "OY"))
                   ((match "OY") (emit "OY"))
                   ((match "OU") (emit "OW"))
                   ((match "OW") (emit "OW"))
                   ((match "AU") (emit "AW"))
                   ((match "AW") (emit "AW"))
                   ((match "IE") (emit "IY"))
                   ((match "UE") (emit "UW"))
                   ((match "UI") (emit "UW"))
                   ((match "EI") (emit "IY"))
                   ((match "EU") (emit "UW"))
                   ((match "ER") (emit "ER"))
                   ((match "AR") (emit "AA") (emit "RR"))
                   ((match "OR") (emit "AO") (emit "RR"))
                   ((match "IR") (emit "ER"))
                   ((match "UR") (emit "ER"))
                   ((char= c #\A)
                    (cond ((char= (peek 1) #\E) (emit "AA") (incf i 2))
                          ((char= (peek 1) #\R) (emit "AA") (emit "RR") (incf i 2))
                          ((and (char= (peek 1) #\I) (char= (peek 2) #\R))
                           (emit "EH") (incf i 3))
                          ((and (char= (peek 1) #\L) (char= (peek 2) #\L))
                           (emit "AO") (incf i 3))
                          (t (emit "AE") (incf i))))
                   ((char= c #\B) (emit "BE") (incf i))
                   ((char= c #\C)
                    (emit (if (member (peek 1) '(#\E #\I #\Y)) "SE" "KK")) (incf i))
                   ((char= c #\D) (emit "DE") (incf i))
                   ((char= c #\E)
                    (cond ((and (char= (peek 1) #\A) (char= (peek 2) #\R))
                           (emit "IY") (emit "AX") (incf i 3))
                          ((and (char= (peek 1) #\U) (char= (peek 2) #\R))
                           (emit "IY") (incf i 3))
                          ((and (char= (peek 1) #\I) (char= (peek 2) #\R))
                           (emit "IY") (incf i 3))
                          ((char= (peek 1) #\W) (emit "UW") (incf i 2))
                          ((and (char= (peek 2) #\E) (not (alpha-char-p (peek 3))))
                           (emit "IY") (incf i 3))
                          ((and (char= (peek 1) #\A) (char= (peek 2) #\D))
                           (emit "EH") (incf i 3))
                          (t (emit "EH") (incf i))))
                   ((char= c #\F) (emit "FF") (incf i))
                   ((char= c #\G)
                    (emit (if (member (peek 1) '(#\E #\I #\Y)) "JH" "GG")) (incf i))
                   ((char= c #\H) (emit "HE") (incf i))
                   ((char= c #\I)
                    (cond ((char= (peek 1) #\R) (emit "ER") (incf i 2))
                          ((and (char= (peek 2) #\E) (not (alpha-char-p (peek 3))))
                           (emit "IY") (incf i 3))
                          (t (emit "IH") (incf i))))
                   ((char= c #\J) (emit "JH") (incf i))
                   ((char= c #\K) (emit "KK") (incf i))
                   ((char= c #\L) (emit "LE") (incf i))
                   ((char= c #\M) (emit "MM") (incf i))
                   ((char= c #\N) (emit "NE") (incf i))
                   ((char= c #\O)
                    (cond ((and (char= (peek 1) #\O) (char= (peek 2) #\R))
                           (emit "AO") (incf i 3))
                          ((char= (peek 1) #\R) (emit "AO") (incf i 2))
                          ((and (char= (peek 2) #\E) (not (alpha-char-p (peek 3))))
                           (emit "OW") (incf i 3))
                          (t (emit "AA") (incf i))))
                   ((char= c #\P) (emit "PE") (incf i))
                   ((char= c #\Q) (emit "KK") (incf i))
                   ((char= c #\R) (emit "RR") (incf i))
                   ((char= c #\S)
                    (cond ((char= (peek 1) #\S) (incf i))
                          ((char= (peek 1) #\C) (emit "SH") (incf i 2))
                          ((and (char= (peek 1) #\I) (char= (peek 2) #\O))
                           (emit "ZH") (incf i 3))
                          ((and (char= (peek 1) #\U) (char= (peek 2) #\R))
                           (emit "ZH") (incf i 3))
                          (t (emit "SE") (incf i))))
                   ((char= c #\T)
                    (cond ((and (char= (peek 1) #\I) (char= (peek 2) #\O))
                           (emit "SH") (incf i 3))
                          ((and (char= (peek 1) #\S) (char= (peek 2) #\S))
                           (incf i 2))
                          ((char= (peek 1) #\T) (incf i))
                          (t (emit "TE") (incf i))))
                   ((char= c #\U)
                    (cond ((char= (peek 1) #\R) (emit "ER") (incf i 2))
                          ((and (char= (peek 2) #\E) (not (alpha-char-p (peek 3))))
                           (emit "UW") (incf i 3))
                          (t (emit "UH") (incf i))))
                   ((char= c #\V) (emit "VV") (incf i))
                   ((char= c #\W) (emit "WW") (incf i))
                   ((char= c #\X) (emit "KK") (emit "SE") (incf i))
                   ((char= c #\Y) (emit "YY") (incf i))
                   ((char= c #\Z) (emit "ZZ") (incf i))
                   (t (incf i))))))
    (nreverse phonemes)))

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
            with speaker = nil
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
                       (setf speaker char-name)
                       ($ "~%~%Speaker: ~a~%" char-name)
                       (let* ((stats (cond
                                       ((char= #\> (char char-name 0)) nil)
                                       ((string-equal "NARRATOR" char-name)
                                        (list :speed 96 :pitch 88 :bend 4))
                                       ((string-equal "PLAYER" char-name)
                                        (list :speed 96 :pitch 80 :bend 5))
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
                         (speech-speak line :atarivox
                                       :voice (voice-profile-for-name speaker))
                         (when interactive
                           ($ "~%  — Press Return to continue —")
                           (read-char stream))))))))))


(defun %setup-voice-params (char-name)
  "Load and apply voice parameters for CHAR-NAME to the AtariVox."
  (let* ((stats (cond
                  ((char= #\> (char char-name 0)) nil)
                  ((string-equal "NARRATOR" char-name)
                   (list :speed 96 :pitch 88 :bend 4))
                  ((string-equal "PLAYER" char-name)
                   (list :speed 96 :pitch 80 :bend 5))
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
         (speaker nil)
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
                  (setf speaker text)
                  (show nil text "")
                  (%setup-voice-params text))
                 (:parenthetical
                  (show nil nil text))
                 (:dialogue
                  (show nil nil text)
                  (handler-case
                      (progn
                        (speech-speak text :atarivox
                                      :voice (voice-profile-for-name speaker))
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

(defun convert-for-intellivoice (string)
  "Convert STRING into a list of IntelliVoice phoneme tokens"
  (ensure-intellivoice-dictionary)
  (let ((words (cl-ppcre:split "\\s+" (string-trim " " string))))
    (mapcar (lambda (word)
              (let* ((lookup (gethash (string-upcase word) *intellivoice-dictionary*))
                     (result (if lookup
                                 (split " " lookup)
                                 (intellivoice-basic-pronunciation word))))
                result))
            words)))
