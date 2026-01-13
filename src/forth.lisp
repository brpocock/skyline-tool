(in-package :skyline-tool)

(defvar *forth-input-stuffing* nil)

(defun read-forth-word ()
  (when *forth-input-stuffing*
    (return-from read-forth-word (pop *forth-input-stuffing*))) 
  (when (member (peek-char nil *standard-input* nil)
                '(#\Space #\Page #\Tab #\Newline))
    (loop for char = (read-char *standard-input* nil nil)
          while (member (peek-char nil *standard-input* nil)
                        '(#\Space #\Page #\Tab #\Newline))))
  (loop for char = (read-char *standard-input* nil nil)
        with word = (make-array 16 :element-type 'character
                                   :adjustable t :fill-pointer 0)
        if (or (null char) (member char '(#\Space #\Page #\Tab #\Newline)))
          do (return-from read-forth-word (presence word))
        else do (vector-push-extend char word)
        finally (return-from read-forth-word (presence word))))

(defvar *words* nil)
(defparameter *forth-bootstrap-pathname* #p"Source/Scripts/Forth/Bootstrap.forth")
(defvar *forth-file* nil)
(defparameter *forth-base* 10)

(defun forth/colon ()
  (let ((name (read-forth-word)))
    (when-let (def (gethash name *words*))
      (destructuring-bind (runtime compiler meta) def
        (destructuring-bind (&key source source-file) meta
          (warn "redefining ~a (now from ~a, previously from ~a~@[ ~a~])"
                name *forth-file* source source-file))))
    (loop for word = (read-forth-word)
          with def = (list)
          do (cond ((null word)
                    (error "End of file while trying to define ~a" name))
                   ((string-equal ";" (princ-to-string word))
                    (setf (gethash name *words*) (list def nil
                                                       (list :source :file :source-file *forth-file*)))
                    (return-from forth/colon name))
                   (t
                    (appendf def (cons word nil)))))
    (error "colon-definition : ~a did not terminate properly with ;" word)))

(defun forth/c-quote ()
  (loop for char = (read-char *standard-input* nil nil)
        with string = (make-array 32 :element-type 'character :fill-pointer 0 :adjustable t)
        if (char= #\" char)
          do (let ((tag (dialogue-hash string)))
               (format t "
~10t.section BankData
~10t.weak
~10t~a~:*_DefinedP := false
~10t.endweak
~10t.if ! ~a~:*_DefinedP
~10t~a~:*_DefinedP := true
~a:
~10t.ptext \"~a\"
~10t.fi
~10t.send

~10t.byte ForthPushWord, <~a, >~:*~a
"
                       tag string tag)
               (return tag))
        else do (vector-push-extend char string)))

(defun forth/speakjet-quote ()
  (loop for word = (read-forth-word)
        with string = (list)
        if (equal word "]SpeakJet")
          do (let ((tag (dialogue-hash (reduce (curry #'concatenate 'string) string))))
               (format t "
~10t.section BankData
~10t.weak
~12t~a~:*_DefinedP := false
~10t.endweak
~10t.if ! ~a~:*_DefinedP
~12t~a~:*_DefinedP := true
~a: ~{~%~12t.byte ~a~^, ~30t~a~^, ~50t~a~^~}
~10t.fi
~10t.send

~10t.byte ForthPushWord, <~a, >~:*~a
"
                       tag
                       (mapcar (lambda (byte)
                                 (if (position #\$ byte)
                                     byte
                                     (format nil "SpeakJet.~a" byte)))
                               string)
                       tag)
               (return tag))
        else do (appendf string (cons word nil))))

(defun forth/discard-speech ()
  "Silently discard speech commands that aren't relevant for the current platform."
  (loop for word = (read-forth-word)
        ;; Skip all words until we find the closing bracket
        if (or (equal word "]SpeakJet") (equal word "]IntelliVoice"))
          do (return-from forth/discard-speech)))

(defun forth/intellivoice-quote ()
  (loop for word = (read-forth-word)
        with string = (list)
        if (equal word "]IntelliVoice")
          do (let ((tag (format nil "~a_IV" (dialogue-hash (reduce (curry #'concatenate 'string) string)))))
               (format t "
~10t.section BankData
~10t.weak
~12t~a~:*_DefinedP := false
~10t.endweak
~10t.if ! ~a~:*_DefinedP
~12t~a~:*_DefinedP := true
~a: ~{~%~12t.byte ~a~^, ~30t~a~^, ~50t~a~^~}
~10t.fi
~10t.send

~10t.byte ForthPushWord, <~a, >~:*~a
"
                       tag
                       (mapcar (lambda (byte)
                                 (if (position #\$ byte)
                                     byte
                                     (format nil "IntelliVoice.~a" byte)))
                               string)
                       tag)
               (return tag))
        else do (appendf string (cons word nil))))

(defun forth/include (&optional (name (read-forth-word)))
  (let* ((script-pathname (merge-pathnames (parse-namestring name)
                                           (make-pathname
                                            :directory (list :relative "Source" "Scripts" "Forth")
                                            :type "forth")))
         (generated-pathname (merge-pathnames (parse-namestring name)
                                              (make-pathname
                                               :directory (list :relative "Source" "Generated")
                                               :type "forth")))
         (pathname (cond ((probe-file script-pathname) script-pathname)
                         ((probe-file generated-pathname) generated-pathname)
                         (t (error "Can't find “~a” to include,~%tried ~a~%and ~a"
                                   name
                                   (enough-namestring script-pathname)
                                   (enough-namestring generated-pathname))))))
    (format *trace-output* " ( including ~a ) " (enough-namestring pathname))
    (with-input-from-file (*standard-input* pathname)
      (let ((*forth-file* pathname))
        (setf *words* (compile-forth-script :dictionary *words*))
        (format *trace-output* " ( done with ~a ) " (enough-namestring pathname))))))

(defun forth/comment-parens ()
  (loop for word = (read-forth-word)
        until (and (stringp word) (string= word ")"))))

(defun forth/trace ()
  (loop for word = (read-forth-word)
        with string = ""
        if (char= #\" (last-elt word))
          do (progn (format *trace-output* "~% ~a: \"~a ~a" *forth-file* string word)
                    (return))
        else do (setf string (format nil "~a ~a" string word))))

(defvar *forth-begin-pdl*)

(defun forth/begin ()
  (let ((*forth-begin-pdl* (genlabel "Loop")))
    (format t "~%~aTop:" *forth-begin-pdl*)
    (loop for word = (read-forth-word)
          until (string= word "REPEAT")
          do (forth-interpret word))
    (format t "~%~10t.byte ForthGo~%~10t.word ~aTop~:*~%~aBottom:" *forth-begin-pdl*)))

(defun forth/leave ()
  (format t "~%~10t.byte ForthGo~%~10t.word ~aBottom" *forth-begin-pdl*))

(defun forth/repeat ()
  (format t "~%~10t.byte ForthGo~%~10t.word ~aTop" *forth-begin-pdl*))

(defun forth/while ()
  (format t "~%~10t.byte ForthUnless~%~10t.word ~aBottom" *forth-begin-pdl*))

(defun forth/until ()
  (format t "~%~10t.byte ForthWhen~%~10t.word ~aBottom" *forth-begin-pdl*))

(defun forth/if ()
  (let ((*forth-begin-pdl* (genlabel "If")))
    (format t "~%~10t.byte ForthUnless~%~10t.word ~aElse" *forth-begin-pdl*)
    (loop for word = (read-forth-word)
          with elsep = nil
          do (cond
               ((string= word "ELSE")
                (format t "
~10t.byte ForthGo
~10t.word ~aThen
~:*~aElse:"
                        *forth-begin-pdl*)
                (setf elsep t))
               ((string= word "THEN")
                (unless elsep
                  (format t "~%~aElse:" *forth-begin-pdl*))
                (format t "~%~aThen:" *forth-begin-pdl*)
                (return))
               (t (forth-interpret word))))))

(defun initialize-forth-dictionary ()
  (let ((dict (make-hash-table :test 'equal)))
    (dolist (sdef `(("include" nil forth/include)
                    (":" nil forth/colon)
                    ("DUP" nil ,(lambda () (format t "~%~10t.byte ForthDup")))
                    ("C!" nil ,(lambda () (format t "~%~10t.byte ForthSetByte")))
                    ("C@" nil ,(lambda () (format t "~%~10t.byte ForthGetByte")))
                    ("!" nil ,(lambda () (format t "~%~10t.byte ForthSetWord")))
                    ("@" nil ,(lambda () (format t "~%~10t.byte ForthGetWord")))
                    ("SWAP" nil ,(lambda () (format t "~%~10t.byte ForthSwap")))
                    ("DROP" nil ,(lambda () (format t "~%~10t.byte ForthDrop")))
                    ("EXECUTE" nil ,(lambda () (format t "~%~10t.byte ForthExecute")))
                    ("BEGIN" nil forth/begin)
                    ("WHILE" nil forth/while)
                    ("UNTIL" nil forth/until)
                    ("REPEAT" nil forth/repeat)
                    ("LEAVE" nil forth/leave)
                    ("IF" nil forth/if)
                    (".\"" nil forth/trace)
                    ("(" nil forth/comment-parens)
                    ("C\"" nil forth/c-quote)
                    ;; Platform-specific speech commands will be added dynamically
                    )
      (destructuring-bind (word runtime &optional compile-time) sdef
        (setf (gethash word dict) (list runtime compile-time (list :source :system)))))
    (with-input-from-file (*standard-input* *forth-bootstrap-pathname*)
      (let ((*forth-file* *forth-bootstrap-pathname*))
        (format *trace-output* "~% ( reading Forth bootstrap ~a ) "
                (enough-namestring *forth-bootstrap-pathname*))
        (compile-forth-script :dictionary dict)))

    ;; Add platform-specific speech commands after bootstrap
    (ecase *machine*
      (7800 ;; Atari 7800 uses SpeakJet
       (setf (gethash "SpeakJet[" dict)
             (list nil 'forth/speakjet-quote (list :source :system)))
       ;; Discard IntelliVoice commands silently
       (setf (gethash "IntelliVoice[" dict)
             (list nil 'forth/discard-speech (list :source :system))))
      (2609 ;; Intellivision uses IntelliVoice
       (setf (gethash "IntelliVoice[" dict)
             (list nil 'forth/intellivoice-quote (list :source :system)))
       ;; Discard SpeakJet commands silently
       (setf (gethash "SpeakJet[" dict)
             (list nil 'forth/discard-speech (list :source :system)))))

    (format *trace-output* " ( bootstrap complete )~2%")
    dict))

(defun forth-eval (expr)
  (if (function expr)
      (let ((value (funcall expr)))
        (when value
          (format nil "~%~10t.word ~a" value)))
      (error "Can't eval Forth in compile-time context yet: ~a" expr)))

(defun mangle-word-for-internals (word)
  "Mangle WORD into the form that an internal (assembly) implementation of a word would appear as"
  (format nil "Forth_~{~a~}"
          (loop for char across word
                collecting (if (alphanumericp char)
                               (string char)
                               (format nil "_~4,'0x" (char-code char))))))

(defun forth-interpret (word)
  (if-let (def (presence (gethash word *words*)))
    (destructuring-bind (run compile &optional metadata) def
      #+ () (format *trace-output* "~& \ forth-interpret ~a found definition from ~a"
                    word (getf metadata :source))
      (if run
          (progn
            (if-let (num (and (= 1 (length run))
                              (or (numberp (first run))
                                  (ignore-errors
                                   (parse-integer (first run) :radix *forth-base*)))))
              (if (< num #x100)
                  (format t "~%~10t.byte ForthPushByte, $~2,'0x ; $~2,'0x ~:*~3d constant: ~a"
                          (logand #xff num)
                          (logand #xff num)
                          word)
                  (format t "~%~10t.byte ForthPushWord, $~2,'0x, $~2,'0x ; $~4,'0x ~:*~5d constant: ~a"
                          (logand #xff num)
                          (ash (logand #xff00 num) -8)
                          (logand #xffff num)
                          word))
              (let (*forth-input-stuffing*)
                (when (< 1 (length run))
                  (format t "~%;;; start of ~a from ~a~@[ ~a~]"
                          word (getf metadata :source "?") (getf metadata :source-file nil)))
                (dolist (w (reverse (copy-list run)))
                  (push w *forth-input-stuffing*))
                (loop while *forth-input-stuffing*
                      do (forth-interpret (read-forth-word)))
                (when (< 1 (length run))
                  (format t "~%;;; end of ~a" word))))
            (force-output *standard-output*))
          (forth-eval compile)))
    ;; else: No def
    (if-let (asm-def (gethash (mangle-word-for-internals word) *words*))
      (destructuring-bind (run _compile &optional metadata) asm-def
        (if run
            (let ((num (parse-integer (first run))))
              (format t "~%~10t.byte ForthPushWord, $~2,'0x, $~2,'0x, ForthExecute ; $~4,'0x ~:*~5d internal: ~a (~a)"
                      (logand #xff num)
                      (ash (logand #xff00 num) -8)
                      (logand #xffff num)
                      word (mangle-word-for-internals word)))
            (forth-eval compile)))
      (if-let (num (ignore-errors (parse-integer word :radix *forth-base*)))
        (if (< num #x100)
            (format t "~%~10t.byte ForthPushByte, $~2,'0x ; $~2,'0x ~:*~3d"
                    (logand #xff num)
                    (logand #xff num))
            (format t "~%~10t.byte ForthPushWord, $~2,'0x, $~2,'0x~32t ; $~4,'0x ~:*~5d"
                    (logand #xff num)
                    (ash (logand #xff00 num) -8)
                    (logand #xffff num)))
        (tagbody top
           (restart-case
               (error "Unknown word: ~a~%(mangles to: ~a)"
                      word (mangle-word-for-internals word))
             (continue ()
               :report "Continue, using a literal zero"
               (format t "~%~10t.byte ForthPushByte, 0~32t; XXX unknown word ~a (~a)"
                       word
                       (mangle-word-for-internals word)))
             (words ()
               :report "Review the list of words in the Forth environment"
               (format *trace-output* "~{~a~^ ~}" (hash-table-keys *words*))
               (when (x11-p)
                 (let ((words *words*))
                   (clim-simple-echo:run-in-simple-echo
                    (lambda () (format t "~{~a~^ ~}" (hash-table-keys words)))
                    :process-name "Forth Words")))
               (go top))))))))

(defun compile-forth-script (&key (dictionary (initialize-forth-dictionary)))
  (let ((*words* dictionary))
    (loop for word = (read-forth-word)
          while word
          do (forth-interpret word))
    (format *trace-output* "~%( now, I know ~:d word~:p ) ok"
            (hash-table-count *words*))
    #| crazy-level debug dump |#
    #+ () (format *trace-output* "~2%~{~a~^ ~}~2%" (hash-table-keys *words*))
    #+ () (dolist (word (hash-table-keys *words*))
            (format *trace-output* "~% : ~a ~{~a~^ ~} ;" word (car (gethash word *words*))))
    (force-output *trace-output*)
    *words*))

;;; Error conditions for Forth compilation

(define-condition forth-compilation-error (error)
  ((forth-file :initarg :forth-file :reader forth-compilation-error-forth-file)
   (output-file :initarg :output-file :reader forth-compilation-error-output-file)
   (architecture :initarg :architecture :reader forth-compilation-error-architecture)
   (context :initarg :context :reader forth-compilation-error-context))
  (:report (lambda (condition stream)
             (format stream "Forth compilation error in ~A for ~A architecture~@[ at ~A~]"
                     (forth-compilation-error-forth-file condition)
                     (forth-compilation-error-architecture condition)
                     (forth-compilation-error-context condition)))))

(define-condition forth-syntax-error (forth-compilation-error)
  ((invalid-token :initarg :invalid-token :reader forth-syntax-error-invalid-token)
   (expected-tokens :initarg :expected-tokens :reader forth-syntax-error-expected-tokens))
  (:report (lambda (condition stream)
             (format stream "Forth syntax error: invalid token '~A'~@[ in ~A~]. ~
                           Expected: ~{~A~^, ~}"
                     (forth-syntax-error-invalid-token condition)
                     (forth-compilation-error-context condition)
                     (forth-syntax-error-expected-tokens condition)))))

(define-condition forth-undefined-word (forth-compilation-error)
  ((word-name :initarg :word-name :reader forth-undefined-word-word-name)
   (available-words :initarg :available-words :reader forth-undefined-word-available-words))
  (:report (lambda (condition stream)
             (format stream "Undefined Forth word '~A'~@[ in ~A~]. ~
                           Available words: ~{~A~^, ~}"
                     (forth-undefined-word-word-name condition)
                     (forth-compilation-error-context condition)
                     (forth-undefined-word-available-words condition)))))

;;; Forth bytecode compilation functions

(defun compile-forth-6502 (forth-file output-file)
  "Compile Forth program to 6502 assembly code (64tass format).

FORTH-FILE: Input Forth source file
OUTPUT-FILE: Output assembly file

Generates 6502 assembly code with proper error handling for syntax and semantic issues."
  (handler-case
      (with-open-file (forth-stream forth-file :direction :input)
        (with-open-file (output-stream output-file :direction :output :if-exists :supersede)
          (format output-stream ";;; 6502 Forth bytecode compilation from ~A~%;;; Generated automatically - do not edit~2%"
                  (enough-namestring forth-file))
          ;; Write standard Forth runtime header for 6502
          (format output-stream ";;; 6502 Forth Runtime~2%")
          (format output-stream ".cpu \"6502\"~2%")
          (format output-stream ";;; Data stack and return stack pointers~%")
          (format output-stream "DSP = $~4,'0X~%" #x100)  ; Data stack pointer
          (format output-stream "RSP = $~4,'0X~2%" #x1FF) ; Return stack pointer

          ;; Compile Forth words to assembly
          (let ((word-count 0))
            (loop for line = (read-line forth-stream nil)
                  while line
                  do (incf word-count)
                     (handler-case
                         (compile-forth-line-6502 line output-stream word-count)
                       (forth-compilation-error (e)
                         (error 'forth-compilation-error
                                :forth-file forth-file
                                :output-file output-file
                                :architecture :6502
                                :context (format nil "line ~D: ~A" word-count (forth-compilation-error-context e)))))))

          (format output-stream "~2%;;; End of compiled Forth program~%")
          (format *trace-output* "~&6502 Forth compilation complete: ~A words processed" word-count)))
    (file-error (e)
      (error 'forth-compilation-error
             :forth-file forth-file
             :output-file output-file
             :architecture :6502
             :context "file access"))))

(defun compile-forth-z80 (forth-file output-file)
  "Compile Forth program to Z80 assembly code (z80asm format).

FORTH-FILE: Input Forth source file
OUTPUT-FILE: Output assembly file

Generates Z80 assembly code with proper error handling."
  (handler-case
      (with-open-file (forth-stream forth-file :direction :input)
        (with-open-file (output-stream output-file :direction :output :if-exists :supersede)
          (format output-stream ";;; Z80 Forth bytecode compilation from ~A~%;;; Generated automatically - do not edit~2%"
                  (enough-namestring forth-file))
          ;; Write standard Forth runtime header for Z80
          (format output-stream ";;; Z80 Forth Runtime~2%")
          (format output-stream ".cpu \"z80\"~2%")
          (format output-stream ";;; Data stack and return stack pointers~%")
          (format output-stream "DSP: equ $~4,'0X~%" #xFF00)  ; Data stack pointer
          (format output-stream "RSP: equ $~4,'0X~2%" #xFFFF) ; Return stack pointer

          ;; Compile Forth words to assembly
          (let ((word-count 0))
            (loop for line = (read-line forth-stream nil)
                  while line
                  do (incf word-count)
                     (handler-case
                         (compile-forth-line-z80 line output-stream word-count)
                       (forth-compilation-error (e)
                         (error 'forth-compilation-error
                                :forth-file forth-file
                                :output-file output-file
                                :architecture :z80
                                :context (format nil "line ~D: ~A" word-count (forth-compilation-error-context e)))))))

          (format output-stream "~2%;;; End of compiled Forth program~%")
          (format *trace-output* "~&Z80 Forth compilation complete: ~A words processed" word-count)))
    (file-error (e)
      (error 'forth-compilation-error
             :forth-file forth-file
             :output-file output-file
             :architecture :z80
             :context "file access"))))

(defun compile-forth-cp1610 (forth-file output-file)
  "Compile Forth program to CP1610 assembly code (as1600 format).

FORTH-FILE: Input Forth source file
OUTPUT-FILE: Output assembly file

Generates CP1610 assembly code with proper error handling."
  (handler-case
      (with-open-file (forth-stream forth-file :direction :input)
        (with-open-file (output-stream output-file :direction :output :if-exists :supersede)
          (format output-stream ";;; CP1610 Forth bytecode compilation from ~A~%;;; Generated automatically - do not edit~2%"
                  (enough-namestring forth-file))
          ;; Write standard Forth runtime header for CP1610
          (format output-stream ";;; CP1610 Forth Runtime~2%")
          (format output-stream ";;; Data stack and return stack pointers~%")
          (format output-stream "DSP: equ $~4,'0X~%" #x2F0)  ; Data stack pointer
          (format output-stream "RSP: equ $~4,'0X~2%" #x2FF) ; Return stack pointer

          ;; Compile Forth words to assembly
          (let ((word-count 0))
            (loop for line = (read-line forth-stream nil)
                  while line
                  do (incf word-count)
                     (handler-case
                         (compile-forth-line-cp1610 line output-stream word-count)
                       (forth-compilation-error (e)
                         (error 'forth-compilation-error
                                :forth-file forth-file
                                :output-file output-file
                                :architecture :cp1610
                                :context (format nil "line ~D: ~A" word-count (forth-compilation-error-context e)))))))

          (format output-stream "~2%;;; End of compiled Forth program~%")
          (format *trace-output* "~&CP1610 Forth compilation complete: ~A words processed" word-count)))
    (file-error (e)
      (error 'forth-compilation-error
             :forth-file forth-file
             :output-file output-file
             :architecture :cp1610
             :context "file access"))))
;; Platform-specific bytecode emission functions (stub implementations)
;; These functions emit Forth bytecodes and other source files in native format

;; 65xx family (64tass assembly)
(defun emit-6502-bytecode (forth-code output-file)
  "Emit Forth bytecode for 65xx family CPUs in 64tass format"
  (error "6502 bytecode emission not yet implemented"))

(defun emit-6507-bytecode (forth-code output-file)
  "Emit Forth bytecode for 6507 in 64tass format"
  (error "6507 bytecode emission not yet implemented"))

(defun emit-6510-bytecode (forth-code output-file)
  "Emit Forth bytecode for 6510 in 64tass format"
  (error "6510 bytecode emission not yet implemented"))

(defun emit-8502-bytecode (forth-code output-file)
  "Emit Forth bytecode for 8502 in 64tass format"
  (error "8502 bytecode emission not yet implemented"))

(defun emit-65c02-bytecode (forth-code output-file)
  "Emit Forth bytecode for 65C02 in 64tass format"
  (error "65C02 bytecode emission not yet implemented"))

(defun emit-65sc02-bytecode (forth-code output-file)
  "Emit Forth bytecode for 65SC02 in 64tass format"
  (error "65SC02 bytecode emission not yet implemented"))

;; CP1610 (as1600 assembly)
(defun emit-cp1610-bytecode (forth-code output-file)
  "Emit Forth bytecode for CP1610 in as1600 format"
  (error "CP1610 bytecode emission not yet implemented"))

;;; Line compilation functions for each architecture

(defun compile-forth-line-6502 (line output-stream line-number)
  "Compile a single line of Forth code to 6502 assembly."
  (let ((tokens (split-sequence #\Space line :remove-empty-subseqs t)))
    (dolist (token tokens)
      (cond
        ;; Numbers (literals)
        ((every #'digit-char-p token)
         (let ((value (parse-integer token)))
           (when (> value 65535)
             (error 'forth-syntax-error
                    :context (format nil "literal value ~A too large for 16-bit" value)
                    :invalid-token token
                    :expected-tokens '("16-bit value")))
           (format output-stream "  lda #<~A~%" value)
           (format output-stream "  sta DSP~%")
           (format output-stream "  lda #>~A~%" value)
           (format output-stream "  sta DSP+1~%")))

        ;; Basic Forth words
        ((string-equal token "+")
         (format output-stream "  ;; Addition~%")
         (format output-stream "  clc~%")
         (format output-stream "  lda DSP~%")
         (format output-stream "  adc DSP+2~%")
         (format output-stream "  sta DSP+2~%")
         (format output-stream "  lda DSP+1~%")
         (format output-stream "  adc DSP+3~%")
         (format output-stream "  sta DSP+3~%"))

        ((string-equal token "dup")
         (format output-stream "  ;; DUP~%")
         (format output-stream "  lda DSP~%")
         (format output-stream "  sta DSP-2~%")
         (format output-stream "  lda DSP+1~%")
         (format output-stream "  sta DSP-1~%"))

        ((string-equal token "drop")
         (format output-stream "  ;; DROP~%")
         (format output-stream "  lda DSP+2~%")
         (format output-stream "  sta DSP~%")
         (format output-stream "  lda DSP+3~%")
         (format output-stream "  sta DSP+1~%"))

        ;; Unknown token
        (t (error 'forth-undefined-word
                  :context (format nil "token '~A'" token)
                  :word-name token
                  :available-words '("numbers" "+" "dup" "drop")))))))

(defun compile-forth-line-z80 (line output-stream line-number)
  "Compile a single line of Forth code to Z80 assembly."
  (let ((tokens (split-sequence #\Space line :remove-empty-subseqs t)))
    (dolist (token tokens)
      (cond
        ;; Numbers (literals)
        ((every #'digit-char-p token)
         (let ((value (parse-integer token)))
           (when (> value 65535)
             (error 'forth-syntax-error
                    :context (format nil "literal value ~A too large for 16-bit" value)
                    :invalid-token token
                    :expected-tokens '("16-bit value")))
           (format output-stream "  ;; Push literal ~A~%" value)
           (format output-stream "  ld hl, ~A~%" value)
           (format output-stream "  push hl~%")))

        ;; Basic Forth words
        ((string-equal token "+")
         (format output-stream "  ;; Addition~%")
         (format output-stream "  pop hl~%")
         (format output-stream "  pop de~%")
         (format output-stream "  add hl, de~%")
         (format output-stream "  push hl~%"))

        ((string-equal token "dup")
         (format output-stream "  ;; DUP~%")
         (format output-stream "  pop hl~%")
         (format output-stream "  push hl~%")
         (format output-stream "  push hl~%"))

        ((string-equal token "drop")
         (format output-stream "  ;; DROP~%")
         (format output-stream "  pop hl~%"))

        ;; Unknown token
        (t (error 'forth-undefined-word
                  :context (format nil "token '~A'" token)
                  :word-name token
                  :available-words '("numbers" "+" "dup" "drop")))))))

(defun compile-forth-line-cp1610 (line output-stream line-number)
  "Compile a single line of Forth code to CP1610 assembly."
  (let ((tokens (split-sequence #\Space line :remove-empty-subseqs t)))
    (dolist (token tokens)
      (cond
        ;; Numbers (literals)
        ((every #'digit-char-p token)
         (let ((value (parse-integer token)))
           (when (> value 65535)
             (error 'forth-syntax-error
                    :context (format nil "literal value ~A too large for 16-bit" value)
                    :invalid-token token
                    :expected-tokens '("16-bit value")))
           (format output-stream "  ;; Push literal ~A~%" value)
           (format output-stream "  mvi #~A, r0~%" (logand value #xFF))
           (format output-stream "  mvi #~A, r1~%" (ash value -8))
           (format output-stream "  pshr r0~%")
           (format output-stream "  pshr r1~%")))

        ;; Basic Forth words
        ((string-equal token "+")
         (format output-stream "  ;; Addition~%")
         (format output-stream "  pulr r0~%")
         (format output-stream "  pulr r1~%")
         (format output-stream "  pulr r2~%")
         (format output-stream "  pulr r3~%")
         (format output-stream "  addr r0, r2~%")
         (format output-stream "  addr r1, r3~%")
         (format output-stream "  pshr r2~%")
         (format output-stream "  pshr r3~%"))

        ((string-equal token "dup")
         (format output-stream "  ;; DUP~%")
         (format output-stream "  pulr r0~%")
         (format output-stream "  pulr r1~%")
         (format output-stream "  pshr r0~%")
         (format output-stream "  pshr r1~%")
         (format output-stream "  pshr r0~%")
         (format output-stream "  pshr r1~%"))

        ((string-equal token "drop")
         (format output-stream "  ;; DROP~%")
         (format output-stream "  pulr r0~%")
         (format output-stream "  pulr r1~%"))

        ;; Unknown token
        (t (error 'forth-undefined-word
                  :context (format nil "token '~A'" token)
                  :word-name token
                  :available-words '("numbers" "+" "dup" "drop")))))))

;; Z80 family (z80asm assembly)
(defun emit-z80-bytecode (forth-code output-file)
  "Emit Forth bytecode for Z80 in z80asm format"
  (error "Z80 bytecode emission not yet implemented"))

(defun emit-z80a-bytecode (forth-code output-file)
  "Emit Forth bytecode for Z80A in z80asm format"
  (error "Z80A bytecode emission not yet implemented"))

;; HuC6280 (NES/SNES)
(defun emit-huc6280-bytecode (forth-code output-file)
  "Emit Forth bytecode for HuC6280 in appropriate format"
  (error "HuC6280 bytecode emission not yet implemented"))
