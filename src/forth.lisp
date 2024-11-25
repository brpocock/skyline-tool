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
        finally (return-from read-forth-word
                  (cond ((emptyp word) nil)
                        ((every #'digit-char-p word) (parse-integer word))
                        (t word)))))

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
~a:
~10t.ptext \"~a\"
~10t.send

~10t.byte ForthPush
~10t.word ~a"
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
~a: ~{~%~10t.byte SpeakJet.~a~^, ~30tSpeakJet.~a~^, ~50tSpeakJet.~a~^~}
~10t.send

~10t.byte ForthPush
~10t.word ~a
"
                       tag string tag)
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
    (format t "~%~aBottom:" *forth-begin-pdl*)))

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
                (when elsep
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
                    ("SpeakJet[" nil forth/speakjet-quote)))
      (destructuring-bind (word runtime &optional compile-time) sdef
        (setf (gethash word dict) (list runtime compile-time (list :source :system)))))
    (with-input-from-file (*standard-input* *forth-bootstrap-pathname*)
      (let ((*forth-file* *forth-bootstrap-pathname*))
        (format *trace-output* "~% ( reading Forth bootstrap ~a ) "
                (enough-namestring *forth-bootstrap-pathname*))
        (compile-forth-script :dictionary dict)))
    (format *trace-output* " ( bootstrap complete )~2%")
    dict))

(defun forth-eval (expr)
  (if (function expr)
      (let ((value (funcall expr)))
        (when value
          (format nil "~%~10t.word ~a" value)))
      (error "Can't eval Forth in compile-time context yet: ~a" expr)))

(defun mangle-word-for-internals (word)
  (format nil "Forth_~{~a~}"
          (loop for char across word
                collecting (if (alphanumericp char)
                               (string char)
                               (format nil "_~4,'0x" (char-code char))))))

(defun forth-interpret (word)
  (if-let (def (or (presence (gethash word *words*))
                   (gethash (mangle-word-for-internals word) *words*)))
    (destructuring-bind (run compile &optional metadata) def
      #+ () (format *trace-output* "~& \ forth-interpret ~a found definition from ~a"
                    word (getf metadata :source))
      (if run
          (progn
            (format t "~%;;; start of ~a from ~a~@[ ~a~]"
                    word (getf metadata :source "?") (getf metadata :source-file nil))
            (let (*forth-input-stuffing*)
              (dolist (w (reverse (copy-list run)))
                (push w *forth-input-stuffing*))
              (loop while *forth-input-stuffing*
                    do (forth-interpret (read-forth-word))))
            (force-output *standard-output*)
            (format t "~%;;; end of ~a" word))
          (forth-eval compile)))
    ;; else: No def
    (if-let (num (ignore-errors (parse-integer word :radix *forth-base*)))
      (format t "~%~10t.byte ForthPush~%~10t.word $~4,'0x~32t; ~:*~d" (logand #xffff num))
      (tagbody top
         (restart-case
             (error "Unknown word: ~a (mangles to: ~a)"
                    word (mangle-word-for-internals word))
           (constant ()
             :report "Assume it is a constant that will be defined"
             (format t "~%~10t.byte ForthPush~%~10t.word Lib.~a~32t; unknown word ~a, hoping it's a constant"
                     (if (every (lambda (ch)
                                  (or (alphanumericp ch) (char= #\_ ch)))
                                word)
                         word
                         (mangle-word-for-internals word))
                     word))
           (function ()
             :report "Assume it is a function that will be called"
             (format t "~%~10t.byte ForthExecute~%~10t.word Lib.~a~32t; unknown word ~a, hoping it's a function"
                     (if (every (lambda (ch)
                                  (or (alphanumericp ch) (char= #\_ ch)))
                                word)
                         word
                         (mangle-word-for-internals word))
                     word))
           (words ()
             :report "Review the list of words in the Forth environment"
             (format *trace-output* "~{~a~^ ~}" (hash-table-keys *words*))
             (when (x11-p)
               (let ((words *words*))
                 (clim-simple-echo:run-in-simple-echo
                  (lambda () (format t "~{~a~^ ~}" (hash-table-keys words)))
                  :process-name "Forth Words")))
             (go top)))))))

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

