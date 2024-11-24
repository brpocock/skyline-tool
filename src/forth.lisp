(in-package :skyline-tool)

(defun read-forth-word ()
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
          (warn "redefining ~a (now from ~a, previously from ~a~@[ ~a~]"
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
~a: .ptext \"~a\"
~10t.send"))
        else do (vector-push-extend char string)))

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
")
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
"
                       tag string)
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
  (loop for char = (read-char *standard-input* nil nil)
        with string = (make-array 32 :element-type 'character :fill-pointer 0 :adjustable t)
        if (char= #\" char)
          do (progn (format *trace-output* "~% ~a: ~a" *forth-file* string)
                    (return))
        else do (vector-push-extend char string)))

(defun initialize-forth-dictionary ()
  (let ((dict (make-hash-table :test 'equal)))
    (dolist (sdef '(("include" nil forth/include)
                    (":" nil forth/colon)
                    (".\"" nil forth/trace)
                    ("(" nil forth/comment-parens)
                    ("c\"" nil forth/c-quote)
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
      (funcall expr)
      (error "Can't eval Forth in compile-time context yet: ~a" expr)))

(defun mangle-word-for-internals (word)
  (format nil " Forth_~{~a~} "
          (loop for char across word
                collecting (if (alphanumericp char)
                               (string char)
                               (format nil "_~4,'0x" (char-code char))))))

(defun compile-forth-script (&key (dictionary (initialize-forth-dictionary)))
  (let ((*words* dictionary))
    (loop for word = (read-forth-word)
          while word
          do (if (numberp word)
                 (format t "~&~10t.word $~4,'0x~20t; ~:*~:d" word)
                 (if-let (def (gethash word *words*))
                   (destructuring-bind (run compile &optional metadata) def
                     (if compile
                         (forth-eval compile)
                         (format t " ( word ~a from ~a~@[ ~a~] )~{~%~10t.word ~a~}"
                                 word (getf metadata :source "?") (getf metadata :source-file nil) run)))
                   (if-let (num (ignore-errors (parse-integer word :radix *forth-base*)))
                     (format t (forth-number num))
                     (if-let (def (gethash (mangle-word-for-internals word) *words*))
                       (destructuring-bind (run compile &optional metadata) def
                         (if compile
                             (forth-eval compile)
                             (format t "~%;;;  ( word ~a from ~a~@[ ~a~] )~{~%~10t.word ~a~}"
                                     word (getf metadata :source "?") (getf metadata :source-file nil) run)))
                       (progn
                         (warn "Unknown word: ~a (mangles to: ~a)"
                               word (mangle-word-for-internals word))
                         (format t "~%~10t.word ~a~32t; unknown word ~a, hoping it's a constant"
                                 (mangle-word-for-internals word) word)))))))
    (format *trace-output* "~%( now, I know ~:d word~:p ) ok"
            (hash-table-count *words*))
    (force-output *trace-output*)
    *words*))
