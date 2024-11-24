(in-package :skyline-tool)

(defun read-forth-word ()
  (loop for char = (read-char *standard-input* nil nil)
        with word = (make-array 16 :element-type 'character :adjustable t :fill-pointer 0)
        if (or (null char) (member char '(#\Space #\Page #\Tab #\Newline)))
          do (return (cond ((emptyp word) nil)
                           ((every #'digit-char-p word) (parse-integer word))
                           (t word)))
        else do (vector-push-extend char word)))

(defvar *words* nil)
(defvar *forth-bootstrap-pathname* #p"Source/Scripts/Forth/Forth.fs")

(defun forth/colon ()
  (let ((name (read-forth-word)))
    (loop for word = (read-forth-word)
          with def = (list)
          do (cond ((null word)
                    (error "End of file while trying to define ~a" name))
                   ((string-equal ";" word)
                    (setf (gethash name *words*) (cons def nil))
                    (return-from forth/colon name))
                   (t
                    (appendf def word))))))

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

(defun forth/include ()
  (let ((pathname (parse-namestring (read-forth-word)
                                    :defaults (make-pathname
                                               :directory (list :relative "Source" "Scripts" "Forth")
                                               :type "fs"))))
    (format *trace-output* " including Forth ~a ")
    (with-input-from-file (fs pathname)
      (setf *words* (compile-forth-script)))))

(defun forth/comment-parens ()
  (loop for word = (read-forth-word)
        until (and (stringp word) (string= word ")"))))

(defun forth/trace ()
  (loop for char = (read-char *standard-input* nil nil)
        with string = (make-array 32 :type 'character :fill-pointer 0 :adjustable t)
        if (char= #\" char)
          do (let ((tag ))
               (format t "
~10t.section BankData
~a: .ptext \"~a\"
~10t.send"))
        else do (vector-push-extend char string)))

(defun initialize-forth-dictionary ()
  (let ((dict (make-hash-table :test 'equal)))
    (dolist (sdef '(
                    ("include" nil forth/include)
                    (":" nil forth/colon)
                    (".\"" nil forth/trace)
                    ("(" nil forth/comment-parens)
                    ("!" forth/bang)
                    ("c\"" nil forth/c-quote)
                    ))
      (destructuring-bind (word runtime &optional compile-time) sdef
        (setf (gethash word dict) (cons runtime compile-time))))
    (with-input-from-file (*standard-input* *forth-bootstrap-pathname*)
      (format *trace-output* " reading Forth bootstrap ~a … "
              (enough-namestring *forth-bootstrap-pathname*))
      (compile-forth-script :dictionary dict))))

(defun forth-eval (expr)
  (if (function expr)
      (funcall expr)
      (error "Can't eval Forth in compile-time context yet: ~a" expr)))

(defun compile-forth-script (&key (dictionary (initialize-forth-dictionary)))
  (let ((*words* dictionary))
    (format *trace-output* " compiling Forth …")
    (force-output *trace-output*)
    (loop for word = (read-forth-word)
          while word
          do (if (numberp word)
                 (format t "~&~10t.word $~2,'0x~20t; ~:*~:d" word)
                 (if-let (def (gethash word *words*))
                   (destructuring-bind (run . compile) def
                     (if compile
                         (forth-eval compile)
                         (format t "~&~10t.word Forth.~a" word)))
                   (error "Unprocessable word: “ ~a ” not in dictionary" word))))
    (format *trace-output* " ok")
    (force-output *trace-output*)
    *words*))
