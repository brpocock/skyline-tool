(in-package :skyline-tool)

(defun load-class-defs (&optional (path "../Source/Classes/Classes.Defs"))
  "Parse Classes.Defs and return a list of class plists.
   Format: ClassName < ParentClass  (class with inheritance)
           .SlotName Size = annotation  (slot)
           #MethodName                  (method)
           ; comment"
  (let ((classes nil) (class-names nil) (class-info nil) (class-name nil)
        (parent nil) (slots nil) (methods nil))
    (labels ((commit-class ()
               (when class-name
                 (push (list :name class-name :parent parent
                             :slots (nreverse slots)
                             :methods (nreverse methods))
                       classes)
                 (push class-name class-names)
                 (setf class-name nil parent nil slots nil methods nil)))))
      (let ((full-path (merge-pathnames path (uiop:getcwd))))
        (when (probe-file full-path)
          (with-open-file (f full-path)
            (loop for line = (read-line f nil nil)
                  while line
                  do (let ((trimmed (string-trim " " line)))
                       (cond
                         ;; Skip blank lines and comments
                         ((or (zerop (length trimmed))
                              (char= (char trimmed 0) #\;)))
                         ;; Class definition: Name < Parent
                         ((search " < " trimmed)
                          (commit-class)
                          (let ((parts (split-sequence " < " trimmed)))
                            (setf class-name (string-trim " " (first parts))
                                  parent (string-trim " " (second parts)))))
                         ;; Method: #MethodName
                         ((char= (char trimmed 0) #\#)
                          (push (subseq trimmed 1) methods))
                         ;; Slot: .SlotName size = annotation
                         ((char= (char trimmed 0) #\.)
                          (let* ((rest (subseq trimmed 1))
                                 (parts (split-sequence #\Space rest))
                                 (sname (first parts))
                                 (ssize (when (> (length parts) 1) (second parts)))
                                 (sannot (when (> (length parts) 2)
                                           (subseq rest (position #\= rest)))))
                            (push (list :name sname :size ssize :annotation sannot) slots)))
                         ;; Bare name without < : class with no parent
                         (t
                          (commit-class)
                          (setf class-name (string-trim " " trimmed)
                                parent nil))))))
            (commit-class))))
  (values (nreverse classes) (nreverse class-names)))

(defun find-method-in-cob (method-name)
  "Find METHOD-NAME in .cob files, return (file line-number) or nil."
  (let* ((files (directory #p"../Source/Classes/*.cob"))
         (patterns (list (format nil "METHOD-ID. \"~a\"" method-name)
                         (format nil "METHOD-ID. ~a" method-name))))
    (dolist (file files)
      (with-open-file (f file)
        (loop for line = (read-line f nil nil)
              for line-num from 1
              while line
              do (when (some (lambda (p) (search p line :test #'char-equal)) patterns)
                   (return-from find-method-in-cob
                     (list (namestring (truename file)) line-num))))))
    nil))

(defun oops-class-inspector ()
  "Open a class hierarchy browser window."
  (multiple-value-bind (classes class-names) (load-class-defs)
    (clim-simple-echo:run-in-simple-echo
     (lambda ()
       (format t "~&~a Class Hierarchy~2%" (or (and (boundp '*game-title*) *game-title*) "Game"))
       (dolist (c classes)
         (let ((name (getf c :name))
               (parent (getf c :parent))
               (slots (getf c :slots))
               (methods (getf c :methods)))
           (format t "~&~a" name)
           (when parent (format t "  <  ~a" parent))
           (terpri)
           (when slots
             (format t "  Slots:~%")
             (dolist (s slots)
               (format t "    .~a (~a)~@[  ~a~]~%"
                       (getf s :name) (or (getf s :size) "?")
                       (getf s :annotation))))
           (when methods
             (format t "  Methods:~%")
             (dolist (m methods)
               (format t "    #~a" m)
               (let ((loc (find-method-in-cob m)))
                 (if loc
                     (format t "  [~a:~d]" (enough-namestring (first loc)) (second loc))
                     (format t "  [not in .cob]")))
               (terpri)))
           (terpri))))))) 
