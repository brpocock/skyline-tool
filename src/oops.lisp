(in-package :skyline-tool)

(defun make-classes-for-oops (&optional (class-defs-pathname #p"./Source/Classes/Classes.Defs"))
  "Writes ClassConstants from CLASS-DEFS-PATHNAME"
  (let ((all-classes-sequentially (list)))
    (with-input-from-file (class-file class-defs-pathname)
      (ensure-directories-exist #p "./Source/Generated/")
      (with-output-to-file (class-methods #p"./Source/Generated/ClassMethods.s"
                                          :if-exists :supersede)
        (format class-methods ";;; ClassMethods derived from ~s~2%"
                (enough-namestring class-defs-pathname))
        (with-output-to-file (class-constants #p"./Source/Generated/ClassConstants.s"
                                              :if-exists :supersede)
          (format class-constants ";;; ClassConstants derived from ~s~2%"
                  (enough-namestring class-defs-pathname))
          (format class-constants "
;;; BasicObject class (builtin to OOPS concept)
~10tBasicObjectClass = $01
~10tClassMethodsPointerOffset = 0

~10tCallBasicObjectClassP = $00
")
          (let ((methods-set (make-hash-table :test 'equal))
                (class-bases (make-hash-table :test 'equal))
                (class-size (make-hash-table :test #'equal)))
            (setf (gethash "BasicObject" class-bases) nil
                  (gethash "BasicObject" class-size) 2)
            (let ((basic-object-methods (make-hash-table :test 'equal)))
              (setf (gethash "ClassP" basic-object-methods) "BasicObject"
                    (gethash "BasicObject" methods-set) basic-object-methods))
            (labels
                ((finalize-oops-class (class-name last-slot-offset)
                   (let ((class-final-size last-slot-offset)
                         (methods (gethash class-name methods-set)))
                     (setf (gethash class-name class-size) class-final-size)
                     (format class-constants "~%~10t~aSize = $~2,'0x"
                             class-name class-final-size)
                     (format class-methods "~2%~aClassMethods:~%"
                             class-name (* 3 (hash-table-count methods)))
                     (dolist (method (hash-table-keys methods))
                       (let* ((original-class (gethash method methods))
                              (class-ancestry
                                (append
                                 (loop with class = class-name
                                       until (string= class original-class)
                                       collecting class
                                       do (setf class (gethash class class-bases)))
                                 (list original-class))))
                         (format class-methods "
~10t.weak~
~{~%~12tMethod~a~a := 0~}
~10t.endweak"
                                 (mapcan (lambda (class) (list class method))
                                         class-ancestry))
                         (format class-methods "
* = ~aClassMethods + Call~a~a
Invoke~a~a:"
                                 class-name original-class method
                                 class-name method)
                         (dolist (class class-ancestry)
                           (format class-methods "
~10t.if Method~a~a > 0
~12tjmp Method~a~a
~10t.else"
                                   class method class method))
                         (format class-methods "
~12t.error ~
\"There is no implementation of the generic function Method~a~a ~
and no ancestor provides an implementation (searched ~{~a~^, ~})\""
                                 class-name method class-ancestry)
                         (dotimes (_ (length class-ancestry))
                           (format class-methods "~%~10t.fi"))))
                     (let ((class-ancestry
                             (loop with class = class-name
                                   while class
                                   collecting class
                                   do (setf class (gethash class class-bases)))))
                       (format class-methods "
* = ~aClassMethods + ~d

Method~aClassP: .block
~10tlda Class~{
~10tcmp # ~aClass
~10tbeq Found~}
~10tlda # 0
Found:
~10tsta Class + 1
~10trts
~10t.bend~%"
                               class-name (* 3 (hash-table-count methods))
                               class-name class-ancestry)))))
              (loop with parent-class = "BasicObject" with current-class = "BasicObject"
                    with class-index = 1 with slot-offset = 2
                    for line = (read-line class-file nil nil) while line
                    do (cond
                         ;; blank line
                         ((emptyp line)
                          (fresh-line class-constants)
                          (fresh-line class-methods))
                         ;; comment
                         ((char= #\; (char line 0))
                          (fresh-line class-constants)
                          (princ line class-constants)
                          (fresh-line class-methods)
                          (princ line class-methods))
                         ;; method name
                         ((char= #\# (char line 0))
                          (if current-class
                              (let ((name (string-trim " " (subseq line 1)))
                                    (methods (gethash current-class methods-set)))
                                (setf (gethash name methods) current-class)
                                (format class-constants "~%~10tCall~a~a = $~2,'0x"
                                        current-class name (* 3 (1- (hash-table-count methods)))))
                              (cerror "Continue, ignoring"
                                      "Ignoring method without class: ~s" line)))
                         ;; slot name & size
                         ((char= #\. (char line 0))
                          (if current-class
                              (destructuring-bind (name size$)
                                  (split-sequence #\Space (subseq line 1)
                                                  :remove-empty-subseqs t)
                                (let ((size (if (char= #\$ (char size$ 0))
                                                (parse-integer (subseq size$ 1) :radix 16)
                                                (parse-integer size$))))
                                  (format class-constants "
~10t~a~a = $~2,'0x~@[~32t; … $~2,'0x~]"
                                          current-class name slot-offset
                                          (when (/= 1 size)
                                            (1- (+ slot-offset size))))
                                  (incf slot-offset size)))
                              (cerror "Continue, ignoring"
                                      "Ignoring slot without class: ~s" line)))
                         ;; class definition
                         ((find #\< line)
                          (destructuring-bind (new-class old-class)
                              (mapcar (curry #'string-trim #(#\Space))
                                      (split-sequence #\< line))
                            (push new-class all-classes-sequentially)
                            (let ((prior-class current-class))
                              (when prior-class
                                (finalize-oops-class prior-class slot-offset))
                              (setf current-class new-class
                                    parent-class old-class
                                    slot-offset (gethash old-class class-size)
                                    (gethash new-class class-bases) old-class

                                    (gethash new-class methods-set)
                                    (copy-hash-table (gethash old-class methods-set)))
                              (finish-output)
                              (unless slot-offset
                                (error "Could not find parent class ~s in ~s"
                                       old-class class-size)))
                            (format class-constants "~%
~10t;; class ~a (parent: ~a)
~10t~aClass = $~2,'0x~%"
                                    current-class parent-class
                                    current-class (incf class-index))))
                         ;; anything else
                         (t (error "Unrecognized line in class definitions: ~s" line)))
                    finally
                       (when current-class
                         (finalize-oops-class current-class slot-offset)))))
          (format class-constants "~3&;;; Finis.~%"))
        (format class-methods "
;;; 
;;; Set up method dispatch jump table pointers

GenericFunctionTables = (BasicObjectClassMethods, BasicObjectClassMethods, ~{~aClassMethods~^, ~})

ClassMethodsL: .byte <(GenericFunctionTables)
ClassMethodsH: .byte >(GenericFunctionTables)


;;; Finis.~%"
                (reverse all-classes-sequentially))))))
