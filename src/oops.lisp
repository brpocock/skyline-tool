(in-package :skyline-tool)

(defun make-classes-for-oops (&optional
                                (class-defs-pathname #p"./Source/Classes/Classes.Defs"))
  "Writes ClassConstants from CLASS-DEFS-PATHNAME"
  (let ((all-classes-sequentially (list)))
    (with-input-from-file (class-file class-defs-pathname)
      (ensure-directories-exist #p"./Source/Generated/")
      (with-output-to-file (classes.forth #p"./Source/Generated/Classes.forth"
                                          :if-exists :supersede)
        (format classes.forth "( -*- forth -*- )
 ( class accessors and such for Forth code, generated from Classes.Defs )~2%")
        (with-output-to-file (class-graph #p"./Source/Generated/Classes.dot"
                                          :if-exists :supersede)
          (format class-graph "digraph Classes {
node [shape=Mrecord];
")
          (with-output-to-file (class-methods #p"./Source/Generated/ClassMethods.s"
                                              :if-exists :supersede)
            (format class-methods ";;; ClassMethods derived from ~s~2%"
                    (enough-namestring class-defs-pathname))
            (with-output-to-file (class-constants #p"./Source/Generated/ClassConstants.s"
                                                  :if-exists :supersede)
              (format class-constants ";;; ClassConstants derived from ~s~2%"
                      (enough-namestring class-defs-pathname))
              (format classes.forth "
( BasicObject class )
: basic-object-destroy BasicObjectClass CallBasicObjectDestroy call-method ;
")
              (format class-constants "
;;; BasicObject class (builtin to OOPS concept)
~10tBasicObjectClass = 1
~10tBasicObjectClassID = 0
~10tClassIDOffset = 0

~10tCallBasicObjectDestroy = 0
")
              (format class-graph "~&BasicObject [label=\"{ Basic Object | . Class ID (1 byte) | # Destroy}\"];")
              (let ((methods-set (make-hash-table :test 'equal))
                    (class-slots (make-hash-table :test 'equal))
                    (class-bases (make-hash-table :test 'equal))
                    (class-size (make-hash-table :test #'equal)))
                (setf (gethash "BasicObject" class-bases) nil
                      (gethash "BasicObject" class-size) 1)
                (let ((basic-object-methods (make-hash-table :test 'equal)))
                  (setf (gethash "Destroy" basic-object-methods) "BasicObject"
                        (gethash "BasicObject" methods-set) basic-object-methods))
                (labels
                    ((finalize-oops-class (class-name last-slot-offset)
                       (let ((class-final-size last-slot-offset)
                             (slots (gethash class-name class-slots))
                             (methods (gethash class-name methods-set)))
                         (format class-graph "
~a [label=\"{~a|~{. ~a (~d byte~:p)~^|~}|~{# ~a~^|~}}\"]"
                                 class-name
                                 (title-case class-name)
                                 (when slots
                                   (loop for field
                                           in (sort (hash-table-keys slots) #'string-lessp)
                                         append (list (title-case field)
                                                      (cdr (gethash field slots)))
                                         do (format classes.forth "
 : ~a-~a! ~a~a prop! ;
 : ~a-~a@ ~a~a prop@ ; "
                                                    (param-case class-name)
                                                    (param-case field)
                                                    class-name field
                                                    (param-case class-name)
                                                    (param-case
                                                     (if (char= #\P (last-elt field))
                                                         (format nil "~a?"
                                                                 (subseq field 0 (1- (length field))))
                                                         (format nil "~a~c" field #\@)))
                                                    class-name field)))
                                 (mapcar
                                  (lambda (method)
                                    (concatenate 'string (title-case method)
                                                 (if (char= #\P (last-elt method))
                                                     "?"
                                                     "")))
                                  (sort (remove-if-not (lambda (key)
                                                         (string-equal (gethash key methods)
                                                                       class-name))
                                                       (hash-table-keys methods))
                                        #'string-lessp)))
                         (setf (gethash class-name class-size) class-final-size)
                         (format class-constants "~%~10t~aSize = $~2,'0x"
                                 class-name class-final-size)
                         (format class-methods "~2%~aClassMethods:~%"
                                 class-name)
                         (dolist (method (sort (hash-table-keys methods) #'string-lessp))
                           (let* ((original-class (gethash method methods))
                                  (class-ancestry
                                    (append
                                     (loop with class = class-name
                                           until (string= class original-class)
                                           collecting class
                                           do (setf class (gethash class class-bases)))
                                     (list original-class))))
                             (format classes.forth "~% : ~a-~a ~aClass CallMethod~:*~a~a call-method ; "
                                     (param-case class-name)
                                     (param-case method)
                                     class-name
                                     method)
                             (format class-methods "
~10t.weak~
~{~%~12tMethod~a~a := MissingMethod~}
~10t.endweak"
                                     (mapcan (lambda (class) (list class method))
                                             class-ancestry))
                             (format class-methods "
~10t* = ~aClassMethods + Call~a~a
Invoke~a~a:"
                                     class-name original-class method
                                     class-name method)
                             (dolist (class class-ancestry)
                               (format class-methods "
~10t.if Method~a~a~0@* != MissingMethod
~12tjmp Method~a~a
~10t.else"
                                       class method))
                             (format class-methods "
~12tjmp MissingMethod
~12t.warn ~
\"There is no implementation of the generic function Method~a~a ~
and no ancestor provides an implementation (searched ~{~a~^, ~})\""
                                     class-name method class-ancestry)
                             (dotimes (_ (length class-ancestry))
                               (format class-methods "~%~10t.fi"))))
                         (format class-methods "
~10t* = ~aClassMethods + 3 * ~d"
                                 class-name (length (hash-table-keys methods)))
                         (format class-methods "
Method~aDestroy: .proc
~10t.mvaw Size, ~:*~aSize
~10t.mva Ref, #-1
~10tjmp Lib.DestroyObject~32t; tail call
~10t.pend~%"
                                 class-name))))
                  (loop with parent-class = "BasicObject"
                        with current-class = "BasicObject"
                        with class-index = 1 with slot-offset = 1
                        for line = (read-line class-file nil nil) while line
                        do (cond
                             ((emptyp line) ; blank line
                              (fresh-line class-constants)
                              (fresh-line class-methods)
                              (fresh-line classes.forth))
                             ((char= #\; (char line 0)) ; comment
                              (fresh-line class-constants)
                              (princ line class-constants)
                              (fresh-line class-methods)
                              (princ line class-methods)
                              (format classes.forth "~& ( ~a ) "
                                      (subseq line
                                              (position-if (lambda (ch)
                                                             (and (char/= #\; ch)
                                                                  (char/= #\Space ch)
                                                                  (char/= #\Tab ch)))
                                                           line))))
                             ((char= #\# (char line 0)) ; method name
                              (if current-class
                                  (let ((name (string-trim #(#\Space) (subseq line 1)))
                                        (methods (gethash current-class methods-set)))
                                    (setf (gethash name methods) current-class)
                                    (format class-constants "~%~10tCall~a~a = $~2,'0x"
                                            current-class name (* 3 (1- (hash-table-count methods)))))
                                  (cerror "Continue, ignoring"
                                          "Ignoring method without class: ~s" line)))
                             ((char= #\. (char line 0)) ; slot name & size
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
                                      (unless (gethash current-class class-slots)
                                        (setf (gethash current-class class-slots)
                                              (make-hash-table :test 'equalp)))
                                      (setf (gethash name (gethash current-class class-slots))
                                            (cons slot-offset size))
                                      (incf slot-offset size)))
                                  (cerror "Continue, ignoring"
                                          "Ignoring slot without class: ~s" line)))
                             ((find #\< line) ; class definition
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
                                (format class-graph "~% \"~a\" -> \"~a\";" parent-class current-class)
                                (format class-constants "~%
~10t;; class ~a (parent: ~a)
~10t~aClass = $~2,'0x~%"
                                        current-class parent-class
                                        current-class (incf class-index))))
                             ;; anything else
                             (t (cerror "Continue, ignoring line"
                                        "Unrecognized line in class definitions: ~s" line)))
                        finally
                           (when current-class
                             (finalize-oops-class current-class slot-offset))))
                (with-output-to-file (inheritance #p"./Source/Generated/ClassInheritance.s"
                                                  :if-exists :supersede)
                  (format inheritance ";;; Class inheritances derived from ~s~2%"
                          (enough-namestring class-defs-pathname))
                  (format inheritance
                          "~2%;;; ~|~2%ParentClass:
~10t.byte 0, 0~{~%~10t.byte ~aClass~40t; parent of ~aClass~}~3&;;; Finis.~%"
                          (mapcan (lambda (class)
                                    (list (gethash class class-bases) class))
                                  (reverse (copy-list all-classes-sequentially)))))
                (with-output-to-file (sizes #p"./Source/Generated/ClassSizes.s"
                                            :if-exists :supersede)
                  (format sizes ";;; Class sizes derived from ~s~2%ClassSize: .block"
                          (enough-namestring class-defs-pathname))
                  (format sizes
                          "~{~&~20t.byte ~d~40t; ~aClass~}~2%~10t.bend~%;;; Finis.~%"
                          (mapcan (lambda (class)
                                    (list (gethash class class-size) class))
                                  (reverse (copy-list all-classes-sequentially))))))
              (fresh-line class-constants)
              (terpri class-constants))
            (format class-methods "
;;; 
;;; Set up method dispatch jump table pointers

GenericFunctionTables = (BasicObjectClassMethods, BasicObjectClassMethods, ~{~aClassMethods~^, ~})

ClassMethodsL: .byte <(GenericFunctionTables)
ClassMethodsH: .byte >(GenericFunctionTables)

;;; Finis.~%"
                    (reverse all-classes-sequentially)))
          (format class-graph "~&}~%"))))))
