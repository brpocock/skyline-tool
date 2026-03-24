(in-package :skyline-tool)

;;; Dynamic variables for class-definition parsing contexts
(defvar *class-bases* nil)
(defvar *class-size* nil)
(defvar *class-slots* nil)
(defvar *class-slots-order* nil)
(defvar *slot-sizes* nil)
(defvar *slot-annotations* nil)
(defvar *class-content-order* nil)
(defvar *class-graph* nil)
(defvar *class-constants* nil)
(defvar *class-methods* nil)
(defvar *classes.forth* nil)
(defvar *methods-set* nil)

;;; Annotation parsing helpers for Classes.Defs

(defun classes-defs-comment-line-p (line)
  "Return true if LINE is a comment (starts with ; or * after trim)."
  (and line
       (let ((trimmed (string-trim #(#\Space #\Tab) line)))
         (and (plusp (length trimmed))
              (or (char= #\; (char trimmed 0))
                  (char= #\* (char trimmed 0)))))))

(defun pascal-to-eightbol-name (name)
  "Convert a PascalCase or hyphenated assembly symbol to a EIGHTBOL NAME in
UPPERCASE-HYPHENATED form, as required by EIGHTBOL naming conventions.
A hyphen is inserted before each uppercase letter that follows a lowercase
letter (or digit); the result is uppercased.
  \"NameLength\"       → \"NAME-LENGTH\"
  \"MaxHP\"            → \"MAX-HP\"
  \"ClassID\"          → \"CLASS-ID\"
  \"AbsoluteDeltaX\"   → \"ABSOLUTE-DELTA-X\"
  \"HP\"               → \"HP\"
  \"WorkFitness\"      → \"WORK-FITNESS\"
  \"work-fitness\"     → \"WORK-FITNESS\"   (already hyphenated)
  \"WORK-FITNESS\"     → \"WORK-FITNESS\"   (already correct)"
  (string-upcase
   (with-output-to-string (out)
     (loop for i from 0 below (length name)
           for ch = (char name i)
           do (when (and (> i 0)
                         (upper-case-p ch)
                         (lower-case-p (char name (1- i))))
                (write-char #\- out))
              (write-char ch out)))))

(defun pascal-to-copybook-filename (name)
  "Convert PascalCase to Title-And-Hyphens for copybook filenames.
   NonPlayerCharacter → Non-Player-Character
   Character → Character
   Uses hyphens (not spaces) so Makefile dependencies parse correctly."
  (format nil "~{~a~^-~}"
          (mapcar #'string-capitalize
                  (split-sequence #\- (param-case name)))))

(defun eightbol-slot-name (pascal-name)
  "Convert PascalCase slot name to EIGHTBOL copybook form. Avoids reserved words
that would conflict with eightbol grammar (e.g. CLASS-ID -> OBJ-CLASS-ID,
POINTER -> ZP-POINTER, SIZE -> OBJ-SIZE, METHOD -> OBJ-METHOD,
REMAINDER -> CART-REMAINDER, TRUE -> CONST-TRUE)."
  (let ((base (pascal-to-eightbol-name pascal-name)))
    (case (intern base :keyword)
      ((:class-id) "OBJ-CLASS-ID")
      ((:pointer) "ZP-POINTER")
      ((:size) "OBJ-SIZE")
      ((:method) "OBJ-METHOD")
      ((:remainder) "CART-REMAINDER")
      ((:true) "CONST-TRUE")
      (t base))))

(defun parse-slot-annotation (parts)
  "Parse the annotation portion of a slot definition (everything after size).
PARTS is a list of strings (the whitespace-split remainder after name and size).
Returns one of:
  NIL                             — no annotation, use default PIC
  (:object-ref class-name)        — @ClassName pointer
  (:varchar n depend-field-name)  — = VARCHAR(n) DEPENDING ON Field
  (:pic string)                   — = PIC-string (verbatim PIC clause)

Pascal-type strings use two adjacent slots with a VARCHAR DEPENDING ON:
  .NameLength 1
  .Name 12 = VARCHAR(12) DEPENDING ON Name-Length
The DEPENDING ON field is referenced by its EIGHTBOL hyphenated name."
  (when parts
    (let ((first (first parts)))
      (cond
        ;; @ClassName — object reference
        ((and (> (length first) 1) (char= #\@ (char first 0)))
         (list :object-ref (subseq first 1)))
        ;; = … — explicit PIC clause or VARCHAR
        ((string= first "=")
         (let ((rest (rest parts)))
           (when rest
             (let ((spec (format nil "~{~a~^ ~}" rest)))
               ;; VARCHAR(n) DEPENDING ON Field — Pascal-type variable-length string
               (cl-ppcre:register-groups-bind (n field)
                   ("(?i)VARCHAR\\((\\d+)\\)\\s+DEPENDING\\s+ON\\s+(\\S+)" spec)
                 (return-from parse-slot-annotation
                   (list :varchar (parse-integer n) field)))
               (list :pic spec)))))
        (t nil)))))

(defun slot-annotation-to-eightbol-pic (annotation size)
  "Convert a slot annotation (from PARSE-SLOT-ANNOTATION) and SIZE (bytes)
to a EIGHTBOL PIC clause string suitable for use in a .cpy file.
Returns a string like \"PIC 9999 USAGE BINARY\" or \"OBJECT REFERENCE Actor\"."
  (cond
    ((null annotation)
     (cond
       ((= size 1) "PIC 99 USAGE BINARY")
       ((= size 2) "PIC 9999 USAGE BINARY")
       (t          (format nil "PIC 99 USAGE BINARY OCCURS ~d TIMES" size))))
    ((eq (car annotation) :object-ref)
     (format nil "USAGE OBJECT REFERENCE ~a" (second annotation)))
    ((eq (car annotation) :varchar)
     ;; Correct COBOL: PIC X OCCURS 0 TO n TIMES DEPENDING ON size-field.
     ;; Size-field does not get a picture.
     (format nil "PIC X OCCURS 0 TO ~d TIMES DEPENDING ON ~a"
             (second annotation) (pascal-to-eightbol-name (third annotation))))
    ((eq (car annotation) :pic)
     (second annotation))
    (t
     (format nil "PIC 99 USAGE BINARY ;; unknown annotation ~s" annotation))))

;;; Copybook generation  (Source/Generated/{ClassName}.cpy)

(defun compute-class-size-during-parse (class-name)
  "Return the size of CLASS-NAME (parent's size + own slots). Uses *class-bases*, *class-size*, *slot-sizes*, *class-slots-order*.

When any of these specials is @code{NIL} or not a hash-table (e.g. tests that bind only a subset), treat missing tables as empty so @code{gethash} is never called with @code{NIL} as the table argument."
  (unless (or (string= class-name "BasicObject")
              (and *class-bases* (hash-table-p *class-bases*)
                   (gethash class-name *class-bases*)))
    (error "Parent class ~s not found" class-name))
  (let ((base-sz (and *class-size* (hash-table-p *class-size*)
                      (gethash class-name *class-size*)))
        (parent (and *class-bases* (hash-table-p *class-bases*)
                     (gethash class-name *class-bases*))))
    (cond (base-sz base-sz)
          ((null parent) 0)
          (t (let ((parent-sz (compute-class-size-during-parse parent))
                   (own-slots (if (and *class-slots-order* (hash-table-p *class-slots-order*))
                                  (gethash class-name *class-slots-order* '())
                                  '()))
                   (sizes-h (or (and *slot-sizes* (hash-table-p *slot-sizes*)
                                     (gethash class-name *slot-sizes*))
                                (make-hash-table :test 'equal))))
               (+ parent-sz
                  (reduce #'+ (mapcar (lambda (s) (gethash s sizes-h 0)) own-slots)
                          :initial-value 0)))))))

(defun class-ancestry-chain (class-name class-bases-hash)
  "Return the ancestor chain for CLASS-NAME as a list from root to CLASS-NAME.
Single inheritance only. E.g. for Character: (\"BasicObject\" \"Entity\" \"Actor\" \"Character\")"
  (let (chain)
    (loop for c = class-name then (gethash c class-bases-hash)
          while c
          do (push c chain))
    chain))

(defun list-classes-defs-method-introductions
    (&optional (class-defs-pathname #p"./Source/Classes/Classes.Defs"))
  "Return alist of (INTRODUCING-CLASS . METHOD-NAME) for each # line in CLASS-DEFS-PATHNAME.

Each METHOD-NAME is the OOPS hash key (text after #), e.g. @code{GetBounds} for a
@code{#GetBounds} line. Parsing matches @code{make-classes-for-oops}: a class
line @code{Child < Parent} sets the current class; each @code{#} line at column
zero binds the method to that class. Comment and blank lines are ignored.

@table @asis
@item CLASS-DEFS-PATHNAME
Path to @file{Classes.Defs} (default @file{./Source/Classes/Classes.Defs}).
@end table

@table @asis
@item Return value
A fresh list of @code{(cons introducing-class method-name-string)} in file order.
Does not include @code{Destroy} on BasicObject (runtime assembly); only explicit
@code{#} lines appear.
@end table"
  (let ((result '())
        ;; Match make-classes-for-oops: method lines before first class decl bind to BasicObject.
        (cur-class "BasicObject"))
    (with-input-from-file (class-file class-defs-pathname)
      (loop for line = (read-line class-file nil nil)
            while line
            for trimmed = (string-trim #(#\Space #\Tab) line)
            unless (or (emptyp trimmed) (classes-defs-comment-line-p line))
              do (cond
                   ((find #\< line)
                    (destructuring-bind (new-class old-class)
                        (mapcar (curry #'string-trim #(#\Space #\Tab))
                                (split-sequence #\< line))
                      (declare (ignore old-class))
                      (setf cur-class new-class)))
                   ((and (plusp (length line)) (char= #\# (char line 0)))
                    (let ((name (string-trim #(#\Space) (subseq line 1))))
                      (when (plusp (length name))
                        (push (cons cur-class name) result))))
                   (t nil))))
    (nreverse result)))

(defun make-eightbol-copybooks
    (&optional (class-defs-pathname #p"./Source/Classes/Classes.Defs"))
  "Generate EIGHTBOL copybook (.cpy) files from Classes.Defs.

For each class defined in CLASS-DEFS-PATHNAME, writes
Source/Generated/{machine}/Classes/{ClassName}.cpy containing all slots (inherited and own)
in ancestry order (root → leaf), with section comments identifying the
origin class of each group of slots.

Slot annotation conventions in Classes.Defs:
  .SlotName size              — default PIC (1→PIC 99, 2→PIC 9999, n→OCCURS n PIC 99)
  .SlotName size @ClassName   — OBJECT REFERENCE ClassName (2-byte pointer)
  .SlotName size = PIC-string — verbatim PIC clause (prefer explicit PIC/USAGE for enums, buffers, …)
  .SlotName size = VARCHAR(n) DEPENDING ON Field — variable-length string"
  (let ((*class-slots*       (make-hash-table :test 'equal))
        (*class-slots-order* (make-hash-table :test 'equal))
        (*class-content-order* (make-hash-table :test 'equal))
        (*class-bases*       (make-hash-table :test 'equal))
        (*class-size*        (make-hash-table :test 'equal))
        (*slot-annotations*  (make-hash-table :test 'equal))
        (*slot-sizes*        (make-hash-table :test 'equal))
        (all-classes         '()))
    ;; Seed BasicObject
    (setf (gethash "BasicObject" *class-bases*) nil
          (gethash "BasicObject" *class-size*)  1
          (gethash "BasicObject" *class-slots*) (make-hash-table :test 'equal)
          (gethash "BasicObject" *class-slots-order*) '("ClassID")
          (gethash "BasicObject" *class-content-order*) '((:slot "ClassID"))
          (gethash "BasicObject" *slot-sizes*)  (let ((h (make-hash-table :test 'equal)))
                                                  (setf (gethash "ClassID" h) 1) h)
          (gethash "BasicObject" *slot-annotations*) (make-hash-table :test 'equal))
    (with-input-from-file (class-file class-defs-pathname)
        (loop with current-class = "BasicObject"
              with slot-offset   = 1
              for line = (read-line class-file nil nil)
              for trimmed = (and line (string-trim #(#\Space #\Tab) line))
              while line do
              (cond
                ((emptyp trimmed) nil)
                ((char= #\; (char trimmed 0))
                 (let ((comment-text (string-trim " " (subseq trimmed 1))))
                   (when (plusp (length comment-text))
                     (push (list :comment comment-text)
                           (gethash current-class *class-content-order* '())))))
                ((char= #\# (char line 0)) nil)  ; method — ignored for copybooks
                ((char= #\. (char line 0))        ; slot definition
               (let* ((parts (split-sequence #\Space (subseq line 1)
                                             :remove-empty-subseqs t))
                      (name  (first parts))
                      (parsed-size (if (and (second parts) (char= #\$ (char (second parts) 0)))
                                      (parse-integer (subseq (second parts) 1) :radix 16)
                                      (parse-integer (or (second parts) "1"))))
                      (annotation (parse-slot-annotation (cddr parts)))
                      (size (if (and annotation (eq (car annotation) :object-ref))
                                (pointer-size-for-machine)
                                parsed-size)))
                 (unless (gethash current-class *class-slots*)
                   (setf (gethash current-class *class-slots*)
                         (make-hash-table :test 'equal)))
                 (unless (gethash current-class *slot-sizes*)
                   (setf (gethash current-class *slot-sizes*)
                         (make-hash-table :test 'equal)))
                 (unless (gethash current-class *slot-annotations*)
                   (setf (gethash current-class *slot-annotations*)
                         (make-hash-table :test 'equal)))
                 (setf (gethash name (gethash current-class *class-slots*))
                       (cons slot-offset size))
                 (setf (gethash name (gethash current-class *slot-sizes*)) size)
                 (when annotation
                   (setf (gethash name (gethash current-class *slot-annotations*))
                         annotation))
                 (pushnew name (gethash current-class *class-slots-order*) :test #'string=)
                 (push (list :slot name) (gethash current-class *class-content-order*))
                 (incf slot-offset size)))
              ((find #\< line)              ; class definition
               (destructuring-bind (new-class old-class)
                   (mapcar (curry #'string-trim #(#\Space))
                           (split-sequence #\< line))
                 (setf (gethash new-class *class-bases*) old-class
                       (gethash new-class *class-slots*) (make-hash-table :test 'equal)
                       (gethash new-class *class-slots-order*) '()
                       (gethash new-class *class-content-order*) '()
                       (gethash new-class *slot-sizes*) (make-hash-table :test 'equal)
                       (gethash new-class *slot-annotations*) (make-hash-table :test 'equal)
                       slot-offset (compute-class-size-during-parse old-class)
                       current-class new-class)
                 (push new-class all-classes))))))
    (setf all-classes (nconc all-classes (list "BasicObject")))
    ;; Finalise sizes
    (dolist (class-name (reverse all-classes))
      (let ((own-slots (gethash class-name *class-slots-order*)))
        (let* ((parent  (gethash class-name *class-bases*))
               (base-sz (or (gethash parent *class-size*) 0))
               (sizes-h (or (and *slot-sizes* (hash-table-p *slot-sizes*)
                                 (gethash class-name *slot-sizes*))
                            (make-hash-table :test 'equal)))
               (own-sz  (reduce #'+ (mapcar (lambda (s) (gethash s sizes-h 0))
                                            (or own-slots '()))
                                :initial-value 0)))
          (setf (gethash class-name *class-size*) (+ base-sz own-sz)))))
    ;; Collect size-fields: slots that are DEPENDING ON targets for :varchar (they get no picture)
    (let ((size-fields (make-hash-table :test 'equal)))
      (dolist (class-name (reverse all-classes))
        (dolist (ancestor (class-ancestry-chain class-name *class-bases*))
          (let ((annot-hash (gethash ancestor *slot-annotations*)))
            (when annot-hash
              (maphash (lambda (slot-name annotation)
                         (declare (ignore slot-name))
                         (when (and (eq (car annotation) :varchar)
                                    (third annotation))
                           (setf (gethash (third annotation) size-fields) t)))
                       annot-hash)))))
    ;; Write one .cpy per class  — canonical path: Source/Generated/{machine}/Classes/{Name}-Slots.cpy
    (let ((generated-dir (merge-pathnames
                          (make-pathname :directory
                                         `(:relative "Source" "Generated"
                                                     ,(machine-directory-name) "Classes"))
                          #p"./")))
      (ensure-directories-exist generated-dir)
      (dolist (class-name (reverse all-classes))
        (let ((cpy-path (merge-pathnames
                         (make-pathname :name (concatenate 'string (pascal-to-copybook-filename class-name) "-Slots") :type "cpy")
                         generated-dir)))
          (with-output-to-file (out cpy-path :if-exists :supersede)
            (with-eightbol-sequence
              (emit-eightbol-comment out (format nil " ~a-Slots -- generated by make-classes-for-oops"
                                             class-name))
              (emit-eightbol-comment out " DO NOT EDIT -- regenerated from Classes.Defs")
              (let ((chain (class-ancestry-chain class-name *class-bases*)))
                (dolist (ancestor chain)
                  (let ((content     (reverse (gethash ancestor *class-content-order* '())))
                        (sizes-hash  (or (and *slot-sizes* (hash-table-p *slot-sizes*)
                                              (gethash ancestor *slot-sizes*))
                                         (make-hash-table :test 'equal)))
                        (annot-hash  (gethash ancestor *slot-annotations*)))
                    (when content
                      (emit-eightbol-blank out)
                      (if (string= ancestor class-name)
                          (emit-eightbol-comment out (format nil " Own slots (~a):" ancestor))
                          (emit-eightbol-comment out (format nil " Inherited from ~a:" ancestor)))
                      (dolist (item content)
                        (ecase (first item)
                          (:comment
                           (emit-eightbol-comment out (second item)))
                          (:slot
                           (let* ((slot-name (second item))
                                  (size       (gethash slot-name sizes-hash 1))
                                  (annotation (when annot-hash
                                                (gethash slot-name annot-hash)))
                                  (pic        (if (gethash slot-name size-fields)
                                                  ""  ; size-field does not get a picture
                                                  (slot-annotation-to-eightbol-pic annotation size)))
                                  (cpy-name   (eightbol-slot-name slot-name)))
                             (emit-eightbol-var 5 cpy-name pic out)))))))))))
          (format *trace-output* "~&Generated ~a~%" (enough-namestring cpy-path)))))
  (values))))

(defun finalize-oops-class (class-name last-slot-offset)
  "Emit OOPS class definitions for CLASS-NAME. Uses *class-graph*, *class-constants*, *class-methods*, *classes.forth*, *class-slots*, *class-bases*, *class-size*, *methods-set*."
  (let ((class-final-size last-slot-offset)
        (slots (gethash class-name *class-slots*))
        (methods (or (gethash class-name *methods-set*)
                     (make-hash-table :test 'equal))))
    (format *class-graph* "
~a [label=\"{~a|~{. ~a (~d byte~:p)~^|~}|~{# ~a~^|~}}\"]"
            class-name
            (title-case class-name)
            (when slots
              (loop for field
                    in (sort (hash-table-keys slots) #'string-lessp)
                    append (list (title-case field)
                                 (cdr (gethash field slots)))
                    do (format *classes.forth* "
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
    (setf (gethash class-name *class-size*) class-final-size)
    (format *class-constants* "~%~10t~aSize = $~2,'0x"
            class-name class-final-size)
    (format *class-methods* "~2%~aClassMethods:~%"
            class-name)
    (dolist (method (sort (hash-table-keys methods) #'string-lessp))
      (let* ((original-class (gethash method methods))
             (class-ancestry
               (append
                (loop with oops-class = class-name
                      until (string= oops-class original-class)
                      collecting oops-class
                      do (setf oops-class (gethash oops-class *class-bases*)))
                (list original-class))))
        (format *classes.forth* "~% : ~a-~a ~aClass CallMethod~:*~a~a call-method ; "
                (param-case class-name)
                (param-case method)
                class-name
                method)
        (format *class-methods* "
~10t.weak~
~{~%~12tMethod~a~a := MissingMethod~}
~10t.endweak"
                (mapcan (lambda (oops-class) (list oops-class method))
                        class-ancestry))
        (format *class-methods* "
~10t* = ~aClassMethods + Call~a~a
Invoke~a~a:"
                class-name original-class method
                class-name method)
        (dolist (oops-class class-ancestry)
          (format *class-methods* "
~10t.if Method~a~a~0@* != MissingMethod
~12tjmp Method~a~a
~10t.else"
                  oops-class method))
        (format *class-methods* "
~12tjmp MissingMethod
~12t.warn ~
\"There is no implementation of the generic function Method~a~a ~
and no ancestor provides an implementation (searched ~{~a~^, ~})\""
                class-name method class-ancestry)
        (dotimes (_ (length class-ancestry))
          (format *class-methods* "~%~10t.fi"))))
    (format *class-methods* "
~10t* = ~aClassMethods + 3 * ~d"
            class-name (length (hash-table-keys methods)))
    (format *class-methods* "
Method~aDestroy: .proc
~10t.mvaw Size, ~:*~aSize
~10t.mva Ref, #-1
~10tjmp Lib.DestroyObject~32t; tail call
~10t.pend~%"
            class-name)))

(defun make-classes-for-oops (&optional
                                (class-defs-pathname (merge-pathnames #p"Source/Classes/Classes.Defs"
                                                                      (project-root))))
  "Generate OOPS class definitions from class specification file.

Processes the Classes.Defs file to generate various output files containing
class constants, Forth definitions, inheritance graphs, and assembly code
for the OOPS (Object-Oriented Programming System). Single inheritance only.

Also generates EIGHTBOL copybooks in Source/Generated/{ClassName}.cpy via
MAKE-EIGHTBOL-COPYBOOKS.

@table @asis
@item CLASS-DEFS-PATHNAME
Path to Classes.Defs file (default: ./Source/Classes/Classes.Defs)
@item Outputs
Generates Classes.forth, Classes.dot, ClassConstants.s, ClassInheritance.s, ClassMethods.s, ClassSizes.s, and {ClassName}.cpy for each class.
@end table"
  (let ((all-classes-sequentially (list))
        (output-dir (format nil "./Source/Generated/~a/" (machine-directory-name))))
    (with-input-from-file (class-file class-defs-pathname)
      (ensure-directories-exist output-dir)
      (with-output-to-file (classes.forth (concatenate 'string output-dir "Classes.forth")
                                          :if-exists :supersede)
        (format classes.forth "( -*- forth -*- )
 ( class accessors and such for Forth code, generated from Classes.Defs )~2%")
        (with-output-to-file (class-graph (concatenate 'string output-dir "Classes.dot")
                                          :if-exists :supersede)
          (format class-graph "digraph Classes {
node [shape=Mrecord];
")
          (with-output-to-file (class-methods (concatenate 'string output-dir "ClassMethods.s")
                                              :if-exists :supersede)
            (format class-methods ";;; ClassMethods derived from ~s~2%"
                    (enough-namestring class-defs-pathname))
            (with-output-to-file (class-constants (concatenate 'string output-dir "ClassConstants.s")
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
              (let ((*class-graph* class-graph)
                    (*class-constants* class-constants)
                    (*class-methods* class-methods)
                    (*classes.forth* classes.forth)
                    (*methods-set* (make-hash-table :test 'equal))
                    (*class-slots* (make-hash-table :test 'equal))
                    (*class-bases* (make-hash-table :test 'equal))
                    (*class-size* (make-hash-table :test #'equal)))
                (setf (gethash "BasicObject" *class-bases*) nil
                      (gethash "BasicObject" *class-size*) 1
                      (gethash "BasicObject" *class-slots*)
                      (let ((h (make-hash-table :test 'equal)))
                        (setf (gethash "ClassID" h) (cons 0 1))
                        h))
                (let ((basic-object-methods (make-hash-table :test 'equal)))
                  (setf (gethash "Destroy" basic-object-methods) "BasicObject"
                        (gethash "BasicObject" *methods-set*) basic-object-methods))
                (do* ((line (read-line class-file nil nil) (read-line class-file nil nil))
                      (parent-class "BasicObject" parent-class)
                      (cur-class "BasicObject" cur-class)
                      (class-index 1 class-index)
                      (cur-slot-offset 1 cur-slot-offset))
                     ((null line)
                      (when cur-class
                        (finalize-oops-class cur-class cur-slot-offset))
                      ;; Write ClassInheritance.s and ClassSizes.s while *class-bases* and *class-size* are bound
                      (with-output-to-file (inheritance (concatenate 'string output-dir "ClassInheritance.s")
                                                        :if-exists :supersede)
                        (format inheritance ";;; Class inheritances derived from ~s~2%"
                                (enough-namestring class-defs-pathname))
                        (format inheritance
                                "~2%;;; ~|~2%ParentClass:
~10t.byte 0, 0~{~%~10t.byte ~aClass~40t; parent of ~aClass~}~3&;;; Finis.~%"
                                (mapcan (lambda (oops-class)
                                          (list (gethash oops-class *class-bases*) oops-class))
                                        (reverse (copy-list all-classes-sequentially)))))
                      (with-output-to-file (sizes (concatenate 'string output-dir "ClassSizes.s")
                                                  :if-exists :supersede)
                        (format sizes ";;; Class sizes derived from ~s~2%ClassSize: .block"
                                (enough-namestring class-defs-pathname))
                        (format sizes
                                "~{~&~20t.byte ~d~40t; ~aClass~}~2%~10t.bend~%;;; Finis.~%"
                                (mapcan (lambda (oops-class)
                                          (list (gethash oops-class *class-size*) oops-class))
                                        (reverse (copy-list all-classes-sequentially)))))
                      (fresh-line *class-constants*)
                      (terpri *class-constants*)
                      (format *class-methods* "
;;; 
;;; Set up method dispatch jump table pointers

GenericFunctionTables = (BasicObjectClassMethods, BasicObjectClassMethods, ~{~aClassMethods~^, ~})

ClassMethodsL: .byte <(GenericFunctionTables)
ClassMethodsH: .byte >(GenericFunctionTables)

;;; Finis.~%"
                              (reverse all-classes-sequentially))
                      (format *class-graph* "~&}~%"))
                  (let ((trimmed (string-trim #(#\Space #\Tab) line)))
                             (when (classes-defs-comment-line-p line)
                               (fresh-line class-constants)
                               (princ line class-constants)
                               (fresh-line class-methods)
                               (princ line class-methods)
                               (let ((content-start (position-if (lambda (ch)
                                                                    (char/= #\; ch)
                                                                       )
                                                                 trimmed)))
                                 (format classes.forth "~& ( ~a ) "
                                         (if content-start (subseq trimmed content-start) ""))))
                             (when (emptyp trimmed)
                               (fresh-line class-constants)
                               (fresh-line class-methods)
                               (fresh-line classes.forth))
                             (when (and (not (classes-defs-comment-line-p line))
                                        (not (emptyp trimmed)))
                               (cond
                             ((find #\< line) ; class definition (X < Y) — check first
                              (destructuring-bind (new-class old-class)
                                  (mapcar (curry #'string-trim #(#\Space #\Tab))
                                          (split-sequence #\< line))
                                (push new-class all-classes-sequentially)
                                (let ((prior-class cur-class))
                                  (when prior-class
                                    (finalize-oops-class prior-class cur-slot-offset))
                                  (setf cur-class new-class
                                        parent-class old-class
                                        cur-slot-offset (gethash old-class *class-size*)
                                        (gethash new-class *class-bases*) old-class
                                        (gethash new-class *methods-set*)
                                        (copy-hash-table (or (gethash old-class *methods-set*)
                                                            (make-hash-table :test 'equal))))
                                  (finish-output)
                                  (unless cur-slot-offset
                                    (error "Could not find parent class ~s in ~s"
                                           old-class *class-size*)))
                                (format *class-graph* "~% \"~a\" -> \"~a\";" parent-class cur-class)
                                (format class-constants "~%
~10t;; class ~a (parent: ~a)
~10t~aClass = $~2,'0x~%"
                                        cur-class parent-class
                                        cur-class (incf class-index))))
                             ((char= #\# (char line 0)) ; method name
                              (if cur-class
                                  (let* ((name (string-trim #(#\Space) (subseq line 1)))
                                         (methods (or (gethash cur-class *methods-set*)
                                                      (setf (gethash cur-class *methods-set*)
                                                            (make-hash-table :test 'equal)))))
                                    (setf (gethash name methods) cur-class)
                                    (format class-constants "~%~10tCall~a~a = $~2,'0x"
                                            cur-class name (* 3 (1- (hash-table-count methods)))))
                                  (cerror "Continue, ignoring"
                                          "Ignoring method without class: ~s" line)))
                             ((char= #\. (char line 0)) ; slot name & size
                              (if cur-class
                                  (let* ((class-slots (or (gethash cur-class *class-slots*)
                                                         (setf (gethash cur-class *class-slots*)
                                                               (make-hash-table :test 'equalp))))
                                         (parts (split-sequence #\Space (subseq line 1)
                                                                :remove-empty-subseqs t))
                                         (name  (first parts))
                                         (size$ (second parts))
                                         (parsed-size (if (and size$ (char= #\$ (char size$ 0)))
                                                          (parse-integer (subseq size$ 1) :radix 16)
                                                          (parse-integer (or size$ "1"))))
                                         (annotation (parse-slot-annotation (cddr parts)))
                                         (size (if (and annotation (eq (car annotation) :object-ref))
                                                    (pointer-size-for-machine)
                                                    parsed-size)))
                                    (format class-constants "
~10t~a~a = $~2,'0x~@[~32t; … $~2,'0x~]"
                                            cur-class name cur-slot-offset
                                            (when (/= 1 size)
                                              (1- (+ cur-slot-offset size))))
                                    (setf (gethash name class-slots)
                                          (cons cur-slot-offset size))
                                    (incf cur-slot-offset size))
                                  (cerror "Continue, ignoring"
                                          "Ignoring slot without class: ~s" line)))
                               ;; anything else
                               (t (cerror "Continue, ignoring line"
                                          "Unrecognized line in class definitions: ~s" line)))))))))
              )
    ;; Generate EIGHTBOL copybooks for all classes
    (make-eightbol-copybooks class-defs-pathname)))))

