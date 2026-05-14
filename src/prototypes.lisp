(in-package :skyline-tool)

(defconstant +spawn-kind-character+ 0
  "Map spawn entry references a @code{CharacterID} from NPC stats.")
(defconstant +spawn-kind-object+ 1
  "Map spawn entry references an object prototype compiled from @file{Source/Objects/}.")

(defvar *object-prototype-index* nil
  "Hash table: prototype basename string → stable compile-time index.")

(defun object-prototypes-directory (&optional (root (project-root)))
  "Return pathname for JSON object prototypes under ROOT."
  (merge-pathnames #p"Source/Objects/" root))

(defun list-object-prototype-json-files (&optional (root (project-root)))
  "Return sorted list of @file{Source/Objects/*.json} pathnames under ROOT."
  (let ((dir (object-prototypes-directory root)))
    (if (probe-file dir)
        (sort (directory (merge-pathnames #p"*.json" dir))
              #'string< :key #'pathname-name)
        nil)))

(defun read-object-prototype-json (pathname)
  "Read single JSON object prototype from PATHNAME.

PATHNAME must contain one JSON object.  The @code{Class} key names the
PascalCase class (@code{NonPlayerCharacter}, etc.).  Remaining keys are
slot field names for that class hierarchy.

@table @asis
@item PATHNAME
Designator for a @file{.json} file under @file{Source/Objects/}.
@end table

@subsection Outputs
Plist with keys @code{:name}, @code{:class}, and keyword keys derived from
JSON field names (uppercased)."
  (let* ((json (with-open-file (stream (merge-pathnames pathname)
                                       :direction :input
                                       :if-does-not-exist :error)
                 (json:decode-json stream)))
         (class-name (or (cdr (assoc "Class" json :test #'string=))
                         (error "Prototype ~a missing required \"Class\" key" pathname))))
    (append (list :name (pathname-name pathname)
                  :class class-name)
            (loop for (key . value) in json
                  unless (string= key "Class")
                    append (list (make-keyword (string-upcase key)) value)))))

(defun class-id-for-name (class-name)
  "Return numeric ClassID for PascalCase CLASS-NAME from generated constants."
  (let ((ids (read-class-ids-from-file)))
    (or (when-let (pair (find class-name (hash-table-plist ids)
                               :key #'cdr :test #'string=))
          (car pair))
        (error "Unknown class ~s — regenerate ClassConstants.s" class-name))))

(defun ensure-object-prototype-index (&optional (root (project-root)))
  "Build or return hash NAME → index for @file{Source/Objects/*.json}."
  (or *object-prototype-index*
      (setf *object-prototype-index*
            (let ((table (make-hash-table :test #'equal)))
              (loop for file in (list-object-prototype-json-files root)
                    for index from 0
                    do (setf (gethash (pathname-name file) table) index))
              table))))

(defun object-prototype-index (name)
  "Return compile-time index for object prototype basename NAME."
  (or (gethash name (ensure-object-prototype-index))
      (error "Unknown object prototype ~s (no Source/Objects/~a.json)" name name)))

(defun json-field-value (prototype field-name)
  "Lookup FIELD-NAME in PROTOTYPE plist (case-insensitive)."
  (let ((key (make-keyword (string-upcase field-name))))
    (or (getf prototype key)
        (loop for (k . v) on (cddr prototype) by #'cddr
              when (and (keywordp k)
                        (string-equal (symbol-name k) field-name))
                return v))))

(defun json-value-for-slot (prototype full-field-name)
  "Find JSON value for FULL-FIELD-NAME using exact or suffix key match."
  (or (json-field-value prototype full-field-name)
      (loop for (key . value) on (cddr prototype) by #'cddr
            when (and (keywordp key)
                      (let ((k (string key)))
                        (and (>= (length full-field-name) (length k))
                             (string-equal k
                                           (subseq full-field-name
                                                   (- (length full-field-name)
                                                      (length k)))))))
              return value)))

(defun speech-color-assembly-value (color-name)
  "Emit MARIA / STIC speech color assembly for COLOR-NAME string."
  (if (= *machine* 2609)
      (format nil "~10t.byte COL~a"
              (string-upcase (pascal-case (or color-name "Gray"))))
      (format nil "~10t.byte CoLu(COL~a, $f)"
              (string-upcase (pascal-case (or color-name "Gray"))))))

(defun palette-color-assembly-value (color-name)
  "Emit CommonPalette decoration color for COLOR-NAME."
  (format nil "~10t.byte PaletteColor_~a" (pascal-case (string color-name))))

(defun emit-prototype-slot-value (class-name full-field-name slot-bytes prototype)
  "Return one assembly line initializing FULL-FIELD-NAME on CLASS-NAME."
  (let ((value (json-value-for-slot prototype full-field-name))
        (slot-name (if (and (>= (length full-field-name) (length class-name))
                            (string= class-name full-field-name
                                     :end1 (length class-name)))
                       (subseq full-field-name (length class-name))
                       full-field-name)))
    (cond
      ((or (string= slot-name "ClassID") (string= full-field-name "BasicObjectClassID"))
       (format nil "~10t.byte ~aClass" class-name))
      ((string= slot-name "Decal")
       (format nil "~10t.byte $ff"))
      ((member slot-name '("SpeechColor") :test #'string=)
       (speech-color-assembly-value value))
      ((member slot-name '("SkinColor" "HairColor" "ClothesColor") :test #'string=)
       (palette-color-assembly-value (or value "Gray")))
      ((string= slot-name "DecalKind")
       (format nil "~10t.byte DecalKind~a" (pascal-case (string (or value "Human")))))
      ((string= slot-name "Shield")
       (format nil "~10t.byte Shield~a" (pascal-case (string (or value "NoShield")))))
      ((string= slot-name "Equipment")
       (format nil "~10t.byte Equip~a" (pascal-case (string (or value "None")))))
      ((string= slot-name "Facing")
       (format nil "~10t.byte ActorFacing~a" (pascal-case (string (or value "Down")))))
      ((string= slot-name "Action")
       (format nil "~10t.byte Action~a" (pascal-case (string (or value "Idle")))))
      ((and value (member slot-name '("HP" "MaxHP") :test #'string=))
       (format nil "~10t.word ~5,'0d"
               (floor (* #x100 (parse-integer (princ-to-string value))))))
      (value
       (ecase slot-bytes
         (1 (format nil "~10t.byte ~a" (parse-integer (princ-to-string value))))
         (2 (format nil "~10t.word ~a" (parse-integer (princ-to-string value))))
         (t (format nil "~10t.byte 0"))))
      (t
       (ecase slot-bytes
         (1 (format nil "~10t.byte 0"))
         (2 (format nil "~10t.word 0"))
         (t (format nil "~10t.byte 0")))))))

(defun class-slot-definitions (class-name)
  "Return alist of (full-field-name . bytes) in layout order for CLASS-NAME."
  (let* ((fields (reverse (first (read-class-fields-from-defs class-name))))
         (total (second (read-class-fields-from-defs class-name))))
    (loop for i from 0 below (length fields)
          for (full-name . offset) = (elt fields i)
          for next-offset = (if (< (1+ i) (length fields))
                                (cdr (elt fields (1+ i)))
                                total)
          collect (cons full-name (- next-offset offset)))))

(defun print-one-object-prototype (name class-name prototype)
  "Emit ca65 prototype block for object NAME of CLASS-NAME."
  (let ((label (format nil "Object_~a" (pascal-case name)))
        (class-size (second (read-class-fields-from-defs class-name))))
    (format t "~%;;;~|~2%~10tAllObjects ..= [[ ~a, ~aClass, ~aSize ]]"
            label class-name class-name)
    (format t "~%;;; ~|~%~a:" label)
    (dolist (slot (class-slot-definitions class-name))
      (destructuring-bind (full-field-name . slot-bytes) slot
        (format t "~2%~10t* = ~a + ~a~%~a"
                label (pascal-case full-field-name)
                (emit-prototype-slot-value class-name full-field-name slot-bytes prototype))))
    (format t "~%~10t* = ~a + ~aSize" label class-name)
    (list label (class-id-for-name class-name) class-size)))

(defun write-object-prototypes (&optional (root (project-root)))
  "Compile @file{Source/Objects/*.json} into ObjectPrototypes.s for the port.

Writes @file{Source/Generated/@var{machine}/ObjectPrototypes.s} with pointer,
class ID, and size tables analogous to @code{ActorPrototypes.s}."
  (format *trace-output* "~&Writing object prototypes to ObjectPrototypes.s…")
  (setf *object-prototype-index* nil)
  (let ((machine-dir (format nil "Source/Generated/~a/" (machine-directory-name))))
    (ensure-directories-exist (merge-pathnames machine-dir root))
    (with-output-to-file (*standard-output*
                          (merge-pathnames (concatenate 'string machine-dir "ObjectPrototypes.s") root)
                          :if-exists :supersede)
      (format t "~&;;; Generated object prototype data from Source/Objects/*.json")
      (format t "~%~10tAllObjects := []")
      (loop for file in (list-object-prototype-json-files root)
            for prototype = (read-object-prototype-json file)
            do (print-one-object-prototype (pathname-name file)
                                             (getf prototype :class)
                                             prototype))
      (format t "~2%
;;; ~|
ObjectPointerL:
~10t.for ci := 0, ci < len(AllObjects), ci += 1
~12t.byte <AllObjects[ci][0]
~10t.next
ObjectPointerH:
~10t.for ci := 0, ci < len(AllObjects), ci += 1
~12t.byte >AllObjects[ci][0]
~10t.next
ObjectClassID:
~10t.for ci := 0, ci < len(AllObjects), ci += 1
~12t.byte AllObjects[ci][1]
~10t.next
ObjectClassSize:
~10t.for ci := 0, ci < len(AllObjects), ci += 1
~12t.byte AllObjects[ci][2]
~10t.next

~10tObjectsCount = len(AllObjects)
"))
    (format *trace-output* " …done.")))

(defun encode-map-spawn-entry (x y kind ref-id)
  "Return five-byte spawn record at tile X,Y."
  (check-type x (integer 0 255))
  (check-type y (integer 0 255))
  (check-type ref-id (integer 0 65535))
  (list x y (ecase kind
             (:character +spawn-kind-character+)
             (:object +spawn-kind-object+))
        (ldb (byte 8 0) ref-id)
        (ldb (byte 8 8) ref-id)))
