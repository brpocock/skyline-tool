(in-package :skyline-tool)

(defconstant +spawn-kind-character+ 0
  "Map spawn entry references a @code{CharacterID} from NPC stats.")
(defconstant +spawn-kind-object+ #x80
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
slot slot names for that class hierarchy.

@table @asis
@item PATHNAME
Designator for a @file{.json} file under @file{Source/Objects/}.
@end table

@subsection Outputs
Plist with keys @code{:name}, @code{:class}, and string keys derived from
JSON slot names (maintaining original capitalization)."
  (let ((json:*json-identifier-name-to-lisp* 'string))
    (let* ((json (with-open-file (stream (merge-pathnames pathname)
                                         :direction :input
                                         :if-does-not-exist :error)
                   (json:decode-json stream))))
      (let ((class-entry (loop for (key . value) in json
                               when (string= key "Class")
                                 return (cons key value))))
        (or class-entry
            (error "Prototype ~a missing required \"Class\" key" pathname))
        (alist-plist json)))))

(defun class-id-for-name (class-name)
  "Return numeric ClassID for PascalCase CLASS-NAME from generated constants."
  (let ((ids (read-class-ids-from-file)))
    (gethash class-name ids)))

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

(defun json-slot-value (prototype slot-name)
  "Lookup SLOT-NAME in PROTOTYPE plist (case-insensitive)."
  (getf prototype slot-name))

(defun json-value-for-slot (prototype full-slot-name)
  "Find JSON value for FULL-SLOT-NAME using exact or suffix key match."
  (json-slot-value prototype full-slot-name))

(defun split-into-bytes-big-endian (value bytes)
  (loop for byte from 0 below bytes
        collecting (logand #xff (ash value (- (* 8 byte))))))

(defun emit-prototype-slot-value (class-name full-slot-name
                                  slot-bytes prototype)
  "Return assembly directives for FULL-SLOT-NAME on CLASS-NAME.
   - Class is special: always .byte ClassNameClass (bareword symbol)
   - String values → .byte/.word/.dword SymbolName (bareword, no quotes)
   - Numbers → emit as hex: .byte $xx, .word $xxxx, .dword $xxxxxxxx
   - Lists → emit as byte array (eight .byte $xx per line)
   - Other sizes (>4 bytes) → split into individual bytes"
  (let ((value (json-value-for-slot prototype full-slot-name)))
    (cond
      ((stringp value)
       (if (string-slot-p class-name full-slot-name)
           (format t "~%~10t.text \"~a\"" value)
           (case slot-bytes
             (1 (format t "~%~10t.byte ~a" value))
             (2 (format t "~%~10t.word ~a" value))
             (4 (format t "~%~10t.dword ~a" value))
             (otherwise (error "Can't figure out how this is a weird ~d byte width: ~a"
                               slot-bytes value)))))
      
      ((integerp value)
       (case slot-bytes
         (1 (format t "~%~10t.byte $~2,'0x" value))
         (2 (format t "~%~10t.word $~4,'0x" value))
         (4 (format t "~%~10t.dword $~8,'0x" value))
         (otherwise
          (format t "~{~%~10t.byte $~2,'0x~^, $~2,'0x~^, $~2,'0x~^, $~2,'0x~^,    ~
$~2,'0x~^, $~2,'0x~^, $~2,'0x~^, $~2,'0x~^, $~2,'0x~}"
                  (split-into-bytes-big-endian value slot-bytes)))))
      
      ((listp value)
       (format t "~{~%~10t.byte $~2,'0x~^, $~2,'0x~^, $~2,'0x~^, $~2,'0x~^,    ~
$~2,'0x~^, $~2,'0x~^, $~2,'0x~^, $~2,'0x~^, $~2,'0x~}"
               value))
      
      (t (error "Unhandled value type for ~a: ~a (type: ~a)" 
                full-slot-name value (type-of value))))))

(defun class-slot-definitions (class-name)
  "Return alist of (full-slot-name . bytes) in layout order for CLASS-NAME."
  (let* ((slots (reverse (first (read-class-slots-from-defs class-name))))
         (total (second (read-class-slots-from-defs class-name))))
    (loop for i from 0 below (length slots)
          for (full-name . offset) = (elt slots i)
          for next-offset = (if (< (1+ i) (length slots))
                                (cdr (elt slots (1+ i)))
                                total)
          collect (cons full-name (- next-offset offset)))))

(defun print-one-object-prototype (name class-name prototype)
  "Emit ca65 prototype block for object NAME of CLASS-NAME."
  (let ((label (pascal-case name)))
    (format t "~%~a: .block" label)
    (format t "~%~10t.byte ~aClass" label)
    (loop for (key value) on (nthcdr 2 prototype) by #'cddr
          for class-slot = (find key (class-slot-definitions class-name)
                                 :key #'first :test #'string-equal)
          for slot-bytes = (rest class-slot)
          do (format t "~2%~10t* = ~a + ~a" label key)
          do (emit-prototype-slot-value class-name key slot-bytes prototype))
    (format t "~%~10t* = ~a + ~aSize" label class-name)
    (format t "~%~10t.bend")
    (terpri)
    (list label (class-id-for-name class-name) )))

(defun write-object-prototypes (&optional (root (project-root)))
  "Compile @file{Source/Objects/*.json} into ObjectPrototypes.s for the port."
  (format *trace-output* "~&Writing object prototypes to ObjectPrototypes.s…")
  (setf *object-prototype-index* nil)
  (let* ((machine-dir (format nil "Source/Generated/~a/" (machine-directory-name)))
         (object-protos (loop for name in (directory #p"Source/Objects/*.json")
                              collect (cons (pathname-name name) (read-object-prototype-json name)))))
    (ensure-directories-exist (merge-pathnames machine-dir root))
    (with-output-to-file (*standard-output*
                          (merge-pathnames (concatenate 'string machine-dir "ObjectPrototypes.s") root)
                          :if-exists :supersede)
      (format t "~&;;; Generated object prototype data from Source/Objects/*.json")
      (format t "~2%~10tSpawnableObjects := [~{ Spawnable.~a~^, ~} ]"
              (loop for (name . prototype) in object-protos
                    collect name))
      (format t "~2%SpawnableObjectL: .byte <(SpawnableObjects)")
      (format t "~%SpawnableObjectH: .byte >(SpawnableObjects)")
      (format t "~%~10tNumSpawnableObjects = len(SpawnableObjects)")
      (format t "~2%Spawnable: .block")
      (loop for (name . prototype) in object-protos
            do (let ((outfile (make-pathname :directory (list :relative
                                                              "Source" "Generated"
                                                              (machine-directory-name)
                                                              "Spawnable")
                                             :name name
                                             :type "s")))
                 (format t "~%~10T.include \"Spawnable/~a.s\"" name)
                 (ensure-directories-exist outfile)
                 (with-output-to-file (*standard-output* outfile
                                                         :if-exists :supersede)
                   (format t ";;; Spawnable object ~a (from Source/Objects/~a.json)"
                           name name)
                   (print-one-object-prototype name (getf prototype :|Class|) prototype))))
      (format t "~%~10t.bend"))
    (write-spawnable-object-ids object-protos root))
  (format *trace-output* " …done."))

(defun write-spawnable-object-ids (object-protos root)
  "Write spawnable object ID mapping to Spawn-IDs.lisp for maps.lisp."
  (let ((machine-dir (format nil "Source/Generated/~a/" (machine-directory-name))))
    (ensure-directories-exist (merge-pathnames machine-dir root))
    (with-output-to-file (*standard-output*
                          (merge-pathnames (concatenate 'string machine-dir "Spawn-IDs.lisp") root)
                          :if-exists :supersede)
      (format t "~&;;; Generated spawnable object ID mappings from Source/Objects/*.json")
      (format t "~%Spawnable: .block")
      (loop for (name . prototype) in object-protos
            for id from 0
            do (format t "~%~10t~a = $~2,'0x"
                       (pascal-case name)
                       id))
      (format t "~%~10t.bend"))))

(defun encode-map-spawn-entry (x y kind ref-id)
  "Return five-byte spawn record at tile X,Y."
  (check-type x (unsigned-byte 8))
  (check-type y (unsigned-byte 8))
  (check-type ref-id (unsigned-byte 8))
  (list x y (ecase kind
              (:character +spawn-kind-character+)
              (:object +spawn-kind-object+))
        ref-id))
