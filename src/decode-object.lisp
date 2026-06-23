(in-package :skyline-tool)

(defvar *classes-defs-cache* nil
  "Cached parsed contents of Classes.Defs file.
   Format: ((class-name . ((slot-name . (offset . type)) ...)) ...)")
(defvar *classes-defs-timestamp* 0
  "Timestamp of Classes.Defs file when cache was last updated.")

(defvar *classes-defs-cache* nil
  "Cached parsed contents of Classes.Defs file.
   Format: ((class-name . ((slot-name . (offset . type)) ...)) ...)")
(defvar *classes-defs-timestamp* 0
  "Timestamp of Classes.Defs file when cache was last updated.")

(defun load-classes-defs ()
  "Load and parse Classes.Defs file, using cache if file hasn't changed.
   Returns alist: ((class-name . ((slot-name . (offset . type)) ...)) ...)
   Where type is :string or :numeric."
  (let* ((pathname (merge-pathnames #p"Source/Classes/Classes.Defs"
                                    (uiop:getcwd)))
         (file-time (file-write-date pathname)))
    (when (and *classes-defs-cache*
               (= *classes-defs-timestamp* file-time))
      (return-from load-classes-defs *classes-defs-cache*))
    
    (let ((classes (make-hash-table :test #'equal)))
      (with-input-from-file (classes.defs pathname :if-does-not-exist :error)
        (let ((current-class nil)
              (offset 0))
          (loop for line = (read-line classes.defs nil nil)
                while line
                do (cond
                     ;; Skip empty lines and comments
                     ((or (zerop (length line))
                          (char= #\; (char line 0)))
                      nil)
                     ;; Class definition line: "Class < ParentClass"
                     ((and (> (length line) 2)
                           (char= #\< (char line 1)))
                      (let ((class-name (string-trim #(#\Space #\Tab)
                                                     (subseq line 0 (position #\< line)))))
                        (when (string/= class-name "")
                          (setf current-class class-name
                                offset 0
                                (gethash current-class classes) '()))))
                     ;; Slot definition line: ".SlotName N = ..."
                     ((char= #\. (char line 0))
                      (when current-class
                        (let* ((parts (split-sequence #\Space (subseq line 1)
                                                      :remove-empty-subseqs t))
                               (slot-name (first parts))
                               (slot-bytes (parse-integer (or (second parts) "1")))
                               (definition (subseq line (+ 1 (length slot-name)))))
                          (let* ((up-def (string-upcase definition))
                                 (has-pic (search "PIC" up-def))
                                 (has-av (or (search "A" up-def)
                                             (search "X" up-def)))
                                 (type (if (and has-pic has-av)
                                           :string
                                           :numeric)))
                            (push (cons slot-name (cons offset type))
                                  (gethash current-class classes))
                            (incf offset slot-bytes))))))))
        (let ((result (alist-plist (hash-table-alist classes))))
          (setf *classes-defs-cache* result
                *classes-defs-timestamp* file-time)
          result)))))

(defun get-slot-info (class-name slot-name)
  "Return (offset . type) for SLOT-NAME in CLASS-NAME from Classes.Defs.
   Type is :string or :numeric, or NIL if not found."
  (rest (assoc slot-name
               (rest (assoc class-name
                            (load-classes-defs)
                            :test #'string=))
               :test #'string=)))

(defun string-slot-p (class-name slot-name)
  "Return true if SLOT-NAME in CLASS-NAME is a string slot (defined with PIC X in Classes.Defs)."
  (let ((info (get-slot-info class-name slot-name)))
    (or (eq slot-name :|CharacterName|) ; XXX HACK until the below is tested to work properly
        ;; FIXME the get-slot-info does not detect strings properly yet
        (and info (eq (cdr info) :string)))))

(defun read-class-methods-from-file (&optional (pathname #p"Object/Bank01.Public.NTSC.o.LABELS.txt"))
  (with-input-from-file (labeled pathname :if-does-not-exist :error)
    (let ((classes-table (make-hash-table)))
      (loop for line = (read-line labeled nil nil)
            while line
            do (destructuring-bind (label$ value$$) (split-sequence #\= line)
                 (let* ((label (string-trim #(#\Space) label$))
                        (value$ (string-trim #(#\Space) value$$))
                        (start (- (length label) 12)))
                   (when (and (< 12 (length label))
                              (string= (subseq label start (+ start 12))
                                       "ClassMethods")
                              (char= #\$ (char value$ 0)))
                     (let ((class-name (subseq label 0 start))
                           (address (parse-integer (subseq value$ 1) :radix 16)))
                       (setf (gethash address classes-table) class-name)))))
            finally (return-from read-class-methods-from-file classes-table)))))

;;; FIXME make-pathname
(defun read-class-ids-from-file (&optional (pathname (merge-pathnames
                                                      (format nil "Source/Generated/~a/ClassConstants.s"
                                                              (skyline-tool::machine-directory-name))
                                                      (uiop:getcwd))))
  (with-input-from-file (labeled pathname :if-does-not-exist :error)
    (let ((classes-table (make-hash-table)))
      (loop for line = (read-line labeled nil nil)
            while line
            do (when (find #\= line)
                 (destructuring-bind (label$ value$$) (split-sequence #\= line)
                   (let* ((label (string-trim #(#\Space) label$))
                          (value$ (string-trim #(#\Space) value$$))
                          (start (- (length label) 5)))
                     (when (and (< 5 (length label))
                                (string= (subseq label start)
                                         "Class")
                                (char= #\$ (char value$ 0)))
                       (let ((class-name (subseq label 0 start))
                             (id (parse-integer (subseq value$ 1) :radix 16)))
                         (setf (gethash id classes-table) class-name))))))
            finally (return-from read-class-ids-from-file classes-table)))))

(defun dereference-class (id)
  (let ((ids-classes (read-class-ids-from-file)))
    (gethash id ids-classes)))

(defun read-class-slots-from-defs (class-name &optional
                                                (pathname (merge-pathnames #p"Source/Classes/Classes.Defs"
                                                                           (uiop:getcwd))))
  (when (string= "BasicObject" class-name)
    (return-from read-class-slots-from-defs
      (list (cons (cons "ClassID" 0) nil) 1)))
  (let ((class-name-< (concatenate 'string class-name " < "))
        (offset 0)
        (slots (list)))
    (with-input-from-file (classes.defs pathname :if-does-not-exist :error)
      (let ((parent-class (loop for line = (read-line classes.defs nil nil)
                                while line
                                do (when (and (< (length class-name-<) (length line))
                                              (string= line
                                                       class-name-<
                                                       :end1 (length class-name-<)))
                                     (return (subseq line (length class-name-<))))
                                finally (unless line
                                          (cerror "Ignore and continue"
                                                  "Can't determine parent class of ~s" class-name)
                                          (return-from read-class-slots-from-defs
                                            (list nil 0))))))
        (destructuring-bind (f$ o$) (read-class-slots-from-defs parent-class pathname)
          (setf slots f$ offset o$))
        (loop for line = (read-line classes.defs nil nil)
              while (and line
                         (plusp (length line))
                         (or (char= #\. (char line 0))
                             (char= #\# (char line 0))
                             (char= #\; (char line 0))))
              do (when (char= #\. (char line 0))
                   (let ((parts (split-sequence #\Space (subseq line 1)
                                                :remove-empty-subseqs t)))
                     (let ((slot-name (first parts))
                           (slot-bytes (parse-integer (or (second parts) "1"))))
                       (push (cons (concatenate 'string class-name slot-name) offset) slots)
                       (incf offset slot-bytes)))))
        (list slots offset)))))

(clim:define-presentation-type ext-file-link () :inherit-from 'pathname)

(defvar *inventory-items* nil)

(defun load-inventory-items (&optional (pathname #p"Source/Tables/Inventory.txt"))
  (with-input-from-file (names pathname)
    (setf *inventory-items* (loop for line = (read-line names nil nil)
                                  while line collect line))))

(defun inventory-item-name (number)
  (unless *inventory-items*
    (load-inventory-items))
  (elt *inventory-items* number))

(defun 8.8-float (value)
  (if (plusp (logand #x80 (second value)))
      (- (+ (logxor #xff (second value)) (/ (logxor #xff (first value)) #x100)))
      (+ (second value) (/ (first value) #x100))))


(defun print-slot-value (name value &optional (stream  *standard-output*))
  (print-slot-value% name value stream))

(defgeneric print-slot-value% (slot-keyword-name slot-value stream)
  (:documentation "Print the SLOT-VALUE for SLOT-KEYWORD-NAME to STREAM")
  (:method ((slot-keyword-name t) (slot-value t) (stream t))
    nil)
  (:method ((slot (eql :boat-id)) value s)
    (load-boats)
    (format s " = ")
    (clim:with-output-as-presentation (s #p"Source/Tables/Boats.ods" 'ext-file-link)
      (format s "The “~a”" (getf (reverse (hash-table-plist *boat-ids*)) (elt value 0)))))
  (:method ((slot (eql :stab-course-limit)) value s)
    (format s " = ~dpx" (first value)))
  (:method ((slot (eql :stab-course-distance)) value s)
    (let ((n (8.8-float value)))
      (format s " = ~,2f (~a)" n (rationalize n))))
  (:method ((slot (eql :stab-course-speed)) value s)
    (let ((n (8.8-float value)))
      (format s " = ~,2fpx/f (~a) = ~%~~~,2fpx/s @60Hz = ~~~,2ftiles/s"
              n (rationalize n) (* 60 n) (/ (* 60 n) 16))))
  (:method ((slot (eql :stab-course-forward-p)) value s)
    (format s " = ~[false~:;true~]" (logand #x80 (first value))))
  (:method ((slot (eql :course-finished-p)) value s)
    (format s " = ~[false~:;true~]" (logand #x80 (first value))))
  (:method ((slot (eql :bresenham-course-delta-x)) value s)
    (format s " = $~1,'0x.~4,'0x tiles/frame ≈ ~5f tiles/s"
            (ash (second value) -6) (ash (+ (* #x100 (logand #x3f (second value))) (first value)) 2)
            (/ (+ (first value) (* #x100 (second value))) (* 60.0 (expt 2 14)))))
  (:method ((slot (eql :bresenham-course-delta-y)) value s)
    (format s " = $~1,'0x.~4,'0x tiles/frame ≈ ~5f tiles/s"
            (ash (second value) -6) (ash (+ (* #x100 (logand #x3f (second value))) (first value)) 2)
            (/ (+ (first value) (* #x100 (second value))) (* 60.0 (expt 2 14)) )))
  (:method ((slot (eql :bresenham-course-absolute-delta-x)) value s)
    (format s " = $~4,'0x ≈ ~f tiles"
            (+ (first value) (* #x100 (second value)))
            (/ (+ (first value) (* #x100 (second value))) 1024.0)))
  (:method ((slot (eql :bresenham-course-absolute-delta-y)) value s)
    (format s " = $~4,'0x ≈ ~f tiles"
            (+ (first value) (* #x100 (second value)))
            (/ (+ (first value) (* #x100 (second value))) 1024.0)))
  (:method ((slot (eql :bresenham-course-sign-x)) value s)
    (case (first value)
      (1 (format s " = + (right, east)"))
      (#xff (format s " = - (left, west)"))
      (0 (format s " = zero"))
      (otherwise (format s " = invalid"))))
  (:method ((slot (eql :bresenham-course-sign-y)) value s)
    (case (first value)
      (1 (format s " = + (down, south)"))
      (#xff (format s " = - (up, north)"))
      (0 (format s " = zero"))
      (otherwise (format s " = invalid"))))
  (:method ((slot (eql :bresenham-course-total-length)) value s)
    (format s " = $~4,'0x ≈ ~f tiles"
            (+ (first value) (* #x100 (second value)))
            (/ (+ (first value) (* #x100 (second value))) 1024.0)))
  (:method ((slot (eql :course-waypoint-x)) value s)
    (format s " = ~d" (first value)))
  (:method ((slot (eql :course-waypoint-y)) value s)
    (format s " = ~d" (first value)))
  (:method ((slot (eql :character-speech-pitch)) value s)
    (format s " = ~d" (first value)))
  (:method ((slot (eql :character-speech-speed)) value s)
    (format s " = ~d" (first value)))
  (:method ((slot (eql :character-speech-bend)) value s)
    (format s " = ~d" (first value)))
  (:method ((slot (eql :character-arrows)) value s)
    (format s " = ~d arrow~:p" (first value)))
  (:method ((slot (eql :character-potions)) value s)
    (format s " = ~d potion~:p" (first value)))
  (:method ((slot (eql :character-chalice)) value s)
    (cond ((zerop (first value)) (format s " = no chalice"))
          ((plusp (logand #x80 (first value))) (format s " = empty chalice"))
          (t (format s " = contents are type ~r" (first value)))))
  (:method ((slot (eql :palette-color)) value s)
    (format s " = ~a"
            (case (first value)
              (1 "Peach") (2 "Green") (3 "Purple")
              (5 "Silver") (6 "Orange") (7 "Brown")
              (9 "White") (10 "Gray") (11 "Black")
              (13 "Yellow") (14 "Red") (15 "Blue")
              (otherwise "(invalid value)"))))
  (:method ((slot (eql :character-skin-color)) value s)
    (print-slot-value :palette-color value s))
  (:method ((slot (eql :character-hair-color)) value s)
    (print-slot-value :palette-color value s))
  (:method ((slot (eql :character-clothes-color)) value s)
    (print-slot-value :palette-color value s))
  (:method ((slot (eql :character-inventory)) value s)
    (if (every #'zerop value)
        (format s " = nil")
        (format s "~@< = ~;~{~:(~a~)~^,~_~5t~}~;~:>"
                (let ((bignum (reduce #'logior (loop for i from 0 by 8
                                                     for byte in value
                                                     collecting (ash byte i)))))
                  (loop for bit from 0 below #x80
                        when (plusp (logand (expt 2 bit) bignum))
                          collect (inventory-item-name bit))))))
  (:method ((slot (eql :character-speech-color)) value s)
    (format s " = ~a" (atari-color-name (ash (logand #xf0 (first value)) -4)))
    (unless (= #x0f (logand #x0f (first value)))
      (format s " (likely incorrect value)")))
  (:method ((slot (eql :actor-facing)) value s)
    (format s " = ~a"
            (case (first value)
              (0 "Up") (8 "Left") (12 "Right") (4 "Down")
              (otherwise "unknown"))))
  (:method ((slot (eql :character-action)) value s)
    (format s " = ~a"
            (case (first value)
              (0 "Idle") (1 "Climbing") (2 "Hurt") (3 "Flying")
              (4 "Knocked Back") (5 "Swimming") (6 "Using Equipment") (7 "Wading")
              (8 "Walking") (9 "Waving Arms") (10 "Gesturing")
              (11 "Sleep") (12 "Non-interactive") (13 "Dance")
              (14 "Panic") (15 "Walk (with shield)") (16 "Idle (with shield)")
              (17 "Boating")
              (otherwise "unknown"))))
  (:method ((slot (eql :actor-course)) value s)
    (if (zerop (second value))
        (format s  " = (no course)")
        (format s " = $~4,10x" (+ (* #x100 (second value)) (first value)))))
  (:method ((slot (eql :boat-width)) value s)
    (format s " = ~r tile~:p" (1+ (first value))))
  (:method ((filed (eql :boat-state)) value s)
    (format s " = ~a" (case (first value)
                        (0 "Anchored")
                        (1 "Sailing West")
                        (2 "Sailing East")
                        (otherwise "(invalid)"))))
  (:method ((slot (eql :character-armor-class)) value s)
    (format s " = ~d" (first value)))
  (:method ((slot (eql :character-hp)) value s)
    (format s " = ~:d" (+ (* #x100 (second value)) (first value))))
  (:method ((slot (eql :character-max-hp)) value s)
    (format s " = ~:d" (+ (* #x100 (second value)) (first value))))
  (:method ((slot (eql :character-crowns)) value s)
    (format s " = ~:d crown~:p" (+ (* #x100 (second value)) (first value))))
  (:method ((slot (eql :character-gender)) value s)
    (format s " = ~a" (case (first value)
                        (1 "♂ Male")
                        (2 "☿ Nonbinary")
                        (3 "♀ Female")
                        (otherwise "(invalid)"))))
  (:method ((slot (eql :character-shield)) value s)
    (format s " = ~a"
            (case (first value)
              (1 "Small Shield") (6 "Large Shield")
              (#x80 "No Shield")
              (otherwise "(invalid value)"))))
  (:method ((slot (eql :character-equipment)) value s)
    ;; TODO: #1220 move these into JSON
    (format s " = ~a"
            (case (first value)
              (#x80 "No Item")
              (0 "Knife") (2 "Hammer")
              (3 "Potion") (4 "Sword")
              (6 "Bow") (7 "Bow")
              (8 "Chalice") (9 "Staff")
              (#x0a "Wand") (#x0b "Rope")
              (#x0c "Glass") (#x0d "Wrench")
              (otherwise "(invalid value)"))))
  (:method ((slot (eql :character-aux-item)) value s)
    (format s " = ~a"
            (case (first value)
              (#x80 "No Item")
              (4 "Potion") (#x12 "Chalice")
              (otherwise "(invalid value)"))))
  (:method ((slot (eql :character-character-id)) value s)
    (clim:with-output-as-presentation (s #p"Source/Tables/NPCStats.ods" 'ext-file-link)
      (format s " = ~:(~a~)"
              (cond
                ((= #xff (first value)) "Narrator")
                ((= #xfe (first value)) "The Player")
                (t (if-let (npc (find-if (lambda (npc)
                                           (= (getf npc :character-id) (first value)))
                                         (or *npc-stats* (load-npc-stats))))
                     (getf npc :name)
                     "(unknown)"))))))
  (:method ((slot (eql :particle-kind)) value s)
    (format s " = ~a" (case (first value)
                        (1 "Emote")
                        (2 "Rain Splat")
                        (3 "Rain Splash")
                        (otherwise "(invalid)"))))
  (:method ((slot (eql :character-decal-kind)) value s)
    (format s " = ~a" (case (first value)
                        (0 "The Player")
                        (1 "Generic Human")
                        (2 "Earl Ulluk")
                        (3 "Captain Caspar")
                        (4 "Princess Aisling")
                        (5 "Elder Tranh")
                        (6 "Nefertem")
                        (7 "The Grand Vizier")
                        (8 "Sentinel")
                        (9 "Sailor")
                        (10 "Enemy")
                        (11 "Block 1 NPC")
                        (12 "Block 2 NPC")
                        (13 "Block 3 NPC")
                        (14 "Block 4 NPC")
                        (15 "(Reserved for expansion)")
                        (otherwise "(invalid)")))))

(defun decode-object (dump &optional offset everything)
  (let* ((class-id (elt dump 0))
         (class-name (dereference-class class-id))
         other-objects)
    (unless class-name
      (format t "~2&#<Malformed object (with class ID $~2,'0x)~@[ at $~2,'0x~]>"
              class-id offset)
      (return-from decode-object))
    (format t "~2&#<" )
    (clim:with-text-size (t :large)
      (format t "Instance of ~a" class-name))
    (when offset
      (format t " at $~4,'0x" offset))
    (destructuring-bind (class-slots class-size) (read-class-slots-from-defs class-name) 
      (format t "~%~5t~:d slot~:p using ~:d byte~:p (reserves ~:d byte~:p)~%"
              (length class-slots) class-size (* 8 (ceiling class-size 8)))
      (when everything
        (let ((bam-start (object-address->bam-block offset)))
          (when (plusp bam-start)
            (loop for bam
                  from bam-start
                    below (+ bam-start (ceiling class-size 8))
                  unless (plusp (elt everything (+ (find-label-from-files "ObjectsBAM") bam)))
                    do (progn
                         (fresh-line)
                         (clim:surrounding-output-with-border (t :shape :drop-shadow
                                                                 :ink clim:+red+)
                           (format t "⚠ BAM clear for block $~2x starting $~4,'0x"
                                   bam
                                   (bam-block->object-address bam))))))))
      (etypecase *standard-output*
        (swank/gray::slime-output-stream
         (loop for i from (1- (length class-slots)) downto 0
               for info = (elt class-slots i)
               for slot-name = (car info)
               for slot-start = (cdr info)
               for next-offset = (if (zerop i)
                                     class-size
                                     (cdr (elt class-slots (1- i))))
               for length = (- next-offset slot-start)
               do (progn
                    (format t "~%~10t~22a" slot-name)
                    (format t "@ $~2,'0x" slot-start)
                    (format t " = ")
                    (format t "~{~2,'0x~^ ~2,'0x~^ ~2,'0x~^ ~2,'0x~^   ~2,'0x~^ ~2,'0x~^ ~2,'0x~^ ~2,'0x~^~}"
                            (coerce (subseq dump slot-start next-offset)
                                    'list))
                    (if (string= "CharacterName" slot-name)
                        (format t "~%“~a”"
                                (minifont->unicode
                                 (subseq dump slot-start
                                         (+ slot-start
                                            (elt dump (cdr (elt class-slots (1+ i))))))))
                        (print-slot-value (make-keyword
                                           (string-upcase
                                            (cl-change-case:param-case slot-name)))
                                          (coerce (subseq dump slot-start next-offset)
                                                  'list)))
                    (when (string= "ActorCourse" slot-name)
                      (push (+ (* #x100 (elt dump (+ 2 (cdr (elt class-slots (1+ i))))))
                               (elt dump (+ 1 (cdr (elt class-slots (1+ i))))))
                            other-objects))
                    (when (string= "ItemWielder" slot-name)
                      (let ((wielder (subseq dump slot-start next-offset)))
                        (push (+ (* #x100 (elt wielder 1)) (elt wielder 0))
                              other-objects)))
                    (terpri))))
        (t
         (fresh-line)
         (clim:formatting-table (t)
           (loop for i from (1- (length class-slots)) downto 0
                 for info = (elt class-slots i)
                 for slot-name = (car info)
                 for slot-start = (cdr info)
                 for next-offset = (if (zerop i)
                                       class-size
                                       (cdr (elt class-slots (1- i))))
                 for length = (- next-offset slot-start)
                 do (clim:formatting-row
                        (t)
                      (clim:formatting-cell (t)
                        (format t "~10t~22a" slot-name))
                      (clim:formatting-cell (t)
                        (format t "@ $~2,'0x" slot-start))
                      (clim:formatting-cell (t)
                        (format t " = "))
                      (clim:formatting-cell (t)
                        (format t "~{~2,'0x~^ ~2,'0x~^ ~2,'0x~^ ~2,'0x~^   ~2,'0x~^ ~2,'0x~^ ~2,'0x~^ ~2,'0x~^~}"
                                (coerce (subseq dump slot-start next-offset)
                                        'list))
                        (if (string= "CharacterName" slot-name)
                            (clim:with-text-style (t
                                                   (clim:make-text-style :serif :italic 16))
                              (format t "~%“~(~a~)”"
                                      (minifont->unicode
                                       (subseq dump slot-start
                                               (+ slot-start
                                                  (elt dump (cdr (elt class-slots (1+ i)))))))))
                            (print-slot-value (make-keyword
                                               (string-upcase
                                                (cl-change-case:param-case slot-name)))
                                              (coerce (subseq dump slot-start next-offset)
                                                      'list)))
                        (when (string= "ActorCourse" slot-name)
                          (push (+ (* #x100 (elt dump (+ 2 (cdr (elt class-slots (1+ i))))))
                                   (elt dump (+ 1 (cdr (elt class-slots (1+ i))))))
                                other-objects))
                        (when (string= "ItemWielder" slot-name)
                          (let ((wielder (subseq dump slot-start next-offset)))
                            (push (+ (* #x100 (elt wielder 1)) (elt wielder 0))
                                  other-objects))))
                      (terpri)))
           (clim:formatting-row (t)
             (clim:formatting-cell (t))
             (clim:formatting-cell (t))
             (clim:formatting-cell (t))
             (clim:formatting-cell (t)
               (format t "~32t >~%")))))))
    (values class-name (remove-if #'null other-objects))))

(defun decode-object-at (dump &optional (offset 0))
  (let ((offset (etypecase offset
                  (number offset)
                  (string (parse-integer offset :radix 16)))))
    (if (= #xff00 (logand #xff00 offset))
        (princ "Scenery object only")
        (multiple-value-bind (class-name other-objects)
            (decode-object (subseq dump offset) offset dump)
          (when other-objects
            (dolist (other-object other-objects)
              (when (and other-object (> other-object #x100))
                (decode-object-at dump other-object))))
          class-name))))

(defun decode-all-objects (&optional (dump (load-dump-into-mem)))
  (decode-player-object dump)
  (loop for i from 0 below #x40
        for object-start = (+ (elt dump (+ i (find-label-from-files "ObjectL")))
                              (* #x100 (elt dump (+ i (find-label-from-files "ObjectH")))))
        with classes = (list)
        unless (zerop object-start)
          do (progn
               (clim:with-text-face (t :bold)
                 (format t "~2&Object # ~d:" i))
               (push (decode-object-at dump object-start) classes))
        finally
           (prog1
               (let ((summary (make-hash-table :test 'equalp)))
                 (dolist (class classes)
                   (incf (gethash class summary 0)))
                 (when (hash-table-count summary)
                   (format t "~2%Classes in use:~{~%~3t~4:d × ~a~}~%"
                           (alist-plist
                            (reverse (sort (cons (cons "Player" 1) (hash-table-alist summary))
                                           #'string>
                                           :key #'car)))))
                 summary))))

(defun object-address->bam-block (address)
  (let ((relative (- address (find-label-from-files "Objects0"))))
    #+()(assert (>= (logand #xff address) #x40) ()
                "Object address is within animation buffer: $~4,'0x" address)
    (+ (* (floor relative #x100) (/ #xc0 8))
       (floor (mod relative #x100) 8))))

(defun bam-block->object-address (bam)
  (let ((page (floor bam (/ #xc0 8)))
        (offset (mod bam (/ #xc0 8))))
    (+ (find-label-from-files "Objects0") (* #x100 page) (* 8 offset))))

(defun size-of-object-at (dump address)
  (let* ((subseq (subseq dump address))
         (class-id (elt subseq 0))
         (class-name (dereference-class class-id)))
    (destructuring-bind (class-slots class-size) (read-class-slots-from-defs class-name)
      (declare (ignore class-slots))
      (* 8 (ceiling class-size 8)))))

(defun mark-object-visited (dump object-start visited)
  (let ((size (ceiling (size-of-object-at dump object-start) 8))
        (bam (object-address->bam-block object-start)))
    #+ ()
    (format t "~&  object at $~4,'0x length $~2,'0x BAM $~2,'0x" object-start size bam)
    (when (<= #xc0 (+ size (object-address->bam-block object-start)))
      (warn "Object starting at $~4,'0x length $~2,'0x overruns end of object store"
            object-start size)
      (setf size (- #xc0 (object-address->bam-block object-start))))
    (dotimes (i size)
      (setf (aref visited (+ i bam)) object-start))))

(defun mark-and-sweep-objects (&key (dump (load-dump-into-mem)) (quietp nil))
  (loop for i from 0 below #x40
        for object-start = (+ (elt dump (+ i (find-label-from-files "ObjectL")))
                              (* #x100 (elt dump (+ i (find-label-from-files "ObjectH")))))
        with visited = (make-array (list #xc0) :initial-element nil)
        unless (< object-start (find-label-from-files "Objects0"))
          do (mark-object-visited dump object-start visited)
        finally (loop for j from 0 below #xc0
                      for bam = (elt dump (+ j (find-label-from-files "ObjectsBAM")))
                      for visitation = (aref visited j)
                      with unreachable = (list)
                      with squatters = (list)
                      do (cond
                           ((or (and (plusp bam) visitation)
                                (and (zerop bam) (not visitation)))
                            #+ () (format t "~& ( $~2,'0x used by $~2,'0x )" j visitation))
                           ((plusp bam)
                            (unless quietp
                              (terpri)
                              (clim:surrounding-output-with-border
                                  (t :shape :drop-shadow
                                     :ink clim:+red+)
                                (format t "⚠ Block $~2,'0x allocated in BAM (value $~2,'0x) but not reachable:  ($~4,'0x)"
                                        j bam (bam-block->object-address j))))
                            (push j unreachable ))
                           (visitation
                            (unless quietp
                              (terpri)
                              (clim:surrounding-output-with-border
                                  (t :shape :drop-shadow
                                     :ink clim:+red+)
                                (format t "⚠ Block NOT allocated in BAM but reachable to objects: $~2,'0x ($~4,'0x)"
                                        j (bam-block->object-address j))))
                            (push j squatters)))
                      finally (return-from mark-and-sweep-objects
                                (values unreachable squatters)))))

(defun room-for-objects (&optional (dump (load-dump-into-mem)))
  (multiple-value-bind (unreachable squatters) (mark-and-sweep-objects :dump dump :quietp t)
    (format t "~%Object pool map (○ available, ● used~@[, ☠ unreachable~]~@[, ✗squatters~])"
            unreachable squatters)
    (let ((longest-span 0)
          (span-blocks 0)
          (free-blocks 0)
          (rows (list)))
      (loop for i from 0 below #xc0
            for bam = (elt dump (+ #x6240 i))
            with current-row = (list)
            if (zerop (mod i 24))
              do (setf span-blocks 0)
            if (zerop bam)
              do (if (member i squatters)
                     (progn (when (> span-blocks longest-span)
                              (setf longest-span span-blocks))
                            (setf span-blocks 0)
                            (push "✗" current-row))
                     (progn (incf free-blocks)
                            (incf span-blocks)
                            (when (> span-blocks longest-span)
                              (setf longest-span span-blocks))
                            (push "○" current-row)))
            else do (if (member i unreachable)
                        (progn (setf span-blocks 0)
                               (push "☠" current-row))
                        (progn (setf span-blocks 0)
                               (push "●" current-row)))
            if (zerop (mod (1+ i) 24))
              do (progn (setf span-blocks 0)
                        (push (format nil "Pool ~d ($~2,'0xXX)" (floor i 24)
                                      (floor (+ (find-label-from-files "Objects0") (* #x100 (floor i 24)))
                                             #x100))
                              current-row)
                        (setf rows (append rows (list (reverse current-row))))
                        (setf current-row (list))))
      (etypecase *standard-output*
        (swank/gray::slime-output-stream
         (format t "~{~%~{~a~^ ~}~}" rows))
        (t (terpri)
         (clim:formatting-table (t)
           (clim:formatting-row (t)
             (loop for addr from #x40 below #x100 by 8
                   do (clim:formatting-cell (t)
                        (format t "~2,'0x" addr))))
           (dolist (row rows)
             (clim:formatting-row (t)
               (dolist (el row)
                 (clim:formatting-cell (t)
                   (princ el))))))))

      (format t "~&
Room for objects:
~10tTotal: $C0 (192) blocks = $600 (1,536) bytes
~10tUsed: $~x (~:*~d) block~:p = $~x (~:*~:d) bytes = ~d%
~10tFree: $~x (~:*~d) block~:p = $~x (~:*~:d) bytes = ~d%
~10tLargest free span: $~x (~:*~d) blocks = $~x (~:*~:d) bytes"
              (- #xc0 free-blocks) (* 8 (- #xc0 free-blocks))
              (round (* 100 (/ (- #xc0 free-blocks) #xc0)))
              free-blocks (* 8 free-blocks)
              (round (* 100 (/ free-blocks #xc0)))
              longest-span (* 8 longest-span)))
    #+ () (when unreachable
            (terpri) (terpri)
            (clim:surrounding-output-with-border (t :shape :drop-shadow
                                                    :ink clim:+red+)
              (if (= 1 (length unreachable))
                  (format t "⚠ An unreachable leaked object exists:")
                  (format t "⚠ Unreachable leaked objects exist:")))
            (dolist (bam unreachable)
              (let ((address (bam-block->object-address bam)))
                (format t "~2&Unreachable object? block $~2,'0x for address $~4,'0x"
                        bam address)
                (decode-object-at dump address))))))

(defun decode-self-object (&optional (dump (load-dump-into-mem)))
  (multiple-value-bind (low pointer) (dump-peek "Self")
    (let ((high (dump-peek (1+ pointer))))
      (decode-object-at dump (+ (* #x100 high) low)))))

(defun decode-player-object (&optional (dump (load-dump-into-mem)))
  (decode-object-at dump (find-label-from-files "PlayerValues")))

(defun show-self-object ()
  "Describe the object pointed-to by the Self pointer from a dump"
  (clim-simple-echo:run-in-simple-echo #'decode-self-object
                                       :height 850
                                       :process-name "Decode Self Objects"
                                       :window-title "Object “Self”"))

(defun show-all-objects ()
  "Decode all objects currently in the object heap"
  (clim-simple-echo:run-in-simple-echo #'decode-all-objects
                                       :height 850
                                       :process-name "All Objects"))

(defun show-player-object ()
  "Describe the Player object from a dump"
  (clim-simple-echo:run-in-simple-echo #'decode-player-object
                                       :height 850
                                       :process-name "Player Object"))

(defun show-room-for-objects ()
  "Show how much room objects take up in the dump"
  (clim-simple-echo:run-in-simple-echo #'room-for-objects
                                       :width 1000
                                       :height 500
                                       :process-name "Room for Objects"))

(defun swap-bytes (value)
  (logior (ash (logand value #xff00) -8) (ash (logand value #x00ff) 8)))

(defun music-source-kind (value)
  (case value
    (0 "Background Music")
    (1 "Foley Sound")
    (2 "Incidental Sound")
    (3 "Incidental Music")
    (#x80 "Vacant sound channel")
    (otherwise (format nil "(invalid source, value $~2,'0x)" value))))

(defun look-up-song-id (value)
  (or (when-let (key (assocdr value
                              (reverse
                               (hash-table-alist
                                (gethash :song
                                         (nth-value 1
                                                    (read-assets-list)))))))
        (format nil "“~a”" (title-case key)))
      (format nil "Song # $~2,'0x" value)))

(defun envelope-stage-name (value)
  (case value
    (0 "Attack") (1 "Sustain") (2 "Decay") (3 "Release")
    (otherwise (format nil "(Invalid envelope stage ~d)" value))))

(defun instrument-name (value)
  (let ((orchestra (get-orchestration)))
    (if (< value (length orchestra))
        (getf (elt orchestra value) :instrument)
        (format nil "Undefined instrument # ~d" value))))

(defun echo-music-stats ()
  (fresh-line)
  (clim:with-text-size (t :large)
    (format t "Music Stats"))
  (terpri)
  (clim:surrounding-output-with-border
      (t :shape :drop-shadow)
    (clim:with-text-size (t :large)
      (format t "Sound Channels"))
    (let ((source (find-label-from-files "SoundChannelSource"))
          (bank (find-label-from-files "SoundChannelBank"))
          (h (find-label-from-files "SoundChannelH"))
          (l (find-label-from-files "SoundChannelL"))
          (timer (find-label-from-files "SoundChannelTimer"))
          (head (find-label-from-files "SoundChannelHead"))
          (tail (find-label-from-files "SoundChannelTail")))
      (dotimes (i (find-label-from-files "MaxSoundChannels"))
        (if (plusp (dump-peek (+ h i)))
            (format t "~%Channel ~d. ~a $~2,'0x:~2,'0x~2,'0x
~10TTime: ~d frame~:p; Head $~2,'0x; Tail $~2,'0x (length ~d)"
                    i (music-source-kind (dump-peek (+ source i)))
                    (dump-peek (+ bank i)) (dump-peek (+ h i)) (dump-peek (+ l i))
                    (dump-peek (+ timer i)) (dump-peek (+ head i)) (dump-peek (+ tail i))
                    (mod (- (+ #x100 (dump-peek (+ tail i))) (dump-peek (+ head i))) #x100))
            (format t "~%Channel ~d. (open)" i)))))
  (terpri)
  (clim:surrounding-output-with-border
      (t :shape :drop-shadow)
    (clim:with-text-size (t :large)
      (format t "Voices"))
    (let ((last-hokey (find-label-from-files "LastHokeyVoice"))
          (source (find-label-from-files "VoiceSource"))
          (f (find-label-from-files "VoiceF"))
          (error-low (find-label-from-files "VoiceErrorLow"))
          (error-high (find-label-from-files "VoiceErrorHigh"))
          (error-phase (find-label-from-files "VoiceErrorPhase"))
          (time (find-label-from-files "VoiceTime"))
          (instrument (find-label-from-files "VoiceInstrument"))
          (max-volume (find-label-from-files "VoiceMaxVolume"))
          (volume (find-label-from-files "VoiceVolume"))
          (volume-fraction (find-label-from-files "VoiceVolumeFraction"))
          (envelope-stage (find-label-from-files "VoiceEnvelopeStage"))
          (envelope-timer (find-label-from-files "VoiceEnvelopeTimer"))
          (tremor (find-label-from-files "VoiceTremor"))
          (vibe (find-label-from-files "VoiceVibe")))
      (dotimes (i (find-label-from-files "NumVoices"))
        (let ((voice-id (if (<= i last-hokey)
                            (format nil "Hokey ~d" i)
                            (format nil "TIA ~d" (- i last-hokey)))))
          (if (zerop (logand #x80 (dump-peek (+ source i))))
              (format t "~%Voice ~a. ~a
~10tF: $~2,'0x; Error: ~3,3f ($~2,'0x)
~10tTime: ~d frame~:p; ~a
~10tVolume: ~3,3f (max: ~d)
~10t~a, ~d frame~:p
~10tTremor $~2,'0x; Vibe: $~2,'0x"
                      voice-id (music-source-kind (dump-peek (+ source i)))
                      (dump-peek (+ f i))
                      (+ (dump-peek (+ error-high i))
                         (/ (dump-peek (+ error-low i)) #x100))
                      (dump-peek (+ error-phase i))
                      (dump-peek (+ time i))
                      (instrument-name (dump-peek (+ instrument i)))
                      (+ (dump-peek (+ volume i))
                         (/ (dump-peek (+ volume-fraction i)) #x100))
                      (dump-peek (+ max-volume i))
                      (envelope-stage-name (dump-peek (+ envelope-stage i)))
                      (dump-peek (+ envelope-timer i))
                      (dump-peek (+ tremor i))
                      (dump-peek (+ vibe i)))
              (format t "~%Voice ~a. (open)" voice-id))))))
  (terpri)
  (when (plusp (logand #x80 (dump-peek "LoopMusicP")))
    (clim:surrounding-output-with-border
        (t :shape :drop-shadow)
      (clim:with-text-size (t :large)
        (format t "Music Requeue Requested"))
      (format t "~%Song: ~a"
              (look-up-song-id (dump-peek "LoopMusicSong")))
      (format t "~%Source: ~a"
              (music-source-kind (dump-peek "LoopMusicSource"))))
    (terpri))
  (when-let (song (dump-peek "CurrentBackgroundSong"))
    (unless (zerop song)
      (clim:surrounding-output-with-border (t :shape :drop-shadow)
        (format t "Background Song: ~a~%"
                (look-up-song-id song)))
      (terpri)))
  (when (plusp (dump-peek "NumIncidentalSongs"))
    (clim:surrounding-output-with-border
        (t :shape :drop-shadow)
      (clim:with-text-size (t :large)
        (format t "Incidental Songs Enqueued"))
      (let ((queue (find-label-from-files "IncidentalSongQueue")))
        (dotimes (i (dump-peek "NumIncidentalSongs"))
          (format t "~%~d. ~a" i (look-up-song-id (dump-peek (+ queue i)))))))
    (terpri))
  (when (plusp (dump-peek "MusicOffP"))
    (clim:with-text-size (t :large)
      (format t "Music is OFF."))))

(defun show-sound-system-info ()
  "Examine songs, channels, voices in a window"
  (clim-simple-echo:run-in-simple-echo #'echo-music-stats
                                       :width 600 :height 1000
                                       :process-name "Sound System"))

(defun echo-all-stacks ()
  (fresh-line)
  (loop for thread in '("Main" "Script" "Stagehand")
        do (clim:surrounding-output-with-border
               (t :shape :drop-shadow)
             (clim:with-text-size (t :large)
               (format t "~a Thread stack" thread))
             (let* ((stack-page (if (string= "Param" thread)
                                    (nth-value 2 (dump-peek "ParamStack"))
                                    #x100))
                    (stack-top (+ stack-page
                                  (find-label-from-files (format nil "~aStackTop" thread))))
                    (stack-bottom (+ stack-page
                                     (find-label-from-files (format nil "~aStackBottom" thread))))
                    (stack-pointer (+ stack-page
                                      (dump-peek (format nil "~aStack" thread))))
                    (canary (find-label-from-files (format nil "~aStackCanary" thread))))
               (format t "~%Top: $~4,'0x … %sp = $~4,'0x … Bottom $~4,'0x  "
                       stack-top stack-pointer stack-bottom)
               (unless (>= stack-top stack-pointer stack-bottom)
                 (clim:surrounding-output-with-border (t :shape :drop-shadow
                                                         :ink clim:+red+)
                   (format t "⚠ Stack pointer out of range")))
               (if (string= "Param" thread)
                   (actual-forth-stack)
                   (progn
                     (if (= stack-pointer stack-top)
                         (format t "~%Ø~%")
                         (loop for stack from stack-top above stack-pointer
                               do (format t "~%$~4,'0x: $~2,'0x" stack (dump-peek stack))
                               when (> stack-top stack (1- stack-pointer))
                                 do (format t "~12T($~4,'0x / $~4,'0x)"
                                            (nth-value 2 (dump-peek stack))
                                            (- (nth-value 2 (dump-peek stack)) 2))
                               when (and (> stack (+ 3 stack-pointer))
                                         (= (nth-value 2 (dump-peek stack))
                                            (nth-value 2 (dump-peek (- stack 2)))))
                                 do (format t " ⚠")))
                     (terpri)
                     (unless (= canary (dump-peek stack-bottom))
                       (clim:surrounding-output-with-border (t :shape :drop-shadow
                                                               :ink clim:+red+)
                         (format t "⚠ Stack canary $~2,'0x overwritten with $~2,'0x"
                                 canary (dump-peek stack-bottom))))))))
        do (fresh-line)))

(defun show-all-stacks ()
  "Show the stacks in a window"
  (clim-simple-echo:run-in-simple-echo #'echo-all-stacks
                                       :width 500 :height 1000
                                       :process-name "Stacks"))

(defun actual-forth-stack ()
  (terpri)
  (if (= #x81 (dump-peek "ForthStack"))
      (format t "~2%     Ø~2%(empty stack)")
      (let ((i (dump-peek "ForthStack")))
        (clim:formatting-table
            (t)
          (loop while (< i #x81)
                do (clim:formatting-row
                       (t)
                     (clim:formatting-cell
                         (t)
                       (format t "$~4,'0x "
                               (+ i (find-label-from-files "ParamStack"))))
                     (clim:formatting-cell
                         (t)
                       (format t " $~2,'0x:  " i))
                     (clim:formatting-cell
                         (t)
                       (format t " $~4,'0x "
                               (nth-value 2 (dump-peek (+ i (find-label-from-files "ParamStack"))))))
                     (clim:formatting-cell
                         (t)
                       (format t " ~:d"
                               (+ (nth-value 2 (dump-peek (+ i (find-label-from-files "ParamStack"))))))))
                do (incf i 2)))))
  (unless (= (find-label-from-files (format nil "ForthStackCanary"))
             (nth-value 2 (dump-peek "ParamStack")))
    (clim:surrounding-output-with-border (t :shape :drop-shadow
                                            :ink clim:+red+)
      (format t "⚠ Stack canary $~4,'0x overwritten with $~4,'0x"
              (find-label-from-files "ForthStackCanary")
              (nth-value 2 (dump-peek "ParamStack"))))))

(defvar *forth-window* nil)

(define-anim-buffer-frame-command (com-refresh-forth :menu t :name t) ()
  (clim:redisplay-frame-panes *forth-window*))

(defun echo-forth-stack ()
  "Print the status of the Forth stack"
  (fresh-line)
  (clim:formatting-table
      (t)
    (clim:formatting-row
        (t)
      (clim:formatting-cell
          (t)
        (clim:surrounding-output-with-border
            (t :shape :drop-shadow)
          (clim:surrounding-output-with-border
              (t :shape :drop-shadow)
            (format t "ParamStack at $~4,'0x" (find-label-from-files "ParamStack"))
            (format t "~%ForthStack: $~2,'0x" (dump-peek "ForthStack"))
            (format t "~%     Depth: ~d" (/ (- #x81 (dump-peek "ForthStack")) 2)))
          (actual-forth-stack))
        )
      (clim:formatting-cell
          (t)
        (clim:surrounding-output-with-border
            (t :shape :drop-shadow)
          (format t "ForthCursor (@$~4,'0x): $~2,'0x:~4,'0x"
                  (find-label-from-files "ForthCursor")
                  (dump-peek "CurrentBank")
                  (nth-value 2 (dump-peek (1+ (find-label-from-files "ForthCursor")))))
          (format t "~&ForthExecuteOneOpcode: $~4,'0x"
                  (find-label-from-files "ForthExecuteOneOpcode")))
        (terpri)
        (clim:surrounding-output-with-border
            (t :shape :drop-shadow)
          (format t "Last opcode: $~2,'0x ~a"
                  (dump-peek "LastForthOp")
                  (pascal-case (string (case (dump-peek "LastForthOp")
                                         (0 'e-o-l)
                                         (13 'push-byte) (1 'push-word)
                                         (2 'set-byte) (3 'set-word)
                                         (4 'get-byte) (5 'get-word)
                                         (6 'execute) (11 'go)
                                         (7 'dup) (8 'drop) (12 'swap)
                                         (9 'unless) (10 'when)

                                         (otherwise "⚠")))))
          (clim:surrounding-output-with-border
              (t :shape :underline)
            (format t "~2%Next opcodes:"))
          (terpri)
          (clim:formatting-table
              (t)
            (loop for i from 1 upto 32
                  with done-yet-p = nil
                  while (and (< i 32) (not done-yet-p))
                  for forth-cursor from (nth-value 2 (dump-peek "ForthCursor"))
                  do (clim:formatting-row
                         (t)
                       (clim:formatting-cell
                           (t)
                         (format t "$~4,'0x:" forth-cursor))
                       (clim:formatting-cell
                           (t)
                         (format t "$~2,'0x" (dump-peek forth-cursor)))
                       (clim:formatting-cell
                           (t)
                         (format t "~a" (pascal-case (string (case (dump-peek forth-cursor)
                                                               (0 'end)
                                                               (13 'push-byte) (1 'push-word)
                                                               (2 'set-byte) (3 'set-word)
                                                               (4 'get-byte) (5 'get-word)
                                                               (6 'execute) (11 'go)
                                                               (7 'dup) (8 'drop) (12 'swap)
                                                               (9 'unless) (10 'when)
                                                               (otherwise "⚠"))))))
                       (clim:formatting-cell
                           (t)
                         (format t "~@[$~4,'0x~]"
                                 (case (dump-peek forth-cursor)
                                   ((1 9 10 11) (nth-value 2 (dump-peek (+ 1 forth-cursor))))
                                   (13 (dump-peek (+ 1 forth-cursor)))
                                   (otherwise nil))))
                       (clim:formatting-cell
                           (t)
                         (format t "~@[~:d~]"
                                 (case (dump-peek forth-cursor)
                                   (0 (setf done-yet-p t) nil)
                                   ((1 9 10 11) (prog1
                                                    (nth-value 2 (dump-peek (+ 1 forth-cursor)))
                                                  (incf forth-cursor 2)))
                                   (13 (prog1
                                           (dump-peek (+ 1 forth-cursor))
                                         (incf forth-cursor 1)))
                                   (otherwise nil))))))))))))

(defun show-forth-stack ()
  "Show the Forth stack in a window"
  (setf *forth-window*
        (clim-simple-echo:run-in-simple-echo #'echo-forth-stack
                                             :width 700 :height 1000
                                             :process-name "Forth Stack")))

(defun decode-dialogue (&optional (dump (load-dump-into-mem)))
  (format t "~2%~a: "
          (minifont->unicode
           (subseq dump (find-label-from-files "DialogueSpeakerName")
                   (+ (find-label-from-files "DialogueSpeakerName")
                      (elt dump (find-label-from-files "DialogueSpeakerNameLength"))))))
  (let ((last-line (dump-peek "DialogueTextLines")))
    (dotimes (line 12)
      (format t "~%“~a”"
              (or (ignore-errors (minifont->unicode
                                  (subseq dump (find-label-from-files (format nil "DialogueLine~x" (1+ line)))
                                          (+ 32 (find-label-from-files (format nil "DialogueLine~x" (1+ line)))))))
                  "✗"))
      (when (= (1+ line) last-line)
        (format t "~%—"))))
  (format t "~%
DialogueLines: ~d …ToShow: ~d …Target: ~d …Shown: ~d …

DialogueTextLines: ~d …Room: ~d"
          (dump-peek "DialogueLines")
          (dump-peek "DialogueLinesToShow")
          (dump-peek "DialogueLinesTarget")
          (dump-peek "DialogueLinesShown")
          (dump-peek "DialogueTextLines")
          (dump-peek "DialogueTextLinesRoom")))

(defun show-dialogue-buffers ()
  "Show the contents of the dialogue buffers"
  (clim-simple-echo:run-in-simple-echo #'decode-dialogue
                                       :width 500 :height 500
                                       :process-name "Dialogue Buffers"))

