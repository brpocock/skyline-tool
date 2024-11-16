(in-package :skyline-tool)

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

(defun read-class-ids-from-file (&optional (pathname #p"Source/Generated/ClassConstants.s"))
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

(defun read-class-fields-from-defs (class-name &optional (pathname #p"Source/Classes/Classes.Defs"))
  (when (string= "BasicObject" class-name)
    (return-from read-class-fields-from-defs
      (list (cons (cons "BasicObjectClassID" 0) nil)
            1)))
  (let ((class-name-< (concatenate 'string class-name " < "))
        (offset 0)
        (fields (list)))
    (with-input-from-file (classes.defs pathname :if-does-not-exist :error)
      (let ((parent-class (loop for line = (read-line classes.defs)
                                do (when (and (< (length class-name-<) (length line))
                                              (string= line
                                                       class-name-<
                                                       :end1 (length class-name-<)))
                                     (return (subseq line (length class-name-<)))))))
        (destructuring-bind (f$ o$) (read-class-fields-from-defs parent-class pathname)
          (setf fields f$ offset o$))
        (loop for line = (read-line classes.defs nil nil)
              while (and line
                         (plusp (length line))
                         (or (char= #\. (char line 0))
                             (char= #\; (char line 0))))
              do (when (char= #\. (char line 0))
                   (destructuring-bind (field-name field-bytes$)
                       (split-sequence #\Space (subseq line 1))
                     (let ((field-bytes (parse-integer field-bytes$)))
                       (push (cons (concatenate 'string class-name field-name) offset) fields)
                       (incf offset field-bytes)))))
        (list fields offset)))))

(clim:define-presentation-type ext-file-link () :inherit-from 'pathname)

(define-show-decal-frame-command (com-open-ext-file :name t :menu t)
    ((pathname 'ext-file-link))
  (clim-sys:make-process (lambda () (uiop:run-program (list "xdg-open" (enough-namestring pathname))))
               :name (format nil "Edit spreadsheet ~a" (enough-namestring pathname))))

(clim:define-presentation-to-command-translator click-for-ext-file
    (ext-file-link com-open-ext-file show-decal-frame
                   :gesture :edit :menu nil
                   :documentation "Open spreadsheet file for editing")
  (pathname)
  (list pathname))

(defvar *inventory-items* nil)

(defun load-inventory-items (&optional (pathname #p"Source/Tables/Inventory.txt"))
  (with-input-from-file (names pathname)
    (setf *inventory-items* (loop for line = (read-line names nil nil)
                                  while line collect line))))

(defun inventory-item-name (number)
  (unless *inventory-items*
    (load-inventory-items))
  (elt *inventory-items* number))

(defgeneric print-field-value (field-keyword-name field-value stream)
  (:documentation "Print the FIELD-VALUE for FIELD-KEYWORD-NAME to STREAM")
  (:method ((field-keyword-name t) (field-value t) (stream t))
    nil)
  (:method ((field (eql :boat-id)) value s)
    (load-boats)
    (format s " = ")
    (clim:with-output-as-presentation (s #p"Source/Tables/Boats.ods" 'ext-file-link)
      (format s "The “~a”" (getf (reverse (hash-table-plist *boat-ids*)) (elt value 0)))))
  (:method ((field (eql :course-finished-p)) value s)
    (format s " = ~[false~:;true~]" (logand #x80 (first value))))
  (:method ((field (eql :course-waypoint-x)) value s)
    (format s " = ~d" (first value)))
  (:method ((field (eql :course-waypoint-y)) value s)
    (format s " = ~d" (first value)))
  (:method ((field (eql :character-speech-pitch)) value s)
    (format s " = ~d" (first value)))
  (:method ((field (eql :character-speech-speed)) value s)
    (format s " = ~d" (first value)))
  (:method ((field (eql :character-speech-bend)) value s)
    (format s " = ~d" (first value)))
  (:method ((field (eql :character-arrows)) value s)
    (format s " = ~d arrow~:p" (first value)))
  (:method ((field (eql :character-potions)) value s)
    (format s " = ~d potion~:p" (first value)))
  (:method ((field (eql :character-chalice)) value s)
    (cond ((zerop (first value)) (format s " = no chalice"))
          ((plusp (logand #x80 (first value))) (format s " = empty chalice"))
          (t (format s " = contents are type ~r" (first value)))))
  (:method ((field (eql :palette-color)) value s)
    (format s " = ~a"
            (case (first value)
              (1 "Peach") (2 "Purple") (3 "Green")
              (5 "Silver") (6 "Orange") (7 "Brown")
              (9 "White") (10 "Gray") (11 "Black")
              (13 "Yellow") (14 "Red") (15 "Blue")
              (otherwise "(invalid value)"))))
  (:method ((field (eql :character-skin-color)) value s)
    (print-field-value :palette-color value s))
  (:method ((field (eql :character-hair-color)) value s)
    (print-field-value :palette-color value s))
  (:method ((field (eql :character-clothes-color)) value s)
    (print-field-value :palette-color value s))
  (:method ((field (eql :character-inventory)) value s)
    (if (every #'zerop value)
        (format s " = nil")
        (format s "~@< = ~;~{~:(~a~)~^,~_~5t~}~;~:>"
                (let ((bignum (reduce #'logior (loop for i from 0 by 8
                                                     for byte in value
                                                     collecting (ash byte i)))))
                  (loop for bit from 0 below #x80
                        when (plusp (logand (expt 2 bit) bignum))
                          collect (inventory-item-name bit))))))
  (:method ((field (eql :character-speech-color)) value s)
    (if (= #x0f (logand #x0f (first value)))
        (format s " = ~a" (atari-color-name (ash (logand #xf0 (first value)) -4)))
        (format s " (likely incorrect value)")))
  (:method ((field (eql :actor-facing)) value s)
    (format s " = ~a"
            (case (first value)
              (0 "Up") (8 "Left") (12 "Right") (4 "Down")
              (otherwise "unknown"))))
  (:method ((field (eql :actor-action)) value s)
    (format s " = ~a"
            (case (first value)
              (0 "Idle") (1 "Climbing") (2 "Hurt") (3 "Flying")
              (4 "Knocked Back") (5 "Swimming") (6 "Using Equipment") (7 "Wading")
              (8 "Walking") (9 "Waving Arms") (10 "Gesturing")
              (11 "Sleep") (12 "Non-interactive") (13 "Dance")
              (14 "Panic") (15 "Walk (with shield)") (16 "Idle (with shield)")
              (17 "Boating")
              (otherwise "unknown"))))
  (:method ((field (eql :actor-course)) value s)
    (if (zerop (second value))
        (format s  " = (no course)")
        (format s " = $~4,10x" (+ (* #x100 (second value)) (first value)))))
  (:method ((field (eql :boat-width)) value s)
    (format s " = ~r tile~:p" (1+ (first value))))
  (:method ((filed (eql :boat-state)) value s)
    (format s " = ~a" (case (first value)
                        (0 "Anchored")
                        (1 "Sailing West")
                        (2 "Sailing East")
                        (otherwise "(invalid)"))))
  (:method ((field (eql :character-armor-class)) value s)
    (format s " = ~d" (first value)))
  (:method ((field (eql :actor-hp)) value s)
    (format s " = ~:d" (+ (* #x100 (second value)) (first value))))
  (:method ((field (eql :actor-max-hp)) value s)
    (format s " = ~:d" (+ (* #x100 (second value)) (first value))))
  (:method ((field (eql :character-crowns)) value s)
    (format s " = ~:d crown~:p" (+ (* #x100 (second value)) (first value))))
  (:method ((field (eql :character-gender)) value s)
    (format s " = ~a" (case (first value)
                        (1 "♂ Male")
                        (2 "♀ Female")
                        (3 "☿ Nonbinary")
                        (otherwise "(invalid)"))))
  (:method ((field (eql :character-shield)) value s)
    (format s " = ~a"
            (case (first value)
              (1 "Small Shield") (6 "Large Shield")
              (#x80 "No Shield")
              (otherwise "(invalid value)"))))
  (:method ((field (eql :character-equipment)) value s)
    ;; TODO move these into JSON
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
  (:method ((field (eql :character-aux-item)) value s)
    (format s " = ~a"
            (case (first value)
              (#x80 "No Item")
              (4 "Potion") (#x12 "Chalice")
              (otherwise "(invalid value)"))))
  (:method ((field (eql :character-character-id)) value s)
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
  (:method ((field (eql :character-decal-kind)) value s)
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
                        (15 "(reserved for expansion)")
                        (otherwise "(invalid)")))))

(defun decode-object (dump &optional offset)
  (let* ((class-id (elt dump 0))
         (class-name (dereference-class class-id))
         (actor-course 0))
    (unless class-name
      (format t "~2&#<Malformed object (with class ID $~2,'0x)~@[ at $~2,'0x~]>"
              class-id offset)
      (return-from decode-object))
    (format t "~2&#<" )
    (clim:with-text-size (t :large)
      (format t "Instance of ~a" class-name))
    (when offset
      (format t " at $~2,'0x" offset))
    (destructuring-bind (class-fields class-size) (read-class-fields-from-defs class-name) 
      (format t "~%~5t~:d field~:p using ~:d byte~:p (reserves ~:d byte~:p)~%"
              (length class-fields) class-size (* 8 (ceiling class-size 8)))
      (etypecase *standard-output*
        (swank/gray::slime-output-stream
         (loop for i from (1- (length class-fields)) downto 0
               for info = (elt class-fields i)
               for field-name = (car info)
               for field-start = (cdr info)
               for next-offset = (if (zerop i)
                                     class-size
                                     (cdr (elt class-fields (1- i))))
               for length = (- next-offset field-start)
               do (progn
                    (format t "~10t~22a" field-name)
                    (format t "@ $~2,'0x" field-start)
                    (format t " = ")
                    (format t "~{~2,'0x~^ ~2,'0x~^ ~2,'0x~^ ~2,'0x~^   ~2,'0x~^ ~2,'0x~^ ~2,'0x~^ ~2,'0x~^~}"
                            (coerce (subseq dump field-start next-offset)
                                    'list))
                    (if (string= "CharacterName" field-name)
                        (format t "~%“~a”"
                                (minifont->unicode
                                 (subseq dump field-start
                                         (+ field-start
                                            (elt dump (cdr (elt class-fields (1+ i))))))))
                        (print-field-value (make-keyword
                                            (string-upcase
                                             (cl-change-case:param-case field-name)))
                                           (coerce (subseq dump field-start next-offset)
                                                   'list)
                                           t))
                    (if (string= "ActorCourse" field-name)
                        (setf actor-course (+ (* #x100 (elt dump (+ 2 (cdr (elt class-fields (1+ i))))))
                                              (elt dump (+ 1 (cdr (elt class-fields (1+ i))))))))
                    (terpri))))
        (t (clim:formatting-table (t)
                                  (loop for i from (1- (length class-fields)) downto 0
                                        for info = (elt class-fields i)
                                        for field-name = (car info)
                                        for field-start = (cdr info)
                                        for next-offset = (if (zerop i)
                                                              class-size
                                                              (cdr (elt class-fields (1- i))))
                                        for length = (- next-offset field-start)
                                        do (clim:formatting-row (t)
                                                                (clim:formatting-cell (t)
                                                                                      (format t "~10t~22a" field-name))
                                                                (clim:formatting-cell (t)
                                                                                      (format t "@ $~2,'0x" field-start))
                                                                (clim:formatting-cell (t)
                                                                                      (format t " = "))
                                                                (clim:formatting-cell (t)
                                                                                      (format t "~{~2,'0x~^ ~2,'0x~^ ~2,'0x~^ ~2,'0x~^   ~2,'0x~^ ~2,'0x~^ ~2,'0x~^ ~2,'0x~^~}"
                                                                                              (coerce (subseq dump field-start next-offset)
                                                                                                      'list))
                                                                                      (if (string= "CharacterName" field-name)
                                                                                          (format t "~%“~a”"
                                                                                                  (minifont->unicode
                                                                                                   (subseq dump field-start
                                                                                                           (+ field-start
                                                                                                              (elt dump (cdr (elt class-fields (1+ i))))))))
                                                                                          (print-field-value (make-keyword
                                                                                                              (string-upcase
                                                                                                               (cl-change-case:param-case field-name)))
                                                                                                             (coerce (subseq dump field-start next-offset)
                                                                                                                     'list)
                                                                                                             t))
                                                                                      (if (string= "ActorCourse" field-name)
                                                                                          (setf actor-course (+ (* #x100 (elt dump (+ 2 (cdr (elt class-fields (1+ i))))))
                                                                                                                (elt dump (+ 1 (cdr (elt class-fields (1+ i)))))))))
                                                                (terpri)))
                                  (clim:formatting-row (t)
                                                       (clim:formatting-cell (t))
                                                       (clim:formatting-cell (t))
                                                       (clim:formatting-cell (t))
                                                       (clim:formatting-cell (t)
                                                                             (format t "~32t >~%")))))))
    (values class-name actor-course)))

(defun decode-object-at (dump &optional (offset 0))
  (let ((offset (etypecase offset
                  (number offset)
                  (string (parse-integer offset :radix 16)))))
    (multiple-value-bind (class-name other-object)
        (decode-object (subseq dump offset) offset)
      (when (and other-object (> other-object #x100))
        (decode-object-at dump other-object))
      class-name)))

(defun decode-all-objects (&optional (dump (load-dump-into-mem)))
  (decode-player-object dump)
  (loop for i from 0 below #x40
        for object-start = (+ (elt dump (+ i (find-label-from-files "ObjectL")))
                              (* #x100 (elt dump (+ i (find-label-from-files "ObjectH")))))
        with classes = (list)
        unless (zerop object-start)
          do (progn
               (format t "~&Object # ~d:" i)
               (push (decode-object-at dump object-start) classes))
        finally
           (prog1
               (let ((summary (make-hash-table :test 'equalp)))
                 (dolist (class classes)
                   (incf (gethash class summary 0)))
                 (when (hash-table-count summary)
                   (format t "~2%Classes in use:~{~%~3t~4:d × ~a~}~%"
                           (alist-plist
                            (reverse (sort (hash-table-alist summary)
                                           #'string>
                                           :key #'car)))))
                 summary)
             (room-for-objects))))

(defun object-address->bam-block (address)
  (let ((relative (- address (find-label-from-files "Objects0"))))
    (assert (>= (logand #xff address) #x40) ()
            "Object address is within animation buffer: $~4,'0x" address)
    (+ (* (floor relative #x100) (/ #xc0 8))
       (floor (mod relative #x100) 8))))

(defun bam-block->object-address (bam)
  (let ((page (floor bam (/ #xc0 8)))
        (offset (mod bam (/ #xc0 8))))
    (+ (find-label-from-files "Objects0") (* #x100 page) (* 8 offset))))

(defun size-of-object-at (dump address)
  (let* ((subseq (subseq dump address))
         (class-methods (+ (elt subseq 0) (* #x100 (elt subseq 1))))
         (class-name (dereference-class class-methods)))
    (destructuring-bind (class-fields class-size) (read-class-fields-from-defs class-name)
      (declare (ignore class-fields))
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
      (setf (aref visited (+ i bam)) (object-address->bam-block object-start)))))

(defun mark-and-sweep-objects (&optional (dump (load-dump-into-mem)))
  (loop for i from 0 below #x40
        for object-start = (+ (elt dump (+ #x6440 i))
                              (* #x100 (elt dump (+ #x6540 i))))
        with visited = (make-array (list #xc0) :initial-element nil)
        unless (zerop object-start)
          do (mark-object-visited dump object-start visited)
        finally (loop for j from 0 below #xc0
                      for bam = (elt dump (+ #x6240 j))
                      for visitation = (aref visited j)
                      do (cond
                           ((or (and (plusp bam) visitation)
                                (and (zerop bam) (not visitation)))
                            (format t "~& ( $~2,'0x used by $~2,'0x )" j visitation))
                           ((plusp bam) (format t "~& Block allocated in BAM ($~2,'0x) but not reachable: $~2,'0x ($~4,'0x)"
                                                bam j (bam-block->object-address j)))
                           (visitation (format t "~& Block NOT allocated in BAM but reachable to objects: $~2,'0x ($~4,'0x)"
                                               j (bam-block->object-address j)))))))

(defun room-for-objects (&optional (dump (load-dump-into-mem)))
  (loop for i from 0 below #xc0
        with longest-span = 0
        with span-blocks = 0
        with free-blocks = 0
        for bam = (elt dump (+ #x6240 i))
        if (zerop bam)
          do (progn (incf free-blocks)
                    (incf span-blocks)
                    (when (> span-blocks longest-span)
                      (setf longest-span span-blocks)))
        else do (setf span-blocks 0)
        if (zerop (mod i 24))
          do (setf span-blocks 0)
        finally (format t "~&
Room for objects:
~10tTotal: $C0 (192) blocks = $600 (1,536) bytes
~10tFree: $~x (~:*~d) blocks = $~x (~:*~:d) bytes
~10tLargest free span: $~x (~:*~d) blocks = $~x (~:*~:d) bytes"
                        free-blocks (* 8 free-blocks)
                        longest-span (* 8 longest-span))))

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
                                                   :process-name "All Objects"
                                                   :window-title "All Objects"))

(defun show-player-object ()
  "Describe the Player object from a dump"
  (clim-simple-echo:run-in-simple-echo #'decode-player-object
                                                   :height 850
                                                   :process-name "Player Object"
                                                   :window-title "Player Object"))
