;;; Skyline-Tool src/gui/gui-character-identity.lisp
;;; Identity tab for Character Inspector

(in-package :skyline-tool)

(define-constant +faction-bits+
    '((#x80 . "Vizier's Forces")
      (#x40 . "Loyalists")
      (#x20 . "Villagers")
      (#x10 . "Merfolk")
      (#x08 . "(unused)")
      (#x04 . "(unused)")
      (#x02 . "(unused)")
      (#x01 . "(unused)"))
  :test 'equalp
  :documentation "Bitmask entries for the eight faction selectors.")

(define-constant +character-flags-bits+
    '((#x01 . "Enemy Walk On")
      (#x02 . "(undefined)")
      (#x04 . "(undefined)")
      (#x08 . "(undefined)")
      (#x10 . "(undefined)")
      (#x20 . "(undefined)")
      (#x40 . "(undefined)")
      (#x80 . "(undefined)"))
  :test 'equalp
  :documentation "Bitmask entries for the eight character-flags selectors.")

(defun decal-kind->display-name (kind)
  (title-case (string-downcase (string kind))))

(defun parse-8.8-string (string)
  (let ((n (ignore-errors (read-from-string string))))
    (if (realp n)
        (let ((rounded (/ (round (* n 256)) 256.0)))
          (logior (ash (floor rounded) 8)
                  (round (* (- rounded (floor rounded)) 256))))
        0)))

(defun course-class-dropdown-items ()
  (mapcar (lambda (entry)
            (let ((cls (first entry))
                  (depth (second entry)))
              (let ((indent (make-string (* depth 2) :initial-element #\Space))
                    (arrow (if (zerop depth) "" "↳ ")))
                (cons (concatenate 'string indent arrow
                                   (title-case (string-upcase cls)))
                      cls))))
          (class-descendants-flat "Course" *class-bases*)))

(defun display-bitmask-section (pane resource bits slot-name label)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :left)
      (clim:with-text-style (pane (clim:make-text-style :sans-serif :bold :larger))
        (format pane label))))
  (let ((mask (slot-value resource slot-name)))
    (loop for (bit . name) in bits
          do (clim:formatting-row (pane)
               (clim:formatting-cell (pane :align-x :left)
                 (clim:make-pane 'clim:check-box-pane
                                 :value (logtest bit mask)
                                 :label name
                                 :callback
                                 (lambda (gadget value)
                                   (declare (ignore gadget))
                                   (if value
                                       (setf (slot-value resource slot-name)
                                             (logior (slot-value resource slot-name) bit))
                                       (setf (slot-value resource slot-name)
                                             (logand (slot-value resource slot-name)
                                                     (lognot bit))))
                                   (publish-resource-changed resource))))
               (clim:formatting-cell (pane)
                 (format nil "$~2,'0x" bit))))))

;; Identity tab — dispatch read-only vs editing by frame-view-mode
(defmethod display-identity-tab ((frame character-inspector-frame) pane)
  (ecase (frame-view-mode frame)
    (:reference (display-identity-reference frame pane))
    (:editing   (display-identity-editing frame pane))))

;; Reference view — read-only display
(defmethod display-identity-reference ((frame character-inspector-frame) pane)
  (let ((resource (frame-resource frame)))
    (clim:formatting-table (pane)
      (display-identity-reference-intro resource pane)
      (display-identity-reference-movement resource pane)
      (display-identity-reference-hit-points resource pane)
      (display-identity-reference-faction resource pane)
      (display-identity-reference-flags resource pane))))

(defun display-identity-reference-intro (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Name: "))
    (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-name resource))))
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "ID: "))
    (clim:formatting-cell (pane :align-x :left) (format pane "~d" (game-resource-character-id resource))))
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Gender: "))
    (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-gender resource))))
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Home: "))
    (clim:formatting-cell (pane :align-x :left) (format pane "~a" (or (game-resource-character-home resource) "None"))))
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Comments: "))
    (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-memo resource)))))

(defun display-identity-reference-movement (resource pane)
  (clim:formatting-row (pane) (clim:formatting-cell (pane :align-x :left) (format pane " ")))
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :left)
      (clim:with-text-style (pane (clim:make-text-style :sans-serif :bold :larger))
        (format pane "Movement"))))
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Course Class: "))
    (clim:formatting-cell (pane :align-x :left)
      (format pane "~a" (or (game-resource-character-course-class resource) "Course"))))
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Course Prototype: "))
    (clim:formatting-cell (pane :align-x :left)
      (format pane "~a" (let ((p (game-resource-character-course-prototype resource)))
                          (if (eq p 0) "Zeroes" (or p "Zeroes")))))))

(defun display-identity-reference-hit-points (resource pane)
  (clim:formatting-row (pane) (clim:formatting-cell (pane :align-x :left) (format pane " ")))
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :left)
      (clim:with-text-style (pane (clim:make-text-style :sans-serif :bold :larger))
        (format pane "Hit Points"))))
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "HP: "))
    (clim:formatting-cell (pane :align-x :left)
      (let ((hp (game-resource-character-hp resource))
            (max (game-resource-character-max-hp resource)))
        (format pane "~d.~2,'0d / ~d.~2,'0d max"
                (ldb (byte 8 8) hp) (ldb (byte 8 0) hp)
                (ldb (byte 8 8) max) (ldb (byte 8 0) max))))))

(defun display-identity-reference-faction (resource pane)
  (clim:formatting-row (pane) (clim:formatting-cell (pane :align-x :left) (format pane " ")))
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :left)
      (clim:with-text-style (pane (clim:make-text-style :sans-serif :bold :larger))
        (format pane "Faction"))))
  (loop for (bit . name) in +faction-bits+
        do (clim:formatting-row (pane)
             (clim:formatting-cell (pane :align-x :right)
               (format pane "~:[☐~;☑~]" (logtest bit (game-resource-character-faction resource))))
             (clim:formatting-cell (pane :align-x :left)
               (format pane "~a | $~2,'0x" name bit)))))

(defun display-identity-reference-flags (resource pane)
  (clim:formatting-row (pane) (clim:formatting-cell (pane :align-x :left) (format pane " ")))
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :left)
      (clim:with-text-style (pane (clim:make-text-style :sans-serif :bold :larger))
        (format pane "Character Flags"))))
  (loop for (bit . name) in +character-flags-bits+
        do (clim:formatting-row (pane)
             (clim:formatting-cell (pane :align-x :right)
               (format pane "~:[☐~;☑~]" (logtest bit (game-resource-character-flags resource))))
             (clim:formatting-cell (pane :align-x :left)
               (format pane "~a | $~2,'0x" name bit)))))

;; Editing view — gadget-based editable display
(defmethod display-identity-editing ((frame character-inspector-frame) pane)
  (let ((resource (frame-resource frame)))
    (clim:formatting-table (pane :multiple-columns t)
      (display-identity-editing-name resource pane)
      (display-identity-editing-id resource pane)
      (display-identity-editing-gender resource pane)
      (display-identity-editing-home resource pane)
      (display-identity-editing-comments resource pane)
      (display-identity-editing-movement resource pane)
      (display-identity-editing-hit-points resource pane)
      (display-identity-editing-faction resource pane)
      (display-identity-editing-flags resource pane))))

(defun validate-name-for-display (name)
  "Validate NAME for minifont compatibility and length. Returns error indicator or NIL."
  (let ((bad-chars
         (remove-if (lambda (c) (find c +all-minifont-chars+ :test #'char-equal))
                    (coerce name 'list))))
    (when bad-chars
      (return-from validate-name-for-display
        (format nil "can't use: ~{~c~^, ~}" bad-chars))))
  (let ((len (length (unicode->minifont name))))
    (when (> len 12)
      (return-from validate-name-for-display "too long"))))

(defun display-identity-editing-name (resource pane)
  (let* ((name (game-resource-character-name resource))
         (error (validate-name-for-display name)))
    (clim:formatting-row (pane)
      (clim:formatting-cell (pane :align-x :right) (format pane "Name: "))
      (clim:formatting-cell (pane :align-x :left)
        (clim:make-pane 'clim:text-field-pane
                        :value name
                        :value-changed-callback
                        (lambda (gadget value)
                          (declare (ignore gadget))
                          (setf (game-resource-character-name resource) value)
                          (publish-resource-changed resource))))
      (clim:formatting-cell (pane :align-x :left)
        (format pane "~2d/12" (length (unicode->minifont name)))))
    (when error
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane " "))
        (clim:formatting-cell (pane :align-x :left)
          (clim:with-drawing-options (pane :ink clim:+red+)
            (format pane "~a ~a"
                    (if (string= error "too long") "🚫" "⛔")
                    error)))))))

(defun display-identity-editing-id (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Character ID: "))
    (clim:formatting-cell (pane :align-x :left)
      (format pane "$~2,'0x" (game-resource-character-id resource)))))

(defun display-identity-editing-decal (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Decal Kind: "))
    (clim:formatting-cell (pane :align-x :left)
      (clim:make-pane 'clim:text-field-pane
                      :value (game-resource-character-decal resource)
                      :activation-callback
                      (lambda (pane)
                        (setf (game-resource-character-decal resource) (clim:gadget-value pane))
                        (publish-resource-changed resource))))))

(defun display-identity-editing-gender (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Gender: "))
    (clim:formatting-cell (pane :align-x :left)
      (clim:make-pane 'clim:radio-box-pane
                      :items '(("Masculine (he)" . :male)
                               ("Feminine (she)" . :female)
                               ("Indefinite (they)" . :indefinite)
                               ("Impersonal (it)" . :impersonal))
                      :current-value (game-resource-character-gender resource)
                      :callback
                      (lambda (pane value)
                        (declare (ignore pane))
                        (setf (game-resource-character-gender resource) value)
                        (publish-resource-changed resource))))))

(defun display-identity-editing-home (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Home: "))
    (clim:formatting-cell (pane :align-x :left)
      (clim:make-pane 'clim:text-field-pane
                      :value (game-resource-character-home resource)
                      :activation-callback
                      (lambda (pane)
                        (setf (game-resource-character-home resource) (clim:gadget-value pane))
                        (publish-resource-changed resource))))))

(defun display-identity-editing-comments (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Comments: "))
    (clim:formatting-cell (pane :align-x :left)
      (clim:make-pane 'clim:text-field-pane
                      :value (game-resource-character-memo resource)
                      :lines 6
                      :scroll-bars :vertical
                      :value-changed-callback
                      (lambda (gadget value)
                        (declare (ignore gadget))
                        (setf (game-resource-character-memo resource) value)
                        (publish-resource-changed resource))))))

(defun display-identity-editing-movement (resource pane)
  (clim:with-text-style (pane (clim:make-text-style :sans-serif :bold :larger))
    (format pane "Movement"))
  (display-identity-editing-course-class resource pane)
  (display-identity-editing-course-prototype resource pane))

(defun display-identity-editing-course-class (resource pane)
  (clim:formatting-table (pane)
    (clim:formatting-row (pane)
      (clim:formatting-cell (pane :align-x :right) (format pane "Course Class: "))
      (clim:formatting-cell (pane :align-x :left)
        (clim:make-pane 'clim:option-pane
                        :items (course-class-dropdown-items)
                        :current-value (game-resource-character-course-class resource)
                        :callback
                        (lambda (pane value)
                          (declare (ignore pane))
                          (setf (game-resource-character-course-class resource) value)
                          (let ((valid (list-object-prototypes-for-class value)))
                            (unless (or (eq (game-resource-character-course-prototype resource) 0)
                                        (member (game-resource-character-course-prototype resource)
                                                valid :test #'string=))
                              (setf (game-resource-character-course-prototype resource) 0)))
                          (publish-resource-changed resource)))))))

(defun display-identity-editing-course-prototype (resource pane)
  (let* ((class (game-resource-character-course-class resource))
         (dropdown (make-prototype-dropdown resource class))
         (current-proto (game-resource-character-course-prototype resource)))
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Course Prototype: "))
        (clim:formatting-cell (pane :align-x :left)
          (clim:make-pane 'clim:radio-box-pane
                          :orientation :vertical
                          :items `(("Zeroes" . 0)
                                   (,(clim:horizontally ()
                                       (clim:make-pane 'clim:label-pane
                                                       :label (format nil "Object-Prototypes of class ~a:" class))
                                       dropdown)
                                    . t))
                          :current-value (if (eq current-proto 0) 0 t)
                          :callback
                          (lambda (radio value)
                            (declare (ignore radio))
                            (if (eq value 0)
                                (setf (game-resource-character-course-prototype resource) 0)
                                (setf (game-resource-character-course-prototype resource)
                                      (clim:gadget-value dropdown)))
                            (publish-resource-changed resource))))))))

(defun make-prototype-dropdown (resource class)
  (let ((prototypes (list-object-prototypes-for-class class))
        (current-proto (game-resource-character-course-prototype resource)))
    (clim:make-pane 'clim:option-pane
                    :items prototypes
                    :current-value current-proto
                    :value-changed-callback
                    (lambda (gadget value)
                      (declare (ignore gadget))
                      (setf (game-resource-character-course-prototype resource) value)
                      (publish-resource-changed resource)))))

(defun display-identity-editing-hit-points (resource pane)
  (clim:formatting-row (pane) (clim:formatting-cell (pane :align-x :left) (format pane " ")))
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :left)
      (clim:with-text-style (pane (clim:make-text-style :sans-serif :bold :larger))
        (format pane "Hit Points"))))
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "HP: "))
    (clim:formatting-cell (pane :align-x :left)
      (let* ((raw-hp (game-resource-character-hp resource))
             (hp-str (format nil "~d.~2,'0d" (ldb (byte 8 8) raw-hp) (ldb (byte 8 0) raw-hp)))
             (raw-max (game-resource-character-max-hp resource))
             (max-str (format nil "~d.~2,'0d" (ldb (byte 8 8) raw-max) (ldb (byte 8 0) raw-max))))
        (clim:horizontally ()
          (make-hp-field resource 'hp hp-str)
          (format pane " / ")
          (make-hp-field resource 'max-hp max-str)))
      (format pane " max"))))

(defun make-hp-field (resource slot value)
  (clim:make-pane 'clim:text-field-pane
                  :value value
                  :activation-callback
                  (lambda (gadget)
                    (setf (slot-value resource slot)
                          (parse-8.8-string (clim:gadget-value gadget)))
                    (publish-resource-changed resource))))

(defun display-identity-editing-faction (resource pane)
  (display-bitmask-section pane resource 'faction *faction-bits* "Faction"))

(defun display-identity-editing-flags (resource pane)
  (display-bitmask-section pane resource 'flags *character-flags-bits* "Character Flags"))
