;;; Skyline-Tool src/gui/gui-character-identity.lisp
;;; Identity tab for Character Inspector

(in-package :skyline-tool)

;; Identity tab — dispatch read-only vs editing by frame-view-mode
(defmethod display-identity-tab ((frame character-inspector-frame) pane)
  (ecase (frame-view-mode frame)
    (:reference (display-identity-reference frame pane))
    (:editing   (display-identity-editing frame pane))))

;; Identity tab - read-only display
(defmethod display-identity-reference ((frame character-inspector-frame) pane)
  (let ((resource (frame-resource frame)))
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Name: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-name resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "ID: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~d" (game-resource-character-id resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Decal: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-decal resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Gender: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-gender resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Home: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (or (game-resource-character-home resource) "None"))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Comments: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-memo resource))))
      ;; ── Movement section ──
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :left)
          (clim:with-text-style (pane (clim:make-text-style :sans-serif :bold nil))
            (format pane "Movement"))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Course Class: "))
        (clim:formatting-cell (pane :align-x :left)
          (format pane "~a" (or (game-resource-character-course-class resource) "Course"))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Course Prototype: "))
        (clim:formatting-cell (pane :align-x :left)
          (format pane "~a" (let ((p (game-resource-character-course-prototype resource)))
                             (if (eq p 0) "Zeroes" (or p "Zeroes"))))))
      ;; ── Hit Points section ──
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :left)
          (clim:with-text-style (pane (clim:make-text-style :sans-serif :bold nil))
            (format pane "Hit Points"))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "HP: "))
        (clim:formatting-cell (pane :align-x :left)
          (let ((hp (game-resource-character-hp resource))
                (max (game-resource-character-max-hp resource)))
            (format pane "~d.~2,'0d / ~d.~2,'0d max"
                    (ldb (byte 8 8) hp) (ldb (byte 8 0) hp)
                    (ldb (byte 8 8) max) (ldb (byte 8 0) max))))))))

;; Identity tab - editable display
(defmethod display-identity-editing ((frame character-inspector-frame) pane)
  (let ((resource (frame-resource frame)))
    (clim:formatting-table (pane :multiple-columns t)
      ;; Name field with immediate validation
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Name: "))
        (clim:formatting-cell (pane :align-x :left)
          (let ((name (game-resource-character-name resource)))
            (clim:make-pane 'clim:text-field-pane
                            :value name
                            :value-changed-callback
                            (lambda (gadget value)
                              (declare (ignore gadget))
                              (setf (game-resource-character-name resource) value)
                              (publish-resource-changed resource))))))
      ;; ID field (read-only)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Character ID: "))
        (clim:formatting-cell (pane :align-x :left)
          (format pane "$~2,'0x" (game-resource-character-id resource))))
      ;; Decal field
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Decal Kind: "))
        (clim:formatting-cell (pane :align-x :left)
          (let ((decal (game-resource-character-decal resource)))
            (clim:make-pane 'clim:text-field-pane
                            :value decal
                            :activation-callback
                            (lambda (pane)
                              (setf (game-resource-character-decal resource) (clim:gadget-value pane))
                              (publish-resource-changed resource))))))
      ;; Gender selection
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Gender: "))
        (clim:formatting-cell (pane :align-x :left)
          (let ((gender (game-resource-character-gender resource)))
            (clim:make-pane 'clim:radio-box-pane
                            :items '(("Masculine (he)" . :male)
                                     ("Feminine (she)" . :female)
                                     ("Indefinite (they)" . :indefinite)
                                     ("Impersonal (it)" . :impersonal))
                            :current-value gender
                            :callback
                            (lambda (pane value)
                              (declare (ignore pane))
                              (setf (game-resource-character-gender resource) value)
                              (publish-resource-changed resource))))))
      ;; Home field (free text, Unicode allowed)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Home: "))
        (clim:formatting-cell (pane :align-x :left)
          (let ((home (game-resource-character-home resource)))
            (clim:make-pane 'clim:text-field-pane
                            :value home
                            :activation-callback
                            (lambda (pane)
                              (setf (game-resource-character-home resource) (clim:gadget-value pane))
                              (publish-resource-changed resource))))))
      ;; ── Movement section ──────────────────────────────────
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :left)
          (clim:with-text-style (pane (clim:make-text-style :sans-serif :bold nil))
            (format pane "Movement"))))
      ;; Course Class dropdown (dynamically populated from OOPS class hierarchy)
      (let ((class (game-resource-character-course-class resource)))
        (clim:formatting-row (pane)
          (clim:formatting-cell (pane :align-x :right) (format pane "Course Class: "))
          (clim:formatting-cell (pane :align-x :left)
            (let ((descendants (class-descendants-flat "Course" *class-bases*)))
              (clim:make-pane 'clim:option-pane
                              :items (mapcar
                                      (lambda (entry)
                                        (let ((cls (first entry))
                                              (depth (second entry)))
                                          (let ((indent (make-string (* depth 2) :initial-element #\Space))
                                                (arrow (if (zerop depth) "" "↳ ")))
                                            (cons (concatenate 'string indent arrow
                                                               (title-case (string-upcase cls)))
                                                  cls))))
                                      descendants)
                              :current-value class
                              :callback
                              (lambda (pane value)
                                (setf (game-resource-character-course-class resource) value)
                                (let ((valid (list-object-prototypes-for-class value)))
                                  (unless (or (eq (game-resource-character-course-prototype resource) 0)
                                              (member (game-resource-character-course-prototype resource)
                                                      valid :test #'string=))
                                    (setf (game-resource-character-course-prototype resource) 0)))
                                (publish-resource-changed resource)))))))
      ;; Prototype: Zeroes radio or filtered Object-Prototypes dropdown
      (let* ((class (game-resource-character-course-class resource))
             (prototypes (list-object-prototypes-for-class class))
             (current-proto (game-resource-character-course-prototype resource))
             (dropdown (clim:make-pane 'clim:option-pane
                                       :items prototypes
                                       :current-value current-proto
                                       :value-changed-callback
                                       (lambda (gadget value)
                                         (declare (ignore gadget))
                                         (setf (game-resource-character-course-prototype resource) value)
                                         (publish-resource-changed resource)))))
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
                              (if (eq value 0)
                                  (setf (game-resource-character-course-prototype resource) 0)
                                  (setf (game-resource-character-course-prototype resource)
                                        (clim:gadget-value dropdown)))
                              (publish-resource-changed resource))))))
      ;; ── Hit Points section ──────────────────────────────────
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :left)
          (clim:with-text-style (pane (clim:make-text-style :sans-serif :bold nil))
            (format pane "Hit Points"))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "HP: "))
        (clim:formatting-cell (pane :align-x :left)
          (let* ((raw-hp (game-resource-character-hp resource))
                 (hp-int (ldb (byte 8 8) raw-hp))
                 (hp-frac (ldb (byte 8 0) raw-hp))
                 (raw-max (game-resource-character-max-hp resource))
                 (max-int (ldb (byte 8 8) raw-max))
                 (max-frac (ldb (byte 8 0) raw-max)))
            (clim:horizontally (pane)
              (clim:make-pane 'clim:text-field-pane
                              :value (format nil "~d.~2,'0d" hp-int hp-frac)
                              :activation-callback
                              (lambda (gadget)
                                (let* ((val (clim:gadget-value gadget))
                                       (dot (position #\. val))
                                       (int (parse-integer (subseq val 0 dot) :junk-allowed t))
                                       (frac (if dot (parse-integer (subseq val (1+ dot)) :junk-allowed t) 0)))
                                  (setf (game-resource-character-hp resource)
                                        (logior (ash (or int 0) 8) (or frac 0)))
                                  (publish-resource-changed resource)))))
              (format pane " / ")
              (clim:make-pane 'clim:text-field-pane
                              :value (format nil "~d.~2,'0d" max-int max-frac)
                              :activation-callback
                              (lambda (gadget)
                                (let* ((val (clim:gadget-value gadget))
                                       (dot (position #\. val))
                                       (int (parse-integer (subseq val 0 dot) :junk-allowed t))
                                       (frac (if dot (parse-integer (subseq val (1+ dot)) :junk-allowed t) 0)))
                                  (setf (game-resource-character-max-hp resource)
                                        (logior (ash (or int 0) 8) (or frac 0)))
                                  (publish-resource-changed resource)))))
              (format pane " max"))))
      ;; Memo text area (multi-line, scrollable)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Memo: "))
        (clim:formatting-cell (pane :align-x :left)
          (let ((memo (game-resource-character-memo resource)))
            (clim:make-pane 'clim:text-field-pane
                            :value memo
                            :scroll-bars :vertical
                            :activation-callback
                            (lambda (pane)
                              (setf (game-resource-character-memo resource) (clim:gadget-value pane))
                              (publish-resource-changed resource)))))))))
