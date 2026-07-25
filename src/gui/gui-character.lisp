;;; Skyline-Tool src/gui/gui-character.lisp
;;; Character Inspector with full CLIM integration

(in-package :skyline-tool)

;; Per-resource save-as menu for Character Inspector
(clim:define-command-table character-save-as-menu
  :menu (("Text..." :command com-character-save-text)
         ("JSON..." :command com-character-save-json)
         ("PDF..." :command com-character-save-pdf)))

(clim:define-command-table character-file-menu
  :menu (("New..." :command com-character-new)
         ("Import..." :command com-character-import)
         (nil :divider :line)
         ("Save" :command com-character-save)
         ("Version" :menu inspector-version-control-menu)
         ("Save as" :menu character-save-as-menu)
         (nil :divider :line)
         ("Print to" :menu inspector-print-to-menu)
         ("Send to" :menu inspector-send-to-menu)
         (nil :divider :line)
         ("Close" :command com-close-frame)))

(clim:define-command-table character-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table character-view-menu
  :menu (("☐ Editable" :command com-inspector-toggle-view :name t)
         (nil :divider :line)
         ("☑ Project Pane" :command com-toggle-project-pane :name t)))

(clim:define-command-table character-run-menu
  :menu (("Build" :menu inspector-build-menu)
         ("Region" :menu inspector-region-menu)
         (nil :divider :line)
         ("Export to AtariVox..." :command com-character-export-avox)))

(clim:define-command-table character-help-menu
  :menu (("How to Manage Characters..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table character-menu-bar
  :menu (("Character" :menu character-file-menu)
         ("Edit" :menu character-edit-menu)
         ("Run" :menu character-run-menu)
         ("View" :menu character-view-menu)
         ("Help" :menu character-help-menu)))

;; Tabbed interface for Character Inspector
(clim:define-application-frame character-inspector-frame (gui-inspector-frame clim:standard-application-frame)
  ((resource :initarg :resource :reader frame-resource)
   (current-tab :initarg :current-tab :accessor frame-current-tab :initform :basic-data))
  (:menu-bar character-menu-bar)
  (:icon (skyline-tool-icon :resource :character))
  (:pretty-name "Character Inspector"))

(defmethod initialize-instance :after ((frame character-inspector-frame) &key)
  (call-next-method)
  (subscribe :resource-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

;; Tab switching commands
(clim:define-command (com-switch-to-basic-data :command-table clim-internals::global-command-table :menu t :name t)
  ()
  (let ((frame clim:*application-frame*))
    (when frame
      (setf (frame-current-tab frame) :basic-data)
      (clim:redisplay-frame-panes frame :force-p t))))

(clim:define-command (com-switch-to-stats :command-table clim-internals::global-command-table :menu t :name t)
  ()
  (let ((frame clim:*application-frame*))
    (when frame
      (setf (frame-current-tab frame) :stats)
      (clim:redisplay-frame-panes frame :force-p t))))

(clim:define-command (com-switch-to-appearance :command-table clim-internals::global-command-table :menu t :name t)
  ()
  (let ((frame clim:*application-frame*))
    (when frame
      (setf (frame-current-tab frame) :appearance)
      (clim:redisplay-frame-panes frame :force-p t))))

(clim:define-command (com-switch-to-inventory :command-table clim-internals::global-command-table :menu t :name t)
  ()
  (let ((frame clim:*application-frame*))
    (when frame
      (setf (frame-current-tab frame) :inventory)
      (clim:redisplay-frame-panes frame :force-p t))))

(clim:define-command (com-switch-to-keys :command-table clim-internals::global-command-table :menu t :name t)
  ()
  (let ((frame clim:*application-frame*))
    (when frame
      (setf (frame-current-tab frame) :keys)
      (clim:redisplay-frame-panes frame :force-p t))))

(clim:define-command (com-switch-to-flags :command-table clim-internals::global-command-table :menu t :name t)
  ()
  (let ((frame clim:*application-frame*))
    (when frame
      (setf (frame-current-tab frame) :flags)
      (clim:redisplay-frame-panes frame :force-p t))))

(defun open-character-inspector (resource)
  (open-resource-inspector (or resource
                               (make-instance 'game-resource-character
                                              :name "New Character"
                                              :character-id 0
                                              :decal ""
                                              :gender :male
                                              :hp 10
                                              :max-hp 10
                                              :ac 0
                                              :hair-color 0
                                              :skin-color 0
                                              :clothes-color 0
                                              :head ""
                                              :body ""
                                              :speech-pitch 7
                                              :speech-speed 5
                                              :speech-bend 0
                                              :speech-color :white
                                              :nicks ""
                                              :memo ""
                                              :equipment ""
                                              :shield ""
                                              :crowns 0
                                              :arrows 0
                                              :potions 0
                                              :chalice nil)) :editing))

(defmethod open-resource-inspector ((resource game-resource-character) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'character-inspector-frame
                                :resource resource
                                :view-mode mode
                                :pretty-name (format nil "~a — ~a ~a"
                                                     (game-resource-title resource)
                                                     *game-title* (machine-directory-name)))))

;; Tab display function
(defun display-current-tab (frame pane)
  "Display the current tab based on frame-current-tab slot."
  (let* ((resource (frame-resource frame))
         (tab (frame-current-tab frame))
         (*standard-output* pane))
    (when resource
      (ecase tab
        (:basic-data    (display-basic-data-tab frame pane))
        (:stats         (display-stats-tab frame pane))
        (:appearance    (display-appearance-tab frame pane))
        (:inventory     (display-inventory-tab frame pane))
        (:keys          (display-keys-tab frame pane))
        (:flags         (display-flags-tab frame pane))))))

;; Tab content display methods - READING mode (read-only)
(defmethod display-basic-data-tab (frame pane)
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
        (clim:formatting-cell (pane :align-x :right) (format pane "Memo: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-memo resource)))))))

(defmethod display-stats-tab (frame pane)
  (let ((resource (frame-resource frame)))
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "HP: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~d / ~d" (game-resource-character-hp resource) (game-resource-character-max-hp resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "AC: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~d" (game-resource-character-ac resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Crowns: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~d" (game-resource-character-crowns resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Arrows: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~d" (game-resource-character-arrows resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Potions: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~d" (game-resource-character-potions resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Chalice: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-chalice resource)))))))

(defmethod display-appearance-tab (frame pane)
  (let ((resource (frame-resource frame)))
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Hair Color: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-hair-color resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Skin Color: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-skin-color resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Clothes Color: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-clothes-color resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Head: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-head resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Body: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-body resource)))))))

(defmethod display-inventory-tab (frame pane)
  (let ((resource (frame-resource frame)))
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Equipment: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-equipment resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Shield: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-shield resource)))))))

(defmethod display-keys-tab (frame pane)
  (let ((resource (frame-resource frame)))
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Nicks: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-nicks resource)))))))

(defmethod display-flags-tab (frame pane)
  "Display Flags tab for character inspector.
   Note: Flag management is handled through the Eightbol OOP system and will be
   implemented in a future release."
  (let ((resource (frame-resource frame)))
    (clim:formatting-table (pane)
      (error "unimplemented"))))

;; Presentation types
(clim:define-presentation-type game-resource-character-reference ()
  :inherit-from 'game-resource-reference)

(clim:define-presentation-type game-resource-character-editable ()
  :inherit-from 'game-resource-editable)

(clim:define-presentation-type game-resource-character-viewing ()
  :inherit-from 'game-resource-viewing)

(defmethod present-reference ((resource game-resource-character) stream)
  (clim:with-output-as-presentation (stream resource 'game-resource-character-reference)
    (clim:formatting-table (stream)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 0)
          (format stream "~3%"))
        (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 125)
          (game-resource-present-icon resource stream))
        (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 150)
          (clim:with-text-size (stream :larger)
            (clim:with-text-face (stream :bold)
              (game-resource-present-title resource stream)))
          (format stream "~%~5t")
          (clim:with-text-size (stream :smaller)
            (clim:with-drawing-options (stream :ink (clim:make-gray-color 0.75))
              (game-resource-present-subheading resource stream))))
        (clim:formatting-cell (stream :align-x :right :align-y :top :min-height 90 :min-width 125)
          (game-resource-present-right-margin resource stream))))))

(defmethod present-reading ((resource game-resource-character) stream)
  (clim:present resource 'game-resource-character-viewing :stream stream))

(defmethod present-editing ((resource game-resource-character) stream)
  (clim:present resource 'game-resource-character-editable :stream stream))

(defmethod clim:present (resource (type (eql 'game-resource-character-viewing)) stream &key)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-name resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "ID: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~d" (game-resource-character-id resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Decal: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-decal resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Gender: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-gender resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "HP: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~d / ~d" (game-resource-character-hp resource) (game-resource-character-max-hp resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "AC: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~d" (game-resource-character-ac resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Crowns: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~d" (game-resource-character-crowns resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Arrows: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~d" (game-resource-character-arrows resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Potions: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~d" (game-resource-character-potions resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Chalice: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-chalice resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Hair Color: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-hair-color resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Skin Color: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-skin-color resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Clothes Color: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-clothes-color resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Head: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-head resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Body: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-body resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Equipment: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-equipment resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Shield: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-shield resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Speech Pitch: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-speech-pitch resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Speech Speed: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-speech-speed resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Speech Bend: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-speech-bend resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Speech Color: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-speech-color resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Nicks: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-nicks resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Memo: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-memo resource))))))

(defmethod clim:present (resource (type (eql 'game-resource-character-editable)) stream &key)
  (let ((frame clim:*application-frame*))
    (display-current-tab frame stream)))
(let* ((name (game-resource-character-name resource))
       (char-id (game-resource-character-id resource))
       (decal (game-resource-character-decal resource))
       (gender (game-resource-character-gender resource))
       (hp (game-resource-character-hp resource))
       (max-hp (game-resource-character-max-hp resource))
       (ac (game-resource-character-ac resource))
       (crowns (game-resource-character-crowns resource))
       (arrows (game-resource-character-arrows resource))
       (potions (game-resource-character-potions resource))
       (chalice (game-resource-character-chalice resource))
       (hair-color (game-resource-character-hair-color resource))
       (skin-color (game-resource-character-skin-color resource))
       (clothes-color (game-resource-character-clothes-color resource))
       (head (game-resource-character-head resource))
       (body (game-resource-character-body resource))
       (equipment (game-resource-character-equipment resource))
       (shield (game-resource-character-shield resource))
       (speech-pitch (game-resource-character-speech-pitch resource))
       (speech-speed (game-resource-character-speech-speed resource))
       (speech-bend (game-resource-character-speech-bend resource))
       (speech-color (game-resource-character-speech-color resource))
       (nicks (game-resource-character-nicks resource))
       (memo (game-resource-character-memo resource)))
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
        (clim:make-pane 'clim:text-field-pane
                        :value name
                        :activation-callback
                        (lambda (pane)
                          (let ((new-value (clim:gadget-value pane)))
                            (when (validate-minifont-name new-value 12)
                              (setf (game-resource-character-name resource) new-value)
                              (publish-resource-changed resource)))))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "ID: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~d (read-only)" char-id)))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Decal: "))
      (clim:formatting-cell (stream :align-x :left)
        (clim:make-pane 'clim:text-field-pane
                        :value decal
                        :activation-callback
                        (lambda (pane)
                          (setf (game-resource-character-decal resource) (clim:gadget-value pane))
                          (publish-resource-changed resource)))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Gender: "))
      (clim:formatting-cell (stream :align-x :left)
        (clim:make-pane 'clim:radio-box-pane
                        :items '(:male :female :other)
                        :current-value gender
                        :callback
                        (lambda (pane value)
                          (setf (game-resource-character-gender resource) value)
                          (publish-resource-changed resource)))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "HP: "))
      (clim:formatting-cell (stream :align-x :left)
        (clim:make-pane 'clim:slider-pane
                        :value hp
                        :min-value 0
                        :max-value max-hp
                        :callback
                        (lambda (pane value)
                          (setf (game-resource-character-hp resource) value)
                          (publish-resource-changed resource)))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Max HP: "))
      (clim:formatting-cell (stream :align-x :left)
        (clim:make-pane 'clim:slider-pane
                        :value max-hp
                        :min-value 1
                        :max-value 255
                        :callback
                        (lambda (pane value)
                          (setf (game-resource-character-max-hp resource) value)
                          (publish-resource-changed resource)))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "AC: "))
      (clim:formatting-cell (stream :align-x :left)
        (clim:make-pane 'clim:slider-pane
                        :value ac
                        :min-value -10
                        :max-value 20
                        :callback
                        (lambda (pane value)
                          (setf (game-resource-character-ac resource) value)
                          (publish-resource-changed resource)))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Crowns: "))
      (clim:formatting-cell (stream :align-x :left)
        (clim:make-pane 'clim:slider-pane
                        :value crowns
                        :min-value 0
                        :max-value 9999
                        :callback
                        (lambda (pane value)
                          (setf (game-resource-character-crowns resource) value)
                          (publish-resource-changed resource)))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Arrows: "))
      (clim:formatting-cell (stream :align-x :left)
        (clim:make-pane 'clim:slider-pane
                        :value arrows
                        :min-value 0
                        :max-value 255
                        :callback
                        (lambda (pane value)
                          (setf (game-resource-character-arrows resource) value)
                          (publish-resource-changed resource)))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Potions: "))
      (clim:formatting-cell (stream :align-x :left)
        (clim:make-pane 'clim:slider-pane
                        :value potions
                        :min-value 0
                        :max-value 99
                        :callback
                        (lambda (pane value)
                          (setf (game-resource-character-potions resource) value)
                          (publish-resource-changed resource)))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Chalice: "))
      (clim:formatting-cell (stream :align-x :left)
        (clim:make-pane 'clim:check-box-pane
                        :value chalice
                        :callback
                        (lambda (pane value)
                          (setf (game-resource-character-chalice resource) value)
                          (publish-resource-changed resource)))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Hair Color: "))
      (clim:formatting-cell (stream :align-x :left)
        (clim:make-pane 'clim:slider-pane
                        :value hair-color
                        :min-value 0
                        :max-value 15
                        :callback
                        (lambda (pane value)
                          (setf (game-resource-character-hair-color resource) value)
                          (publish-resource-changed resource)))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Skin Color: "))
      (clim:formatting-cell (stream :align-x :left)
        (clim:make-pane 'clim:slider-pane
                        :value skin-color
                        :min-value 0
                        :max-value 15
                        :callback
                        (lambda (pane value)
                          (setf (game-resource-character-skin-color resource) value)
                          (publish-resource-changed resource)))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Clothes Color: "))
      (clim:formatting-cell (stream :align-x :left)
        (clim:make-pane 'clim:slider-pane
                        :value clothes-color
                        :min-value 0
                        :max-value 15
                        :callback
                        (lambda (pane value)
                          (setf (game-resource-character-clothes-color resource) value)
                          (publish-resource-changed resource)))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Head: "))
      (clim:formatting-cell (stream :align-x :left)
        (clim:make-pane 'clim:text-field-pane
                        :value head
                        :activation-callback
                        (lambda (pane)
                          (setf (game-resource-character-head resource) (clim:gadget-value pane))
                          (publish-resource-changed resource)))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Body: "))
      (clim:formatting-cell (stream :align-x :left)
        (clim:make-pane 'clim:text-field-pane
                        :value body
                        :activation-callback
                        (lambda (pane)
                          (setf (game-resource-character-body resource) (clim:gadget-value pane))
                          (publish-resource-changed resource)))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Equipment: "))
      (clim:formatting-cell (stream :align-x :left)
        (clim:make-pane 'clim:text-field-pane
                        :value equipment
                        :activation-callback
                        (lambda (pane)
                          (setf (game-resource-character-equipment resource) (clim:gadget-value pane))
                          (publish-resource-changed resource)))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Shield: "))
      (clim:formatting-cell (stream :align-x :left)
        (clim:make-pane 'clim:text-field-pane
                        :value shield
                        :activation-callback
                        (lambda (pane)
                          (setf (game-resource-character-shield resource) (clim:gadget-value pane))
                          (publish-resource-changed resource)))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Speech Pitch: "))
      (clim:formatting-cell (stream :align-x :left)
        (clim:make-pane 'clim:slider-pane
                        :value speech-pitch
                        :min-value 0
                        :max-value 15
                        :callback
                        (lambda (pane value)
                          (setf (game-resource-character-speech-pitch resource) value)
                          (publish-resource-changed resource)))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Speech Speed: "))
      (clim:formatting-cell (stream :align-x :left)
        (clim:make-pane 'clim:slider-pane
                        :value speech-speed
                        :min-value 0
                        :max-value 10
                        :callback
                        (lambda (pane value)
                          (setf (game-resource-character-speech-speed resource) value)
                          (publish-resource-changed resource)))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Speech Bend: "))
      (clim:formatting-cell (stream :align-x :left)
        (clim:make-pane 'clim:slider-pane
                        :value speech-bend
                        :min-value -2
                        :max-value 2
                        :callback
                        (lambda (pane value)
                          (setf (game-resource-character-speech-bend resource) value)
                          (publish-resource-changed resource)))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Speech Color: "))
      (clim:formatting-cell (stream :align-x :left)
        (clim:make-pane 'clim:radio-box-pane
                        :items '(:white :red :green :blue :yellow :cyan :magenta)
                        :current-value speech-color
                        :callback
                        (lambda (pane value)
                          (setf (game-resource-character-speech-color resource) value)
                          (publish-resource-changed resource)))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Nicks: "))
      (clim:formatting-cell (stream :align-x :left)
        (clim:make-pane 'clim:text-field-pane
                        :value nicks
                        :activation-callback
                        (lambda (pane)
                          (setf (game-resource-character-nicks resource) (clim:gadget-value pane))
                          (publish-resource-changed resource)))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Memo: "))
      (clim:formatting-cell (stream :align-x :left)
        (clim:make-pane 'clim:text-field-pane
                        :value memo
                        :activation-callback
                        (lambda (pane)
                          (setf (game-resource-character-memo resource) (clim:gadget-value pane))
                          (publish-resource-changed resource)))))))

(defun character-to-plist (resource)
  "Convert character resource to plist for JSON export."
  (list :name (game-resource-character-name resource)
        :id (game-resource-character-id resource)
        :decal (game-resource-character-decal resource)
        :gender (game-resource-character-gender resource)
        :hp (game-resource-character-hp resource)
        :max-hp (game-resource-character-max-hp resource)
        :ac (game-resource-character-ac resource)
        :crowns (game-resource-character-crowns resource)
        :arrows (game-resource-character-arrows resource)
        :potions (game-resource-character-potions resource)
        :chalice (game-resource-character-chalice resource)
        :hair-color (game-resource-character-hair-color resource)
        :skin-color (game-resource-character-skin-color resource)
        :clothes-color (game-resource-character-clothes-color resource)
        :head (game-resource-character-head resource)
        :body (game-resource-character-body resource)
        :equipment (game-resource-character-equipment resource)
        :shield (game-resource-character-shield resource)
        :speech-pitch (game-resource-character-speech-pitch resource)
        :speech-speed (game-resource-character-speech-speed resource)
        :speech-bend (game-resource-character-speech-bend resource)
        :speech-color (game-resource-character-speech-color resource)
        :nicks (game-resource-character-nicks resource)
        :memo (game-resource-character-memo resource)))

(defun character-to-text (resource)
  "Convert character resource to plain text."
  (with-output-to-string (s)
    (format s "~&Character: ~a~%" (game-resource-character-name resource))
    (format s "ID: ~d~%" (game-resource-character-id resource))
    (format s "Decal: ~a~%" (game-resource-character-decal resource))
    (format s "Gender: ~a~%" (game-resource-character-gender resource))
    (format s "HP: ~d / ~d~%" (game-resource-character-hp resource) (game-resource-character-max-hp resource))
    (format s "AC: ~d~%" (game-resource-character-ac resource))
    (format s "Crowns: ~d~%" (game-resource-character-crowns resource))
    (format s "Arrows: ~d~%" (game-resource-character-arrows resource))
    (format s "Potions: ~d~%" (game-resource-character-potions resource))
    (format s "Chalice: ~a~%" (game-resource-character-chalice resource))
    (format s "Hair Color: ~a~%" (game-resource-character-hair-color resource))
    (format s "Skin Color: ~a~%" (game-resource-character-skin-color resource))
    (format s "Clothes Color: ~a~%" (game-resource-character-clothes-color resource))
    (format s "Head: ~a~%" (game-resource-character-head resource))
    (format s "Body: ~a~%" (game-resource-character-body resource))
    (format s "Equipment: ~a~%" (game-resource-character-equipment resource))
    (format s "Shield: ~a~%" (game-resource-character-shield resource))
    (format s "Speech Pitch: ~a~%" (game-resource-character-speech-pitch resource))
    (format s "Speech Speed: ~a~%" (game-resource-character-speech-speed resource))
    (format s "Speech Bend: ~a~%" (game-resource-character-speech-bend resource))
    (format s "Speech Color: ~a~%" (game-resource-character-speech-color resource))
    (format s "Nicks: ~a~%" (game-resource-character-nicks resource))
    (format s "Memo: ~a~%" (game-resource-character-memo resource))))

(defun character-to-postscript (resource)
  "Convert character resource to PostScript."
  (with-output-to-string (s)
    (format s "%%!PS-Adobe-3.0~%")
    (format s "%%Title: ~a~%" (game-resource-character-name resource))
    (format s "%%Creator: Skyline-Tool~%")
    (format s "%%Pages: 1~%")
    (format s "%%EndComments~%")
    (format s "/Helvetica findfont 12 scalefont setfont~%")
    (format s "72 720 moveto~%")
    (format s "(Character: ~a) show~%" (game-resource-character-name resource))
    (format s "72 700 moveto~%")
    (format s "(ID: ~d) show~%" (game-resource-character-id resource))
    (format s "72 680 moveto~%")
    (format s "(HP: ~d / ~d) show~%" (game-resource-character-hp resource) (game-resource-character-max-hp resource))
    (format s "72 660 moveto~%")
    (format s "(AC: ~d) show~%" (game-resource-character-ac resource))
    (format s "72 640 moveto~%")
    (format s "(Crowns: ~d) show~%" (game-resource-character-crowns resource))
    (format s "72 620 moveto~%")
    (format s "(Arrows: ~d) show~%" (game-resource-character-arrows resource))
    (format s "72 600 moveto~%")
    (format s "(Potions: ~d) show~%" (game-resource-character-potions resource))
    (format s "72 580 moveto~%")
    (format s "(Chalice: ~a) show~%" (game-resource-character-chalice resource))
    (format s "72 560 moveto~%")
    (format s "(Equipment: ~a) show~%" (game-resource-character-equipment resource))
    (format s "72 540 moveto~%")
    (format s "(Shield: ~a) show~%" (game-resource-character-shield resource))
    (format s "72 520 moveto~%")
    (format s "(Speech Pitch: ~a) show~%" (game-resource-character-speech-pitch resource))
    (format s "72 500 moveto~%")
    (format s "(Speech Speed: ~a) show~%" (game-resource-character-speech-speed resource))
    (format s "72 480 moveto~%")
    (format s "(Speech Bend: ~a) show~%" (game-resource-character-speech-bend resource))
    (format s "72 460 moveto~%")
    (format s "(Speech Color: ~a) show~%" (game-resource-character-speech-color resource))
    (format s "showpage~%")))

;; Save commands with atomic replacement
(defun write-character-atomically (resource)
  "Write character resource atomically using temp file + rename."
  (let ((path (first (game-resource-pathnames resource))))
    (when path
      (uiop/stream:with-temporary-file (:stream temp-stream :pathname temp-path
                                        :direction :output)
        (write-resource-to-stream resource temp-stream)
        (uiop:rename-file-overwriting-target temp-path path))
      (publish :resource-changed :payload resource)
      path)))

(defun write-resource-to-stream (resource stream)
  "Write character resource data to stream in ODS-compatible format."
  ;; This would write to the ODS format used by NPCStats.ods
  ;; For now, emit a simple text representation
  (format stream "~a" (character-to-text resource)))

(clim:define-command (com-character-save :command-table clim-internals::global-command-table :menu t :name t)
    ()
  ()
  "Save the character resource"
  (let ((frame clim:*application-frame*))
    (when frame
      (let ((resource (frame-resource frame)))
        (when resource
          (write-character-atomically resource)
          (clim-simple-echo:run-in-simple-echo
           (lambda () (format t "~&Saved ~a~%" (game-resource-title resource)))))))))

(clim:define-command (com-character-save-text :command-table clim-internals::global-command-table :menu t :name t)
  ()
  ()
  "Export character as plain text"
  (let* ((frame clim:*application-frame*)
         (resource (frame-resource frame))
         (path (prompt-save-pathname (format nil "~a.txt" (game-resource-title resource))
                                     :type "txt")))
    (when path
      (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8)
        (format s "~a" (character-to-text resource)))
      (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Exported character as text to ~a~%" path))))))

(clim:define-command (com-character-save-json :command-table clim-internals::global-command-table :menu t :name t)
    ()
  ()
  "Export character as JSON"
  (let* ((frame clim:*application-frame*)
         (resource (frame-resource frame))
         (path (prompt-save-pathname (format nil "~a.json" (game-resource-title resource))
                                     :type "json")))
    (when path
      (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8)
        (json:encode-json (character-to-plist resource) s))
      (clim-simple-echo:run-in-simple-echo
       (lambda () (format t "~&Exported character as JSON to ~a~%" path))))))

(clim:define-command (com-character-save-pdf :command-table clim-internals::global-command-table :menu t :name t)
    ()
  ()
  "Export character as PDF via PostScript"
  (let* ((frame clim:*application-frame*)
         (resource (frame-resource frame))
         (ps-path (prompt-save-pathname (format nil "~a.ps" (game-resource-title resource))
                                        :type "ps")))
    (when ps-path
      (with-open-file (s ps-path :direction :output :if-exists :supersede :external-format :utf-8)
        (format s "~a" (character-to-postscript resource)))
      (let ((pdf-path (make-pathname :type "pdf" :defaults ps-path)))
        (ignore-errors
         (uiop:run-program (list "ps2pdf" (namestring ps-path) (namestring pdf-path)) :output nil)))
      (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Exported character as PDF to ~a~%" pdf-path))))))

;; AtariVox integration


(defun parse-usb-serial-port (line)
  "Parse a lsusb line for serial port path."
  (let ((pos (search "/dev/" line)))
    (when pos
      (let ((end (or (position #\Space line :start pos) (length line))))
        (subseq line pos end)))))

(defun encode-speech-for-avox (text pitch speed bend color)
  "Encode text string to AtariVox phoneme bytes with voice parameter prefix.
   Uses the same phonetic dictionary as Fountain scripting."
  (let* ((phonemes (fountain-phonetize text))
         (avox-bytes (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer t))
         (color-code (ecase color
                       (:white 0) (:red 1) (:green 2) (:blue 3)
                       (:yellow 4) (:cyan 5) (:magenta 6))))
    ;; Prefix: reset (0x00), set pitch (0x10), pitch value, set speed (0x11), speed value
    ;; set bend (0x12), bend value (signed byte + 128), set color (0x13), color value
    (vector-push-extend #x00 avox-bytes)  ; Reset
    (vector-push-extend #x10 avox-bytes)  ; Set pitch
    (vector-push-extend (mod pitch 16) avox-bytes)
    (vector-push-extend #x11 avox-bytes)  ; Set speed
    (vector-push-extend (mod speed 16) avox-bytes)
    (vector-push-extend #x12 avox-bytes)  ; Set bend
    (vector-push-extend (mod (+ bend 128) 256) avox-bytes)
    (vector-push-extend #x13 avox-bytes)  ; Set color
    (vector-push-extend color-code avox-bytes)
    ;; Phoneme data
    (dolist (phoneme phonemes)
      (let ((byte (avox-phoneme-to-byte phoneme)))
        (when byte
          (vector-push-extend byte avox-bytes))))
    avox-bytes))

(defun send-to-atari-vox (port byte-array)
  "Send byte array to AtariVox on specified serial port."
  (handler-case
      (uiop:with-temporary-file (:stream s :pathname p :direction :output :element-type '(unsigned-byte 8))
        (write-sequence byte-array s)
        (finish-output s)
        (uiop:run-program (list "cat" p ">" port) :input nil :output nil :error-output nil))
    (error (e)
      (format *error-output* "~&Failed to send to AtariVox on ~a: ~a~%" port e)
      nil)))

(defun read-speech-text ()
  "Read speech text from a CLIM text input pane."
  (error "unimplemented"))

(defun choose-serial-port (ports)
  "Present available serial ports via CLIM input gadget and return selected port."
  (let ((selected (first ports)))
    (dolist (p ports)
      (format t "  ~a~%" p))
    (format t "Select port: ")
    (let ((choice (read)))
      (when (find choice ports :test 'equal)
        (setf selected choice)))
    selected))

(clim:define-command (com-character-export-avox :command-table clim-internals::global-command-table :menu t :name t)
    ()
  ()
  "Export current speech to AtariVox via serial port"
  (let* ((resource (frame-resource clim:*application-frame*))
         (ports (available-serial-ports))
         (byte-array (when (and resource ports)
                       (let* ((port (choose-serial-port ports))
                              (text (read-speech-text))
                              (pitch (game-resource-character-speech-pitch resource))
                              (speed (game-resource-character-speech-speed resource))
                              (bend (game-resource-character-speech-bend resource))
                              (color (game-resource-character-speech-color resource))
                              (bytes (encode-speech-for-avox text pitch speed bend color)))
                         (when (and port text)
                           (when (send-to-atari-vox port bytes)
                             (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Sent ~d bytes to AtariVox on ~a~%" (length bytes) port)))))))))))

(clim:define-command (com-character-new :command-table clim-internals::global-command-table :menu t :name t)
  ()
  (open-character-inspector nil))

;; Import command (stub)
(clim:define-command (com-character-import :command-table clim-internals::global-command-table :menu t :name t)
  ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Import functionality not yet implemented~%"))))

(defmethod game-resource-action-menu ((resource game-resource-character))
  (list (make-menu-item "Inspect..." (lambda () (open-character-inspector resource)))))

(defun validate-minifont-name (name max-bytes)
  "Validate that NAME encodes to MAX-BYTES or fewer in minifont.
   Signals an error if the name is too long."
  (let* ((encoded (minifont-encode name))
         (len (length encoded)))
    (if (<= len max-bytes)
        t
        (error 'minifont-name-too-long
               :name name
               :encoded-length len
               :max-length max-bytes))))

(define-condition minifont-name-too-long (error)
  ((name :initarg :name :reader error-name)
   (encoded-length :initarg :encoded-length :reader error-encoded-length)
   (max-length :initarg :max-length :reader error-max-length))
  (:report (lambda (condition stream)
             (format stream "Name ~a encodes to ~d bytes (max ~d)"
                     (error-name condition)
                     (error-encoded-length condition)
                     (error-max-length condition)))
   (:documentation "Signalled when a minifont name exceeds the byte limit.")))
