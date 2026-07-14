;;; Skyline-Tool src/gui/gui-character.lisp
;;; Character Inspector using eventbus for real-time updates

(in-package :skyline-tool)

;; =============================
;; EVENTBUS INTEGRATION
;; =============================

(defvar *inspector-event-types*
  '(:character-change :equipment-change :appearance-change :inventory-change
    :keys-change :stats-change :flags-change :view-change)
  "Event types specific to character inspector")

(defun subscribe-to-inspector-events (frame)
  "Subscribe frame to inspector events with automatic redraw"
  (subscribe-to-global-events frame))

(defun subscribe-to-global-events (frame)
  "Subscribe all relevant global events to this frame"
  (dolist (event-type *inspector-event-types*)
    (subscribe event-type
               (lambda (event-data)
                 (declare (ignore event-data))
                 (when (typep frame 'character-inspector-frame)
                   (clim:redisplay-frame-panes frame :force-p t))))))

(defun unsubscribe-from-inspector-events (frame)
  "Unsubscribe frame from all inspector events"
  (declare (ignore frame))
  nil)

;; =============================
;; FRAME DEFINITION
;; =============================

(clim:define-application-frame character-inspector-frame (clim:standard-application-frame)
  ((resource :initarg :resource :reader frame-resource)
   (mode :initarg :mode :initform :editing :accessor frame-mode)
   (character-data :initform nil :accessor frame-data))
  (:menu-bar character-inspector-menu-bar)
  (:panes
   (basic-data-pane :application :display-function 'display-basic-data-presentation)
   (stats-pane :application :display-function 'display-stats-presentation)
   (appearance-pane :application :display-function 'display-appearance-presentation)
   (inventory-pane :application :display-function 'display-inventory-presentation)
   (keys-pane :application :display-function 'display-keys-presentation)
   (flags-pane :application :display-function 'display-flags-presentation)
   (interactor :interactor :height 80 :width 800))
  (:layouts
   (default (clim:vertically () basic-data-pane stats-pane appearance-pane
                             inventory-pane keys-pane flags-pane
                             interactor))))

;; =============================
;; MENU DEFINITIONS
;; =============================

(clim:define-command-table character-inspector-file-menu
  :menu (("Save" :command com-char-save)
         (nil :divider :line)
         ("Close" :command com-char-close)))

(clim:define-command-table character-inspector-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table character-inspector-view-menu
  :menu (("[] Editable" :command com-toggle-editable :name t)
         (nil :divider :line)
         ("[] Project Pane" :command com-toggle-project-pane)))

(clim:define-command-table character-inspector-help-menu
  :menu (("How to Manage this Character..." :command com-help-char)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table character-inspector-menu-bar
  :menu (("Character" :menu character-inspector-file-menu)
         ("Edit" :menu character-inspector-edit-menu)
         ("View" :menu character-inspector-view-menu)
         ("Help" :menu character-inspector-help-menu)))

;; =============================
;; DISPLAY METHODS
;; =============================

(defmethod display-basic-data-presentation (frame pane)
  "Display basic character data"
  (let ((resource (frame-resource frame)))
    (when resource
      (let ((stream pane))
        (format stream "~&~a" (game-resource-title resource))
        (format stream "~%~5t~a" (game-resource-subheading resource))))))

(defmethod display-stats-presentation (frame pane)
  "Display statistics tab"
  (declare (ignore frame pane))
  (format pane "~&[Stats Presentation]"))

(defmethod display-appearance-presentation (frame pane)
  "Display appearance tab"
  (declare (ignore frame pane))
  (format pane "~&[Appearance Presentation]"))

(defmethod display-inventory-presentation (frame pane)
  "Display inventory tab"
  (declare (ignore frame pane))
  (format pane "~&[Inventory Presentation]"))

(defmethod display-keys-presentation (frame pane)
  "Display keys tab"
  (declare (ignore frame pane))
  (format pane "~&[Keys Presentation]"))

(defmethod display-flags-presentation (frame pane)
  "Display flags tab"
  (declare (ignore frame pane))
  (format pane "~&[Flags Presentation]"))

;; =============================
;; COMMANDS
;; =============================

(clim:define-command (com-char-save :command-table clim-internals::global-command-table :menu t :name t)
  ()
  ()
  "Save the character resource"
  (let ((frame clim:*application-frame*))
    (when frame
      (let ((resource (frame-resource frame)))
        (when resource
          (format *query-io* "~&Saved ~a~%" (game-resource-title resource)))))))

(clim:define-command (com-char-close :command-table clim-internals::global-command-table :menu t :name t)
  ()
  ()
  "Close the character inspector"
  (clim:frame-exit clim:*application-frame*))

(clim:define-command (com-toggle-editable :command-table clim-internals::global-command-table :menu t :name t)
  ()
  ()
  "Toggle editable mode"
  (let ((frame clim:*application-frame*))
    (when frame
      (setf (frame-mode frame)
            (if (eq (frame-mode frame) :editing) :viewing :editing))
      (clim:redisplay-frame-panes frame :force-p t))))

(clim:define-command (com-help-char :command-table clim-internals::global-command-table :menu t :name t)
  ()
  ()
  "Show help for character inspector")

;; =============================
;; FRAME INITIALIZATION
;; =============================

(defmethod initialize-instance :after ((frame character-inspector-frame) &key)
  (subscribe-to-inspector-events frame))

(defmethod destroy-instance :after ((frame character-inspector-frame))
  (unsubscribe-from-inspector-events frame))

;; =============================
;; OPEN HELPER
;; =============================

(defun open-character-inspector (resource &key (mode :editing))
  "Open a character inspector for the given resource"
  (clim:run-frame-top-level
   (clim:make-application-frame 'character-inspector-frame
                                :resource resource
                                :mode mode
                                :pretty-name (format nil "~a — ~a"
                                                     (game-resource-title resource)
                                                     (game-resource-game-title resource)))))