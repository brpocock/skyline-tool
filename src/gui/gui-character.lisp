;;; Skyline-Tool src/gui/gui-character.lisp
;;; Character Inspector with full CLIM integration

(in-package :skyline-tool)

;; 
;; EVENTBUS INTEGRATION
;; 

(defvar *inspector-event-types*
  '(:character-change :equipment-change :appearance-change :inventory-change
    :keys-change :stats-change :flags-change :view-change)
  "Event types specific to character inspector")

(defun subscribe-to-inspector-events (frame)
  "Subscribe frame to inspector events with automatic redraw"
  (dolist (event-type *inspector-event-types*)
    (subscribe event-type
               (lambda (event-data)
                 (declare (ignore event-data))
                 (when (and frame (typep frame 'character-inspector-frame))
                   (clim:redisplay-frame-panes frame :force-p t))))))

(defun unsubscribe-from-inspector-events (frame)
  "Unsubscribe frame from all inspector events"
  nil)

;; 
;; DATA MODEL
;; 

(defstruct character-data
  (stats (list :health 100 :strength 50 :magic 30 :defense 25
               :level 1 :experience 0))
  (inventory '())
  (appearance (list :clothing "Adventure Suit" :color-scheme "Default" :visual-effects "None"))
  (keys '((:move-up "W") (:move-down "S") (:move-left "A") (:move-right "D")
          (:attack "Space") (:interact "E")))
  (flags (list :active t :invulnerable nil :visible t :equipped t :alive t)))

;; 
;; FRAME DEFINITION
;; 

(clim:define-application-frame character-inspector-frame (gui-inspector-frame clim:standard-application-frame)
  ((resource :initarg :resource :reader frame-resource)
   (mode :initarg :mode :initform :editing :accessor frame-mode)
   (character-data :initarg :character-data :accessor frame-data))
  (:menu-bar character-inspector-menu-bar)
  (:icon (skyline-tool-icon :resource :character))
  (:panes
   (basic-data-pane :application :display-function 'display-basic-data-presentation
                    :scroll-bars nil)
   (stats-pane :application :display-function 'display-stats-presentation
               :scroll-bars t)
   (appearance-pane :application :display-function 'display-appearance-presentation
                    :scroll-bars t)
   (inventory-pane :application :display-function 'display-inventory-presentation
                   :scroll-bars t)
   (keys-pane :application :display-function 'display-keys-presentation
              :scroll-bars t)
   (flags-pane :application :display-function 'display-flags-presentation
               :scroll-bars t)
   (interactor :interactor :height 80 :width 800))
  (:layouts
   (default (clim:vertically () basic-data-pane stats-pane appearance-pane
                             inventory-pane keys-pane flags-pane
                             interactor))))

;; 
;; MENU DEFINITIONS
;; 

(clim:define-command-table character-inspector-save-as-menu
  :menu (("JSON..." :command com-char-save-json)
         ("Text..." :command com-char-save-text)
         ("PDF..." :command com-char-save-pdf)))

(clim:define-command-table character-inspector-file-menu
  :menu (("New..." :command com-char-new)
         ("Import..." :command com-char-import)
         (nil :divider :line)
         ("Save" :command com-char-save)
         ("Save as" :menu character-inspector-save-as-menu)
         (nil :divider :line)
         ("Print to" :menu inspector-print-to-menu)
         ("Send to" :menu inspector-send-to-menu)
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
         ("[] Project Pane" :command com-toggle-project-pane :name t)))

(clim:define-command-table character-inspector-run-menu
  :menu (("Build" :menu project-build-menu)
         ("Region" :menu project-region-menu)
         (nil :divider :line)
         ("Make" :menu resource-make-menu)
         (nil :divider :line)
         ("Build a Release Package..." :command com-build-release-package)))

(clim:define-command-table character-inspector-help-menu
  :menu (("How to Manage this Character..." :command com-help-char)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-scripting-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table character-inspector-menu-bar
  :menu (("Character" :menu character-inspector-file-menu)
         ("Edit" :menu character-inspector-edit-menu)
         ("Run" :menu character-inspector-run-menu)
         ("View" :menu character-inspector-view-menu)
         ("Help" :menu character-inspector-help-menu)))

;; 
;; DISPLAY METHODS
;; 

(defmethod display-basic-data-presentation (frame pane)
  "Display basic character data"
  (let ((resource (frame-resource frame)))
    (when resource
      (let ((*standard-output* pane))
        (format pane "~&~a" (game-resource-title resource))
        (format pane "~%~5t~a" (game-resource-subheading resource))))))

(defmethod display-stats-presentation (frame pane)
  "Display statistics tab"
  (let ((char-data (frame-data frame)))
    (when char-data
      (let ((*standard-output* pane))
        (format pane "~&=== STATS ===")
        (dolist (stat-key '(:health :strength :magic :defense :level :experience))
          (format pane "~%~a: ~a" stat-key (getf (character-data-stats char-data) stat-key)))))))

(defmethod display-appearance-presentation (frame pane)
  "Display appearance tab"
  (let ((char-data (frame-data frame)))
    (when char-data
      (let ((*standard-output* pane))
        (format pane "~&=== APPEARANCE ===")
        (dolist (key '(:clothing :color-scheme :visual-effects))
          (format pane "~%~a: ~a" key (getf (character-data-appearance char-data) key)))))))

(defmethod display-inventory-presentation (frame pane)
  "Display inventory tab"
  (let ((char-data (frame-data frame)))
    (when char-data
      (let ((*standard-output* pane))
        (format pane "~&=== INVENTORY ===")
        (dolist (item (character-data-inventory char-data))
          (format pane "~%~10a | ~5a | ~8a" 
                  (first item) (second item) (third item)))))))

(defmethod display-keys-presentation (frame pane)
  "Display keys tab"
  (let ((char-data (frame-data frame)))
    (when char-data
      (let ((*standard-output* pane))
        (format pane "~&=== KEYS ===")
        (dolist (binding (character-data-keys char-data))
          (format pane "~%~10a -> ~a" (string-upcase (symbol-name (first binding))) (second binding)))))))

(defmethod display-flags-presentation (frame pane)
  "Display flags tab"
  (let ((char-data (frame-data frame)))
    (when char-data
      (let ((*standard-output* pane))
        (format pane "~&=== FLAGS ===")
        (dolist (flag '(:active :invulnerable :visible :equipped :alive))
          (format pane "~%[~a]: ~a" flag (getf (character-data-flags char-data) flag)))))))

;; 
;; COMMANDS
;; 

(clim:define-command (com-char-save :command-table clim-internals::global-command-table :menu t :name t)
  ()
  ()
  "Save the character resource"
  (let ((frame clim:*application-frame*))
    (when frame
      (let ((resource (frame-resource frame))
            (char-data (frame-data frame)))
        (when resource
          (save-character-resource resource char-data)
          (format t "~&Saved ~a~%" (game-resource-title resource)))))))

(clim:define-command (com-char-save-json :command-table clim-internals::global-command-table :menu t :name t)
  ()
  ()
  "Save character as JSON"
  (let* ((frame clim:*application-frame*)
         (char-data (frame-data frame))
         (path (prompt-save-pathname "Save Character JSON" :defaults "character.json")))
    (when path
      (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8)
        (json:encode-json (character-data-to-plist char-data) s))
      (format t "~&Saved JSON to ~a~%" path))))

(clim:define-command (com-char-save-text :command-table clim-internals::global-command-table :menu t :name t)
  ()
  ()
  "Save character as plain text"
  (let* ((frame clim:*application-frame*)
         (char-data (frame-data frame))
         (path (prompt-save-pathname "Save Character Text" :defaults "character.txt")))
    (when path
      (with-open-file (s path :direction :output :if-exists :supersede)
        (let ((*standard-output* s))
          (display-character-data s char-data)))
      (format t "~&Saved text to ~a~%" path))))

(clim:define-command (com-char-save-pdf :command-table clim-internals::global-command-table :menu t :name t)
  ()
  ()
  "Save character as PDF"
  (let* ((frame clim:*application-frame*)
         (char-data (frame-data frame))
         (path (prompt-save-pathname "Save Character PDF" :defaults "character.ps")))
    (when path
      (with-open-file (s path :direction :output :if-exists :supersede)
        (format s "%!PS-Adobe-1.0~%%Title: Character~%~%")
        (display-character-data s char-data)
        (format s "~%showpage~%"))
      (let ((pdf-path (make-pathname :type "pdf" :defaults path)))
        (uiop:run-program (list "ps2pdf" (namestring path) (namestring pdf-path)) :output nil))
      (format t "~&Saved PDF to ~a~%" path))))

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
      (setf (frame-mode frame) (if (eq (frame-mode frame) :editing) :viewing :editing))
      (clim:redisplay-frame-panes frame :force-p t))))

(clim:define-command (com-help-char :command-table clim-internals::global-command-table :menu t :name t)
  ()
  ()
  "Show help for character inspector")

;; 
;; HELPERS
;; 

(defun display-character-data (stream char-data)
  "Display all character data to stream"
  (format stream "~&=== CHARACTER DATA ===")
  (let ((*standard-output* stream))
    (format stream "~&=== STATS ===")
    (dolist (stat-key '(:health :strength :magic :defense :level :experience))
      (format stream "~%~a: ~a" stat-key (getf (character-data-stats char-data) stat-key)))
    (format stream "~&~&=== INVENTORY ===")
    (dolist (item (character-data-inventory char-data))
      (format stream "~%~10a | ~5a | ~8a" (first item) (second item) (third item)))
    (format stream "~&~&=== APPEARANCE ===")
    (dolist (key '(:clothing :color-scheme :visual-effects))
      (format stream "~%~a: ~a" key (getf (character-data-appearance char-data) key)))
    (format stream "~&~&=== KEYS ===")
    (dolist (binding (character-data-keys char-data))
      (format stream "~%~10a -> ~a" (string-upcase (symbol-name (first binding))) (second binding)))
    (format stream "~&~&=== FLAGS ===")
    (dolist (flag '(:active :invulnerable :visible :equipped :alive))
      (format stream "~%[~a]: ~a" flag (getf (character-data-flags char-data) flag)))))

(defun character-data-to-plist (char-data)
  "Convert to plist for JSON export"
  (list :stats (character-data-stats char-data)
        :inventory (character-data-inventory char-data)
        :appearance (character-data-appearance char-data)
        :keys (character-data-keys char-data)
        :flags (character-data-flags char-data)))

(defun save-character-resource (resource char-data)
  "Persist character resource"
  (let ((path (first (game-resource-pathnames resource))))
    (when path
      (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8)
        (json:encode-json (character-data-to-plist char-data) s))
      (publish :character-change :payload char-data))))

(defmethod initialize-instance :after ((frame character-inspector-frame) &key)
  (ensure-printer-discovery-started)
  (populate-print-menu 'inspector-print-to-menu)
  (populate-send-to-menu 'inspector-send-to-menu)
  (subscribe-to-inspector-events frame))

(defmethod finalize-instance :after ((frame character-inspector-frame))
  (unsubscribe-from-inspector-events frame))

;; 
;; OPEN HELPER
;; 

(defun open-character-inspector (resource &key (mode :editing))
  "Open a character inspector for the given resource"
  (clim:run-frame-top-level
   (clim:make-application-frame 'character-inspector-frame
                                :resource resource
                                :mode mode
                                :character-data (or (getf resource :character-data)
                                                    (make-character-data))
                                :pretty-name (format nil "~a — ~a ~a"
                                                     (game-resource-title resource)
                                                     (game-resource-game-title resource)
                                                     (machine-directory-name)))))
;; 
;; NEW/IMPORT COMMANDS
;; 

(clim:define-command (com-char-new :command-table clim-internals::global-command-table :menu t :name t)
  ()
  ()
  "Create a new character"
  (let ((path (prompt-save-pathname "New Character" :defaults "character.json")))
    (when path
      (let ((char-data (make-character-data)))
        (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8)
          (json:encode-json (character-data-to-plist char-data) s))
        (open-character-inspector (make-instance 'game-resource :pathnames (list path)))))))

(clim:define-command (com-char-import :command-table clim-internals::global-command-table :menu t :name t)
  ()
  ()
  "Import a character from file"
  (let ((path (prompt-load-pathname "Import Character" :defaults "character.json")))
    (when path
      (open-character-inspector (make-instance 'game-resource :pathnames (list path))))))
