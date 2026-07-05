;;; src/version-control/gui/windows.lisp
;;; Version Control GUI windows for Skyline-Tool

(in-package :skyline-tool.version-control)

(defun format-vc-window-title (thing)
  "Format window title as: $(Thing) - Skyline-Tool for *Game-Title* (Machine-Directory-Name)"
  (format nil "~a - Skyline-Tool for ~a (~a)"
          thing
          (cl-change-case:title-case skyline-tool::*game-title*)
          (skyline-tool::machine-directory-name)))

;; Git Browser Window
(clim:define-application-frame git-browser-frame ()
  ((repo-path :initarg :repo-path :accessor git-browser-repo-path))
  (:panes
   (browser :application
            :display-function 'draw-git-browser
            :scroll-bars :vertical)
   (command-line :interactor))
  (:layouts
   (default (clim:horizontally () 
              (clim:vertically () 
                browser)
              command-line)))
  (:menu-bar t))

(defmethod clim:frame-pretty-name ((frame git-browser-frame))
  (format-vc-window-title "Git Repository"))

;; Issue Tracker Window
(clim:define-application-frame issue-tracker-frame ()
  ((project-root :initarg :project-root :accessor issue-tracker-project-root))
  (:panes
   (tracker :application
            :display-function 'draw-issue-tracker
            :scroll-bars :vertical)
   (command-line :interactor))
  (:layouts
   (default (vertically () tracker command-line)))
  (:menu-bar t))

(defmethod clim:frame-pretty-name ((frame issue-tracker-frame))
  (format-vc-window-title "Issue Tracker"))

;; Welcome/Clone Wizard Window
(clim:define-application-frame welcome-frame ()
  ((mode :initarg :mode :accessor welcome-mode))
  (:panes
   (wizard :application
           :display-function 'draw-welcome-wizard)
   (command-line :interactor))
  (:layouts
   (default (vertically () wizard command-line)))
  (:menu-bar t))

(defmethod clim:frame-pretty-name ((frame welcome-frame))
  (format-vc-window-title "Welcome"))

;; Drawing functions (stubs)
(defun draw-git-browser (frame pane)
  (declare (ignore frame))
  (clim:stream-set-cursor-position pane 10 50)
  (write-string "Git Repository Browser" pane)
  (clim:stream-set-cursor-position pane 10 70)
  (write-string "Repository path: (will show path here)" pane))

(defun draw-issue-tracker (frame pane)
  (declare (ignore frame))
  (clim:stream-set-cursor-position pane 10 50)
  (write-string "Issue Tracker" pane)
  (clim:stream-set-cursor-position pane 10 70)
  (write-string "No issues loaded yet" pane))

(defun draw-welcome-wizard (frame pane)
  (declare (ignore frame))
  (clim:stream-set-cursor-position pane 10 50)
  (write-string "Welcome to Skyline-Tool Version Control" pane)
  (clim:stream-set-cursor-position pane 10 70)
  (write-string "1. Clone Repository" pane)
  (clim:stream-set-cursor-position pane 10 90)
  (write-string "2. Initialize New Repository" pane)
  (clim:stream-set-cursor-position pane 10 110)
  (write-string "3. Open Existing Repository" pane))

;; Frame invocation functions
(defun show-git-browser (&optional (repo-path (uiop:getcwd)))
  "Show the Git browser window"
  (clim:run-frame-top-level 
   (clim:make-application-frame 'git-browser-frame
     :width 800 :height 600
     :title "Git Repository"
     :repo-path repo-path)))

(defun show-issue-tracker (&optional (project-root (uiop:getcwd)))
  "Show the Issue Tracker window"
  (clim:run-frame-top-level 
   (clim:make-application-frame 'issue-tracker-frame
     :width 800 :height 600
     :title "Issue Tracker"
     :project-root project-root)))

(defun show-welcome-wizard ()
  "Show the welcome wizard for version control setup"
  (clim:run-frame-top-level 
   (clim:make-application-frame 'welcome-frame
     :width 600 :height 400
     :title "Welcome")))

;; Preferences Inspector Window
(clim:define-application-frame preferences-frame ()
  ((current-tab :initarg :current-tab :accessor preferences-current-tab :initform :general))
  (:panes
   (notebook :application
             :display-function 'draw-preferences-dialog
             :scroll-bars :vertical)
   (command-line :interactor))
  (:layouts
   (default (vertically () notebook command-line)))
  (:menu-bar t))

(defmethod clim:frame-pretty-name ((frame preferences-frame))
  (format-vc-window-title "Preferences"))

(defun draw-preferences-dialog (frame pane)
  (declare (ignore frame))
  (clim:stream-set-cursor-position pane 10 50)
  (write-string "Preferences Inspector" pane)
  (clim:stream-set-cursor-position pane 10 70)
  (write-string "Tabs: General | Sharing | Printing & PDF | Version Control | Issue Tracking" pane))

(defun show-preferences-inspector ()
  "Show the Preferences Inspector window"
  (clim:run-frame-top-level 
   (clim:make-application-frame 'preferences-frame
     :width 800 :height 600
     :title "Preferences")))

;; Project Inspector Window - with gadget panes for editing
(clim:define-application-frame project-inspector-frame ()
  ((project-data :initarg :project-data :accessor project-inspector-data :initform nil)
   (current-tab :initform :game-info :accessor project-current-tab)
   ;; Gadget references
   (game-title-field :accessor game-title-field :initform nil)
   (game-full-title-field :accessor game-full-title-field :initform nil)
   (game-version-field :accessor game-version-field :initform nil)
   (studio-field :accessor studio-field :initform nil)
   (part-number-field :accessor part-number-field :initform nil)
   (publisher-field :accessor publisher-field :initform nil)
   (zero-page-checkbox :accessor zero-page-checkbox :initform nil)
   (machine-combo-box :accessor machine-combo-box :initform nil)
   (hokey-checkbox :accessor hokey-checkbox :initform nil)
   (palette-fields :accessor palette-fields :initform (make-hash-table))
   (script-chooser-button :accessor script-chooser-button :initform nil)
   (script-display-field :accessor script-display-field :initform nil))
  (:panes
   (notebook :application
             :display-function 'draw-project-notebook
             :scroll-bars :vertical)
   (command-line :interactor))
  (:layouts
   (default (vertically () notebook command-line)))
  (:menu-bar t))

(defmethod clim:frame-pretty-name ((frame project-inspector-frame))
  (format-vc-window-title "Project Inspector"))

;; Helper: get machine list from asset-allocator
(defun get-all-machines ()
  "Return alist of (display-name . machine-id) for all supported machines"
  (declare (internal get-all-machines))
  `((("Atari ProSystem CX-7800" . 7800)
    ("Atari SuperSystem CX-5200" . 5200)
    ("ColecoVision" . 9918)
    ("Android" . 9001)
    ("Atari vcs800" . 7850)
    ("Atari 2600" . 2600)
    ("Intellivision" . 2609)
    ("Atari 400" . 400)
    ("Atari 800" . 800)
    ("Nintendor VS" . 6122)
    ("Sinclair ZX Spectrum" . 2068)
    ("Sega Mega Drive/Genesis" . 1601)
    ("Sega 32X" . 1624)
    ("Sega SG-1000" . 1000)
    ("Sega Master System" . 3010)
    ("Sega Game Gear" . 837)
    ("Nintendo GBA" . 3296)
    ("3DO Interactive Multiplayer" . 4386)
    ("Neo Geo AES" . 4800)
    ("Nintendo VS (duplicate?)" . 6122)
    ("Sony PlayStation" . 9001)
    ("Atari Jaguar" . 8011)
    ("Sinclair ZX Spectrum (alt)" . 7600)
    ("Atari vcs800 (alt)" . 7850)
    ("Commodore 128" . 7801)
    ("Atari ST" . 1080)
    ("Toshiba T2000HX" . 1200)
    ("Nintendo SNES" . 2068)
    ("3DO" . 4386)
    ("Game Boy" . 20953)
    ("Game Boy Color" . 35902)
    ("MSX" . 810))))

(defun get-machine-all-machines ()
  "Return the same list as get-all-machines for compatibility."
  (get-all-machines))

;; Build machine lookup table
(defparameter *machine-lookup-table*
  (loop for (name . id) in (get-all-machines)
        collect (cons id (list :title name :id id))))

(defun get-machine-info (machine-id)
  (cdr (assoc machine-id *machine-lookup-table* :test #'equal)))

(defun find-machine-by-title (title)
  (loop for (name . id) in (get-all-machines)
        when (string= name title)
        return id))

;; Drawing functions
(defun draw-project-notebook (frame pane)
  (declare (ignore frame))
  (clim:stream-set-cursor-position pane 10 10)
  (write-string "Project Inspector" pane)
  (clim:stream-set-cursor-position pane 10 30)
  (write-string "Tabs: Game Info | Machine Settings | Sound/Palette | New Game Script" pane))

;; Game Info tab - with actual editable gadgets
(defun draw-project-game-info (frame pane)
  (let* ((data (project-inspector-data frame)))
    ;; Labels
    (clim:stream-set-cursor-position pane 10 50)
    (write-string "Game Title:" pane)
    (clim:stream-set-cursor-position pane 10 90)
    (write-string "Game Full Title:" pane)
    (clim:stream-set-cursor-position pane 10 130)
    (write-string "Game Version:" pane)
    (clim:stream-set-cursor-position pane 10 170)
    (write-string "Studio:" pane)
    (clim:stream-set-cursor-position pane 10 210)
    (write-string "Part Number:" pane)
    (clim:stream-set-cursor-position pane 10 250)
    (write-string "Publisher:" pane)
    
    ;; Create or update gadgets
    (unless (game-title-field frame)
      (setf (game-title-field frame)
            (clim:make-pane 'text-field
                            :value (or (getf data :game-title) "")
                            :width 300
                            :activate-callback (lambda (gadget value)
                                                 (declare (ignore gadget))
                                                 (setf (getf (project-inspector-data frame) :game-title) value)))))
    (unless (game-full-title-field frame)
      (setf (game-full-title-field frame)
            (clim:make-pane 'text-field
                            :value (or (getf data :game-full-title) "")
                            :width 500
                            :activate-callback (lambda (gadget value)
                                                 (declare (ignore gadget))
                                                 (setf (getf (project-inspector-data frame) :game-full-title) value)))))
    (unless (game-version-field frame)
      (setf (game-version-field frame)
            (clim:make-pane 'text-field
                            :value (or (getf data :game-version) "")
                            :width 150
                            :activate-callback (lambda (gadget value)
                                                 (declare (ignore gadget))
                                                 (setf (getf (project-inspector-data frame) :game-version) value)))))
    (unless (studio-field frame)
      (setf (studio-field frame)
            (clim:make-pane 'text-field
                            :value (or (getf data :studio) "")
                            :width 300
                            :activate-callback (lambda (gadget value)
                                                 (declare (ignore gadget))
                                                 (setf (getf (project-inspector-data frame) :studio) value)))))
    (unless (part-number-field frame)
      (setf (part-number-field frame)
            (clim:make-pane 'text-field
                            :value (or (getf data :part-number) "")
                            :width 150
                            :activate-callback (lambda (gadget value)
                                                 (declare (ignore gadget))
                                                 (setf (getf (project-inspector-data frame) :part-number) value)))))
    (unless (publisher-field frame)
      (setf (publisher-field frame)
            (clim:make-pane 'text-field
                            :value (or (getf data :publisher) "")
                            :width 300
                            :activate-callback (lambda (gadget value)
                                                 (declare (ignore gadget))
                                                 (setf (getf (project-inspector-data frame) :publisher) value)))))
    (unless (zero-page-checkbox frame)
      (setf (zero-page-checkbox frame)
            (clim:make-pane 'check-box
                            :label "Zero Page Homebrew Edition"
                            :value (getf data :zero-page-homebrew nil)
                            :value-changed-callback (lambda (gadget value)
                                                      (declare (ignore gadget))
                                                      (setf (getf (project-inspector-data frame)
                                                                  :zero-page-homebrew)
                                                            value)))))
    
    ;; Position gadgets
    (clim:stream-set-cursor-position pane 150 48)
    (clim:note-gadget-activated (game-title-field frame) pane)
    (clim:stream-set-cursor-position pane 150 88)
    (clim:note-gadget-activated (game-full-title-field frame) pane)
    (clim:stream-set-cursor-position pane 150 128)
    (clim:note-gadget-activated (game-version-field frame) pane)
    (clim:stream-set-cursor-position pane 150 168)
    (clim:note-gadget-activated (studio-field frame) pane)
    (clim:stream-set-cursor-position pane 150 208)
    (clim:note-gadget-activated (part-number-field frame) pane)
    (clim:stream-set-cursor-position pane 150 248)
    (clim:note-gadget-activated (publisher-field frame) pane)
    (clim:stream-set-cursor-position pane 150 288)
    (clim:note-gadget-activated (zero-page-checkbox frame) pane)))

;; Machine Settings tab - with combo box gadget
(defun draw-project-machine (frame pane)
  (declare (ignore frame))
  (let* ((data (project-inspector-data frame))
         (current-machine (or (getf data :machine) 7800))
         (machine-info (get-machine-info current-machine))
         (machine-title (when machine-info (getf machine-info :title))))
    ;; Labels
    (clim:stream-set-cursor-position pane 10 50)
    (write-string "Machine:" pane)
    (clim:stream-set-cursor-position pane 10 90)
    (write-string "CPU:" pane)
    (clim:stream-set-cursor-position pane 10 130)
    (write-string "Sound:" pane)
    
    ;; Create machine combo box
    (unless (machine-combo-box frame)
      (setf (machine-combo-box frame)
            (clim:make-pane 'combo-box
                            :items (mapcar #'first (get-machine-all-machines))
                            :value machine-title
                            :width 300
                            :value-changed-callback (lambda (gadget value)
                                                      (declare (ignore gadget))
                                                      (let ((id (find-machine-by-title value)))
                                                        (when id
                                                          (setf (getf (project-inspector-data frame) :machine) id)
                                                          (clim:redisplay-frame-pane frame pane :force-p t)))))))
    
    ;; Create Hokey checkbox
    (unless (hokey-checkbox frame)
      (setf (hokey-checkbox frame)
            (clim:make-pane 'check-box
                            :label "Hokey enabled"
                            :value (getf data :hokey-enabled nil)
                            :value-changed-callback (lambda (gadget value)
                                                      (declare (ignore gadget))
                                                      (setf (getf (project-inspector-data frame)
                                                                  :hokey-enabled)
                                                            value)))))
    
    ;; Position gadgets
    (clim:stream-set-cursor-position pane 150 48)
    (clim:note-gadget-activated (machine-combo-box frame) pane)
    
    ;; CPU display (read-only)
    (clim:stream-set-cursor-position pane 150 88)
    (write-string "6502 (read-only for most systems)" pane)
    
    (clim:stream-set-cursor-position pane 150 128)
    (clim:note-gadget-activated (hokey-checkbox frame) pane)))

;; Sound/Palette tab - with palette text fields
(defun draw-project-sound-palette (frame pane)
  (declare (ignore frame))
  (let* ((data (project-inspector-data frame))
         (palettes (getf data :palette-names '(("P4C1" "") ("P4C2" "") ("P4C3" "") ("P5C1" "") ("P7C3" "")))))
    (clim:stream-set-cursor-position pane 10 50)
    (write-string "Sound:" pane)
    (clim:stream-set-cursor-position pane 10 90)
    (write-string "Common Palette Names:" pane)
    
    ;; Hokey checkbox
    (unless (hokey-checkbox frame)
      (setf (hokey-checkbox frame)
            (clim:make-pane 'check-box
                            :label "Hokey enabled"
                            :value (getf data :hokey-enabled nil)
                            :value-changed-callback (lambda (gadget value)
                                                      (setf (getf (project-inspector-data frame) :hokey-enabled)
                                                            value)))))
    
    (clim:stream-set-cursor-position pane 150 48)
    (clim:note-gadget-activated (hokey-checkbox frame) pane)
    
    ;; Palette fields
    (loop for (name . default) in palettes
          for i from 0
          do (let ((field (gethash name (palette-fields frame))))
               (unless field
                 (setf field
                       (clim:make-pane 'text-field
                                       :value default
                                       :width 200
                                       :activate-callback (lambda (gadget value)
                                                            (setf (getf (getf (project-inspector-data frame)
                                                                              :palette-names)
                                                                        name)
                                                                  value)
                                                            (setf (gethash name (palette-fields frame)) field)))
                       (gethash name (palette-fields frame)) field)
                 (clim:stream-set-cursor-position pane 50 (+ 130 (* i 40)))
                 (write-string (format nil "~A:" name) pane)
                 (clim:stream-set-cursor-position pane 150 (+ 128 (* i 40)))
                 (clim:note-gadget-activated field pane))))))

;; New Game Script tab - with resource chooser
(defun draw-project-script (frame pane)
  (declare (ignore frame))
  (let* ((data (project-inspector-data frame))
         (current-script (or (getf data :new-game-script) "Global \"New Game\"")))
    (clim:stream-set-cursor-position pane 10 50)
    (write-string "New Game Script:" pane)
    
    ;; Create script display field
    (unless (script-display-field frame)
      (setf (script-display-field frame)
            (clim:make-pane 'text-field
                            :value current-script
                            :width 400
                            :activate-callback (lambda (gadget value)
                                                 (setf (getf (project-inspector-data frame) :new-game-script) value)))))
    
    ;; Create Choose button
    (unless (script-chooser-button frame)
      (setf (script-chooser-button frame)
            (clim:make-pane
             'push-button
             :label "Choose..."
             :activate-callback (lambda (gadget)
                                  (declare (ignore gadget))
                                  (show-resource-chooser
                                   :kind :script
                                   :callback
                                   (lambda (selected)
                                     (when selected
                                       (setf (getf (project-inspector-data frame) :new-game-script) selected)
                                       (setf (clim:gadget-value (script-display-field frame)) selected)
                                       (clim:redisplay-frame-pane frame pane :force-p t))))))))
    
    (clim:stream-set-cursor-position pane 150 48)
    (clim:note-gadget-activated (script-display-field frame) pane)
    (clim:stream-set-cursor-position pane 560 48)
    (clim:note-gadget-activated (script-chooser-button frame) pane)))

;; Resource chooser for scripts
(defun show-resource-chooser (&key (kind "script") callback)
  "Show a resource chooser dialog for KIND, calling CALLBACK with selected resource"
  (clim:with-output-as-presentation (stream (make-instance 'resource-chooser-dialog
                                                           :kind kind :callback callback)
                                            'resource-chooser)
    (format stream "Resource Chooser: ~A~%~%" kind)
    (write-string "Select a resource..." stream)))

(defclass resource-chooser-dialog ()
  ((kind :initarg :kind :accessor chooser-kind)
   (callback :initarg :callback :accessor chooser-callback)))

(defun show-project-inspector (&optional (project-data *project.json*))
  "Show the Project Inspector window"
  (clim:run-frame-top-level 
   (clim:make-application-frame 'project-inspector-frame
     :width 800 :height 700
     :title "Project Inspector"
     :project-data project-data)))
