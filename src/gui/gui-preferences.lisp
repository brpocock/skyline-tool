;; Preferences Inspector - Proper CLIM gadgets for development preferences

(in-package :skyline-tool)

#|

Preferences Inspector:

#### menu

Preferences
------------
Reset to Defaults...
---
Save as > Text...
          JSON...
          PDF...
Send to > { p2p recipient list }
Print to > { printer list }
---
Close

Edit
-----
Cut
Copy
Paste
---
Find...

View
-----
[] Editable
[] Project Pane

Help
-----
How to Manage Preferences...
Skyline-Tool Developers' Guide...
Skyline-Tool Scripting Guide...
---
About Skyline-Tool...

### Window

Printing
---------

Paper Size: <> U.S. Letter
            <> U.S. Legal
            <> A4
            <> Custom: ____ [ mm - ] × ____ [ mm - ]
                            [ cm   ]        [ cm   ]
                            [ in   ]        [ in   ]

Network
--------

[] LAN Resource Sharing
[] Share "Dist/" Folder
[] Share Music as Media Server

Versioning
-----------

Version Control System: <> Git                   | git
                        <> Subversion            | svn
                        <> Bazaar                | bzr
                        <> Mercurial             | hg
                        <> Concurrent            | cvs
                        <> Revision Control      | rcs

Git                     # Specific to the version control system in use
----

   User
   -----
   Name: ________________________
   eMail: ________________________

   Signing key: [ Bruce-Robert Pocock <brpocock@interworldly.com>    - ]
                [ ---                                                  ]
                [ Generate and publish a new key...                    ]
     [] Sign Commits
     [] Sign Tags

   Tools
   ------
   Merge: [ meld - ]  [] Prompt first
   Diff:  [ meld - ]  [] Prompt first
            ##( git difftool --tool-help first section of output only )
            ##( "may be set to one of the following:" options only )
            ##( do not list "valid, but not currently available" list )

   Remotes
   --------

   | Remote        | URL                               | Fetch                               |
   |-----------------------------------------------------------------------------------------|
   | origin        | git@github.com:brpocock/Phantasia | +refs/heads/*:refs/remotes/origin/* |

                                                                                          ( + )

   Submodules
   -----------
   [] Skyline-Tool and Eightbol

   [] Atari 7800 Tools
   [] Intellivision Tools

   Pushing
   --------
   [] When pushing, automatically set up new branches on remote

   When pulling, automatically [ fast-forward only   - ]

   Default Branch: ___________                                              # main default

Issue Tracking
---------------

Issue tracker kind: <> GitHub
                    <> GitLab
                    <> Bugzilla

Tracker URL: ____________________________________________________

     ( Sign in... )

|#

(define-constant +paper-sizes+
    '((:us-letter "U.S. Letter" 215.9 279.4)
      (:us-legal "U.S. Legal" 215.9 355.6)
      (:a4 "A4" 210.0 297.0))
  :test 'equalp
  :documentation "Paper size options: (key label width-mm height-mm).")

;;  Preferences data model

(defvar *preferences-config* nil
  "Global preferences plist. Keys:
:paper-size, :lan-sharing, :dist-sharing, :music-sharing,
:vc-system, :issue-tracker, :tracker-url,
:ssh-key-path, :gpg-key-path,
:theme, :language, :show-tips, :auto-save, :driver")

(defun default-preferences ()
  (list :paper-size :us-letter
        :paper-unit :mm
        :paper-width 215.9
        :paper-height 279.4
        :lan-sharing nil
        :dist-sharing nil
        :music-sharing nil
        :vc-system :git
        :git-user-name ""
        :git-user-email ""
        :git-signing-key ""
        :git-sign-commits nil
        :git-sign-tags nil
        :git-merge-tool "meld"
        :git-merge-prompt t
        :git-diff-tool "meld"
        :git-diff-prompt t
        :git-remotes '(("origin" "git@github.com:brpocock/Phantasia" "+refs/heads/*:refs/remotes/origin/*"))
        :git-submodules '(:skyline-tool t :a7800-tools nil :intellivision-tools nil)
        :git-push-auto-setup t
        :git-pull-behavior "fast-forward only"
        :git-default-branch "main"
        :issue-tracker :github
        :tracker-url "https://github.com/adventuring/phantasia"
        :ssh-key-path ""
        :gpg-key-path ""
        :language "en"
        :atarivox-port nil
        :atarivox-volume 12
        :build :demo
        :region :ntsc
        :units (list :length :mm)
        :last-save-dir (list :default #p"~/work")

        ;; P2P Sharing Preferences
        :p2p-enabled nil
        :p2p-user (user-full-name) ; Auto-detect OS username
        :p2p-domain "local." ; DNS-SD domain
        :p2p-interface "eth0" ; Default interface for Avahi
        :p2p-advertise-enabled nil ; Auto-start on preference change
        :p2p-advertise-interval 30 ; Seconds between service announcements
        :p2p-discovery-interval 30 ; Seconds between service discovery checks
        :p2p-min-port 50000 ; Minimum UDP port for peer connections
        :p2p-max-port 60000 ; Maximum UDP port for peer connections
        :p2p-pubkey-algo "ed25519" ; Public key algorithm for signing offers
        ))

(defun convert-unit (value from-unit to-unit)
  "Convert VALUE (a length) between :mm, :cm, :in, :pt.
All stored dimensions are kept internally in millimetres."
  (let ((factors '(:mm 1.0 :cm 10.0 :in 25.4 :pt 25.4/72)))
    (when (or (null value) (zerop value)) (return-from convert-unit 0))
    (let ((in-mm (* value (getf factors from-unit)))
          (to-factor (getf factors to-unit)))
      (/ in-mm to-factor))))

;;  Frame

(clim:define-application-frame preferences-inspector-frame (clim:standard-application-frame)
  ((config :initarg :config :accessor frame-config)
   (dirty :initform nil :accessor frame-dirty)
   ;; Paper section
   (paper-size-box :initform nil :accessor paper-size-box)
   (paper-size-radio :initform nil :accessor paper-size-radio)
   (paper-unit-box :initform nil :accessor paper-unit-box)
   (paper-unit-radio :initform nil :accessor paper-unit-radio)
   (paper-width-field :initform nil :accessor paper-width-field)
   (paper-height-field :initform nil :accessor paper-height-field)
   ;; Sharing section
   (lan-sharing-checkbox :initform nil :accessor lan-sharing-checkbox)
   (dist-sharing-checkbox :initform nil :accessor dist-sharing-checkbox)
   (music-sharing-checkbox :initform nil :accessor music-sharing-checkbox)
   ;; VC section
   (vc-system-box :initform nil :accessor vc-system-box)
   (vc-system-radio :initform nil :accessor vc-system-radio)
   (ssh-key-field :initform nil :accessor ssh-key-field)
   (gpg-key-field :initform nil :accessor gpg-key-field)
   ;; Git-specific fields
   (git-user-name-field :initform nil :accessor git-user-name-field)
   (git-user-email-field :initform nil :accessor git-user-email-field)
   (git-signing-key-field :initform nil :accessor git-signing-key-field)
   (git-sign-commits-checkbox :initform nil :accessor git-sign-commits-checkbox)
   (git-sign-tags-checkbox :initform nil :accessor git-sign-tags-checkbox)
   (git-merge-tool-field :initform nil :accessor git-merge-tool-field)
   (git-merge-prompt-checkbox :initform nil :accessor git-merge-prompt-checkbox)
   (git-diff-tool-field :initform nil :accessor git-diff-tool-field)
   (git-diff-prompt-checkbox :initform nil :accessor git-diff-prompt-checkbox)
   (git-push-auto-setup-checkbox :initform nil :accessor git-push-auto-setup-checkbox)
   (git-pull-behavior-combo :initform nil :accessor git-pull-behavior-combo)
   (git-default-branch-field :initform nil :accessor git-default-branch-field)
   ;; Submodules
   (git-submodule-skyline-checkbox :initform nil :accessor git-submodule-skyline-checkbox)
   (git-submodule-a7800-checkbox :initform nil :accessor git-submodule-a7800-checkbox)
   (git-submodule-intellivision-checkbox :initform nil :accessor git-submodule-intellivision-checkbox)
    ;; Issue Tracker section
    (issue-tracker-box :initform nil :accessor issue-tracker-box)
    (issue-tracker-radio :initform nil :accessor issue-tracker-radio)
    (tracker-url-field :initform nil :accessor tracker-url-field)
    (sign-in-button :initform nil :accessor sign-in-button)
    ;; P2P Sharing section
    (p2p-enabled-checkbox :initform nil :accessor p2p-enabled-checkbox)
    (p2p-user-field :initform nil :accessor p2p-user-field)
    (p2p-domain-field :initform nil :accessor p2p-domain-field)
    (p2p-interface-field :initform nil :accessor p2p-interface-field)
    (p2p-advertise-interval-field :initform nil :accessor p2p-advertise-interval-field)
    (p2p-discovery-interval-field :initform nil :accessor p2p-discovery-interval-field)
    (p2p-min-port-field :initform nil :accessor p2p-min-port-field)
    (p2p-max-port-field :initform nil :accessor p2p-max-port-field)
    (p2p-pubkey-algo-field :initform nil :accessor p2p-pubkey-algo-field)
    (p2p-auto-start-checkbox :initform nil :accessor p2p-auto-start-checkbox)
    (watcher-thread :initform nil :accessor prefs-watcher-thread))
  (:menu-bar preferences-inspector-menu-bar)
  (:icon (skyline-tool-icon :resource :preferences))
  (:pretty-name "Preferences")
  (:panes
   (main-pane :application
              :display-function 'display-preferences
              :height 700 :width 800
              :scroll-bars :vertical)
   (status-pane :application
                :display-function 'display-prefs-status
                :height 30 :width 800))
  (:layouts
   (default (clim:vertically () main-pane status-pane))))

(defmethod clim:frame-pretty-name ((frame preferences-inspector-frame))
  (format nil "Preferences — ~a (~a)"
          (title-case (if (boundp '*game-title*) *game-title* "Game"))
          (machine-directory-name)))

(defmethod initialize-instance :after ((frame preferences-inspector-frame) &key)
  (let ((prefs-path (prefs-pathname)))
    (when (probe-file prefs-path)
      (ensure-directories-exist (make-pathname :defaults prefs-path :name nil :type nil))
      (setf (prefs-watcher-thread frame)
            (submit-task
             (lambda ()
               (handler-case
                   (inotify:with-inotify (inot (list (list prefs-path inotify:in-modify)))
                     (loop for ev = (inotify:read-events inot)
                           do (ignore-errors
                               (load-preferences-config)
                               (setf (frame-config frame) *preferences-config*)
                               (clim:redisplay-frame-panes frame :force-p t))))
                 (error (c)
                   (declare (ignore c)))))))))
  ;; Populate dynamic menus
  (ignore-errors (populate-print-menu 'preferences-print-to-menu))
  ;; Subscribe to external preference-change events so other windows stay in sync
  (subscribe :preference-change
             (lambda (event)
               (declare (ignore event))
               (ignore-errors
                (load-preferences-config)
                (setf (frame-config frame) *preferences-config*)
                (clim:redisplay-frame-panes frame :force-p t)))))

;;  Own thread launcher

(defun open-preferences-inspector ()
  (unless *preferences-config*
    (setf *preferences-config* (default-preferences)))
  (let ((frame (clim:make-application-frame
                'preferences-inspector-frame
                :config *preferences-config*)))
    (make-thread (lambda ()
                      (clim:run-frame-top-level frame))
                    :name (format nil "Preferences Inspector: ~a (~a)"
                                  (title-case (if (boundp '*game-title*) *game-title* "Game"))
                                  (machine-directory-name)))))

;;  Menus

(clim:define-command-table preferences-save-as-menu
  :menu (("Text..." :command com-preferences-save-text)
         ("JSON..." :command com-preferences-save-json)
         ("PDF..." :command com-preferences-save-pdf)))

(clim:define-command-table preferences-send-to-menu
  :menu ())

(clim:define-command-table preferences-print-to-menu
  :menu ())

(clim:define-command-table preferences-inspector-file-menu
  :menu (("Reset to Defaults" :command com-preferences-reset)
         (nil :divider :line)
         ("Save as" :menu preferences-save-as-menu)
         ("Send to" :menu preferences-send-to-menu)
         ("Print to" :menu preferences-print-to-menu)
         (nil :divider :line)
         ("Close" :command com-close-frame)))

(clim:define-command-table preferences-inspector-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table preferences-inspector-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :toggle t)
         (nil :divider :line)
         ("Project Pane" :command com-toggle-project-pane :toggle t)))

(clim:define-command-table preferences-inspector-help-menu
  :menu (("How to Manage Preferences..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-scripting-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table preferences-inspector-menu-bar
  :menu (("Preferences" :menu preferences-inspector-file-menu)
         ("Edit" :menu preferences-inspector-edit-menu)
         ("View" :menu preferences-inspector-view-menu)
         ("Help" :menu preferences-inspector-help-menu)))

;;  Commands

(clim:define-command (com-preferences-reset :command-table clim-internals::global-command-table
                                             :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (setf (frame-config frame) (default-preferences))
    (setf (frame-dirty frame) t)
    (save-preferences-now frame)
    (clim:redisplay-frame-panes frame :force-p t)))

(clim:define-command (com-close-frame :command-table clim-internals::global-command-table
                                      :menu t :name t) ()
  (clim:frame-exit clim:*application-frame*))

(clim:define-command (com-preferences-save-text :command-table clim-internals::global-command-table
                                                 :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (path (prompt-save-pathname "preferences.txt")))
    (when path
      (with-open-file (s path :direction :output :if-exists :supersede)
        (let ((*standard-output* s))
          (with-text-output (s)
            (format s "Preferences — ~a (~a)~%"
                    (title-case (if (boundp '*game-title*) *game-title* "Game"))
                    (machine-directory-name))
            (loop for (k v) on (frame-config frame) by #'cddr
                  do (format s "  ~s: ~s~%" k v))))))))

(clim:define-command (com-preferences-save-json :command-table clim-internals::global-command-table
                                                 :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (path (prompt-save-pathname "preferences.json")))
    (when path
      (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8)
        (json:encode-json
         (loop for (k v) on (frame-config frame) by #'cddr
               collect (cons (string-downcase k) v))
         s)))))

(clim:define-command (com-preferences-save-pdf :command-table clim-internals::global-command-table
                                                :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (path (prompt-save-pathname "preferences.ps")))
    (when path
      (with-open-file (s path :direction :output :if-exists :supersede)
        (write-ps-header-bar s "Preferences" (format-timestring nil (get-universal-time))
                             (user-homedir-pathname)
                             (title-case (if (boundp '*game-title*) *game-title* "Game")))
        (loop for (k v) on (frame-config frame) by #'cddr
              do (format s "(~s: ~s) showpage show~%" k v)))
      (let ((pdf-path (make-pathname :type "pdf" :defaults path)))
        (uiop:run-program (list "ps2pdf" (namestring path) (namestring pdf-path))
                          :output nil :ignore-error-status t)))))

(clim:define-command (com-cut :command-table clim-internals::global-command-table
                              :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (declare (ignore frame))))

(clim:define-command (com-copy :command-table clim-internals::global-command-table
                               :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (declare (ignore frame))))

(clim:define-command (com-paste :command-table clim-internals::global-command-table
                                :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (declare (ignore frame))))

(clim:define-command (com-find :command-table clim-internals::global-command-table
                               :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (declare (ignore frame))))

(clim:define-command (com-inspector-toggle-view :command-table clim-internals::global-command-table
                                                :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (declare (ignore frame))))

(clim:define-command (com-toggle-project-pane :command-table clim-internals::global-command-table
                                              :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (declare (ignore frame))))

(clim:define-command (com-help-for-window :command-table clim-internals::global-command-table
                                          :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (declare (ignore frame))))

(clim:define-command (com-open-dev-guide :command-table clim-internals::global-command-table
                                         :menu t :name t) ()
  (let ((guide-path (asdf:system-relative-pathname :skyline-tool
                                                    "../Source/Documentation/PhantasiaDevGuide.texi")))
    (when (probe-file guide-path)
      (uiop:run-program (list "xdg-open" (namestring guide-path)) :output nil :ignore-error-status t))))

(clim:define-command (com-open-scripting-guide :command-table clim-internals::global-command-table
                                               :menu t :name t) ()
  (let ((guide-path (asdf:system-relative-pathname :skyline-tool
                                                    "../Source/Documentation/FountainScripting.md")))
    (when (probe-file guide-path)
      (uiop:run-program (list "xdg-open" (namestring guide-path)) :output nil :ignore-error-status t))))

;;  Presentation types for right-click context menus

(clim:define-presentation-type preferences-section ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'preferences-section)))
  (stringp object))

(clim:define-presentation-type preferences-gadget ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'preferences-gadget)))
  t)

(clim:define-presentation-type git-remote-item ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'git-remote-item)))
  (listp object))

;;  Context menus for right-click on preferences sections and gadgets

(clim:define-command-table preferences-section-context-menu
  :menu (("Reset Section to Defaults" :command com-preferences-reset-section)
         ("How to Manage This Setting..." :command com-help-for-window)))

(clim:define-command-table preferences-gadget-context-menu
  :menu (("Copy Value" :command com-copy)
         ("Reset to Default" :command com-preferences-reset-section)
         ("How to Manage..." :command com-help-for-window)))

(clim:define-command (com-preferences-reset-section :command-table clim-internals::global-command-table
                                                     :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (setf (frame-config frame) (default-preferences))
    (save-preferences-now frame)
    (clim:redisplay-frame-panes frame :force-p t)))

;;  Display

(defun display-preferences (frame pane)
  (let ((config (frame-config frame)))
    (create-preference-gadgets frame pane config)
    (position-preference-gadgets frame pane)))

(defun save-preferences-now (frame)
  (write-preferences-config frame)
  (setf (frame-dirty frame) nil)
  (publish :preference-change :payload (frame-config frame)))

(defun make-preference-callback (frame key &key converter redisplay)
  (lambda (gadget value)
    (declare (ignore gadget))
    (let ((val (if converter (funcall converter value) value)))
      (setf (getf (frame-config frame) key) val))
    (save-preferences-now frame)
    (when redisplay
      (clim:redisplay-frame-panes frame :force-p t))))

(defun create-preference-gadgets (frame pane config)
  ;; Paper Size
  (clim:stream-set-cursor-position pane 10 14)
  (clim:with-text-face (pane :bold)
    (format pane "Paper Size"))
  (unless (paper-size-box frame)
    (let* ((current-size (getf config :paper-size :us-letter))
           (custom-p (eq current-size :custom))
           (box (clim:make-pane 'clim:radio-box)))
      (setf (paper-size-box frame) box)
      (setf (paper-size-radio frame)
            (loop for (key label w h) in +paper-sizes+
                  collect (clim:make-pane 'clim:toggle-button
                                          :label label
                                          :value (eq current-size key)
                                          :group box
                                          :value-changed-callback
                                          (lambda (g v)
                                            (declare (ignore g))
                                            (when v
                                              (setf (getf (frame-config frame) :paper-size) key)
                                              (when w
                                                (setf (getf (frame-config frame) :paper-width) w)
                                                (setf (getf (frame-config frame) :paper-height) h))
                                              (save-preferences-now frame)
                                              (clim:redisplay-frame-panes frame :force-p t))))))))
  ;; Paper Unit (only show when Custom is selected)
  (when (eq (getf config :paper-size) :custom)
    (clim:stream-set-cursor-position pane 10 74)
    (clim:with-text-face (pane :bold)
      (format pane "Paper Unit"))
    (unless (paper-unit-box frame)
      (let* ((current-unit (getf config :paper-unit :mm))
             (box (clim:make-pane 'clim:radio-box)))
        (setf (paper-unit-box frame) box)
        (setf (paper-unit-radio frame)
              (loop for unit in '(:mm :cm :in :pt)
                    collect (clim:make-pane 'clim:toggle-button
                                            :label (string unit)
                                            :value (eq current-unit unit)
                                            :group box
                                            :value-changed-callback
                                            (lambda (g v)
                                              (declare (ignore g))
                                              (when v
                                                (let ((old-unit (getf (frame-config frame) :paper-unit))
                                                      (new-unit unit))
                                                  (setf (getf (frame-config frame) :paper-unit) new-unit)
                                                  (let ((w (getf (frame-config frame) :paper-width))
                                                        (h (getf (frame-config frame) :paper-height)))
                                                    (when (and w h)
                                                      (setf (getf (frame-config frame) :paper-width)
                                                            (round (convert-unit w old-unit new-unit) 1))
                                                      (setf (getf (frame-config frame) :paper-height)
                                                            (round (convert-unit h old-unit new-unit) 1)))))
                                                (save-preferences-now frame)
                                                (clim:redisplay-frame-panes frame :force-p t))))))))
    ;; Paper Width/Height (only show when Custom is selected)
    (clim:stream-set-cursor-position pane 10 120)
    (format pane "Width:")
    (unless (paper-width-field frame)
      (setf (paper-width-field frame)
            (clim:make-pane 'clim:text-field
                            :value (format nil "~a" (getf config :paper-width 215.9))
                            :width 80
                            :activate-callback
                            (lambda (gadget value)
                              (declare (ignore gadget))
                              (setf (getf (frame-config frame) :paper-width) (ignore-errors (read-from-string value)))
                              (save-preferences-now frame)))))
    (clim:stream-set-cursor-position pane 10 150)
    (format pane "Height:")
    (unless (paper-height-field frame)
      (setf (paper-height-field frame)
            (clim:make-pane 'clim:text-field
                            :value (format nil "~a" (getf config :paper-height 279.4))
                            :width 80
                            :activate-callback
                            (lambda (gadget value)
                              (declare (ignore gadget))
                              (setf (getf (frame-config frame) :paper-height) (ignore-errors (read-from-string value)))
                              (save-preferences-now frame))))))

;; Sharing Services
   (clim:stream-set-cursor-position pane 10 194)
   (clim:with-text-face (pane :bold)
     (format pane "Sharing Services"))
   (unless (lan-sharing-checkbox frame)
     (setf (lan-sharing-checkbox frame)
           (clim:make-pane 'clim:check-box
                           :label "LAN Resource Sharing"
                           :value (getf config :lan-sharing nil)
                           :value-changed-callback
                           (lambda (g v)
                             (declare (ignore g))
                             (setf (getf (frame-config frame) :lan-sharing) v)
                             (save-preferences-now frame)
                             (clim:redisplay-frame-panes frame :force-p t)))))
   (unless (dist-sharing-checkbox frame)
     (setf (dist-sharing-checkbox frame)
           (clim:make-pane 'clim:check-box
                           :label "Share Dist/ Folder"
                           :value (getf config :dist-sharing nil)
                           :value-changed-callback
                           (lambda (g v)
                             (declare (ignore g))
                             (setf (getf (frame-config frame) :dist-sharing) v)
                             (save-preferences-now frame)
                             (clim:redisplay-frame-panes frame :force-p t)))))
   (unless (music-sharing-checkbox frame)
     (setf (music-sharing-checkbox frame)
           (clim:make-pane 'clim:check-box
                           :label "Share Music as Media Server"
                           :value (getf config :music-sharing nil)
                           :value-changed-callback
                           (lambda (g v)
                             (declare (ignore g))
                             (setf (getf (frame-config frame) :music-sharing) v)
                             (save-preferences-now frame)
                             (clim:redisplay-frame-panes frame :force-p t)))))

   ;; P2P Sharing
   (clim:stream-set-cursor-position pane 10 334)
   (clim:with-text-face (pane :bold)
     (format pane "P2P Sharing"))
   (unless (p2p-enabled-checkbox frame)
     (setf (p2p-enabled-checkbox frame)
           (clim:make-pane 'clim:check-box
                           :label "Enable P2P Resource Sharing"
                           :value (getf config :p2p-enabled nil)
                           :value-changed-callback
                           (lambda (g v)
                             (declare (ignore g))
                             (setf (getf (frame-config frame) :p2p-enabled) v)
                             (save-preferences-now frame)
                             (clim:redisplay-frame-panes frame :force-p t)))))
   (clim:stream-set-cursor-position pane 20 370)
   (format pane "Advertised User Name:")
   (unless (p2p-user-field frame)
     (setf (p2p-user-field frame)
           (clim:make-pane 'clim:text-field
                           :value (or (getf config :p2p-user "")
                                      (user-full-name))
                           :width 300
                           :activate-callback
                           (lambda (gadget value)
                             (declare (ignore gadget))
                             (setf (getf (frame-config frame) :p2p-user) value)
                             (save-preferences-now frame)))))
   (clim:stream-set-cursor-position pane 20 400)
   (format pane "Service Domain:")
   (unless (p2p-domain-field frame)
     (setf (p2p-domain-field frame)
           (clim:make-pane 'clim:text-field
                           :value (getf config :p2p-domain "local.")
                           :width 200
                           :activate-callback
                           (lambda (gadget value)
                             (declare (ignore gadget))
                             (setf (getf (frame-config frame) :p2p-domain) value)
                             (save-preferences-now frame)))))
   (clim:stream-set-cursor-position pane 20 430)
   (format pane "Network Interface:")
   (unless (p2p-interface-field frame)
     (setf (p2p-interface-field frame)
           (clim:make-pane 'clim:text-field
                           :value (getf config :p2p-interface "eth0")
                           :width 200
                           :activate-callback
                           (lambda (gadkit value)
                             (declare (ignore gadget))
                             (setf (getf (frame-config frame) :p2p-interface) value)
                             (save-preferences-now frame)))))
   (clim:stream-set-cursor-position pane 20 460)
   (format pane "Advertise Interval (sec):")
   (unless (p2p-advertise-interval-field frame)
     (setf (p2p-advertise-interval-field frame)
           (clim:make-pane 'clim:text-field
                           :value (format nil "~a" (getf config :p2p-advertise-interval 30))
                           :width 80
                           :activate-callback
                           (lambda (gadget value)
                             (declare (ignore gadget))
                             (let ((val (parse-integer (or value "30") :junk-allowed t)))
                               (setf (getf (frame-config frame) :p2p-advertise-interval) (max 5 (min 300 val)))
                               (save-preferences-now frame))))))
   (clim:stream-set-cursor-position pane 20 490)
   (format pane "Discovery Interval (sec):")
   (unless (p2p-discovery-interval-field frame)
     (setf (p2p-discovery-interval-field frame)
           (clim:make-pane 'clim:text-field
                           :value (format nil "~a" (getf config :p2p-discovery-interval 30))
                           :width 80
                           :activate-callback
                           (lambda (gadget value)
                             (declare (ignore gadget))
                             (let ((val (parse-integer (or value "30") :junk-allowed t)))
                               (setf (getf (frame-config frame) :p2p-discovery-interval) (max 5 (min 300 val)))
                               (save-preferences-now frame))))))
   (clim:stream-set-cursor-position pane 20 520)
   (format pane "Port Range:")
   (clim:stream-set-cursor-position pane 20 550)
   (format pane "Min:")
   (unless (p2p-min-port-field frame)
     (setf (p2p-min-port-field frame)
           (clim:make-pane 'clim:text-field
                           :value (format nil "~a" (getf config :p2p-min-port 50000))
                           :width 80
                           :activate-callback
                           (lambda (gadget value)
                             (declare (ignore gadget))
                             (let ((val (parse-integer (or value "50000") :junk-allowed t)))
                               (setf (getf (frame-config frame) :p2p-min-port) (max 1024 (min 65535 val)))
                               (save-preferences-now frame))))))
   (clim:stream-set-cursor-position pane 120 550)
   (format pane "Max:")
   (unless (p2p-max-port-field frame)
     (setf (p2p-max-port-field frame)
           (clim:make-pane 'clim:text-field
                           :value (format nil "~a" (getf config :p2p-max-port 60000))
                           :width 80
                           :activate-callback
                           (lambda (gadget value)
                             (declare (ignore gadget))
                             (let ((val (parse-integer (or value "60000") :junk-allowed t)))
                               (setf (getf (frame-config frame) :p2p-max-port) (max (getf (frame-config frame) :p2p-min-port) (min 65535 val)))
                               (save-preferences-now frame))))))
   (clim:stream-set-cursor-position pane 20 580)
   (format pane "Public Key Algorithm:")
   (unless (p2p-pubkey-algo-field frame)
     (setf (p2p-pubkey-algo-field frame)
           (clim:make-pane 'clim:option-pane
                           :items '("ed25519" "rsa4096" "ecdsa")
                           :value (getf config :p2p-pubkey-algo "ed25519")
                           :value-changed-callback
                           (lambda (g v)
                             (declare (ignore g))
                             (setf (getf (frame-config frame) :p2p-pubkey-algo) v)
                             (save-preferences-now frame)))))
   (clim:stream-set-cursor-position pane 20 610)
   (format pane "Auto-start P2P on startup:")
   (unless (p2p-auto-start-checkbox frame)
     (setf (p2p-auto-start-checkbox frame)
           (clim:make-pane 'clim:check-box
                           :label "Start P2P service when application launches"
                           :value (getf config :p2p-advertise-enabled nil)
                           :value-changed-callback
                           (lambda (g v)
                             (declare (ignore g))
                             (setf (getf (frame-config frame) :p2p-advertise-enabled) v)
                             (save-preferences-now frame)
                             (clim:redisplay-frame-panes frame :force-p t)))))

   ;; Version Control
   (clim:stream-set-cursor-position pane 10 674)
   (clim:with-text-face (pane :bold)
     (format pane "Version Control"))
  (format pane "~%  VC System:")
  (unless (vc-system-box frame)
    (let* ((current-vc (getf config :vc-system :git))
           (box (clim:make-pane 'clim:radio-box)))
      (setf (vc-system-box frame) box)
      (setf (vc-system-radio frame)
            (loop for (vc-key vc-label) in '((:git "Git") (:svn "Subversion") (:bzr "Bazaar")
                                              (:hg "Mercurial") (:cvs "Concurrent") (:rcs "Revision Control"))
                  collect (clim:make-pane 'clim:toggle-button
                                          :label vc-label
                                          :value (eq current-vc vc-key)
                                          :group box
                                          :value-changed-callback
                                          (lambda (g v)
                                            (declare (ignore g))
                                            (when v
                                              (setf (getf (frame-config frame) :vc-system) vc-key)
                                              (save-preferences-now frame)
                                              (clim:redisplay-frame-panes frame :force-p t))))))))

  ;; Git-specific sub-settings
  (when (eq (getf config :vc-system) :git)
    (clim:stream-set-cursor-position pane 10 440)
    (clim:with-text-face (pane :bold)
      (format pane "Git"))

    ;; User Name
    (clim:stream-set-cursor-position pane 20 470)
    (format pane "User Name:")
    (unless (git-user-name-field frame)
      (setf (git-user-name-field frame)
            (clim:make-pane 'clim:text-field
                            :value (getf config :git-user-name "")
                            :width 300
                            :activate-callback
                            (lambda (g v)
                              (declare (ignore g))
                              (setf (getf (frame-config frame) :git-user-name) v)
                              (save-preferences-now frame)))))
    ;; User Email
    (clim:stream-set-cursor-position pane 20 500)
    (format pane "eMail:")
    (unless (git-user-email-field frame)
      (setf (git-user-email-field frame)
            (clim:make-pane 'clim:text-field
                            :value (getf config :git-user-email "")
                            :width 300
                            :activate-callback
                            (lambda (g v)
                              (declare (ignore g))
                              (setf (getf (frame-config frame) :git-user-email) v)
                              (save-preferences-now frame)))))
    ;; Signing key
    (clim:stream-set-cursor-position pane 20 530)
    (format pane "Signing key:")
    (unless (git-signing-key-field frame)
      (let* ((current-key (getf config :git-signing-key ""))
             (key-items (list current-key "---" "Generate and publish a new key...")))
        (setf (git-signing-key-field frame)
              (clim:make-pane 'clim:option-pane
                              :items key-items
                              :value current-key
                              :value-changed-callback
                              (lambda (g v)
                                (declare (ignore g))
                                (if (string= v "Generate and publish a new key...")
                                    (format *trace-output* "~&[Preferences] Generate key requested~%")
                                    (progn
                                      (setf (getf (frame-config frame) :git-signing-key) v)
                                      (save-preferences-now frame))))))))
    ;; Sign commits/tags
    (clim:stream-set-cursor-position pane 20 560)
    (unless (git-sign-commits-checkbox frame)
      (setf (git-sign-commits-checkbox frame)
            (clim:make-pane 'clim:check-box
                            :label "Sign Commits"
                            :value (getf config :git-sign-commits nil)
                            :value-changed-callback
                            (lambda (g v)
                              (declare (ignore g))
                              (setf (getf (frame-config frame) :git-sign-commits) v)
                              (save-preferences-now frame)))))
    (clim:stream-set-cursor-position pane 200 560)
    (unless (git-sign-tags-checkbox frame)
      (setf (git-sign-tags-checkbox frame)
            (clim:make-pane 'clim:check-box
                            :label "Sign Tags"
                            :value (getf config :git-sign-tags nil)
                            :value-changed-callback
                            (lambda (g v)
                              (declare (ignore g))
                              (setf (getf (frame-config frame) :git-sign-tags) v)
                              (save-preferences-now frame)))))

    ;; Merge/Diff tools
    (clim:stream-set-cursor-position pane 20 600)
    (format pane "Merge tool:")
    (unless (git-merge-tool-field frame)
      (setf (git-merge-tool-field frame)
            (clim:make-pane 'clim:text-field
                            :value (getf config :git-merge-tool "meld")
                            :width 200
                            :activate-callback
                            (lambda (g v)
                              (declare (ignore g))
                              (setf (getf (frame-config frame) :git-merge-tool) v)
                              (save-preferences-now frame)))))
    (clim:stream-set-cursor-position pane 320 600)
    (unless (git-merge-prompt-checkbox frame)
      (setf (git-merge-prompt-checkbox frame)
            (clim:make-pane 'clim:check-box
                            :label "Prompt first"
                            :value (getf config :git-merge-prompt t)
                            :value-changed-callback
                            (lambda (g v)
                              (declare (ignore g))
                              (setf (getf (frame-config frame) :git-merge-prompt) v)
                              (save-preferences-now frame)))))
    (clim:stream-set-cursor-position pane 20 630)
    (format pane "Diff tool:")
    (unless (git-diff-tool-field frame)
      (setf (git-diff-tool-field frame)
            (clim:make-pane 'clim:text-field
                            :value (getf config :git-diff-tool "meld")
                            :width 200
                            :activate-callback
                            (lambda (g v)
                              (declare (ignore g))
                              (setf (getf (frame-config frame) :git-diff-tool) v)
                              (save-preferences-now frame)))))
    (clim:stream-set-cursor-position pane 320 630)
    (unless (git-diff-prompt-checkbox frame)
      (setf (git-diff-prompt-checkbox frame)
            (clim:make-pane 'clim:check-box
                            :label "Prompt first"
                            :value (getf config :git-diff-prompt t)
                            :value-changed-callback
                            (lambda (g v)
                              (declare (ignore g))
                              (setf (getf (frame-config frame) :git-diff-prompt) v)
                              (save-preferences-now frame)))))

    ;; Remotes table (display-only list of stored remotes)
    (clim:stream-set-cursor-position pane 20 670)
    (clim:with-text-face (pane :bold)
      (format pane "Remotes"))
    (clim:stream-set-cursor-position pane 20 690)
    (format pane "~&  ~10a  ~-40s  ~-40s" "Remote" "URL" "Fetch")
    (clim:stream-set-cursor-position pane 20 710)
    (clim:draw-line* pane 20 710 550 710 :ink (clim:make-rgb-color 0.5 0.5 0.5))
    (let ((remotes (getf config :git-remotes '("origin" "git@github.com:brpocock/Phantasia" "+refs/heads/*:refs/remotes/origin/*"))))
      (loop for (name url fetch) in remotes
            for row from 0
            do (clim:stream-set-cursor-position pane 20 (+ 720 (* row 20)))
               (format pane "  ~10a  ~-40s  ~-40s" name url fetch)))

    ;; Submodules
    (clim:stream-set-cursor-position pane 20 770)
    (clim:with-text-face (pane :bold)
      (format pane "Submodules"))
    (unless (git-submodule-skyline-checkbox frame)
      (setf (git-submodule-skyline-checkbox frame)
            (clim:make-pane 'clim:check-box
                            :label "Skyline-Tool and Eightbol"
                            :value (getf (getf config :git-submodules) :skyline-tool t)
                            :value-changed-callback
                            (lambda (g v)
                              (declare (ignore g))
                              (push (list :skyline-tool v) (getf (frame-config frame) :git-submodules))
                              (save-preferences-now frame)))))
    (clim:stream-set-cursor-position pane 20 800)
    (unless (git-submodule-a7800-checkbox frame)
      (setf (git-submodule-a7800-checkbox frame)
            (clim:make-pane 'clim:check-box
                            :label "Atari 7800 Tools"
                            :value (getf (getf config :git-submodules) :a7800-tools nil)
                            :value-changed-callback
                            (lambda (g v)
                              (declare (ignore g))
                              (push (list :a7800-tools v) (getf (frame-config frame) :git-submodules))
                              (save-preferences-now frame)))))
    (clim:stream-set-cursor-position pane 20 830)
    (unless (git-submodule-intellivision-checkbox frame)
      (setf (git-submodule-intellivision-checkbox frame)
            (clim:make-pane 'clim:check-box
                            :label "Intellivision Tools"
                            :value (getf (getf config :git-submodules) :intellivision-tools nil)
                            :value-changed-callback
                            (lambda (g v)
                              (declare (ignore g))
                              (push (list :intellivision-tools v) (getf (frame-config frame) :git-submodules))
                              (save-preferences-now frame)))))

    ;; Push/pull settings
    (clim:stream-set-cursor-position pane 20 870)
    (clim:with-text-face (pane :bold)
      (format pane "Pushing"))
    (unless (git-push-auto-setup-checkbox frame)
      (setf (git-push-auto-setup-checkbox frame)
            (clim:make-pane 'clim:check-box
                            :label "When pushing, automatically set up new branches on remote"
                            :value (getf config :git-push-auto-setup nil)
                            :value-changed-callback
                            (lambda (g v)
                              (declare (ignore g))
                              (setf (getf (frame-config frame) :git-push-auto-setup) v)
                              (save-preferences-now frame)))))
    (clim:stream-set-cursor-position pane 20 910)
    (format pane "When pulling, automatically")
    (unless (git-pull-behavior-combo frame)
      (setf (git-pull-behavior-combo frame)
            (clim:make-pane 'clim:option-pane
                            :items '("fast-forward only" "merge" "rebase")
                            :value (getf config :git-pull-behavior "fast-forward only")
                            :value-changed-callback
                            (lambda (g v)
                              (declare (ignore g))
                              (setf (getf (frame-config frame) :git-pull-behavior) v)
                              (save-preferences-now frame)))))
    ;; Default branch
    (clim:stream-set-cursor-position pane 20 940)
    (format pane "Default Branch:")
    (unless (git-default-branch-field frame)
      (setf (git-default-branch-field frame)
            (clim:make-pane 'clim:text-field
                            :value (getf config :git-default-branch "main")
                            :width 150
                            :activate-callback
                            (lambda (g v)
                              (declare (ignore g))
                              (setf (getf (frame-config frame) :git-default-branch) v)
                              (save-preferences-now frame))))))

  ;; Issue Tracker
  (clim:stream-set-cursor-position pane 10 (+ (if (eq (getf config :vc-system) :git) 980 454)))
  (clim:with-text-face (pane :bold)
    (format pane "Issue Tracking"))
  (format pane "~%  Issue tracker kind:")
  (unless (issue-tracker-box frame)
    (let* ((current-tracker (getf config :issue-tracker :github))
           (box (clim:make-pane 'clim:radio-box)))
      (setf (issue-tracker-box frame) box)
      (setf (issue-tracker-radio frame)
            (loop for (tracker-key tracker-label) in '((:github "GitHub") (:gitlab "GitLab") (:bugzilla "Bugzilla"))
                  collect (clim:make-pane 'clim:toggle-button
                                          :label tracker-label
                                          :value (eq current-tracker tracker-key)
                                          :group box
                                          :value-changed-callback
                                          (lambda (g v)
                                            (declare (ignore g))
                                            (when v
                                              (setf (getf (frame-config frame) :issue-tracker) tracker-key)
                                              (save-preferences-now frame)
                                              (clim:redisplay-frame-panes frame :force-p t))))))))
  ;; Tracker URL
  (clim:stream-set-cursor-position pane 10 (+ (if (eq (getf config :vc-system) :git) 1040 514)))
  (format pane "Tracker URL:")
  (unless (tracker-url-field frame)
    (setf (tracker-url-field frame)
          (clim:make-pane 'clim:text-field
                          :value (getf config :tracker-url "https://github.com/adventuring/phantasia")
                          :width 500
                          :activate-callback
                          (lambda (g v)
                            (declare (ignore g))
                            (setf (getf (frame-config frame) :tracker-url) v)
                            (save-preferences-now frame)))))
  ;; Sign-in button
  (unless (sign-in-button frame)
    (setf (sign-in-button frame)
          (clim:make-pane 'clim:push-button
                          :label "Sign in..."
                          :activate-callback
                          (lambda (g)
                            (declare (ignore g))
                            (format *trace-output* "~&[Preferences] Sign-in requested for ~a at ~a~%"
                                    (getf (frame-config frame) :issue-tracker)
                                    (getf (frame-config frame) :tracker-url))))))
  (clim:stream-set-cursor-position pane 10 (+ (if (eq (getf config :vc-system) :git) 1070 544)))
  ;; SSH Key
  (clim:stream-set-cursor-position pane 10 (+ (if (eq (getf config :vc-system) :git) 1100 574)))
  (format pane "SSH Key Path:")
  (unless (ssh-key-field frame)
    (setf (ssh-key-field frame)
          (clim:make-pane 'clim:text-field
                          :value (getf config :ssh-key-path "")
                          :width 400
                          :activate-callback
                          (lambda (gadget value)
                            (declare (ignore gadget))
                            (setf (getf (frame-config frame) :ssh-key-path) value)
                            (save-preferences-now frame)))))

  ;; GPG Key
  (clim:stream-set-cursor-position pane 10 (+ (if (eq (getf config :vc-system) :git) 1160 634)))
  (format pane "GPG Key Path:")
  (unless (gpg-key-field frame)
    (setf (gpg-key-field frame)
          (clim:make-pane 'clim:text-field
                          :value (getf config :gpg-key-path "")
                          :width 400
                          :activate-callback
                          (lambda (gadget value)
                            (declare (ignore gadget))
                            (setf (getf (frame-config frame) :gpg-key-path) value)
                            (save-preferences-now frame))))))

(defun position-preference-gadgets (frame pane)
  (clim:stream-set-cursor-position pane 10 30)
  (clim:note-gadget-activated (paper-size-box frame) pane)
  (when (eq (getf (frame-config frame) :paper-size) :custom)
    (clim:stream-set-cursor-position pane 10 90)
    (clim:note-gadget-activated (paper-unit-box frame) pane)
    (clim:stream-set-cursor-position pane 70 120)
    (clim:note-gadget-activated (paper-width-field frame) pane)
    (clim:stream-set-cursor-position pane 70 150)
    (clim:note-gadget-activated (paper-height-field frame) pane))
   (clim:stream-set-cursor-position pane 10 220)
   (clim:note-gadget-activated (lan-sharing-checkbox frame) pane)
   (clim:stream-set-cursor-position pane 10 260)
   (clim:note-gadget-activated (dist-sharing-checkbox frame) pane)
   (clim:stream-set-cursor-position pane 10 300)
   (clim:note-gadget-activated (music-sharing-checkbox frame) pane)
   ;; P2P Sharing
   (clim:stream-set-cursor-position pane 10 370)
   (clim:note-gadget-activated (p2p-enabled-checkbox frame) pane)
   (clim:stream-set-cursor-position pane 30 370)
   (clim:note-gadget-activated (p2p-user-field frame) pane)
   (clim:stream-set-cursor-position pane 30 400)
   (clim:note-gadget-activated (p2p-domain-field frame) pane)
   (clim:stream-set-cursor-position pane 30 430)
   (clim:note-gadget-activated (p2p-interface-field frame) pane)
   (clim:stream-set-cursor-position pane 30 460)
   (clim:note-gadget-activated (p2p-advertise-interval-field frame) pane)
   (clim:stream-set-cursor-position pane 30 490)
   (clim:note-gadget-activated (p2p-discovery-interval-field frame) pane)
   (clim:stream-set-cursor-position pane 30 520)
   (clim:note-gadget-activated (p2p-min-port-field frame) pane)
   (clim:stream-set-cursor-position pane 120 520)
   (clim:note-gadget-activated (p2p-max-port-field frame) pane)
   (clim:stream-set-cursor-position pane 30 580)
   (clim:note-gadget-activated (p2p-pubkey-algo-field frame) pane)
   (clim:stream-set-cursor-position pane 30 610)
   (clim:note-gadget-activated (p2p-auto-start-checkbox frame) pane)
   ;; Version Control
   (clim:stream-set-cursor-position pane 10 380)
   (clim:note-gadget-activated (vc-system-box frame) pane)
  (let ((config (frame-config frame)))
    (when (eq (getf config :vc-system) :git)
      (clim:stream-set-cursor-position pane 140 470)
      (clim:note-gadget-activated (git-user-name-field frame) pane)
      (clim:stream-set-cursor-position pane 120 500)
      (clim:note-gadget-activated (git-user-email-field frame) pane)
      (clim:stream-set-cursor-position pane 140 530)
      (clim:note-gadget-activated (git-signing-key-field frame) pane)
      (clim:stream-set-cursor-position pane 30 560)
      (clim:note-gadget-activated (git-sign-commits-checkbox frame) pane)
      (clim:stream-set-cursor-position pane 210 560)
      (clim:note-gadget-activated (git-sign-tags-checkbox frame) pane)
      (clim:stream-set-cursor-position pane 140 600)
      (clim:note-gadget-activated (git-merge-tool-field frame) pane)
      (clim:stream-set-cursor-position pane 320 600)
      (clim:note-gadget-activated (git-merge-prompt-checkbox frame) pane)
      (clim:stream-set-cursor-position pane 140 630)
      (clim:note-gadget-activated (git-diff-tool-field frame) pane)
      (clim:stream-set-cursor-position pane 320 630)
      (clim:note-gadget-activated (git-diff-prompt-checkbox frame) pane)
      (clim:stream-set-cursor-position pane 30 800)
      (clim:note-gadget-activated (git-submodule-skyline-checkbox frame) pane)
      (clim:stream-set-cursor-position pane 30 830)
      (clim:note-gadget-activated (git-submodule-a7800-checkbox frame) pane)
      (clim:stream-set-cursor-position pane 30 860)
      (clim:note-gadget-activated (git-submodule-intellivision-checkbox frame) pane)
      (clim:stream-set-cursor-position pane 30 900)
      (clim:note-gadget-activated (git-push-auto-setup-checkbox frame) pane)
      (clim:stream-set-cursor-position pane 220 910)
      (clim:note-gadget-activated (git-pull-behavior-combo frame) pane)
      (clim:stream-set-cursor-position pane 160 940)
      (clim:note-gadget-activated (git-default-branch-field frame) pane))
    ;; Issue tracker
    (let ((offset (if (eq (getf config :vc-system) :git) 980 454)))
      (clim:stream-set-cursor-position pane 10 (+ offset 30))
      (clim:note-gadget-activated (issue-tracker-box frame) pane)
      (clim:stream-set-cursor-position pane 150 (+ offset 90))
      (clim:note-gadget-activated (tracker-url-field frame) pane))
    ;; Sign-in button
    (let ((offset (if (eq (getf config :vc-system) :git) 1070 544)))
      (clim:stream-set-cursor-position pane 10 offset)
      (clim:note-gadget-activated (sign-in-button frame) pane))
    ;; SSH/GPG
    (let ((offset (if (eq (getf config :vc-system) :git) 1100 574)))
      (clim:stream-set-cursor-position pane 150 offset)
      (clim:note-gadget-activated (ssh-key-field frame) pane))
    (let ((offset (if (eq (getf config :vc-system) :git) 1160 634)))
      (clim:stream-set-cursor-position pane 150 offset)
      (clim:note-gadget-activated (gpg-key-field frame) pane))))

(defun display-prefs-status (frame pane)
  (clim:stream-set-cursor-position pane 10 8)
  (format pane "Preferences ~a"
          (if (frame-dirty frame) "(unsaved)" "")))

;;  I/O

(defun load-preferences-config ()
  (let ((path (prefs-pathname)))
    (when (probe-file path)
      (with-open-file (f path)
        (setf *preferences-config* (read f nil nil))))))

(defun write-preferences-config (frame)
  (let* ((path (prefs-pathname))
         (dir (make-pathname :defaults path :name nil :type nil)))
    (ensure-directories-exist dir)
    (with-open-file (f path :direction :output :if-exists :supersede)
      (print (frame-config frame) f))))
