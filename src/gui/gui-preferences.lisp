;; Skyline-Tool src/gui/gui-preferences.lisp
;; Copyright © 2026 Interworldly Adventuring, LLC

;; Preferences Inspector - Complete CLIM tabbed interface for development preferences

#|
Preferences Inspector documentation: Requirements. NO MODIFICATIONS permitted. ;

Tab Bar
/ Accessibility \ / Printing \ / Network \ / Version Control \ / Issue Tracking \

Printing Tab
-------------

Paper Size: <> U.S. Letter              ;
            <> U.S. Legal                           ;
            <> A4                                   ;
            <> Custom                               ;
               ____ [ mm - ] × ____ [ mm - ]
                    [ cm   ]        [ cm   ]                     ;
                    [ pt   ]        [ pt   ]
                    [ pc   ]        [ pc   ]
                    [ in   ]        [ in   ]
                                        
Network Tab                             ;
------------
[] LAN Resource Sharing                 ;
[] Share "Dist/" Folder                 ;
[] Share Music as Media Server          ;
                                        ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
Version Control Tab                     ;
Versioning                              ;
-----------                             ;
Version Control System: <> Git | git    ;
<> Subversion | svn                     ;
<> Bazaar | bzr                         ;
<> Mercurial | hg                       ;
<> Concurrent | cvs                     ;
<> Revision Control | rcs               ;
                                        ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
Git # Specific to the version control system in use ;
----                                    ;
User                                    ;
-----                                   ;
Name: ________________________          ;
eMail: ________________________         ;
Signing key:  Bruce-Robert Pocock <brpocock@interworldly.com>    - ;
---                                     ;
Generate and publish a new key...       ;
[] Sign Commits                         ;
[] Sign Tags                            ;
Tools                                   ;
------                                  ;
Merge:  meld -   [] Prompt first        ;
Diff:   meld -   [] Prompt first        ;
##( git difftool --tool-help first section of output only ) ;
##( "may be set to one of the following:" options only ) ;
##( do not list "valid, but not currently available" list ) ;
Remotes                                 ;
--------                                ;
| Remote        | URL                               | Fetch                               | ;
|-----------------------------------------------------------------------------------------| ;
| origin        | git@github.com:brpocock/Phantasia | +refs/heads/:refs/remotes/origin/ | ;
( + )                                   ;
Submodules                              ;
-----------                             ;
[] Skyline-Tool and Eightbol            ;
[] Atari 7800 Tools                     ;
[] Intellivision Tools                  ;
Pushing                                 ;
--------                                ;
[] When pushing, automatically set up new branches on remote ;

[] When pulling, automatically  fast-forward only   - ;

Default Branch: ___________                                              # main default ;
                                        ; ; ; ; ; ; ; ; ; ; ; ; ; ;
Issue Tracking Tab                      ;
Issue Tracking                          ;
---------------                         ;
Issue tracker kind: <> GitHub           ;
<> GitLab                               ;
<> Bugzilla                             ;
Tracker URL: ____________________________________________________ ;
( Sign in... )                          ;
|#

;; Preferences Inspector:
;; 
;; #### menu
;; 
;; Preferences
;; ------------ 
;; Reset to Defaults... 
;; --- 
;; Save as >> 
;; Reset to Defaults... 
;; --- 
;; Save as > Text... 
;;            JSON... 
;;            PDF... 
;; Send to > { p2p recipient list } 
;; Save as > Text... 
;;            JSON... 
;;            PDF... 
;; Send to > { p2p recipient list } 
;; Print to > { printer list } 
;; --- 
;; Close
;; 
;; Edit
;; -----
;; Cut
;; Copy
;; Paste
;; ---
;; Find...
;; 
;; View
;; -----
;; [] Editable
;; ---
;; [] Project Pane
;; 
;; Help
;; -----
;; How to Manage Preferences...
;; Skyline-Tool Developers' Guide...
;; Skyline-Tool Scripting Guide...
;; ---
;; About Skyline-Tool...
;; 
;; ### Window Contents
;; 
;; ### Tab Bar
;; 
;; / Accessibility \ / Printing \ /
;; 
;; ### Printing Tab
;; 
;; Printing
;; ---------
;; 
;; Paper Size: <> U.S. Letter
;; <> U.S. Legal
;; <> A4
;; <> Custom
;; ____ [ mm - ] × ____ [ mm - ]
;; [ cm   ]        [ cm   ]
;; [ in   ]        [ in   ]
;; 
;; ### Network Tab
;; 
;; Network
;; --------
;; 
;; [] LAN Resource Sharing
;; [] Share "Dist/" Folder
;; [] Share Music as Media Server
;; 
;; ### Version Control Tab
;; 
;; Versioning
;; -----------
;; 
;; Version Control System: <> Git                   | git
;; <> Subversion            | svn
;; <> Bazaar                | bzr
;; <> Mercurial             | hg
;; <> Concurrent            | cvs
;; <> Revision Control      | rcs
;; 
;; Git                     # Specific to the version control system in use
;; ----
;; 
;; User
;; -----
;; Name: ________________________
;; eMail: ________________________
;; 
;; Signing key: [ Bruce-Robert Pocock <brpocock@interworldly.com>    - ]
;; [ ---                                                  ]
;; [ Generate and publish a new key...                    ]
;; [] Sign Commits
;; [] Sign Tags
;; 
;; Tools
;; ------
;; Merge: [ meld - ]  [] Prompt first
;; Diff:  [ meld - ]  [] Prompt first
;; ##( git difftool --tool-help first section of output only )
;; ##( "may be set to one of the following:" options only )
;; ##( do not list "valid, but not currently available" list )
;; 
;; Remotes
;; --------
;; 
;; | Remote        | URL                               | Fetch                               |
;; |-----------------------------------------------------------------------------------------|
;; | origin        | git@github.com:brpocock/Phantasia | +refs/heads/*:refs/remotes/origin/* |
;; 
;; ( + )
;; 
;; Submodules
;; -----------
;; [] Skyline-Tool and Eightbol
;; 
;; [] Atari 7800 Tools
;; [] Intellivision Tools
;; 
;; Pushing
;; --------
;; [] When pushing, automatically set up new branches on remote
;; 
;; When pulling, automatically [ fast-forward only   - ]
;; 
;; Default Branch: ___________                                              # main default
;; 
;; ### Issue Tracking Tab
;; 
;; Issue Tracking
;; ---------------
;; 
;; Issue tracker kind: <> GitHub
;; <> GitLab
;; <> Bugzilla
;; 
;; Tracker URL: ____________________________________________________
;; 
;; ( Sign in... )
;; 
;; |#
(in-package :skyline-tool)

;;;; Constants
(define-constant +paper-sizes+
    '((:us-letter "U.S. Letter" 215.9 279.4)
      (:us-legal "U.S. Legal" 215.9 355.6)
      (:a4 "A4" 210.0 297.0))
  :test 'equalp
  :documentation "Paper size options: (key label width-mm height-mm).")

;;;; Utility functions
(defun convert-unit (value from-unit to-unit)
  "Convert VALUE from FROM-UNIT to TO-UNIT.
   Units: :mm, :cm, :in, :pt (points = 1/72 inch)."
  (let ((mm-per-unit
           (case from-unit
             (:mm 1)
             (:cm 10)
             (:in 25.4)
             (:pt (/ 25.4 72))
             (t 1)))
        (units-per-mm
          (case to-unit
            (:mm 1)
            (:cm (/ 1 10))
            (:in (/ 1 25.4))
            (:pt (/ 72 25.4))
            (t 1))))
    (* value mm-per-unit units-per-mm)))

(defun default-preferences ()
  "Return a plist of default preferences."
  `(:paper-size :us-letter
    :paper-width 215.9
    :paper-height 279.4
    :units :mm
    :lan-sharing-enabled nil
    :dist-sharing-enabled nil
    :music-sharing-enabled nil
    :version-control-system "git"
    :ssh-key ""
    :gpg-key ""
    :git-user ""
    :git-email ""
    :git-signing-key ""
    :git-sign-commits nil
    :git-sign-tags nil
    :git-merge-tool "meld"
    :git-merge-prompt nil
    :git-diff-tool "meld"
    :git-diff-prompt nil
    :git-push-auto-setup nil
    :git-pull-behavior "fast-forward"
    :git-default-branch "main"
    :git-submodule-skyline nil
    :git-submodule-a7800 nil
    :git-submodule-intellivision nil
    :issue-tracker-kind "github"
    :tracker-url ""
    :p2p-enabled nil
    :p2p-user ""
    :p2p-domain "local."
    :p2p-interface "eth0"
    :p2p-advertise-interval 30
    :p2p-discovery-interval 60
    :p2p-min-port 50000
    :p2p-max-port 60000
    :p2p-pubkey-algo "ed25519"
    :p2p-auto-start nil))

;;;; Frame definition
(clim:define-application-frame preferences-inspector-frame (clim:standard-application-frame)
  ((dirty :initform nil :accessor frame-dirty)
   (current-tab :initform :accessibility :accessor frame-current-tab)
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
   ;; VERSION-CONTROL section
   (version-control-system-box :initform nil :accessor version-control-system-box)
   (version-control-system-radio :initform nil :accessor version-control-system-radio)
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
   ;; Accessibility section
   (accessibility-theme-box :initform nil :accessor accessibility-theme-box)
   (accessibility-theme-radio :initform nil :accessor accessibility-theme-radio)
   (accessibility-shortcut-box :initform nil :accessor accessibility-shortcut-box)
   (accessibility-shortcut-radio :initform nil :accessor accessibility-shortcut-radio)
   (accessibility-latitude-field :initform nil :accessor accessibility-latitude-field)
   (accessibility-longitude-field :initform nil :accessor accessibility-longitude-field)
   (watcher-thread :initform nil :accessor prefs-watcher-thread))
  (:menu-bar preferences-inspector-menu-bar)
  (:icon (skyline-tool-icon :resource :preferences))
  (:pretty-name "Preferences")
  (:panes
   (tab-bar :application :display-function 'display-tab-bar
                         :height 30 :width 400
                         :scroll-bars nil)
   (main-pane :application
              :display-function 'display-preferences
              :height 700 :width 400
              :scroll-bars :vertical)
   (find-bar :application :display-function 'display-find-bar
                          :height 30 :width 400
                          :scroll-bars nil)
   (project-bar :application :display-function 'display-project-bar
                             :height 30 :width 400
                             :scroll-bars nil))
  (:layouts
   (default (clim:vertically () tab-bar main-pane))
   (searching (clim:vertically () tab-bar main-pane find-bar))
   (searching+project (clim:vertically () tab-bar main-pane find-bar project-bar))
   (project (clim:vertically () tab-bar main-pane project-bar))))

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
               (inotify:with-inotify (inot (list (list prefs-path inotify:in-modify)))
                 (loop for ev = (inotify:read-events inot)
                       do (clim:redisplay-frame-panes frame :force-p t)))))))
  ;; Populate dynamic menus
  (populate-print-menu 'preferences-print-to-menu)
  ;; Subscribe to external preference-change events so other windows stay in sync
  (skyline-tool::subscribe :preference-change
              (lambda (event)
                (declare (ignore event))
                (clim:redisplay-frame-panes frame :force-p t)))))

(defun open-preferences-inspector ()
  (let ((frame (clim:make-application-frame
                 'preferences-inspector-frame)))
    (bt:make-thread (lambda ()
                      (clim:run-frame-top-level frame))
                    :name (format nil "Preferences Inspector: ~a (~a)"
                                  (title-case (if (boundp '*game-title*) *game-title* "Game"))
                                  (machine-directory-name)))))

;;;; Menus
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

;;;; Commands
(clim:define-command (com-preferences-reset :command-table clim-internals::global-command-table
                                             :menu t :name t) ()
  (let ((confirmed (prompt-for-confirmation "Reset all preferences to defaults?")))
    (when confirmed
      (setf *prefs-cache* (default-preferences))
      (setf (frame-dirty clim:*application-frame*) t)
      (save-preferences-now clim:*application-frame*)
      (clim:redisplay-frame-panes clim:*application-frame* :force-p t))))

(clim:define-command (com-close-frame :command-table clim-internals::global-command-table
                                       :menu t :name t) ()
  (clim:frame-exit clim:*application-frame*))

(clim:define-command (com-preferences-save-text :command-table clim-internals::global-command-table
                                                :menu t :name t) ()
  (let* ((path (prompt-save-pathname "Preferences.txt" (list :dir :preferences :text))))
    (when path
      (with-open-file (s path :direction :output :if-exists :supersede)
        (format s "Preferences — ~a (~a)~%"
                (title-case (if (boundp '*game-title*) *game-title* "Game"))
                (machine-directory-name))
        (labels ((recurse-keys (set &optional (indent 0))
                   (loop for (k v) on set by #'cddr
                         do (if (consp v)
                                (progn
                                  (format s "~vt ~s: ..." indent k)
                                  (recurse-keys v (+ 2 indent)))
                                (format s "~vt  ~s: ~s~%" indent k v)))))
          (recurse-keys *prefs-cache*))))))

(clim:define-command (com-preferences-save-json :command-table clim-internals::global-command-table
                                                :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (path (prompt-save-pathname "Preferences.json" '(:dir :preferences :json))))
    (when path
      (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8)
        (json:encode-json
         (loop for (k v) on *prefs-cache* by #'cddr
               collect (cons (string-downcase (symbol-name k)) v))
         s)))))

(clim:define-command (com-preferences-save-pdf :command-table clim-internals::global-command-table
                                               :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (path (prompt-save-pathname "preferences.ps" :default-name "preferences.ps")))
    (when path
      (with-open-file (s path :direction :output :if-exists :supersede)
        (write-ps-header-bar s "Preferences" (format-timestring nil (get-universal-time))
                             (user-homedir-pathname)
                             (title-case (if (boundp '*game-title*) *game-title* "Game")))
        (loop for (k v) on *prefs-cache* by #'cddr
              do (format s "(~s: ~s) showpage show~%" k v))
        (let ((pdf-path (make-pathname :type "pdf" :defaults path)))
          (uiop:run-program (list "ps2pdf" (namestring path) (namestring pdf-path))
                            :output nil))))))

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
      (uiop:run-program (list "xdg-open" (namestring guide-path)) :output nil))))

(clim:define-command (com-open-scripting-guide :command-table clim-internals::global-command-table
                                                :menu t :name t) ()
   (let ((guide-path (asdf:system-relative-pathname :skyline-tool
                                                    "../Source/Documentation/FountainScripting.md")))
     (when (probe-file guide-path)
       (uiop:run-program (list "xdg-open" (namestring guide-path)) :output nil))))

;;;; Tab switching commands
(clim:define-command (com-switch-to-accessibility :command-table clim-internals::global-command-table
                                                 :menu t :name t)
  ()
  (let ((frame clim:*application-frame*))
    (when frame
      (setf (frame-current-tab frame) :accessibility)
      (clim:redisplay-frame-panes frame :force-p t))))

(clim:define-command (com-switch-to-printing :command-table clim-internals::global-command-table
                                             :menu t :name t)
  ()
  (let ((frame clim:*application-frame*))
    (when frame
      (setf (frame-current-tab frame) :printing)
      (clim:redisplay-frame-panes frame :force-p t))))

(clim:define-command (com-switch-to-network :command-table clim-internals::global-command-table
                                            :menu t :name t)
  ()
  (let ((frame clim:*application-frame*))
    (when frame
      (setf (frame-current-tab frame) :network)
      (clim:redisplay-frame-panes frame :force-p t))))

(clim:define-command (com-switch-to-version-control :command-table clim-internals::global-command-table
                                                    :menu t :name t)
  ()
  (let ((frame clim:*application-frame*))
    (when frame
      (setf (frame-current-tab frame) :version-control)
      (clim:redisplay-frame-panes frame :force-p t))))

(clim:define-command (com-switch-to-issue-tracking :command-table clim-internals::global-command-table
                                                   :menu t :name t)
  ()
  (let ((frame clim:*application-frame*))
    (when frame
      (setf (frame-current-tab frame) :issue-tracking)
      (clim:redisplay-frame-panes frame :force-p t))))

;;;; Presentation types for right-click context menus
(clim:define-presentation-type preferences-section ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'preferences-section)))
  (stringp object))

(clim:define-presentation-type preferences-gadget ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'preferences-gadget)))
  t)

(clim:define-presentation-type git-remote-item ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'git-remote-item)))
  (listp object))

;;;; Context menus for right-click on preferences sections and gadgets
(clim:define-command-table preferences-section-context-menu
  :menu (("Reset Section to Defaults" :command com-preferences-reset-section)
         ("How to Manage This Setting..." :command com-help-for-window)))

(clim:define-command-table preferences-gadget-context-menu
  :menu (("Copy Value" :command com-copy)
         ("Reset to Default" :command com-preferences-reset-section)
         ("How to Manage..." :command com-help-for-window)))

(clim:define-command (com-preferences-reset-section :command-table clim-internals::global-command-table
                                                    :menu t :name t) ()
  (setf *prefs-cache* (default-preferences))
  (save-preferences-now clim:*application-frame*)
  (clim:redisplay-frame-panes clim:*application-frame* :force-p t))

;;;; Display helpers
(defmacro make-section-header (stream title)
  "Render a section heading in a formatting-table row."
  `(clim:formatting-row (,stream)
     (clim:formatting-cell (,stream :align-x :left :min-width 760)
       (clim:with-text-size (,stream :larger)
         (clim:with-text-face (,stream :bold)
           (format ,stream "~a" ,title))))))

(defmacro make-label-value-row (stream label cell2)
  "Render a LABEL: VALUE row in a formatting-table."
  `(clim:formatting-row (,stream)
     (clim:formatting-cell (,stream :align-x :right :align-y :top
                                    :min-width 180)
       (format ,stream "~a:" ,label))
     (clim:formatting-cell (,stream :align-x :left :align-y :top
                                    :min-width 580)
       ,cell2)))

(defmacro make-full-width-row (stream cell)
  "Render a single full-width cell row (for checkboxes, etc.)."
  `(clim:formatting-row (,stream)
     (clim:formatting-cell (,stream :align-x :left :min-width 760)
       ,cell)))

(defmacro make-subheader-row (stream title)
  "Render a sub-section heading."
  `(clim:formatting-row (,stream)
     (clim:formatting-cell (,stream :align-x :left :min-width 760)
       (clim:with-text-face (,stream :bold)
         (format ,stream "~a" ,title)))))

;;;; Main display function - TABS IMPLEMENTATION
(defun display-preferences (frame pane)
  "Display the complete preferences interface using tabbed layout."
  
  ;; === Paper Size Tab ===
  
  (make-section-header pane "Paper Size")
  (let ((current-size (get-pref '(:paper :size) :us-letter)))
    (let ((box (clim:make-pane 'clim:radio-box)))
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
                                              (setf (get-pref '(:paper :size)) key)
                                              (when w
                                                (setf (get-pref '(:paper :width)) w)
                                                (setf (get-pref '(:paper :height)) h))
                                              (clim:redisplay-frame-panes frame :force-p t))))))
      (make-full-width-row pane (clim:note-gadget-activated box pane))
      
      ;; --- Paper Unit (only when Custom) ---
      (when (eq (get-pref '(:paper :size) :us-letter) :custom)
        (make-section-header pane "Paper Unit")
        (let* ((current-unit (get-pref '(:units :length) :mm))
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
                                                  (let ((old-unit (get-pref '(:units :length)))
                                                        (new-unit unit))
                                                    (setf (get-pref '(:units :length)) new-unit)
                                                    (let ((w (get-pref '(:paper :width)))
                                                          (h (get-pref '(:paper :height))))
                                                      (when (and w h)
                                                        (setf (get-pref '(:paper :width))
                                                              (round (convert-unit w old-unit new-unit) 1))
                                                        (setf (get-pref '(:paper :height))
                                                              (round (convert-unit h old-unit new-unit) 1)))))
                                                  (save-preferences-now frame)
                                                  (clim:redisplay-frame-panes frame :force-p t))))))
          (make-full-width-row pane (clim:note-gadget-activated box pane))

          (make-label-value-row pane "Width"
                                (unless (paper-width-field frame)
                                  (setf (paper-width-field frame)
                                        (clim:make-pane 'clim:text-field
                                                        :value (format nil "~a" (get-pref '(:paper :width) 215.9))
                                                        :width 80
                                                        :activate-callback
                                                        (lambda (gadget value)
                                                          (declare (ignore gadget))
                                                          (setf (get-pref '(:paper :width)) (parse-number value))
                                                          (save-preferences-now frame))))))
          (clim:note-gadget-activated (paper-width-field frame) pane)

          (make-label-value-row pane "Height"
                                (unless (paper-height-field frame)
                                  (setf (paper-height-field frame)
                                        (clim:make-pane 'clim:text-field
                                                        :value (format nil "~a" (get-pref '(:paper :height) 279.4))
                                                        :width 80
                                                        :activate-callback
                                                        (lambda (gadget value)
                                                          (declare (ignore gadget))
                                                          (setf (get-pref '(:paper :height)) (parse-number value))
                                                          (save-preferences-now frame))))))
          (clim:note-gadget-activated (paper-height-field frame) pane))

        ;; === Sharing Services Tab ===
        (make-section-header pane "Sharing Services")
        (unless (lan-sharing-checkbox frame)
          (setf (lan-sharing-checkbox frame)
                (clim:make-pane 'clim:check-box
                                :label "LAN Resource Sharing"
                                :value (get-pref '(:lan-sharing :enabled) nil)
                                :value-changed-callback
                                (lambda (g v)
                                  (declare (ignore g))
                                  (setf (get-pref '(:lan-sharing :enabled)) v)
                                  (save-preferences-now frame)
                                  (clim:redisplay-frame-panes frame :force-p t)))))
        (make-full-width-row pane (clim:note-gadget-activated (lan-sharing-checkbox frame) pane))
        (unless (dist-sharing-checkbox frame)
          (setf (dist-sharing-checkbox frame)
                (clim:make-pane 'clim:check-box
                                :label "Share Dist/ Folder"
                                :value (get-pref '(:dist-sharing :enabled) nil)
                                :value-changed-callback
                                (lambda (g v)
                                  (declare (ignore g))
                                  (setf (get-pref '(:dist-sharing :enabled)) v)
                                  (save-preferences-now frame)
                                  (clim:redisplay-frame-panes frame :force-p t)))))
        (make-full-width-row pane (clim:note-gadget-activated (dist-sharing-checkbox frame) pane))
        (unless (music-sharing-checkbox frame)
          (setf (music-sharing-checkbox frame)
                (clim:make-pane 'clim:check-box
                                :label "Share Music as Media Server"
                                :value (get-pref '(:music-sharing :enabled) nil)
                                :value-changed-callback
                                (lambda (g v)
                                  (declare (ignore g))
                                  (setf (get-pref '(:music-sharing :enabled)) v)
                                  (save-preferences-now frame)
                                  (clim:redisplay-frame-panes frame :force-p t)))))
        (make-full-width-row pane (clim:note-gadget-activated (music-sharing-checkbox frame) pane))

        ;; === P2P Sharing Tab === (User clarified: should be called "LAN Sharing")
        (make-section-header pane "LAN Sharing")
        (unless (p2p-enabled-checkbox frame)
          (setf (p2p-enabled-checkbox frame)
                (clim:make-pane 'clim:check-box
                                :label "Enable P2P Resource Sharing"
                                :value (get-pref '(:p2p :enabled) nil)
                                :value-changed-callback
                                (lambda (g v)
                                  (declare (ignore g))
                                  (setf (get-pref '(:p2p :enabled)) v)
                                  (save-preferences-now frame)
                                  (clim:redisplay-frame-panes frame :force-p t)))))
        (make-full-width-row pane (clim:note-gadget-activated (p2p-enabled-checkbox frame) pane))
        (make-label-value-row pane "Advertised User Name"
                              (unless (p2p-user-field frame)
                                (setf (p2p-user-field frame)
                                      (clim:make-pane 'clim:text-field
                                                      :value (or (get-pref '(:p2p :user) "") (user-full-name))
                                                      :width 300
                                                      :activate-callback
                                                      (lambda (gadget value)
                                                        (declare (ignore gadget))
                                                        (setf (get-pref '(:p2p :user)) value)
                                                        (save-preferences-now frame))))))
        (clim:note-gadget-activated (p2p-user-field frame) pane)
        (make-label-value-row pane "Service Domain"
                              (unless (p2p-domain-field frame)
                                (setf (p2p-domain-field frame)
                                      (clim:make-pane 'clim:text-field
                                                      :value (get-pref '(:p2p :domain) "local.")
                                                      :width 200
                                                      :activate-callback
                                                      (lambda (gadget value)
                                                        (declare (ignore gadget))
                                                        (setf (get-pref '(:p2p :domain)) value)
                                                        (save-preferences-now frame))))))
        (clim:note-gadget-activated (p2p-domain-field frame) pane)
        (make-label-value-row pane "Network Interface"
                              (unless (p2p-interface-field frame)
                                (setf (p2p-interface-field frame)
                                      (clim:make-pane 'clim:text-field
                                                      :value (get-pref '(:p2p :interface) "eth0")
                                                      :width 200
                                                      :activate-callback
                                                      (lambda (gadget value)
                                                        (declare (ignore gadget))
                                                        (error "truncated file")
                                                        ))))))))
  (error "truncated file"))
;; Fix this to point to actual preferences inspector
;; Currently it's a placeholder in src/gui/gui-preferences.lisp
;; The preferences inspector is in src/gui/gui-preferences.lisp
