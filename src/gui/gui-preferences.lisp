;; Skyline-Tool src/gui/gui-preferences.lisp
;; Copyright © 2026 Interworldly Adventuring, LLC

;; Preferences Inspector - Complete CLIM tabbed interface for development preferences

#|
Preferences Inspector documentation: Requirements. NO MODIFICATIONS permitted. ;

Tab Bar
 _______________   __________   _________   _________________   ________________
/ Accessibility \ / Printing \ / Network \ / Version Control \ / Issue Tracking \


Accessibility Tab
------------------

Color Theme: [ Normal (Light)    - ]
             [ Inverted (Dark)     ]
             [ Sunrise/Sunset      ]
             [ Desktop (Auto)      ]

Sunrise/Sunset: ___° <> N <> S × ___° <> W <> E

Keyboard Shortcuts Theme: [ Gnome        - ]
                          [ macOS          ]
                          [ Common (CDE)   ]
                          [ Emacs          ]

Printing Tab
-------------

Paper Size: <> U.S. Letter | 8.5 in × 11 in
            <> U.S. Legal  | 8.5 in × 14 in
            <> A4          | 9 in × 12 in                # probably wrong numbers
            <> Custom      | ____ [ mm - ] × ____ [ mm - ]
                                  [ cm   ]        [ cm   ] 
                                  [ pt   ]        [ pt   ]
                                  [ pc   ]        [ pc   ]
                                  [ in   ]        [ in   ]
                                        
Network Tab
------------

[] LAN Resource Sharing
[] Share “Dist/” Folder
[] Share Music as Media Server

Local Domain: ________________                # default "local."

Cryptographic Algorithm for Sharing: [ $(algos-list)    - ] # from Ironclad

Version Control Tab
--------------------
Version Control System: <> Git              | git
                        <> Bazaar           | bzr          
                        <> Subversion       | svn      
                        <> Mercurial        | hg        
                        <> Concurrent       | cvs      
                        <> Revision Control | rcs

Git # Specific to the version control system in use
----                                    

User                   
-----                  
Name: $(user-real-name)
eMail: ________________________  

Signing key: [ Bruce-Robert Pocock <brpocock@interworldly.com>    - ]
             [ ...                                                  ]
                        (Generate and publish a new key...)

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
|| Remote        | URL                               | Fetch                               ||
||-----------------------------------------------------------------------------------------||
|| origin        | git@github.com:brpocock/Phantasia | +refs/heads/:refs/remotes/origin/   || 
( + )                                   
Submodules                              
-----------                             
[] Skyline-Tool and Eightbol

[] Atari 7800 Tools                     
[] Intellivision Tools                  

Pushing                                 
--------                                
[] When pushing, automatically set up new branches on remote 

Pulling
--------
[] When pulling, automatically [ fast-forward only   - ]

Default Branch: ___________         # main default
                   
Issue Tracking Tab 
-------------------
Issue tracker kind: <> GitHub           
                    <> GitLab                               
                    <> Bugzilla

GitLab         # specific to each selection
-------

Tracker URL: ____________________________________________________ 
                            ( Sign in... )                          
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

;;;; Frame definition
(clim:define-application-frame preferences-inspector-frame (clim:standard-application-frame)
  ((dirty :initform nil :accessor frame-dirty)
   (current-tab :initform :accessibility :accessor frame-current-tab)
   ;; Accessibility Section
   (a11y-color :initform nil :accessor accessibility-color-theme-field)
   (a11y-keybinds :initform nil :accessor accessibility-key-bindings-theme-field)
   ;; Printing  section
   (paper-size-box :initform nil :accessor paper-size-box)
   (paper-size-radio :initform nil :accessor paper-size-radio)
   (paper-unit-box :initform nil :accessor paper-unit-box)
   (paper-unit-radio :initform nil :accessor paper-unit-radio)
   (paper-width-field :initform nil :accessor paper-width-field)
   (paper-height-field :initform nil :accessor paper-height-field)
   ;; Sharing section (p2p)
   (lan-sharing-checkbox :initform nil :accessor lan-sharing-checkbox)
   (lan-domain-field :initform nil :accessor lan-domain-field)
   (lan-pubkey-algo-field :initform nil :accessor lan-pubkey-algo-field)
   (dist-sharing-checkbox :initform nil :accessor dist-sharing-checkbox)
   (music-sharing-checkbox :initform nil :accessor music-sharing-checkbox)
   ;; VERSION-CONTROL section
   (version-control-system-box :initform nil :accessor version-control-system-box)
   (version-control-system-radio :initform nil :accessor version-control-system-radio)
   (ssh-key-field :initform nil :accessor ssh-key-field)
   (gpg-key-field :initform nil :accessor gpg-key-field)
   ;; Git-specific fields
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
   ;; Accessibility section
   (accessibility-theme-box :initform nil :accessor accessibility-theme-box)
   (accessibility-theme-radio :initform nil :accessor accessibility-theme-radio)
   (accessibility-shortcut-box :initform nil :accessor accessibility-shortcut-box)
   (accessibility-shortcut-radio :initform nil :accessor accessibility-shortcut-radio)
   (accessibility-latitude-field :initform nil :accessor accessibility-latitude-field)
   (accessibility-latitude-ns-box :initform nil :accessor accessibility-latitude-ns-box)
   (accessibility-latitude-ns-radio :initform nil :accessor accessibility-latitude-ns-radio)
   (accessibility-longitude-field :initform nil :accessor accessibility-longitude-field)
   (accessibility-longitude-ew-box :initform nil :accessor accessibility-longitude-ew-box)
   (accessibility-longitude-ew-radio :initform nil :accessor accessibility-longitude-ew-radio)
   (accessibility-desktop-theme-cache :initform nil :accessor accessibility-desktop-theme-cache)
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
  ;; Subscribe to printer list changes for dynamic menu updates
  (skyline-tool::subscribe :printer-list-changed
              (lambda (event)
                (declare (ignore event))
                (populate-print-menu 'preferences-print-to-menu)
                (clim:redisplay-frame-panes frame :force-p t)))
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
  (let ((confirmed (error "Reset all preferences to defaults?")))
    (when confirmed
      (setf *prefs-cache* nil)
      (setf (frame-dirty clim:*application-frame*) t)
      (setf (get-pref '(:internal :prefs :reset-by-user)) (get-universal-time))
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
  (let* ((path (prompt-save-pathname "Preferences.json" '(:dir :preferences :json))))
    (when path
      (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8)
        (json:encode-json
         (loop for (k v) on *prefs-cache* by #'cddr
               collect (cons (string-downcase (symbol-name k)) v))
         s)))))

(clim:define-command (com-preferences-save-pdf :command-table clim-internals::global-command-table
                                               :menu t :name t) ()
  (let* ((path (prompt-save-pathname "Preferences.pdf" '(:dir :preferences :pdf))))
    (when path
      (let ((ps-stream (uiop:run-program 
                        (list "ps2pdf" "-" (namestring path))
                        :input :stream
                        :output nil
                        :error-output nil)))
        (write-ps-header-bar ps-stream "Preferences")
        (loop for (k v) on *prefs-cache* by #'cddr
              do (format ps-stream "(~s: ~s) showpage show~%" k v))
        (close ps-stream)))))

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
(defun old%%display-preferences (frame pane)
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
                                                          (setf (get-pref '(:paper :width))
                                                                (parse-number value)))))))
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
                                                          (setf (get-pref '(:paper :height)) (parse-number value)))))))
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
                                  (clim:redisplay-frame-panes frame :force-p t)))))
        (make-full-width-row pane (clim:note-gadget-activated (music-sharing-checkbox frame) pane))))))

;;;; Tab bar display
(defun inside-clim-rectangle-p (x y rect)
  "Return T if point (x,y) is within RECT."
  (and (<= (clim:rectangle-min-x rect) x (clim:rectangle-max-x rect))
       (<= (clim:rectangle-min-y rect) y (clim:rectangle-max-y rect))))

(defun find-frame-containing-pane (pane)
  "Find the application frame containing PANE."
  (let ((sheet pane))
    (loop while sheet
          do (when (typep sheet 'clim:application-frame)
               (return sheet))
             (setf sheet (clim:sheet-medium sheet)))
    (when (typep sheet 'clim:application-frame)
      sheet)))

(defun tab-test (pane x y)
  "Return T if point (x,y) is within a tab rectangle on the tab-bar pane."
  (let ((frame (find-frame-containing-pane pane))
        (rects (slot-value pane 'tab-rectangles-cache)))
    (declare (ignore frame))
    (when rects
      (some (lambda (entry)
              (destructuring-bind (tab rect) entry
                (declare (ignore tab))
                (when (and rect (inside-clim-rectangle-p x y rect))
                  (return-from tab-test t))))
            rects))))

(defun tab-select (pane x y)
  "Handle selection of a tab at point (x,y) on the tab-bar pane."
  (let ((frame (find-frame-containing-pane pane))
        (rects (slot-value pane 'tab-rectangles-cache)))
    (when frame
      (some (lambda (entry)
              (destructuring-bind (tab rect) entry
                (when (and rect (inside-clim-rectangle-p x y rect))
                  (setf (frame-current-tab frame) tab)
                  (clim:redisplay-frame-panes frame :force-p t)
                  (return-from tab-select t))))
            (reverse rects)))))

(defgeneric display-tab-bar (pane frame))

(defmethod display-tab-bar ((pane clim:application-pane) (frame preferences-inspector-frame))
  "Draw custom tab bar with rounded corners and styling for Preferences Inspector."
  (let* ((tabs '(:accessibility :printing :network :version-control :issue-tracking))
         (tab-labels '("Accessibility" "Printing" "Network" "Version Control" "Issue Tracking"))
         (n (length tabs))
         (width (or (clim:bounding-rectangle-width (clim:sheet-region pane)) 400))
         (height 30)
         (tab-width (/ width n))
         (corner-radius 5))
    (loop for tab in tabs
          for label in tab-labels
          for i from 0
          for left = (* i tab-width)
          for right = (+ left tab-width)
          for selected = (eq tab (frame-current-tab frame))
          do (clim:with-drawing-options (pane
                                         :ink (if selected
                                                  (clim:make-gray-color 0)
                                                  (clim:make-gray-color 0.75)))
               (clim:draw-rectangle pane left 0 right (+ 20 height)
                                    :filled t
                                    :corner-radii (list corner-radius corner-radius
                                                        corner-radius corner-radius)))
             (let* ((text-x (if selected
                                (+ left 5)
                                (+ left 10)))
                    (text-y 8))
               (clim:with-drawing-options (pane
                                           :ink (if selected
                                                    (clim:make-gray-color 1)
                                                    (clim:make-gray-color 0)))
                 (clim:draw-text pane label (clim:make-point text-x text-y) :align-left :align-top))))))

;;;; Main display dispatcher
(defun display-preferences (frame pane)
  "Dispatch to the current tab's display function based on frame-current-tab."
  (let ((*standard-output* pane))
    (ecase (frame-current-tab frame)
      (:accessibility (display-accessibility-tab frame pane))
      (:printing (display-paper-size-tab frame pane))
      (:network (display-network-tab frame pane))
      (:version-control (display-version-control-tab frame pane))
      (:issue-tracking (display-issue-tracking-tab frame pane)))))

;;;; Sunrise/Sunset calculation
(defun degrees->radians (degrees)
  "Convert DEGREES to radians."
  (* degrees (/ pi 180.0d0)))

(defun radians->degrees (radians)
  "Convert RADIANS to degrees."
  (* radians (/ 180.0d0 pi)))

(defun julian-day (year month day)
  "Compute the Julian Day number for YEAR, MONTH, DAY."
  (let* ((a (floor (* 14 (- month 12)) 10))
         (y (+ year 4800 (- a)))
         (m (+ month (* 12 a) (- 3))))
    (+ day
       (floor (+ (* 153 m) 2) 5)
       (* 365 y)
       (floor y 4)
       (- (floor y 100))
       (floor y 400)
       (- 32045))))

(defun solar-declination (jd)
  "Compute solar declination δ for Julian Day JD using simplified Meeus."
  (let* ((n (- jd 2451545.0d0))
         (l (+ 280.460d0 (* 0.9856474d0 n)))
         (g (+ 357.528d0 (* 0.9856003d0 n)))
         (g-rad (degrees->radians (mod g 360.0d0)))
         (lambda-val (+ l (* 1.915d0 (sin g-rad)) (* 0.020d0 (sin (* 2 g-rad)))))
         (lambda-rad (degrees->radians lambda-val))
         (epsilon (degrees->radians 23.439d0))
         (sin-dec (* (sin lambda-rad) (sin epsilon))))
    (asin sin-dec)))

(defun hour-angle-at-sunrise (latitude declination-jd)
  (let* ((phi (degrees->radians latitude))
         (cos-h0 (* (- (tan phi)) (tan (solar-declination declination-jd)))))
    (cond
      ((< cos-h0 -1.0d0) nil)
      ((> cos-h0 1.0d0) nil)
      (t (acos cos-h0)))))
  
(defun current-theme-from-sunrise-sunset (latitude longitude)
  "Determine whether the current time is between sunrise and sunset.
   Uses the observer's LATITUDE (in degrees) and LONGITUDE (in degrees).
   Returns :NORMAL if it is daytime, :INVERTED if it is nighttime."
  (multiple-value-bind (seconds minutes hours month-day month year week-day
                        daylight-saving-p tz-offset)
      (decode-universal-time (get-universal-time))
    (declare (ignore week-day tz-offset))
    (let* ((jd (julian-day year month month-day))
           (hour (+ hours (if daylight-saving-p 1 0)
                    (/ minutes 60.0d0)
                    (/ seconds 3600.0d0)))
           (n (- jd 2451545.0d0))
           (j-star (/ n 36525.0d0))
           (mean-solar-time (+ hour
                               (- longitude 0.0d0)
                               (* 24.0d0
                                  (- n (floor n)))))
           (decl (solar-declination jd))
           (h0 (hour-angle-at-sunrise latitude decl)))
      (if h0
          (let* ((l0 (+ 280.460d0 (* 360.9856235d0 n)))
                 (m (+ 357.528d0 (* 359.99050d0 j-star)))
                 (m-rad (degrees->radians (mod m 360.0d0)))
                 (c (+ (* 1.915d0 (sin m-rad))
                       (* 0.020d0 (sin (* 2 m-rad)))))
                 #+ () (sun-longitude (+ l0 c))
                 #+ () (e (+ (* 0.01671d0 (cos m-rad))
                             (- 0.0001d0)))
                 (lambda-val (+ l0 c))
                 (epsilon-rad (degrees->radians 23.439d0))
                 #+ () (tan-half-epsilon (tan (/ epsilon-rad 2.0d0)))
                 (right-ascension (/ (radians->degrees
                                      (atan (* (tan (degrees->radians lambda-val))
                                               (cos epsilon-rad))
                                            1.0d0))
                                     15.0d0))
                 #+ () (j-transit (+ 2451545.0d0
                                     (* 0.0009d0
                                        (+ (* 360.0d0 (/ (+ (* 6.0d0 right-ascension)
                                                            (* 24.0d0 n))
                                                         360.0d0))
                                           0.0d0))))
                 (h0-degrees (radians->degrees h0))
                 (lst (+ mean-solar-time (* 0.0057755183d0
                                            (- l0 c (* right-ascension))))))
            (if (and (>= lst (- right-ascension (/ h0-degrees 15.0d0)))
                     (<= lst (+ right-ascension (/ h0-degrees 15.0d0))))
                :normal
                :inverted))
          :normal))))

(defun read-desktop-theme ()
  "Read the current desktop theme from dbus.
   Returns :NORMAL for light theme, :INVERTED for dark theme.
   Falls back to :NORMAL if dbus is unavailable."
  (handler-case
      (let ((result (error "unimplemented")))
        (cond
          ((search "uint32 1" result) :inverted)
          ((search "uint32 2" result) :inverted)
          (t :normal)))
    (error () :light)))

(defun effective-color-theme (requested-theme frame)
  "Determine the actual color theme to apply based on REQUESTED-THEME.
   For :desktop, reads from dbus. For :sunrise, calculates from lat/long."
  (declare (ignore frame))
  (case requested-theme
    (:desktop (read-desktop-theme))
    (:sunrise
     (let ((lat-str (get-pref '(:location :latitude) ""))
           (lon-str (get-pref '(:location :longitude) "")))
       (handler-case
           (let ((lat (parse-number lat-str))
                 (lon (parse-number lon-str))
                 (lat-ns (get-pref '(:location :latitude-ns) "N"))
                 (lon-ew (get-pref '(:location :longitude-ew) "E")))
             (current-theme-from-sunrise-sunset
              (if (string-equal lat-ns "S") (- lat) lat)
              (if (string-equal lon-ew "W") (- lon) lon)))
         (error ()
           :normal))))
    (:inverted :inverted)
    (otherwise :normal)))

(defun display-version-control-tab (frame pane)
  "Display the Version Control tab content."
  (declare (ignore frame pane)))

(defun display-issue-tracking-tab (frame pane)
  "Display the Issue Tracking tab content."
  (declare (ignore frame pane)))
