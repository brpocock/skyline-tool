;;; Skyline-Tool src/gui/gui-preferences.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC
#|
Preferences Inspector documentation: Requirements. NO MODIFICATIONS permitted. ;

Tab Bar
/ Accessibility \ / Printing \ / Network \ / Version Control \ / Issue Tracking \

Accessibility Tab
------------------

Color Theme: <> Normal (Light)
<> Inverse (Dark)
<> Desktop Setting
<> Sunrise/Sunset

Sunrise/Sunset Location: ______° <> N <> S ______° <> W <> E

Keyboard Shortcuts: <> Gnome
<> macOS
<> Emacs


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
                                        ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
Version Control Tab                     ;
Versioning                              ;
-----------                             ;
Version Control System: <> Git | git    ;
<> Subversion | svn                     ;
<> Bazaar | bzr                         ;
<> Mercurial | hg                       ;
<> Concurrent | cvs                     ;
<> Revision Control | rcs               ;
                                        ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
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
                                        ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
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


;;;; Main display function - TABS IMPLEMENTATION
(defun display-preferences (frame pane)
  "Display the complete preferences interface using tabbed layout."
  (clim:tabbed-pane (pane :name "preferences-tabs")
    ;; === Paper Size Tab ===
    (clim:formatting-table-pane (pane :name "paper-tab"))
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
                                                (save-preferences-now frame)
                                                (clim:redisplay-frame-panes frame :force-p t))))))
        (make-full-width-row pane (clim:note-gadget-activated box pane)))

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
    (clim:formatting-table-pane (pane :name "sharing-tab"))
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

    ;; === LAN Sharing Tab ===
    (clim:formatting-table-pane (pane :name "lan-sharing-tab"))
    (make-section-header pane "LAN Sharing")
    (unless (p2p-enabled-checkbox frame)
      (setf (p2p-enabled-checkbox frame)
            (clim:make-pane 'clim:check-box
                            :label "Enable LAN Resource Sharing"
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
                                                    (setf (get-pref '(:p2p :interface)) value)
                                                    (save-preferences-now frame))))))
    (clim:note-gadget-activated (p2p-interface-field frame) pane)
    (make-label-value-row pane "Advertise Interval (sec)"
                          (unless (p2p-advertise-interval-field frame)
                            (setf (p2p-advertise-interval-field frame)
                                  (clim:make-pane 'clim:text-field
                                                  :value (format nil "~a" (get-pref '(:p2p :advertise-interval) 30))
                                                  :width 80
                                                  :activate-callback
                                                  (lambda (gadget value)
                                                    (declare (ignore gadget))
                                                    (let ((val (parse-integer (or value "30") :junk-allowed t)))
                                                      (setf (get-pref '(:p2p :advertise-interval)) (max 5 (min 300 val)))
                                                      (save-preferences-now frame))))))
    (clim:note-gadget-activated (p2p-advertise-interval-field frame) pane)
    (make-label-value-row pane "Discovery Interval (sec)"
                          (unless (p2p-discovery-interval-field frame)
                            (setf (p2p-discovery-interval-field frame)
                                  (clim:make-pane 'clim:text-field
                                                  :value (format nil "~a" (get-pref '(:p2p :discovery-interval) 60))
                                                  :width 80
                                                  :activate-callback
                                                  (lambda (gadget value)
                                                    (declare (ignore gadget))
                                                    (let ((val (parse-integer (or value "60") :junk-allowed t)))
                                                      (setf (get-pref '(:p2p :discovery-interval)) (max 10 (min 600 val)))
                                                      (save-preferences-now frame))))))
    (clim:note-gadget-activated (p2p-discovery-interval-field frame) pane)
    (make-label-value-row pane "Port Range:"
                          (make-label-value-row (pane "Min:")
                                                (unless (p2p-min-port-field frame)
                                                  (setf (p2p-min-port-field frame)
                                                        (clim:make-pane 'clim:text-field
                                                                        :value (format nil "~a" (get-pref '(:p2p :min-port) 50000))
                                                                        :width 80
                                                                        :activate-callback
                                                                        (lambda (gadget value)
                                                                          (declare (ignore gadget))
                                                                          (let ((val (parse-integer (or value "50000") :junk-allowed t)))
                                                                            (setf (get-pref '(:p2p :min-port)) (max 1024 (min 65535 val)))
                                                                            (save-preferences-now frame))))))
                    (clim:note-gadget-activated (p2p-min-port-field frame) pane))
                          (make-label-value-row (pane "Max:")
                                                (unless (p2p-max-port-field frame)
                                                  (setf (p2p-max-port-field frame)
                                                        (clim:make-pane 'clim:text-field
                                                                        :value (format nil "~a" (get-pref '(:p2p :max-port) 60000))
                                                                        :width 80
                                                                        :activate-callback
                                                                        (lambda (gadget value)
                                                                          (declare (ignore gadget))
                                                                          (let ((val (parse-integer (or value "60000") :junk-allowed t)))
                                                                            (setf (get-pref '(:p2p :max-port)) (max (get-pref '(:p2p :min-port)) (min 65535 val)))
                                                                            (save-preferences-now frame))))))
                    (clim:note-gadget-activated (p2p-max-port-field frame) pane))
    (make-label-value-row pane "Public Key Algorithm"
                          (unless (p2p-pubkey-algo-field frame)
                            (setf (p2p-pubkey-algo-field frame)
                                  (clim:make-pane 'clim:option-pane
                                                  :items '("ed25519" "rsa4096" "ecdsa")
                                                  :value (get-pref '(:p2p :pubkey-algo) "ed25519")
                                                  :value-changed-callback
                                                  (lambda (g v)
                                                    (declare (ignore g))
                                                    (setf (get-pref '(:p2p :pubkey-algo)) v)
                                                    (save-preferences-now frame))))))

    ;; === Accessibility Tab (a11y) ===
    (clim:formatting-table-pane (pane :name "a11y-tab"))
    (make-section-header pane "Accessibility")
    (load 'gui-preferences-a11y)
    (display-accessibility-tab frame pane)

    ;; === Issue Tracking Tab ===
    (clim:formatting-table-pane (pane :name "issue-tracking-tab"))
    (make-section-header pane "Issue Tracking")
    (load 'gui-preferences-issue-tracking)
    (display-issue-tracking-tab frame pane)))
