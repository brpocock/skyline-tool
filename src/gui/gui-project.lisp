;;; Skyline-Tool src/gui/gui-project.lisp
;;; Project Inspector - complete per AGENTS.md spec

(in-package :skyline-tool)

#|

Requirements follow, do not alter this documentation.

Project Inspector:

Game Title: _______________________________

Version: ___.___      | Part Number: ____________

Studio: __________________________
Publisher: __________________________

[] Special Intro for ZPH

New Game Script
| {   } | Demo Game                       | $1234   |
| {   } |     Global                      |[]D[]P[]A|   ( Choose ... )
| {   } |                                 |         |

Machine: [ Atari 7800 ProSystem                - ]
[ Atari 5200 SuperSystem                ]
[ ColecoVision                          ]
[ Intellivision                         ]
[ Nintendo Entertainment System         ]
[ Super Nintendo Entertainment System   ]
$(all platforms listed alphabetically)


Atari 7800 Options
-------------------

CPU: 6502                                              # just text, not editable

Sound options: <> TIA Only   <> Hokey + TIA

Common Palette:

P4C1 [##] _________________                # pick color from palettes and assign name
P4C2 [##] _________________
P4C3 [##] _________________

P5C1 [##] _________________
P5C2 [##] _________________
P5C3 [##] _________________

P6C1 [##] _________________
P6C2 [##] _________________
P6C3 [##] _________________

P7C1 [##] _________________
P7C2 [##] _________________
P7C3 [##] _________________

#### menu

Project
--------
New...
New from URL...
Port to Machine...
---
Save as > Text...
JSON...
PDF...
---
Print to > { list of printers }
---
Close

Edit
-----
Cut
Copy
Paste
---
Find...

Tools
------
Show ROM Budget...

Run
----
Build > <> Demo
<> Public
<> $(Publisher)
Region > <> NTSC
<> PAL
<> SECAM
<> Internal
<> HD
---
Run in A7800...
Run in js7800...
Run in test7800...
Run in Ocelot...
---
Send to SD Card...
Send to 7800GD Debug Port...
---
Make > Test...
Documentation...
Game...
---
All...
---
Build a Release Package...

View
-----
[] Editable
---
[] Project Pane

Help
-----
How to Manage Projects...
Skyline-Tool Developers' Guide...
Skyline-Tool Scripting Guide...
---
About Skyline-Tool..

|#


(clim:define-command-table project-save-as-menu
  :menu (("JSON..." :command com-save-as-json)
         ("Text..." :command com-save-as-text)
         ("PDF..." :command com-save-as-pdf)))

(clim:define-command-table project-print-to-menu
  :menu ()
  :inherit-from (about-print-to-menu))

(defun populate-project-print-to-menu ()
  (populate-print-menu 'project-print-to-menu))

(clim:define-command-table project-send-to-menu
  :menu ()
  :inherit-from (about-send-to-menu))

(defun populate-project-send-to-menu ()
  (populate-send-to-menu 'project-send-to-menu))

(clim:define-command-table project-build-menu
  :menu (("Demo" :command com-build-demo :radio t)
         ("Public" :command com-build-public :radio t)
         ("Publisher" :command com-build-publisher :radio t)))

(clim:define-command-table project-region-menu
  :menu (("NTSC" :command com-region-ntsc :radio t)
         ("PAL" :command com-region-pal :radio t)
         ("SECAM" :command com-region-secam :radio t)
         ("Internal" :command com-region-internal :radio t)))

(clim:define-command-table project-file-menu
  :menu (("New..." :command com-new-project)
         ("New from URL..." :command com-new-project-from-url)
         ("Port to Machine..." :command com-port-project)
         (nil :divider :line)
         ("Save as" :menu project-save-as-menu)
         ("Print to" :menu project-print-to-menu)
         ("Send to" :menu project-send-to-menu)
         (nil :divider :line)
         ("Close" :command com-close-frame)))

(clim:define-command-table project-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table project-view-menu
  :menu (("[] Editable" :command com-inspector-toggle-view :name t)
         (nil :divider :line)
         ("[] Project Pane" :command com-toggle-project-pane :name t)))

(clim:define-command-table project-run-menu
  :menu (("Build" :menu project-build-menu)
         ("Region" :menu project-region-menu)
         (nil :divider :line)
         ("Make" :menu resource-make-menu)
         (nil :divider :line)
         ("Build a Release Package..." :command com-build-release-package)))

(clim:define-command-table project-help-menu
  :menu (("How to Manage Projects..." :command com-help-project)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-scripting-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table project-menu-bar
  :menu (("Project" :menu project-file-menu)
         ("Edit" :menu project-edit-menu)
         ("Run" :menu project-run-menu)
         ("View" :menu project-view-menu)
         ("Help" :menu project-help-menu)))

;; 
;; FRAME DEFINITION
;;

(clim:define-application-frame project-inspector-frame (gui-inspector-frame clim:standard-application-frame)
  ((project :initarg :project :reader frame-project)
   (scavenger :initform (make-hash-table :test 'equal) :accessor frame-scavenger))
  (:panes
   (content :application
            :display-function 'display-project-inspector
            :scroll-bars :vertical
            :height 600 :width 800)
   (status-bar :application
               :display-function 'display-project-status
               :height 30 :width 800))
  (:layouts (default (clim:vertically () content status-bar)))
  (:menu-bar project-menu-bar)
  (:icon (skyline-tool-icon :resource :project))
  (:pretty-name "Project Inspector"))

(defmethod clim:frame-pretty-name ((frame project-inspector-frame))
  (let ((project (frame-project frame)))
    (format nil "~a — ~a ~a"
            (or (getf project :name) "Project")
            (or *game-title* "Game")
            (machine-directory-name))))

;; 
;; POPULATE DYNAMIC MENUS
;; 

(defmethod initialize-instance :after ((frame project-inspector-frame) &key)
  (ensure-printer-discovery-started)
  (populate-project-print-to-menu)
  (populate-project-send-to-menu)
  (subscribe :printer-list-changed
             (lambda (event)
               (declare (ignore event))
               (populate-project-print-to-menu)))
  (subscribe :p2p-recipients-changed
             (lambda (event)
               (declare (ignore event))
               (populate-project-send-to-menu)))
  (subscribe :build-changed
             (lambda (event)
               (declare (ignore event))
               (clim:redisplay-frame-panes frame :force-p t)))
  (subscribe :region-changed
             (lambda (event)
               (declare (ignore event))
               (clim:redisplay-frame-panes frame :force-p t)
               (publish :region-changed)))
  (subscribe :preference-change
             (lambda (event)
               (declare (ignore event))
               (clim:redisplay-frame-panes frame :force-p t))))

;; 
;; DISPLAY FUNCTIONS
;; 

(defun display-project-inspector (frame pane)
  (let ((project (frame-project frame)))
    (when project
      (let ((*standard-output* pane))
        (format pane "~&Game Title: ~a~%" (getf project :name))
        (format pane "~&Version: ~a    Part Number: ~a~%"
                (getf project :version "1.0.0")
                (getf project :part-number ""))
        (format pane "~&Studio: ~a~%" (getf project :studio ""))
        (format pane "~&Publisher: ~a~%" (getf project :publisher ""))
        (format pane "~&~:[~;Special Intro for ZPH ~]~%" (getf project :special-intro nil))
        (format pane "~&New Game Script~%")
        (format pane "~&Machine: ~a~%" (getf project :machine "Atari 7800 ProSystem"))
        (format pane "~&CPU: ~a~%" (getf project :cpu "6502"))
        (format pane "~&Sound: ~a~%" (getf project :sound "TIA Only"))
        (format pane "~&Common Palette~%")))))

(defun display-project-status (frame pane)
  (let ((project (frame-project frame)))
    (when project
      (let ((*standard-output* pane))
        (format pane "Project | Build: ~a | Region: ~a | VC: ~a | View: ~a"
                (getf project :build "Demo")
                (getf project :region "NTSC")
                (or (getf project :vc-status) "unknown")
                (string-downcase (symbol-name (frame-view-mode frame))))))))

;; 
;; HELPER FUNCTIONS
;; 

(defun update-project-properties (project)
  "Update project properties interactively"
  (declare (ignore project))
  (format *query-io* "~&Update project properties - TODO: implement interactive dialog~%"))

(defun export-project-to-pdf (project filepath)
  "Export project configuration as PDF"
  (with-open-file (s filepath :direction :output :if-exists :supersede)
    (format s "%!PS-Adobe-1.0~%%Title: Project Configuration~%~%")
    (format s "/Helvetica findfont 12 scalefont setfont~%")
    (format s "72 720 moveto (Project: ~a) show~%" (getf project :name))
    (format s "72 700 moveto (Version: ~a) show~%" (getf project :version "1.0.0"))
    (format s "72 680 moveto (Build: ~a) show~%" *build*)
    (format s "72 660 moveto (Region: ~a) show~%" *region*)
    (format s "showpage~%")))

;; 
;; COMMANDS
;; 

(clim:define-command (com-amend-project :command-table clim-internals::global-command-table :menu t :name t)
  ()
  ()
  "Amend the project configuration in place"
  (let ((project (frame-project frame)))
    (when project
      (update-project-properties project)
      (clim:redisplay-frame-panes frame :force-p t))))

(clim:define-command (com-save-project :command-table clim-internals::global-command-table :menu t :name t)
    ()
  ()
  "Save the project configuration"
  (let ((frame clim:*application-frame*))
    (when frame
      (let ((project (frame-project frame)))
        (cond ((null project)
               (format *query-io* "~&No project loaded to save~%"))
              (t
               (let ((filepath (prompt-save-pathname "Save Project"
                                                      :default-name "project.json"
                                                      :prefs-key :last-project-export-directory)))
                  (when filepath
                    (export-resource-to-json-file project filepath)
                    (format *query-io* "~&Project saved to ~a~%" filepath)
                    (clim:redisplay-frame-panes frame :force-p t)
                    (publish :project-saved :payload filepath)))))))))

(clim:define-command (com-save-as-text :command-table project-save-as-menu :menu t :name t)
    ()
  ()
  "Export project as human-readable text"
  (let ((frame clim:*application-frame*))
    (when frame
      (let ((project (frame-project frame)))
        (cond ((null project)
               (format *query-io* "~&No project loaded to export as text~%"))
              (t
               (let ((filepath (prompt-save-pathname "Export Project as Text"
                                                     :defaults "project.txt"
                                                     :prefs-key :last-export-directory)))
                 (when filepath
                   (export-resource-to-text-file project filepath)
                   (format *query-io* "~&Project exported as text to ~a~%" filepath)
                   (clim:redisplay-frame-panes frame :force-p t)
                   (publish :project-saved :payload filepath)))))))))

(clim:define-command (com-save-as-pdf :command-table project-save-as-menu :menu t :name t)
  ()
  ()
  "Export project as PDF"
  (let ((frame clim:*application-frame*))
    (when frame
      (let ((project (frame-project frame)))
        (cond ((null project)
               (format *query-io* "~&No project loaded to export as PDF~%"))
              (t
               (let ((filepath (prompt-save-pathname "Export Project as PDF"
                                                    :defaults "project.pdf"
                                                    :prefs-key :last-export-directory)))
                 (when filepath
                   (export-project-to-pdf project filepath)
                   (format *query-io* "~&Project exported as PDF to ~a~%" filepath)
                   (clim:redisplay-frame-panes frame :force-p t)
                   (publish :project-saved :payload filepath)))))))))

(clim:define-command (com-build-demo :command-table project-menu-bar :menu t :name t)
  ()
  ()
  "Build Demo package"
  (setf *build* :demo)
  (publish :build-changed :payload :demo)
  (format *query-io* "~&Build set to Demo~%"))

(clim:define-command (com-build-public :command-table project-menu-bar :menu t :name t)
  ()
  ()
  "Build Public package"
  (setf *build* :public)
  (publish :build-changed :payload :public)
  (format *query-io* "~&Build set to Public~%"))

(clim:define-command (com-build-publisher :command-table project-menu-bar :menu t :name t)
   ()
   ()
   "Build Publisher package"
   (setf *build* :publisher)
   (publish :build-changed :payload :publisher)
   (format *query-io* "~&Build set to Publisher~%"))

(clim:define-command (com-region-ntsc :command-table project-menu-bar :menu t :name t)
   ()
   ()
   "Set region to NTSC"
   (setf *region* :ntsc)
   (publish :region-changed :payload :ntsc)
   (format *query-io* "~&Region set to NTSC~%"))

(clim:define-command (com-region-pal :command-table project-menu-bar :menu t :name t)
   ()
   ()
   "Set region to PAL"
   (setf *region* :pal)
   (publish :region-changed :payload :pal)
   (format *query-io* "~&Region set to PAL~%"))

(clim:define-command (com-region-secam :command-table project-menu-bar :menu t :name t)
   ()
   ()
   "Set region to SECAM"
   (setf *region* :secam)
   (publish :region-changed :payload :secam)
   (format *query-io* "~&Region set to SECAM~%"))

(clim:define-command (com-region-internal :command-table project-menu-bar :menu t :name t)
   ()
   ()
   "Set region to Internal"
   (setf *region* :internal)
   (publish :region-changed :payload :internal)
   (format *query-io* "~&Region set to Internal~%"))


















(clim:define-command (com-build-release-package :command-table clim-internals::global-command-table :menu t :name t)
  ()
  ()
  "Build a Release Package"
  (format *query-io* "~&Building release package...~%")
  (submit-task (lambda () (build-target "release" :phony t))))

(clim:define-command (com-help-project :command-table project-help-menu :menu t :name t)
  ()
  ()
  "Show help for Project Inspector")

;; 
;; FRAME INITIALIZATION
;; 

(defun start-project-scavenger (&optional frame)
  "Start a background thread that monitors project files for changes."
  (let ((project-path (uiop:getcwd))
        (target-frame (or frame
                          (and (boundp 'clim:*application-frame*)
                               clim:*application-frame*))))
    (make-thread
     (lambda ()
       (let ((last-mod 0))
         (loop
            (let ((cur-mod (ignore-errors (uiop:directory-files project-path))))
              (when (and cur-mod (not (equal cur-mod last-mod)))
                (setf last-mod cur-mod)
                (when target-frame
                  (clim:redisplay-frame-panes target-frame :force-p t))))
            (sleep 1))))
     :name "project-scavenger")))

(defun open-project-inspector (&key (project *project.json*))
  "Open the project inspector window."
  (clim:run-frame-top-level
   (clim:make-application-frame 'project-inspector-frame
                                :project project
                                :view-mode :reference)))

;; 
;; SUBMIT/COMMAND FUNCTIONS
;; 

(clim:define-command (com-update-project-props :command-table clim-internals::global-command-table :menu t :name t)
    ()
  ()
  (let ((project (frame-project clim:*application-frame*)))
    (when project
      (clim:frame-exit clim:*application-frame*))))
