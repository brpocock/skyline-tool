;;; Skyline-Tool src/gui/gui-project.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

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
  (:menu-bar project-inspector-menu-bar)
  (:pretty-name "Project Inspector"))

;; Menu definitions

(clim:define-command-table com-save-project-as
  :menu (("JSON..." :command com-save-as-json)
         ("Text..." :command com-save-as-text)
         ("PDF..." :command com-save-as-pdf)))

(clim:define-command-table com-print-project-to
  :menu ()
  :inherit-from (about-print-to-menu))

(defun populate-project-print-to-menu ()
  (populate-print-menu 'com-print-project-to))

(clim:define-command-table project-inspector-file-menu
  :menu (("Save" :command com-save-project)
         ("Save as" :menu com-save-project-as)
         ("Print to" :menu com-print-project-to)
         ("Close" :command com-close-frame)))

(clim:define-command-table com-build-project-menu
  :menu (("Build" :command com-build-project)
         ("Demo" :command (lambda () (format t "~&Demo build not implemented~%")))
         ("Public" :command (lambda () (format t "~&Public build not implemented~%")))
         ("$(Publisher)" :command (lambda () (format t "~&Publisher build not implemented~%")))))

(clim:define-command-table com-build-project-region
  :menu (("NTSC" :command (lambda () (format t "~&NTSC region build not implemented~%")))
         ("PAL" :command (lambda () (format t "~&PAL region build not implemented~%")))
         ("SECAM" :command (lambda () (format t "~&SECAM region build not implemented~%")))))

(clim:define-command-table project-inspector-run-menu
  :menu (("Build" :menu com-build-project-menu)
         ("Region" :menu com-build-project-region)
         (nil :divider :line)
         ("Make" :menu resource-make-menu)
         (nil :divider :line)
         ("Build a Release Package..." :command com-build-release-package)))

(clim:define-command-table project-inspector-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :menu t :name t
           :documentation "Toggle editable mode")))

(clim:define-command-table project-inspector-menu-bar
  :menu (("Project" :menu project-inspector-file-menu)
         ("Edit" :menu inspector-edit-menu)
         ("Run" :menu project-inspector-run-menu)
         ("View" :menu project-inspector-view-menu)
         ("Help" :menu inspector-help-menu)))

;; Display functions
(defun display-project-inspector (frame pane)
  (let ((project (frame-project frame)))
    (when project
      (let ((*standard-output* pane))
        (format pane "Project: ~a~%" (getf project :name))
        (format pane "Build: ~a~%" (getf project :build))
        (format pane "Resources: ~a~%" (length (getf project :resources)))))))

(defun display-project-status (frame pane)
  (let ((project (frame-project frame)))
    (when project
      (let ((*standard-output* pane))
        (format pane "Project | VC: ~a | View: ~a"
                (or (getf project :vc-status) "unknown")
                (string-downcase (symbol-name (frame-view-mode frame))))))))

;; Commands
(clim:define-command (com-save-project :command-table clim-internals::global-command-table
                                       :menu t :name t)
    ()
  "Save the project configuration."
  (error  "Save project not implemented."))

(clim:define-command (com-build-project :command-table clim-internals::global-command-table
                                        :menu t :name t)
    ()
  "Build the project."
  (make-thread (lambda () (build-target "quickclean" :phony t)) :name "Make All"))

(clim:define-command (com-clean-project :command-table clim-internals::global-command-table
                                        :menu t :name t)
    ()
  "Clean the project."
  (make-thread (lambda () (build-target "quickclean" :phony t)) :name "Make Quickclean"))

;; Scavenger for project
(defun start-project-scavenger (&optional frame)
  "Start a background thread that monitors project files for changes.
Called from the thread-pool with no arguments; FRAME defaults to
CLIM:*APPLICATION-FRAME* when available."
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

;; Need :AFTER method to properly chain with gui-inspector-frame's :AFTER method
(defmethod initialize-instance :after ((frame project-inspector-frame) &key)
  (ensure-printer-discovery-started)
  ;; No special initialization needed beyond the parent's initialization
  )
