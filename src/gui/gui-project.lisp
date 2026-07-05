;;; Skyline-Tool src/gui/gui-project.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-application-frame project-inspector-frame (gui-inspector-frame)
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
(clim:define-command-table project-inspector-file-menu
  :menu (("Save" :command com-save-project)
         ("Save as" :menu com-save-project-as)
         ("Print to" :menu com-print-project-to)
         ("Close" :command com-close-frame)))

(clim:define-command-table project-inspector-run-menu
  :menu (("Build" :menu com-build-project-menu)
         ("Region" :menu com-build-project-region)
         (nil :divider :line)
         ("Quick Clean" :command com-clean-project)
         ("Full Clean" :command com-clean-project-all)))

(clim:define-command-table project-inspector-view-menu
  :menu (("☐ Editable" :command nil)
         (nil :divider :line)))

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
  (make-thread (lambda () (build-make-target "quickclean" :phony t)) :name "Make All"))

(clim:define-command (com-clean-project :command-table clim-internals::global-command-table
                                        :menu t :name t)
    ()
  "Clean the project."
  (make-thread (lambda () (build-make-target "quickclean" :phony t)) :name "Make Quickclean"))

;; Scavenger for project
(defun start-project-scavenger (frame)
  "Start a background thread that monitors project files for changes."
  (let ((project-path (uiop:getcwd)))
    (make-thread
     (lambda ()
       (let ((last-mod 0))
         (loop
            (let ((cur-mod (ignore-errors (uiop:directory-files project-path))))
              (when (and cur-mod (not (equal cur-mod last-mod)))
                (setf last-mod cur-mod)
                (when (boundp 'clim:*application-frame*)
                  (clim:redisplay-frame-panes frame :force-p t))))
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
  (call-next-method))
