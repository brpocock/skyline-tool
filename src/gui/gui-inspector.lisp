;;; Skyline-Tool src/gui/gui-inspector.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC


#|

Correct menus include

```text

BLOB # This should be the name of the kind of Resource
-----
New...
Import...
---
Save
Version > [] Tracked # This applies to file-type resources
... [] Staged
... ---
... [] Ignored
Save as > JSON... # The first three are universal
... Text...
... PDF...
... ---
... PNG... # This is BLOB-specific
---
Send to > { all p2p recipients list }
Print to > { all dns-sd or cups printers list }
---
Close


Edit
-----
Cut
Copy
Paste
---
Find...

Run
----
Open in Gimp... # these are resource-specific items
Convert to PNG...
Compile to Source...

View
-----
[] Editable
---
[] Project Pane

Help
-----
How to Inspect a BLOB... # specific to the window
---
Skyline-Tool Developers' Guide...
Skyline-Tool Scripting Guide...
---
About Skyline-Tool...

```

These are  the minimum menus, and  with the exceptions noted,  shared by
all  Game-Resource Inspectors  ...  and largely  shared by  Preferences,
Thread, and Project Inspectors as well.

|#

(in-package :skyline-tool)

(defparameter *inspector-view* :editing
  "Current view mode for inspectors: :reference, :reading, or :editing")

(defun make-output-path (resource extension)
  "Build the output pathname from the resource's moniker.
(make-output-path resource :png) → #P\"SomeName.png\""
  (make-pathname :type (string-downcase (string extension))
                 :name (game-resource-title resource)))

(defun start-file-watcher (frame)
  "Start a background thread that monitors the resource's file for changes
and updates the inspector when the modification time changes.
Returns nil when FRAME has no game-resource or no watchable paths."
  (let ((resource (ignore-errors (slot-value frame 'frame-resource))))
    (unless (typep resource 'game-resource)
      (return-from start-file-watcher nil))
    (let ((paths (remove-if-not #'probe-file
                                (remove-if-not #'identity
                                               (game-resource-pathnames resource)))))
      (unless paths
        (return-from start-file-watcher nil))
      (make-thread
       (lambda ()
         (inotify:with-inotify (inot (mapcar (lambda (pathname)
                                               (list pathname inotify:in-all-events))
                                             paths))
           (loop for ev = (inotify:read-events inot)
                 do (ignore-errors
                     (clim:redisplay-frame-panes frame)))))
       :name (format nil "Watcher for ~a" (ignore-errors (game-resource-title resource)))))))

(defun stop-file-watcher (frame)
  "Stop the file watcher thread if running."
  (let ((thread (ignore-errors (frame-watcher-thread frame))))
    (when (and thread (bt:thread-alive-p thread))
      (bt:destroy-thread thread))))

;; 
;; Menu definitions (MUST come before uniform-inspector-frame which references them)
;; 
(clim:define-command-table inspector-vc-menu
  :menu (("Tracked" :command com-toggle-vc-tracked :toggle t)
         ("Staged" :command com-toggle-vc-staged :toggle t)
         (nil :divider :line)
         ("Ignored" :command com-toggle-vc-ignored :toggle t)))

(clim:define-command-table inspector-save-as-menu
  :menu (("JSON..." :command com-save-as-json)
         ("Text..." :command com-save-as-text)
         ("PDF..." :command com-save-as-pdf)))

(clim:define-command-table inspector-send-to-menu
  :menu (("Send as Backup..." :command com-send-to-backup)
         ("Send to Repository..." :command com-send-to-repo)))

(clim:define-command-table inspector-print-to-menu
  :menu ())

(clim:define-command-table inspector-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table inspector-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :toggle t)
         (nil :divider :line)
         ("Project Pane" :command com-toggle-project-pane :toggle t)))

(clim:define-command-table inspector-run-menu
  :menu (("Run..." :command com-run-resource))) ; Overridden by per-type inspectors

(clim:define-command-table inspector-help-menu
  :menu (("How to Inspect..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table inspector-file-menu
  :menu (("New..." :command com-new-resource)
         ("Import..." :command com-import-resource)
         (nil :divider :line)
         ("Save" :command com-save-resource)
         ("Version" :menu inspector-vc-menu)
         ("Save as" :menu inspector-save-as-menu)
         (nil :divider :line)
         ("Send to" :menu inspector-send-to-menu)
         ("Print to" :menu inspector-print-to-menu)
         (nil :divider :line)
         ("Close" :command com-close-frame)))

(clim:define-command-table inspector-menu-bar
  :menu (("Resource" :menu inspector-file-menu)
         ("Edit" :menu inspector-edit-menu)
         ("Run"  :menu inspector-run-menu)
         ("View" :menu inspector-view-menu)
         ("Help" :menu inspector-help-menu)))

;; 
;; Uniform Inspector Base Class
;; Provides standardized layout, display functions, and menu infrastructure
;; for all resource type inspectors
;;
(defclass uniform-inspector-frame ()
  ())

;; Standardized display functions for uniform 3-column layout
;; display-inspector-content is a generic defined in game-resource.lisp

(defun display-inspector-status (frame pane)
  (let* ((resource (inspector-resource frame))
         (file-path (when resource (first (game-resource-pathnames resource))))
         (vc-status (when file-path (vc-file-status file-path))))
    (when resource
      (let ((*standard-output* pane))
        (format pane "~a | ~a | VC: ~a | View: ~a"
                (game-resource-kind resource)
                (game-resource-title resource)
                (or vc-status "unknown")
                (string-downcase (symbol-name (frame-view-mode frame))))))))


(clim:define-application-frame gui-inspector-frame (resource-inspector-mixin uniform-inspector-frame clim:standard-application-frame)
  ((view-mode :initform :editing :initarg :view-mode :accessor frame-view-mode)
   (watcher-thread :initform nil :accessor frame-watcher-thread))
  (:panes
   (content-pane :application :display-function 'display-inspector-content
                               :scroll-bars :vertical :height 600 :width 800)
   (status-pane :application :display-function 'display-inspector-status
                             :scroll-bars nil :height 30 :width 800))
  (:layouts
   (default (clim:vertically () content-pane status-pane)))
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "Skyline-Tool Resource Inspector"))

(defmethod initialize-instance :after ((frame gui-inspector-frame) &key)
  (ensure-printer-discovery-started)
  (setf (frame-watcher-thread frame) (start-file-watcher frame))
  ;; Subscribe to resource events so the inspector redraws when resources change
  (dolist (event-type '(:resource-added :resource-changed :resource-removed
                        :resource-cache-dump :resource-scan-complete))
    (subscribe event-type
               (lambda (event)
                 (declare (ignore event))
                 (ignore-errors
                  (clim:redisplay-frame-panes frame :force-p t))))))

(clim:define-command (com-import-resource :command-table clim-internals::global-command-table
                                          :menu t :name t)
    ()
  "Import a resource from a file."
  (error "Import not implemented for this type."))

(clim:define-command (com-save-resource :command-table clim-internals::global-command-table
                                          :menu t :name t)
   ()
   "Save the resource to its current file."
   (let* ((frame clim:*application-frame*)
          (resource (inspector-resource frame)))
     (when resource
       (save-resource resource)
       (publish-resource-changed resource)
       (clim:redisplay-frame-panes frame :force-p t))))

(clim:define-command (com-save-as-json :command-table clim-internals::global-command-table
                                       :menu t :name t)
    ()
  "Export resource as JSON."
  (let* ((frame clim:*application-frame*)
         (resource (inspector-resource frame))
         (path (prompt-save-pathname 
                (format nil "~a.sky.json" (game-resource-title resource))
                :type "sky.json"
                :prefs-key (list "save-path"
                                 (string (class-name (class-of resource)))
                                 "json"))))
    (when path
      (export-resource-to-json-file resource path)
      (clim-simple-echo:run-in-simple-echo (lambda () (format t "Exported JSON to ~a" path))))))

(clim:define-command (com-save-as-text :command-table clim-internals::global-command-table
                                       :menu t :name t)
    ()
  "Export resource as plain text."
  (let* ((frame clim:*application-frame*)
         (resource (inspector-resource frame))
         (path (prompt-save-pathname (format nil "~a.txt" (game-resource-title resource)))))
    (when path
      (export-resource-to-text-file resource path)
      (clim-simple-echo:run-in-simple-echo (lambda () (format t "Exported text to ~a" path))))))

(clim:define-command (com-save-as-ods :command-table clim-internals::global-command-table
                                      :menu t :name t)
    ()
  "Export resource as ODS (spreadsheet)."
  (error "ODS export is not implemented."))

(clim:define-command (com-save-as-odt :command-table clim-internals::global-command-table
                                      :menu t :name t)
    ()
  "Export resource as ODT (word processor)."
  (error "ODT export is not implemented."))

(clim:define-command (com-save-as-pdf :command-table clim-internals::global-command-table
                                      :menu t :name t)
    ()
  "Export resource as PDF via PostScript."
  (let* ((frame clim:*application-frame*)
         (resource (inspector-resource frame)))
    (unless resource
      (error "No resource to export — this command requires an inspector with an assigned resource."))
    (let ((path (prompt-save-pathname (format nil "~a.pdf" (game-resource-title resource)))))
      (when path
        (export-resource-to-ps-file resource path
                                    :title (game-resource-title resource)
                                    :author (user-homedir-pathname))
        (uiop:run-program (list "ps2pdf" path (make-pathname :type "pdf" :defaults path)) :output nil)
        (clim-simple-echo:run-in-simple-echo (lambda () (format t "Exported PDF to ~a" path)))))))

(clim:define-command (com-toggle-vc-tracked :command-table clim-internals::global-command-table
                                            :menu t :name t)
    ()
  "Toggle version control tracking status."
  (error "Version control toggle not implemented."))

(clim:define-command (com-toggle-vc-staged :command-table clim-internals::global-command-table
                                           :menu t :name t)
    ()
  "Toggle version control staging status."
  (error "Version control toggle not implemented."))

(clim:define-command (com-toggle-vc-ignored :command-table clim-internals::global-command-table
                                            :menu t :name t)
    ()
  "Toggle version control ignore status."
  (error "Version control toggle not implemented."))

(clim:define-command (com-toggle-project-pane :command-table clim-internals::global-command-table
                                              :menu t :name t)
    ()
  "Toggle project pane visibility. Only available in All Resources view."
  (let* ((frame clim:*application-frame*)
         (resource (inspector-resource frame)))
    (unless (and resource (game-resource-pathnames resource))
      (error "Project pane is only available for resources with file backing (e.g., game assets with pathnames)."))))

(clim:define-command (com-inspector-toggle-view :command-table clim-internals::global-command-table
                                                :menu t :name t)
    ()
  "Toggle between editing and reference view modes."
  (let* ((frame clim:*application-frame*)
         (current (frame-view-mode frame))
         (new (if (eq current :editing) :reference :editing)))
    (setf (frame-view-mode frame) new)
    (clim:redisplay-frame-panes frame :force-p t)))

(clim:define-command (com-send-to-backup :command-table clim-internals::global-command-table
                                          :menu t :name t)
    ()
  "Send resource as backup by copying to ~/Backups/"
  (let* ((frame clim:*application-frame*)
         (resource (inspector-resource frame)))
    (when resource
      (let ((backup-dir (merge-pathnames #p"Backups/Skyline-Tool/" (user-homedir-pathname))))
        (ensure-directories-exist backup-dir)
        (let* ((title (game-resource-title resource))
               (timestamp (format-timestring nil (get-universal-time)
                                              :format '(:year "-" :month "-" :day "T"
                                                        :hour ":" :min ":" :sec)))
               (backup-path (merge-pathnames (format nil "~a-~a.backup" title timestamp)
                                             backup-dir)))
          (export-resource-to-json-file resource backup-path)
          (format t "~&Backed up ~a to ~a~%" title backup-path))))))

(clim:define-command (com-print-to-default :command-table clim-internals::global-command-table
                                           :menu t :name t)
    ()
  (let* ((frame clim:*application-frame*)
         (resource (inspector-resource frame)))
    (when resource
      (let ((printer (first (get-printer-list))))
        (if printer
            (progn
              (print-to-printer resource printer)
              (clim-simple-echo:run-in-simple-echo (lambda () (format t "Printing ~a to ~a..." (game-resource-title resource) printer)))
              (clim-simple-echo:run-in-simple-echo (lambda () (format t "No printers found"))))))))

  (clim:define-command (com-print-to-specific :command-table clim-internals::global-command-table
                                              :menu t :name t)
      ()
    (let* ((frame clim:*application-frame*)
           (resource (inspector-resource frame))
           (printers (get-printer-list)))
      (if printers
          (let* ((choice (error "CLIM:ACCEPT was used (which is not allowed)")))
            (if (member choice printers :test #'string-equal)
                (print-to-printer resource choice)
                (clim-simple-echo:run-in-simple-echo (lambda () (format t "Invalid printer selected")))))
          (clim-simple-echo:run-in-simple-echo (lambda () (format t "No printers found"))))))

  (clim:define-command (com-refresh-printers :command-table clim-internals::global-command-table
                                             :menu t :name t)
      ()
    (clim-simple-echo:run-in-simple-echo (lambda () (format t "Printer list refreshed")))
    (clim:redisplay-frame-panes clim:*application-frame* :force-p t))

  (defun print-to-printer (resource printer-name)
    "Print RESOURCE to PRINTER-NAME by exporting to PostScript and sending to the printer."
    (unless (find printer-name (get-printer-list) :test #'string-equal)
      (error "Printer ~a not found. Available printers: ~a" printer-name (get-printer-list)))
    (uiop:with-temporary-file (:stream stream :pathname temp-file)
      (export-resource-to-ps-file resource temp-file
                                  :title (game-resource-title resource)
                                  :author (user-homedir-pathname))
      (uiop:run-program (list "lp" "-d" printer-name (namestring temp-file)) :output nil)))

  (clim:define-command (com-run-resource :command-table clim-internals::global-command-table
                                         :menu t :name t)
      ()
    "Run the resource (e.g., compile, export)."
    (error "Run not implemented for this type."))

  (clim:define-command (com-open-in-gimp :command-table clim-internals::global-command-table
                                         :menu t :name t)
      ()
    "Open resource in GIMP."
    (error "GIMP integration not implemented."))

  (clim:define-command (com-convert-to-png :command-table clim-internals::global-command-table
                                           :menu t :name t)
      ()
    "Convert resource to PNG."
    (error "PNG conversion not implemented."))

  (clim:define-command (com-compile-to-source :command-table clim-internals::global-command-table
                                              :menu t :name t)
      ()
    "Compile resource to source."
    (error "Compilation not implemented."))

  (clim:define-command (com-show-rom-budget :command-table clim-internals::global-command-table
                                            :menu t :name t)
      ()
    "Show ROM budget by building the default target and opening the budget viewer."
    (submit-task
     (lambda ()
       (build-target #p"Dist/7800/Phantasia.Demo.NTSC.a78")
       (show-rom-budget)))))

;; 
;; Frame lifecycle: start/stop watcher
;; 
(defmethod finalize-instance :after ((frame gui-inspector-frame))
  (stop-file-watcher frame))

;; 
;; Display functions (reuse from gui-presentations.lisp)
;; 
(defun display-resource-preview (frame pane)
  (let* ((resource (slot-value frame 'frame-resource))
         (view (frame-view-mode frame)))
    (when resource
      (let ((*standard-output* pane))
        (ecase view
          (:reference (present-reference resource pane))
          (:reading   (present-reading resource pane))
          (:editable  (present-editing resource pane)))))))

(defun display-resource-status (frame pane)
  (let* ((resource (slot-value frame 'frame-resource))
         (file-path (when resource
                      (or (game-resource-full-path resource)
                          (ignore-errors (game-resource-collective-path resource)))))
         (vc-status (when file-path
                      (vc-file-status file-path))))
    (when resource
      (let ((*standard-output* pane))
        (format pane "~a | ~a | VC: ~a | View: ~a"
                (game-resource-kind resource)
                (game-resource-title resource)
                (or vc-status "unknown")
                (string-downcase (symbol-name (frame-view-mode frame))))))))

;; 
;; Generic inspector opener (already exists in gui-presentations.lisp)
;; 
;; We rely on open-resource-inspector from gui-presentations.lisp.
;; Specific inspectors should define open-<kind>-inspector that calls it.

(clim:define-presentation-type game-resource-reference ()
  :inherit-from 'game-resource)

(clim:define-presentation-type game-resource-editing ()
  :inherit-from 'game-resource-reference)

(clim:define-presentation-type game-resource-viewing ()
  :inherit-from 'game-resource-reference)

(defgeneric open-resource-inspector (resource &optional mode)
  (:documentation "Open an inspector window for RESOURCE. MODE can be :editing (default), :reading, or :reference."))

(defmethod open-resource-inspector ((resource game-resource) &optional (mode :editing))
  (declare (ignore mode))
  (error "Inspector not implemented for this resource type"))

(defmethod open-resource-inspector :around ((resource game-resource) &optional (mode :editing))
  "Launch every inspector in its own thread so the calling frame stays responsive."
  (declare (ignore mode))
  (bt:make-thread (lambda ()
                    (call-next-method))
                  :name (format nil "Inspector: ~a" (ignore-errors (game-resource-title resource)))))

;; 
;; External tool execution helper
;; 
(defun make-external-run-command (tool args)
  (run-command-in-terminal-echo (list tool args)
                                :title (format nil "Running tool ~a" tool)))

;; Also add reference and reading view commands to the inspector view menu
(clim:define-command (com-inspector-view-reference :command-table clim-internals::global-command-table
                                                   :menu t :name t) ()
  "Switch to reference view"
  (setf (frame-view-mode clim:*application-frame*) :reference)
  (clim:redisplay-frame-panes clim:*application-frame* :force-p t))

(clim:define-command (com-inspector-view-reading :command-table clim-internals::global-command-table
                                                 :menu t :name t) ()
  "Switch to reading view"
  (setf (frame-view-mode clim:*application-frame*) :reading)
  (clim:redisplay-frame-panes clim:*application-frame* :force-p t))

