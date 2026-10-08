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
  :menu ())

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

;; Define submenu for default printer fallback (used by populate-print-menu)
(clim:define-command-table default-printer-submenu
  :menu (("Default Printer (lpr)" :command com-print-to-default)))

;; Define printer and p2p menus for All Resources frame
(clim:define-command-table printer-menu
  :menu ())

(clim:define-command-table p2p-sharing-menu
  :menu ())

;; 
;; Shared Submenu Population Functions
;; These populate dynamic submenus (Print To, Send To, Build, Region)
;; and should be called in initialize-instance :after of each inspector frame.
;;

(defun populate-print-menu (command-table)
  (ensure-printer-scavenger-is-running)
  ;; Remove stale printer entries
  (dolist (printer *ipp-printer-registry*)
    (ignore-errors (clim:remove-menu-item-from-command-table
                    command-table (ipp-name (cdr printer)))))
  (ignore-errors (clim:remove-menu-item-from-command-table
                  command-table "Default Printer (lpr)"))
  ;; Add current printer entries
  (dolist (printer *ipp-printer-registry*)
    (let* ((struct (cdr printer))
           (display (ipp-name struct)))
      (clim:add-menu-item-to-command-table
       command-table display
       :command `(com-print-to-specific ,struct)
       :after :end)))
  ;; Add default printer fallback
  (clim:add-menu-item-to-command-table
   command-table "Default Printer (lpr)"
   :command 'com-print-to-default
   :after :end))

(defun populate-send-to-menu (command-table)
  "Populate COMMAND-TABLE with discovered p2p recipients via mDNS."
  (dolist (recipient (ignore-errors (discover-p2p-recipients)))
    (let ((name (car recipient))
          (service (cdr recipient)))
      (unless (clim:find-menu-item name command-table)
        (clim:add-menu-item-to-command-table
         command-table name
         :command `(com-send-to-p2p ,(first service) ,(second service))
         :after :end)))))

(defun populate-inspector-build-menu (command-table)
  "Populate COMMAND-TABLE with available build targets.
Reflects the current project configuration (e.g., Demo/Public/Publisher builds).
Call this when build targets change or on initial window creation."
  (dolist (build (or (ignore-errors (project-build-targets *project.json*))
                     '("Demo" "Public" "Publisher")))
    (let ((label build))
      (unless (clim:find-menu-item label command-table)
        (clim:add-menu-item-to-command-table
         command-table label
         :command `(com-build-target ,build)
         :after :end)))))

(clim:define-command (com-build-target :command-table clim-internals::global-command-table
                                       :menu t :name t)
    ((build-name 'string :prompt "Build target"))
  "Set the active build target and rebuild."
  (setf *build* (make-keyword (string-upcase build-name)))
  (format *query-io* "~&Build target set to ~a~%" build-name))

(defun populate-inspector-region-menu (command-table)
  "Populate COMMAND-TABLE with available region targets.
Reflects the current project configuration (NTSC/PAL/SECAM).
Call this when region targets change or on initial window creation."
  (dolist (region (or (ignore-errors (project-region-targets *project.json*))
                      '("NTSC" "PAL" "SECAM")))
    (unless (clim:find-menu-item region command-table)
      (clim:add-menu-item-to-command-table
       command-table region
       :command `(com-build-region ,region)
       :after :end))))

(clim:define-command (com-build-region :command-table clim-internals::global-command-table
                                       :menu t :name t)
    ((region-name 'string :prompt "Region"))
  "Set the active region and rebuild."
  (setf *region* (make-keyword (string-upcase region-name)))
  (publish :region-changed :payload *region*)
  (format *query-io* "~&Region set to ~a~%" region-name))

;; 
;; Uniform Inspector Base Class
;; Provides standardized layout, display functions, menu infrastructure,
;; thread management, and eventbus subscription for all resource type inspectors
;;
(defclass uniform-inspector-frame (clim:standard-application-frame)
  ((watcher-thread :initform nil :accessor frame-watcher-thread)
   (eventbus-subscriber :initform nil :accessor frame-eventbus-subscriber)
   (view-mode :initform :editing :initarg :view-mode :accessor frame-view-mode)))

(defmethod initialize-instance :after ((frame uniform-inspector-frame) &key)
  "Handle post-initialization for uniform inspector frames."
  (call-next-method))

(defmethod initialize-instance :before ((frame uniform-inspector-frame) &key)
  (declare (ignore frame key)))

(defun setup-inspector-eventbus (frame)
  "Subscribe FRAME to resource change events for auto-redisplay.
Returns the subscriber function for later cleanup with teardown-inspector-eventbus."
   (let* ((event-types '(:resource-added :resource-changed :resource-removed
                          :resource-cache-dump :resource-scan-complete
                          :region-changed))
          (redisplay-handler (lambda (event)
                               (declare (ignore event))
                               (ignore-errors
                                (clim:redisplay-frame-panes frame :force-p t))))
          (printer-handler (lambda (event)
                             (declare (ignore event))
                             (populate-print-menu 'inspector-print-to-menu)
                             (ignore-errors
                              (clim:redisplay-frame-panes frame :force-p t)))))
     (dolist (event-type event-types)
       (subscribe event-type redisplay-handler))
     (subscribe :printer-list-changed printer-handler)
     (setf (frame-eventbus-subscriber frame)
           (list event-types redisplay-handler printer-handler))))

(defun teardown-inspector-eventbus (frame)
  "Remove FRAME's eventbus subscriptions."
  (let ((subscriber (frame-eventbus-subscriber frame)))
    (when subscriber
      (destructuring-bind (event-types redisplay-handler printer-handler) subscriber
        (dolist (event-type event-types)
          (unsubscribe event-type redisplay-handler))
        (unsubscribe :printer-list-changed printer-handler))
      (setf (frame-eventbus-subscriber frame) nil))))

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
  ()
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
  "Initialize inspector frame: start printer discovery, watcher, and eventbus subscriptions."
  (ensure-printer-discovery-started)
  (setf (frame-watcher-thread frame) (start-file-watcher frame))
  (populate-print-menu 'inspector-print-to-menu)
  (setup-inspector-eventbus frame))

(defmethod finalize-instance :after ((frame gui-inspector-frame))
  "Cleanup: stop watcher thread and remove eventbus subscriptions."
  (stop-file-watcher frame)
  (teardown-inspector-eventbus frame))

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
  "Toggle project pane visibility."
  (let ((frame clim:*application-frame*))
    (ignore-errors (clim:redisplay-frame-panes frame :force-p t))))

(clim:define-command (com-inspector-toggle-view :command-table clim-internals::global-command-table
                                                :menu t :name t)
    ()
  "Toggle between editing and reference view modes."
  (let* ((frame clim:*application-frame*)
         (current (frame-view-mode frame))
         (new (if (eq current :editing) :reference :editing)))
    (setf (frame-view-mode frame) new)
    (clim:redisplay-frame-panes frame :force-p t)))

(clim:define-command (com-print-to-default :command-table clim-internals::global-command-table
                                           :menu t :name t)
    ()
  (let* ((frame clim:*application-frame*)
         (resource (inspector-resource frame)))
    (when resource
      (pipe-to-lpr nil resource)
      (clim-simple-echo:run-in-simple-echo
       (lambda () (format t "Printing ~a to default printer..." (game-resource-title resource)))))))

(clim:define-command (com-print-to-specific :command-table clim-internals::global-command-table
                                            :menu t :name t)
    ((printer t))
  (let* ((frame clim:*application-frame*)
         (resource (inspector-resource frame)))
    (when resource
      (if (typep printer 'ipp-printer)
          (pipe-to-ipp printer resource)
          (pipe-to-lpr printer resource))
      (clim-simple-echo:run-in-simple-echo
       (lambda () (format t "Printing ~a to ~a..." (game-resource-title resource)
                          (if (typep printer 'ipp-printer) (ipp-name printer) printer)))))))

(clim:define-command (com-refresh-printers :command-table clim-internals::global-command-table
                                         :menu t :name t)
    ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "Printer list refreshed")))
  (clim:redisplay-frame-panes clim:*application-frame* :force-p t))

(defun print-to-printer (resource printer-name)
  "Print RESOURCE to PRINTER-NAME via pipe-to-lpr.
PRINTER-NAME can be a queue name string or nil for default printer."
  (pipe-to-lpr printer-name resource))

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
     (build-target (make-pathname :directory (list :relative "Dist" (machine-directory-name))
                                  :name (format nil "~a.~a~a"
                                                *game-title*
                                                (ecase *build*
                                                  (:demo "Demo")
                                                  (:public "Public")
                                                  (:publisher "AA" #| TODO |# ))
                                                (case *region*
                                                  (:ntsc ".NTSC")
                                                  (:pal ".PAL")
                                                  (:secam ".SECAM")
                                                  (otherwise "")))
                                  :type (emulator-binary-extension)))
     (show-rom-budget))))

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
  (make-window-thread (format nil "Inspector: ~a" (ignore-errors (game-resource-title resource)))
                      (lambda () (call-next-method))))

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

