;;; Skyline-Tool src/gui/gui-inspector.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(defparameter *inspector-view* :editing
  "Current view mode for inspectors: :reference, :reading, or :editing")

(defun make-output-path (resource extension)
  "Build the output pathname from the resource's moniker.
   (make-output-path resource :png) → #P\"SomeName.png\""
  (let* ((basename (game-resource-moniker resource))
         (stem (substitute #\_ #\- basename))
         (type (case extension
                 (:png "png")
                 (:tmx "tmx")
                 (:pdf "pdf")
                 (:ogg "ogg")
                 (t (string-downcase (format nil "~a" extension))))))
    (make-pathname :type type :name stem :defaults (game-resource-pathname resource))))

(defun start-file-watcher (frame)
  "Start a background thread that polls the resource's file for changes
and updates the inspector when the modification time changes."
  (let ((resource (slot-value frame 'frame-resource)))
    (unless (null resource)
      (let ((path (game-resource-full-path resource))
            (last-mod (when path (file-write-date path))))
        (make-thread
         (lambda ()
           (loop
             (when path
               (let ((cur-mod (ignore-errors (file-write-date path))))
                 (when (and cur-mod last-mod (not (= cur-mod last-mod)))
                   (setf last-mod cur-mod)
                   (clim:redisplay-frame-panes frame :force-p t))))
             (sleep 1)))) ;; poll every second
        :name "file-watcher"))))

(defun stop-file-watcher (frame)
  "Stop the file watcher thread if running."
  ;; In a real implementation we would keep track of the thread and terminate it.
  ;; For simplicity, we rely on the thread exiting when the frame is closed.
  ;; This function is a placeholder.
  (declare (ignore frame))
  nil)

(clim:define-application-frame gui-inspector-frame (resource-inspector-mixin clim:standard-application-frame)
   ((view-mode :initform :editable :initarg :view-mode :accessor frame-view-mode)
    (watcher-thread :initform nil :accessor frame-watcher-thread)
    (frame-resource :accessor frame-resource :initform nil))
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "Skyline-Tool Resource Inspector")
  (:panes
   (content :application
            :display-function 'display-resource-preview
            :scroll-bars :vertical
            :height 600 :width 800)
   (status-bar :application
               :display-function 'display-resource-status
               :height 30 :width 800))
  (:layouts (default (clim:vertically () content status-bar)))
  (:menu-bar inspector-menu-bar))

;; ------------------------------------------------------------------------
;; Menu definitions
;; ------------------------------------------------------------------------
(clim:define-command-table inspector-file-menu
  :menu (("New..."          :command com-new-resource)
         ("Import..."      :command com-import-resource)
         (nil :divider :line)
         ("Save"           :command com-save-resource)
         ("Save as >"      :menu inspector-save-as-menu)
         (nil :divider :line)
         ("Send to >"      :menu inspector-send-to-menu)
         ("Print to >"     :menu inspector-print-to-menu)
         (nil :divider :line)
         ("Close"          :command com-close-frame)))

(clim:define-command-table inspector-save-as-menu
  :menu (("JSON..."   :command com-save-as-json)
         ("ODS..."    :command com-save-as-ods)
         ("ODT..."    :command com-save-as-odt)
         ("PDF..."    :command com-save-as-pdf)))

(clim:define-command-table inspector-send-to-menu
  :menu (("Email..."   :command com-send-to-email)
         ("Repository":command com-send-to-repo)))

(clim:define-command-table inspector-print-to-menu
  :menu (("Default Printer":command com-print-to-default)
         ("Specific..."   :command com-print-to-specific)))

(clim:define-command-table inspector-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table inspector-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :radio t)))

(clim:define-command-table inspector-run-menu
  :menu (("Run..." :command com-run-resource))) ; placeholder, overridden per inspector

(clim:define-command-table inspector-help-menu
  :menu (("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table inspector-menu-bar
  :menu (("File" :menu inspector-file-menu)
         ("Edit" :menu inspector-edit-menu)
         ("Run"  :menu inspector-run-menu)
         ("View" :menu inspector-view-menu)
         ("Help" :menu inspector-help-menu)))

;; ------------------------------------------------------------------------
;; View toggle command
;; ------------------------------------------------------------------------
(clim:define-command (com-inspector-toggle-view :command-table clim-internals::global-command-table
                                                :menu t :name t) ()
  "Toggle between editable and read-only views"
  (let* ((frame clim:*application-frame*)
         (new-view (if (eq (frame-view-mode frame) :editable)
                       :read-only
                       :editable)))
    (setf (frame-view-mode frame) new-view)
    (clim:redisplay-frame-panes frame :force-p t)))

;; ------------------------------------------------------------------------
;; File menu commands (stubs)
;; ------------------------------------------------------------------------
(clim:define-command (com-new-resource :command-table clim-internals::global-command-table
                                      :menu t :name t)
  ()
  "Create a new resource of the inspector's type."
  (clim-simple-echo:run-in-simple-echo "New resource not implemented for this type."))

(clim:define-command (com-import-resource :command-table clim-internals::global-command-table
                                         :menu t :name t)
  ()
  "Import a resource from a file."
  (clim-simple-echo:run-in-simple-echo "Import not implemented for this type."))

(clim:define-command (com-save-resource :command-table clim-internals::global-command-table
                                        :menu t :name t)
  ()
  "Save the resource to its current file."
  (let* ((frame clim:*application-frame*)
         (resource (slot-value frame 'frame-resource)))
    (when resource
      (clim-simple-echo:run-in-simple-echo (format nil "Saving ~a..." (game-resource-title resource)))
      ;; In a real implementation we would call a save method on the resource.
      )))

(clim:define-command (com-save-as-json :command-table clim-internals::global-command-table
                                       :menu t :name t)
  ()
  "Export resource as JSON."
  (let* ((frame clim:*application-frame*)
         (resource (slot-value frame 'frame-resource))
         (path (clim:accept 'pathname :prompt "Save JSON as:"
                            :default (make-pathname :name (game-resource-moniker resource)
                                                    :type "json"
                                                    :defaults (or (game-resource-pathname resource)
                                                                    (user-homedir-pathname))))))
    (when path
      (export-resource-to-json-file resource path)
      (clim-simple-echo:run-in-simple-echo (format nil "Exported JSON to ~a" path)))))

(clim:define-command (com-save-as-ods :command-table clim-internals::global-command-table
                                     :menu t :name t)
  ()
  "Export resource as ODS (spreadsheet)."
  (clim-simple-echo:run-in-simple-echo "ODS export not yet implemented."))

(clim:define-command (com-save-as-odt :command-table clim-internals::global-command-table
                                     :menu t :name t)
  ()
  "Export resource as ODT (word processor)."
  (clim-simple-echo:run-in-simple-echo "ODT export not yet implemented."))

(clim:define-command (com-save-as-pdf :command-table clim-internals::global-command-table
                                     :menu t :name t)
  ()
  "Export resource as PDF via PostScript."
  (let* ((frame clim:*application-frame*)
         (resource (slot-value frame 'frame-resource))
         (path (clim:accept 'pathname :prompt "Save PDF as:"
                            :default (make-pathname :name (game-resource-moniker resource)
                                                    :type "pdf"
                                                    :defaults (or (game-resource-pathname resource)
                                                                    (user-homedir-pathname))))))
    (when path
      (export-resource-to-ps-file resource path
                                  :title (game-resource-title resource)
                                  :author (user-homedir-pathname))
      (uiop:run-program (list "ps2pdf" path (make-pathname :type "pdf" :defaults path)) :output nil)
      (clim-simple-echo:run-in-simple-echo (format nil "Exported PDF to ~a" path)))))

(clim:define-command (com-send-to-email :command-table clim-internals::global-command-table
                                       :menu t :name t)
  ()
  "Send resource via email."
  (clim-simple-echo:run-in-simple-echo "Send to email not implemented."))

(clim:define-command (com-send-to-repo :command-table clim-internals::global-command-table
                                      :menu t :name t)
  ()
  "Send resource to version control repository."
  (clim-simple-echo:run-in-simple-echo "Send to repository not implemented."))

(clim:define-command (com-print-to-default :command-table clim-internals::global-command-table
                                          :menu t :name t)
  ()
  "Print resource to default printer."
  (let* ((frame clim:*application-frame*)
         (resource (slot-value frame 'frame-resource)))
    (when resource
      (clim-simple-echo:run-in-simple-echo (format nil "Printing ~a..." (game-resource-title resource)))
      ;; Actual printing would use lp or similar.
      )))

(clim:define-command (com-print-to-specific :command-table clim-internals::global-command-table
                                           :menu t :name t)
  ()
  "Print resource to a selected printer."
  (clim-simple-echo:run-in-simple-echo "Specific printer selection not implemented."))

(clim:define-command (com-run-resource :command-table clim-internals::global-command-table
                                      :menu t :name t)
  ()
  "Run the resource (e.g., compile, export)."
  (clim-simple-echo:run-in-simple-echo "Run not implemented for this type."))

;; ------------------------------------------------------------------------
;; Frame lifecycle: start/stop watcher
;; ------------------------------------------------------------------------
(defmethod initialize-instance :after ((frame gui-inspector-frame) &key)
  (setf (frame-watcher-thread frame) (start-file-watcher frame)))

(defmethod finalize-instance :after ((frame gui-inspector-frame))
  (stop-file-watcher frame))

;; ------------------------------------------------------------------------
;; Display functions (reuse from gui-presentations.lisp)
;; ------------------------------------------------------------------------
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
         (vc-status (when resource
                      (vc-file-status (or (game-resource-full-path resource)
                                          (game-resource-collective-path resource))))))
    (when resource
      (let ((*standard-output* pane))
        (format pane "~a | ~a | VC: ~a | View: ~a"
                (game-resource-kind resource)
                (game-resource-moniker resource)
                (or vc-status "unknown")
                (string-downcase (symbol-name (frame-view-mode frame))))))))

;; ------------------------------------------------------------------------
;; Generic inspector opener (already exists in gui-presentations.lisp)
;; ------------------------------------------------------------------------
;; We rely on open-resource-inspector from gui-presentations.lisp.
;; Specific inspectors should define open-<kind>-inspector that calls it.

(clim:define-presentation-type game-resource-reference ()
  :inherit-from 'game-resource)

(clim:define-presentation-type game-resource-editing ()
  :inherit-from 'game-resource-reference)

(clim:define-presentation-type game-resource-viewing ()
  :inherit-from 'game-resource-reference)

(defgeneric open-resource-inspector (resource &optional mode)
  (:documentation "Open an inspector window for RESOURCE.
   MODE can be :editing (default) or :reading"))

(defmethod open-resource-inspector (resource &optional (mode :editing))
  (declare (ignore mode))
  (error "Inspector not implemented for this resource type"))

;; ------------------------------------------------------------------------
;; External tool execution helper
;; ------------------------------------------------------------------------
(defun make-external-run-command (tool args &key (input-path nil) (output-path nil))
  "Construct a PTYXIS command to run an external tool.
   TOOL: the external tool name (e.g., \"gimp\", \"tiled\", \"musescore3\")
   ARGS: list of arguments to pass to the tool
  
  
   INPUT-PATH: optional input file path
   OUTPUT-PATH: optional output file path
   Returns a list suitable for UIOP:RUN-PROGRAM."
  (let ((cmd (list "ptyxis" "-s" "-x" tool)))
    (when input-path
      (push (namestring input-path) cmd))
    (when output-path
      (push (namestring output-path) cmd))
    (append cmd args)))

;; Fix the global *inspector-view* to use frame's view-mode
(clim:define-command (com-inspector-toggle-view :command-table clim-internals::global-command-table
                                                :menu t :name t) ()
  "Toggle between editable and read-only views"
  (let* ((frame clim:*application-frame*)
         (new-view (if (eq (frame-view-mode frame) :editable)
                       :read-only
                       :editable)))
    (setf (frame-view-mode frame) new-view)
    (clim:redisplay-frame-panes frame :force-p t)))

