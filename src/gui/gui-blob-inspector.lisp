;;; Skyline-Tool src/gui/gui-blob-inspector.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-application-frame game-resource-blob-inspector (gui-inspector-frame)
  ((resource :initarg :resource :reader frame-resource))
  (:menu-bar inspector-blob-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "Blob Inspector"))

;; ------------------------------------------------------------------------
;; Inspector open helper
;; ------------------------------------------------------------------------
(defun open-blob-inspector (resource)
  (open-resource-inspector (or resource (make-instance 'game-resource-blob)) :editing))

;; Specialize open-resource-inspector for blob resources to use the blob-specific frame
(defmethod open-resource-inspector ((resource game-resource-blob) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'game-resource-blob-inspector
                                :resource resource
                                :view-mode mode)))

(defmethod initialize-instance :after ((frame game-resource-blob-inspector) &key)
  (declare (ignore key))
  ;; Blob-specific initialization without call-next-method
  (format t "Blob Inspector initialized~%"))

;; ------------------------------------------------------------------------
;; Menus attached to the inspector
;; ------------------------------------------------------------------------
(clim:define-command-table inspector-blob-file-menu
  :menu (("New Blob..."      :command com-new-blob)
         ("Blob from File..." :command com-open-blob)
         (nil :divider :line)
         ("Save"           :command com-save-blob)
         ("Close"          :command com-close-frame)))

(clim:define-command-table inspector-blob-run-menu
  :menu (("Make PNG..."      :command com-make-blob-png)   ; external GIMP export
         ("Compile Blob..."   :command com-compile-blob)    ; blob-rip-7800
         (nil :divider :line)
         ("Close"            :command com-close-frame)))

(clim:define-command-table inspector-blob-menu-bar
  :menu (("File"     :menu inspector-blob-file-menu)
         ("Edit"     :menu inspector-edit-menu)
         ("View"     :menu inspector-view-menu)
         ("Run"      :menu inspector-blob-run-menu)
         ("Help"     :menu inspector-help-menu)))

;; ------------------------------------------------------------------------
;; Commands used by the Run menu
;; ------------------------------------------------------------------------
(clim:define-command (com-make-blob-png :command-table clim-internals::global-command-table
                                        :menu t :name t)
  ((resource 'game-resource-blob :gesture :select))
  "Export the BLOB's XCF to PNG via GIMP (runs inside a PTYXIS session)."
  (let* ((xcf-path (first (game-resource-pathnames resource)))
         (png-path  (make-output-path resource :png)))   ; builds “SomeName.png”
    (uiop:run-program
     (list "ptyxis" "-s" "-x" "gimp" "-i" "-b" 
           (format nil "(gimp-file-save RUN-NONINTERACTIVE ~s ~s) (gimp-quit 0)" xcf-path png-path)
           xcf-path png-path)
     :output nil
     :ignore-error-status t)))

(clim:define-command (com-compile-blob :command-table clim-internals::global-command-table
                                      :menu t :name t)
  ((resource 'game-resource-blob :gesture :select))
  "Compile the BLOB using blob-rip-7800 on the exported PNG."
  (let* ((png-path (make-output-path resource :png))
         (png-pathname (if (pathnamep png-path) png-path (pathname png-path))))
    (unless (probe-file png-pathname)
      (error "PNG file not found: ~a. Please export PNG first via 'Make PNG...'." png-pathname))
    (clim-simple-echo:run-in-simple-echo
     (format nil "blob-rip-7800 ~a" (namestring png-pathname)))))

;; Placeholder commands for File menu (to be implemented)
(clim:define-command (com-new-blob :command-table clim-internals::global-command-table
                                   :menu t :name t)
  ()
  "Create a new blob resource."
  (open-blob-inspector nil))

(clim:define-command (com-open-blob :command-table clim-internals::global-command-table
                                   :menu t :name t)
  ()
  "Open a blob from file."
  (clim-simple-echo:run-in-simple-echo "Open blob not implemented."))

(clim:define-command (com-save-blob :command-table clim-internals::global-command-table
                                   :menu t :name t)
  ()
  "Save the blob resource."
  (let* ((frame clim:*application-frame*)
         (resource (slot-value frame 'frame-resource)))
    (when resource
      (clim-simple-echo:run-in-simple-echo (format nil "Saving ~a..." (game-resource-title resource))))))