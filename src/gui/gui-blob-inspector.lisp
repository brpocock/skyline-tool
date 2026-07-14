;;; Skyline-Tool src/gui/gui-blob-inspector.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

;; Specialize open-resource-inspector for blob resources to use the blob-specific frame
(defmethod open-resource-inspector ((resource game-resource-blob) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'game-resource-blob-inspector
                                :resource resource
                                :view-mode mode)))

(clim:define-application-frame game-resource-blob-inspector (gui-inspector-frame clim:standard-application-frame)
  ()
  (:menu-bar inspector-blob-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "Blob Inspector"))

;; ------------------------------------------------------------------------
;; Inspector open helper
;; ------------------------------------------------------------------------
(defun open-blob-inspector (resource)
  (open-resource-inspector (or resource (make-instance 'game-resource-blob
                                                        :kind "Blob"
                                                        :full-path nil
                                                        :moniker "Blobs/new-blob.xcf")) :editing))

;; ------------------------------------------------------------------------
;; Menus attached to the inspector
;; ------------------------------------------------------------------------
(clim:define-command-table inspector-blob-file-menu
  :menu (("New..."          :command com-new-blob)
         ("Import..."       :command com-open-blob)
         (nil :divider :line)
         ("Save"            :command com-save-blob)
         ("Version"         :command com-version-pane
          :menu (("Tracked"  :command com-track-resource
                   :toggle t)
                 ("Staged"   :command com-stage-resource
                   :toggle t)
                 (nil :divider :line)
                 ("Ignored"  :command com-ignore-resource
                   :toggle t)))
         ("Save as"         :menu (("JSON..." :command com-save-as-json)
                                    ("Text..." :command com-save-as-text)
                                    ("PDF..."  :command com-save-as-pdf)
                                    (nil :divider :line)
                                    ("PNG..."  :command com-save-as-png)))
         (nil :divider :line)
         ("Send to"         :command com-send-to-p2p)
         ("Print to"        :command com-print-to)
         (nil :divider :line)
         ("Close"          :command com-close-frame)))

(clim:define-command-table inspector-blob-run-menu
  :menu (("Make PNG..."      :command com-make-blob-png)
         ("Compile Blob..."   :command com-compile-blob)))

(clim:define-command-table inspector-blob-help-menu
  :menu (("How to Inspect a BLOB..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table inspector-blob-menu-bar
  :menu (("BLOB"     :menu inspector-blob-file-menu)
         ("Edit"     :menu inspector-edit-menu)
         ("View"     :menu inspector-view-menu)
         ("Run"      :menu inspector-blob-run-menu)
         ("Help"     :menu inspector-blob-help-menu)))

;; ------------------------------------------------------------------------
;; Commands used by the Run menu
;; ------------------------------------------------------------------------
(clim:define-command (com-make-blob-png :command-table clim-internals::global-command-table
                                        :menu t :name t)
  ((resource 'game-resource-blob :gesture :select :default (inspector-resource clim:*application-frame*)))
  "Export the BLOB's XCF to PNG via GIMP (runs inside a PTYXIS session)."
  (let* ((xcf-path (first (game-resource-pathnames resource)))
         (png-path  (make-output-path resource :png)))   ; builds "SomeName.png"
    (uiop:run-program
     (list "ptyxis" "-s" "-x" "gimp" "-i" "-b" 
           (format nil "(gimp-file-save RUN-NONINTERACTIVE ~s ~s) (gimp-quit 0)" xcf-path png-path)
           xcf-path png-path)
     :output nil
     :ignore-error-status t)))

(clim:define-command (com-compile-blob :command-table clim-internals::global-command-table
                                       :menu t :name t)
  ((resource 'game-resource-blob :gesture :select :default (inspector-resource clim:*application-frame*)))
  "Compile the BLOB using blob-rip-7800 on the exported PNG."
  (let* ((png-path (make-output-path resource :png))
         (png-pathname (if (pathnamep png-path) png-path (pathname png-path))))
    (unless (probe-file png-pathname)
      (error "PNG file not found: ~a. Please export PNG first via 'Make PNG...'." png-pathname))
    (clim-simple-echo:run-in-simple-echo
     (lambda ()
       (format t "blob-rip-7800 ~a~%" (namestring png-pathname))))))

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
  (error "Open blob not implemented."))

(clim:define-command (com-save-blob :command-table clim-internals::global-command-table
                                    :menu t :name t)
  ()
  "Save the blob resource."
  (let* ((frame clim:*application-frame*)
         (resource (inspector-resource frame)))
    (when resource
      (error (format nil "Saving ~a..." (game-resource-title resource))))))
