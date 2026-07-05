;;; Skyline-Tool src/gui/gui-script-inspector.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-application-frame game-resource-script-inspector (gui-inspector-frame)
  ((resource :initarg :resource :reader frame-resource))
  (:menu-bar inspector-script-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "Script Inspector"))

(defmethod initialize-instance :after ((frame game-resource-script-inspector) &key)
  (call-next-method))

;; ------------------------------------------------------------------------
;; Inspector open helper
;; ------------------------------------------------------------------------
(defun open-script-inspector (resource)
  (open-resource-inspector (or resource (make-instance 'game-resource-script)) :editing))

;; Specialize open-resource-inspector for script resources
(defmethod open-resource-inspector ((resource game-resource-script) &optional (mode :editing))
  (clim:run-frame-top-level
   (make-application-frame 'game-resource-script-inspector
                           :resource resource
                           :view-mode mode)))

;; ------------------------------------------------------------------------
;; Menus attached to the inspector
;; ------------------------------------------------------------------------
(clim:define-command-table inspector-script-file-menu
  :menu (("New Script..."    :command com-new-script-from-menu)
         ("Script from File..." :command com-open-script)
         (nil :divider :line)
         ("Save"             :command com-save-script)
         ("Close"            :command com-close-frame)))

(clim:define-command-table inspector-script-run-menu
  :menu (("Make PDF..."    :command com-make-script-pdf)   ; external LaTeX → PDF
         ("Compile Script..." :command com-compile-script) ; internal Skyline‑Tool compile
         (nil :divider :line)
         ("Close"          :command com-close-frame)))

(clim:define-command-table inspector-script-menu-bar
  :menu (("File"      :menu inspector-script-file-menu)
         ("Edit"      :menu inspector-edit-menu)
         ("View"      :menu inspector-view-menu)
         ("Run"       :menu inspector-script-run-menu)
         ("Help"      :menu inspector-help-menu)))

;; ------------------------------------------------------------------------
;; Commands used by the menus
;; ------------------------------------------------------------------------
(clim:define-command (com-make-script-pdf :command-table clim-internals::global-command-table
                                          :menu t :name t)
  ((resource 'game-resource-script :gesture :select))
  "Export the SCRIPT to a PDF (LaTeX → PDF) using an external PTYXIS call."
  (let* ((txt (game-resource-full-path resource))
         (pdf  (make-output-path resource :pdf)))
    (uiop:run-program
     (list "ptyxis" "-s" "-x" "pdflatex" "-interaction=nonstopmode"
           "-output-directory" (pathname-directory pdf)
           (pathname-name txt))
     :output nil
     :ignore-error-status t)))

(clim:define-command (com-compile-script :command-table clim-internals::global-command-table
                                         :menu t :name t)
  ((resource 'game-resource-script :gesture :select))
  "Compile the SCRIPT using Skyline-Tool's internal compile‑script routine."
  (clim-simple-echo:run-in-simple-echo
   (format nil "compile-script ~a" (game-resource-moniker resource))))

(clim:define-command (com-new-script-from-menu :command-table clim-internals::global-command-table
                                               :menu t :name t)
  ()
  "Create a new script resource."
  (clim-simple-echo:run-in-simple-echo "New script not implemented."))

(clim:define-command (com-open-script :command-table clim-internals::global-command-table
                                      :menu t :name t)
  ()
  "Open a script from file."
  (clim-simple-echo:run-in-simple-echo "Open script not implemented."))

(clim:define-command (com-save-script :command-table clim-internals::global-command-table
                                       :menu t :name t)
  ()
  "Save the script resource."
  (let* ((frame clim:*application-frame*)
         (resource (slot-value frame 'frame-resource)))
    (when resource
      (clim-simple-echo:run-in-simple-echo (format nil "Saving ~a..." (game-resource-title resource))))))