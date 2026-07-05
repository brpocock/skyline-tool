;;; Skyline-Tool src/gui/gui-map-inspector.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-application-frame game-resource-map-inspector (gui-inspector-frame)
  ((resource :initarg :resource :reader frame-resource))
  (:menu-bar inspector-map-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "Map Inspector"))

;; ------------------------------------------------------------------------
;; Inspector open helper
;; ------------------------------------------------------------------------
(defun open-map-inspector (resource)
  (open-resource-inspector (or resource (make-instance 'game-resource-map)) :editing))

;; Specialize open-resource-inspector for map resources
(defmethod open-resource-inspector ((resource game-resource-map) &optional (mode :editing))
  (clim:run-frame-top-level
   (make-application-frame 'game-resource-map-inspector
                           :resource resource
                           :view-mode mode)))

;; ------------------------------------------------------------------------
;; Menus attached to the inspector
;; ------------------------------------------------------------------------
(clim:define-command-table inspector-map-file-menu
  :menu (("New Map..."       :command com-new-map)
         ("Map from File..." :command com-open-map)
         (nil :divider :line)
         ("Save"             :command com-save-map)
         ("Close"            :command com-close-frame)))

(clim:define-command-table inspector-map-run-menu
  :menu (("Make TMX..."   :command com-make-map-tmx)   ; external Tiled export
         ("Compile Map..." :command com-compile-map)    ; internal compiler
         (nil :divider :line)
         ("Close"          :command com-close-frame)))

(clim:define-command-table inspector-map-menu-bar
  :menu (("File"     :menu inspector-map-file-menu)
         ("Edit"     :menu inspector-edit-menu)
         ("View"     :menu inspector-view-menu)
         ("Run"      :menu inspector-map-run-menu)
         ("Help"     :menu inspector-help-menu)))

;; ------------------------------------------------------------------------
;; Commands used by the menus
;; ------------------------------------------------------------------------
(clim:define-command (com-make-map-tmx :command-table clim-internals::global-command-table
                                       :menu t :name t)
  ((resource 'game-resource-map :gesture :select))
  "Export the MAP to a Tiled (.tmx) file."
  (let* ((src (game-resource-full-path resource))
         (dst (make-output-path resource :tmx)))
    (uiop:run-program
     (list "ptyxis" "-s" "-x" "tiled" "-b" 
           (format nil "tiled-export ~a -o ~a" src dst)
           src dst)
     :output nil
     :ignore-error-status t)))

(clim:define-command (com-compile-map :command-table clim-internals::global-command-table
                                      :menu t :name t)
  ((resource 'game-resource-map :gesture :select))
  "Compile the MAP using Skyline-Tool's internal map compiler."
  (clim-simple-echo:run-in-simple-echo
   (format nil "compile-map ~a" (game-resource-moniker resource))))

(clim:define-command (com-new-map :command-table clim-internals::global-command-table
                                 :menu t :name t)
  ()
  "Create a new map resource."
  (clim-simple-echo:run-in-simple-echo "New map not implemented."))

(clim:define-command (com-open-map :command-table clim-internals::global-command-table
                                   :menu t :name t)
  ()
  "Open a map from file."
  (clim-simple-echo:run-in-simple-echo "Open map not implemented."))

(clim:define-command (com-save-map :command-table clim-internals::global-command-table
                                   :menu t :name t)
  ()
  "Save the map resource."
  (let* ((frame clim:*application-frame*)
         (resource (slot-value frame 'frame-resource)))
    (when resource
      (clim-simple-echo:run-in-simple-echo (format nil "Saving ~a..." (game-resource-title resource))))))