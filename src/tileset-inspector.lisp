;;;; Tileset Inspector
;;;; Copyright © 2026 Interworldly Adventuring, LLC.

(in-package :skyline-tool)

;; --- Tileset Inspector frame ---

(clim:define-application-frame tileset-inspector-frame (resource-inspector-mixin)
  ((%pathname :initform nil :accessor :accessor tileset-inspector-pathname)
   (%tileset :initform nil :accessor tileset-inspector-tileset))
  (:panes (tileset-pane :application :height 600 :width 800
                                    :display-function 'display-tileset-inspector)
          (interactor :interactor :height 100 :width 800
                                  :max-height 100))
  (:menu-bar tileset-inspector-menu-bar)
  (:icon (skyline-tool-icon))
  (:layouts (default (clim:vertically () tileset-pane interactor))))

;; --- Command tables ---

(clim:define-command-table tileset-inspector-file-menu
  :menu (("Close" :command com-close-tileset-inspector)))

(clim:define-command-table tileset-inspector-menu-bar
  :menu (("Tileset" :menu tileset-file-menu)))

;; --- Tileset Inspector commands ---

(clim:define-command (com-close-tileset-inspector :menu nil :name t) ()
  (clim:frame-exit clim:*application-frame*))

;; --- Tileset Inspector launcher entry function ---

(defun open-tileset-inspector (pathname)
  "Open a tileset inspector for the given TSX file."
  (let* ((resource (make-instance 'game-resource-tileset
                                  :moniker (pathname-name pathname)
                                  :kind "Tileset"
                                  :full-path (truename pathname)))
         (fm (clim:find-frame-manager :port (or (clim:find-port) (clim:find-port :server-path :x))))
         (frame (clim:make-application-frame 'tileset-inspector-frame
                                              :resource resource
                                              :pathname pathname
                                              :frame-manager fm
                                              :width 820 :height 700)))
    (clim-sys:make-process
     (lambda ()
       (let ((clim:*application-frame* frame))
         (setf (tileset-inspector-pathname frame) pathname)
         (setf (tileset-inspector-tileset frame) (load-tileset pathname))
         (clim:run-frame-top-level frame)))
     :name (format nil "Tileset Inspector ~a" (pathname-name pathname)))))

;; --- Display function ---

(defmethod display-inspector-content ((frame tileset-inspector-frame) pane)
  (display-tileset-inspector frame pane))

(defun display-tileset-inspector (frame pane)
  (declare (ignore frame))
  (clim:window-clear pane)
  (format pane "Tileset Inspector - not yet fully implemented~%"))