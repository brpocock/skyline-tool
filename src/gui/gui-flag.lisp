;;; Skyline-Tool src/gui/gui-flag.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)


#|

Flags inspector:


Flag ID: $~2,'0d # not editable per se

Name: ___________________ # minifont validation and length validation


|#

(clim:define-presentation-type game-resource-flag-reference ()
  :inherit-from 'game-resource-flag)

(defun insert-gadget (stream &key label variable activation-callback)
  "Insert a simple text field gadget into the stream."
  (declare (ignore label))
  (let ((gadget (clim:make-pane 'clim:text-field 
                                :value variable
                                :activate-callback activation-callback)))
    (clim:change-space-requirements gadget)
    gadget))

(defmethod present-reference ((resource game-resource-flag) stream)
    (clim:with-output-as-presentation
        (stream resource 'game-resource-flag-reference)
      (clim:formatting-table (stream)
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 0)
            (format stream "~3%"))
          (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 125)
            (game-resource-present-icon resource stream))
          (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 150)
            (clim:with-text-face (stream :bold)
              (game-resource-present-title resource stream))
            (format stream "~%~5t")
            (clim:with-text-size (stream :smaller)
              (clim:with-drawing-options (stream :ink (clim:make-gray-color 0.75))
                (game-resource-present-subheading resource stream))))
          (clim:formatting-cell (stream :align-x :right :align-y :top :min-height 90 :min-width 125)
            (game-resource-present-right-margin resource stream))))))

(clim:define-command-table flag-run-menu
  :menu (("Compile Flags..." :command com-compile-flags)))

(clim:define-command-table flag-view-menu
   :menu (("Editable" :command com-toggle-flag-editable :documentation "Toggle editable mode")))

(clim:define-command (com-compile-flags :command-table flag-run-menu
                                      :menu t :name t)
  "Compile flags for current resource."
  (format t "~&Compiling flags...~%"))

(clim:define-command (com-toggle-flag-editable :command-table flag-view-menu
                                             :menu t :name t)
  "Toggle resource editability"
  (format t "~&Toggle editability~%"))

(clim:define-command (com-new-flag :command-table inspector-file-menu
                               :menu t :name t)
  "Create new flag resource."
  (open-flag-inspector (make-instance 'game-resource-flag)))

(defmethod game-resource-present-icon ((resource game-resource-flag) stream)
  "Default icon for flags"
  (format stream "⚑"))

(defun open-flag-inspector (resource)
  (let* ((frame (clim:make-application-frame 'flag-inspector-frame
                                             :resource (or resource
                                                           (make-instance 'game-resource-flag
                                                                         :flag-id 0
                                                                         :name "New Flag"))
                                             :pretty-name "Flag Inspector"
                                             :view-mode :editing)))
    (clim:run-frame-top-level frame)))

(defmethod open-resource-inspector ((resource game-resource-flag) &optional (mode :editing))
  (open-flag-inspector resource))

(clim:define-application-frame flag-inspector-frame (gui-inspector-frame clim:standard-application-frame)
  ((frame-resource :initarg :resource :reader frame-resource
                   :initform nil))
  (:menu-bar flag-menu-bar)
  (:panes
   (content-pane :application :display-function 'display-inspector-content
                               :scroll-bars :vertical :height 600 :width 800)
   (status-pane :application :display-function 'display-inspector-status
                             :scroll-bars nil :height 30 :width 800))
  (:layouts
   (default (clim:vertically () content-pane status-pane)))
  (:pretty-name "Flag Inspector"))

(clim:define-command-table flag-menu-bar
  :menu (("Flag" :menu inspector-file-menu)
         ("Edit" :menu inspector-edit-menu)
         ("Run"  :menu flag-run-menu)
         ("View" :menu flag-view-menu)
         ("Help" :menu inspector-help-menu)))

(defmethod present-reading ((resource game-resource-flag) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Index "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "$~2,'0x (~d)"
                (game-resource-index resource)
                (game-resource-index resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-name resource))))))

(defmethod present-editing ((resource game-resource-flag) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Index "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "$~2,'0x (~d)"
                (game-resource-index resource)
                (game-resource-index resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
        (insert-gadget stream
                       :label nil
                       :variable (game-resource-name resource)
                       :activation-callback
                       (lambda (gadget)
                         (setf (game-resource-name resource)
                               (clim:gadget-value gadget))))))))