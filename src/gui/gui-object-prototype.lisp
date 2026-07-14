;;; Skyline-Tool src/gui/gui-object-prototype.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-object-prototype-reference ()
  :inherit-from 'game-resource-object-prototype)

(defmethod present-reference ((resource game-resource-object-prototype) stream)
  (clim:with-output-as-presentation
    (stream resource 'game-resource-object-prototype-reference)
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

(defun open-object-prototype-inspector (resource)
  (open-resource-inspector (or resource (make-instance 'game-resource-object-prototype)) :editing))

(defmethod open-resource-inspector ((resource game-resource-object-prototype) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'game-resource-object-prototype-inspector
                                :resource resource
                                :view-mode mode)))

(clim:define-application-frame game-resource-object-prototype-inspector (gui-inspector-frame clim:standard-application-frame)
  ()
  (:menu-bar object-prototype-menu-bar)
  (:pretty-name "Object Prototype Inspector"))

(clim:define-command-table object-prototype-menu-bar
  :menu (("File" :menu inspector-file-menu)
          ("Edit" :menu inspector-edit-menu)
          ("View" :menu inspector-view-menu)
          ("Run" :menu object-prototype-run-menu)
          ("Help" :menu inspector-help-menu)))

(clim:define-command-table object-prototype-run-menu
  :menu (("Build" :command com-build-object-prototype)
          ("Show ROM Budget..." :command com-show-rom-budget)))

(clim:define-command (com-build-object-prototype :command-table clim-internals::global-command-table
                                                  :menu t :name t)
  ()
  "Build the object prototype."
  (let* ((frame clim:*application-frame*)
         (resource (frame-resource frame)))
    (when resource
      (clim-simple-echo:run-in-simple-echo
       (format nil "Building object prototype: ~a" (game-resource-title resource))))))

(defmethod present-editing ((resource game-resource-object-prototype) stream)
  (let ((name (game-resource-title resource)))
    (clim:formatting-table (stream)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Name: "))
        (clim:formatting-cell (stream :align-x :left)
          (let ((gadget (insert-gadget stream
                                       :label nil
                                       :variable name
                                       :activation-callback
                                       (lambda (gadget)
                                         (declare (ignore gadget))
                                         (error "Editing name not fully implemented")))))
            (declare (ignore gadget)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Kind: "))
        (clim:formatting-cell (stream :align-x :left)
          (format stream "~a (read-only)" (game-resource-kind resource))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Moniker: "))
        (clim:formatting-cell (stream :align-x :left)
          (format stream "~a" (cerror "fuck that guy, that is so dumb" "some fucking moron thought there was a moniker on ~s"  resource)))))))

(defmethod present-reading ((resource game-resource-object-prototype) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-title resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Kind: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-kind resource))))))
