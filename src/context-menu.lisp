(in-package :skyline-tool)

(defun find-existing-frame (frame-name)
  "Find an existing frame by name, or return NIL."
  (let ((fm (clim:find-frame-manager)))
    (when fm
      (ignore-errors (find-frame fm frame-name)))))

(defun open-sprite-sheet-inspector (&optional resource)
  "Inspector for sprite sheet resources"
  (let ((frame (find-existing-frame 'sprite-sheet-inspector-frame)))
    (unless frame
      (setf frame (clim:make-application-frame
                   'sprite-sheet-inspector-frame
                   :pretty-name "Sprite Sheet Inspector")))
    (when resource
      (setf (slot-value frame 'frame-resource) resource))
    (clim:run-frame-top-level frame)))

(defun open-script-inspector (&optional resource)
  "Inspector for script resources"
  (let ((frame (find-existing-frame 'script-inspector-frame)))
    (unless frame
      (setf frame (clim:make-application-frame
                   'script-inspector-frame
                   :pretty-name "Script Inspector")))
    (when resource
      (setf (slot-value frame 'frame-resource) resource))
    (clim:run-frame-top-level frame)))
