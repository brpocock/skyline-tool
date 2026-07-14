(clim:define-application-frame sprite-sheet-inspector-frame (resource-inspector-mixin clim:standard-application-frame)
   ((frame-resource :accessor frame-resource :initform nil))
   (:icon (skyline-tool-icon :resource :sprite-sheet))
   (:pretty-name "Sprite Sheet Inspector")
   (:documentation "Frame for inspecting sprite sheets."))

(defmethod initialize-instance :after ((frame sprite-sheet-inspector-frame) &key)
  (ensure-printer-discovery-started))