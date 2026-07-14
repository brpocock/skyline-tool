(in-package :skyline-tool)

(defun %show-resource-context-menu (resource)
  "Show context menu for a resource entry - opens the appropriate inspector."
  (let ((kind (game-resource-kind resource)))
    (case kind
      ("Characters"
       (open-character-inspector resource))
      ("Scripts"
       (open-script-inspector resource))
      ("Sprite Sheets"
       (open-sprite-sheet-inspector resource))
      ("Maps"
       (open-map-inspector resource))
      ("Songs"
       (open-song-inspector resource))
      ("Blobs"
       (open-blob-inspector resource))
      ("Tilesets"
       (open-tileset-inspector resource))
      ("Objects"
       (open-object-prototype-inspector resource))
      ("Classes"
       (open-class-inspector resource))
      ("Routines"
       (open-routine-inspector resource))
      ("Items"
       (open-item-inspector resource))
      ("Flags"
       (open-flag-inspector resource))
      ("Keys"
       (open-key-inspector resource))
      ("Boats"
       (open-boat-inspector resource))
      ("Instruments"
       (open-instrument-inspector resource))
      (t
       (list
        (cons "Open in Editor" (lambda () (format t "Open in editor: ~a~%" (cerror "fuck that guy, that is so dumb" "some fucking moron thought there was a moniker on ~s"  resource))))
        (cons "Open in File Manager" (lambda () (format t "Open in file manager: ~a~%" (cerror "fuck that guy, that is so dumb" "some fucking moron thought there was a moniker on ~s"  resource))))
        (cons "Delete" (lambda () (format t "Delete: ~a~%" (cerror "fuck that guy, that is so dumb" "some fucking moron thought there was a moniker on ~s"  resource)))))))))

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
