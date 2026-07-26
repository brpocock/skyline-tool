;;; Skyline-Tool src/gui/gui-map.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-map-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-map-reference)))
  (typep object 'game-resource-map))

(clim:define-presentation-type game-resource-map-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-map-editable)))
  (typep object 'game-resource-map))

(clim:define-presentation-type game-resource-map-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-map-viewing)))
  (typep object 'game-resource-map))

(clim:define-presentation-method clim:present ((resource game-resource-map) (type game-resource-map-reference) stream view &key)
  (declare (ignore view))
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 0)
        (format stream "~3%"))
      (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 125)
        (game-resource-present-icon resource stream))
      (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 150)
        (clim:with-text-size (stream :larger)
          (clim:with-text-face (stream :bold)
            (game-resource-present-title resource stream)))
        (format stream "~%~5t")
        (clim:with-text-size (stream :smaller)
          (clim:with-drawing-options (stream :ink (clim:make-gray-color 0.75))
            (game-resource-present-subheading resource stream))))
      (clim:formatting-cell (stream :align-x :right :align-y :top :min-height 90 :min-width 125)
        (game-resource-present-right-margin resource stream)))))

(clim:define-presentation-method clim:present ((resource game-resource-map) (type game-resource-map-editable) stream view &key)
  (declare (ignore view))
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
        (interactive-editing-gadget-with-validation
         stream resource
         (lambda (r) (game-resource-title r))
         (lambda (r v) (rename-asset r v))
         :label "Name:"
         :validator #'validate-asset-name
         :max-length 200)))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Subdirectory: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a (changing subdirectory not supported)" (game-resource-locale resource))))))

(clim:define-presentation-method clim:present ((resource game-resource-map) (type game-resource-map-viewing) stream view &key)
   (declare (ignore view))
   (clim:surrounding-output-with-border (stream :shape :rounded)
     (format stream "[MAP ~a] ~a" (game-resource-asset-id resource)
             (game-resource-title resource))))

(defmethod present-editing ((resource game-resource-map) stream)
  (clim:present resource 'game-resource-map-editable :stream stream))

(defmethod present-reading ((resource game-resource-map) stream)
  (clim:present resource 'game-resource-map-viewing :stream stream))

(defmethod present-reference ((resource game-resource-map) stream)
  (clim:present resource 'game-resource-map-reference :stream stream))

(defun open-map-inspector (resource)
  (open-resource-inspector (or resource
                                (make-instance 'game-resource-map
                                  :kind "Map"
                                  :moniker "Maps/new-map.tmx")) :editing))

(defmethod game-resource-action-menu ((resource game-resource-map))
  (list
   (make-menu-item "Inspect..." (lambda () (open-map-inspector resource)))
   (make-menu-item "Open in Tiled..."
                   (lambda ()
                     (uiop:run-program (list "tiled" (or (game-resource-full-path resource) (game-asset-moniker resource)))
                                       :output nil :ignore-error-status t)))))

;;; Skyline-Tool src/gui/gui-map-inspector.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-map-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-map-reference)))
  (typep object 'game-resource-map))

(clim:define-presentation-type game-resource-map-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-map-editable)))
  (typep object 'game-resource-map))

(clim:define-presentation-type game-resource-map-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-map-viewing)))
  (typep object 'game-resource-map))

(defmethod present-reference ((resource game-resource-map) stream)
  (clim:with-output-as-presentation
      (stream resource 'game-resource-map-reference)
    (clim:formatting-table (stream)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 0)
          (format stream "~3%"))
        (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 125)
          (game-resource-present-icon resource stream))
        (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 150)
          (clim:with-text-size (stream :larger)
            (clim:with-text-face (stream :bold)
              (game-resource-present-title resource stream)))
          (format stream "~%~5t")
          (clim:with-text-size (stream :smaller)
            (clim:with-drawing-options (stream :ink (clim:make-gray-color 0.75))
              (game-resource-present-subheading resource stream))))
        (clim:formatting-cell (stream :align-x :right :align-y :top :min-height 90 :min-width 125)
          (game-resource-present-right-margin resource stream))))))

(defmethod present-reading ((resource game-resource-map) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-title resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Moniker: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (or (game-asset-moniker resource) "(none)"))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Locale: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (or (game-resource-locale resource) "—"))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Notes: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (or (game-resource-notes resource) "—"))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Asset ID: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (or (game-resource-asset-id resource) "—"))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Kind: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-kind resource))))))

(defmethod present-editing ((resource game-resource-map) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
        (interactive-editing-gadget-with-validation
         stream resource
         (lambda (r) (game-resource-title r))
         (lambda (r v) (setf (game-resource-title r) v))
         :label "Name:"
         :validator #'validate-asset-name
         :max-length 200)))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Moniker: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a (read-only)" (or (game-asset-moniker resource) "(none)"))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Locale: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a (read-only)" (or (game-resource-locale resource) "—"))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Notes: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a (read-only)" (or (game-resource-notes resource) "—"))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Asset ID: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a (read-only)" (or (game-resource-asset-id resource) "—"))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Kind: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a (read-only)" (game-resource-kind resource))))))

(clim:define-command-table inspector-map-build-menu
  :menu (("Demo" :command com-build-map-demo)
         ("Public" :command com-build-map-public)
         ("$(Publisher)" :command com-build-map-publisher)))

(clim:define-command-table inspector-map-region-menu
  :menu (("NTSC" :command com-region-ntsc-map)
         ("PAL" :command com-region-pal-map)
         ("SECAM" :command com-region-secam-map)))

(clim:define-command-table inspector-map-run-menu
  :menu (("Build" :menu inspector-map-build-menu)
         ("Region" :menu inspector-map-region-menu)
         (nil :divider :line)
         ("Make TMX..." :command com-make-map-tmx)
         ("Compile Map..." :command com-compile-map)))

(clim:define-command-table inspector-map-file-menu
  :menu (("New Map..." :command com-new-map)
         ("Map from File..." :command com-open-map)
         (nil :divider :line)
         ("Save" :command com-save-resource)
         ("Version" :menu inspector-vc-menu)
         ("Save as" :menu inspector-save-as-menu)
         (nil :divider :line)
         ("Send to" :menu inspector-send-to-menu)
         ("Print to" :menu inspector-print-to-menu)
         (nil :divider :line)
         ("Close" :command com-close-frame)))

(clim:define-command-table map-inspector-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table inspector-map-menu-bar
  :menu (("Map" :menu inspector-map-file-menu)
         ("Edit"     :menu map-inspector-edit-menu)
         ("View"     :menu inspector-view-menu)
         ("Run"      :menu inspector-map-run-menu)
         ("Help"     :menu inspector-help-menu)))

(clim:define-application-frame game-resource-map-inspector (gui-inspector-frame clim:standard-application-frame)
  ((resource :initarg :resource :reader frame-resource))
  (:menu-bar inspector-map-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "Map Inspector"))

(defmethod initialize-instance :after ((frame game-resource-map-inspector) &key)
  (call-next-method)
  (subscribe :region-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

(defun open-map-inspector (resource)
  (open-resource-inspector (or resource
                               (make-instance 'game-resource-map
                                              :kind "Map"
                                              :moniker "Maps/new-map.tmx")) :editing))

(defmethod open-resource-inspector ((resource game-resource-map) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'game-resource-map-inspector
                                :resource resource
                                :pretty-name (format nil "~a — ~a ~a"
                                                     (game-resource-title resource)
                                                     *game-title* (machine-directory-name))
                                :view-mode mode)))

(clim:define-command (com-make-map-tmx :command-table clim-internals::global-command-table :menu t :name t)
    ((resource 'game-resource-map :gesture :select))
  (let* ((src (game-resource-full-path resource))
         (dst (make-output-path resource :tmx)))
    (uiop:run-program
     (list "ptyxis" "-s" "-x" "tiled" "-b"
           (format nil "tiled-export ~a -o ~a" src dst)
           src dst)
     :output nil
     :ignore-error-status t)))

(clim:define-command (com-compile-map :command-table clim-internals::global-command-table :menu t :name t)
    ((resource 'game-resource-map :gesture :select))
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Compiling map...~%"))))

(clim:define-command (com-new-map :command-table clim-internals::global-command-table :menu t :name t) ()
  (open-map-inspector nil))

(clim:define-command (com-open-map :command-table clim-internals::global-command-table :menu t :name t) ()
  (error "Open map not implemented."))

(clim:define-command (com-build-map-demo :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building map (Demo)...~%"))))

(clim:define-command (com-build-map-public :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building map (Public)...~%"))))

(clim:define-command (com-build-map-publisher :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building map (Publisher)...~%"))))

(clim:define-command (com-region-ntsc-map :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :ntsc)
  (publish :region-changed :payload :ntsc))

(clim:define-command (com-region-pal-map :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :pal)
  (publish :region-changed :payload :pal))

(clim:define-command (com-region-secam-map :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :secam)
  (publish :region-changed :payload :secam))
