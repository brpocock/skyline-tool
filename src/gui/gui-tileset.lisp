;;; Skyline-Tool src/gui/gui-tileset.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-tileset-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-tileset-reference)))
  (typep object 'game-resource-tileset))

(clim:define-presentation-type game-resource-tileset-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-tileset-editable)))
  (typep object 'game-resource-tileset))

(clim:define-presentation-type game-resource-tileset-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-tileset-viewing)))
  (typep object 'game-resource-tileset))

(defmethod present-reference ((resource game-resource-tileset) stream)
  (clim:with-output-as-presentation (stream resource 'game-resource-tileset-reference)
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

(defmethod present-reading ((resource game-resource-tileset) stream)
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
        (format stream "~a" (game-resource-kind resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Path: "))
      (clim:formatting-cell (stream :align-x :left)
        (if (typep resource 'game-resource-from-file)
            (princ (game-resource-full-path resource) stream)
            (format stream "(none)"))))))

(defmethod present-editing ((resource game-resource-tileset) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
        (interactive-editing-gadget-with-validation
         stream resource
         :getter (lambda (r) (game-resource-title r))
         :setter (lambda (r v) (setf (game-resource-title r) v))
         :label "Name:"
         :validator #'validate-asset-name
         :max-length 200)))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Kind: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a (read-only)" (game-resource-kind resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Path: "))
      (clim:formatting-cell (stream :align-x :left)
        (if (typep resource 'game-resource-from-file)
            (princ (game-resource-pathnames resource) stream)
            (format stream "(none)"))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Tiles: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~d" (or (ignore-errors (tileset-tile-count resource)) 0))))))

(clim:define-command-table tileset-file-menu
  :menu (("New..." :command com-new-tileset)
         ("Import..." :command com-import-resource)
         (nil :divider :line)
         ("Save" :command com-save-resource)
         ("Version" :menu inspector-vc-menu)
         ("Save as" :menu inspector-save-as-menu)
         (nil :divider :line)
         ("Send to" :menu inspector-send-to-menu)
         ("Print to" :menu inspector-print-to-menu)
         (nil :divider :line)
         ("Close" :command com-close-frame)))

(clim:define-command-table tileset-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table tileset-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :toggle t)
         (nil :divider :line)
         ("Project Pane" :command com-toggle-project-pane :toggle t)))

(clim:define-command-table tileset-build-menu
  :menu (("Demo" :command com-build-tileset-demo)
         ("Public" :command com-build-tileset-public)
         ("$(Publisher)" :command com-build-tileset-publisher)))

(clim:define-command-table tileset-region-menu
  :menu (("NTSC" :command com-region-ntsc-tileset)
         ("PAL" :command com-region-pal-tileset)
         ("SECAM" :command com-region-secam-tileset)))

(clim:define-command-table tileset-run-menu
  :menu (("Build" :menu tileset-build-menu)
         ("Region" :menu tileset-region-menu)
         (nil :divider :line)
         ("Export PNG..." :command com-tileset-export-png)
         ("Open in Tiled..." :command com-tileset-open-tiled)))

(clim:define-command-table tileset-help-menu
  :menu (("How to Manage Tilesets..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table tileset-menu-bar
  :menu (("Tileset" :menu tileset-file-menu)
         ("Edit" :menu tileset-edit-menu)
         ("View" :menu tileset-view-menu)
         ("Run" :menu tileset-run-menu)
         ("Help" :menu tileset-help-menu)))

(clim:define-application-frame tileset-inspector-frame (gui-inspector-frame clim:standard-application-frame)
  ((resource :initarg :resource :reader frame-resource)
   (pathname :initarg :pathname :accessor frame-pathname :initform nil))
  (:menu-bar tileset-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "Tileset Inspector"))

(defmethod initialize-instance :after ((frame tileset-inspector-frame) &key)
  (call-next-method)
  (subscribe :resource-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

(defun open-tileset-inspector (resource)
  (open-resource-inspector (or resource (make-instance 'game-resource-tileset)) :editing))

(defmethod open-resource-inspector ((resource game-resource-tileset) &optional (mode :editing))
  (let ((path (game-resource-full-path resource)))
    (clim:run-frame-top-level
     (clim:make-application-frame 'tileset-inspector-frame
                                  :resource resource
                                  :pathname path
                                  :pretty-name (format nil "~a — ~a ~a"
                                                       (game-resource-title resource)
                                                       *game-title* (machine-directory-name))
                                  :view-mode mode
                                  :width 820 :height 700))))

(clim:define-command (com-new-tileset :command-table clim-internals::global-command-table :menu t :name t) ()
  (open-tileset-inspector nil))

(clim:define-command (com-tileset-export-png :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Exporting tileset to PNG...~%"))))

(clim:define-command (com-tileset-open-tiled :command-table clim-internals::global-command-table :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (resource (frame-resource frame)))
    (when resource
      (let ((path (game-resource-full-path resource)))
        (when (and path (probe-file path))
          (uiop:run-program (list "tiled" path) :output nil :ignore-error-status t))))))

(clim:define-command (com-build-tileset-demo :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building tileset (Demo)...~%"))))

(clim:define-command (com-build-tileset-public :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building tileset (Public)...~%"))))

(clim:define-command (com-build-tileset-publisher :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building tileset (Publisher)...~%"))))

(clim:define-command (com-region-ntsc-tileset :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :ntsc)
  (publish :region-changed :payload :ntsc))

(clim:define-command (com-region-pal-tileset :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :pal)
  (publish :region-changed :payload :pal))

(clim:define-command (com-region-secam-tileset :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :secam)
  (publish :region-changed :payload :secam))

(defmethod game-resource-action-menu ((resource game-resource-tileset))
  (list
   (make-menu-item "Inspect..." (lambda () (open-tileset-inspector resource)))
   (make-menu-item "Open in Tiled..."
                   (lambda ()
                     (let ((path (game-resource-full-path resource)))
                       (when (and path (probe-file path))
                         (uiop:run-program (list "tiled" path) :output nil :ignore-error-status t)))))))

;; --- Display function ---

(defmethod display-inspector-content ((frame tileset-inspector-frame) pane)
  (display-tileset-inspector frame pane))

(defun display-tileset-inspector (frame pane)
  (declare (ignore frame))
  (clim:window-clear pane)
  (format pane "Tileset Inspector - not yet started to be implemented~%"))
