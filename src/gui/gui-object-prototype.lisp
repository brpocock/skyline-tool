;;; Skyline-Tool src/gui/gui-object-prototype.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-object-prototype-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-object-prototype-reference)))
  (typep object 'game-resource-object-prototype))

(clim:define-presentation-type game-resource-object-prototype-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-object-prototype-editable)))
  (typep object 'game-resource-object-prototype))

(clim:define-presentation-type game-resource-object-prototype-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-object-prototype-viewing)))
  (typep object 'game-resource-object-prototype))

(defmethod present-reference ((resource game-resource-object-prototype) stream)
  (clim:with-output-as-presentation (stream resource 'game-resource-object-prototype-reference)
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
        (format stream "~a" (game-resource-kind resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Path: "))
      (clim:formatting-cell (stream :align-x :left)
        (if (typep resource 'game-resource-from-file)
            (princ (game-resource-full-path resource) stream)
            (format stream "(none)"))))))

(defmethod present-editing ((resource game-resource-object-prototype) stream)
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
        (format stream "Kind: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a (read-only)" (game-resource-kind resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Path: "))
      (clim:formatting-cell (stream :align-x :left)
        (if (typep resource 'game-resource-from-file)
            (princ (game-resource-full-path resource) stream)
            (format stream "(none)"))))))

(clim:define-command-table object-prototype-file-menu
  :menu (("New..." :command com-new-object-prototype)
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

(clim:define-command-table object-prototype-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table object-prototype-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :toggle t)
         (nil :divider :line)
         ("Project Pane" :command com-toggle-project-pane :toggle t)))

(clim:define-command-table object-prototype-build-menu
  :menu (("Demo" :command com-build-object-prototype-demo)
         ("Public" :command com-build-object-prototype-public)
         ("$(Publisher)" :command com-build-object-prototype-publisher)))

(clim:define-command-table object-prototype-region-menu
  :menu (("NTSC" :command com-region-ntsc-object-prototype)
         ("PAL" :command com-region-pal-object-prototype)
         ("SECAM" :command com-region-secam-object-prototype)))

(clim:define-command-table object-prototype-run-menu
  :menu (("Build" :menu object-prototype-build-menu)
         ("Region" :menu object-prototype-region-menu)
         (nil :divider :line)
         ("Show ROM Budget..." :command com-show-rom-budget)))

(clim:define-command-table object-prototype-help-menu
  :menu (("How to Manage Object Prototypes..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table object-prototype-menu-bar
  :menu (("Object" :menu object-prototype-file-menu)
         ("Edit" :menu object-prototype-edit-menu)
         ("View" :menu object-prototype-view-menu)
         ("Run" :menu object-prototype-run-menu)
         ("Help" :menu object-prototype-help-menu)))

(clim:define-application-frame game-resource-object-prototype-inspector (gui-inspector-frame clim:standard-application-frame)
  ((resource :initarg :resource :reader frame-resource))
  (:menu-bar object-prototype-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "Object Prototype Inspector"))

(defmethod initialize-instance :after ((frame game-resource-object-prototype-inspector) &key)
  (call-next-method)
  (subscribe :resource-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

(defun open-object-prototype-inspector (resource)
  (open-resource-inspector (or resource (make-instance 'game-resource-object-prototype)) :editing))

(defmethod open-resource-inspector ((resource game-resource-object-prototype) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'game-resource-object-prototype-inspector
                                :resource resource
                                :pretty-name (format nil "~a — ~a ~a"
                                                     (game-resource-title resource)
                                                     *game-title* (machine-directory-name))
                                :view-mode mode)))

(clim:define-command (com-new-object-prototype :command-table clim-internals::global-command-table :menu t :name t) ()
  (open-object-prototype-inspector nil))

(clim:define-command (com-build-object-prototype-demo :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building object prototype (Demo)...~%"))))

(clim:define-command (com-build-object-prototype-public :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building object prototype (Public)...~%"))))

(clim:define-command (com-build-object-prototype-publisher :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building object prototype (Publisher)...~%"))))

(clim:define-command (com-region-ntsc-object-prototype :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :ntsc)
  (publish :region-changed :payload :ntsc))

(clim:define-command (com-region-pal-object-prototype :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :pal)
  (publish :region-changed :payload :pal))

(clim:define-command (com-region-secam-object-prototype :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :secam)
  (publish :region-changed :payload :secam))

(defmethod game-resource-action-menu ((resource game-resource-object-prototype))
  (list
   (make-menu-item "Inspect..." (lambda () (open-object-prototype-inspector resource)))))
