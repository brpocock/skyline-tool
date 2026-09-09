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
(in-package :skyline-tool)

(defun load-prototype-file (path)
  "Load a JSON prototype file, return the decoded alist."
  (when (probe-file path)
    (let ((*package* (find-package :keyword)))
      (cl-json:decode-json-from-string
       (alexandria:read-file-into-string path)))))

(defun save-prototype-file (path data)
  "Save DATA (alist) as pretty-printed JSON to PATH."
  (with-open-file (f path :direction :output :if-exists :supersede
                      :external-format :utf-8)
    (write-json-pretty data f)))

(clim:define-application-frame prototype-inspector-frame (resource-inspector-mixin clim:standard-application-frame)
  ((path :initarg :path :accessor frame-path)
   (data :initarg :data :accessor frame-data))
  (:menu-bar prototype-inspector-menu-bar)
  (:panes
   (inspector-pane :application :display-function 'display-prototype
                   :height 600 :width 500 :scroll-bars :vertical)
   (interactor :interactor :height 80 :width 500))
  (:layouts (default (clim:vertically () inspector-pane interactor))))

(clim:define-command-table prototype-inspector-help-menu
  :menu (("How to Edit Prototypes" :command com-help-for-window)
         ("Skyline-Tool Developers' Guide" :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide" :command com-open-scripting-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table prototype-inspector-menu-bar
  :menu (("File" :menu (("Save" :command com-proto-save)
                         (nil :divider :line)
                         ("Close" :command com-proto-close)))
         ("Edit" :menu (("Edit Field..." :command com-proto-edit)))
         ("Help" :menu prototype-inspector-help-menu)))

(defmethod display-inspector-content ((frame prototype-inspector-frame) pane)
  (display-prototype frame pane))

(defun display-prototype (frame pane)
  (clim:window-clear pane)
  (let ((data (frame-data frame)))
    (format pane "~&Prototype Fields:~2%")
    (loop for (key . value) in data
          for i from 0
          do (format pane "~&~3d. ~a = ~a~%" i
                     (cl-change-case:title-case (string key))
                     (if (stringp value) value
                         (with-output-to-string (s)
                           (write-json-pretty (list (cons key value)) s)))))))

(clim:define-command (com-proto-edit :command-table clim-internals::global-command-table
                                      :menu nil :name t)
    ((index 'integer :gesture :select))
  (let* ((frame (and (boundp 'clim:*application-frame*) clim:*application-frame*))
         (data (and frame (frame-data frame)))
         (path (and frame (frame-path frame))))
    (when (and data path (<= 0 index (1- (length data))))
      (let* ((pair (nth index data))
             (key (car pair))
             (old (cdr pair))
             (str-old (if (stringp old) old (princ-to-string old)))
             (new (clim:accept 'string :prompt (format nil "~a" key) :default str-old)))
        (when (and new (plusp (length (string-trim " " new))))
          (let ((parsed (ignore-errors (cl-json:decode-json-from-string new))))
            (setf (cdr (nth index (frame-data frame)))
                  (if parsed parsed new))
            (save-prototype-file path (frame-data frame))
            (clim:redisplay-frame-panes frame)))))))

(clim:define-command (com-proto-save :menu t :name t) ()
  (let* ((frame (and (boundp 'clim:*application-frame*) clim:*application-frame*))
         (data (and frame (frame-data frame)))
         (path (and frame (frame-path frame))))
    (when (and data path)
      (save-prototype-file path data)
      (format *query-io* "~&Saved.~%"))))

(clim:define-command (com-proto-close :menu t :name t) ()
  (let ((frame (and (boundp 'clim:*application-frame*) clim:*application-frame*)))
    (when frame (clim:frame-exit frame))))

(defun open-prototype-inspector (path)
  "Open a prototype JSON file in the inspector."
  (let* ((data (load-prototype-file path))
         (resource (make-instance 'game-resource-object-prototype
                                  :moniker (pathname-name path)
                                 
                                  :full-path (truename path)))
         (fm (clim:find-frame-manager :port (or (clim:find-port) (clim:find-port :server-path :x))))
         (frame (clim:make-application-frame
                  'prototype-inspector-frame
                  :resource resource
                  :pretty-name (format nil "Prototype: ~a" (pathname-name path))
                  :path path :data data :frame-manager fm)))
    (clim:run-frame-top-level frame)))
