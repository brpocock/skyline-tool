;;; Skyline-Tool src/gui/gui-pascal-routine.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-routine-rc-pascal-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-routine-rc-pascal-reference)))
  (typep object 'game-resource-routine-rc-pascal))

(clim:define-presentation-type game-resource-routine-rc-pascal-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-routine-rc-pascal-editable)))
  (typep object 'game-resource-routine-rc-pascal))

(clim:define-presentation-type game-resource-routine-rc-pascal-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-routine-rc-pascal-viewing)))
  (typep object 'game-resource-routine-rc-pascal))

(defmethod present-reference ((resource game-resource-routine-rc-pascal) stream)
  (clim:with-output-as-presentation (stream resource 'game-resource-routine-rc-pascal-reference)
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

(defmethod present-reading ((resource game-resource-routine-rc-pascal) stream)
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
        (format stream "~a (Pascal/RC)" (game-resource-kind resource))))))

(defmethod present-editing ((resource game-resource-routine-rc-pascal) stream)
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

(clim:define-command-table pascal-routine-file-menu
  :menu (("New..." :command com-new-pascal-routine)
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

(clim:define-command-table pascal-routine-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table pascal-routine-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :toggle t)
         (nil :divider :line)
         ("Project Pane" :command com-toggle-project-pane :toggle t)))

(clim:define-command-table pascal-routine-build-menu
  :menu (("Demo" :command com-build-pascal-routine-demo)
         ("Public" :command com-build-pascal-routine-public)
         ("$(Publisher)" :command com-build-pascal-routine-publisher)))

(clim:define-command-table pascal-routine-region-menu
  :menu (("NTSC" :command com-region-ntsc-pascal-routine)
         ("PAL" :command com-region-pal-pascal-routine)
         ("SECAM" :command com-region-secam-pascal-routine)))

(clim:define-command-table pascal-routine-run-menu
  :menu (("Build" :menu pascal-routine-build-menu)
         ("Region" :menu pascal-routine-region-menu)
         (nil :divider :line)
         ("Compile..." :command com-compile-pascal-routine)))

(clim:define-command-table pascal-routine-help-menu
  :menu (("How to Manage Pascal Routines..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table pascal-routine-menu-bar
  :menu (("Pascal" :menu pascal-routine-file-menu)
         ("Edit" :menu pascal-routine-edit-menu)
         ("View" :menu pascal-routine-view-menu)
         ("Run" :menu pascal-routine-run-menu)
         ("Help" :menu pascal-routine-help-menu)))

(clim:define-application-frame pascal-routine-inspector-frame (gui-inspector-frame clim:standard-application-frame)
  ((resource :initarg :resource :reader frame-resource))
  (:menu-bar pascal-routine-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "Pascal Routine Inspector"))

(defmethod initialize-instance :after ((frame pascal-routine-inspector-frame) &key)
  (call-next-method)
  (subscribe :resource-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

(defun open-pascal-routine-inspector (resource)
  (open-resource-inspector (or resource (make-instance 'game-resource-routine-rc-pascal)) :editing))

(defmethod open-resource-inspector ((resource game-resource-routine-rc-pascal) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'pascal-routine-inspector-frame
                                :resource resource
                                :pretty-name (format nil "~a — ~a ~a"
                                                     (game-resource-title resource)
                                                     *game-title* (machine-directory-name))
                                :view-mode mode)))

(clim:define-command (com-new-pascal-routine :command-table clim-internals::global-command-table :menu t :name t) ()
  (open-pascal-routine-inspector nil))

(clim:define-command (com-compile-pascal-routine :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Compiling Pascal routine...~%"))))

(clim:define-command (com-build-pascal-routine-demo :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building Pascal routine (Demo)...~%"))))

(clim:define-command (com-build-pascal-routine-public :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building Pascal routine (Public)...~%"))))

(clim:define-command (com-build-pascal-routine-publisher :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building Pascal routine (Publisher)...~%"))))

(clim:define-command (com-region-ntsc-pascal-routine :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :ntsc)
  (publish :region-changed :payload :ntsc))

(clim:define-command (com-region-pal-pascal-routine :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :pal)
  (publish :region-changed :payload :pal))

(clim:define-command (com-region-secam-pascal-routine :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :secam)
  (publish :region-changed :payload :secam))

(defmethod game-resource-action-menu ((resource game-resource-routine-rc-pascal))
  (list
   (make-menu-item "Inspect..." (lambda () (open-pascal-routine-inspector resource)))
   (make-menu-item "Open in Emacs..."
                   (lambda ()
                     (open-in-emacs (game-resource-full-path resource))))))
