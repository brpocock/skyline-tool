;;; Skyline-Tool src/gui/gui-basic-routine.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-routine-rc-basic-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-routine-rc-basic-reference)))
  (typep object 'game-resource-routine-rc-basic))

(clim:define-presentation-type game-resource-routine-rc-basic-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-routine-rc-basic-editable)))
  (typep object 'game-resource-routine-rc-basic))

(clim:define-presentation-type game-resource-routine-rc-basic-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-routine-rc-basic-viewing)))
  (typep object 'game-resource-routine-rc-basic))

(defmethod present-reference ((resource game-resource-routine-rc-basic) stream)
  (clim:with-output-as-presentation (stream resource 'game-resource-routine-rc-basic-reference)
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

(defmethod present-reading ((resource game-resource-routine-rc-basic) stream)
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
        (format stream "~a (BASIC/RC)" (game-resource-kind resource))))))

(defmethod present-editing ((resource game-resource-routine-rc-basic) stream)
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

(clim:define-command-table basic-routine-file-menu
  :menu (("New..." :command com-new-basic-routine)
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

(clim:define-command-table basic-routine-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table basic-routine-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :toggle t)
         (nil :divider :line)
         ("Project Pane" :command com-toggle-project-pane :toggle t)))

(clim:define-command-table basic-routine-build-menu
  :menu (("Demo" :command com-build-basic-routine-demo)
         ("Public" :command com-build-basic-routine-public)
         ("$(Publisher)" :command com-build-basic-routine-publisher)))

(clim:define-command-table basic-routine-region-menu
  :menu (("NTSC" :command com-region-ntsc-basic-routine)
         ("PAL" :command com-region-pal-basic-routine)
         ("SECAM" :command com-region-secam-basic-routine)))

(clim:define-command-table basic-routine-run-menu
  :menu (("Build" :menu basic-routine-build-menu)
         ("Region" :menu basic-routine-region-menu)
         (nil :divider :line)
         ("Compile..." :command com-compile-basic-routine)))

(clim:define-command-table basic-routine-help-menu
  :menu (("How to Manage BASIC Routines..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table basic-routine-menu-bar
  :menu (("BASIC" :menu basic-routine-file-menu)
         ("Edit" :menu basic-routine-edit-menu)
         ("View" :menu basic-routine-view-menu)
         ("Run" :menu basic-routine-run-menu)
         ("Help" :menu basic-routine-help-menu)))

(clim:define-application-frame basic-routine-inspector-frame (gui-inspector-frame clim:standard-application-frame)
  ((resource :initarg :resource :reader frame-resource))
  (:menu-bar basic-routine-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "BASIC Routine Inspector"))

(defmethod initialize-instance :after ((frame basic-routine-inspector-frame) &key)
  (call-next-method)
  (subscribe :resource-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

(defun open-basic-routine-inspector (resource)
  (open-resource-inspector (or resource (make-instance 'game-resource-routine-rc-basic)) :editing))

(defmethod open-resource-inspector ((resource game-resource-routine-rc-basic) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'basic-routine-inspector-frame
                                :resource resource
                                :pretty-name (format nil "~a — ~a ~a"
                                                     (game-resource-title resource)
                                                     *game-title* (machine-directory-name))
                                :view-mode mode)))

(clim:define-command (com-new-basic-routine :command-table clim-internals::global-command-table :menu t :name t) ()
  (open-basic-routine-inspector nil))

(clim:define-command (com-compile-basic-routine :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Compiling BASIC routine...~%"))))

(clim:define-command (com-build-basic-routine-demo :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building BASIC routine (Demo)...~%"))))

(clim:define-command (com-build-basic-routine-public :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building BASIC routine (Public)...~%"))))

(clim:define-command (com-build-basic-routine-publisher :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building BASIC routine (Publisher)...~%"))))

(clim:define-command (com-region-ntsc-basic-routine :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :ntsc)
  (publish :region-changed :payload :ntsc))

(clim:define-command (com-region-pal-basic-routine :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :pal)
  (publish :region-changed :payload :pal))

(clim:define-command (com-region-secam-basic-routine :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :secam)
  (publish :region-changed :payload :secam))

(defmethod game-resource-action-menu ((resource game-resource-routine-rc-basic))
  (list
   (make-menu-item "Inspect..." (lambda () (open-basic-routine-inspector resource)))
   (make-menu-item "Open in Emacs..."
                   (lambda ()
                     (open-in-emacs (game-resource-full-path resource))))))
