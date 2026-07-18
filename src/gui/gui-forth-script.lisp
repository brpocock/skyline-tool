;;; Skyline-Tool src/gui/gui-forth-script.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-routine-forth-library-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-routine-forth-library-reference)))
  (typep object 'game-resource-routine-forth-library))

(clim:define-presentation-type game-resource-routine-forth-library-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-routine-forth-library-editable)))
  (typep object 'game-resource-routine-forth-library))

(clim:define-presentation-type game-resource-routine-forth-library-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-routine-forth-library-viewing)))
  (typep object 'game-resource-routine-forth-library))

(defmethod present-reference ((resource game-resource-routine-forth-library) stream)
  (clim:with-output-as-presentation (stream resource 'game-resource-routine-forth-library-reference)
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

(defmethod present-reading ((resource game-resource-routine-forth-library) stream)
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
        (format stream "~a (Forth Library)" (game-resource-kind resource))))))

(defmethod present-editing ((resource game-resource-routine-forth-library) stream)
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

(clim:define-command-table forth-library-file-menu
  :menu (("New..." :command com-new-forth-library)
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

(clim:define-command-table forth-library-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table forth-library-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :toggle t)
         (nil :divider :line)
         ("Project Pane" :command com-toggle-project-pane :toggle t)))

(clim:define-command-table forth-library-build-menu
  :menu (("Demo" :command com-build-forth-library-demo)
         ("Public" :command com-build-forth-library-public)
         ("$(Publisher)" :command com-build-forth-library-publisher)))

(clim:define-command-table forth-library-region-menu
  :menu (("NTSC" :command com-region-ntsc-forth-library)
         ("PAL" :command com-region-pal-forth-library)
         ("SECAM" :command com-region-secam-forth-library)))

(clim:define-command-table forth-library-run-menu
  :menu (("Build" :menu forth-library-build-menu)
         ("Region" :menu forth-library-region-menu)
         (nil :divider :line)
         ("Compile..." :command com-compile-forth-library)))

(clim:define-command-table forth-library-help-menu
  :menu (("How to Manage Forth Libraries..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table forth-library-menu-bar
  :menu (("Forth Library" :menu forth-library-file-menu)
         ("Edit" :menu forth-library-edit-menu)
         ("View" :menu forth-library-view-menu)
         ("Run" :menu forth-library-run-menu)
         ("Help" :menu forth-library-help-menu)))

(clim:define-application-frame forth-library-inspector-frame (gui-inspector-frame clim:standard-application-frame)
  ((resource :initarg :resource :reader frame-resource))
  (:menu-bar forth-library-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "Forth Library Inspector"))

(defmethod initialize-instance :after ((frame forth-library-inspector-frame) &key)
  (call-next-method)
  (subscribe :resource-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

(defun open-forth-library-inspector (resource)
  (open-resource-inspector (or resource (make-instance 'game-resource-routine-forth-library)) :editing))

(defmethod open-resource-inspector ((resource game-resource-routine-forth-library) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'forth-library-inspector-frame
                                :resource resource
                                :pretty-name (format nil "~a — ~a ~a"
                                                     (game-resource-title resource)
                                                     *game-title* (machine-directory-name))
                                :view-mode mode)))

(clim:define-command (com-new-forth-library :command-table clim-internals::global-command-table :menu t :name t) ()
  (open-forth-library-inspector nil))

(clim:define-command (com-compile-forth-library :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Compiling Forth library...~%"))))

(clim:define-command (com-build-forth-library-demo :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building Forth library (Demo)...~%"))))

(clim:define-command (com-build-forth-library-public :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building Forth library (Public)...~%"))))

(clim:define-command (com-build-forth-library-publisher :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building Forth library (Publisher)...~%"))))

(clim:define-command (com-region-ntsc-forth-library :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :ntsc)
  (publish :region-changed :payload :ntsc))

(clim:define-command (com-region-pal-forth-library :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :pal)
  (publish :region-changed :payload :pal))

(clim:define-command (com-region-secam-forth-library :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :secam)
  (publish :region-changed :payload :secam))

(defmethod game-resource-action-menu ((resource game-resource-routine-forth-library))
  (list
   (make-menu-item "Inspect..." (lambda () (open-forth-library-inspector resource)))
   (make-menu-item "Open in Emacs..."
                   (lambda ()
                     (open-in-emacs (game-resource-full-path resource))))))
