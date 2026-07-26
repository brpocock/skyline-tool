;;; Skyline-Tool src/gui/gui-script.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-script-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-script-reference)))
  (typep object 'game-resource-script))

(clim:define-presentation-type game-resource-script-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-script-editable)))
  (typep object 'game-resource-script))

(clim:define-presentation-type game-resource-script-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-script-viewing)))
  (typep object 'game-resource-script))

(clim:define-presentation-method clim:present ((resource game-resource-script) (type game-resource-script-reference) stream view &key)
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

(clim:define-presentation-method clim:present ((resource game-resource-script) (type game-resource-script-editable) stream view &key)
   (declare (ignore view))
   (clim:formatting-table (stream)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Title: "))
        (clim:formatting-cell (stream :align-x :left)
          (interactive-editing-gadget-with-validation
           stream resource
           (lambda (r) (game-resource-title r))
           (lambda (r v) (setf (game-resource-title r) v))
           :label "Title:"
           :validator #'validate-asset-name
           :max-length 200)))))

(clim:define-presentation-method clim:present ((resource game-resource-script) (type game-resource-script-viewing) stream view &key)
  (declare (ignore view))
  (clim:surrounding-output-with-border (stream :shape :rounded)
     (format stream "[SCRIPT] ~a" (game-resource-title resource))))

(defmethod present-editing ((resource game-resource-script) stream)
  (clim:present resource 'game-resource-script-editable :stream stream))

(defmethod present-reading ((resource game-resource-script) stream)
  (clim:present resource 'game-resource-script-viewing :stream stream))

(defmethod present-reference ((resource game-resource-script) stream)
  (clim:present resource 'game-resource-script-reference :stream stream))

(defun open-script-inspector (resource)
  (open-resource-inspector (or resource
                                (make-instance 'game-resource-script
                                  :kind "Script"
                                  :moniker "Scripts/new-script.sky")) :editing))

(defmethod game-resource-action-menu ((resource game-resource-script))
  (list
   (make-menu-item "Inspect..." (lambda () (open-script-inspector resource)))
   (make-menu-item "Open in Emacs..."
                   (lambda ()
                     (open-in-emacs (game-resource-full-path resource))))))

;;; Skyline-Tool src/gui/gui-script-inspector.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-command-table inspector-script-build-menu
  :menu (("Demo" :command com-build-script-demo)
         ("Public" :command com-build-script-public)
         ("$(Publisher)" :command com-build-script-publisher)))

(clim:define-command-table inspector-script-region-menu
  :menu (("NTSC" :command com-region-ntsc-script)
         ("PAL" :command com-region-pal-script)
         ("SECAM" :command com-region-secam-script)))

(clim:define-command-table inspector-script-run-menu
  :menu (("Build" :menu inspector-script-build-menu)
         ("Region" :menu inspector-script-region-menu)
         (nil :divider :line)
         ("Make PDF..." :command com-make-script-pdf)
         ("Compile Script..." :command com-compile-script)))

(clim:define-command-table inspector-script-file-menu
  :menu (("New Script..." :command com-new-script-from-menu)
         ("Script from File..." :command com-open-script)
         (nil :divider :line)
         ("Save" :command com-save-resource)
         ("Version" :menu inspector-vc-menu)
         ("Save as" :menu inspector-save-as-menu)
         (nil :divider :line)
         ("Send to" :menu inspector-send-to-menu)
         ("Print to" :menu inspector-print-to-menu)
         (nil :divider :line)
         ("Close" :command com-close-frame)))

(clim:define-command-table script-inspector-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table inspector-script-menu-bar
  :menu (("Script" :menu inspector-script-file-menu)
         ("Edit" :menu script-inspector-edit-menu)
         ("View" :menu inspector-view-menu)
         ("Run" :menu inspector-script-run-menu)
         ("Help" :menu inspector-help-menu)))

(clim:define-application-frame game-resource-script-inspector (gui-inspector-frame clim:standard-application-frame)
  ((resource :initarg :resource :reader frame-resource))
  (:menu-bar inspector-script-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "Script Inspector"))

(defmethod initialize-instance :after ((frame game-resource-script-inspector) &key)
  (call-next-method)
  (subscribe :region-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

(defun open-script-inspector (resource)
  (open-resource-inspector (or resource
                               (make-instance 'game-resource-script
                                              :kind "Script"
                                              :moniker "Scripts/new-script.sky")) :editing))

(defmethod open-resource-inspector ((resource game-resource-script) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'game-resource-script-inspector
                                :resource resource
                                :pretty-name (format nil "~a — ~a ~a"
                                                     (game-resource-title resource)
                                                     *game-title* (machine-directory-name))
                                :view-mode mode)))

(clim:define-command (com-make-script-pdf :command-table clim-internals::global-command-table :menu t :name t)
    ((resource 'game-resource-script :gesture :select))
  (let* ((txt (game-resource-full-path resource))
         (pdf (make-output-path resource :pdf)))
    (uiop:run-program
     (list "ptyxis" "-s" "-x" "pdflatex" "-interaction=nonstopmode"
           "-output-directory" (pathname-directory pdf)
           (pathname-name txt))
     :output nil
     :ignore-error-status t)))

(clim:define-command (com-compile-script :command-table clim-internals::global-command-table :menu t :name t)
    ((resource 'game-resource-script :gesture :select))
  (clim-simple-echo:run-in-simple-echo
   (format nil "compile-script ~a" (game-resource-title resource))))

(clim:define-command (com-new-script-from-menu :command-table clim-internals::global-command-table :menu t :name t) ()
  (error "New script not implemented."))

(clim:define-command (com-open-script :command-table clim-internals::global-command-table :menu t :name t) ()
  (error "Open script not implemented."))

(clim:define-command (com-build-script-demo :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building script (Demo)...~%"))))

(clim:define-command (com-build-script-public :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building script (Public)...~%"))))

(clim:define-command (com-build-script-publisher :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building script (Publisher)...~%"))))

(clim:define-command (com-region-ntsc-script :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :ntsc)
  (publish :region-changed :payload :ntsc))

(clim:define-command (com-region-pal-script :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :pal)
  (publish :region-changed :payload :pal))

(clim:define-command (com-region-secam-script :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :secam)
  (publish :region-changed :payload :secam))
