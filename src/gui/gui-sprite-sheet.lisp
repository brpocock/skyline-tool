;;; Skyline-Tool src/gui/gui-sprite-sheet.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-sprite-sheet-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-sprite-sheet-reference)))
  (typep object 'game-resource-sprite-sheet))

(clim:define-presentation-type game-resource-sprite-sheet-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-sprite-sheet-editable)))
  (typep object 'game-resource-sprite-sheet))

(clim:define-presentation-type game-resource-sprite-sheet-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-sprite-sheet-viewing)))
  (typep object 'game-resource-sprite-sheet))

(defmethod present-reference ((resource game-resource-sprite-sheet) stream)
  (clim:with-output-as-presentation (stream resource 'game-resource-sprite-sheet-reference)
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

(defmethod present-reading ((resource game-resource-sprite-sheet) stream)
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

(defmethod present-editing ((resource game-resource-sprite-sheet) stream)
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
            (princ (game-resource-pathnames resource) stream)
            (format stream "(none)"))))))

(defun open-sprite-sheet-inspector (resource)
  (open-resource-inspector (or resource
                                (make-instance 'game-resource-sprite-sheet
                                  :full-path nil)) :editing))

(defmethod open-resource-inspector ((resource game-resource-sprite-sheet) &optional (mode :editing))
  (declare (ignore mode))
  (let ((fm (clim:find-frame-manager :port (or (clim:find-port) (clim:find-port :server-path :x))))
        (frame (clim:make-application-frame 'sprite-sheet-inspector-frame
                                            :resource (or resource (make-instance 'game-resource-sprite-sheet))
                                            :pretty-name (format nil "~a — ~a ~a"
                                                                 (game-resource-title resource)
                                                                 *game-title* (machine-directory-name))
                                            :frame-manager fm)))
    (clim:run-frame-top-level frame)))

(defmethod game-resource-action-menu ((resource game-resource-sprite-sheet))
  (list
   (make-menu-item "Inspect..." (lambda () (open-sprite-sheet-inspector resource)))
   (make-menu-item "Open in Gimp..."
                   (lambda ()
                     (uiop:run-program
                      (list "gimp" (truename (first (game-resource-pathnames resource))))
                      :output nil :ignore-error-status t)))))


;;; Skyline-Tool src/gui/sprite-sheet-inspector.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-application-frame sprite-sheet-inspector-frame (gui-inspector-frame clim:standard-application-frame)
  ((frame-resource :accessor frame-resource :initform nil))
  (:menu-bar sprite-sheet-menu-bar)
  (:icon (skyline-tool-icon :resource :sprite-sheet))
  (:pretty-name "Sprite Sheet Inspector"))

(defmethod initialize-instance :after ((frame sprite-sheet-inspector-frame) &key)
  (call-next-method)
  (subscribe :resource-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

(clim:define-command-table sprite-sheet-file-menu
  :menu (("New..." :command com-new-sprite-sheet)
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

(clim:define-command-table sprite-sheet-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table sprite-sheet-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :toggle t)
         (nil :divider :line)
         ("Project Pane" :command com-toggle-project-pane :toggle t)))

(clim:define-command-table sprite-sheet-build-menu
  :menu (("Demo" :command com-build-sprite-sheet-demo)
         ("Public" :command com-build-sprite-sheet-public)
         ("$(Publisher)" :command com-build-sprite-sheet-publisher)))

(clim:define-command-table sprite-sheet-region-menu
  :menu (("NTSC" :command com-region-ntsc-sprite-sheet)
         ("PAL" :command com-region-pal-sprite-sheet)
         ("SECAM" :command com-region-secam-sprite-sheet)))

(clim:define-command-table sprite-sheet-run-menu
  :menu (("Build" :menu sprite-sheet-build-menu)
         ("Region" :menu sprite-sheet-region-menu)
         (nil :divider :line)
         ("Export PNG..." :command com-sprite-sheet-export-png)
         ("Open in Gimp..." :command com-open-in-gimp)))

(clim:define-command-table sprite-sheet-help-menu
  :menu (("How to Manage Sprite Sheets..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table sprite-sheet-menu-bar
  :menu (("Sprite" :menu sprite-sheet-file-menu)
         ("Edit" :menu sprite-sheet-edit-menu)
         ("View" :menu sprite-sheet-view-menu)
         ("Run" :menu sprite-sheet-run-menu)
         ("Help" :menu sprite-sheet-help-menu)))

(clim:define-command (com-new-sprite-sheet :command-table clim-internals::global-command-table :menu t :name t) ()
  (open-sprite-sheet-inspector nil))

(clim:define-command (com-sprite-sheet-export-png :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Exporting sprite sheet to PNG...~%"))))

(clim:define-command (com-build-sprite-sheet-demo :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building sprite sheet (Demo)...~%"))))

(clim:define-command (com-build-sprite-sheet-public :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building sprite sheet (Public)...~%"))))

(clim:define-command (com-build-sprite-sheet-publisher :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building sprite sheet (Publisher)...~%"))))

(clim:define-command (com-region-ntsc-sprite-sheet :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :ntsc)
  (publish :region-changed :payload :ntsc))

(clim:define-command (com-region-pal-sprite-sheet :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :pal)
  (publish :region-changed :payload :pal))

(clim:define-command (com-region-secam-sprite-sheet :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :secam)
  (publish :region-changed :payload :secam))
