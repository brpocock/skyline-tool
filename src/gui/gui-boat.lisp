;;; Skyline-Tool src/gui/gui-boat.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-boat-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-boat-reference)))
  (typep object 'game-resource-boat))

(clim:define-presentation-type game-resource-boat-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-boat-editable)))
  (typep object 'game-resource-boat))

(clim:define-presentation-type game-resource-boat-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-boat-viewing)))
  (typep object 'game-resource-boat))

(defmethod present-reference ((resource game-resource-boat) stream)
  (clim:with-output-as-presentation (stream resource 'game-resource-boat-reference)
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

(defmethod present-reading ((resource game-resource-boat) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-title resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Boat Class: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-boat-class resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Boat ID: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~d" (game-resource-boat-id resource))))))

(defmethod present-editing ((resource game-resource-boat) stream)
  (let ((name (game-resource-title resource))
        (boat-class (game-resource-boat-class resource))
        (boat-id (game-resource-boat-id resource)))
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
           :validator #'validate-minifont-name
           :max-length 20)))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Boat Class: "))
        (clim:formatting-cell (stream :align-x :left)
          (insert-gadget stream
                         :label nil
                         :variable boat-class
                         :activation-callback
                         (lambda (gadget)
                           (setf (game-resource-boat-class resource)
                                 (clim:gadget-value gadget))))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Boat ID: "))
        (clim:formatting-cell (stream :align-x :left)
          (format stream "~d (read-only)" boat-id)))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Kind: "))
        (clim:formatting-cell (stream :align-x :left)
          (format stream "~a (read-only)" (game-resource-kind resource)))))))

(clim:define-command-table boat-file-menu
  :menu (("New..." :command com-new-boat)
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

(clim:define-command-table boat-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table boat-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :toggle t)
         (nil :divider :line)
         ("Project Pane" :command com-toggle-project-pane :toggle t)))

(clim:define-command-table boat-build-menu
  :menu (("Demo" :command com-build-boat-demo)
         ("Public" :command com-build-boat-public)
         ("$(Publisher)" :command com-build-boat-publisher)))

(clim:define-command-table boat-region-menu
  :menu (("NTSC" :command com-region-ntsc-boat)
         ("PAL" :command com-region-pal-boat)
         ("SECAM" :command com-region-secam-boat)))

(clim:define-command-table boat-run-menu
  :menu (("Build" :menu boat-build-menu)
         ("Region" :menu boat-region-menu)))

(clim:define-command-table boat-help-menu
  :menu (("How to Manage Boats..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table boat-menu-bar
  :menu (("Boat" :menu boat-file-menu)
         ("Edit" :menu boat-edit-menu)
         ("View" :menu boat-view-menu)
         ("Run" :menu boat-run-menu)
         ("Help" :menu boat-help-menu)))

(clim:define-application-frame boat-inspector-frame (gui-inspector-frame clim:standard-application-frame)
  ((resource :initarg :resource :reader frame-resource))
  (:menu-bar boat-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "Boat Inspector"))

(defmethod initialize-instance :after ((frame boat-inspector-frame) &key)
  (call-next-method)
  (subscribe :resource-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

(defun open-boat-inspector (resource)
  (open-resource-inspector (or resource
                                (make-instance 'game-resource-boat
                                  :id 0
                                  :name "New Boat"
                                  :boat-class "rowboat"
                                  :notes "")) :editing))

(defmethod open-resource-inspector ((resource game-resource-boat) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'boat-inspector-frame
                                :resource resource
                                :pretty-name (format nil "~a — ~a ~a"
                                                     (game-resource-title resource)
                                                     *game-title* (machine-directory-name))
                                :view-mode mode)))

(clim:define-command (com-new-boat :command-table clim-internals::global-command-table :menu t :name t) ()
  (open-boat-inspector nil))

(clim:define-command (com-build-boat-demo :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building boat (Demo)...~%"))))

(clim:define-command (com-build-boat-public :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building boat (Public)...~%"))))

(clim:define-command (com-build-boat-publisher :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building boat (Publisher)...~%"))))

(clim:define-command (com-region-ntsc-boat :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :ntsc)
  (publish :region-changed :payload :ntsc))

(clim:define-command (com-region-pal-boat :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :pal)
  (publish :region-changed :payload :pal))

(clim:define-command (com-region-secam-boat :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :secam)
  (publish :region-changed :payload :secam))

(defmethod game-resource-action-menu ((resource game-resource-boat))
  (list
   (make-menu-item "Inspect..." (lambda () (open-boat-inspector resource)))))
