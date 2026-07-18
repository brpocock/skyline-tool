;;; Skyline-Tool src/gui/gui-item.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-item-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-item-reference)))
  (typep object 'game-resource-item))

(clim:define-presentation-type game-resource-item-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-item-editable)))
  (typep object 'game-resource-item))

(clim:define-presentation-type game-resource-item-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-item-viewing)))
  (typep object 'game-resource-item))

(defmethod present-reference ((resource game-resource-item) stream)
  (clim:with-output-as-presentation (stream resource 'game-resource-item-reference)
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

(defmethod present-reading ((resource game-resource-item) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-title resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Item ID: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~d" (game-resource-item-id resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Equippable: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~:[No~;Yes~]" (game-resource-item-equippable-p resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Slot: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-item-equipment-slot resource))))))

(defmethod present-editing ((resource game-resource-item) stream)
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
        (format stream "Item ID: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~d (read-only)" (game-resource-item-id resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Equippable: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~:[No~;Yes~]" (game-resource-item-equippable-p resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Slot: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a (read-only)" (game-resource-item-equipment-slot resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Kind: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a (read-only)" (game-resource-kind resource))))))

(clim:define-command-table item-file-menu
  :menu (("New..." :command com-new-item)
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

(clim:define-command-table item-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table item-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :toggle t)
         (nil :divider :line)
         ("Project Pane" :command com-toggle-project-pane :toggle t)))

(clim:define-command-table item-build-menu
  :menu (("Demo" :command com-build-item-demo)
         ("Public" :command com-build-item-public)
         ("$(Publisher)" :command com-build-item-publisher)))

(clim:define-command-table item-region-menu
  :menu (("NTSC" :command com-region-ntsc-item)
         ("PAL" :command com-region-pal-item)
         ("SECAM" :command com-region-secam-item)))

(clim:define-command-table item-run-menu
  :menu (("Build" :menu item-build-menu)
         ("Region" :menu item-region-menu)))

(clim:define-command-table item-help-menu
  :menu (("How to Manage Items..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table item-menu-bar
  :menu (("Item" :menu item-file-menu)
         ("Edit" :menu item-edit-menu)
         ("View" :menu item-view-menu)
         ("Run" :menu item-run-menu)
         ("Help" :menu item-help-menu)))

(clim:define-application-frame game-resource-item-inspector (gui-inspector-frame clim:standard-application-frame)
  ((frame-resource :initarg :resource :reader frame-resource :initform nil))
  (:menu-bar item-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "Item Inspector"))

(defmethod initialize-instance :after ((frame game-resource-item-inspector) &key)
  (call-next-method)
  (subscribe :resource-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

(defun open-item-inspector (resource)
  (open-resource-inspector (or resource
                                (make-instance 'game-resource-item
                                  :item-id 0
                                  :name "New Item"
                                  :equippable-p nil
                                  :slot :item
                                  :sound ""
                                  :entity-class ""
                                  :entity-prototype ""
                                  :course-class ""
                                  :course-prototype ""
                                  :decal-bank 0
                                  :decal-sheet 0
                                  :decal-up 0
                                  :decal-down 0
                                  :decal-right 0
                                  :decal-left 0
                                  :drawing-mode ""
                                  :palette 0
                                  :displacement-up 0
                                  :displacement-down 0
                                  :displacement-right 0
                                  :displacement-left 0)) :editing))

(defmethod open-resource-inspector ((resource game-resource-item) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'game-resource-item-inspector
                                :resource (or resource (make-instance 'game-resource-item))
                                :pretty-name (format nil "~a — ~a ~a"
                                                     (game-resource-title resource)
                                                     *game-title* (machine-directory-name))
                                :view-mode mode)))

(clim:define-command (com-new-item :command-table clim-internals::global-command-table :menu t :name t) ()
  (open-item-inspector nil))

(clim:define-command (com-build-item-demo :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building item (Demo)...~%"))))

(clim:define-command (com-build-item-public :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building item (Public)...~%"))))

(clim:define-command (com-build-item-publisher :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building item (Publisher)...~%"))))

(clim:define-command (com-region-ntsc-item :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :ntsc)
  (publish :region-changed :payload :ntsc))

(clim:define-command (com-region-pal-item :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :pal)
  (publish :region-changed :payload :pal))

(clim:define-command (com-region-secam-item :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :secam)
  (publish :region-changed :payload :secam))

(defmethod game-resource-action-menu ((resource game-resource-item))
  (list
   (make-menu-item "Inspect..." (lambda () (open-item-inspector resource)))))
