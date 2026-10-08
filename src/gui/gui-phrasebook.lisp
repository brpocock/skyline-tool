;;; Skyline-Tool src/gui/gui-phrasebook.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-phrasebook-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-phrasebook-reference)))
  (typep object 'game-resource-phrasebook))

(clim:define-presentation-type game-resource-phrasebook-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-phrasebook-editable)))
  (typep object 'game-resource-phrasebook))

(clim:define-presentation-type game-resource-phrasebook-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-phrasebook-viewing)))
  (typep object 'game-resource-phrasebook))

(defmethod present-reference ((resource game-resource-phrasebook) stream)
  (clim:with-output-as-presentation (stream resource 'game-resource-phrasebook-reference)
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

(defmethod present-reading ((resource game-resource-phrasebook) stream)
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
        (format stream "Language: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-translation-language resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "English Key: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-translation-english-key resource))))))

(defmethod present-editing ((resource game-resource-phrasebook) stream)
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
        (format stream "Language: "))
      (clim:formatting-cell (stream :align-x :left)
        (insert-gadget stream :label nil
                       :variable (game-translation-language resource)
                       :activation-callback
                       (lambda (g)
                         (setf (game-translation-language resource)
                               (clim:gadget-value g))))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "English Key: "))
      (clim:formatting-cell (stream :align-x :left)
        (insert-gadget stream :label nil
                       :variable (game-translation-english-key resource)
                       :activation-callback
                       (lambda (g)
                         (setf (game-translation-english-key resource)
                               (clim:gadget-value g))))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Translation: "))
      (clim:formatting-cell (stream :align-x :left)
        (insert-gadget stream :label nil
                       :variable (game-translation-translation resource)
                       :activation-callback
                       (lambda (g)
                         (setf (game-translation-translation resource)
                               (clim:gadget-value g))))))))

(clim:define-command-table phrasebook-file-menu
  :menu (("New..." :command com-new-phrasebook)
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

(clim:define-command-table phrasebook-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table phrasebook-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :toggle t)
         (nil :divider :line)
         ("Project Pane" :command com-toggle-project-pane :toggle t)))

(clim:define-command-table phrasebook-build-menu
  :menu (("Demo" :command com-build-phrasebook-demo)
         ("Public" :command com-build-phrasebook-public)
         ("$(Publisher)" :command com-build-phrasebook-publisher)))

(clim:define-command-table phrasebook-region-menu
  :menu (("NTSC" :command com-region-ntsc-phrasebook)
         ("PAL" :command com-region-pal-phrasebook)
         ("SECAM" :command com-region-secam-phrasebook)))

(clim:define-command-table phrasebook-run-menu
  :menu (("Build" :menu phrasebook-build-menu)
         ("Region" :menu phrasebook-region-menu)))

(clim:define-command-table phrasebook-help-menu
  :menu (("How to Manage Phrasebooks..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table phrasebook-menu-bar
  :menu (("Phrasebook" :menu phrasebook-file-menu)
         ("Edit" :menu phrasebook-edit-menu)
         ("View" :menu phrasebook-view-menu)
         ("Run" :menu phrasebook-run-menu)
         ("Help" :menu phrasebook-help-menu)))

(clim:define-application-frame phrasebook-inspector-frame (gui-inspector-frame clim:standard-application-frame)
  ((resource :initarg :resource :reader frame-resource))
  (:menu-bar phrasebook-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "Phrasebook Inspector"))

(defmethod initialize-instance :after ((frame phrasebook-inspector-frame) &key)
  (call-next-method)
  (subscribe :resource-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

(defun open-phrasebook-inspector (resource)
  (open-resource-inspector (or resource (make-instance 'game-resource-phrasebook)) :editing))

(defmethod open-resource-inspector ((resource game-resource-phrasebook) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'phrasebook-inspector-frame
                                :resource resource
                                :pretty-name (format nil "~a — ~a ~a"
                                                     (game-resource-title resource)
                                                     *game-title* (machine-directory-name))
                                :view-mode mode)))

(clim:define-command (com-new-phrasebook :command-table clim-internals::global-command-table :menu t :name t) ()
  (open-phrasebook-inspector nil))

(clim:define-command (com-build-phrasebook-demo :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building phrasebook (Demo)...~%"))))

(clim:define-command (com-build-phrasebook-public :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building phrasebook (Public)...~%"))))

(clim:define-command (com-build-phrasebook-publisher :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building phrasebook (Publisher)...~%"))))

(clim:define-command (com-region-ntsc-phrasebook :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :ntsc)
  (publish :region-changed :payload :ntsc))

(clim:define-command (com-region-pal-phrasebook :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :pal)
  (publish :region-changed :payload :pal))

(clim:define-command (com-region-secam-phrasebook :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :secam)
  (publish :region-changed :payload :secam))

(defmethod game-resource-action-menu ((resource game-resource-phrasebook))
  (list
   (make-menu-item "Inspect..." (lambda () (open-phrasebook-inspector resource)))))
