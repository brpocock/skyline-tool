;;; Skyline-Tool src/gui/gui-magic-desk-dictionary.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-magic-desk-dictionary-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-magic-desk-dictionary-reference)))
  (typep object 'game-resource-magic-desk-dictionary))

(clim:define-presentation-type game-resource-magic-desk-dictionary-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-magic-desk-dictionary-editable)))
  (typep object 'game-resource-magic-desk-dictionary))

(clim:define-presentation-type game-resource-magic-desk-dictionary-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-magic-desk-dictionary-viewing)))
  (typep object 'game-resource-magic-desk-dictionary))

(defmethod present-reference ((resource game-resource-magic-desk-dictionary) stream)
  (clim:with-output-as-presentation (stream resource 'game-resource-magic-desk-dictionary-reference)
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

(defmethod present-reading ((resource game-resource-magic-desk-dictionary) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Word: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-title resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Phonetics: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-translation-phonetics resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Language: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-translation-language resource))))))

(defmethod present-editing ((resource game-resource-magic-desk-dictionary) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Word: "))
      (clim:formatting-cell (stream :align-x :left)
        (interactive-editing-gadget-with-validation
         stream resource
         (lambda (r) (game-resource-title r))
         (lambda (r v) (setf (game-resource-title r) v))
         :label "Word:"
         :validator #'validate-asset-name
         :max-length 200)))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Phonetics: "))
      (clim:formatting-cell (stream :align-x :left)
        (insert-gadget stream :label nil
                       :variable (game-translation-phonetics resource)
                       :activation-callback
                       (lambda (g)
                         (setf (game-translation-phonetics resource)
                               (clim:gadget-value g))))))
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
        (format stream "Kind: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a (read-only)" (game-resource-kind resource))))))

(clim:define-command-table magic-desk-file-menu
  :menu (("New..." :command com-new-magic-desk-word)
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

(clim:define-command-table magic-desk-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table magic-desk-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :toggle t)
         (nil :divider :line)
         ("Project Pane" :command com-toggle-project-pane :toggle t)))

(clim:define-command-table magic-desk-build-menu
  :menu (("Demo" :command com-build-magic-desk-demo)
         ("Public" :command com-build-magic-desk-public)
         ("$(Publisher)" :command com-build-magic-desk-publisher)))

(clim:define-command-table magic-desk-region-menu
  :menu (("NTSC" :command com-region-ntsc-magic-desk)
         ("PAL" :command com-region-pal-magic-desk)
         ("SECAM" :command com-region-secam-magic-desk)))

(clim:define-command-table magic-desk-run-menu
  :menu (("Build" :menu magic-desk-build-menu)
         ("Region" :menu magic-desk-region-menu)))

(clim:define-command-table magic-desk-help-menu
  :menu (("How to Edit Magic Desk Words..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table magic-desk-menu-bar
  :menu (("Magic Desk" :menu magic-desk-file-menu)
         ("Edit" :menu magic-desk-edit-menu)
         ("View" :menu magic-desk-view-menu)
         ("Run" :menu magic-desk-run-menu)
         ("Help" :menu magic-desk-help-menu)))

(clim:define-application-frame magic-desk-dictionary-inspector-frame (gui-inspector-frame clim:standard-application-frame)
  ((resource :initarg :resource :reader frame-resource))
  (:menu-bar magic-desk-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "Magic Desk Dictionary Inspector"))

(defmethod initialize-instance :after ((frame magic-desk-dictionary-inspector-frame) &key)
  (call-next-method)
  (subscribe :resource-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

(defun open-magic-desk-dictionary-inspector (resource)
  (open-resource-inspector (or resource (make-instance 'game-resource-magic-desk-dictionary)) :editing))

(defmethod open-resource-inspector ((resource game-resource-magic-desk-dictionary) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'magic-desk-dictionary-inspector-frame
                                :resource resource
                                :pretty-name (format nil "~a — ~a ~a"
                                                     (game-resource-title resource)
                                                     *game-title* (machine-directory-name))
                                :view-mode mode)))

(clim:define-command (com-new-magic-desk-word :command-table clim-internals::global-command-table :menu t :name t) ()
  (open-magic-desk-dictionary-inspector nil))

(clim:define-command (com-build-magic-desk-demo :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building Magic Desk dictionary (Demo)...~%"))))

(clim:define-command (com-build-magic-desk-public :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building Magic Desk dictionary (Public)...~%"))))

(clim:define-command (com-build-magic-desk-publisher :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building Magic Desk dictionary (Publisher)...~%"))))

(clim:define-command (com-region-ntsc-magic-desk :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :ntsc)
  (publish :region-changed :payload :ntsc))

(clim:define-command (com-region-pal-magic-desk :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :pal)
  (publish :region-changed :payload :pal))

(clim:define-command (com-region-secam-magic-desk :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :secam)
  (publish :region-changed :payload :secam))

(defmethod game-resource-action-menu ((resource game-resource-magic-desk-dictionary))
  (list
   (make-menu-item "Inspect..." (lambda () (open-magic-desk-dictionary-inspector resource)))))
