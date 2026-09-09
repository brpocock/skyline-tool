;;; Skyline-Tool src/gui/gui-intellivoice-dictionary.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-intellivoice-dictionary-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-intellivoice-dictionary-reference)))
  (typep object 'game-resource-intellivoice-dictionary))

(clim:define-presentation-type game-resource-intellivoice-dictionary-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-intellivoice-dictionary-editable)))
  (typep object 'game-resource-intellivoice-dictionary))

(clim:define-presentation-type game-resource-intellivoice-dictionary-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-intellivoice-dictionary-viewing)))
  (typep object 'game-resource-intellivoice-dictionary))

(defmethod present-reference ((resource game-resource-intellivoice-dictionary) stream)
  (clim:with-output-as-presentation (stream resource 'game-resource-intellivoice-dictionary-reference)
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

(defmethod present-reading ((resource game-resource-intellivoice-dictionary) stream)
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

(defmethod present-editing ((resource game-resource-intellivoice-dictionary) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Word: "))
      (clim:formatting-cell (stream :align-x :left)
(interactive-editing-gadget-with-validation
          stream resource
          :getter (lambda (r) (game-resource-title r))
          :setter (lambda (r v) (setf (game-resource-title r) v))
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

(clim:define-command-table intellivoice-file-menu
  :menu (("New..." :command com-new-intellivoice-word)
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

(clim:define-command-table intellivoice-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table intellivoice-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :toggle t)
         (nil :divider :line)
         ("Project Pane" :command com-toggle-project-pane :toggle t)))

(clim:define-command-table intellivoice-build-menu
  :menu (("Demo" :command com-build-intellivoice-demo)
         ("Public" :command com-build-intellivoice-public)
         ("$(Publisher)" :command com-build-intellivoice-publisher)))

(clim:define-command-table intellivoice-region-menu
  :menu (("NTSC" :command com-region-ntsc-intellivoice)
         ("PAL" :command com-region-pal-intellivoice)
         ("SECAM" :command com-region-secam-intellivoice)))

(clim:define-command-table intellivoice-run-menu
  :menu (("Build" :menu intellivoice-build-menu)
         ("Region" :menu intellivoice-region-menu)))

(clim:define-command-table intellivoice-help-menu
  :menu (("How to Edit IntelliVoice Words..." :command com-help-for-window)
         ("IntelliVoice Documentation..." :command com-open-fountain-manual)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table intellivoice-menu-bar
  :menu (("IntelliVoice" :menu intellivoice-file-menu)
         ("Edit" :menu intellivoice-edit-menu)
         ("View" :menu intellivoice-view-menu)
         ("Run" :menu intellivoice-run-menu)
         ("Help" :menu intellivoice-help-menu)))

(clim:define-application-frame intellivoice-dictionary-inspector-frame (gui-inspector-frame clim:standard-application-frame)
  ((resource :initarg :resource :reader frame-resource))
  (:menu-bar intellivoice-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "IntelliVoice Dictionary Inspector"))

(defmethod initialize-instance :after ((frame intellivoice-dictionary-inspector-frame) &key)
  (call-next-method)
  (subscribe :resource-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

(defun open-intellivoice-dictionary-inspector (resource)
  (open-resource-inspector (or resource (make-instance 'game-resource-intellivoice-dictionary
                                                        :kind "IntelliVoice Dictionary"
                                                        :moniker "IntelliVoice Dictionary/IntelliVoice.dic")) :editing))

(defmethod open-resource-inspector ((resource game-resource-intellivoice-dictionary) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'intellivoice-dictionary-inspector-frame
                                :resource resource
                                :pretty-name (format nil "~a — ~a ~a"
                                                     (game-resource-title resource)
                                                     *game-title* (machine-directory-name))
                                :view-mode mode)))

(clim:define-command (com-new-intellivoice-word :command-table clim-internals::global-command-table :menu t :name t) ()
  (open-intellivoice-dictionary-inspector nil))

(clim:define-command (com-build-intellivoice-demo :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building IntelliVoice dictionary (Demo)...~%"))))

(clim:define-command (com-build-intellivoice-public :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building IntelliVoice dictionary (Public)...~%"))))

(clim:define-command (com-build-intellivoice-publisher :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building IntelliVoice dictionary (Publisher)...~%"))))

(clim:define-command (com-region-ntsc-intellivoice :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :ntsc)
  (publish :region-changed :payload :ntsc))

(clim:define-command (com-region-pal-intellivoice :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :pal)
  (publish :region-changed :payload :pal))

(clim:define-command (com-region-secam-intellivoice :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :secam)
  (publish :region-changed :payload :secam))

(defmethod game-resource-action-menu ((resource game-resource-intellivoice-dictionary))
  (list
   (make-menu-item "Inspect..." (lambda () (open-intellivoice-dictionary-inspector resource)))))
