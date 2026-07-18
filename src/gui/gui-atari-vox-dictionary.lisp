;;; Skyline-Tool src/gui/gui-atari-vox-dictionary.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-atari-vox-dictionary-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-atari-vox-dictionary-reference)))
  (typep object 'game-resource-atari-vox-dictionary))

(clim:define-presentation-type game-resource-atari-vox-dictionary-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-atari-vox-dictionary-editable)))
  (typep object 'game-resource-atari-vox-dictionary))

(clim:define-presentation-type game-resource-atari-vox-dictionary-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-atari-vox-dictionary-viewing)))
  (typep object 'game-resource-atari-vox-dictionary))

(defmethod present-reference ((resource game-resource-atari-vox-dictionary) stream)
  (clim:with-output-as-presentation (stream resource 'game-resource-atari-vox-dictionary-reference)
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

(defmethod present-reading ((resource game-resource-atari-vox-dictionary) stream)
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
        (format stream "~a" (game-translation-language resource))))
    (when (typep resource 'game-resource-from-file)
      (let ((path (game-resource-full-path resource)))
        (when (and path (probe-file path))
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream :align-x :right)
              (format stream "Path: "))
            (clim:formatting-cell (stream :align-x :left)
              (format stream "~a" path))))))))

(defmethod present-editing ((resource game-resource-atari-vox-dictionary) stream)
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

(clim:define-command-table atari-vox-file-menu
  :menu (("New..." :command com-new-atari-vox-word)
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

(clim:define-command-table atari-vox-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table atari-vox-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :toggle t)
         (nil :divider :line)
         ("Project Pane" :command com-toggle-project-pane :toggle t)))

(clim:define-command-table atari-vox-build-menu
  :menu (("Demo" :command com-build-atari-vox-demo)
         ("Public" :command com-build-atari-vox-public)
         ("$(Publisher)" :command com-build-atari-vox-publisher)))

(clim:define-command-table atari-vox-region-menu
  :menu (("NTSC" :command com-region-ntsc-atari-vox)
         ("PAL" :command com-region-pal-atari-vox)
         ("SECAM" :command com-region-secam-atari-vox)))

(clim:define-command-table atari-vox-run-menu
  :menu (("Build" :menu atari-vox-build-menu)
         ("Region" :menu atari-vox-region-menu)
         (nil :divider :line)
         ("Speak on AtariVox..." :command com-speak-atari-vox)))

(clim:define-command-table atari-vox-help-menu
  :menu (("How to Edit AtariVox Words..." :command com-help-for-window)
         ("SpeakJet Phoneme Documentation..." :command com-open-fountain-manual)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table atari-vox-menu-bar
  :menu (("AtariVox" :menu atari-vox-file-menu)
         ("Edit" :menu atari-vox-edit-menu)
         ("View" :menu atari-vox-view-menu)
         ("Run" :menu atari-vox-run-menu)
         ("Help" :menu atari-vox-help-menu)))

(clim:define-application-frame atari-vox-dictionary-inspector-frame (gui-inspector-frame clim:standard-application-frame)
  ((resource :initarg :resource :reader frame-resource))
  (:menu-bar atari-vox-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "AtariVox Dictionary Inspector"))

(defmethod initialize-instance :after ((frame atari-vox-dictionary-inspector-frame) &key)
  (call-next-method)
  (subscribe :resource-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

(defun open-atari-vox-dictionary-inspector (resource)
  (open-resource-inspector (or resource (make-instance 'game-resource-atari-vox-dictionary
                                                        :kind "AtariVox Dictionary"
                                                        :moniker "AtariVox Dictionary/SpeakJet.dic")) :editing))

(defmethod open-resource-inspector ((resource game-resource-atari-vox-dictionary) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'atari-vox-dictionary-inspector-frame
                                :resource resource
                                :pretty-name (format nil "~a — ~a ~a"
                                                     (game-resource-title resource)
                                                     *game-title* (machine-directory-name))
                                :view-mode mode)))

(clim:define-command (com-new-atari-vox-word :command-table clim-internals::global-command-table :menu t :name t) ()
  (open-atari-vox-dictionary-inspector nil))

(clim:define-command (com-speak-atari-vox :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Speaking on AtariVox...~%"))))

(clim:define-command (com-build-atari-vox-demo :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building AtariVox dictionary (Demo)...~%"))))

(clim:define-command (com-build-atari-vox-public :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building AtariVox dictionary (Public)...~%"))))

(clim:define-command (com-build-atari-vox-publisher :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building AtariVox dictionary (Publisher)...~%"))))

(clim:define-command (com-region-ntsc-atari-vox :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :ntsc)
  (publish :region-changed :payload :ntsc))

(clim:define-command (com-region-pal-atari-vox :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :pal)
  (publish :region-changed :payload :pal))

(clim:define-command (com-region-secam-atari-vox :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :secam)
  (publish :region-changed :payload :secam))

(defmethod game-resource-action-menu ((resource game-resource-atari-vox-dictionary))
  (list
   (make-menu-item "Inspect..." (lambda () (open-atari-vox-dictionary-inspector resource)))))
