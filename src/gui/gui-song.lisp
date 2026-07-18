;;; Skyline-Tool src/gui/gui-song.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-song-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-song-reference)))
  (typep object 'game-resource-song))

(clim:define-presentation-type game-resource-song-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-song-editable)))
  (typep object 'game-resource-song))

(clim:define-presentation-method clim:present ((resource game-resource-song) (type game-resource-song-reference) stream view &key acceptably for-context-type)
  (declare (ignore view acceptably for-context-type))
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

(clim:define-presentation-method clim:present ((resource game-resource-song) (type game-resource-song-editable) stream view &key acceptably for-context-type)
  (declare (ignore view acceptably for-context-type))
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
         :max-length 200)))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Composer: "))
      (clim:formatting-cell (stream :align-x :left)
        (interactive-editing-gadget-with-validation
         stream resource
         (lambda (r) (game-resource-song-mscz-composer r))
         (lambda (r v) (setf (game-resource-song-mscz-composer r) v))
         :label "Composer:"
         :validator #'validate-asset-name
         :max-length 200))
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Copyright: "))
      (clim:formatting-cell (stream :align-x :left)
        (interactive-editing-gadget-with-validation
         stream resource
         (lambda (r) (game-resource-song-mscz-copyright r))
         (lambda (r v) (setf (game-resource-song-mscz-copyright r) v))
         :label "Copyright:"
         :validator #'validate-copyright
         :max-length 200)))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Duration: "))
      (clim:formatting-cell (stream :align-x :left)
        (interactive-editing-gadget-with-validation
         stream resource
         (lambda (r) (game-resource-song-duration r))
         (lambda (r v) (setf (game-resource-song-duration r) v))
         :label "Duration:"
         :validator #'validate-duration
         :max-length 50))))

(defmethod present-editing ((resource game-resource-song) stream)
  (clim:present resource 'game-resource-song-editable :stream stream))

(defmethod present-reading ((resource game-resource-song) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Title: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-title resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Subtitle: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-song-mscz-subtitle resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Composer: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-song-mscz-composer resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Copyright: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-song-mscz-copyright resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Duration: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-song-duration resource))))))

(defmethod present-reference ((resource game-resource-song) stream)
  (clim:present resource 'game-resource-song-reference :stream stream))

(defun open-song-inspector (resource)
  (open-resource-inspector (or resource
                               (make-instance 'game-resource-song
                                              :moniker "New Song"
                                              :full-path nil
                                              :mscz-title ""
                                              :mscz-subtitle ""
                                              :mscz-composer ""
                                              :mscz-copyright ""
                                              :mscz-lyrics "")) :editing))

(defmethod game-resource-action-menu ((resource game-resource-song))
  (list
   (make-menu-item "Inspect..." (lambda () (open-song-inspector resource)))
   (make-menu-item "Open in MuseScore"
                   (lambda ()
                     (uiop:run-program (list "musescore" (game-resource-full-path resource))
                                       :output nil :ignore-error-status t)))))

(clim:define-command-table song-save-menu
  :menu (("MIDI..." :command com-song-save-midi)
         ("Ogg Vorbis..." :command com-song-save-ogg-vorbis)
         ("Ogg FLAC..." :command com-song-save-ogg-flac)
         ("MP3..." :command com-song-save-mp3)
         ("Sheet Music PDF..." :command com-song-save-sheet-music)
         (nil :divider :line)
         ("JSON..." :command com-song-export-json)
         ("Text..." :command com-song-export-text)
         ("PDF..." :command com-song-export-pdf)))

(clim:define-command-table song-file-menu
  :menu (("New..." :command com-song-new)
         ("Import File..." :command com-song-import)
         ("Open in MuseScore" :command com-song-open-musescore)
         (nil :divider :line)
         ("Save" :command com-save-resource)
         ("Version" :menu inspector-vc-menu)
         ("Save as" :menu song-save-menu)
         ("Send to" :menu inspector-send-to-menu)
         ("Print to" :menu inspector-print-to-menu)
         (nil :divider :line)
         ("Close" :command com-close-frame)))

(clim:define-command-table song-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table song-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :toggle t)
         (nil :divider :line)
         ("Project Pane" :command com-toggle-project-pane :toggle t)))

(clim:define-command-table song-build-menu
  :menu (("Demo" :command com-song-build-demo)
         ("Public" :command com-song-build-public)
         ("$(Publisher)" :command com-song-build-publisher)))

(clim:define-command-table song-region-menu
  :menu (("NTSC" :command com-song-region-ntsc)
         ("PAL" :command com-song-region-pal)
         ("SECAM" :command com-song-region-secam)))

(clim:define-command-table song-run-menu
  :menu (("Build" :menu song-build-menu)
         ("Region" :menu song-region-menu)
         (nil :divider :line)
         ("Generate Source..." :command com-song-generate-source)))

(clim:define-command-table song-help-menu
  :menu (("How to Manage Songs..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table song-menu-bar
  :menu (("Song" :menu song-file-menu)
         ("Edit" :menu song-edit-menu)
         ("View" :menu song-view-menu)
         ("Run" :menu song-run-menu)
         ("Help" :menu song-help-menu)))

(clim:define-application-frame song-inspector-frame (gui-inspector-frame clim:standard-application-frame)
  ()
  (:default-initargs :view-mode :editable)
  (:menu-bar song-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:panes
   (resource-pane :application
                  :display-function 'display-resource-preview
                  :scroll-bars :vertical)
   (project-pane :application
                 :display-function 'display-resource-project-status))
  (:layouts
   (default (clim:vertically () resource-pane))
   (project (clim:vertically () resource-pane project-pane))))

(defmethod initialize-instance :after ((frame song-inspector-frame) &key)
  (call-next-method)
  (subscribe :region-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t))))
  (ensure-printer-discovery-started))

;; Song export commands
(clim:define-command (com-song-save-midi :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Exporting MIDI...~%"))))

(clim:define-command (com-song-save-ogg-vorbis :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Exporting Ogg Vorbis...~%"))))

(clim:define-command (com-song-save-ogg-flac :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Exporting Ogg FLAC...~%"))))

(clim:define-command (com-song-save-mp3 :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Exporting MP3...~%"))))

(clim:define-command (com-song-save-sheet-music :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Exporting sheet music PDF...~%"))))

(clim:define-command (com-song-export-json :command-table clim-internals::global-command-table :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (resource (inspector-resource frame)))
    (when resource
      (let ((path (prompt-save-pathname
                   (format nil "~a.sky.json" (game-resource-title resource)))))
        (when path
          (export-resource-to-json-file resource path)
          (clim-simple-echo:run-in-simple-echo (lambda () (format t "Exported JSON to ~a" path))))))))

(clim:define-command (com-song-export-text :command-table clim-internals::global-command-table :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (resource (inspector-resource frame)))
    (when resource
      (let ((path (prompt-save-pathname (format nil "~a.txt" (game-resource-title resource)))))
        (when path
          (export-resource-to-text-file resource path)
          (clim-simple-echo:run-in-simple-echo (lambda () (format t "Exported text to ~a" path))))))))

(clim:define-command (com-song-export-pdf :command-table clim-internals::global-command-table :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (resource (inspector-resource frame)))
    (when resource
      (let ((path (prompt-save-pathname (format nil "~a.pdf" (game-resource-title resource)))))
        (when path
          (export-resource-to-ps-file resource path
                                      :title (game-resource-title resource)
                                      :author (user-homedir-pathname))
          (uiop:run-program (list "ps2pdf" path (make-pathname :type "pdf" :defaults path)) :output nil)
          (clim-simple-echo:run-in-simple-echo (lambda () (format t "Exported PDF to ~a" path))))))))

(clim:define-command (com-song-new :command-table clim-internals::global-command-table :menu t :name t) ()
  (open-song-inspector nil))

(clim:define-command (com-song-import :command-table clim-internals::global-command-table :menu t :name t) ()
  (error "Song import not implemented."))

(clim:define-command (com-song-open-musescore :command-table clim-internals::global-command-table :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (resource (inspector-resource frame)))
    (when resource
      (let ((path (game-resource-full-path resource)))
        (when (and path (probe-file path))
          (uiop:run-program (list "musescore" path) :output nil :ignore-error-status t))))))

(clim:define-command (com-song-generate-source :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Generating song source...~%"))))

(clim:define-command (com-song-build-demo :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building song (Demo)...~%"))))

(clim:define-command (com-song-build-public :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building song (Public)...~%"))))

(clim:define-command (com-song-build-publisher :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building song (Publisher)...~%"))))

(clim:define-command (com-song-region-ntsc :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :ntsc)
  (publish :region-changed :payload :ntsc))

(clim:define-command (com-song-region-pal :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :pal)
  (publish :region-changed :payload :pal))

(clim:define-command (com-song-region-secam :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :secam)
  (publish :region-changed :payload :secam))

(clim:define-command (com-song-help-inspect :command-table clim-internals::global-command-table :menu t :name t) ()
  (com-open-scripting-guide))

(clim:define-command (com-song-help-developers :command-table clim-internals::global-command-table :menu t :name t) ()
  (com-open-dev-guide))

(clim:define-command (com-song-help-scripting :command-table clim-internals::global-command-table :menu t :name t) ()
  (com-open-fountain-manual))
)
