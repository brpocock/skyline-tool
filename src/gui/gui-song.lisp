;;; Skyline-Tool src/gui/gui-song.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-song-reference ()
  :inherit-from 'game-resource-song)

(clim:define-presentation-type game-resource-song-editable ()
  :inherit-from 'game-resource-song)

(clim:define-presentation-type game-resource-song-viewing ()
  :inherit-from 'game-resource-song)

(clim:define-presentation-method clim:present ((resource game-resource-song) (type game-resource-song-reference) stream view &key acceptably for-context-type)
  (declare (ignore view acceptably for-context-type))
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 0)
        (format stream "~3%"))
      (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 125)
        (game-resource-present-icon resource stream))
      (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 150)
        (clim:with-text-face (stream :bold)
          (game-resource-present-title resource stream))
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
        (fixme-interactive-editing-gadget-with-validation stream resource 'game-resource-title)))))

(clim:define-presentation-method clim:present ((resource game-resource-song) (type game-resource-song-viewing) stream view &key acceptably for-context-type)
  (declare (ignore view acceptably for-context-type))
  (clim:surrounding-output-with-border (stream :shape :rounded)
    (format stream "[SONG] ~a" (game-resource-title resource))))

(defun open-song-inspector (resource)
  (open-resource-inspector (or resource (make-instance 'game-resource-song)) :editing))

(defmethod game-resource-action-menu ((resource game-resource-song))
  (list
   (make-menu-item "Inspect..." (lambda () (open-song-inspector resource)))
   (make-menu-item "Open in MuseScore"
                   (lambda ()
                     (uiop:run-program (list "musescore" (game-resource-full-path resource))
                                       :output nil :ignore-error-status t)))))

(clim:define-command-table song-menu-bar
  :menu (("Song" :menu song-file-menu)
         ("Edit" :menu song-edit-menu)
         ("Run" :menu song-run-menu)
         ("View" :menu song-view-menu)
         ("Help" :menu song-help-menu)))

(clim:define-command-table song-file-menu
  :menu (("New..." :command com-song-new)
         ("Import File..." :command com-song-import)
         ("Open in MuseScore" :command com-song-open-musescore)
         (nil :divider :line)
         ("Save" :menu song-save-menu)
         ("Send to" :menu send-to-menu)
         ("Print to" :menu print-to-menu)
         (nil :divider :line)
         ("Close" :command com-preview-close)))

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

(clim:define-command-table song-edit-menu
  :menu (("Cut" :command com-song-cut)
         ("Copy" :command com-song-copy)
         ("Paste" :command com-song-paste)
         (nil :divider :line)
         ("Find..." :command com-song-find)))

(clim:define-command-table song-view-menu
  :menu (("Editable" :command com-song-toggle-editable)
          ("Project Pane" :command com-song-toggle-project-pane)))

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
         ("Make Source/Generated/7800/Assets/Song.ThisSongName.s..." :command com-song-generate-source)))

(clim:define-command-table song-help-menu
  :menu (("How to Inspect Songs..." :command com-song-help-inspect)
         ("Skyline-Tool Developers' Guide..." :command com-song-help-developers)
         ("Skyline-Tool Scripting Guide..." :command com-song-help-scripting)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-application-frame song-inspector-frame ()
  ()
  (:command-table (song-menu-bar))
  (:menu-bar song-menu-bar)
  (:panes
   (resource-pane :application
                  :display-function 'display-resource-preview
                  :scroll-bars :vertical)
   (project-pane :application
                 :display-function 'display-resource-project-status))
  (:layouts
   (default (clim:vertically () resource-pane))
   (project (clim:vertically () resource-pane project-pane))))
