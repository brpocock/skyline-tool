;;; Skyline-Tool src/gui/gui-song-inspector.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC
;;;
;;; Song inspector frame additions — the frame itself is defined in gui-song.lisp.
;;; This file provides the open-resource-inspector specialization and any
;;; song-specific inspector commands.

(in-package :skyline-tool)

(defmethod open-resource-inspector ((resource game-resource-song) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'song-inspector-frame
                                :resource resource
                                :pretty-name (format nil "~a — ~a ~a"
                                                     (game-resource-title resource)
                                                     *game-title* (machine-directory-name))
                                :view-mode mode)))
