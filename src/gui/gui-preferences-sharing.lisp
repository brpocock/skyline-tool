;; Sharing Services Tab - Preferences Inspector
;; Contains LAN resource sharing, Dist folder sharing, and music sharing controls

(in-package :skyline-tool)

(defun display-sharing-services-tab (frame pane)
  "Display the Sharing Services tab content"
  (clim:formatting-table-pane (pane :name "sharing-tab"))
  (make-section-header pane "Sharing Services")
  (unless (lan-sharing-checkbox frame)
    (setf (lan-sharing-checkbox frame)
          (clim:make-pane 'clim:check-box
                          :label "LAN Resource Sharing"
                          :value (get-pref '(:lan-sharing :enabled) nil)
                          :value-changed-callback
                          (lambda (g v)
                            (declare (ignore g))
                            (setf (get-pref '(:lan-sharing :enabled)) v)
                            (save-preferences-now frame)
                            (clim:redisplay-frame-panes frame :force-p t)))))
  (make-full-width-row pane (clim:note-gadget-activated (lan-sharing-checkbox frame) pane))
  (unless (dist-sharing-checkbox frame)
    (setf (dist-sharing-checkbox frame)
          (clim:make-pane 'clim:check-box
                          :label "Share Dist/ Folder"
                          :value (get-pref '(:dist-sharing :enabled) nil)
                          :value-changed-callback
                          (lambda (g v)
                            (declare (ignore g))
                            (setf (get-pref '(:dist-sharing :enabled)) v)
                            (save-preferences-now frame)
                            (clim:redisplay-frame-panes frame :force-p t)))))
  (make-full-width-row pane (clim:note-gadget-activated (dist-sharing-checkbox frame) pane))
  (unless (music-sharing-checkbox frame)
    (setf (music-sharing-checkbox frame)
          (clim:make-pane 'clim:check-box
                          :label "Share Music as Media Server"
                          :value (get-pref '(:music-sharing :enabled) nil)
                          :value-changed-callback
                          (lambda (g v)
                            (declare (ignore g))
                            (setf (get-pref '(:music-sharing :enabled)) v)
                            (save-preferences-now frame)
                            (clim:redisplay-frame-panes frame :force-p t)))))
  (make-full-width-row pane (clim:note-gadget-activated (music-sharing-checkbox frame) pane)))