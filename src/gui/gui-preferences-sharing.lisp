(in-package :skyline-tool)

(defun display-network-tab (frame pane)
  ;; LAN Resource Sharing checkbox
  (unless (lan-sharing-checkbox frame)
    (setf (lan-sharing-checkbox frame)
          (clim:make-pane 'clim:check-box
                          :label "LAN Resource Sharing"
                          :value (get-pref '(:lan-sharing :enabled) nil)
                          :value-changed-callback
                          (lambda (g value)
                            (declare (ignore g))
                            (setf (get-pref '(:lan-sharing :enabled)) value)
                            (if value
                                (start-resource-sharing-server)
                                (or :TODO (stop-resource-sharing-server)))
                            (clim:redisplay-frame-panes frame :force-p t)))))
  (make-full-width-row pane (clim:note-gadget-activated (lan-sharing-checkbox frame) pane))
  
  ;; Share "Dist/" Folder checkbox
  (unless (dist-sharing-checkbox frame)
    (setf (dist-sharing-checkbox frame)
          (clim:make-pane 'clim:check-box
                          :label "Share “Dist/” Folder"
                          :value (get-pref '(:dist-sharing :enabled) nil)
                          :value-changed-callback
                          (lambda (g v)
                            (declare (ignore g))
                            (setf (get-pref '(:dist-sharing :enabled)) v)
                            (clim:redisplay-frame-panes frame :force-p t)))))
  (make-full-width-row pane (clim:note-gadget-activated (dist-sharing-checkbox frame) pane))
  
  ;; Share Music as Media Server checkbox
  (unless (music-sharing-checkbox frame)
    (setf (music-sharing-checkbox frame)
          (clim:make-pane 'clim:check-box
                          :label "Share Music as Media Server"
                          :value (get-pref '(:music-sharing :enabled) nil)
                          :value-changed-callback
                          (lambda (g v)
                            (declare (ignore g))
                            (setf (get-pref '(:music-sharing :enabled)) v)
                            (clim:redisplay-frame-panes frame :force-p t)))))
  (clim:note-gadget-activated (music-sharing-checkbox frame) pane)
  
  ;; Local Domain text field
  
  (make-label-value-row
   pane "Local Domain:"
   (clim:make-pane 'clim:text-field
                   :value (get-pref '(:lan-sharing :domain) "local.")
                   :width 200
                   :activate-callback
                   (lambda (g v)
                     (declare (ignore g))
                     (setf (get-pref '(:lan-sharing :domain)) v)
                     (clim:redisplay-frame-panes frame :force-p t))))
  
  ;; Cryptographic Algorithm option menu
  (let* ((algo-list (ironclad:list-all-ciphers))
         (algo-box (clim:make-pane 'clim:radio-box)))
    (setf (lan-pubkey-algo-field frame) algo-box)
    (loop for algo in algo-list
          collect (clim:make-pane 'clim:toggle-button
                                  :label (symbol-name algo)
                                  :value (eq (get-pref '(:lan-sharing :pubkey-algo)) algo)
                                  :group algo-box
                                  :value-changed-callback
                                  (lambda (g v)
                                    (declare (ignore g))
                                    (when v
                                      (setf (get-pref '(:lan-sharing :pubkey-algo)) algo)
                                      (clim:redisplay-frame-panes frame :force-p t)))))
    (make-full-width-row pane (clim:note-gadget-activated algo-box pane))))
