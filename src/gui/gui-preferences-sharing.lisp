(in-package :skyline-tool)

(defun make-sharing-tab-pane (frame)
  (flet ((update-pref (key value)
           (setf (get-pref key) value)
           (clim:redisplay-frame-panes frame :force-p t)))
    (clim:vertically (:name 'sharing-tab-pane :spacing 4)
      (clim:make-pane 'clim:check-box
                      :label "LAN Resource Sharing"
                      :value (get-pref '(:lan-sharing :enabled) nil)
                      :value-changed-callback
                      (lambda (g v)
                        (declare (ignore g))
                        (setf (get-pref '(:lan-sharing :enabled)) v)
                        (if v
                            (start-resource-sharing-server)
                            (stop-resource-sharing-server))
                        (clim:redisplay-frame-panes frame :force-p t)))
      (clim:make-pane 'clim:check-box
                      :label "Share “Dist/” Folder"
                      :value (get-pref '(:dist-sharing :enabled) nil)
                      :value-changed-callback
                      (lambda (g v)
                        (declare (ignore g))
                        (update-pref '(:dist-sharing :enabled) v)))
      (clim:make-pane 'clim:check-box
                      :label "Share Music as Media Server"
                      :value (get-pref '(:music-sharing :enabled) nil)
                      :value-changed-callback
                      (lambda (g v)
                        (declare (ignore g))
                        (update-pref '(:music-sharing :enabled) v)))
      (clim:horizontally (:spacing 2)
        (clim:labelling (:label "Local Domain:"))
        (clim:make-pane 'clim:text-field
                        :value (get-pref '(:lan-sharing :domain) "local.")
                        :width 200
                        :activate-callback
                        (lambda (g v)
                          (declare (ignore g))
                          (update-pref '(:lan-sharing :domain) v))))
      (clim:horizontally (:spacing 2)
        (clim:labelling (:label "Cryptographic Algorithm:"))
        (clim:make-pane 'clim:option-pane
                        :items (mapcar (lambda (a) (cons (string a) a))
                                      (ironclad:list-all-ciphers))
                        :value (get-pref '(:lan-sharing :pubkey-algo) (first (ironclad:list-all-ciphers)))
                        :value-changed-callback
                        (lambda (g v)
                          (declare (ignore g))
                          (update-pref '(:lan-sharing :pubkey-algo) v)))))))
