(in-package :skyline-tool)

(defun make-a11y-tab-pane (frame)
  (flet ((update (key value)
           (setf (get-pref key) value)
           (clim:redisplay-frame-panes frame :force-p t)))
    (let* ((color-theme (get-pref '(:accessibility :color-theme) :normal))
           (keybind-theme (get-pref '(:accessibility :keybind-theme) :gnome))
           (lat-ns (get-pref '(:location :latitude-ns) "N"))
           (lon-ew (get-pref '(:location :longitude-ew) "E"))
           (lat-field (clim:make-pane 'clim:text-field
                                       :value (get-pref '(:location :latitude) "")
                                       :width 3
                                       :activate-callback
                                       (lambda (g v) (declare (ignore g))
                                         (setf (get-pref '(:location :latitude)) v))))
           (lon-field (clim:make-pane 'clim:text-field
                                       :value (get-pref '(:location :longitude) "")
                                       :width 3
                                       :activate-callback
                                       (lambda (g v) (declare (ignore g))
                                         (setf (get-pref '(:location :longitude)) v))))
           (find-me (clim:make-pane 'clim:push-button :label "Find Me"
                                                       :activate-callback
                                                       (lambda (g)
                                                         (declare (ignore g))
                                                         (multiple-value-bind (lat lon ns ew)
                                                             (get-location-from-geoclue)
                                                           (when lat
                                                             (setf (clim:gadget-value lat-field) (format nil "~,4F" lat)
                                                                   (clim:gadget-value lon-field) (format nil "~,4F" lon))
                                                             (setf (get-pref '(:location :latitude)) (format nil "~,4F" lat)
                                                                   (get-pref '(:location :longitude)) (format nil "~,4F" lon)
                                                                   (get-pref '(:location :latitude-ns)) ns
                                                                   (get-pref '(:location :longitude-ew)) ew))))))
           (color-buttons
             (loop for (label key) in '(("Normal (Light)" :normal)
                                          ("Inverted (Dark)" :inverted)
                                          ("Sunrise/Sunset" :sunrise)
                                          ("Desktop (Auto)" :desktop))
                   collect (clim:make-pane 'clim:toggle-button :label label :id key
                                                              :indicator-type :one-of)))
           (color-box
             (clim:make-pane 'clim:radio-box
                             :choices color-buttons
                             :current-selection (find color-theme color-buttons :key #'clim:gadget-id)
                             :value-changed-callback
                             (lambda (g v)
                               (declare (ignore g))
                               (when v (update '(:accessibility :color-theme) (clim:gadget-id v))))))
           (ns-buttons
             (loop for label in '("N" "S")
                   collect (clim:make-pane 'clim:toggle-button :label label :id label
                                                              :indicator-type :one-of)))
           (ns-box
             (clim:make-pane 'clim:radio-box :orientation :horizontal
                             :choices ns-buttons
                             :current-selection (find lat-ns ns-buttons :key #'clim:gadget-id
                                                        :test #'string=)
                             :value-changed-callback
                             (lambda (g v)
                               (declare (ignore g))
                               (when v (update '(:location :latitude-ns) (clim:gadget-id v))))))
           (ew-buttons
             (loop for label in '("E" "W")
                   collect (clim:make-pane 'clim:toggle-button :label label :id label
                                                              :indicator-type :one-of)))
           (ew-box
             (clim:make-pane 'clim:radio-box :orientation :horizontal
                             :choices ew-buttons
                             :current-selection (find lon-ew ew-buttons :key #'clim:gadget-id
                                                        :test #'string=)
                             :value-changed-callback
                             (lambda (g v)
                               (declare (ignore g))
                               (when v (update '(:location :longitude-ew) (clim:gadget-id v))))))
           (shortcut-buttons
             (loop for (label key) in '(("Gnome" :gnome) ("macOS" :macos)
                                          ("Common (CDE)" :common) ("Emacs" :emacs))
                   collect (clim:make-pane 'clim:toggle-button :label label :id key
                                                              :indicator-type :one-of)))
           (shortcut-box
             (clim:make-pane 'clim:radio-box
                             :choices shortcut-buttons
                             :current-selection (find keybind-theme shortcut-buttons :key #'clim:gadget-id)
                             :value-changed-callback
                             (lambda (g v)
                               (declare (ignore g))
                               (when v (update '(:accessibility :keybind-theme) (clim:gadget-id v)))))))
      (clim:vertically (:name 'a11y-tab-pane :spacing 12)
        (clim:horizontally (:name 'color-row :spacing 4)
          (clim:labelling (:label "Color Theme:" :text-style (clim:make-text-style nil :bold :normal)))
          color-box)
        (clim:horizontally (:name 'sunrise-row :spacing 4)
          (clim:labelling (:label "Sunrise/Sunset Location:"))
          lat-field (clim:labelling (:label "°"))
          ns-box
          (clim:labelling (:label "×"))
          lon-field (clim:labelling (:label "°"))
          ew-box
          (clim:labelling (:label " "))
          find-me)
        (clim:horizontally ()
          (clim:labelling (:label "Keyboard Shortcuts Theme:" :text-style (clim:make-text-style nil :bold :normal)))
          shortcut-box)))))
