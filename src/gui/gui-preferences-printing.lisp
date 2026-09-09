(in-package :skyline-tool)

(defun make-printing-tab-pane (frame)
  (flet ((update-pref (key value)
           (setf (get-pref key) value)
           (clim:redisplay-frame-panes frame :force-p t)))
    (let* ((current-size (get-pref '(:paper :size) :us-letter))
           (paper-buttons
             (loop for (key label w h) in +paper-sizes+
                   collect (clim:make-pane 'clim:toggle-button
                                           :label (format nil "~a  |  ~,1f mm × ~,1f mm" label w h)
                                           :id key
                                           :indicator-type :one-of)))
           (paper-box
             (clim:make-pane 'clim:radio-box :name :paper-size-box
                             :choices paper-buttons
                             :current-selection (find current-size paper-buttons :key #'clim:gadget-id)
                             :value-changed-callback
                             (lambda (g v)
                               (declare (ignore g))
                               (when v (update-pref '(:paper :size) (clim:gadget-id v)))))))
      (clim:vertically (:name 'printing-tab-pane :spacing 4)
        (clim:labelling (:label "Paper Size" :text-style (clim:make-text-style nil :bold :larger))
          paper-box)
        (clim:labelling (:label "Custom")
          (clim:horizontally (:spacing 4)
            (clim:make-pane 'clim:text-field
                            :value (format nil "~a" (get-pref '(:paper :width) 215.9))
                            :width 80
                            :activate-callback (lambda (g v)
                                                 (declare (ignore g))
                                                 (update-pref '(:paper :width)
                                                              (parse-number v))))
            (clim:make-pane 'clim:option-pane
                            :items '(("mm" :mm) ("cm" :cm) ("in" :in) ("pt" :pt) ("pc" :pc))
                            :value (get-pref '(:units :length-width) :mm)
                            :value-changed-callback
                            (lambda (g v)
                              (declare (ignore g))
                              (update-pref '(:units :length-width) v)))
            (clim:labelling (:label "×"))
            (clim:make-pane 'clim:text-field
                            :value (format nil "~a" (get-pref '(:paper :height) 279.4))
                            :width 80
                            :activate-callback (lambda (g v)
                                                 (declare (ignore g))
                                                 (update-pref '(:paper :height)
                                                              (parse-number v))))
            (clim:make-pane 'clim:option-pane
                            :items '(("mm" :mm) ("cm" :cm) ("in" :in) ("pt" :pt) ("pc" :pc))
                            :value (get-pref '(:units :length-height) :mm)
                            :value-changed-callback
                            (lambda (g v)
                              (declare (ignore g))
                              (update-pref '(:units :length-height) v)))))))))
