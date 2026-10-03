(in-package :skyline-tool)

(defun make-issue-tracking-tab-pane (frame)
  (flet ((update-pref (key value)
           (setf (get-pref key) value)
           (clim:redisplay-frame-panes frame :force-p t)))
    (let* ((current-kind (get-pref '(:issue-tracker :kind) :github))
           (tracker-buttons
             (loop for (key label) in '((:github "GitHub") (:gitlab "GitLab") (:bugzilla "Bugzilla"))
                   collect (clim:make-pane 'clim:toggle-button
                                           :label label
                                           :id key
                                           :indicator-type :one-of)))
           (tracker-box
             (clim:make-pane 'clim:radio-box :name :tracker-box
                             :choices tracker-buttons
                             :current-selection (find current-kind tracker-buttons :key #'clim:gadget-id)
                             :value-changed-callback
                             (lambda (g v)
                               (declare (ignore g))
                               (when v (update-pref '(:issue-tracker :kind) (clim:gadget-id v)))))))
      (clim:vertically (:name 'issue-tracking-tab-pane :spacing 4)
        (clim:labelling (:label "Issue tracker kind"
                         :text-style (clim:make-text-style nil :bold :larger))
          tracker-box)
        (clim:horizontally (:spacing 2)
          (clim:labelling (:label "Tracker URL:"))
          (clim:make-pane 'clim:text-field
                          :value (get-pref '(:issue-tracker :url) "")
                          :width 400
                          :activate-callback
                          (lambda (g v) (declare (ignore g)) (update-pref '(:issue-tracker :url) v))))
        (clim:make-pane 'clim:push-button
                        :label "Sign in…"
                        :activate-callback
                        (lambda (g) (declare (ignore g)) (com-sign-in-to-issue-tracker frame)))))))
