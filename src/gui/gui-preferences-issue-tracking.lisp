;; Issue Tracking Tab - Preferences Inspector
;; Contains issue tracker configuration: kind selection, URL, and sign-in

(in-package :skyline-tool)

(defun display-issue-tracking-tab (frame pane)
  "Display the Issue Tracking tab content"
  (clim:formatting-table-pane (pane :name "issue-tracking-tab"))
  (make-section-header pane "Issue Tracking")
  (unless (issue-tracker-box frame)
    (setf (issue-tracker-box frame)
          (clim:make-pane 'clim:radio-box)))
  (setf (issue-tracker-radio frame)
        (loop for (key label) in '((:github "GitHub")
                                   (:gitlab "GitLab")
                                   (:bugzilla "Bugzilla"))
              collect (clim:make-pane 'clim:toggle-button
                                      :label label
                                      :value (eq (get-pref '(:issue-tracker :kind) :github) key)
                                      :group (issue-tracker-box frame)
                                      :value-changed-callback
                                      (lambda (g v)
                                        (declare (ignore g))
                                        (when v
                                          (setf (get-pref '(:issue-tracker :kind)) key)
                                          (save-preferences-now frame)
                                          (clim:redisplay-frame-panes frame :force-p t))))))
  (make-full-width-row pane (clim:note-gadget-activated (issue-tracker-box frame) pane))
  (make-label-value-row pane "Tracker URL"
                        (unless (tracker-url-field frame)
                          (setf (tracker-url-field frame)
                                (clim:make-pane 'clim:text-field
                                                :value (get-pref '(:issue-tracker :url) "")
                                                :width 400
                                                :activate-callback
                                                (lambda (gadget value)
                                                  (declare (ignore gadget))
                                                  (setf (get-pref '(:issue-tracker :url)) value)
                                                  (save-preferences-now frame))))))
  (clim:note-gadget-activated (tracker-url-field frame) pane)
  (make-full-width-row pane
                       (unless (sign-in-button frame)
                         (setf (sign-in-button frame)
                               (clim:make-pane 'clim:push-button
                                               :label "Sign in..."
                                               :activate-callback
                                               (lambda (gadget)
                                                 (declare (ignore gadget))
                                                 (com-sign-in-to-issue-tracker frame)))))))
  (clim:note-gadget-activated (sign-in-button frame) pane))