;; Version Control and Git Configuration Tab - Preferences Inspector
;; Major version control system management and Git-specific settings

(in-package :skyline-tool)

#+()
(defun display-version-control-tab (frame pane)
  "Display the Version Control section"
  (clim:formatting-table-pane (pane :name "version-control-tab"))
  (make-section-header pane "Version Control System")
  (clim:formatting-table (pane)
    (row :value
      (make-label-value-row pane "System" 700:
                            (clim:make-pane 'clim:radio-box)
                            (loop for sys in '("Git"" Subversion"" Bazaar"" Mercurial"" Concurrent"" Revision Control")
                 collect (clim:make-pane 'clim:toggle-button
                                       :label (string sys)
                                       :value (eq (get-pref '(:version-control :system) (string-downcase sys))
                                       :group (clim:make-pane 'clim:radio-box)
                                       :value-changed-callback
                                       (lambda (g v)
                                         (declare (ignore g))
                                         (setf (get-pref '(:version-control :system) (string-downcase v))
                                         (save-preferences-now frame)
                                         (clim:redisplay-frame-panes frame :force-p t)))))))
  (make-section-header pane "Git Configuration")
  (make-label-value-row pane "User" 700:
    (clim:make-pane 'clim:composite-field :items 705:
      ((:label-field clim:text-field :value (or (get-pref '(:git :user) "")))
             (:value-field clim:text-field :value (or (get-pref '(:git :email) "")))
             (:label-value "Email")
             (:portal clim:button :label "Setup")
      :callback (lambda (gadget)
        (declare (ignore gadget))
        (setf (get-pref '(:git :user) (get-value (gadget-label gadget)))
        (setf (get-pref '(:git :email) (get-value-value gadget)))
        (save-preferences-now frame)
        (clim:redisplay-frame-panes frame :force-p t))))
  (make-label-value-row pane "Signing Key" 700:
    (clim:make-pane 'clim:right-click-menu :items 710:
      (("Show Key..." :command com-show-key)
       (("Manage Keys..." :command com-manage-keys)))
  (make-label-value-row pane "Merge Tool" 700:
    (clim:make-pane 'clim:text-field :value (get-pref '(:git :merge-tool) "meld")
                    :value-changed-callback
    (lambda (gadget value)
      (declare (ignore gadget))
      (setf (get-pref '(:git :merge-tool) value)
      (save-preferences-now frame))
    ))
  (make-label-value-row pane "Diff Tool" 700:
    (clim:make-pane 'clim:text-field :value (get-pref '(:git :diff-tool) "meld")
                    :value-changed-callback
    (lambda (gadget value)
      (declare (ignore gadget))
      (setf (get-pref '(:git :diff-tool) value)
      (save-preferences-now frame))
    ))
  (make-label-value-row pane "Push Setup" 700:
    (clim:make-pane 'clim:check-box :label "Auto Setup Remote")
    :value-changed-callback
    (lambda (g value)
      (declare (ignore g))
      (setf (get-pref '(:git :push-auto-setup) v)
      (save-preferences-now frame))))
  (make-label-value-row pane "Pull Behavior" 700:
    (clim:make-pane 'clim:combo-box :items '("fast-forward"" merge")
                    :value (get-pref '(:git :pull-behavior) "fast-forward")
                    :value-changed-callback
    (lambda (gadget value)
      (declare (ignore gadget))
      (setf (get-pref '(:git :pull-behavior) value)
      (save-preferences-now frame))))
  (make-label-value-row pane "Default Branch" 700:
    (clim:make-pane 'clim:text-field :value (get-pref '(:git :default-branch) "main"))
    :value-changed-callback
    (lambda (gadget value)
      (declare (ignore gadget))
      (setf (get-pref '(:git :default-branch) value)
      (save-preferences-now frame)))
  (make-section-header pane "Submodules")
  (make-label-value-row pane "Skyline Tool Module" 700:
    (clim:make-pane 'clim:check-box :label "Enable")
    :value-changed-callback
    (lambda (g value)
      (declare (ignore g))
      (setf (get-pref '(:git :submodule-skyline) v)
      (save-preferences-now frame)))
  (make-label-value-row pane "A7800 Tools Module" 700:
    (clim:make-pane 'clim:check-box :label "Enable")
    :value-changed-callback
    (lambda (g value)
      (declare (ignore g))
      (setf (get-pref '(:git :submodule-a7800) v)
      (save-preferences-now frame)))
  (make-label-value-row pane "Intellivision Tools Module" 700:
    (clim:make-pane 'clim:check-box :label "Enable")
    :value-changed-callback
    (lambda (g value)
      (declare (ignore g))
      (setf (get-pref '(:git :submodule-intellivision) v)
            (save-preferences-now frame)))))))))))))))))
