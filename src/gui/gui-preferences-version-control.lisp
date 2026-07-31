(in-package :skyline-tool)

(defun make-version-control-tab-pane (frame)
  (flet ((update-pref (key value)
           (setf (get-pref key) value)
           (clim:redisplay-frame-panes frame :force-p t)))
    (let* ((current-vcs (get-pref '(:version-control :system) :git))
           (vcs-buttons
             (loop for (key label) in '((:git "Git") (:bzr "Bazaar")
                                        (:svn "Subversion") (:hg "Mercurial")
                                        (:cvs "Concurrent") (:rcs "Revision Control"))
                   collect (clim:make-pane 'clim:toggle-button
                                           :label (format nil "~a  | ~(~a~)" label key)
                                           :id key
                                           :indicator-type :one-of)))
           (vcs-box
             (clim:make-pane 'clim:radio-box :name :vcs-box
                             :choices vcs-buttons
                             :current-selection (find current-vcs vcs-buttons :key #'clim:gadget-id)
                             :value-changed-callback
                             (lambda (g v)
                               (declare (ignore g))
                               (when v (update-pref '(:version-control :system) (clim:gadget-id v)))))))
      (clim:vertically (:name 'version-control-tab-pane :spacing 4)
        (clim:labelling (:label "Version Control System"
                         :text-style (clim:make-text-style nil :bold :larger))
          vcs-box)
        (clim:labelling (:label "User" :text-style (clim:make-text-style nil :bold :larger))
          (clim:vertically (:spacing 2)
            (clim:horizontally (:spacing 2)
              (clim:labelling (:label "Name:"))
              (clim:make-pane 'clim:text-field
                              :value (get-pref '(:git :user) "")
                              :width 300
                              :activate-callback
                              (lambda (g v) (declare (ignore g)) (update-pref '(:git :user) v))))
            (clim:horizontally (:spacing 2)
              (clim:labelling (:label "eMail:"))
              (clim:make-pane 'clim:text-field
                              :value (get-pref '(:git :email) "")
                              :width 300
                              :activate-callback
                              (lambda (g v) (declare (ignore g)) (update-pref '(:git :email) v))))))
        (clim:horizontally (:spacing 2)
          (clim:labelling (:label "Signing key:"))
          (clim:make-pane 'clim:option-pane
                          :items (list (cons "None" nil))
                          :value nil
                          :value-changed-callback
                          (lambda (g v) (declare (ignore g)) (update-pref '(:git :signing-key) v)))
          (clim:make-pane 'clim:push-button
                          :label "Generate and publish a new key…"
                          :activate-callback (lambda (g) (declare (ignore g))
                                              (com-generate-signing-key frame))))
        (clim:make-pane 'clim:check-box
                        :label "Sign Commits"
                        :value (get-pref '(:git :sign-commits) nil)
                        :value-changed-callback
                        (lambda (g v) (declare (ignore g)) (update-pref '(:git :sign-commits) v)))
        (clim:make-pane 'clim:check-box
                        :label "Sign Tags"
                        :value (get-pref '(:git :sign-tags) nil)
                        :value-changed-callback
                        (lambda (g v) (declare (ignore g)) (update-pref '(:git :sign-tags) v)))
        (clim:labelling (:label "Tools" :text-style (clim:make-text-style nil :bold :larger))
          (clim:vertically (:spacing 2)
            (clim:horizontally (:spacing 2)
              (clim:labelling (:label "Merge:"))
              (clim:make-pane 'clim:option-pane
                              :items (list (cons "meld" "meld") (cons "kdiff3" "kdiff3")
                                           (cons "vimdiff" "vimdiff") (cons "emerge" "emerge"))
                              :value (get-pref '(:git :merge-tool) "meld")
                              :value-changed-callback
                              (lambda (g v) (declare (ignore g)) (update-pref '(:git :merge-tool) v)))
              (clim:make-pane 'clim:check-box
                              :label "Prompt first"
                              :value (get-pref '(:git :merge-prompt) nil)
                              :value-changed-callback
                              (lambda (g v) (declare (ignore g)) (update-pref '(:git :merge-prompt) v))))
            (clim:horizontally (:spacing 2)
              (clim:labelling (:label "Diff:"))
              (clim:make-pane 'clim:option-pane
                              :items (list (cons "meld" "meld") (cons "kdiff3" "kdiff3")
                                           (cons "vimdiff" "vimdiff") (cons "emerge" "emerge"))
                              :value (get-pref '(:git :diff-tool) "meld")
                              :value-changed-callback
                              (lambda (g v) (declare (ignore g)) (update-pref '(:git :diff-tool) v)))
              (clim:make-pane 'clim:check-box
                              :label "Prompt first"
                              :value (get-pref '(:git :diff-prompt) nil)
                              :value-changed-callback
                              (lambda (g v) (declare (ignore g)) (update-pref '(:git :diff-prompt) v))))))
        (clim:labelling (:label "Remotes" :text-style (clim:make-text-style nil :bold :larger))
          (clim:vertically (:spacing 2)
            (clim:labelling (:label (format nil "~25a  ~40@a  ~a" "Remote" "URL" "Fetch")))
            (clim:labelling (:label (format nil "~25a  ~40@a  ~a"
                                            "origin"
                                            "git@github.com:brpocock/Phantasia"
                                            "+refs/heads/:refs/remotes/origin/")))
            (clim:make-pane 'clim:push-button :label "+"
                            :activate-callback (lambda (g) (declare (ignore g))
                                                (com-add-remote frame)))))
        (clim:labelling (:label "Submodules" :text-style (clim:make-text-style nil :bold :larger))
          (clim:vertically (:spacing 2)
            (clim:make-pane 'clim:check-box
                            :label "Skyline-Tool and Eightbol"
                            :value (get-pref '(:git :submodule-skyline) t)
                            :value-changed-callback
                            (lambda (g v) (declare (ignore g)) (update-pref '(:git :submodule-skyline) v)))
            (clim:make-pane 'clim:check-box
                            :label "Atari 7800 Tools"
                            :value (get-pref '(:git :submodule-a7800) nil)
                            :value-changed-callback
                            (lambda (g v) (declare (ignore g)) (update-pref '(:git :submodule-a7800) v)))
            (clim:make-pane 'clim:check-box
                            :label "Intellivision Tools"
                            :value (get-pref '(:git :submodule-intellivision) nil)
                            :value-changed-callback
                            (lambda (g v) (declare (ignore g)) (update-pref '(:git :submodule-intellivision) v)))))
        (clim:make-pane 'clim:check-box
                        :label "When pushing, automatically set up new branches on remote"
                        :value (get-pref '(:git :push-auto-setup) nil)
                        :value-changed-callback
                        (lambda (g v) (declare (ignore g)) (update-pref '(:git :push-auto-setup) v)))
        (clim:horizontally (:spacing 2)
          (clim:make-pane 'clim:check-box
                          :label "When pulling, automatically"
                          :value (get-pref '(:git :pull-automatically) nil)
                          :value-changed-callback
                          (lambda (g v) (declare (ignore g)) (update-pref '(:git :pull-automatically) v)))
          (clim:make-pane 'clim:option-pane
                          :items '(("fast-forward only" :ff-only) ("merge" :merge) ("rebase" :rebase))
                          :value (get-pref '(:git :pull-behavior) :ff-only)
                          :value-changed-callback
                          (lambda (g v) (declare (ignore g)) (update-pref '(:git :pull-behavior) v))))
        (clim:horizontally (:spacing 2)
          (clim:labelling (:label "Default Branch:"))
          (clim:make-pane 'clim:text-field
                          :value (get-pref '(:git :default-branch) "main")
                          :width 200
                          :activate-callback
                          (lambda (g v) (declare (ignore g)) (update-pref '(:git :default-branch) v))))))))
