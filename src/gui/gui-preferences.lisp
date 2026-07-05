;; Preferences Inspector - Handles development preferences with expanded service names

(in-package :skyline-tool)

;; --- Preferences data model ---

(defvar *preferences-config* nil
  "Global preferences plist. Keys:
   :paper-size, :lan-sharing, :dist-sharing, :music-sharing,
   :vc-system, :issue-tracker, :tracker-url,
   :ssh-key-path, :gpg-key-path,
   :theme, :language, :show-tips, :auto-save, :driver")

(defun default-preferences ()
  (list :paper-size :us-letter
        :lan-sharing nil :dist-sharing nil :music-sharing nil
        :vc-system :git
        :issue-tracker :github
        :tracker-url "https://github.com/adventuring/phantasia"
        :ssh-key-path "" :gpg-key-path ""
        :theme "light" :language "en" :show-tips t :auto-save t :driver "7800"))

;; --- Frame ---

(defun open-preferences-inspector ()
  (unless *preferences-config*
    (setf *preferences-config* (default-preferences)))
  (let ((frame (clim:make-application-frame
                'preferences-inspector-frame
                :pretty-name "Preferences Inspector"
                :config *preferences-config*)))
    (clim:run-frame-top-level frame)))

(clim:define-application-frame preferences-inspector-frame (gui-inspector-frame)
  ((config :initarg :config :accessor frame-config)
   (dirty :initform nil :accessor frame-dirty))
  (:menu-bar preferences-inspector-menu-bar)
  (:panes
   (editor-pane :application :display-function 'display-preferences
                                :height 700 :width 800 :scroll-bars :vertical))
  (:layouts
   (default (clim:vertically () editor-pane))))

;; --- Commands ---

(clim:define-command (com-preferences-save :command-table clim-internals::global-command-table
                                           :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (write-preferences-config frame)
    (setf (frame-dirty frame) nil)))

(clim:define-command (com-preferences-close :command-table clim-internals::global-command-table
                                            :menu t :name t) ()
  (clim:frame-exit clim:*application-frame*))

(clim:define-command (com-preferences-reset :command-table clim-internals::global-command-table
                                            :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (setf (frame-config frame) (default-preferences))
    (setf (frame-dirty frame) t)
    (clim:redisplay-frame-panes frame :force-p t)))

;; --- Sharing service commands ---

(clim:define-command (com-toggle-lan-sharing :command-table clim-internals::global-command-table
                                             :menu t :name t) ()
  "Toggle LAN resource sharing service."
  (let ((frame clim:*application-frame*))
    (setf (getf (frame-config frame) :lan-sharing)
          (not (getf (frame-config frame) :lan-sharing)))
    (when (getf (frame-config frame) :lan-sharing)
      (format t "~&Starting LAN resource sharing service...~%")
      ;; TODO: Start actual LAN sharing service
      )
    (when (not (getf (frame-config frame) :lan-sharing))
      (format t "~&Stopping LAN resource sharing service...~%")
      ;; TODO: Stop actual LAN sharing service
      )
    (setf (frame-dirty frame) t)
    (clim:redisplay-frame-panes frame :force-p t)))

(clim:define-command (com-toggle-dist-sharing :command-table clim-internals::global-command-table
                                              :menu t :name t) ()
  "Toggle Dist/ folder sharing over LAN."
  (let ((frame clim:*application-frame*))
    (setf (getf (frame-config frame) :dist-sharing)
          (not (getf (frame-config frame) :dist-sharing)))
    (when (getf (frame-config frame) :dist-sharing)
      (format t "~&Starting Dist/ folder LAN sharing...~%")
      ;; TODO: Start actual Dist sharing service
      )
    (when (not (getf (frame-config frame) :dist-sharing))
      (format t "~&Stopping Dist/ folder LAN sharing...~%")
      ;; TODO: Stop actual Dist sharing service
      )
    (setf (frame-dirty frame) t)
    (clim:redisplay-frame-panes frame :force-p t)))

(clim:define-command (com-toggle-music-sharing :command-table clim-internals::global-command-table
                                               :menu t :name t) ()
  "Toggle Music sharing over Media Sharing."
  (let ((frame clim:*application-frame*))
    (setf (getf (frame-config frame) :music-sharing)
          (not (getf (frame-config frame) :music-sharing)))
    (when (getf (frame-config frame) :music-sharing)
      (format t "~&Starting Music media sharing service...~%")
      ;; TODO: Start actual Music sharing service
      )
    (when (not (getf (frame-config frame) :music-sharing))
      (format t "~&Stopping Music media sharing service...~%")
      ;; TODO: Stop actual Music sharing service
      )
    (setf (frame-dirty frame) t)
    (clim:redisplay-frame-panes frame :force-p t)))

;; --- Paper size commands ---

(clim:define-command (com-set-paper-us-letter :command-table clim-internals::global-command-table
                                              :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (setf (getf (frame-config frame) :paper-size) :us-letter)
    (setf (getf (frame-config frame) :paper-width) nil)
    (setf (getf (frame-config frame) :paper-height) nil)
    (setf (frame-dirty frame) t)
    (clim:redisplay-frame-panes frame :force-p t)))

(clim:define-command (com-set-paper-a4 :command-table clim-internals::global-command-table
                                       :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (setf (getf (frame-config frame) :paper-size) :a4)
    (setf (getf (frame-config frame) :paper-width) nil)
    (setf (getf (frame-config frame) :paper-height) nil)
    (setf (frame-dirty frame) t)
    (clim:redisplay-frame-panes frame :force-p t)))

(clim:define-command (com-set-paper-custom :command-table clim-internals::global-command-table
                                           :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (setf (getf (frame-config frame) :paper-size) :custom)
    (setf (frame-dirty frame) t)
    (clim:redisplay-frame-panes frame :force-p t)))

;; --- Menu bar ---

(clim:define-command-table preferences-inspector-file-menu
  :menu (("Save Preferences" :command com-preferences-save)
         ("Save as" :menu inspector-save-as-menu)
         (nil :divider :line)
         ("Print to" :menu inspector-print-to-menu)
         (nil :divider :line)
         ("Close" :command com-preferences-close)))

(clim:define-command-table preferences-inspector-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table preferences-inspector-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :menu t :name t :documentation "Toggle editable mode")))

(clim:define-command-table preferences-inspector-help-menu
  :menu (("How to Manage Preferences..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-scripting-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table preferences-inspector-menu-bar
  :menu (("Preferences" :menu preferences-inspector-file-menu)
         ("Edit" :menu preferences-inspector-edit-menu)
         ("View" :menu preferences-inspector-view-menu)
         ("Help" :menu preferences-inspector-help-menu)))

;; --- Display function ---

(defun display-preferences (frame pane)
  (clim:window-clear pane)
  (let ((config (frame-config frame)))

    (clim:with-text-family (pane :fix)
      (clim:with-text-face (pane :bold)
        (format pane "PDF Export & Printing~%"))
      (clim:formatting-table (pane)
        (clim:formatting-row (pane)
          (clim:formatting-cell (pane) (format pane "Paper Size:"))
          (clim:formatting-cell (pane)
            (clim:with-output-as-presentation (pane (getf config :paper-size) '(member :us-letter :a4 :custom))
              (format pane "~[US Letter~;A4~;Custom~]" (ecase (getf config :paper-size)
                                                            (:us-letter 0)
                                                            (:a4 1)
                                                            (:custom 2)))))))
      (format pane "~%")

      (clim:with-text-face (pane :bold)
        (format pane "Network Sharing~%"))
      (clim:formatting-table (pane)
        (clim:formatting-row (pane)
          (clim:formatting-cell (pane) (format pane "LAN resource sharing:"))
          (clim:formatting-cell (pane)
            (clim:with-output-as-presentation (pane (getf config :lan-sharing) '(member t nil))
              (format pane "~:[No~;Yes~]" (getf config :lan-sharing)))))
        (clim:formatting-row (pane)
          (clim:formatting-cell (pane) (format pane "Share Dist/ folder over LAN:"))
          (clim:formatting-cell (pane)
            (clim:with-output-as-presentation (pane (getf config :dist-sharing) '(member t nil))
              (format pane "~:[No~;Yes~]" (getf config :dist-sharing)))))
        (clim:formatting-row (pane)
          (clim:formatting-cell (pane) (format pane "Share Music over Media Sharing:"))
          (clim:formatting-cell (pane)
            (clim:with-output-as-presentation (pane (getf config :music-sharing) '(member t nil))
              (format pane "~:[No~;Yes~]" (getf config :music-sharing))))))
      (format pane "~%")

      (clim:with-text-face (pane :bold)
        (format pane "Version Control~%"))
      (clim:formatting-table (pane)
        (clim:formatting-row (pane)
          (clim:formatting-cell (pane) (format pane "System:"))
          (clim:formatting-cell (pane)
            (clim:with-output-as-presentation (pane (getf config :vc-system) '(member :git :subversion :cvs :rcs :mercurial))
              (format pane "~a"
                      (case (getf config :vc-system)
                        (:git "Git (Git)")
                        (:subversion "Subversion (SVN)")
                        (:cvs "Concurrent Versioning System (CVS)")
                        (:rcs "Revision Control System (RCS)")
                        (:mercurial "Mercurial (Hg)")
                        (t (getf config :vc-system)))))))
        (clim:formatting-row (pane)
          (clim:formatting-cell (pane) (format pane "Remotes:"))
          (clim:formatting-cell (pane)
            (format pane "origin   git@github.com:adventuring/phantasia.git")))
        (clim:formatting-row (pane)
          (clim:formatting-cell (pane) (format pane "SSH Key:"))
          (clim:formatting-cell (pane)
            (clim:with-output-as-presentation (pane (getf config :ssh-key-path) 'string)
              (format pane "~a" (if (plusp (length (getf config :ssh-key-path)))
                                     (getf config :ssh-key-path)
                                     "{ ... }")))))
        (clim:formatting-row (pane)
          (clim:formatting-cell (pane) (format pane "GPG Key:"))
          (clim:formatting-cell (pane)
            (clim:with-output-as-presentation (pane (getf config :gpg-key-path) 'string)
              (format pane "~a" (if (plusp (length (getf config :gpg-key-path)))
                                     (getf config :gpg-key-path)
                                     "{ ... }"))))))
      (format pane "~%")

      (clim:with-text-face (pane :bold)
        (format pane "Issue Tracker~%"))
      (clim:formatting-table (pane)
        (clim:formatting-row (pane)
          (clim:formatting-cell (pane) (format pane "Tracker Type:"))
          (clim:formatting-cell (pane)
            (clim:with-output-as-presentation (pane (getf config :issue-tracker) '(member :github :gitlab :bugzilla))
              (format pane "~a"
                      (case (getf config :issue-tracker)
                        (:github "GitHub")
                        (:gitlab "GitLab")
                        (:bugzilla "Bugzilla")
                        (t (getf config :issue-tracker)))))))
        (clim:formatting-row (pane)
          (clim:formatting-cell (pane) (format pane "Tracker URL:"))
          (clim:formatting-cell (pane)
            (clim:with-output-as-presentation (pane (getf config :tracker-url) 'string)
              (format pane "~a" (getf config :tracker-url))))))
      (format pane "~%"))))

;; --- I/O ---

(defun load-preferences-config ()
  (let* ((path (merge-pathnames #p".config/Skyline-Tool/Preferences.lisp"
                                (user-homedir-pathname))))
    (when (probe-file path)
      (with-open-file (f path)
        (setf *preferences-config* (read f nil nil))))))

(defun write-preferences-config (frame)
  (let* ((path (merge-pathnames #p".config/Skyline-Tool/Preferences.lisp"
                                (user-homedir-pathname)))
         (dir (make-pathname :defaults path :name nil :type nil)))
    (ensure-directories-exist dir)
    (with-open-file (f path :direction :output :if-exists :supersede)
      (print (frame-config frame) f))))