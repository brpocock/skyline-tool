;; Preferences Inspector - Proper CLIM gadgets for development preferences

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
        :paper-unit :mm
        :paper-width 215.9
        :paper-height 279.4
        :lan-sharing nil
        :dist-sharing nil
        :music-sharing nil
        :vc-system :git
        :issue-tracker :github
        :tracker-url "https://github.com/adventuring/phantasia"
        :ssh-key-path ""
        :gpg-key-path ""
        :language "en"
        :atarivox-port nil
        :atarivox-volume 12))

(define-constant +paper-sizes+
    '((:us-letter "US Letter" 215.9 279.4)
      (:us-legal "US Legal" 215.9 355.6)
      (:a4 "A4" 210.0 297.0))
  :test 'equalp
  :documentation "Named paper sizes with their dimensions in millimetres.")

(defun convert-unit (value from-unit to-unit)
  "Convert VALUE (a length) between :mm, :cm, :in, :pt.
All stored dimensions are kept internally in millimetres."
  (let ((factors '(:mm 1.0 :cm 10.0 :in 25.4 :pt 25.4/72)))
    (when (or (null value) (zerop value)) (return-from convert-unit 0))
    (let ((in-mm (* value (getf factors from-unit)))
          (to-factor (getf factors to-unit)))
      (/ in-mm to-factor))))

;; --- Frame ---

(clim:define-application-frame preferences-inspector-frame (clim:standard-application-frame)
  ((config :initarg :config :accessor frame-config)
   (dirty :initform nil :accessor frame-dirty)
   ;; Paper section
   (paper-size-box :initform nil :accessor paper-size-box)
   (paper-size-radio :initform nil :accessor paper-size-radio)
   (paper-unit-box :initform nil :accessor paper-unit-box)
   (paper-unit-radio :initform nil :accessor paper-unit-radio)
   (paper-width-field :initform nil :accessor paper-width-field)
   (paper-height-field :initform nil :accessor paper-height-field)
   ;; Sharing section
   (lan-sharing-checkbox :initform nil :accessor lan-sharing-checkbox)
   (dist-sharing-checkbox :initform nil :accessor dist-sharing-checkbox)
   (music-sharing-checkbox :initform nil :accessor music-sharing-checkbox)
   ;; VC section
   (vc-system-box :initform nil :accessor vc-system-box)
   (vc-system-radio :initform nil :accessor vc-system-radio)
   (ssh-key-field :initform nil :accessor ssh-key-field)
   (gpg-key-field :initform nil :accessor gpg-key-field)
   ;; Issue Tracker section
   (issue-tracker-box :initform nil :accessor issue-tracker-box)
   (issue-tracker-radio :initform nil :accessor issue-tracker-radio)
   (tracker-url-field :initform nil :accessor tracker-url-field)
   (watcher-thread :initform nil :accessor prefs-watcher-thread))
  (:menu-bar preferences-inspector-menu-bar)
  (:panes
   (main-pane :application
              :display-function 'display-preferences
              :height 700 :width 800
              :scroll-bars :vertical)
   (status-pane :application
                :display-function 'display-prefs-status
                :height 30 :width 800))
  (:layouts
   (default (clim:vertically () main-pane status-pane))))

(defmethod initialize-instance :after ((frame preferences-inspector-frame) &key)
  (let ((prefs-path (prefs-pathname)))
    (when (probe-file prefs-path)
      (ensure-directories-exist (make-pathname :defaults prefs-path :name nil :type nil))
      (setf (prefs-watcher-thread frame)
            (bt:make-thread
             (lambda ()
               (handler-case
                   (inotify:with-inotify (inot (list (list prefs-path inotify:in-modify)))
                     (loop for ev = (inotify:read-events inot)
                           do (ignore-errors
                               (load-preferences-config)
                               (setf (frame-config frame) *preferences-config*)
                               (clim:redisplay-frame-panes frame :force-p t))))
                 (error (c)
                   (declare (ignore c)))))
             :name "Preferences File Watcher")))))

(defun open-preferences-inspector ()
  (unless *preferences-config*
    (setf *preferences-config* (default-preferences)))
  (let ((frame (clim:make-application-frame
                'preferences-inspector-frame
                :pretty-name "Preferences Inspector"
                :config *preferences-config*)))
    (bt:make-thread (lambda ()
                      (clim:run-frame-top-level frame))
                    :name "Preferences Inspector")))

;; --- Menus ---

(clim:define-command-table preferences-inspector-file-menu
  :menu (("Save" :command com-preferences-save)
         ("Reset to Defaults" :command com-preferences-reset)
         (nil :divider :line)
         ("Close" :command com-close-frame)))

(clim:define-command-table preferences-inspector-help-menu
  :menu (("How to Use..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table preferences-inspector-menu-bar
  :menu (("Preferences" :menu preferences-inspector-file-menu)
         ("Help" :menu preferences-inspector-help-menu)))

;; --- Commands ---

(clim:define-command (com-preferences-save :command-table clim-internals::global-command-table
                                           :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (write-preferences-config frame)
    (setf (frame-dirty frame) nil)))

(clim:define-command (com-preferences-reset :command-table clim-internals::global-command-table
                                            :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (setf (frame-config frame) (default-preferences))
    (setf (frame-dirty frame) t)
    (clim:redisplay-frame-panes frame :force-p t)))

(clim:define-command (com-close-frame :command-table clim-internals::global-command-table
                                      :menu t :name t) ()
  (clim:frame-exit clim:*application-frame*))

;; --- Display ---

(defun display-preferences (frame pane)
  "Display preferences with CLIM gadgets created in display function."
  (let ((config (frame-config frame)))
    (create-preference-gadgets frame pane config)
    (position-preference-gadgets frame pane)))

(defun create-preference-gadgets (frame pane config)
  "Create all preference gadgets on first display."
  ;; Paper Size
  (clim:stream-set-cursor-position pane 10 14)
  (clim:with-text-face (pane :bold)
    (format pane "Paper Size"))
  (unless (paper-size-box frame)
    (let* ((current-size (getf config :paper-size :us-letter))
           (box (clim:make-pane 'clim:radio-box)))
      (setf (paper-size-box frame) box)
      (setf (paper-size-radio frame)
            (loop for (key label w h) in +paper-sizes+
                  collect (clim:make-pane 'clim:toggle-button
                                          :label label
                                          :value (eq current-size key)
                                          :group box
                                          :value-changed-callback
                                          (lambda (g v)
                                            (declare (ignore g))
                                            (when v
                                              (setf (getf (frame-config frame) :paper-size) key)
                                              (setf (getf (frame-config frame) :paper-width) w)
                                              (setf (getf (frame-config frame) :paper-height) h)
                                              (setf (frame-dirty frame) t)
                                              (clim:redisplay-frame-panes frame :force-p t)))))))

    ;; Paper Unit
    (clim:stream-set-cursor-position pane 10 74)
    (clim:with-text-face (pane :bold)
      (format pane "Paper Unit"))
    (unless (paper-unit-box frame)
      (let* ((current-unit (getf config :paper-unit :mm))
             (box (clim:make-pane 'clim:radio-box)))
        (setf (paper-unit-box frame) box)
        (setf (paper-unit-radio frame)
              (loop for unit in '(:mm :cm :in :pt)
                    collect (clim:make-pane 'clim:toggle-button
                                            :label (string unit)
                                            :value (eq current-unit unit)
                                            :group box
                                            :value-changed-callback
                                            (lambda (g v)
                                              (declare (ignore g))
                                              (when v
                                                (let ((old-unit (getf (frame-config frame) :paper-unit))
                                                      (new-unit unit))
                                                  (setf (getf (frame-config frame) :paper-unit) new-unit)
                                                  (let ((w (getf (frame-config frame) :paper-width))
                                                        (h (getf (frame-config frame) :paper-height)))
                                                    (when (and w h)
                                                      (setf (getf (frame-config frame) :paper-width)
                                                            (round (convert-unit w old-unit new-unit) 1))
                                                      (setf (getf (frame-config frame) :paper-height)
                                                            (round (convert-unit h old-unit new-unit) 1)))))
                                                (setf (frame-dirty frame) t)
                                                (clim:redisplay-frame-panes frame :force-p t))))))))

    ;; Paper Width/Height
    (clim:stream-set-cursor-position pane 10 120)
    (format pane "Width:")
    (unless (paper-width-field frame)
      (setf (paper-width-field frame)
            (clim:make-pane 'clim:text-field
                            :value (format nil "~a" (getf config :paper-width 215.9))
                            :width 80
                            :activate-callback
                            (lambda (gadget value)
                              (declare (ignore gadget))
                              (setf (getf (frame-config frame) :paper-width) (ignore-errors (read-from-string value)))
                              (setf (frame-dirty frame) t)))))
    (clim:stream-set-cursor-position pane 10 150)
    (format pane "Height:")
    (unless (paper-height-field frame)
      (setf (paper-height-field frame)
            (clim:make-pane 'clim:text-field
                            :value (format nil "~a" (getf config :paper-height 279.4))
                            :width 80
                            :activate-callback
                            (lambda (gadget value)
                              (declare (ignore gadget))
                              (setf (getf (frame-config frame) :paper-height) (ignore-errors (read-from-string value)))
                              (setf (frame-dirty frame) t)))))

    ;; Sharing Services
    (clim:stream-set-cursor-position pane 10 194)
    (clim:with-text-face (pane :bold)
      (format pane "Sharing Services"))
    (unless (lan-sharing-checkbox frame)
      (setf (lan-sharing-checkbox frame)
            (clim:make-pane 'clim:check-box
                            :label "LAN Resource Sharing"
                            :value (getf config :lan-sharing nil)
                            :value-changed-callback
                            (lambda (g v)
                              (declare (ignore g))
                              (setf (getf (frame-config frame) :lan-sharing) v)
                              (setf (frame-dirty frame) t)
                              (clim:redisplay-frame-panes frame :force-p t)))))
    (unless (dist-sharing-checkbox frame)
      (setf (dist-sharing-checkbox frame)
            (clim:make-pane 'clim:check-box
                            :label "Dist/ Folder LAN Sharing"
                            :value (getf config :dist-sharing nil)
                            :value-changed-callback
                            (lambda (g v)
                              (declare (ignore g))
                              (setf (getf (frame-config frame) :dist-sharing) v)
                              (setf (frame-dirty frame) t)
                              (clim:redisplay-frame-panes frame :force-p t)))))
    (unless (music-sharing-checkbox frame)
      (setf (music-sharing-checkbox frame)
            (clim:make-pane 'clim:check-box
                            :label "Music Media Sharing"
                            :value (getf config :music-sharing nil)
                            :value-changed-callback
                            (lambda (g v)
                              (declare (ignore g))
                              (setf (getf (frame-config frame) :music-sharing) v)
                              (setf (frame-dirty frame) t)
                              (clim:redisplay-frame-panes frame :force-p t)))))

    ;; Version Control
    (clim:stream-set-cursor-position pane 10 334)
    (clim:with-text-face (pane :bold)
      (format pane "Version Control"))
    (format pane "~%  VC System:")
    (unless (vc-system-box frame)
      (let* ((current-vc (getf config :vc-system :git))
             (box (clim:make-pane 'clim:radio-box)))
        (setf (vc-system-box frame) box)
        (setf (vc-system-radio frame)
              (loop for (vc-key vc-label) in '((:git "Git") (:svn "Subversion") (:hg "Mercurial") (:bzr "Bazaar"))
                    collect (clim:make-pane 'clim:toggle-button
                                            :label vc-label
                                            :value (eq current-vc vc-key)
                                            :group box
                                            :value-changed-callback
                                            (lambda (g v)
                                              (declare (ignore g))
                                              (when v
                                                (setf (getf (frame-config frame) :vc-system) vc-key)
                                                (setf (frame-dirty frame) t)
                                                (clim:redisplay-frame-panes frame :force-p t))))))))

    ;; SSH Key
    (clim:stream-set-cursor-position pane 10 434)
    (format pane "SSH Key Path:")
    (unless (ssh-key-field frame)
      (setf (ssh-key-field frame)
            (clim:make-pane 'clim:text-field
                            :value (getf config :ssh-key-path "")
                            :width 400
                            :activate-callback
                            (lambda (gadget value)
                              (declare (ignore gadget))
                              (setf (getf (frame-config frame) :ssh-key-path) value)
                              (setf (frame-dirty frame) t)))))

    ;; GPG Key
    (clim:stream-set-cursor-position pane 10 494)
    (format pane "GPG Key Path:")
    (unless (gpg-key-field frame)
      (setf (gpg-key-field frame)
            (clim:make-pane 'clim:text-field
                            :value (getf config :gpg-key-path "")
                            :width 400
                            :activate-callback
                            (lambda (gadget value)
                              (declare (ignore gadget))
                              (setf (getf (frame-config frame) :gpg-key-path) value)
                              (setf (frame-dirty frame) t)))))

    ;; Issue Tracker
    (clim:stream-set-cursor-position pane 10 554)
    (clim:with-text-face (pane :bold)
      (format pane "Issue Tracker"))
    (format pane "~%  Tracker Type:")
    (unless (issue-tracker-box frame)
      (let* ((current-tracker (getf config :issue-tracker :github))
             (box (clim:make-pane 'clim:radio-box)))
        (setf (issue-tracker-box frame) box)
        (setf (issue-tracker-radio frame)
              (loop for (tracker-key tracker-label) in '((:github "GitHub") (:gitlab "GitLab") (:bugzilla "Bugzilla"))
                    collect (clim:make-pane 'clim:toggle-button
                                            :label tracker-label
                                            :value (eq current-tracker tracker-key)
                                            :group box
                                            :value-changed-callback
                                            (lambda (g v)
                                              (declare (ignore g))
                                              (when v
                                                (setf (getf (frame-config frame) :issue-tracker) tracker-key)
                                                (setf (frame-dirty frame) t)
                                                (clim:redisplay-frame-panes frame :force-p t))))))))

    ;; Tracker URL
    (clim:stream-set-cursor-position pane 10 654)
    (format pane "Tracker URL:")
    (unless (tracker-url-field frame)
      (setf (tracker-url-field frame)
            (clim:make-pane 'clim:text-field
                            :value (getf config :tracker-url "https://github.com/adventuring/phantasia")
                            :width 500
                            :activate-callback
                            (lambda (gadget value)
                              (declare (ignore gadget))
                              (setf (getf (frame-config frame) :tracker-url) value)
                              (setf (frame-dirty frame) t)))))))

(defun position-preference-gadgets (frame pane)
  "Position all gadgets in the pane."
  (clim:stream-set-cursor-position pane 10 30)
  (clim:note-gadget-activated (paper-size-box frame) pane)
  (clim:stream-set-cursor-position pane 10 90)
  (clim:note-gadget-activated (paper-unit-box frame) pane)
  (clim:stream-set-cursor-position pane 70 120)
  (clim:note-gadget-activated (paper-width-field frame) pane)
  (clim:stream-set-cursor-position pane 70 150)
  (clim:note-gadget-activated (paper-height-field frame) pane)
  (clim:stream-set-cursor-position pane 10 220)
  (clim:note-gadget-activated (lan-sharing-checkbox frame) pane)
  (clim:stream-set-cursor-position pane 10 260)
  (clim:note-gadget-activated (dist-sharing-checkbox frame) pane)
  (clim:stream-set-cursor-position pane 10 300)
  (clim:note-gadget-activated (music-sharing-checkbox frame) pane)
  (clim:stream-set-cursor-position pane 10 380)
  (clim:note-gadget-activated (vc-system-box frame) pane)
  (clim:stream-set-cursor-position pane 150 434)
  (clim:note-gadget-activated (ssh-key-field frame) pane)
  (clim:stream-set-cursor-position pane 150 494)
  (clim:note-gadget-activated (gpg-key-field frame) pane)
  (clim:stream-set-cursor-position pane 10 600)
  (clim:note-gadget-activated (issue-tracker-box frame) pane)
  (clim:stream-set-cursor-position pane 150 654)
  (clim:note-gadget-activated (tracker-url-field frame) pane))

(defun display-prefs-status (frame pane)
  "Status bar."
  (clim:stream-set-cursor-position pane 10 8)
  (format pane "Preferences ~a"
          (if (frame-dirty frame) "(unsaved)" "")))

;; --- I/O ---

(defun load-preferences-config ()
  (let ((path (prefs-pathname)))
    (when (probe-file path)
      (with-open-file (f path)
        (setf *preferences-config* (read f nil nil))))))

(defun write-preferences-config (frame)
  (let* ((path (prefs-pathname))
         (dir (make-pathname :defaults path :name nil :type nil)))
    (ensure-directories-exist dir)
    (with-open-file (f path :direction :output :if-exists :supersede)
      (print (frame-config frame) f))))
