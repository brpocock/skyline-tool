(in-package :skyline-tool)

;; Key Inspector - for viewing/editing key definitions

(defvar +valid-key-keys+ "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.:-=!@#$%^&*()"
  "Valid characters for key names")

;; Load keys from a text file (or create empty list)
(defun load-keys (&optional (path "Source/Tables/Keys.txt"))
  "Return a list of key names from PATH or create empty list"
  (let ((full (merge-pathnames path (uiop:getcwd))))
    (if (probe-file full)
        (with-open-file (f full)
          (loop for line = (read-line f nil nil)
                while line
                collect (string-trim " " line)))
        '())))

;; Save keys to file
(defun save-keys (keys &optional (path "Source/Tables/Keys.txt"))
  "Write KEYS to PATH"
  (let ((full (merge-pathnames path (uiop:getcwd))))
    (ensure-directories-exist full)
    (with-open-file (f full :direction :output :if-exists :supersede)
      (dolist (key keys)
        (format f "~a~%
" key))))

;; Key Inspector Frame
(clim:define-application-frame keys-inspector-frame (resource-inspector-mixin clim:standard-application-frame)
  ((path :initarg :path :accessor frame-path)
   (keys :initarg :keys :accessor frame-keys))
  (:menu-bar keys-inspector-menu-bar)
  (:panes
   (editor-pane :application :display-function 'display-resource-inspector
                :height 600 :width 500
                :scroll-bars :vertical)
   (interactor :interactor :height 80 :width 500))
  (:layouts (default (clim:vertically () editor-pane interactor))))

;; Menu definitions
(clim:define-command-table keys-inspector-help-menu
  :menu (("How to Edit Keys..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-scripting-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table keys-inspector-menu-bar
  :menu (("File" :menu (("Close" :command com-keys-close)))
         ("Help" :menu keys-inspector-help-menu)))

;; Display function
(defmethod display-inspector-content ((frame keys-inspector-frame) pane)
  (display-keys frame pane))

(defun display-keys (frame pane)
  (clim:window-clear pane)
  (let ((keys (frame-keys frame)))
    (format pane "~&  #   Key Name~%"
            "  --- ---------~%"
            (loop for key in keys
                   for i from 0
                   do (format pane "~&~3d.  ~a~%" i key)))))

;; Inspector commands
(clim:define-command (com-keys-close :menu t :name t) ()
  (let ((frame (and (boundp 'clim:*application-frame*) clim:*application-frame*)))
  (when frame (clim:frame-exit frame)))

;; Open keys inspector
(defun open-key-inspector (&optional (path "Source/Tables/Keys.txt"))
  "Open the Key Inspector."
  (let* ((full (merge-pathnames path (uiop:getcwd)))
         (keys (load-keys full))
         (resource (make-instance 'game-resource-from-file
                                  :moniker "Keys Index"
                                 
                                  :full-path (when (probe-file full) (truename full))))
         (fm (clim:find-frame-manager))
         (frame (clim:make-application-frame 'keys-inspector-frame
                 :resource resource
                 :path full
                 :keys keys
                 :frame-manager fm)))
    (clim:run-frame-top-level frame)))

;; New blank key command
(clim:define-command (com-new-key-blank :command-table clim-internals::global-command-table
                                        :menu nil :name t)
    ()
  "Open key inspector for creating a new key."
  (open-key-inspector))