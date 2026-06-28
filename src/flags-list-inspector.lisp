(in-package :skyline-tool)

(defun load-flags-list (&optional (path "../Source/Tables/Flags.txt"))
  "Load flag names from PATH, one per line."
  (let ((full (merge-pathnames path (uiop:getcwd))))
    (when (probe-file full)
      (with-open-file (i full)
        (loop for line = (read-line i nil nil)
              while line
              collect (string-trim '(#\Space #\Tab) line))))))

(defun save-flags-list (names &optional (path "../Source/Tables/Flags.txt"))
  "Write NAMES to PATH, one per line."
  (let ((full (merge-pathnames path (uiop:getcwd))))
    (ensure-directories-exist full)
    (with-open-file (f full :direction :output :if-exists :supersede
                        :external-format :utf-8)
      (dolist (name names) (format f "~a~%" name)))))

(clim:define-application-frame flags-inspector-frame ()
  ((path :initarg :path :accessor frame-path)
   (names :initarg :names :accessor frame-names))
  (:menu-bar flags-inspector-menu-bar)
  (:panes
   (editor-pane :application :display-function 'display-flags
                :height 600 :width 450 :scroll-bars :vertical)
   (interactor :interactor :height 80 :width 450))
  (:layouts (default (clim:vertically () editor-pane interactor))))

(clim:define-command-table flags-inspector-help-menu
  :menu (("How to Edit Flags" :command com-help-for-window)
         ("Skyline-Tool Developers' Guide" :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide" :command com-open-scripting-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table flags-inspector-menu-bar
  :menu (("File" :menu (("Close" :command com-flags-close)))
         ("Edit" :menu (("Edit Flag..." :command com-flags-edit)))
         ("Help" :menu flags-inspector-help-menu)))

(defun display-flags (frame pane)
  (clim:window-clear pane)
  (format pane "~&  #  Flag Name~%")
  (format pane "  -- ---------~%")
  (loop for name in (frame-names frame) for i from 0
        do (format pane "~&~3d  ~a~%" i name)))

(clim:define-command (com-flags-edit :command-table clim-internals::global-command-table
                                      :menu nil :name t)
    ((index 'integer :gesture :select))
  (let* ((frame (and (boundp '*application-frame*) *application-frame*))
         (names (and frame (frame-names frame)))
         (path (and frame (frame-path frame))))
    (when (and names path (<= 0 index (1- (length names))))
      (let* ((old (elt names index))
             (new (clim:accept 'string :prompt (format nil "Flag #~d" index) :default old)))
        (when (and new (plusp (length (string-trim " " new))))
          (setf (elt (frame-names frame) index) new)
          (save-flags-list (frame-names frame) path)
          (clim:redisplay-frame-panes frame))))))

(clim:define-command (com-flags-close :menu t :name t) ()
  (let ((frame (and (boundp '*application-frame*) *application-frame*)))
    (when frame (clim:frame-exit frame))))