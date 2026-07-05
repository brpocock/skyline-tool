(in-package :skyline-tool)

(defvar +valid-minifont-chars+ "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.:-'()!? ")

(defun sanitize-name (name)
  "Filter NAME to only include valid minifont characters, upcasing."
  (remove-if-not (lambda (c) (find (char-upcase c) +valid-minifont-chars+))
                 (string-upcase (or name ""))))

(defun load-name-list (path)
  "Load a list of names from PATH, one per line."
  (let ((full (merge-pathnames path (uiop:getcwd))))
    (when (probe-file full)
      (with-open-file (f full)
        (loop for line = (read-line f nil nil)
              while line
              collect (string-trim '(#\Space #\Tab #\Newline) line))))))

(defun save-name-list (path names)
  "Write NAMES to PATH, one per line."
  (let ((full (merge-pathnames path (uiop:getcwd))))
    (ensure-directories-exist full)
    (with-open-file (f full :direction :output :if-exists :supersede
                        :external-format :utf-8)
      (dolist (name names)
        (format f "~a~%" name)))))

(clim:define-application-frame item-list-inspector-frame (resource-inspector-mixin)
  ((path :initarg :path :accessor frame-path)
   (names :initarg :names :accessor frame-names)
   (gadgets :initform nil :accessor frame-gadgets))
  (:menu-bar item-list-inspector-menu-bar)
  (:panes
   (editor-pane :application :display-function 'display-resource-inspector
                :height 600 :width 450
                :scroll-bars :vertical)
   (interactor :interactor :height 80 :width 450))
  (:layouts
   (default (clim:vertically () editor-pane interactor))))

(clim:define-command-table item-list-inspector-file-menu
  :menu (("Close" :command com-close-editor)))

(clim:define-command-table item-list-inspector-edit-menu
  :menu (("Edit Name..." :command com-item-list-inspector-click)))

(clim:define-command-table item-list-inspector-help-menu
  :menu (("How to Edit Lists" :command com-help-for-window)
         ("Skyline-Tool Developers' Guide" :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide" :command com-open-scripting-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table item-list-inspector-menu-bar
  :menu (("File" :menu item-list-inspector-file-menu)
         ("Edit" :menu item-list-inspector-edit-menu)
         ("Help" :menu item-list-inspector-help-menu)))

(defmethod display-inspector-content ((frame item-list-inspector-frame) pane)
  (display-editor frame pane))

(defun display-editor (frame pane)
  (clim:window-clear pane)
  (format pane "~&  #   Name~%")
  (format pane "  --- ------~%")
  (loop for name in (frame-names frame)
        for i from 0
        do (format pane "~&~3d.  [~a]~%" i name)))

(clim:define-command (com-item-list-inspector-click :command-table clim-internals::global-command-table
                                            :menu nil :name t)
    ((index 'integer :gesture :select))
  (let* ((frame (and (boundp 'clim:*application-frame*) clim:*application-frame*))
         (names (and frame (frame-names frame)))
         (path (and frame (frame-path frame))))
    (when (and frame names path (<= 0 index (1- (length names))))
      (let* ((old (elt names index))
             (new (clim:accept 'string :prompt (format nil "Edit #~d" index) :default old)))
        (when (and new (plusp (length (string-trim " " new))))
          (setf (elt (frame-names frame) index) (sanitize-name new))
          (save-name-list path (frame-names frame))
          (clim:redisplay-frame-panes frame))))))

(clim:define-command (com-close-editor :menu t :name t) ()
  (let ((frame (and (boundp 'clim:*application-frame*) clim:*application-frame*)))
    (when frame (clim:frame-exit frame))))

(defun open-items-index (&optional (path "../Source/Tables/Items.txt"))
  "Open the Items List Inspector."
  (let* ((full (merge-pathnames path (uiop:getcwd)))
         (names (load-name-list full))
         (resource (make-instance 'game-resource-from-file
                                  :moniker "Items Index"
                                  :kind "Items"
                                  :full-path (truename full)))
         (fm (clim:find-frame-manager :port (or (clim:find-port) (clim:find-port :server-path :x))))
         (frame (clim:make-application-frame 'item-list-inspector-frame
                 :resource resource
                 :path full :names names
                 :frame-manager fm)))
    (clim:run-frame-top-level frame)))
