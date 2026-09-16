;;; Skyline-Tool src/gui/gui-flag.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-flag-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-flag-reference)))
  (typep object 'game-resource-flag))

(clim:define-presentation-type game-resource-flag-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-flag-editable)))
  (typep object 'game-resource-flag))

(clim:define-presentation-type game-resource-flag-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-flag-viewing)))
  (typep object 'game-resource-flag))

(defmethod present-reference ((resource game-resource-flag) stream)
  (clim:with-output-as-presentation (stream resource 'game-resource-flag-reference)
    (clim:formatting-table (stream)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 0)
          (format stream "~3%"))
        (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 125)
          (game-resource-present-icon resource stream))
        (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 150)
          (clim:with-text-size (stream :larger)
            (clim:with-text-face (stream :bold)
              (game-resource-present-title resource stream)))
          (format stream "~%~5t")
          (clim:with-text-size (stream :smaller)
            (clim:with-drawing-options (stream :ink (clim:make-gray-color 0.75))
              (game-resource-present-subheading resource stream))))
        (clim:formatting-cell (stream :align-x :right :align-y :top :min-height 90 :min-width 125)
          (game-resource-present-right-margin resource stream))))))

(defmethod present-reading ((resource game-resource-flag) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Index: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "$~2,'0x (~d)" (game-resource-index resource) (game-resource-index resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-name resource))))))

(defmethod present-editing ((resource game-resource-flag) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Index: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "$~2,'0x (~d)" (game-resource-index resource) (game-resource-index resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
        (insert-gadget stream :label nil
                       :variable (game-resource-name resource)
                       :activation-callback
                       (lambda (gadget)
                         (setf (game-resource-name resource)
                               (clim:gadget-value gadget))))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Kind: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a (read-only)" (game-resource-kind resource))))))

(clim:define-command-table flag-file-menu
  :menu (("New..." :command com-new-flag)
         ("Import..." :command com-import-resource)
         (nil :divider :line)
         ("Save" :command com-save-resource)
         ("Version" :menu inspector-vc-menu)
         ("Save as" :menu inspector-save-as-menu)
         (nil :divider :line)
         ("Send to" :menu inspector-send-to-menu)
         ("Print to" :menu inspector-print-to-menu)
         (nil :divider :line)
         ("Close" :command com-close-frame)))

(clim:define-command-table flag-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table flag-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :toggle t)
         (nil :divider :line)
         ("Project Pane" :command com-toggle-project-pane :toggle t)))

(clim:define-command-table flag-build-menu
  :menu (("Demo" :command com-build-flag-demo)
         ("Public" :command com-build-flag-public)
         ("$(Publisher)" :command com-build-flag-publisher)))

(clim:define-command-table flag-region-menu
  :menu (("NTSC" :command com-region-ntsc-flag)
         ("PAL" :command com-region-pal-flag)
         ("SECAM" :command com-region-secam-flag)))

(clim:define-command-table flag-run-menu
  :menu (("Build" :menu flag-build-menu)
         ("Region" :menu flag-region-menu)
         (nil :divider :line)
         ("Compile Flags..." :command com-compile-flags)))

(clim:define-command-table flag-help-menu
  :menu (("How to Manage Flags..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table flag-menu-bar
  :menu (("Flag" :menu flag-file-menu)
         ("Edit" :menu flag-edit-menu)
         ("View" :menu flag-view-menu)
         ("Run" :menu flag-run-menu)
         ("Help" :menu flag-help-menu)))

(clim:define-application-frame flag-inspector-frame (gui-inspector-frame clim:standard-application-frame)
  ((frame-resource :initarg :resource :reader frame-resource :initform nil))
  (:menu-bar flag-menu-bar)
  (:panes
   (content-pane :application :display-function 'display-inspector-content
                  :scroll-bars :vertical :height 600 :width 800)
   (status-pane :application :display-function 'display-inspector-status
                :scroll-bars nil :height 30 :width 800))
  (:layouts
   (default (clim:vertically () content-pane status-pane)))
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "Flag Inspector"))

(defmethod initialize-instance :after ((frame flag-inspector-frame) &key)
  (call-next-method)
  (subscribe :resource-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

(defun open-flag-inspector (resource)
  (open-resource-inspector (or resource
                                (make-instance 'game-resource-flag
                                  :flag-id 0
                                  :name "New Flag")) :editing))

(defmethod open-resource-inspector ((resource game-resource-flag) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'flag-inspector-frame
                                :resource resource
                                :pretty-name (format nil "~a — ~a ~a"
                                                     (game-resource-title resource)
                                                     *game-title* (machine-directory-name))
                                :view-mode mode)))

(clim:define-command (com-new-flag :command-table clim-internals::global-command-table :menu t :name t) ()
  (open-flag-inspector (make-instance 'game-resource-flag)))

(clim:define-command (com-compile-flags :command-table flag-run-menu :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Compiling flags...~%"))))

(clim:define-command (com-build-flag-demo :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building flag (Demo)...~%"))))

(clim:define-command (com-build-flag-public :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building flag (Public)...~%"))))

(clim:define-command (com-build-flag-publisher :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building flag (Publisher)...~%"))))

(clim:define-command (com-region-ntsc-flag :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :ntsc)
  (publish :region-changed :payload :ntsc))

(clim:define-command (com-region-pal-flag :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :pal)
  (publish :region-changed :payload :pal))

(clim:define-command (com-region-secam-flag :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :secam)
  (publish :region-changed :payload :secam))

(defmethod game-resource-action-menu ((resource game-resource-flag))
  (list
   (make-menu-item "Inspect..." (lambda () (open-flag-inspector resource)))))


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

(clim:define-application-frame flags-inspector-frame (resource-inspector-mixin clim:standard-application-frame)
  ((path :initarg :path :accessor frame-path)
   (names :initarg :names :accessor frame-names))
  (:menu-bar flags-inspector-menu-bar)
  (:panes
   (editor-pane :application :display-function 'display-resource-inspector
                             :height 600 :width 450 :scroll-bars :vertical)
   (interactor :interactor :height 80 :width 450))
  (:layouts (default (clim:vertically () editor-pane interactor))))

(clim:define-command-table flags-inspector-help-menu
  :menu (("How to Edit Flags..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-scripting-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table flags-inspector-menu-bar
  :menu (("File" :menu (("Close" :command com-flags-close)))
         ("Edit" :menu (("Edit Flag..." :command com-flags-edit)))
         ("Help" :menu flags-inspector-help-menu)))

(defmethod display-inspector-content ((frame flags-inspector-frame) pane)
  (display-flags frame pane))

(defun display-flags (frame pane)
  (clim:window-clear pane)
  (format pane "~&  #  Flag Name~%")
  (format pane "  -- ---------~%")
  (loop for name in (frame-names frame) for i from 0
        do (format pane "~&~3d  ~a~%" i name)))

(clim:define-command (com-flags-edit :command-table clim-internals::global-command-table
                                     :menu nil :name t)
    ((index 'integer :gesture :select))
  (let* ((frame (and (boundp 'clim:*application-frame*) clim:*application-frame*))
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
  (let ((frame (and (boundp 'clim:*application-frame*) clim:*application-frame*)))
    (when frame (clim:frame-exit frame))))

(defun open-flags-index (&optional (path "../Source/Tables/Flags.txt"))
  "Open the Flags List Inspector."
  (let* ((full (merge-pathnames path (uiop:getcwd)))
         (names (load-flags-list full))
         (resource (make-instance 'game-resource-from-file
                                  :moniker "Flags Index"
                                  
                                  :full-path (truename full)))
         (fm (clim:find-frame-manager :port (or (clim:find-port) (clim:find-port :server-path :x))))
         (frame (clim:make-application-frame 'flags-inspector-frame
                                             :resource resource
                                             :path full :names names
                                             :frame-manager fm)))
    (clim:run-frame-top-level frame)))
