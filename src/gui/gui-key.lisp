;;; Skyline-Tool src/gui/gui-key.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-key-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-key-reference)))
  (typep object 'game-resource-key))

(clim:define-presentation-type game-resource-key-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-key-editable)))
  (typep object 'game-resource-key))

(clim:define-presentation-type game-resource-key-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-key-viewing)))
  (typep object 'game-resource-key))

(defmethod present-reference ((resource game-resource-key) stream)
  (clim:with-output-as-presentation (stream resource 'game-resource-key-reference)
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

(defmethod present-reading ((resource game-resource-key) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-title resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Key ID: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~d" (game-resource-key-id resource))))))

(defmethod present-editing ((resource game-resource-key) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
        (interactive-editing-gadget-with-validation
         stream resource
         (lambda (r) (game-resource-title r))
         (lambda (r v) (setf (game-resource-title r) v))
         :label "Name:"
         :validator #'validate-minifont-name
         :max-length 20)))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Key ID: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~d (read-only)" (game-resource-key-id resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Kind: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a (read-only)" (game-resource-kind resource))))))

(clim:define-command-table key-file-menu
  :menu (("New..." :command com-new-key)
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

(clim:define-command-table key-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table key-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :toggle t)
         (nil :divider :line)
         ("Project Pane" :command com-toggle-project-pane :toggle t)))

(clim:define-command-table key-build-menu
  :menu (("Demo" :command com-build-key-demo)
         ("Public" :command com-build-key-public)
         ("$(Publisher)" :command com-build-key-publisher)))

(clim:define-command-table key-region-menu
  :menu (("NTSC" :command com-region-ntsc-key)
         ("PAL" :command com-region-pal-key)
         ("SECAM" :command com-region-secam-key)))

(clim:define-command-table key-run-menu
  :menu (("Build" :menu key-build-menu)
         ("Region" :menu key-region-menu)))

(clim:define-command-table key-help-menu
  :menu (("How to Manage Keys..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table key-menu-bar
  :menu (("Key" :menu key-file-menu)
         ("Edit" :menu key-edit-menu)
         ("View" :menu key-view-menu)
         ("Run" :menu key-run-menu)
         ("Help" :menu key-help-menu)))

(clim:define-application-frame game-resource-key-inspector (gui-inspector-frame clim:standard-application-frame)
  ((resource :initarg :resource :reader frame-resource))
  (:menu-bar key-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "Key Inspector"))

(defmethod initialize-instance :after ((frame game-resource-key-inspector) &key)
  (call-next-method)
  (subscribe :resource-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

(defun open-key-inspector (resource)
  (open-resource-inspector (or resource
                                (make-instance 'game-resource-key
                                  :key-id 0
                                  :name "New Key")) :editing))

(defmethod open-resource-inspector ((resource game-resource-key) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'game-resource-key-inspector
                                :resource resource
                                :pretty-name (format nil "~a — ~a ~a"
                                                     (game-resource-title resource)
                                                     *game-title* (machine-directory-name))
                                :view-mode mode)))

(clim:define-command (com-new-key :command-table clim-internals::global-command-table :menu t :name t) ()
  (open-key-inspector nil))

(clim:define-command (com-build-key-demo :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building key (Demo)...~%"))))

(clim:define-command (com-build-key-public :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building key (Public)...~%"))))

(clim:define-command (com-build-key-publisher :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building key (Publisher)...~%"))))

(clim:define-command (com-region-ntsc-key :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :ntsc)
  (publish :region-changed :payload :ntsc))

(clim:define-command (com-region-pal-key :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :pal)
  (publish :region-changed :payload :pal))

(clim:define-command (com-region-secam-key :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :secam)
  (publish :region-changed :payload :secam))

(defmethod game-resource-action-menu ((resource game-resource-key))
  (list
   (make-menu-item "Inspect..." (lambda () (open-key-inspector resource)))))
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