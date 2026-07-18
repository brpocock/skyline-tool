;;; Skyline-Tool src/gui/gui-script-inspector.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-command-table inspector-script-build-menu
  :menu (("Demo" :command com-build-script-demo)
         ("Public" :command com-build-script-public)
         ("$(Publisher)" :command com-build-script-publisher)))

(clim:define-command-table inspector-script-region-menu
  :menu (("NTSC" :command com-region-ntsc-script)
         ("PAL" :command com-region-pal-script)
         ("SECAM" :command com-region-secam-script)))

(clim:define-command-table inspector-script-run-menu
  :menu (("Build" :menu inspector-script-build-menu)
         ("Region" :menu inspector-script-region-menu)
         (nil :divider :line)
         ("Make PDF..." :command com-make-script-pdf)
         ("Compile Script..." :command com-compile-script)))

(clim:define-command-table inspector-script-file-menu
  :menu (("New Script..." :command com-new-script-from-menu)
         ("Script from File..." :command com-open-script)
         (nil :divider :line)
         ("Save" :command com-save-resource)
         ("Version" :menu inspector-vc-menu)
         ("Save as" :menu inspector-save-as-menu)
         (nil :divider :line)
         ("Send to" :menu inspector-send-to-menu)
         ("Print to" :menu inspector-print-to-menu)
         (nil :divider :line)
         ("Close" :command com-close-frame)))

(clim:define-command-table script-inspector-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table inspector-script-menu-bar
  :menu (("Script" :menu inspector-script-file-menu)
         ("Edit" :menu script-inspector-edit-menu)
         ("View" :menu inspector-view-menu)
         ("Run" :menu inspector-script-run-menu)
         ("Help" :menu inspector-help-menu)))

(clim:define-application-frame game-resource-script-inspector (gui-inspector-frame clim:standard-application-frame)
  ((resource :initarg :resource :reader frame-resource))
  (:menu-bar inspector-script-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "Script Inspector"))

(defmethod initialize-instance :after ((frame game-resource-script-inspector) &key)
  (call-next-method)
  (subscribe :region-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

(defun open-script-inspector (resource)
  (open-resource-inspector (or resource
                                (make-instance 'game-resource-script
                                  :kind "Script"
                                  :moniker "Scripts/new-script.sky")) :editing))

(defmethod open-resource-inspector ((resource game-resource-script) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'game-resource-script-inspector
                                :resource resource
                                :pretty-name (format nil "~a — ~a ~a"
                                                     (game-resource-title resource)
                                                     *game-title* (machine-directory-name))
                                :view-mode mode)))

(clim:define-command (com-make-script-pdf :command-table clim-internals::global-command-table :menu t :name t)
    ((resource 'game-resource-script :gesture :select))
  (let* ((txt (game-resource-full-path resource))
         (pdf (make-output-path resource :pdf)))
    (uiop:run-program
     (list "ptyxis" "-s" "-x" "pdflatex" "-interaction=nonstopmode"
           "-output-directory" (pathname-directory pdf)
           (pathname-name txt))
     :output nil
     :ignore-error-status t)))

(clim:define-command (com-compile-script :command-table clim-internals::global-command-table :menu t :name t)
    ((resource 'game-resource-script :gesture :select))
  (clim-simple-echo:run-in-simple-echo
   (format nil "compile-script ~a" (game-resource-title resource))))

(clim:define-command (com-new-script-from-menu :command-table clim-internals::global-command-table :menu t :name t) ()
  (error "New script not implemented."))

(clim:define-command (com-open-script :command-table clim-internals::global-command-table :menu t :name t) ()
  (error "Open script not implemented."))

(clim:define-command (com-build-script-demo :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building script (Demo)...~%"))))

(clim:define-command (com-build-script-public :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building script (Public)...~%"))))

(clim:define-command (com-build-script-publisher :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building script (Publisher)...~%"))))

(clim:define-command (com-region-ntsc-script :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :ntsc)
  (publish :region-changed :payload :ntsc))

(clim:define-command (com-region-pal-script :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :pal)
  (publish :region-changed :payload :pal))

(clim:define-command (com-region-secam-script :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :secam)
  (publish :region-changed :payload :secam))
