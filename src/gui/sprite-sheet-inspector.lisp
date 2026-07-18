;;; Skyline-Tool src/gui/sprite-sheet-inspector.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-application-frame sprite-sheet-inspector-frame (gui-inspector-frame clim:standard-application-frame)
  ((frame-resource :accessor frame-resource :initform nil))
  (:menu-bar sprite-sheet-menu-bar)
  (:icon (skyline-tool-icon :resource :sprite-sheet))
  (:pretty-name "Sprite Sheet Inspector"))

(defmethod initialize-instance :after ((frame sprite-sheet-inspector-frame) &key)
  (call-next-method)
  (subscribe :resource-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

(clim:define-command-table sprite-sheet-file-menu
  :menu (("New..." :command com-new-sprite-sheet)
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

(clim:define-command-table sprite-sheet-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table sprite-sheet-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :toggle t)
         (nil :divider :line)
         ("Project Pane" :command com-toggle-project-pane :toggle t)))

(clim:define-command-table sprite-sheet-build-menu
  :menu (("Demo" :command com-build-sprite-sheet-demo)
         ("Public" :command com-build-sprite-sheet-public)
         ("$(Publisher)" :command com-build-sprite-sheet-publisher)))

(clim:define-command-table sprite-sheet-region-menu
  :menu (("NTSC" :command com-region-ntsc-sprite-sheet)
         ("PAL" :command com-region-pal-sprite-sheet)
         ("SECAM" :command com-region-secam-sprite-sheet)))

(clim:define-command-table sprite-sheet-run-menu
  :menu (("Build" :menu sprite-sheet-build-menu)
         ("Region" :menu sprite-sheet-region-menu)
         (nil :divider :line)
         ("Export PNG..." :command com-sprite-sheet-export-png)
         ("Open in Gimp..." :command com-open-in-gimp)))

(clim:define-command-table sprite-sheet-help-menu
  :menu (("How to Manage Sprite Sheets..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table sprite-sheet-menu-bar
  :menu (("Sprite" :menu sprite-sheet-file-menu)
         ("Edit" :menu sprite-sheet-edit-menu)
         ("View" :menu sprite-sheet-view-menu)
         ("Run" :menu sprite-sheet-run-menu)
         ("Help" :menu sprite-sheet-help-menu)))

(clim:define-command (com-new-sprite-sheet :command-table clim-internals::global-command-table :menu t :name t) ()
  (open-sprite-sheet-inspector nil))

(clim:define-command (com-sprite-sheet-export-png :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Exporting sprite sheet to PNG...~%"))))

(clim:define-command (com-build-sprite-sheet-demo :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building sprite sheet (Demo)...~%"))))

(clim:define-command (com-build-sprite-sheet-public :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building sprite sheet (Public)...~%"))))

(clim:define-command (com-build-sprite-sheet-publisher :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building sprite sheet (Publisher)...~%"))))

(clim:define-command (com-region-ntsc-sprite-sheet :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :ntsc)
  (publish :region-changed :payload :ntsc))

(clim:define-command (com-region-pal-sprite-sheet :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :pal)
  (publish :region-changed :payload :pal))

(clim:define-command (com-region-secam-sprite-sheet :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :secam)
  (publish :region-changed :payload :secam))
