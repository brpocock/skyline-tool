;;; Skyline-Tool src/gui/gui-blob-inspector.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-blob-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-blob-reference)))
  (typep object 'game-resource-blob))

(clim:define-presentation-type game-resource-blob-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-blob-editable)))
  (typep object 'game-resource-blob))

(clim:define-presentation-type game-resource-blob-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-blob-viewing)))
  (typep object 'game-resource-blob))

(defmethod present-reference ((resource game-resource-blob) stream)
  (clim:with-output-as-presentation
      (stream resource 'game-resource-blob-reference)
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

(defmethod present-reading ((resource game-resource-blob) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Moniker: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (or (game-asset-moniker resource) "(none)"))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Asset ID: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (or (game-resource-asset-id resource) "—"))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Builds: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (or (game-resource-builds resource) "—"))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Kind: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-kind resource))))))

(defmethod present-editing ((resource game-resource-blob) stream)
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
        (format stream "Moniker: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a (read-only)" (or (game-asset-moniker resource) "(none)"))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Asset ID: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a (read-only)" (or (game-resource-asset-id resource) "—"))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Builds: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a (read-only)" (or (game-resource-builds resource) "—"))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Kind: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a (read-only)" (game-resource-kind resource))))))

(clim:define-application-frame game-resource-blob-inspector (gui-inspector-frame clim:standard-application-frame)
  ()
  (:menu-bar inspector-blob-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "Blob Inspector"))

(defmethod initialize-instance :after ((frame game-resource-blob-inspector) &key)
  (call-next-method)
  (subscribe :region-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

(defun open-blob-inspector (resource)
  (open-resource-inspector (or resource (make-instance 'game-resource-blob
                                                         :full-path nil
                                                         :moniker "Blobs/new-blob.xcf")) :editing))

(defmethod open-resource-inspector ((resource game-resource-blob) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'game-resource-blob-inspector
                                :resource resource
                                :pretty-name (format nil "~a — ~a ~a"
                                                     (game-resource-title resource)
                                                     *game-title* (machine-directory-name))
                                :view-mode mode)))

(clim:define-command-table inspector-blob-file-menu
  :menu (("New..." :command com-new-blob)
         ("Import..." :command com-open-blob)
         (nil :divider :line)
         ("Save" :command com-save-resource)
         ("Version" :menu inspector-vc-menu)
         ("Save as" :menu (("JSON..." :command com-save-as-json)
                            ("Text..." :command com-save-as-text)
                            ("PDF..." :command com-save-as-pdf)
                            (nil :divider :line)
                            ("PNG..." :command com-save-as-png)))
         (nil :divider :line)
         ("Send to" :menu inspector-send-to-menu)
         ("Print to" :menu inspector-print-to-menu)
         (nil :divider :line)
         ("Close" :command com-close-frame)))

(clim:define-command-table inspector-blob-build-menu
  :menu (("Demo" :command com-build-blob-demo)
         ("Public" :command com-build-blob-public)
         ("$(Publisher)" :command com-build-blob-publisher)))

(clim:define-command-table inspector-blob-region-menu
  :menu (("NTSC" :command com-region-ntsc-blob)
         ("PAL" :command com-region-pal-blob)
         ("SECAM" :command com-region-secam-blob)))

(clim:define-command-table inspector-blob-run-menu
  :menu (("Build" :menu inspector-blob-build-menu)
         ("Region" :menu inspector-blob-region-menu)
         (nil :divider :line)
         ("Make PNG..." :command com-make-blob-png)
         ("Compile Blob..." :command com-compile-blob)))

(clim:define-command-table inspector-blob-help-menu
  :menu (("How to Manage BLOBs..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table inspector-blob-menu-bar
  :menu (("BLOB" :menu inspector-blob-file-menu)
         ("Edit" :menu inspector-edit-menu)
         ("View" :menu inspector-view-menu)
         ("Run" :menu inspector-blob-run-menu)
         ("Help" :menu inspector-blob-help-menu)))

(clim:define-command (com-make-blob-png :command-table clim-internals::global-command-table :menu t :name t)
    ((resource 'game-resource-blob :gesture :select :default (inspector-resource clim:*application-frame*)))
  (let* ((xcf-path (first (game-resource-pathnames resource)))
         (png-path (make-output-path resource :png)))
    (uiop:run-program
     (list "ptyxis" "-s" "-x" "gimp" "-i" "-b"
           (format nil "(gimp-file-save RUN-NONINTERACTIVE ~s ~s) (gimp-quit 0)" xcf-path png-path)
           xcf-path png-path)
     :output nil
     :ignore-error-status t)))

(clim:define-command (com-compile-blob :command-table clim-internals::global-command-table :menu t :name t)
    ((resource 'game-resource-blob :gesture :select :default (inspector-resource clim:*application-frame*)))
  (let* ((png-path (make-output-path resource :png))
         (png-pathname (if (pathnamep png-path) png-path (pathname png-path))))
    (unless (probe-file png-pathname)
      (error "PNG file not found: ~a. Please export PNG first via 'Make PNG...'." png-pathname))
    (clim-simple-echo:run-in-simple-echo
     (lambda ()
       (format t "blob-rip-7800 ~a~%" (namestring png-pathname))))))

(clim:define-command (com-new-blob :command-table clim-internals::global-command-table :menu t :name t) ()
  (open-blob-inspector nil))

(clim:define-command (com-open-blob :command-table clim-internals::global-command-table :menu t :name t) ()
  (error "Open blob not implemented."))

(clim:define-command (com-save-as-png :command-table clim-internals::global-command-table :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (resource (inspector-resource frame)))
    (when resource
      (let ((path (prompt-save-pathname (format nil "~a.png" (game-resource-title resource)))))
        (when path
          (com-make-blob-png resource)
          (clim-simple-echo:run-in-simple-echo (lambda () (format t "Exported PNG to ~a" path))))))))

(clim:define-command (com-build-blob-demo :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building blob (Demo)...~%"))))

(clim:define-command (com-build-blob-public :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building blob (Public)...~%"))))

(clim:define-command (com-build-blob-publisher :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building blob (Publisher)...~%"))))

(clim:define-command (com-region-ntsc-blob :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :ntsc)
  (publish :region-changed :payload :ntsc))

(clim:define-command (com-region-pal-blob :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :pal)
  (publish :region-changed :payload :pal))

(clim:define-command (com-region-secam-blob :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :secam)
  (publish :region-changed :payload :secam))
