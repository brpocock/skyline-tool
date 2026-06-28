(in-package :skyline-tool)

(defun load-prototype-file (path)
  "Load a JSON prototype file, return the decoded alist."
  (when (probe-file path)
    (let ((*package* (find-package :keyword)))
      (cl-json:decode-json-from-string
       (alexandria:read-file-into-string path)))))

(defun save-prototype-file (path data)
  "Save DATA (alist) as pretty-printed JSON to PATH."
  (with-open-file (f path :direction :output :if-exists :supersede
                      :external-format :utf-8)
    (write-json-pretty data f)))

(clim:define-application-frame prototype-inspector-frame ()
  ((path :initarg :path :accessor frame-path)
   (data :initarg :data :accessor frame-data))
  (:menu-bar prototype-inspector-menu-bar)
  (:panes
   (inspector-pane :application :display-function 'display-prototype
                   :height 600 :width 500 :scroll-bars :vertical)
   (interactor :interactor :height 80 :width 500))
  (:layouts (default (clim:vertically () inspector-pane interactor))))

(clim:define-command-table prototype-inspector-help-menu
  :menu (("How to Edit Prototypes" :command com-help-for-window)
         ("Skyline-Tool Developers' Guide" :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide" :command com-open-scripting-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table prototype-inspector-menu-bar
  :menu (("File" :menu (("Save" :command com-proto-save)
                         (nil :divider :line)
                         ("Close" :command com-proto-close)))
         ("Edit" :menu (("Edit Field..." :command com-proto-edit)))
         ("Help" :menu prototype-inspector-help-menu)))

(defun display-prototype (frame pane)
  (clim:window-clear pane)
  (let ((data (frame-data frame)))
    (format pane "~&Prototype Fields:~2%")
    (loop for (key . value) in data
          for i from 0
          do (format pane "~&~3d. ~a = ~a~%" i
                     (cl-change-case:title-case (string key))
                     (if (stringp value) value
                         (with-output-to-string (s)
                           (write-json-pretty (list (cons key value)) s)))))))

(clim:define-command (com-proto-edit :command-table clim-internals::global-command-table
                                      :menu nil :name t)
    ((index 'integer :gesture :select))
  (let* ((frame (and (boundp '*application-frame*) *application-frame*))
         (data (and frame (frame-data frame)))
         (path (and frame (frame-path frame))))
    (when (and data path (<= 0 index (1- (length data))))
      (let* ((pair (nth index data))
             (key (car pair))
             (old (cdr pair))
             (str-old (if (stringp old) old (princ-to-string old)))
             (new (clim:accept 'string :prompt (format nil "~a" key) :default str-old)))
        (when (and new (plusp (length (string-trim " " new))))
          (let ((parsed (ignore-errors (cl-json:decode-json-from-string new))))
            (setf (cdr (nth index (frame-data frame)))
                  (if parsed parsed new))
            (save-prototype-file path (frame-data frame))
            (clim:redisplay-frame-panes frame)))))))

(clim:define-command (com-proto-save :menu t :name t) ()
  (let* ((frame (and (boundp '*application-frame*) *application-frame*))
         (data (and frame (frame-data frame)))
         (path (and frame (frame-path frame))))
    (when (and data path)
      (save-prototype-file path data)
      (format *query-io* "~&Saved.~%"))))

(clim:define-command (com-proto-close :menu t :name t) ()
  (let ((frame (and (boundp '*application-frame*) *application-frame*)))
    (when frame (clim:frame-exit frame))))

(defun open-prototype-inspector (path)
  "Open a prototype JSON file in the inspector."
  (let* ((data (load-prototype-file path))
         (fm (clim:find-frame-manager :port (or (clim:find-port) (clim:find-port :server-path :x))))
         (frame (clim:make-application-frame
                 'prototype-inspector-frame
                 :pretty-name (format nil "Prototype: ~a" (pathname-name path))
                 :path path :data data :frame-manager fm)))
    (clim:run-frame-top-level frame)))
