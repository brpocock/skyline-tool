(in-package :skyline-tool)

(defvar +boat-classes+ '("catamaran" "sloop" "rowboat" "galleon" "longship" "raft" "canoe" "skiff")
  "Known boat classes for the enum dropdown.")

(defun load-boats (&optional (path "Source/Tables/Boats.ods"))
  "Load boat data from an ODS spreadsheet. Returns a list of (name class notes)."
  (let* ((full (merge-pathnames path (uiop:getcwd)))
         (pages (ignore-errors (read-ods-into-lists full)))
         (rows (and pages (ss->lol (first pages)))))
    (loop for row in rows
          collect (list (getf row :name)
                        (getf row :class)
                        (getf row :notes)))))

(defun save-boats (boats &optional (path "../Source/Tables/Boats.ods"))
  "Save boat data back to an ODS file. Not yet implemented - writes JSON for now."
  (let ((json-path (merge-pathnames
                    (make-pathname :type "json" :defaults path)
                    (uiop:getcwd))))
    (with-open-file (f json-path :direction :output :if-exists :supersede
                        :external-format :utf-8)
      (let ((*print-pretty* t))
        (format f "~s" (loop for b in boats
                             collect (list :name (first b) :class (second b)
                                           :notes (third b))))))
    (format *query-io* "~&Saved ~a~%" (namestring json-path))))

(clim:define-application-frame boat-inspector-frame ()
  ((boats :initarg :boats :accessor frame-boats)
   (path :initarg :path :accessor frame-path))
  (:menu-bar boat-inspector-menu-bar)
  (:panes
   (editor-pane :application :display-function 'display-boats
                :height 600 :width 600 :scroll-bars :vertical)
   (interactor :interactor :height 80 :width 600))
  (:layouts
   (default (clim:vertically () editor-pane interactor))))

(clim:define-command-table boat-inspector-file-menu
  :menu (("Save" :command com-boats-save)
         (nil :divider :line)
         ("Close" :command com-boats-close)))

(clim:define-command-table boat-inspector-edit-menu
  :menu (("Edit Boat..." :command com-boats-edit)))

(clim:define-command-table boat-inspector-help-menu
  :menu (("How to Edit Boats" :command com-help-for-window)
         ("Skyline-Tool Developers' Guide" :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide" :command com-open-scripting-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table boat-inspector-menu-bar
  :menu (("File" :menu boat-inspector-file-menu)
         ("Edit" :menu boat-inspector-edit-menu)
         ("Help" :menu boat-inspector-help-menu)))

(defun display-boats (frame pane)
  (clim:window-clear pane)
  (format pane "~&  #  ~-30a  ~-20a  ~a~%" "Name" "Class" "Notes")
  (format pane "  --  ~-30a  ~-20a  ~a~%" "----" "-----" "-----")
  (loop for boat in (frame-boats frame)
        for i from 0
        do (destructuring-bind (name class notes) boat
             (format pane "~&~3d  ~-30a  ~-20a  ~a~%" i name class notes))))

(defun run-boat-inspector (&optional (path "../Source/Tables/Boats.ods"))
  "Open the boat editor window."
  (let* ((boats (or (load-boats path)
                    (list (list "Galileo" "rowboat" "Sample boat"))))
         (fm (clim:find-frame-manager :port (or (clim:find-port) (clim:find-port :server-path :x))))
         (frame (clim:make-application-frame
                 'boat-inspector-frame
                 :pretty-name "Boat Editor"
                 :boats boats :path path
                 :frame-manager fm)))
    (clim:run-frame-top-level frame)))

(clim:define-command (com-boats-edit :command-table clim-internals::global-command-table
                                      :menu nil :name t)
    ((index 'integer :gesture :select))
  (let* ((frame (and (boundp '*application-frame*) *application-frame*))
         (boats (and frame (frame-boats frame))))
    (when (and frame boats (<= 0 index (1- (length boats))))
      (let* ((old (elt boats index))
             (new-name (clim:accept 'string :prompt "Boat name" :default (first old)))
             (new-class (clim:accept '(member "catamaran" "sloop" "rowboat" "galleon"
                                              "longship" "raft" "canoe" "skiff")
                                     :prompt "Class" :default (second old)))
             (new-notes (clim:accept 'string :prompt "Notes" :default (third old))))
        (setf (elt boats index) (list new-name new-class new-notes))
        (setf (frame-boats frame) boats)
        (clim:redisplay-frame-panes frame)))))

(clim:define-command (com-boats-save :menu t :name t) ()
  (let* ((frame (and (boundp '*application-frame*) *application-frame*))
         (boats (and frame (frame-boats frame)))
         (path (and frame (frame-path frame))))
    (when (and boats path)
      (save-boats boats path))))

(clim:define-command (com-boats-close :menu t :name t) ()
  (let ((frame (and (boundp '*application-frame*) *application-frame*)))
    (when frame (clim:frame-exit frame))))

(defun open-boat-inspector ()
  "Launcher entry point for the Boat Editor."
  (run-boat-inspector))

