;;; Skyline-Tool src/gui/gui-class.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-class-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-class-reference)))
  (typep object 'game-resource-class))

(clim:define-presentation-type game-resource-class-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-class-editable)))
  (typep object 'game-resource-class))

(clim:define-presentation-type game-resource-class-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-class-viewing)))
  (typep object 'game-resource-class))

(defmethod present-reference ((resource game-resource-class) stream)
  (clim:with-output-as-presentation (stream resource 'game-resource-class-reference)
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

(defmethod present-reading ((resource game-resource-class) stream)
  (let ((class-name (game-resource-title resource)))
    (clim:formatting-table (stream)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Class Name:"))
        (clim:formatting-cell (stream :align-x :left)
          (format stream "~a" class-name)))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Parent Class:"))
        (clim:formatting-cell (stream :align-x :left)
          (clim:with-output-as-presentation
              (stream (or (find-parent-class class-name) (make-instance 'game-resource))
                'game-resource-reference))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Child Classes:"))
        (clim:formatting-cell (stream :align-x :left)
          (present-child-classes class-name stream)))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Methods:"))
        (clim:formatting-cell (stream :align-x :left)
          (present-methods class-name stream))))))

(defmethod present-editing ((resource game-resource-class) stream)
  (let ((class-name (game-resource-title resource)))
    (clim:formatting-table (stream)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Class Name:"))
        (clim:formatting-cell (stream :align-x :left)
(interactive-editing-gadget-with-validation
            stream resource
            :getter (lambda (r) (game-resource-title r))
            :setter (lambda (r v) (setf (game-resource-title r) v))
            :label "Class Name:"
           :validator #'validate-asset-name
           :max-length 200)))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Parent Class:"))
        (clim:formatting-cell (stream :align-x :left)
          (insert-gadget stream :label nil
                         :variable (find-parent-class class-name)
                         :presentation-type 'game-resource-reference
                         :activation-callback
                         (lambda (gadget)
                           (set-parent-class class-name
                                             (game-resource-title (clim:gadget-value gadget)))))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Kind: "))
        (clim:formatting-cell (stream :align-x :left)
          (format stream "~a (read-only)" (game-resource-kind resource))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Methods:"))
        (clim:formatting-cell (stream :align-x :left)
          (present-method-edit-buttons class-name stream))))))

(defun find-parent-class (class-name)
  (when (find class-name *known-classes* :key #'class-name :test #'string-equal)
    (make-instance 'game-resource-class :full-path (format nil "Source/Classes/~a.cob" class-name))))

(defun present-child-classes (class-name stream)
  (let ((children (find-children class-name)))
    (when children
      (clim:formatting-table (stream)
        (dolist (child children)
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream :align-x :left)
              (clim:with-output-as-presentation
                  (stream (make-instance 'game-resource-class :full-path (format nil "Source/Classes/~a.cob" child))
                   'game-resource-reference)))))))))

(defun present-methods (class-name stream)
  (let ((methods (find-methods class-name)))
    (when methods
      (clim:formatting-table (stream)
        (dolist (method methods)
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream :align-x :left)
              (format stream "~a~%" method))))))))

(defun present-method-edit-buttons (class-name stream)
  (let ((methods (find-methods class-name)))
    (when methods
      (clim:formatting-table (stream)
        (dolist (method methods)
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream :align-x :left)
              (format stream "~a" method))
            (clim:formatting-cell (stream :align-x :right)
              (insert-button stream
                             :label "Edit in Emacs"
                             :activation-callback
                             (lambda ()
                               (uiop:run-program (list "emacs" (method-file-path class-name method))
                                                 :output nil))))))))))

(clim:define-command-table class-file-menu
  :menu (("New..." :command com-new-class)
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

(clim:define-command-table class-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table class-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :toggle t)
         (nil :divider :line)
         ("Project Pane" :command com-toggle-project-pane :toggle t)))

(clim:define-command-table class-build-menu
  :menu (("Demo" :command com-build-class-demo)
         ("Public" :command com-build-class-public)
         ("$(Publisher)" :command com-build-class-publisher)))

(clim:define-command-table class-region-menu
  :menu (("NTSC" :command com-region-ntsc-class)
         ("PAL" :command com-region-pal-class)
         ("SECAM" :command com-region-secam-class)))

(clim:define-command-table class-run-menu
  :menu (("Build" :menu class-build-menu)
         ("Region" :menu class-region-menu)
         (nil :divider :line)
         ("Compile Class..." :command com-compile-class)))

(clim:define-command-table class-help-menu
  :menu (("How to Manage Classes..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table class-menu-bar
  :menu (("Class" :menu class-file-menu)
         ("Edit" :menu class-edit-menu)
         ("View" :menu class-view-menu)
         ("Run" :menu class-run-menu)
         ("Help" :menu class-help-menu)))

(clim:define-application-frame game-resource-class-inspector (gui-inspector-frame clim:standard-application-frame)
  ((resource :initarg :resource :reader frame-resource))
  (:menu-bar class-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "Class Inspector"))

(defmethod initialize-instance :after ((frame game-resource-class-inspector) &key)
  (call-next-method)
  (subscribe :resource-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

(defun open-class-inspector (resource)
  (open-resource-inspector (or resource (make-instance 'game-resource-class)) :editing))

(defmethod open-resource-inspector ((resource game-resource-class) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'game-resource-class-inspector
                                :resource resource
                                :pretty-name (format nil "~a — ~a ~a"
                                                     (game-resource-title resource)
                                                     *game-title* (machine-directory-name))
                                :view-mode mode)))

(clim:define-command (com-new-class :command-table clim-internals::global-command-table :menu t :name t) ()
  (open-class-inspector nil))

(clim:define-command (com-compile-class :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Compiling class via Eightbol...~%"))))

(clim:define-command (com-build-class-demo :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building class (Demo)...~%"))))

(clim:define-command (com-build-class-public :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building class (Public)...~%"))))

(clim:define-command (com-build-class-publisher :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building class (Publisher)...~%"))))

(clim:define-command (com-region-ntsc-class :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :ntsc)
  (publish :region-changed :payload :ntsc))

(clim:define-command (com-region-pal-class :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :pal)
  (publish :region-changed :payload :pal))

(clim:define-command (com-region-secam-class :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :secam)
  (publish :region-changed :payload :secam))

(defmethod game-resource-action-menu ((resource game-resource-class))
  (list
   (make-menu-item "Inspect..." (lambda () (open-class-inspector resource)))
   (make-menu-item "Open in Emacs..."
                   (lambda ()
                     (open-in-emacs (game-resource-full-path resource))))))
(in-package :skyline-tool)

(defun load-class-defs (&optional (path "../Source/Classes/Classes.Defs"))
  "Parse Classes.Defs and return a list of class plists.
   Format: ClassName < ParentClass  (class with inheritance)
           .SlotName Size = annotation  (slot)
           #MethodName                  (method)
           ; comment"
  (let ((classes nil) (class-names nil) (class-info nil) (class-name nil)
        (parent nil) (slots nil) (methods nil))
    (labels ((commit-class ()
               (when class-name
                 (push (list :name class-name :parent parent
                             :slots (nreverse slots)
                             :methods (nreverse methods))
                       classes)
                 (push class-name class-names)
                 (setf class-name nil parent nil slots nil methods nil)))))
      (let ((full-path (merge-pathnames path (uiop:getcwd))))
        (when (probe-file full-path)
          (with-open-file (f full-path)
            (loop for line = (read-line f nil nil)
                  while line
                  do (let ((trimmed (string-trim " " line)))
                       (cond
                         ;; Skip blank lines and comments
                         ((or (zerop (length trimmed))
                              (char= (char trimmed 0) #\;)))
                         ;; Class definition: Name < Parent
                         ((search " < " trimmed)
                          (commit-class)
                          (let ((parts (split-sequence " < " trimmed)))
                            (setf class-name (string-trim " " (first parts))
                                  parent (string-trim " " (second parts)))))
                         ;; Method: #MethodName
                         ((char= (char trimmed 0) #\#)
                          (push (subseq trimmed 1) methods))
                         ;; Slot: .SlotName size = annotation
                         ((char= (char trimmed 0) #\.)
                          (let* ((rest (subseq trimmed 1))
                                 (parts (split-sequence #\Space rest))
                                 (sname (first parts))
                                 (ssize (when (> (length parts) 1) (second parts)))
                                 (sannot (when (> (length parts) 2)
                                           (subseq rest (position #\= rest)))))
                            (push (list :name sname :size ssize :annotation sannot) slots)))
                         ;; Bare name without < : class with no parent
                         (t
                          (commit-class)
                          (setf class-name (string-trim " " trimmed)
                                parent nil))))))
            (commit-class))))
  (values (nreverse classes) (nreverse class-names)))

(defun find-method-in-cob (method-name)
  "Find METHOD-NAME in .cob files, return (file line-number) or nil."
  (let* ((files (directory #p"../Source/Classes/*.cob"))
         (patterns (list (format nil "METHOD-ID. \"~a\"" method-name)
                         (format nil "METHOD-ID. ~a" method-name))))
    (dolist (file files)
      (with-open-file (f file)
        (loop for line = (read-line f nil nil)
              for line-num from 1
              while line
              do (when (some (lambda (p) (search p line :test #'char-equal)) patterns)
                   (return-from find-method-in-cob
                     (list (namestring (truename file)) line-num))))))
    nil))

(defun oops-class-inspector ()
  "Open a class hierarchy browser window."
  (multiple-value-bind (classes class-names) (load-class-defs)
    (clim-simple-echo:run-in-simple-echo
     (lambda ()
       (format t "~&~a Class Hierarchy~2%" (or (and (boundp '*game-title*) *game-title*) "Game"))
       (dolist (c classes)
         (let ((name (getf c :name))
               (parent (getf c :parent))
               (slots (getf c :slots))
               (methods (getf c :methods)))
           (format t "~&~a" name)
           (when parent (format t "  <  ~a" parent))
           (terpri)
           (when slots
             (format t "  Slots:~%")
             (dolist (s slots)
               (format t "    .~a (~a)~@[  ~a~]~%"
                       (getf s :name) (or (getf s :size) "?")
                       (getf s :annotation))))
           (when methods
             (format t "  Methods:~%")
             (dolist (m methods)
               (format t "    #~a" m)
               (let ((loc (find-method-in-cob m)))
                 (if loc
                     (format t "  [~a:~d]" (enough-namestring (first loc)) (second loc))
                     (format t "  [not in .cob]")))
               (terpri)))
           (terpri))))))) 
