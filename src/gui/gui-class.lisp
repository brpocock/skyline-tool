;;; Skyline-Tool src/gui/gui-class.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-class-reference ()
  :inherit-from 'game-resource-class)

(clim:define-presentation-type game-resource-class-editable ()
  :inherit-from 'game-resource-class)

(clim:define-presentation-type game-resource-class-viewing ()
  :inherit-from 'game-resource-class)

(defmethod present-reference ((resource game-resource-class) stream)
  (clim:with-output-as-presentation
      (stream resource 'game-resource-class-reference)
    (clim:formatting-table (stream)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 0)
          (format stream "~3%"))
        (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 125)
          (game-resource-present-icon resource stream))
        ;; Title
        (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 150)
          (clim:with-text-face (stream :bold)
            (game-resource-present-title resource stream))
          ;; Subheading on next line in small, possibly gray text
          (format stream "~%~5t")
          (clim:with-text-size (stream :smaller)
            (clim:with-drawing-options (stream :ink (clim:make-gray-color 0.75))
              (game-resource-present-subheading resource stream))))
        (clim:formatting-cell (stream :align-x :right :align-y :top :min-height 90 :min-width 125)
          (game-resource-present-right-margin resource stream))))))

(defmethod open-resource-inspector ((resource game-resource-class) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'game-resource-class-inspector
                                 :resource (or resource (make-instance 'game-resource-class))
                                :view-mode mode)))

(clim:define-application-frame game-resource-class-inspector (gui-inspector-frame clim:standard-application-frame)
  ()
  (:menu-bar class-menu-bar)
  (:pretty-name "Class Inspector"))

(clim:define-command-table class-menu-bar
  :menu (("Class" :menu inspector-file-menu)
         ("Edit" :menu inspector-edit-menu)
         ("Run"  :menu inspector-run-menu)
         ("View" :menu inspector-view-menu)
         ("Help" :menu inspector-help-menu)))

(defun open-class-inspector (resource)
  (open-resource-inspector (or resource (make-instance 'game-resource-class)) :editing))

(defmethod present-reading ((resource game-resource-class) stream)
  (let* ((class-name (game-resource-title resource))
         (full-path (game-resource-full-path resource))
         (class-def (when (and full-path (probe-file full-path))
                      (with-open-file (in full-path :direction :input)
                        (read-line-in full-path)))))
    (clim:formatting-table (stream)
      ;; Class Name
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Class Name:"))
        (clim:formatting-cell (stream :align-x :left)
          (format stream "~a" class-name)))
      ;; Parent Class
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Parent Class:"))
        (clim:formatting-cell (stream :align-x :left)
          (clim:with-output-as-presentation
              (stream (or (find-parent-class class-name) (make-instance 'game-resource))
                'game-resource-reference))))
      ;; Child Classes
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Child Classes:"))
        (clim:formatting-cell (stream :align-x :left)
          (present-child-classes class-name stream)))
      ;; Methods
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Methods:"))
        (clim:formatting-cell (stream :align-x :left)
          (present-methods class-name stream))))))

(defmethod present-editing ((resource game-resource-class) stream)
  (let* ((class-name (game-resource-title resource))
         (full-path (game-resource-full-path resource)))
    (clim:formatting-table (stream)
      ;; Class Name (editable)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Class Name:"))
        (clim:formatting-cell (stream :align-x :left)
          (insert-gadget stream
                         :label nil
                         :variable class-name
                         :activation-callback
                         (lambda (gadget)
                           (setf (game-resource-title resource)
                                 (clim:gadget-value gadget))))))
      ;; Parent Class (editable via reference)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Parent Class:"))
        (clim:formatting-cell (stream :align-x :left)
          (insert-gadget stream
                         :label nil
                         :variable (find-parent-class class-name)
                         :presentation-type 'game-resource-reference
                         :activation-callback
                         (lambda (gadget)
                           (set-parent-class class-name
                                             (game-resource-title (clim:gadget-value gadget)))))))
      ;; Methods (list with edit buttons)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Method:"))
        (clim:formatting-cell (stream :align-x :left)
          (present-method-edit-buttons class-name stream))))))

;; Helper functions (to be implemented based on actual class system)
(defun find-parent-class (class-name)
  ;; Return the parent class resource or nil if none
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
