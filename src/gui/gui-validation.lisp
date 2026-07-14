;;; Skyline-Tool src/gui/gui-validation.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(define-constant +all-minifont-chars+ 
    (concatenate 'string "0123456789abcdefghijklmnopqrstuvwxyz" +minifont-punctuation+)
  :test 'string=)

(defun validate-minifont-name (name &optional (max-length 20))
  "Validate that NAME contains only minifont characters and respects length limits.
   Returns (VALUES valid-p error-indicator)"
  (let ((len (length (unicode->minifont name)))
        (invalid-chars nil))
    (when (> len max-length)
      (values nil :too-long))
    (dolist (char (coerce name 'list))
      (unless (find char +all-minifont-chars+ :test #'char-equal)
        (push char invalid-chars)))
    (if invalid-chars
        (values nil (cons :invalid-chars invalid-chars))
        (values t nil))))

(defun validate-blob-name (name &optional (max-length 200))
  "Validate that NAME is PascalCase and consists of Unicode alphanumeric characters.
   Returns (VALUES valid-p error-indicator)"
  (cond
    ((= (length name) 0)
     (values nil :too-short))
    ((> (length name) max-length)
     (values nil :too-long))
    ((and (> (length name) 1) (not (char-equal (elt name 0) (char-upcase (elt name 0)))))
     (values nil :not-pascalcase))
    ((not (every #'alphanumericp (coerce name 'list)))
     (values nil :invalid-characters))
    (t (values t nil))))

(defun validate-asset-name (name &optional (max-length 200))
  "Validate that NAME consists of Unicode alphanumeric characters, 1-200 long.
   Used for Map, Song, Object-Prototype, Script, and BLOB resource names.
   Returns (VALUES valid-p error-indicator)"
  (cond
    ((= (length name) 0)
     (values nil :too-short))
    ((> (length name) max-length)
     (values nil :too-long))
    ((not (every #'alphanumericp (coerce name 'list)))
     (values nil :invalid-characters))
    (t (values t nil))))

(defun rename-asset (resource new-name)
  "Attempt to rename an asset. Currently not permitted - signals an error."
  (declare (ignore new-name))
  (error "Renaming assets is not currently supported for resource: ~a" (game-resource-title resource)))

;; Gadget geometry accessors
(defun gadget-left (gadget)
  (clim:bounding-rectangle-min-x (clim:bounding-rectangle gadget)))

(defun gadget-right (gadget)
  (clim:bounding-rectangle-max-x (clim:bounding-rectangle gadget)))

(defun gadget-top (gadget)
  (clim:bounding-rectangle-min-y (clim:bounding-rectangle gadget)))

;; Full-featured editing gadget with validation feedback.
;; GETTER takes resource and returns current value.
;; SETTER takes resource and new-value to set it.
;; Returns the gadget.
(defun interactive-editing-gadget-with-validation (stream resource getter setter
                                                   &key (label nil)
                                                        (validator #'validate-minifont-name)
                                                        (max-length 20)
                                                        (callback nil))
  (let* ((current-value (funcall getter resource))
         (gadget (insert-gadget stream
                                :label label
                                :variable current-value
                                :activation-callback
(lambda (gadget)
                                  (let ((new-value (clim:gadget-value gadget)))
                                    (multiple-value-bind (valid-p _error)
                                        (funcall validator new-value max-length)
                                      (declare (ignore _error))
                                      (if valid-p
                                          (progn
                                            (funcall setter resource new-value)
                                            (when callback
                                              (funcall callback resource new-value)))
                                          (progn
                                            (clim:with-drawing-options (stream :ink :red :line-width 2)
                                              (clim:draw-line* stream
                                                               (gadget-left gadget)
                                                               (+ (gadget-top gadget) 10)
                                                               (gadget-right gadget)
                                                               (+ (gadget-top gadget) 10))
                                              (clim:draw-text* stream "✗"
                                                               (+ (gadget-right gadget) 5) (gadget-top gadget))))))))))
    gadget))

(defun insert-combo-gadget-with-validation (stream resource accessor items
                                            &key (label nil) (callback nil))
  "Insert a combo-box gadget with validation (selected item must be in ITEMS)."
  (let ((current-value (funcall accessor resource)))
    (insert-gadget stream
                   :label label
                   :variable current-value
                   :activation-callback
                   (lambda (gadget)
                     (let ((new-value (clim:gadget-value gadget)))
                       (when (member new-value items :test #'string-equal)
                         (funcall accessor resource new-value)
                         (when callback
                           (funcall callback resource new-value))))))))

(defun fixme-interactive-editing-gadget-with-validation (stream resource accessor)
  "Backward-compatible shim for FIXME placeholder.
   ACCESSOR is a symbol naming a generic function (e.g. 'game-resource-title).
   Creates a proper interactive editing gadget with validation."
  (interactive-editing-gadget-with-validation
   stream resource
   (lambda (r) (funcall accessor r))
   (lambda (r v) (setf (funcall (fdefinition `(setf ,accessor)) r) v))
   :label (string-downcase (symbol-name accessor))
   :validator #'validate-minifont-name
   :max-length 20))

;; Present editing with validation for game-resource-flag
(defmethod present-editing :around ((resource game-resource-flag) stream)
  (let ((name (game-resource-title resource))
        (stream stream))
    (multiple-value-bind (valid-p _error)
        (validate-minifont-name name)
      (declare (ignore _error))
      (clim:formatting-table (stream)
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream)
            (clim:with-text-face (stream :bold)
              (format stream "Name:")))
          (clim:formatting-cell (stream)
            (let ((gadget (insert-gadget stream
                                         :label nil
                                         :variable name
                                         :activation-callback
                                         (lambda (gadget)
                                           (setf (game-resource-flag-name resource)
                                                 (clim:gadget-value gadget))))))
              (unless valid-p
                ;; Show red underline and ballot X
                (clim:with-drawing-options (stream :ink :red :line-width 2)
                  (clim:draw-line* stream 
                                   (gadget-left gadget) 
                                   (+ (gadget-top gadget) 10)
                                   (gadget-right gadget) 
                                   (+ (gadget-top gadget) 10))
                  (clim:draw-text* stream "✗" 
                                   (+ (gadget-right gadget) 5) (gadget-top gadget)))))))))))

;; Present editing with validation for game-resource-key
(defmethod present-editing :around ((resource game-resource-key) stream)
  (let ((name (game-resource-title resource))
        (stream stream))
    (multiple-value-bind (valid-p _error)
        (validate-minifont-name name)
      (declare (ignore _error))
      (clim:formatting-table (stream)
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream)
            (clim:with-text-face (stream :bold)
              (format stream "Name:")))
          (clim:formatting-cell (stream)
            (let ((gadget (insert-gadget stream
                                         :label nil
                                         :variable name
                                         :activation-callback
                                         (lambda (gadget)
                                           (setf (game-resource-key-name resource)
                                                 (clim:gadget-value gadget))))))
              (unless valid-p
                (clim:with-drawing-options (stream :ink :red)
                  (clim:draw-line* stream (gadget-left gadget) 
                                   (+ (gadget-top gadget) 10)
                                   (gadget-right gadget) 
                                   (+ (gadget-top gadget) 10))
                  (clim:draw-text* stream "✗" 
                                   (+ (gadget-right gadget) 5) (gadget-top gadget)))))))))))

;; Present editing for game-resource-item
(defmethod present-editing :around ((resource game-resource-item) stream)
  (let* ((item resource)
         (ods-path (game-resource-collective-path item)))
    (clim:formatting-table (stream)
      (format stream "Item editing requires ODS integration - not yet implemented~%"))))
)