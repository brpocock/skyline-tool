;;; Skyline-Tool src/gui/gui-validation.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(define-constant +all-minifont-chars+ 
    (concatenate 'string "0123456789abcdefghijklmnopqrstuvwxyz" +minifont-punctuation+)
  :test 'string=)

(defun validate-minifont-name (name &optional (max-length 20))
  "Validate that NAME contains only minifont characters and respects length limits.
   Returns (VALUES valid-p error-indicator)"
  (let ((invalid-chars nil))
    (dolist (char (coerce name 'list))
      (unless (find char +all-minifont-chars+ :test #'char-equal)
        (push char invalid-chars)))
    (when invalid-chars
      (return-from validate-minifont-name
        (values nil (format nil "Unacceptable characters: ~{ ~c~:*~:@c~^  ~}"
                            invalid-chars)))))
  (let* ((minifonted (unicode->minifont name))
         (len (length minifonted)))
    (when (> len max-length)
      (return-from validate-minifont-name
        (values nil (format nil "Too long, would be truncated to “~a”"
                            (minifont->unicode minifonted)))))))

(defun validate-file-name (name &optional (max-length 200))
  "Validate that NAME consists of Unicode alphanumeric characters, 1-200 long.
   Used for filenames.
   Returns (VALUES valid-p error-indicator)"
  (cond
    ((null name)
     (values nil :nil))
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

(defun insert-gadget (stream &key label variable
                                  presentation-type activation-callback)
  "Insert a text-field gadget into STREAM with initial value VARIABLE.
LABEL is an optional label string displayed before the field.
PRESENTATION-TYPE is currently ignored (text fields don't use presentations).
ACTIVATION-CALLBACK is called with the gadget when the value changes.
Returns the gadget."
  (declare (ignore presentation-type))
  (when label
    (clim:with-text-face (stream :bold)
      (format stream "~a " label)))
  (let ((gadget (clim:with-output-as-gadget (stream)
                  (clim:make-pane 'clim:text-field
                                  :value (or variable "")
                                  :activate-callback activation-callback))))
    gadget))

(defun insert-button (stream &key label activation-callback)
  "Insert a button gadget into STREAM with label LABEL.
ACTIVATION-CALLBACK is a zero-argument function called when the button is pressed.
Returns the gadget."
  (clim:with-output-as-gadget (stream)
    (clim:make-pane 'clim:push-button
                    :label (or label "Button")
                    :activate-callback (lambda (gadget)
                                         (declare (ignore gadget))
                                         (funcall activation-callback)))))

;; Gadget geometry accessors
(defun gadget-left (gadget)
  (clim:bounding-rectangle-min-x (clim:bounding-rectangle gadget)))

(defun gadget-right (gadget)
  (clim:bounding-rectangle-max-x (clim:bounding-rectangle gadget)))

(defun gadget-top (gadget)
  (clim:bounding-rectangle-min-y (clim:bounding-rectangle gadget)))

(defun gadget-bottom (gadget)
  (clim:bounding-rectangle-max-y (clim:bounding-rectangle gadget)))

;; Full-featured editing gadget with validation feedback.
;; GETTER takes resource and returns current value.
;; SETTER takes resource and new-value to set it.
;; Returns the gadget.
(defun interactive-editing-gadget-with-validation (stream resource
                                                   &key getter setter
                                                        (label nil)
                                                        (validator #'validate-minifont-name)
                                                        (max-length 20)
                                                        (callback nil))
  (let ((current-value (funcall getter resource)))
    (insert-gadget stream
                   :label label
                   :variable current-value
                   :activation-callback
                   (lambda (gadget)
                     (let ((new-value (clim:gadget-value gadget)))
                       (multiple-value-bind (valid-p error)
                           (funcall validator new-value max-length)
                         (if valid-p
                             (progn
                               (funcall setter resource new-value)
                               (when callback
                                 (funcall callback resource new-value)))
                             (progn
                               (clim:with-drawing-options (stream :ink :red :line-width 2)
                                 (clim:draw-line* stream
                                                  (gadget-left gadget)
                                                  (+ (gadget-bottom gadget) 3)
                                                  (gadget-right gadget)
                                                  (+ (gadget-bottom gadget) 3))
                                 (clim:draw-text* stream "✗"
                                                  (+ (gadget-left gadget) 5)
                                                  (+ (gadget-bottom gadget) 7))
                                 (clim:draw-text* stream error
                                                  (+ (gadget-left gadget) 45)
                                                  (+ (gadget-bottom gadget) 7)))))))))))

(defun insert-combo-gadget-with-validation (stream resource
                                            &key accessor items
                                                 (label nil) (callback nil))
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


