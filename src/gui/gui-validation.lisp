;;; Skyline-Tool src/gui/gui-validation.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(define-constant +all-minifont-chars+ 
    (concatenate 'string "0123456789abcdefghijklmnopqrstuvwxyz" +minifont-punctuation+)
  :test 'string=)

(defun validate-minifont-name (name length)
  "Validate that NAME contains only minifont characters and respects length limits.
   Returns (VALUES valid-p error-indicator)"
  (let ((len (length (unicode->minifont name)))
        (invalid-chars nil))
    (when (> len length)
      (return-from validate-minifont-name (values nil :too-long)))
    (dolist (char (coerce name 'list))
      (unless (find char +all-minifont-chars+ :test #'char-equal)
        (push char invalid-chars)))
    (if invalid-chars
        (values nil (cons :invalid-chars invalid-chars))
        (values t nil))))

(defmethod present-editing :around ((resource game-resource-flag) stream)
  (let ((name (game-resource-title resource))
        (stream stream))
    (multiple-value-bind (valid-p error)
        (validate-minifont-name name)
      (clim:formatting-table (stream)
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream)
            (clim:with-text-face (stream :bold)
              (format stream "Name:")))
          (clim:formatting-cell (stream)
            (let ((gadget (clim:insert-gadget stream
                                              :label nil
                                              :variable name
                                              :activation-callback
                                              (lambda (gadget)
                                                (setf (game-resource-title resource)
                                                      (clim:gadget-value gadget))))))
              (when error
                ;; Show red underline and ballot X
                (clim:with-drawing-options (stream :ink :red :line-width 2)
                  (clim:draw-line* stream 
                                   (clim:gadget-left gadget) 
                                   (+ (clim:gadget-top gadget) 10)
                                   (clim:gadget-right gadget) 
                                   (+ (clim:gadget-top gadget) 10))
                  (clim:draw-text stream "✗" 
                                  (+ (clim:gadget-right gadget) 5)
                                  (clim:gadget-top gadget)))))))))))

(defmethod present-editing :around ((resource game-resource-key) stream)
  (let ((name (game-resource-title resource))
        (stream stream))
    (multiple-value-bind (valid-p error)
        (validate-minifont-name name)
      (clim:formatting-table (stream)
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream)
            (clim:with-text-face (stream :bold)
              (format stream "Name:")))
          (clim:formatting-cell (stream)
            (let ((gadget (clim:insert-gadget stream
                                              :label nil
                                              :variable name
                                              :activation-callback
                                              (lambda (gadget)
                                                (setf (game-resource-title resource)
                                                      (clim:gadget-value gadget))))))
              (when error
                (clim:with-drawing-options (stream :ink :red)
                  (clim:draw-line* stream (clim:gadget-left gadget) 
                                   (+ (clim:gadget-top gadget) 10)
                                   (clim:gadget-right gadget) 
                                   (+ (clim:gadget-top gadget) 10))
                  (clim:draw-text stream "✗" 
                                  (+ (clim:gadget-right gadget) 5)
                                  (clim:gadget-top gadget)))))))))))

(defmethod present-editing :around ((resource game-resource-item) stream)
  (let* ((item resource)
         (ods-path (game-resource-collective-path item))
         (headers (get-ods-headers ods-path))
         (values (get-item-values item)))
    (clim:formatting-table (stream)
      (dolist (header headers)
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream)
            (clim:with-text-face (stream :bold)
              (format stream "~a" header)))
          (clim:formatting-cell (stream)
            (let* ((value (assoc header values :test #'string-equal))
                   (gadget-value (if value (cdr value) "")))
              (clim:insert-gadget stream
                :label nil
                :variable gadget-value
                :activation-callback
                (lambda (gadget)
                  (setf (cdr (assoc header values :test #'string-equal))
                        (clim:gadget-value gadget))
                  (save-item-to-ods item ods-path values))))))))))

