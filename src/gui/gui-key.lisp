;;; Skyline-Tool src/gui/gui-key.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

;; Presentation types for game-resource keys
(clim:define-presentation-type game-resource-key-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-key-reference)))
  (typep object 'game-resource-key))

(clim:define-presentation-type game-resource-key-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-key-editable)))
  (typep object 'game-resource-key))

(clim:define-presentation-type game-resource-key-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-key-viewing)))
  (typep object 'game-resource-key))

;; Reference presentation
(clim:define-presentation-method clim:present ((resource game-resource-key)
                                               (type game-resource-key-reference) stream view &key)
  (declare (ignore view))
  (clim:formatting-table (stream)
    ;; Header row
    (clim:formatting-row (stream)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Locator: "))
        (clim:formatting-cell (stream :align-x :left)
          (format stream "~a" (game-resource-locator resource))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Name: "))
        (clim:formatting-cell (stream :align-x :left)
          (format stream "~a" (game-resource-title resource)))))))

;; Editable presentation
(clim:define-presentation-method clim:present ((resource game-resource-key)
                                               (type game-resource-key-editable) stream view &key)
  (declare (ignore view))
  (clim:formatting-table (stream)
    ;; Header row
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :center :align-y :top :min-height 30)
        (clim:with-text-face (stream :bold)
          (format stream "EDIT KEY")))
      ;; ID row
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "ID: "))
        (clim:formatting-cell (stream :align-x :left)
          (format stream "~a" (game-resource-id resource))))
      ;; Moniker row
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :left :min-width 150)
          (format stream "Moniker: ~a" (game-asset-moniker resource))))
      ;; Name row
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Name: "))
        (clim:formatting-cell (stream :align-x :left)
          (interactive-editing-gadget-with-validation
           stream resource
           (lambda (r) (game-resource-title r))
           (lambda (r v) (setf (game-resource-title r) v))
           :label "Name:"
           :validator #'validate-key-name
           :max-length 25))
        ;; Build indicators row
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream :align-x :left)
            (format stream "Build: "))
          (clim:formatting-cell (stream :align-x :left)
            (when (game-resource-build-high-res resource)
              (format stream "✓ High-Res "))
            (when (game-resource-build-compressed resource)
              (format stream "✓ Compressed "))))
        ;; Palette row (if applicable)
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream :align-x :center :min-width 200)
            (when (let* ((png-path (make-pathname :defaults xcf-path :type "png"))
                         (png-data (png-read:read-png-file png-path))))
              (clim-image stream (png->image png-data) :fit-to-width 200))))))))

;; Viewing presentation
(clim:define-presentation-method clim:present ((resource game-resource-key)
                                               (type game-resource-key-viewing) stream view &key)
  (declare (ignore view))
  (clim:formatting-table (stream)
    ;; Header row
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :center :align-y :top :min-height 30)
        (clim:with-text-face (stream :bold)
          (format stream "KEY DETAILS")))
      ;; ID row
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "ID: "))
        (clim:formatting-cell (stream :align-x :left)
          (format stream "~a" (game-resource-id resource))))
      ;; Moniker row
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :left :min-width 150)
          (format stream "Moniker: ~a" (game-asset-moniker resource))))
      ;; Name row
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Name: "))
        (clim:formatting-cell (stream :align-x :left)
          (format stream "~a" (game-resource-title resource))))
      ;; Palette preview
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :center :min-width 300)
          (when (let* ((png-path (make-pathname :defaults xcf-path :type "png"))
                       (png-data (png-read:read-png-file png-path))))
            (clim-image stream (png->image png-data) :fit-to-width 300)))))))

;; Action menu
(defmethod game-resource-action-menu ((resource game-resource-key))
  (list
   (make-menu-item "Inspect..." (lambda () (open-key-inspector resource)))
   (make-menu-item "Open in Text Editor...
                   " (lambda ()
                       (uiop:run-program
                        (list "gedit" (truename (first (game-resource-pathnames resource))))
                        :output nil :ignore-error-status t)))))

;; Helper functions
(defun validate-key-name (name &optional (max-length 25))
  "Validate that NAME is a valid key name with special rules."
  (when (find " " name)
    (error "Key names may not contain spaces."))
  (call-next-method))

(defun write-resource-common-ps (resource ps)
  (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont~%")
  (format ps "56 560 moveto~%")
  (format ps "(KEY: ~a) show~%" (escape-ps-string (game-asset-moniker resource)))
  (format ps "showpage~%"))

(defun write-resource-common-text (resource stream)
  (format stream "Key: ~a\n" (escape-ps-string (game-asset-moniker resource))))

(defun encode-file-to-base91 (path)
  "Read file at PATH and return base91-encoded string, or NIL if file not found."
  (when (and path (probe-file path))
    (with-open-file (stream path :element-type '(unsigned-byte 8))
      (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
        (read-sequence bytes stream)
        (encode-base91 bytes)))))

(defmethod resource-to-json ((resource game-resource-key))
  "Convert KEY resource to JSON."
  (error "unimplemented"))
