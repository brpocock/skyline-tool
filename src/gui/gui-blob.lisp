;;; Skyline-Tool src/gui/gui-blob.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

;; Presentation types for BLOB resources
(clim:define-presentation-type game-resource-blob-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-blob-reference)))
  (typep object 'game-resource-blob))

(clim:define-presentation-type game-resource-blob-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-blob-editable)))
  (typep object 'game-resource-blob))

(clim:define-presentation-type game-resource-blob-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-blob-viewing)))
  (typep object 'game-resource-blob))

;; Reference presentation
(clim:define-presentation-method clim:present ((resource game-resource-blob)
                                                (type game-resource-blob-reference) stream view &key)
  (declare (ignore view))
  (clim:formatting-table (stream)
    ;; Header row
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :center :align-y :top :min-height 30)
        (clim:with-text-face (stream :bold)
          (format stream "BLOB Resource"))))
    ;; Icon row
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 125)
        (game-resource-present-icon resource stream))
      (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 200)
        (clim:with-text-size (stream :larger)
          (clim:with-text-face (stream :bold)
            (game-resource-present-title resource stream)))
        (format stream "~%")
        (clim:with-text-size (stream :smaller)
          (clim:with-drawing-options (stream :ink (clim:make-gray-color 0.75))
            (game-resource-present-subheading resource stream))))
      (clim:formatting-cell (stream :align-x :right :align-y :top :min-height 90 :min-width 125)
        (game-resource-present-right-margin resource stream)))))

;; Editable presentation
(clim:define-presentation-method clim:present ((resource game-resource-blob)
                                               (type game-resource-blob-editable)
                                               stream view &key)
  (declare (ignore view))
  (clim:formatting-table (stream)
    ;; Header row
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :center :align-y :top :min-height 30)
        (clim:with-text-face (stream :bold)
          (format stream "Edit BLOB"))))
    ;; ID row
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Locator: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-locator resource))))
    ;; Moniker row
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Moniker: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-asset-moniker resource))))
    ;; Name row
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
(interactive-editing-gadget-with-validation
          stream resource
          :getter (lambda (r) (game-resource-title r))
          :setter (lambda (r v) (setf (game-resource-title r) v))
          :label "Name:"
         :validator #'validate-blob-name
         :max-length 200)))
;; Build indicators row
(clim:formatting-row (stream)
  (clim:formatting-cell (stream :align-x :left)
    (format stream "Builds: "))
  (clim:formatting-cell (stream :align-x :left)
    (when (game-resource-build-demo-p resource)
      (format stream "✓ Demo "))
    (when (game-resource-build-public-p resource)
      (format stream "✓ Public "))
    (let ((publisher (game-resource-publisher-name resource)))
      (when publisher
        (format stream "✓ ~a " publisher))))))

;; Viewing presentation
(clim:define-presentation-method clim:present ((resource game-resource-blob) (type game-resource-blob-viewing) stream view &key)
  (declare (ignore view))
  (clim:formatting-table (stream)
    ;; Header row
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :center :align-y :top :min-height 30)
        (clim:with-text-face (stream :bold)
          (format stream "BLOB Details"))))
    ;; ID row
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Locator: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-locator resource))))
    ;; Moniker row
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Moniker: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-asset-moniker resource))))
    ;; Name row
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-title resource))))
    ;; Build indicators row
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :left)
        (format stream "Builds: "))
      (clim:formatting-cell (stream :align-x :left)
        (when (game-resource-build-high-res resource)
          (format stream "✓ High-Res "))
        (when (game-resource-build-compressed resource)
          (format stream "✓ Compressed "))))
    ;; Image preview
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :center :min-width 300)
        (let ((png-path (make-pathname :defaults (first (game-resource-pathnames resource)) :type "png")))
          (unless (probe-file png-path)
            (build-target png-path))
          (when (probe-file png-path)
            (let ((png-data (png-read:read-png-file png-path)))
              (clim-image stream (png->image png-data) :fit-to-width 300))))))))

(defmethod present-editing ((resource game-resource-blob) stream)
  (clim:present resource 'game-resource-blob-editable :stream stream))

(defmethod present-reading ((resource game-resource-blob) stream)
  (clim:present resource 'game-resource-blob-viewing :stream stream))

(defmethod present-reference ((resource game-resource-blob) stream)
  (clim:present resource 'game-resource-blob-reference :stream stream))

(defun open-blob-inspector (resource)
  (open-resource-inspector (or resource
                                 (make-instance 'game-resource-blob
                                                :full-path nil
                                                :moniker "Blobs/new-blob.xcf")) :editing))

(defmethod game-resource-action-menu ((resource game-resource-blob))
  (list
   (make-menu-item "Inspect..." (lambda () (open-blob-inspector resource)))
   (make-menu-item "Open in Gimp..."
                   (lambda ()
                     (uiop:run-program
                      (list "gimp" (truename (first (game-resource-pathnames resource))))
                      :output nil :ignore-error-status t)))))

(clim:define-application-frame game-resource-blob-inspector (gui-inspector-frame clim:standard-application-frame)
  ()
  (:menu-bar inspector-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "Blob Inspector"))

(defmethod initialize-instance :after ((frame game-resource-blob-inspector) &key)
  (call-next-method)
  (subscribe :region-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

(defmethod open-resource-inspector ((resource game-resource-blob) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'game-resource-blob-inspector
                                :resource resource
                                :pretty-name (format nil "~a — ~a ~a"
                                                     (game-resource-title resource)
                                                     *game-title* (machine-directory-name))
                                :view-mode mode)))

;; 
;; Resource-specific export methods for game-resource-blob
;; 

(defmethod resource-to-json ((resource game-resource-blob))
  (call-next-method))

(defmethod resource-from-json (json (class (eql 'game-resource-blob)))
  (make-instance 'game-resource-blob
                 :moniker (gethash "moniker" json)
                 :kind (or (ignore-errors (kind-by-name (gethash "kind" json)))
                           (gethash "kind" json))
                 :full-path (gethash "path" json)))

  (defmethod write-resource-ps-content ((resource game-resource-blob) ps)
    (write-resource-common-ps resource ps)
    (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont~%")
    (format ps "56 560 moveto~%")
    (format ps "(BLOB: ~a) show~%" (escape-ps-string (game-asset-moniker resource)))
    (format ps "showpage~%")
    (format ps "<< /PageSize [792 612] >> setpagedevice~%")
    (format ps "56 480 moveto~%")
    (format ps "/Helvetica-Bold findfont 14 scalefont setfont~%")
    (format ps "(Palette Colors) show~%")
    (format ps "56 460 moveto~%")
    (format ps "/Helvetica findfont 10 scalefont setfont~%")
    (let* ((xcf-path (first (game-resource-pathnames resource)))
           (png-path (make-pathname :defaults xcf-path :type "png")))
      (unless (probe-file png-path)
        (build-target png-path))
      (when (probe-file png-path)
        (let* ((png-data (png-read:read-png-file png-path))
               (palette (png->palette (png-read:image-data png-data)
                                      (png-read:transparency png-data)))
               (num-colors (min 16 (array-dimension palette 0))))
          (dotimes (i num-colors)
            (let* ((color (aref palette i))
                   (r (nth 0 color))
                   (g (nth 1 color))
                   (b (nth 2 color))
                   (y (+ 460 (* i 10))))
              (format ps "~d ~d ~d setrgbcolor~%" r g b)
              (format ps "56 ~d 10 10 rectfill~%" y)
              (format ps "0 0 0 setrgbcolor~%")
              (format ps "72 ~d moveto~%" (+ y 2))
              (format ps "/Helvetica findfont 6 scalefont setfont~%")
              (format ps "(~a) show~%" (format nil "[~2,'0x]" r)))))))
    (format ps "showpage~%")
    (format ps "0 0 moveto~%")
    (format ps "/Times-Roman findfont 8 scalefont setfont~%")
    (format ps "(Skyline-Tool for Phantasia 7800 | ~a | Page 1) show~%"
            (escape-ps-string (game-asset-moniker resource))))
