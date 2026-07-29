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

;; Build selector check-box — shared across resource inspectors
(defun %build-selector (stream label resource keyword slot-string)
  (clim:with-output-as-gadget (stream)
    (clim:make-pane 'clim:check-box
                    :label label
                    :value (not (null (game-asset-build-p resource keyword)))
                    :value-changed-callback
                    (lambda (g v)
                      (declare (ignore g))
                      (let ((builds (game-asset-builds resource)))
                        (if v
                            (unless (member slot-string builds :test 'string-equal)
                              (setf (slot-value resource 'builds)
                                    (append builds (list slot-string))))
                            (setf (slot-value resource 'builds)
                                  (remove slot-string builds :test 'string-equal :count 1))))
                      (ignore-errors
                       (clim:redisplay-frame-panes clim:*application-frame*
                                                   :force-p t))))))

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
         :validator #'validate-file-name
         :max-length 200)))
    ;; Build indicators row
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :left)
        (format stream "Builds: "))
      (clim:formatting-cell (stream :align-x :left)
        (%build-selector stream "Demo" resource :demo "Demo")
        (terpri stream)
        (%build-selector stream "Public" resource :public "Public")
        (terpri stream)
        (%build-selector stream (string *publisher*) resource :publisher "AA")))))

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
        (when (game-asset-build-p resource :demo)
          (format stream "✓ Demo "))
        (when (game-asset-build-p resource :public)
          (format stream "✓ Public "))
        (when (game-asset-build-p resource :publisher)
          (format stream "✓ ~a" *publisher*))))
    ;; Image preview with region-adjusted palette
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :center :min-width 300)
        (let ((png-path (make-pathname :defaults (first (game-resource-pathnames resource)) :type "png")))
          (unless (probe-file png-path)
            (build-target png-path))
          (when (probe-file png-path)
            (let* ((png (png-read:read-png-file png-path))
                   (*region* (get-pref :region)))
              (clim-image stream (png->image png) :fit-to-width 300))))))))

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

(defun format-atari-color-name (register &optional (tv *region*))
  (let* ((hue (ash register -4))
         (keyword (atari-color-name hue tv))
         (name (subseq (string keyword) 3)))
    (format nil "~:(~a~) $~x" (string-downcase name) register)))

(defmethod write-resource-ps-content ((resource game-resource-blob) ps)
  (write-resource-common-ps resource ps)
  (format ps "/plot-wide-pixel {  % x y r g b — fill 12×10 rect swatch at (x,y) with color
    /b exch def /g exch def /r exch def /y exch def /x exch def
    r g b setrgbcolor x y 12 10 rectfill
} def~%")
  (format ps "56 480 moveto /Helvetica-Bold findfont 14 scalefont setfont (Palette Colors) show~%")
  (format ps "/Helvetica findfont 8 scalefont setfont~%")
  (let* ((xcf-path (first (game-resource-pathnames resource)))
         (png-path (make-pathname :defaults xcf-path :type "png")))
    (unless (probe-file png-path)
      (build-target png-path))
    (when (probe-file png-path)
      (let* ((*region* (get-pref :region))
             (png (png-read:read-png-file png-path))
             (indices (png->palette (png-read:image-data png)
                                    (png-read:transparency png)))
             (machine-pal (machine-palette))
             (unique (remove-duplicates
                      (loop for x below (array-dimension indices 0)
                            nconc (loop for y below (array-dimension indices 1)
                                        for idx = (aref indices x y)
                                        when idx collect idx))))
             (sorted (sort (subseq unique 0 (min 25 (length unique))) #'<)))
        (loop for slot from 0
              for reg in sorted
              for y from 460 downto 0 by 12
              for label = (if (zerop slot) "BACKGRND"
                              (format nil "P~dC~d" (floor (1- slot) 3) (1+ (mod (1- slot) 3))))
              for rgb = (nth reg machine-pal)
              do (destructuring-bind (r g b) rgb
                   (format ps "56 ~d moveto (~a) show~%" y label)
                   (format ps "120 ~d ~f ~f ~f plot-wide-pixel~%"
                           y (/ r 255.0) (/ g 255.0) (/ b 255.0))
                   (format ps "140 ~d moveto (~a) show~%"
                           (+ y 2)
                           (escape-ps-string (format-atari-color-name reg))))))
      (format ps "showpage~%"))))
