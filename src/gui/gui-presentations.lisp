;;; Skyline-Tool src/gui/gui-presentations.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC
;;; Common presentation infrastructure for all game-resource types

(in-package :skyline-tool)

;; 
;; Common Presentation Infrastructure
;; 

(defun validate-json-version (json)
  "Validate JSON version against expected version, erroring if incompatible."
  (let ((my-version (asdf:component-version (asdf:find-system :skyline-tool)))
        (json-version (gethash "Skyline-Tool" json)))
    (unless (version-dotted-triple->= my-version json-version)
      (error "Incompatible JSON version ~a (this is ~a). Resource import aborted."
             json-version my-version))))

(defgeneric resource-to-json (resource)
  (:documentation "Convert RESOURCE to JSON-compatible object for export."))

(defgeneric resource-from-json (json class)
  (:documentation "Create a RESOURCE instance of CLASS from JSON object."))

(defgeneric resource-to-postscript (resource stream)
  (:documentation "Write RESOURCE as PostScript to STREAM with proper headers/footers."))

(defgeneric resource-to-text (resource stream)
  (:documentation "Write RESOURCE as plain text to STREAM for human reading."))



;; 
;; JSON Export/Import Infrastructure
;; 

(defun export-resource-to-json-file (resource filepath)
  "Export RESOURCE to JSON file at FILEPATH."
  (let ((json (resource-to-json resource)))
    (with-open-file (stream filepath :direction :output :if-exists :supersede
                                     :if-does-not-exist :create)
      (write-string (cl-json:encode-json-to-string json) stream))))

(defun import-resource-from-json-file (filepath class)
  "Import RESOURCE of CLASS from JSON file at FILEPATH."
  (let ((json (cl-json:decode-json-from-string
               (uiop:read-file-string filepath))))
    (validate-json-version json)
    (let ((resource (resource-from-json json class)))
      (restore-resource-content-from-json resource json)
      resource)))

(defun restore-resource-content-from-json (resource json)
  "If JSON has an embedded 'content' block (base91-encoded), write it back to disk.
Used by importers to materialize the actual file from a portable JSON export."
  (let ((content (gethash "content" json)))
    (when (and (listp content)
               (string-equal (getf content :encoding) "base91"))
      (let ((data (getf content :data))
            (path (or (ignore-errors (game-resource-full-path resource))
                      (gethash "path" json))))
        (when (and data path)
          (ensure-directories-exist path)
          (with-open-file (stream path :direction :output
                                       :element-type '(unsigned-byte 8)
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
            (let ((bytes (decode-base91 data)))
              (write-sequence bytes stream))))))))

(defun wrap-with-headers (obj class-name)
  "Wrap JSON object with skyline-tool version and class headers."
  (let ((wrapped (make-hash-table :test 'equal)))
    (setf (gethash "skyline-tool" wrapped) 0.6)
    (setf (gethash "class" wrapped) class-name)
    (loop for key being the hash-keys of obj
          do (setf (gethash key wrapped) (gethash key obj)))
    wrapped))

;; 
;; PostScript Export Infrastructure
;; 

(defun export-resource-to-pdf-file (resource filepath)
  "Export RESOURCE to a PDF file at FILEPATH with proper headers/footers."
  (let* ((ps2pdf (uiop:run-program (list "ps2pdf" "-" filepath) :input :stream :output nil))
         (ps (uiop:process-info-input ps2pdf)))
    (write-ps-font-encodings ps)
    (write-ps-docinfo ps resource)
    (resource-to-postscript resource ps)
    filepath))

(defun export-resource-to-text-file (resource filepath)
  "Export RESOURCE to plain text file at FILEPATH."
  (with-open-file (out filepath :direction :output :if-exists :supersede :external-format :utf-8)
    (resource-to-text resource out))
  filepath)

;; 
;; Inspector/Preview Window Infrastructure
;; 

(clim:define-application-frame resource-preview (gui-inspector-frame clim:standard-application-frame)
  ()
  (:panes
   (content :application
            :display-function 'display-resource-preview
            :scroll-bars :vertical
            :height 600 :width 800)
   (status-bar :application
               :display-function 'display-resource-status
               :height 30 :width 800))
  (:layouts
   (default (clim:vertically () content status-bar)))
  (:menu-bar resource-preview-menu-bar))

(clim:define-command-table resource-preview-menu-bar
  :menu (("File" :menu resource-preview-file-menu)
         ("View" :menu resource-preview-view-menu)
         ("Help" :menu resource-preview-help-menu)))

(clim:define-command-table resource-preview-file-menu
  :menu (("Print..." :command com-preview-print)
         ("Export to PDF..." :command com-preview-export-pdf)
         ("Export to Text..." :command com-preview-export-text)
         (nil :divider :line)
         ("Close" :command com-preview-close)))

(clim:define-command-table resource-preview-view-menu
  :menu (("Reference View" :command com-preview-view-reference)
         ("Reading View" :command com-preview-view-reading)))

(clim:define-command-table resource-preview-help-menu
  :menu (("About Skyline-Tool..." :command com-help-about)))

(defun display-resource-preview (frame pane)
  (let ((resource (inspector-resource frame)))
    (when resource
      (let ((*standard-output* pane))
        (ecase (frame-view-mode frame)
          (:reference (present-reference resource pane))
          (:reading   (present-reading resource pane))
          (:editing   (present-editing resource pane)))))))

(defun display-resource-status (frame pane)
  (let ((resource (inspector-resource frame)))
    (when resource
      (let ((*standard-output* pane))
        (format pane " ~a | ~a | View: ~a"
                (game-resource-kind resource)
                (game-resource-title resource)
                (string-downcase (symbol-name (frame-view-mode frame))))))))

(clim:define-command (com-preview-close :command-table resource-preview-menu-bar
                                        :menu t :name t) ()
  (clim:frame-exit clim:*application-frame*))

(clim:define-command (com-preview-view-reference :command-table resource-preview-menu-bar
                                                 :menu t :name t) ()
  (setf (frame-view-mode clim:*application-frame*) :reference)
  (clim:redisplay-frame-panes clim:*application-frame* :force-p t))

(clim:define-command (com-preview-view-reading :command-table resource-preview-menu-bar
                                               :menu t :name t) ()
  (setf (frame-view-mode clim:*application-frame*) :reading)
  (clim:redisplay-frame-panes clim:*application-frame* :force-p t))

(clim:define-command (com-preview-export-pdf :command-table resource-preview-menu-bar
                                             :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (resource (frame-resource frame)))
    (when resource
      (let ((filepath (prompt-save-pathname (format nil "~a.pdf" (game-resource-title resource))
                                            (list :dir (game-resource-kind resource) :pdf))))
        (when filepath
          (export-resource-to-pdf-file resource filepath))))))

(clim:define-command (com-preview-export-text :command-table resource-preview-menu-bar
                                              :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (resource (frame-resource frame)))
    (when resource
      (let ((filepath (prompt-save-pathname (format nil "~a.txt" (game-resource-title resource))
                                            (list :dir (game-resource-kind resource) :text))))
        (when filepath
          (export-resource-to-text-file resource filepath))))))

(clim:define-command (com-preview-print :command-table resource-preview-menu-bar
                                        :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (resource (frame-resource frame))
         (printer (cdar *ipp-printer-registry*)))
    (when resource
      (if printer
          (pipe-to-ipp printer resource)
          (pipe-to-lpr nil resource)))))

;; Helper: pipe PostScript directly to lpr via stdin
(defun write-resource-postscript-to-stream (resource ps-stream)
  (write-ps-font-encodings ps-stream)
  (write-ps-docinfo ps-stream resource)
  (format ps-stream "~%%% Begin resource content~%")
  (write-resource-ps-content resource ps-stream)
  (format ps-stream "~%%% End resource content~%"))

(defun pipe-to-lpr (printer resource)
  "Pipe RESOURCE's PostScript to lpr command via stdin stream.
PRINTER can be an ipp-printer struct (uses ipp-name for -d), a queue name string, or nil for system default."
  (let* ((args (cond
                ((typep printer 'ipp-printer) (list "lpr" "-d" (ipp-queue printer)))
                ((stringp printer) (list "lpr" "-d" printer))
                (t (list "lpr"))))
         (ps-proc (uiop:run-program args :input :stream))
         (ps-stream (uiop:process-info-input ps-proc)))
    (write-resource-postscript-to-stream resource ps-stream)
    (close ps-stream)
    (loop while (uiop:process-alive-p ps-proc)
          do (sleep 2))))

(defun pipe-to-ipp (printer resource)
  "Pipe RESOURCE's PostScript directly to IPP server via Drakma.
PRINTER must be an ipp-printer struct. Content streamed via Drakma's callback interface."
  (let ((url (format nil "http://~a:~d/printers/~a"
                     (ipp-host printer) (ipp-port printer) (ipp-queue printer))))
    (drakma:http-request url
                         :method :post
                         :content (lambda (stream)
                                    (write-resource-postscript-to-stream resource stream))
                         :content-type "application/postscript"
                         :user-agent (format nil "Skyline-Tool/~a; Drakma/~a (~a/~a; ~a/~a)"
                                             (asdf:component-version (asdf:find-system :skyline-tool))
                                             (asdf:component-version (asdf:find-system :drakma))
                                             (software-type) (software-version)
                                             (machine-type) (machine-version)))))

(clim:define-command (com-help-about :command-table resource-preview-menu-bar
                                     :menu t :name t) ()
  (show-about-skyline-tool))

;; 
;; JSON Export/Import Infrastructure
;; 
;; Common PostScript Content Generation
;; 

(defgeneric write-resource-ps-content (resource ps)
  (:documentation "Write the main content of RESOURCE to PostScript stream PS."))

(defmethod resource-to-postscript ((resource game-resource) ps)
  (write-ps-docinfo ps resource)
  (write-ps-font-encodings ps)
  (format ps "~%%% Begin resource content~%")
  (write-resource-ps-content resource ps)
  (format ps "~%%% End resource content~%"))

(defgeneric write-resource-text-content (resource stream)
  (:documentation "Write human-readable text representation of RESOURCE to STREAM."))

(defmethod resource-to-text ((resource game-resource) stream)
  (write-resource-text-content resource stream)
  (terpri stream))

;; 
;; Standardized Resource Reference Presentation
;; 
;; 
;; Layout:
;;   {Icon} | Title (red if missing from builds) | Locator
;;   | Subheading (75% gray, smaller) | Build checkboxes for assets
;;   | VERSION-CONTROL/issue indicators bottom-right
;; 

(defun present-resource-reference (stream resource)
  "Present RESOURCE in the standardized reference format.
Used as the default display for all resource types in inspectors and listings."
  (clim:with-output-as-presentation (stream resource 'game-resource-reference)
    (clim:formatting-table (stream)
      (clim:formatting-row (stream)
        ;; Icon column
        (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 0)
          (format stream "~3%"))
        (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 48)
          (ignore-errors
           (game-resource-present-icon resource stream)))
        ;; Title and subheading column
        (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 300)
          ;; Title - red if missing from builds
          (let ((ink (if (and (typep resource 'game-resource-asset)
                              (zerop (game-resource-builds resource)))
                         +missing-red+
                         clim:+black+)))
            (clim:with-drawing-options (stream :ink ink)
              (clim:with-text-size (stream :larger)
                (clim:with-text-face (stream :bold)
                  (game-resource-present-title resource stream))))
            (format stream "~%~5t")
            ;; Subheading in 75% gray
            (clim:with-drawing-options (stream :ink +dark-gray+)
              (clim:with-text-size (stream :smaller)
                (game-resource-present-subheading resource stream)))
            ;; Build checkboxes for assets
            (when (typep resource 'game-resource-asset)
              (format stream "~%~5t")
              (clim:with-text-size (stream :smaller)
                (format stream "[~a] Build"
                        (if (plusp (game-resource-builds resource))
                            "✓" " ")))))
          ;; Right margin: locator / VERSION-CONTROL status / issue indicators
          (clim:formatting-cell (stream :align-x :right :align-y :top :min-height 90 :min-width 125)
            (game-resource-present-right-margin resource stream)))))))

;; 
;; Standard presentation generics
;; 

(defgeneric game-resource-present-icon (resource stream)
  (:documentation "Present an icon glyph for RESOURCE on STREAM."))

(defgeneric game-resource-present-title (resource stream)
  (:documentation "Present the title text for RESOURCE on STREAM."))

(defgeneric game-resource-present-subheading (resource stream)
  (:documentation "Present the subheading line for RESOURCE on STREAM."))

(defgeneric game-resource-present-right-margin (resource stream)
  (:documentation "Present right-margin info (locator, VERSION-CONTROL, issues) for RESOURCE on STREAM."))

(defmethod game-resource-present-icon ((resource game-resource) stream)
  "Default icon - shows a bullet character."
  (format stream "•"))

(defmethod game-resource-present-title ((resource game-resource) stream)
  "Default title display."
  (format stream "~a" (game-resource-title resource)))

(defmethod game-resource-present-subheading ((resource game-resource) stream)
  "Default subheading: shows resource kind."
  (format stream "~a" (game-resource-kind resource)))

(defmethod game-resource-present-right-margin ((resource game-resource) stream)
  "Default right margin: shows locator and VERSION-CONTROL status."
  (let ((locator (ignore-errors (game-resource-locator resource)))
        (version-control-status (ignore-errors
                                 (version-control-file-status (or (game-resource-full-path resource)
                                                                  (game-resource-collective-path resource))))))
    (when locator
      (clim:with-drawing-options (stream :ink clim:+black+)
        (clim:with-text-face (stream :roman)
          (clim:with-text-size (stream :smaller)
            (format stream "~a" locator))))
      (format stream "~%"))
    (when version-control-status
      (clim:with-drawing-options (stream :ink +dark-gray+)
        (clim:with-text-size (stream :smaller)
          (format stream "VERSION-CONTROL: ~a" version-control-status))))))

;; 
;; Helper Functions for Common Fields
;; 

(defun write-resource-common-ps (resource ps)
  "Write common resource fields to PS stream."
  (let ((title (game-resource-title resource))
        (kind (game-resource-kind resource))
        (version-control-status (version-control-file-status (or (game-resource-full-path resource)
                                                                 (game-resource-collective-path resource)))))
    (format ps "/Times-Roman-ISOLatin1 findfont 14 scalefont setfont~%")
    (format ps "56 600 moveto~%")
    (format ps "0.0 0.0 0.0 setrgbcolor~%")
    (format ps "(~a) show~%" (escape-ps-string title))
    (format ps "56 580 moveto~%")
    (format ps "(Kind: ~a) show~%" (escape-ps-string kind))
    (when version-control-status
      (format ps "56 540 moveto~%")
      (format ps "(VERSION-CONTROL Status: ~a) show~%" (escape-ps-string version-control-status)))))

(defun write-resource-common-text (resource stream)
  "Write common resource fields to text stream."
  (format stream "Title: ~a~%" (game-resource-title resource))
  (format stream "Kind: ~a~%" (game-resource-kind resource))
  (let ((version-control-status
          (version-control-file-status (or (game-resource-full-path resource)
                                           (game-resource-collective-path resource)))))
    (when version-control-status
      (format stream "Version Control Status: ~a~%" version-control-status))))

;; 
;; resource-to-json methods for all concrete resource classes
;; 

(defun encode-file-to-base91 (path)
  "Read file at PATH and return base91-encoded string, or NIL if file not found.
Base91 provides ~23% overhead vs base64's 33%."
  (when (and path (probe-file path))
    (with-open-file (stream path :element-type '(unsigned-byte 8))
      (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
        (read-sequence bytes stream)
        (encode-base91 bytes)))))

(defmethod resource-to-json ((resource game-resource))
   (let ((obj (make-hash-table :test 'equal)))
     (setf (gethash "name" obj) (game-resource-title resource))
     (setf (gethash "kind" obj) (game-resource-kind resource))
     (setf (gethash "language" obj) (game-resource-language resource))
     (when (typep resource 'game-resource-asset)
       (setf (gethash "moniker" obj) (game-asset-moniker resource)))
     (when (typep resource 'game-resource-from-file)
       (let ((path (game-resource-full-path resource)))
         (setf (gethash "path" obj) (namestring path))
         ;; Include file contents as base91 for file-based resources
         (let ((content (encode-file-to-base91 path)))
           (when content
             (setf (gethash "content" obj)
                   (list :encoding "base91"
                         :size (file-length (game-resource-full-path resource))
                         :data content))))))
     (when (typep resource 'game-resource-asset)
       (setf (gethash "assetId" obj) (game-resource-asset-id resource))
       (setf (gethash "builds" obj) (game-asset-builds resource)))
     (wrap-with-headers obj (class-name (class-of resource)))))

(defmethod resource-to-json ((resource game-resource-map))
  (let ((obj (call-next-method)))
    (setf (gethash "locale" obj) (game-resource-locale resource))
    obj))

(defmethod resource-to-json ((resource game-resource-script))
  (let ((obj (call-next-method)))
    (setf (gethash "locale" obj) (game-resource-locale resource))
    obj))

(defmethod resource-to-json ((resource game-resource-song))
  (call-next-method))

(defmethod resource-to-json ((resource game-resource-boat))
  (call-next-method))

(defmethod resource-to-json ((resource game-resource-instrument))
  (call-next-method))

(defmethod resource-to-json ((resource game-resource-item))
  (call-next-method))

(defmethod resource-to-json ((resource game-resource-flag))
  (call-next-method))

(defmethod resource-to-json ((resource game-resource-key))
  (call-next-method))

(defmethod resource-to-json ((resource game-resource-object-prototype))
  (call-next-method))

(defmethod resource-to-json ((resource game-resource-character))
  (call-next-method))

(defmethod resource-to-json ((resource game-resource-translation))
  (call-next-method))

(defmethod resource-to-json ((resource game-resource-atari-vox-dictionary))
  (call-next-method))

(defmethod resource-to-json ((resource game-resource-intellivoice-dictionary))
  (call-next-method))

(defmethod resource-to-json ((resource game-resource-phrasebook))
  (call-next-method))

(defmethod resource-to-json ((resource game-resource-class))
  (call-next-method))

(defmethod resource-to-json ((resource game-resource-tileset))
  (call-next-method))

(defmethod resource-to-json ((resource game-resource-sprite-sheet))
  (call-next-method))

(defmethod resource-to-json ((resource game-resource-routine))
  (call-next-method))

(defmethod resource-to-json ((resource game-resource-routine-forth-library))
  (call-next-method))

(defmethod resource-to-json ((resource game-resource-routine-run-commands))
  (call-next-method))

;; 
;; resource-from-json methods for all concrete resource classes
;; 

(defmethod resource-from-json (json (class (eql 'game-resource)))
   (make-instance class
                  :name (gethash "name" json)
                  :kind (ignore-errors (kind-by-name (gethash "kind" json)))
                  :language (gethash "language" json))) 

(defmethod resource-from-json (json (class (eql 'game-resource-map)))
  (error "unimplemented")
  (make-instance 'game-resource-map
                 :moniker (gethash "moniker" json)))

(defmethod resource-from-json (json (class (eql 'game-resource-script)))
  (error "unimplemented")
  (make-instance 'game-resource-script
                 :moniker (gethash "moniker" json)))

(defmethod resource-from-json (json (class (eql 'game-resource-song)))
  (error "unimplemented")
  (make-instance 'game-resource-song
                 :moniker (gethash "moniker" json)))

(defmethod resource-from-json (json (class (eql 'game-resource-boat)))
  (error "unimplemented")
  (make-instance 'game-resource-boat))

(defmethod resource-from-json (json (class (eql 'game-resource-instrument)))
  (error "unimplemented")
  (make-instance 'game-resource-instrument))

(defmethod resource-from-json (json (class (eql 'game-resource-item)))
  (make-instance 'game-resource-item))

(defmethod resource-from-json (json (class (eql 'game-resource-flag)))
  (error "unimplemented")
  (make-instance 'game-resource-flag))

(defmethod resource-from-json (json (class (eql 'game-resource-key)))
  (error "unimplemented")
  (make-instance 'game-resource-key))

(defmethod resource-from-json (json (class (eql 'game-resource-object-prototype)))
  (error "unimplemented")
  (make-instance 'game-resource-object-prototype))

(defmethod resource-from-json (json (class (eql 'game-resource-character)))
  (error "unimplemented")
  (make-instance 'game-resource-character))

(defmethod resource-from-json (json (class (eql 'game-resource-translation)))
  (error "unimplemented")
  (make-instance 'game-resource-translation))

(defmethod resource-from-json (json (class (eql 'game-resource-atari-vox-dictionary)))
  (error "unimplemented")
  (make-instance 'game-resource-atari-vox-dictionary))

(defmethod resource-from-json (json (class (eql 'game-resource-intellivoice-dictionary)))
  (error "unimplemented")
  (make-instance 'game-resource-intellivoice-dictionary))

(defmethod resource-from-json (json (class (eql 'game-resource-phrasebook)))
  (error "unimplemented")
  (make-instance 'game-resource-phrasebook))

(defmethod resource-from-json (json (class (eql 'game-resource-class)))
  (error "unimplemented")
  (make-instance 'game-resource-class))

(defmethod resource-from-json (json (class (eql 'game-resource-tileset)))
  (error "unimplemented")
  (make-instance 'game-resource-tileset))

(defmethod resource-from-json (json (class (eql 'game-resource-sprite-sheet)))
  (error "unimplemented")
  (make-instance 'game-resource-sprite-sheet))

(defmethod resource-from-json (json (class (eql 'game-resource-routine)))
  (error "unimplemented")
  (make-instance 'game-resource-routine))

(defmethod resource-from-json (json (class (eql 'game-resource-routine-forth-library)))
  (error "unimplemented")
  (make-instance 'game-resource-routine-forth-library))

(defmethod resource-from-json (json (class (eql 'game-resource-routine-run-commands)))
  (error "unimplemented")
  (make-instance 'game-resource-routine-run-commands))

;; 
;; write-resource-ps-content methods for all concrete resource classes
;; 

(defmethod write-resource-ps-content ((resource game-resource-map) ps)
  (write-resource-common-ps resource ps)
  (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont~%")
  (format ps "56 560 moveto~%")
  (format ps "(Moniker: ~a) show~%" (escape-ps-string (game-asset-moniker resource)))
  (format ps "56 545 moveto~%")
  (format ps "(Locale: ~a) show~%" (escape-ps-string (or (game-resource-locale resource) "N/A"))))

(defmethod write-resource-ps-content ((resource game-resource-script) ps)
  (write-resource-common-ps resource ps)
  (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont~%")
  (format ps "56 560 moveto~%")
  (format ps "(Moniker: ~a) show~%" (escape-ps-string (game-asset-moniker resource)))
  (format ps "56 545 moveto~%")
  (format ps "(Locale: ~a) show~%" (escape-ps-string (or (game-resource-locale resource) "N/A"))))

(defmethod write-resource-ps-content ((resource game-resource-song) ps)
  (write-resource-common-ps resource ps)
  (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont~%")
  (format ps "56 560 moveto~%")
  (format ps "(Moniker: ~a) show~%" (escape-ps-string (game-asset-moniker resource))))

(defmethod write-resource-ps-content ((resource game-resource-blob) ps)
  (write-resource-common-ps resource ps)
  (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont~%")
  (format ps "56 560 moveto~%")
  (format ps "(BLOB: ~a) show~%" (escape-ps-string (game-asset-moniker resource)))
  (format ps "showpage~%")
  ;; FIXME: missing page with palette swatches
  ;; Landscape page with image centered
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
             (image (png->palette (png-read:image-data png-data)
                                  (png-read:transparency png-data))))
        (dotimes (y (array-dimension image 1))
          (dotimes (x (array-dimension image 0))
            (let* ((color (aref image x y)))
              (destructuring-bind (r g b) (palette->rgb color)
                (format ps "~d ~d ~d setrgbcolor~%" (/ r 255.0) (/ g 255.0) (/ b 255.0))
                (format ps "56 ~d 8 8 rectfill~%" y)))))))
    ;; FIXME: Incorrect footer contents
    (format ps "showpage~%")
    (format ps "0 0 moveto~%")
    (format ps "/Times-Roman findfont 8 scalefont setfont~%")
    (format ps "(Skyline-Tool for Phantasia 7800 | ~a | Page ~d) show~%"
            (escape-ps-string (game-asset-moniker resource)) 1)))

(defmethod write-resource-ps-content ((resource game-resource-boat) ps)
  (write-resource-common-ps resource ps)
  (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont~%")
  (format ps "56 560 moveto~%")
  (format ps "(Locator: ~a) show~%" (escape-ps-string (game-resource-locator resource))))

(defmethod write-resource-ps-content ((resource game-resource-instrument) ps)
  (write-resource-common-ps resource ps)
  (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont~%")
  (format ps "56 560 moveto~%")
  (format ps "(Locator: ~a) show~%" (escape-ps-string (game-resource-locator resource))))

(defmethod write-resource-ps-content ((resource game-resource-item) ps)
  (write-resource-common-ps resource ps)
  (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont~%")
  (format ps "56 560 moveto~%")
  (format ps "(Locator: ~a) show~%" (escape-ps-string (game-resource-locator resource))))

;; Image display utilities
(defun png->image (png)
  (let* ((raw-data (png-read:image-data png))
         (width (array-dimension raw-data 0))
         (height (array-dimension raw-data 1))
         (palette-indices (png->palette raw-data (png-read:transparency png)))
         (machine-pal (machine-palette)))
    (make-array (list width height 3) :element-type '(unsigned-byte 8)
                :initial-contents
                (loop for x below width collect
                  (loop for y below height collect
                    (destructuring-bind (r g b)
                        (let ((idx (aref palette-indices x y)))
                          (if idx (nth idx machine-pal) '(0 0 0)))
                      (list r g b)))))))

(defun clim-image (stream rgb-array &key fit-to-width)
  (let* ((width (array-dimension rgb-array 0))
         (height (array-dimension rgb-array 1))
         (scale (if fit-to-width (/ fit-to-width (max 1 width)) 1)))
    (dotimes (y height)
      (dotimes (x width)
        (let* ((r (aref rgb-array x y 0))
               (g (aref rgb-array x y 1))
               (b (aref rgb-array x y 2)))
          (clim:draw-rectangle* stream
            (* x scale) (* y scale)
            (* (1+ x) scale) (* (1+ y) scale)
            :ink (clim:make-rgb-color (/ r 255.0) (/ g 255.0) (/ b 255.0))
            :filled t))))))

(defmethod write-resource-ps-content ((resource game-resource-flag) ps)
  (write-resource-common-ps resource ps)
  (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont~%")
  (format ps "56 560 moveto~%")
  (format ps "(Locator: ~a) show~%" (escape-ps-string (game-resource-locator resource))))

(defmethod write-resource-ps-content ((resource game-resource-key) ps)
  (write-resource-common-ps resource ps)
  (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont~%")
  (format ps "56 560 moveto~%")
  (format ps "(Locator: ~a) show~%" (escape-ps-string (game-resource-locator resource))))

(defmethod write-resource-ps-content ((resource game-resource-object-prototype) ps)
  (write-resource-common-ps resource ps)
  (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont~%")
  (format ps "56 560 moveto~%")
  (format ps "(Locator: ~a) show~%" (escape-ps-string (game-resource-locator resource))))

(defmethod write-resource-ps-content ((resource game-resource-character) ps)
  (write-resource-common-ps resource ps)
  (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont~%")
  (format ps "56 560 moveto~%")
  (format ps "(Locator: ~a) show~%" (escape-ps-string (game-resource-locator resource))))

(defmethod write-resource-ps-content ((resource game-resource-translation) ps)
  (write-resource-common-ps resource ps)
  (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont~%")
  (format ps "56 560 moveto~%")
  (format ps "(Locator: ~a) show~%" (escape-ps-string (game-resource-locator resource))))

(defmethod write-resource-ps-content ((resource game-resource-atari-vox-dictionary) ps)
  (write-resource-common-ps resource ps)
  (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont~%")
  (format ps "56 560 moveto~%")
  (format ps "(Locator: ~a) show~%" (escape-ps-string (game-resource-locator resource))))

(defmethod write-resource-ps-content ((resource game-resource-intellivoice-dictionary) ps)
  (write-resource-common-ps resource ps)
  (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont~%")
  (format ps "56 560 moveto~%")
  (format ps "(Locator: ~a) show~%" (escape-ps-string (game-resource-locator resource))))

(defmethod write-resource-ps-content ((resource game-resource-phrasebook) ps)
  (write-resource-common-ps resource ps)
  (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont~%")
  (format ps "56 560 moveto~%")
  (format ps "(Locator: ~a) show~%" (escape-ps-string (game-resource-locator resource))))

(defmethod write-resource-ps-content ((resource game-resource-class) ps)
  (write-resource-common-ps resource ps)
  (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont~%")
  (format ps "56 560 moveto~%")
  (format ps "(Locator: ~a) show~%" (escape-ps-string (game-resource-locator resource))))

(defmethod write-resource-ps-content ((resource game-resource-tileset) ps)
  (write-resource-common-ps resource ps)
  (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont~%")
  (format ps "56 560 moveto~%")
  (format ps "(Locator: ~a) show~%" (escape-ps-string (game-resource-locator resource))))

(defmethod write-resource-ps-content ((resource game-resource-sprite-sheet) ps)
  (write-resource-common-ps resource ps)
  (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont~%")
  (format ps "56 560 moveto~%")
  (format ps "(Locator: ~a) show~%" (escape-ps-string (game-resource-locator resource))))

(defmethod write-resource-ps-content ((resource game-resource-routine) ps)
  (write-resource-common-ps resource ps)
  (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont~%")
  (format ps "56 560 moveto~%")
  (format ps "(Locator: ~a) show~%" (escape-ps-string (game-resource-locator resource))))

(defmethod write-resource-ps-content ((resource game-resource-routine-forth-library) ps)
  (write-resource-common-ps resource ps)
  (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont~%")
  (format ps "56 560 moveto~%")
  (format ps "(Locator: ~a) show~%" (escape-ps-string (game-resource-locator resource))))

(defmethod write-resource-ps-content ((resource game-resource-routine-run-commands) ps)
  (write-resource-common-ps resource ps)
  (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont~%")
  (format ps "56 560 moveto~%")
  (format ps "(Locator: ~a) show~%" (escape-ps-string (game-resource-locator resource))))

