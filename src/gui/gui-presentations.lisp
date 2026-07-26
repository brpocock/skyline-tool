;;; Skyline-Tool src/gui/gui-presentations.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC
;;; Common presentation infrastructure for all game-resource types

(in-package :skyline-tool)

;; 
;; Common Presentation Infrastructure
;; 

(defun validate-json-version (json expected-version)
  "Validate JSON version against expected version, erroring if incompatible."
  (let ((tool-version (gethash "skyline-tool" json)))
    (unless (clojure->number= tool-version expected-version)
      (error "Incompatible JSON version ~a (expected ~a). Resource import aborted."
             tool-version expected-version))))

(defgeneric resource-to-json (resource)
  (:documentation "Convert RESOURCE to JSON-compatible object for export."))

(defgeneric resource-from-json (json class)
  (:documentation "Create a RESOURCE instance of CLASS from JSON object."))

(defgeneric resource-to-postscript (resource stream &key title author)
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
    (validate-json-version json 0.6)
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

(defun export-resource-to-ps-file (resource filepath
                                   &key title (author (user-real-name)) (last-updated (local-time:now)))
  "Export RESOURCE to PostScript file at FILEPATH with proper headers/footers."
  (let* ((ps2pdf (uiop:run-program (list "ps2pdf" "-" filepath) :input :stream :output nil))
         (ps (uiop:process-info-input ps2pdf)))
    (write-ps-font-encodings ps)
    (write-ps-docinfo ps title author (user-homedir-pathname))
    (resource-to-postscript resource ps :title title :author author :last-updated last-updated)
    (write-ps-page-footer ps 1 1 title
                          (format-timestring nil last-updated :format '(:year "-" :month "-" :day " " :hour ":" :min))
                          author
                          (machine-instance)))
  filepath)

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
  :menu (("About Skyline-Tool..." :command com-about-skyline-tool)))

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
         (resource (inspector-resource frame)))
    (when resource
      (let ((filepath (error "Gnome Save As dialog must be used here")))
        (when filepath
          (export-resource-to-ps-file resource filepath
                                      :title (game-resource-title resource)
                                      :author (user-homedir-pathname)))))))

(clim:define-command (com-preview-export-text :command-table resource-preview-menu-bar
                                              :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (resource (inspector-resource frame)))
    (when resource
      (let ((filepath (error "Gnome Save as window must be used here")))
        (when filepath
          (export-resource-to-text-file resource filepath))))))

(clim:define-command (com-preview-print :command-table resource-preview-menu-bar
                                        :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (resource (inspector-resource frame)))
    (when resource
      (let* ((ps-filepath (merge-pathnames
                           (format nil "/tmp/~a-print.ps" (game-resource-title resource))
                           (uiop:getcwd)))
             (pdf-filepath (merge-pathnames
                            (format nil "/tmp/~a-print.pdf" (game-resource-title resource))
                            (uiop:getcwd))))
        (export-resource-to-ps-file resource ps-filepath
                                    :title (game-resource-title resource)
                                    :author (user-homedir-pathname))
        (uiop:run-program (list "ps2pdf" ps-filepath pdf-filepath) :output nil)
        (uiop:run-program (list "lp" pdf-filepath) :output nil)))))


;; 
;; JSON Export/Import Infrastructure
;; 
;; Common PostScript Content Generation
;; 

(defgeneric write-resource-ps-content (resource ps)
  (:documentation "Write the main content of RESOURCE to PostScript stream PS."))

(defmethod resource-to-postscript ((resource game-resource) ps &key title author last-updated)
  (declare (ignore author))
  (write-ps-docinfo ps title "Skyline-Tool" (user-homedir-pathname))
  (write-ps-font-encodings ps)
  (format ps "~%%% Begin resource content~%")
  (write-resource-ps-content resource ps)
  (format ps "~%%% End resource content~%")
  (write-ps-page-footer ps 1 1 title
                        (format-timestring nil last-updated
                                           :format '(:year "-" :month "-" :day " " :hour ":" :min))
                        (user-real-name)
                        (machine-instance)
                        1 1))

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
;;   | VC/issue indicators bottom-right
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
          ;; Right margin: locator / VC status / issue indicators
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
  (:documentation "Present right-margin info (locator, VC, issues) for RESOURCE on STREAM."))

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
  "Default right margin: shows locator and VC status."
  (let ((locator (ignore-errors (game-resource-locator resource)))
        (vc-status (ignore-errors
                    (vc-file-status (or (game-resource-full-path resource)
                                        (game-resource-collective-path resource))))))
    (when locator
      (clim:with-drawing-options (stream :ink clim:+black+)
        (clim:with-text-face (stream :roman)
          (clim:with-text-size (stream :smaller)
            (format stream "~a" locator))))
      (format stream "~%"))
    (when vc-status
      (clim:with-drawing-options (stream :ink +dark-gray+)
        (clim:with-text-size (stream :smaller)
          (format stream "VC: ~a" vc-status))))))

;; 
;; Helper Functions for Common Fields
;; 

(defun write-resource-common-ps (resource ps)
  "Write common resource fields to PS stream."
  (let ((title (game-resource-title resource))
        (kind (game-resource-kind resource))
        (vc-status (vc-file-status (or (game-resource-full-path resource)
                                       (game-resource-collective-path resource)))))
    (format ps "/Times-Roman-ISOLatin1 findfont 14 scalefont setfont~%")
    (format ps "56 600 moveto~%")
    (format ps "0.0 0.0 0.0 setrgbcolor~%")
    (format ps "(~a) show~%" (escape-ps-string title))
    (format ps "56 580 moveto~%")
    (format ps "(Kind: ~a) show~%" (escape-ps-string kind))
    (when vc-status
      (format ps "56 540 moveto~%")
      (format ps "(VC Status: ~a) show~%" (escape-ps-string vc-status)))))

(defun write-resource-common-text (resource stream)
  "Write common resource fields to text stream."
  (format stream "Title: ~a~%" (game-resource-title resource))
  (format stream "Kind: ~a~%" (game-resource-kind resource))
  (let ((vc-status (vc-file-status (or (game-resource-full-path resource)
                                       (game-resource-collective-path resource)))))
    (when vc-status
      (format stream "VC Status: ~a~%" vc-status))))

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
      (setf (gethash "builds" obj) (game-resource-builds resource)))
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

(defmethod resource-to-json ((resource game-resource-blob))
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

(defmethod resource-from-json (json (class (eql 'game-resource-map)))
  (make-instance 'game-resource-map
                 :moniker (gethash "moniker" json)
                 :kind (or (ignore-errors (kind-by-name (gethash "kind" json)))
                           (gethash "kind" json))
                 :full-path (gethash "path" json)))

(defmethod resource-from-json (json (class (eql 'game-resource-script)))
  (make-instance 'game-resource-script
                 :moniker (gethash "moniker" json)
                 :kind (or (ignore-errors (kind-by-name (gethash "kind" json)))
                           (gethash "kind" json))
                 :full-path (gethash "path" json)))

(defmethod resource-from-json (json (class (eql 'game-resource-song)))
  (make-instance 'game-resource-song
                 :moniker (gethash "moniker" json)
                 :kind (or (ignore-errors (kind-by-name (gethash "kind" json)))
                           (gethash "kind" json))
                 :full-path (gethash "path" json)))

(defmethod resource-from-json (json (class (eql 'game-resource-blob)))
  (make-instance 'game-resource-blob
                 :moniker (gethash "moniker" json)
                 :kind (or (ignore-errors (kind-by-name (gethash "kind" json)))
                           (gethash "kind" json))
                 :full-path (gethash "path" json)))

(defmethod resource-from-json (json (class (eql 'game-resource-boat)))
  (make-instance 'game-resource-boat
                 :kind (gethash "kind" json)))

(defmethod resource-from-json (json (class (eql 'game-resource-instrument)))
  (make-instance 'game-resource-instrument
                 :kind (gethash "kind" json)))

(defmethod resource-from-json (json (class (eql 'game-resource-item)))
  (make-instance 'game-resource-item
                 :kind (gethash "kind" json)))

(defmethod resource-from-json (json (class (eql 'game-resource-flag)))
  (make-instance 'game-resource-flag
                 :kind (gethash "kind" json)))

(defmethod resource-from-json (json (class (eql 'game-resource-key)))
  (make-instance 'game-resource-key
                 :kind (gethash "kind" json)))

(defmethod resource-from-json (json (class (eql 'game-resource-object-prototype)))
  (make-instance 'game-resource-object-prototype
                 :kind (or (ignore-errors (kind-by-name (gethash "kind" json)))
                           (gethash "kind" json))
                 :full-path (gethash "path" json)))

(defmethod resource-from-json (json (class (eql 'game-resource-character)))
  (make-instance 'game-resource-character
                 :kind (or (ignore-errors (kind-by-name (gethash "kind" json)))
                           (gethash "kind" json))
                 :full-path (gethash "path" json)))

(defmethod resource-from-json (json (class (eql 'game-resource-translation)))
  (make-instance 'game-resource-translation
                 :kind (or (ignore-errors (kind-by-name (gethash "kind" json)))
                           (gethash "kind" json))
                 :full-path (gethash "path" json)))

(defmethod resource-from-json (json (class (eql 'game-resource-atari-vox-dictionary)))
  (make-instance 'game-resource-atari-vox-dictionary
                 :kind (or (ignore-errors (kind-by-name (gethash "kind" json)))
                           (gethash "kind" json))
                 :full-path (gethash "path" json)))

(defmethod resource-from-json (json (class (eql 'game-resource-intellivoice-dictionary)))
  (make-instance 'game-resource-intellivoice-dictionary
                 :kind (or (ignore-errors (kind-by-name (gethash "kind" json)))
                           (gethash "kind" json))
                 :full-path (gethash "path" json)))

(defmethod resource-from-json (json (class (eql 'game-resource-phrasebook)))
  (make-instance 'game-resource-phrasebook
                 :kind (or (ignore-errors (kind-by-name (gethash "kind" json)))
                           (gethash "kind" json))
                 :full-path (gethash "path" json)))

(defmethod resource-from-json (json (class (eql 'game-resource-class)))
  (make-instance 'game-resource-class
                 :kind (or (ignore-errors (kind-by-name (gethash "kind" json)))
                           (gethash "kind" json))
                 :full-path (gethash "path" json)))

(defmethod resource-from-json (json (class (eql 'game-resource-tileset)))
  (make-instance 'game-resource-tileset
                 :kind (or (ignore-errors (kind-by-name (gethash "kind" json)))
                           (gethash "kind" json))
                 :full-path (gethash "path" json)))

(defmethod resource-from-json (json (class (eql 'game-resource-sprite-sheet)))
  (make-instance 'game-resource-sprite-sheet
                 :kind (or (ignore-errors (kind-by-name (gethash "kind" json)))
                           (gethash "kind" json))
                 :full-path (gethash "path" json)))

(defmethod resource-from-json (json (class (eql 'game-resource-routine)))
  (make-instance 'game-resource-routine
                 :kind (or (ignore-errors (kind-by-name (gethash "kind" json)))
                           (gethash "kind" json))
                 :full-path (gethash "path" json)))

(defmethod resource-from-json (json (class (eql 'game-resource-routine-forth-library)))
  (make-instance 'game-resource-routine-forth-library
                 :kind (or (ignore-errors (kind-by-name (gethash "kind" json)))
                           (gethash "kind" json))
                 :full-path (gethash "path" json)))

(defmethod resource-from-json (json (class (eql 'game-resource-routine-run-commands)))
  (make-instance 'game-resource-routine-run-commands
                 :kind (or (ignore-errors (kind-by-name (gethash "kind" json)))
                           (gethash "kind" json))
                 :full-path (gethash "path" json)))

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
  ;; Landscape page with palette colors
  (format ps "<< /PageSize [792 612] >> setpagedevice~%")
  (format ps "56 480 moveto~%")
  (format ps "/Helvetica-Bold findfont 14 scalefont setfont~%")
  (format ps "(Palette Colors) show~%")
  (format ps "56 460 moveto~%")
  (format ps "/Helvetica findfont 10 scalefont setfont~%")
  (let* ((xcf-path (game-resource-full-path resource))
         (png-path (make-output-path resource :png)))
    (unless (probe-file png-path)
      (build-target png-path))
    (when (probe-file png-path)
      (let* ((png-data (error "unimplemented"))
             (palette (png->palette (png-read:image-data png-data)
                                    (png-read:transparency png-data)))
             (num-colors (min 16 (array-dimension palette 0))))
        (dotimes (i num-colors)
          (let* ((color (aref palette i))
                 (r (nth 0 color))
                 (g (nth 1 color))
                 (b (nth 2 color)))
            (format ps "~d ~d moveto~%" 56 (+ 440 (* i 10)))
            (format ps "[##] P~dC~d: (~a) ~$~d~%" i i
                    (format-color-name r g b) r))))))
  ;; PDF footer with proper headers and footers
  (format ps "showpage~%")
  (format ps "%--- Footer ---%~%")
  (format ps "0 0 moveto~%")
  (format ps "/Times-Roman findfont 8 scalefont setfont~%")
  (format ps "(Skyline-Tool for Phantasia 7800 | ~a | Page ~d) show~%"
          (escape-ps-string (game-asset-moniker resource)) 1))

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

