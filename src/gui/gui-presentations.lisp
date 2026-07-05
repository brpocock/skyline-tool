;;; Skyline-Tool src/gui/gui-presentations.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC
;;; Common presentation infrastructure for all game-resource types

(in-package :skyline-tool)

;; ============================================================================
;; Common Presentation Infrastructure
;; ============================================================================

(defgeneric resource-to-json (resource)
  (:documentation "Convert RESOURCE to JSON-compatible object for export."))

(defgeneric resource-from-json (json class)
  (:documentation "Create a RESOURCE instance of CLASS from JSON object."))

(defgeneric resource-to-postscript (resource stream &key title author)
  (:documentation "Write RESOURCE as PostScript to STREAM with proper headers/footers."))

(defgeneric resource-to-text (resource stream)
  (:documentation "Write RESOURCE as plain text to STREAM for human reading."))

(defgeneric open-resource-preview (resource)
  (:documentation "Open a read-only preview window for RESOURCE."))

(defgeneric open-resource-inspector (resource &optional mode)
  (:documentation "Open an inspector window for RESOURCE. MODE can be :editing, :reading, or :reference."))

;; ============================================================================
;; JSON Export/Import Infrastructure
;; ============================================================================

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
    (resource-from-json json class)))

;; ============================================================================
;; PostScript Export Infrastructure
;; ============================================================================

(defun export-resource-to-ps-file (resource filepath &key title author)
  "Export RESOURCE to PostScript file at FILEPATH with proper headers/footers."
  (with-open-file (ps filepath :direction :output :if-exists :supersede :external-format :utf-8)
    (write-ps-font-encodings ps)
    (write-ps-docinfo ps title author (user-homedir-pathname))
    (resource-to-postscript resource ps :title title :author author)
    (write-ps-page-footer ps 1 1 title
                          (format-timestring nil (get-universal-time) :format '(:year "-" :month "-" :day " " :hour ":" :min))
                          (user-homedir-pathname)
                          (machine-instance)))
  filepath)

(defun export-resource-to-text-file (resource filepath)
  "Export RESOURCE to plain text file at FILEPATH."
  (with-open-file (out filepath :direction :output :if-exists :supersede :external-format :utf-8)
    (resource-to-text resource out))
  filepath)

;; ============================================================================
;; Inspector/Preview Window Infrastructure
;; ============================================================================

(clim:define-application-frame resource-preview (gui-inspector-frame)
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
                (game-resource-moniker resource)
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
      (let ((filepath (clim:accept 'pathname :prompt "Save PDF as:"
                                    :default (merge-pathnames
                                               (format nil "~a.ps" (game-resource-title resource))
                                               (uiop:getcwd)))))
        (when filepath
          (export-resource-to-ps-file resource filepath
                                      :title (game-resource-title resource)
                                      :author (user-homedir-pathname))
          (format t "~&Exported to ~a~%" filepath))))))

(clim:define-command (com-preview-export-text :command-table resource-preview-menu-bar
                                               :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (resource (inspector-resource frame)))
    (when resource
      (let ((filepath (clim:accept 'pathname :prompt "Save Text as:"
                                    :default (merge-pathnames
                                               (format nil "~a.txt" (game-resource-title resource))
                                               (uiop:getcwd)))))
        (when filepath
          (export-resource-to-text-file resource filepath)
          (format t "~&Exported to ~a~%" filepath))))))

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
        (uiop:run-program (list "lp" pdf-filepath) :output nil)
        (format t "~&Sent to printer~%")))))

;; ============================================================================
;; Open Functions
;; ============================================================================

(defmethod open-resource-preview (resource)
  "Open a read-only preview window for RESOURCE."
  (clim:run-frame-top-level
   (clim:make-application-frame 'resource-preview
                                :resource resource
                                :view-mode :reading)))

(defmethod open-resource-inspector ((resource game-resource) &optional (mode :editing))
  "Open an inspector window for RESOURCE."
  (clim:run-frame-top-level
   (clim:make-application-frame 'gui-inspector-frame
                                :resource resource
                                :view-mode mode)))

;; ============================================================================
;; Common PostScript Content Generation
;; ============================================================================

(defgeneric write-resource-ps-content (resource ps)
  (:documentation "Write the main content of RESOURCE to PostScript stream PS."))

(defmethod resource-to-postscript ((resource game-resource) ps &key title author)
  (declare (ignore author))
  (write-ps-docinfo ps title "Skyline-Tool" (user-homedir-pathname))
  (write-ps-font-encodings ps)
  (format ps "~%%% Begin resource content~%")
  (write-resource-ps-content resource ps)
  (format ps "~%%% End resource content~%")
  (write-ps-page-footer ps 1 1 title
                        (format-timestring nil (get-universal-time) :format '(:year "-" :month "-" :day " " :hour ":" :min))
                        (user-homedir-pathname)
                        (machine-instance)
                        1 1))

(defgeneric write-resource-text-content (resource stream)
  (:documentation "Write human-readable text representation of RESOURCE to STREAM."))

(defmethod resource-to-text ((resource game-resource) stream)
  (write-resource-text-content resource stream)
  (terpri stream))

;; ============================================================================
;; Helper Functions for Common Fields
;; ============================================================================

(defun write-resource-common-ps (resource ps)
  "Write common resource fields to PS stream."
  (let ((title (game-resource-title resource))
        (kind (game-resource-kind resource))
        (moniker (game-resource-moniker resource))
        (vc-status (vc-file-status (or (game-resource-full-path resource)
                                       (game-resource-collective-path resource)))))
    (format ps "/Times-Roman-ISOLatin1 findfont 14 scalefont setfont~%")
    (format ps "56 600 moveto~%")
    (format ps "0.0 0.0 0.0 setrgbcolor~%")
    (format ps "(~a) show~%" (escape-ps-string title))
    (format ps "56 580 moveto~%")
    (format ps "(Kind: ~a) show~%" (escape-ps-string kind))
    (format ps "56 560 moveto~%")
    (format ps "(Moniker: ~a) show~%" (escape-ps-string moniker))
    (when vc-status
      (format ps "56 540 moveto~%")
      (format ps "(VC Status: ~a) show~%" (escape-ps-string vc-status)))))

(defun write-resource-common-text (resource stream)
  "Write common resource fields to text stream."
  (format stream "Title: ~a~%" (game-resource-title resource))
  (format stream "Kind: ~a~%" (game-resource-kind resource))
  (format stream "Moniker: ~a~%" (game-resource-moniker resource))
  (let ((vc-status (vc-file-status (or (game-resource-full-path resource)
                                       (game-resource-collective-path resource)))))
    (when vc-status
      (format stream "VC Status: ~a~%" vc-status))))

