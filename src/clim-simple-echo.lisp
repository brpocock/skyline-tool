(map nil #'ql:quickload '(:clim :clim-lisp :cl-plumbing))

(defpackage clim-simple-echo
  (:use :clim :clim-lisp :clim-extensions)
  (:export #:run-in-simple-echo))

(in-package :clim-simple-echo)

(defclass echo-view (textual-view) ())

(define-presentation-method present :around
  ((object sequence) (type sequence) stream (view echo-view)
                     &key acceptably for-context-type)
  (present object 'expression :stream stream :view view
                              :acceptably acceptably :for-context-type for-context-type))

(define-command-table echo-save-as-menu
  :menu (("Text..." :command com-save-text)
         ("PDF..." :command com-print-pdf)))

(define-command-table echo-print-to-menu
  :menu (("Select Printer..." :command com-print-select)))

(define-command-table echo-regenerate-builds-menu
  :menu (("NTSC Demo" :command com-regenerate-ntsc-demo)
         ("NTSC Public" :command com-regenerate-ntsc-public)
         ("NTSC Publisher" :command com-regenerate-ntsc-publisher)
         (nil :divider :line)
         ("PAL Demo" :command com-regenerate-pal-demo)
         ("PAL Public" :command com-regenerate-pal-public)
         ("PAL Publisher" :command com-regenerate-pal-publisher)))

(define-command-table echo-file-menu
  :menu (("Save As" :menu echo-save-as-menu)
         ("Print To" :menu echo-print-to-menu)
         (nil :divider :line)
         ("Regenerate" :command com-regenerate)
         (nil :divider :line)
         ("Close" :command com-close-echo)))

(define-command-table echo-edit-menu
  :menu (("Copy" :command com-copy-clipboard)))

(define-command-table echo-help-menu
  :menu (("How to Use This Window" :command com-help-for-window)
         ("Skyline-Tool Developers' Guide" :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide" :command com-open-scripting-guide)
         (nil :divider :line)
         ("About Skyline-Tool" :command com-about-echo)))

(define-application-frame simple-echo (standard-application-frame)
  ((pipe :initarg :pipe :reader frame-pipe)
   (captured-text :initform nil :accessor frame-captured-text))
  (:panes (echo-container :application :height 1000 :width 1000
                                         :display-function 'echo-echo))
  (:command-table (simple-echo))
  (:menu-bar echo-menu-bar)
  (:icon (skyline-tool:skyline-tool-icon))
  (:layouts (default echo-container)))

(defmethod initialize-instance :after ((frame simple-echo) &key)
  (ignore-errors (skyline-tool::%print-menu-populate 'echo-print-to-menu)))

(define-command-table echo-menu-bar
  :menu (("Report" :menu echo-file-menu) ("Edit" :menu echo-edit-menu) ("Help" :menu echo-help-menu)))

;; --- Helper: default save directory ---

(defvar *last-export-directory* nil
  "Most recently used export directory (pathname or NIL).")

(defun default-save-directory ()
  "Return the preferred directory for saving exports:
   most-recently-used, ~/work, ~/Work, or ~/Documents."
  (or *last-export-directory*
      (and (probe-file (merge-pathnames #p"work/" (user-homedir-pathname)))
           (merge-pathnames #p"work/" (user-homedir-pathname)))
      (and (probe-file (merge-pathnames #p"Work/" (user-homedir-pathname)))
           (merge-pathnames #p"Work/" (user-homedir-pathname)))
      (merge-pathnames #p"Documents/" (user-homedir-pathname))))

(defun window-title->filename (frame extension)
  "Convert a frame's pretty name to a filesystem-safe filename with EXTENSION.
   e.g. \"About Skyline-Tool\" + \"pdf\" → \"About Skyline-Tool.pdf\"."
  (let* ((title (or (ignore-errors (frame-pretty-name frame)) "Output"))
         (safe (remove-if (lambda (c) (find c "\\/:*?\"<>|" :test #'char=)) title)))
    (format nil "~a.~a" safe extension)))

;; --- Commands for simple-echo ---

(define-simple-echo-command (com-save-text :menu nil :name t) ()
  (let* ((text (frame-captured-text *application-frame*))
         (frame *application-frame*)
         (save-dir (default-save-directory))
         (default-name (namestring
                        (merge-pathnames
                         (window-title->filename frame "txt")
                         save-dir)))
         (path (%zenity-or-clim "Save As..." default-name)))
    (when (and path (plusp (length path)))
      (let ((p (string-trim '(#\Newline #\Space #\Tab) path)))
        (setf *last-export-directory*
              (make-pathname :defaults p :name nil :type nil))
        (with-open-file (f p :direction :output :if-exists :supersede
                           :external-format :utf-8)
          (princ text f))
        (format *query-io* "~&Saved ~a (~d bytes).~%" p (length text))))))

(defun %zenity-or-clim (title default-name)
  "Prompt for a filename using zenity (if available) or CLIM's accept."
  (let ((zenity-out (ignore-errors
                     (string-trim '(#\Newline #\Space)
                      (uiop:run-program (list "zenity" "--file-selection" "--save"
                                              (format nil "--filename=~a" default-name)
                                              (format nil "--title=~a" title))
                                        :output :string :ignore-error-status t)))))
    (if (and zenity-out (plusp (length zenity-out)))
        zenity-out
        (let ((path (clim:accept 'pathname :prompt title :default default-name)))
          (when path (namestring path))))))

(define-simple-echo-command (com-print-pdf :menu nil :name t) ()
  (let* ((text (frame-captured-text *application-frame*))
         (frame *application-frame*)
         (save-dir (default-save-directory))
         (default-name (namestring
                        (merge-pathnames
                         (window-title->filename frame "pdf")
                         save-dir)))
         (pdf-path (%zenity-or-clim "Save As PDF..." default-name)))
    (when pdf-path
      (let* ((base (pathname-name pdf-path))
             (dir (make-pathname :defaults pdf-path :name nil :type nil))
             (ps-path (merge-pathnames (make-pathname :name base :type "ps") dir))
             (pdf-final (merge-pathnames (make-pathname :name base :type "pdf") dir)))
        ;; Calculate page layout
        (let* ((game-title (string-capitalize (or (ignore-errors (symbol-value 'skyline-tool::*game-title*)) "unknown")))
               (title (format nil "Skyline-Tool for ~a" game-title))
               (author (ignore-errors (skyline-tool::user-real-name)))
               (date-str (multiple-value-bind (s m h d mo y) (get-decoded-time)
                           (declare (ignore s))
                           (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" y mo d h m)))
               (lines (with-input-from-string (s text) (loop for l = (read-line s nil nil) while l count l)))
               (lines-per-page (max 1 (floor (- 700 50) 10)))
               (total-pages (max 1 (ceiling lines lines-per-page))))
          (with-open-file (ps ps-path :direction :output :if-exists :supersede
                               :external-format :utf-8)
            (format ps "%!PS-Adobe-3.0~%")
            (format ps "<< /PageSize [612 792] >> setpagedevice~%")
            (skyline-tool::write-ps-font-encodings ps)
            (with-input-from-string (s text)
              (dotimes (page total-pages)
                (format ps "%%Page: ~d ~d~%" (1+ page) total-pages)
                (skyline-tool::write-ps-header-bar ps title date-str author game-title)
                ;; Body text
                (format ps "/Times-Roman-ISOLatin1 findfont 9 scalefont setfont~%")
                (let ((y 700) (line-height 10))
                  (loop for line = (read-line s nil nil)
                        while (and line (>= y 50))
                        do (format ps "50 ~d moveto (~a) show~%" y (skyline-tool::escape-ps-string line))
                           (decf y line-height)))
                ;; Footer: page number right-justified
                (format ps "gsave~%")
                (format ps "/Times-Roman-ISOLatin1 findfont 7 scalefont setfont 0.6 0.6 0.6 setrgbcolor~%")
                (format ps "550 15 moveto (~d of ~d) show~%" (1+ page) total-pages)
                (format ps "grestore~%")
                (format ps "showpage~%")))))
        (uiop:run-program (list "ps2pdf" (namestring ps-path) (namestring pdf-final))
                          :output nil :ignore-error-status t)
        (ignore-errors (delete-file ps-path))
        (format *query-io* "~&Saved ~a~%" (namestring pdf-final))
        (uiop:run-program (list "xdg-open" (namestring pdf-final)) :output nil :ignore-error-status t)))))

(defun %framed-text-content ()
  "Return the captured text of the current echo frame, or nil."
  (and (boundp '*application-frame*)
       *application-frame*
       (typep *application-frame* 'simple-echo)
       (frame-captured-text *application-frame*)))

(defun %print-text-to-printer (printer-name)
  "Print the current echo frame's captured text to PRINTER-NAME via lp."
  (let* ((text (%framed-text-content))
         (base (format nil "EchoOutput-~d" (get-universal-time)))
         (ps-path (format nil "~a.ps" base))
         (pdf-path (format nil "~a.pdf" base))
         (lines (and text (count #\Newline text)))
         (total-pages (and lines (max 1 (ceiling lines (- 700 50))))))
    (when text
      (with-open-file (ps ps-path :direction :output :if-exists :supersede)
        (format ps "%!PS-Adobe-3.0~%")
        (format ps "<< /PageSize [612 792] >> setpagedevice~%")
        (with-input-from-string (s text)
          (dotimes (page total-pages)
            (format ps "%%Page: ~d ~d~%" (1+ page) total-pages)
            (let ((y 700) (line-height 10))
              (loop for line = (read-line s nil nil)
                    while (and line (>= y 50))
                    do (format ps "50 ~d moveto (~a) show~%" y (skyline-tool::escape-ps-string line))
                       (decf y line-height)))
            (format ps "showpage~%"))))
      (uiop:run-program (list "ps2pdf" ps-path pdf-path)
                        :output nil :ignore-error-status t)
      (ignore-errors (delete-file ps-path))
      (uiop:run-program (list "lp" "-d" printer-name pdf-path)
                        :output nil :ignore-error-status t)
      (ignore-errors (delete-file pdf-path))
      (format *query-io* "~&Printed ~a to ~a~%" pdf-path printer-name))))

(define-simple-echo-command (com-print-select :menu nil :name t) ()
  (let* ((text (frame-captured-text *application-frame*))
         (printers (ignore-errors (skyline-tool::discover-printers))))
    (if (null printers)
        (format *query-io* "~&No printers discovered.~%")
        (progn
          (format *query-io* "~&Select printer (1-~d):~%" (length printers))
          (dotimes (i (length printers))
            (format *query-io* "  ~d. ~a~%" (1+ i) (elt printers i)))
          (force-output *query-io*)
          (let* ((choice (clim:accept 'integer :prompt "Printer :" :default 1))
                 (printer (elt printers (1- choice)))
                 (base (format nil "EchoOutput-~d" (get-universal-time)))
                 (ps-path (format nil "~a.ps" base))
                 (pdf-path (format nil "~a.pdf" base))
                 (lines (count #\Newline text))
                 (lines-per-page (- 700 50))
                 (total-pages (max 1 (ceiling lines lines-per-page))))
            (with-open-file (ps ps-path :direction :output :if-exists :supersede)
              (format ps "%!PS-Adobe-3.0~%")
              (format ps "<< /PageSize [612 792] >> setpagedevice~%")
              (with-input-from-string (s text)
                (dotimes (page total-pages)
                  (format ps "%%Page: ~d ~d~%" (1+ page) total-pages)
                  (let ((y 700) (line-height 10))
                    (loop for line = (read-line s nil nil)
                          while (and line (>= y 50))
                          do (format ps "50 ~d moveto (~a) show~%" y
                                     (skyline-tool::escape-ps-string line))
                             (decf y line-height)))
                  (format ps "showpage~%"))))
            (uiop:run-program (list "ps2pdf" ps-path pdf-path)
                              :output nil :ignore-error-status t)
            (ignore-errors (delete-file ps-path))
            (format *query-io* "~&Sending ~a to ~a~%" pdf-path printer)
            (uiop:run-program (list "lp" "-d" printer pdf-path)
                              :output nil :ignore-error-status t))))))

(define-simple-echo-command (com-about-echo :menu nil :name t) ()
  (let* ((frame (and (boundp '*application-frame*) *application-frame*))
         (title (when frame (ignore-errors (frame-pretty-name frame)))))
    (if (and title (search "About Skyline-Tool" title :test #'char-equal))
        (format *query-io* "~&Already viewing About Skyline-Tool.~%")
        (skyline-tool::about-skyline-tool))))

(defun %clipboard-copy (text)
  "Copy TEXT to the system clipboard using wl-copy or xclip."
  (block nil
    (let ((prog (or (ignore-errors (string-trim '(#\Newline #\Space)
                                      (uiop:run-program '("which" "wl-copy") :output :string)))
                    (ignore-errors (string-trim '(#\Newline #\Space)
                                      (uiop:run-program '("which" "xclip") :output :string))))))
      (unless prog (return nil))
      (with-input-from-string (in text)
        (let ((args (if (search "wl-copy" prog)
                        (list prog)
                        (list prog "-selection" "clipboard"))))
          (let ((exit (uiop:run-program args :input in :output nil
                                        :ignore-error-status t :force-shell nil)))
            (unless exit (return nil))
            (when (and (integerp exit) (zerop exit)) t)))))))

(define-simple-echo-command (com-copy-clipboard :menu nil :name t) ()
  (if (not (boundp '*application-frame*))
      (format *query-io* "~&No active window to copy from.~%")
      (let ((text (frame-captured-text *application-frame*)))
        (if (and (plusp (length text)) (%clipboard-copy text))
            (format *query-io* "~&Copied ~d characters to clipboard.~%" (length text))
            (format *query-io* "~&Copy to clipboard: no clipboard tool found.~%")))))

(define-simple-echo-command (com-close-echo :menu nil :name t) ()
  (frame-exit *application-frame*))

(define-simple-echo-command (com-open-dev-guide :menu nil :name t) ()
  (let ((html-index (namestring
                     (asdf:system-relative-pathname
                      :skyline-tool
                      #p"../Dist/7800/PhantasiaDevGuide-html/index.html"))))
    (if (probe-file html-index)
        (uiop:run-program (list "xdg-open" html-index) :output nil)
        (progn
          (format *query-io* "~&Developers' Guide not built. Run make doc.~%")
          (if (clim:accept 'boolean :prompt "Build now" :default t)
              (clim-sys:make-process
               (lambda ()
                 (uiop:run-program (list "ptyxis" "-s" "--title" "Building Dev Guide"
                                         "--" "make" "doc")
                                   :output nil :ignore-error-status t))
               :name "Building Dev Guide")
              (format *query-io* "~&Run make doc when ready.~%"))))))

(define-simple-echo-command (com-regenerate :menu nil :name t) ()
  "Re-run the current display function to regenerate the report."
  (clim:redisplay-frame-panes *application-frame*))

(macrolet ((def-regen (name build region)
             `(define-simple-echo-command (,name :menu nil :name t) ()
                (skyline-tool::rom-budget ,build ,region))))
  (def-regen com-regenerate-ntsc-demo "Demo" :ntsc)
  (def-regen com-regenerate-ntsc-public "Public" :ntsc)
  (def-regen com-regenerate-ntsc-publisher "Publisher" :ntsc)
  (def-regen com-regenerate-pal-demo "Demo" :pal)
  (def-regen com-regenerate-pal-public "Public" :pal)
  (def-regen com-regenerate-pal-publisher "Publisher" :pal))

;; --- Run function ---

(defun run-in-simple-echo (function &key (width 800)
                                         (height 400)
                                         port
                                         frame-manager
                                         (process-name (format nil "Echo from ~s" function))
                                         (window-title process-name))
  (let* ((fm (or frame-manager (find-frame-manager :port (or port (find-port)))))
         (pipe function)
         (frame (make-application-frame 'simple-echo
                                        :pretty-name window-title
                                        :function function
                                        :frame-manager fm
                                        :pipe pipe
                                        :width width
                                        :height height)))
    (clim-sys:make-process (lambda ()
                             (run-frame-top-level frame))
                           :name process-name)))

;; --- Display function with output capture ---

(defun echo-echo (frame pane)
  (clim:window-clear pane)
  (ignore-errors (setf (clim:window-viewport-position pane) (values 0 0)))
  (let ((capture (make-string-output-stream)))
    (let ((*standard-output* (make-broadcast-stream pane capture))
          (*trace-output* *standard-output*)
          (*error-output* *standard-output*))
      (funcall (frame-pipe frame)))
    (setf (frame-captured-text frame) (get-output-stream-string capture))
    (force-output pane)))
