(defpackage clim-simple-echo
  (:use :clim :clim-lisp :clim-extensions)
  (:export #:run-in-simple-echo
           #:capturing-stream
           #:capturing-target
           #:frame-captured-text))

(in-package :clim-simple-echo)

(defclass echo-view (textual-view) ())
(define-presentation-method present :around
                            ((object sequence)
                             (type sequence) stream
                             (view t) &key acceptably for-context-type)
                            (declare (ignore view))
                            (present object 'expression :stream stream
                                     :acceptably acceptably :for-context-type
                                     for-context-type))

(defun present-slot-value (stream resource slot)
  "Fallback presentation method that outputs the slot value of RESOURCE to STREAM.
Used when more sophisticated presentation methods are not available."
  (format stream "~a" (slot-value resource slot)))

(define-command-table echo-save-as-menu
  :menu (("Text..." :command com-save-text)
         ("PDF..." :command com-print-pdf)))

(define-command-table echo-send-to-menu
  :menu ()) ; FIXME populate with p2p recipients

(define-command-table echo-print-to-menu
  :menu ()) ; FIXME populate with printers

(define-command-table echo-edit-menu
  :menu (("Copy" :command com-copy-clipboard)
         ("Find..." :command com-find-in-echo)))

(define-command-table echo-file-menu
  :menu (("Regenerate" :command com-regenerate)
         (nil :divider :line)
         ("Save as" :menu echo-save-as-menu)
         ("Send to" :menu echo-send-to-menu)
         ("Print to" :menu echo-print-to-menu)
         (nil :divider :line)
         ("Close" :command com-close-echo)))

(define-command-table echo-help-menu
  :menu (("How to Use this Report..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-scripting-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-echo)))

(define-application-frame simple-echo (standard-application-frame)
  ((pipe :initarg :pipe :reader frame-pipe)
   (captured-text :initform nil :accessor frame-captured-text)
   (pdf-function :initform nil :initarg :pdf-function :accessor frame-pdf-function))
  (:panes (echo-container :application :height 1000 :width 1000
                                          :display-function 'echo-echo))
  (:command-table (simple-echo))
  (:menu-bar echo-menu-bar)
  (:icon (skyline-tool::skyline-tool-icon))
  (:layouts (default echo-container)))

(defmethod initialize-instance :after ((frame simple-echo) &key)
  (ignore-errors (populate-echo-print-menu frame)))

(define-command-table simple-echo
  :inherit-from (clim-internals::global-command-table)
  :menu (("Report" :menu echo-file-menu) ("Edit" :menu echo-edit-menu) ("Help" :menu echo-help-menu)))

(define-command-table echo-menu-bar
  :menu (("Report" :menu echo-file-menu) ("Edit" :menu echo-edit-menu) ("Help" :menu echo-help-menu)))

;;  Helper: default save directory 

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

;;  Commands for simple-echo 

(define-simple-echo-command (com-save-text :menu nil :name t) ()
  (let* ((text (frame-captured-text clim:*application-frame*))
         (frame clim:*application-frame*)
         (default-name (window-title->filename frame "txt"))
         (path (funcall (intern "PROMPT-SAVE-PATHNAME" (find-package "SKYLINE-TOOL"))
                        default-name :prefs-key :last-export-directory)))
    (when path
      (setf *last-export-directory* (make-pathname :name nil :type nil :defaults path))
      (with-open-file (f path :direction :output :if-exists :supersede
                              :external-format :utf-8)
        (princ text f)))))

(define-simple-echo-command (com-print-pdf :menu nil :name t) ()
  (let* ((frame clim:*application-frame*)
         (pdf-fn (frame-pdf-function frame))
         (default-name (window-title->filename frame "pdf"))
         (pdf-path (prompt-save-pathname default-name :prefs-key :last-export-directory)))
    (when pdf-path
      (let* ((base (pathname-name pdf-path))
             (dir (make-pathname :defaults pdf-path :name nil :type nil))
             (ps-path (merge-pathnames (make-pathname :name base :type "ps") dir))
             (pdf-final (merge-pathnames (make-pathname :name base :type "pdf") dir)))
        (if pdf-fn
            ;; Custom PDF generator (e.g. Assets Index)
            (progn
              (funcall pdf-fn ps-path)
              (uiop:run-program (list "ps2pdf" (namestring ps-path) (namestring pdf-final))
                                :output nil :ignore-error-status t)
              (ignore-errors (delete-file ps-path))
              (uiop:run-program (list "xdg-open" (namestring pdf-final))
                                :output nil :ignore-error-status t))
            ;; Text-based PDF generation
            (let* ((text (frame-captured-text frame)))
              (unless (and text (stringp text) (plusp (length text)))
                (return-from com-print-pdf))
              (let* ((frame-name (ignore-errors (clim:frame-pretty-name frame)))
                     (game-title (string-capitalize
                                  (or (ignore-errors (symbol-value '*game-title*))
                                      "unknown")))
                     (title (or frame-name (format nil "Skyline-Tool: ~a" game-title)))
                     (author (ignore-errors (user-real-name)))
                     (hostname (machine-instance))
                     (date-str (multiple-value-bind (s m h d mo y) (get-decoded-time)
                                 (declare (ignore s))
                                 (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" y mo d h m)))
                     (lines (with-input-from-string (s text)
                              (loop for l = (read-line s nil nil) while l count l)))
                     (lines-per-page (max 1 (floor (- 680 80) 10)))
                     (total-pages (max 1 (ceiling lines lines-per-page))))
                (with-open-file (ps ps-path :direction :output :if-exists :supersede
                                            :external-format :utf-8)
                  (format ps "%!PS-Adobe-3.0~%")
                  (write-ps-docinfo ps title "Skyline-Tool"
                                                  (format nil "~a on ~a" author (machine-instance)))
                  (format ps "<< /PageSize [612 792] >> setpagedevice~%")
                  (write-ps-font-encodings ps)
                  (with-input-from-string (s text)
                    (dotimes (page total-pages)
                      (format ps "%%Page: ~d ~d~%" (1+ page) total-pages)
                      (write-ps-header-bar ps title date-str author game-title)
                      (format ps "/Times-Roman-ISOLatin1 findfont 9 scalefont setfont 0 0 0 setrgbcolor~%")
                      (let ((y 680) (line-height 10) (bar-w 108) (bar-h 8))
                        (declare (ignore bar-w))
                        (loop for line = (read-line s nil nil)
                              while (and line (>= y 80))
                              do (let ((bracket-pos (position #\[ line))
                                       (pct-pos (position #\% line)))
                                   (cond
                                     ((and bracket-pos pct-pos (find #\] line :start bracket-pos)
                                           (> (length line) (+ bracket-pos 10)))
                                      (let* ((end-bracket (position #\] line :start bracket-pos))
                                             (pct-str (string-trim " " (subseq line (1+ end-bracket))))
                                             (pct (ignore-errors (parse-integer pct-str :junk-allowed t)))
                                             (bar-x 350) (bar-w 108)
                                             (pct (or pct 0)))
                                        (let ((label (string-trim " " (subseq line 0 bracket-pos))))
                                          (format ps "50 ~d moveto (~a) show~%" y
                                                  (escape-ps-string label)))
                                        (format ps "gsave newpath ~d ~d ~d ~d rectstroke 0.7 0.85 1.0 setrgbcolor fill grestore~%"
                                                bar-x y bar-w bar-h)
                                        (when (> pct 0)
                                          (let ((fill-w (max 1 (round (* bar-w (/ pct 100))))))
                                            (format ps "gsave newpath ~d ~d ~d ~d rectfill 0.0 0.0 0.4 setrgbcolor grestore~%"
                                                    bar-x y fill-w bar-h)
                                            (format ps " ~d ~d moveto (~d%) show~%" (+ bar-x bar-w 5) y pct)))))
                                     ((and bracket-pos pct-pos (> bracket-pos 20))
                                      (let* ((end-bracket (position #\] line :start bracket-pos))
                                             (pct-str (string-trim " " (subseq line (1+ end-bracket))))
                                             (pct (ignore-errors (parse-integer pct-str :junk-allowed t)))
                                             (pct (or pct 0))
                                             (bar-x 50) (bar-w 512) (bar-h 16))
                                        (format ps "gsave newpath ~d ~d ~d ~d rectstroke 0.7 0.85 1.0 setrgbcolor fill grestore~%"
                                                bar-x y bar-w bar-h)
                                        (when (> pct 0)
                                          (let ((fill-w (max 1 (round (* bar-w (/ pct 100))))))
                                            (format ps "gsave newpath ~d ~d ~d ~d rectfill 0.0 0.0 0.4 setrgbcolor grestore~%"
                                                    bar-x y fill-w bar-h)
                                            (format ps " ~d ~d moveto (~d%) show~%" (+ bar-x bar-w 5) y pct)))))
                                     (t
                                      (format ps "50 ~d moveto (~a) show~%" y
                                              (escape-ps-string line))))
                                   (decf y line-height)))
                        (write-ps-page-footer ps (1+ page) total-pages game-title date-str author hostname)
                        (format ps "showpage~%")))))
                (uiop:run-program (list "ps2pdf" (namestring ps-path) (namestring pdf-final))
                                  :output nil :ignore-error-status t)
                (ignore-errors (delete-file ps-path))
                (uiop:run-program (list "xdg-open" (namestring pdf-final))
                                  :output nil :ignore-error-status t))))))))

(defun %framed-text-content ()
  "Return the captured text of the current echo frame, or nil."
  (and (boundp 'clim:*application-frame*)
       clim:*application-frame*
       (typep clim:*application-frame* 'simple-echo)
       (frame-captured-text clim:*application-frame*)))

(defun %print-text-to-printer (printer-name)
  "Print the current echo frame to PRINTER-NAME via lp.
   Uses frame-pdf-function if available, otherwise captured text."
  (let* ((frame (and (boundp 'clim:*application-frame*) clim:*application-frame*))
         (pdf-fn (and frame (typep frame 'simple-echo) (frame-pdf-function frame)))
         (base (format nil "EchoOutput-~d" (get-universal-time)))
         (ps-path (format nil "~a.ps" base))
         (pdf-path (format nil "~a.pdf" base)))
    (if pdf-fn
        (progn
          (funcall pdf-fn ps-path)
          (uiop:run-program (list "ps2pdf" ps-path pdf-path)
                            :output nil :ignore-error-status t)
          (ignore-errors (delete-file ps-path))
          (uiop:run-program (list "lp" "-d" printer-name pdf-path)
                            :output nil :ignore-error-status t)
          (ignore-errors (delete-file pdf-path)))
        (let ((text (%framed-text-content)))
          (when text
            (let* ((lines (count #\Newline text))
                   (total-pages (max 1 (ceiling lines (- 700 50))))
                   (title (format nil "Skyline-Tool for ~a"
                                  (string-capitalize
                                   (or (ignore-errors (symbol-value '*game-title*)) "Game"))))
                   (author (ignore-errors (user-real-name))))
              (with-open-file (ps ps-path :direction :output :if-exists :supersede)
                (format ps "%!PS-Adobe-3.0~%")
                (write-ps-docinfo ps title "Skyline-Tool"
                                                (format nil "~a on ~a" author (machine-instance)))
                (format ps "<< /PageSize [612 792] >> setpagedevice~%")
                (with-input-from-string (s text)
                  (dotimes (page total-pages)
                    (format ps "%%Page: ~d ~d~%" (1+ page) total-pages)
                    (let ((y 700) (line-height 10))
                      (loop for line = (read-line s nil nil)
                            while (and line (>= y 50))
                            do (format ps "50 ~d moveto (~a) show~%" y (escape-ps-string line))
                               (decf y line-height)))
                    (format ps "showpage~%"))))
              (uiop:run-program (list "ps2pdf" ps-path pdf-path)
                                :output nil :ignore-error-status t)
              (ignore-errors (delete-file ps-path))
              (uiop:run-program (list "lp" "-d" printer-name pdf-path)
                                :output nil :ignore-error-status t)
              (ignore-errors (delete-file pdf-path))))))))

(define-simple-echo-command (com-about-echo :menu nil :name t) ()
  (com-about-skyline-tool))

(defun %clipboard-copy (text)
  "Copy TEXT to the system clipboard using wl-copy or xclip, or output it."
  (block nil
    (let ((prog (or (ignore-errors (string-trim '(#\Newline #\Space)
                                    (uiop:run-program '("which" "wl-copy") :output :string)))
                 (ignore-errors (string-trim '(#\Newline #\Space)
                                 (uiop:run-program '("which" "xclip") :output :string))))))
      (if prog
          (with-input-from-string (in text)
            (let ((args (if (search "wl-copy" prog)
                            (list prog)
                            (list prog "-selection" "clipboard"))))
              (let ((exit (uiop:run-program args :input in :output nil
                                                 :ignore-error-status t :force-shell nil)))
                (unless exit (return nil))
                (when (and (integerp exit) (zerop exit)) t))))
          ;; No clipboard tool — output to *query-io* instead
          (progn
            (format *query-io* "~&~a" text)
            t)))))

(define-simple-echo-command (com-copy-clipboard :menu nil :name t) ()
  (if (not (boundp 'clim:*application-frame*))
      (format *query-io* "~&No active window to copy from.~%")
      (let ((text (frame-captured-text clim:*application-frame*)))
        (if (and (plusp (length text)) (%clipboard-copy text))
            (format *query-io* "~&Copied ~d characters.~%" (length text))
            (format *query-io* "~&Failed to copy.~%")))))

(define-simple-echo-command (com-copy-as-json :menu nil :name t) ()
  (if (not (boundp 'clim:*application-frame*))
      (format *query-io* "~&No active window to copy from.~%")
      (let* ((text (frame-captured-text clim:*application-frame*))
             (json (if (and (> (length text) 0) (char= (char text 0) #\{))
                       text
                       (format nil "{ \"window\": ~s, \"content\": ~s, \"timestamp\": ~s }"
                               (ignore-errors (clim:frame-pretty-name clim:*application-frame*))
                               text
                               (multiple-value-bind (s m h d mo y) (get-decoded-time)
                                 (declare (ignore s))
                                 (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" y mo d h m))))))
        (if (%clipboard-copy json)
            (format *query-io* "~&Copied ~d characters as JSON to clipboard.~%" (length json))
            (format *query-io* "~&Copy to clipboard: no clipboard tool found.~%")))))

(define-simple-echo-command (com-save-as-json :menu nil :name t) ()
  (if (not (boundp 'clim:*application-frame*))
      (format *query-io* "~&No active window.~%")
      (let* ((text (frame-captured-text clim:*application-frame*))
             (frame-name (ignore-errors (clim:frame-pretty-name clim:*application-frame*)))
             (default-name (format nil "~a.json" (or frame-name "output")))
             (path (prompt-save-pathname default-name :prefs-key :last-export-directory)))
        (when path
          (let* ((timestamp (multiple-value-bind (s m h d mo y) (get-decoded-time)
                              (declare (ignore s))
                              (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" y mo d h m)))
                 (json (if (and (> (length text) 0) (char= (char text 0) #\{))
                           text  ; already structured JSON (About dialog)
                           (format nil "{ \"title\": ~s, \"content\": ~s, \"generated\": ~s }"
                                   frame-name text timestamp))))
            (with-open-file (f path :direction :output :if-exists :supersede
                                    :external-format :utf-8)
              (princ json f))
            (format *query-io* "~&Saved ~a (~d bytes).~%" path (length json)))))))

(define-simple-echo-command (com-close-echo :menu nil :name t) ()
  (clim:frame-exit clim:*application-frame*))

(define-simple-echo-command (com-find-in-echo :menu nil :name t) ()
  (error "Find is not available in this window."))

(define-simple-echo-command (com-open-dev-guide :menu nil :name t) ()
  (let ((html-index (namestring
                     (asdf:system-relative-pathname
                      :skyline-tool
                      #p"../Dist/7800/PhantasiaDevGuide-html/index.html"))))
    (if (probe-file html-index)
        (uiop:run-program (list "xdg-open" html-index) :output nil)
        (progn
          (funcall (intern "BUILD-MAKE-TARGET" (find-package "SKYLINE-TOOL")) "doc")
          (uiop:run-program (list "xdg-open" html-index) :output nil)))))

(define-simple-echo-command (com-regenerate :menu nil :name t) ()
  "Re-run the current display function to regenerate the report."
  (clim:redisplay-frame-panes clim:*application-frame*))

;;  Print To menu population 

(defun %print-echo-to-printer (printer)
  "Print the current echo frame's content to PRINTER.
PRINTER can be an ipp-printer struct, a queue name string, or nil for default."
  (let* ((queue (cond
                 ((typep printer 'ipp-printer) (ipp-queue printer))
                 ((stringp printer) printer)
                 (t nil)))
         (frame (and (boundp 'clim:*application-frame*) clim:*application-frame*))
         (pdf-fn (and frame (frame-pdf-function frame)))
         (base (format nil "EchoOutput-~d" (get-universal-time)))
         (ps-path (format nil "~a.ps" base))
         (pdf-path (format nil "~a.pdf" base)))
    (if pdf-fn
        (progn
          (funcall pdf-fn ps-path)
          (uiop:run-program (list "ps2pdf" ps-path pdf-path)
                            :output nil :ignore-error-status t)
          (ignore-errors (delete-file ps-path))
          (uiop:run-program (if queue (list "lp" "-d" queue pdf-path) (list "lp" pdf-path))
                            :output nil :ignore-error-status t)
          (ignore-errors (delete-file pdf-path))
          (format *query-io* "~&Printed assets index to ~a~%" (or queue "default")))
        (let* ((text (and frame (frame-captured-text frame))))
          (unless (and text (stringp text) (plusp (length text)))
            (format *query-io* "~&No content to print.~%")
            (return-from %print-echo-to-printer))
          (let* ((lines (count #\Newline text))
                 (total-pages (max 1 (ceiling lines (- 700 50))))
                 (title (format nil "Skyline-Tool for ~a"
                                (string-capitalize
                                 (or (ignore-errors (symbol-value '*game-title*)) "Game"))))
                 (author (ignore-errors (user-real-name))))
            (with-open-file (ps ps-path :direction :output :if-exists :supersede)
              (format ps "%!PS-Adobe-3.0~%")
              (write-ps-docinfo ps title "Skyline-Tool"
                                              (format nil "~a on ~a" author (machine-instance)))
              (format ps "<< /PageSize [612 792] >> setpagedevice~%")
              (write-ps-font-encodings ps)
              (with-input-from-string (s text)
                (dotimes (page total-pages)
                  (format ps "%%Page: ~d ~d~%" (1+ page) total-pages)
                  (let ((y 700) (line-height 10))
                    (loop for line = (read-line s nil nil)
                          while (and line (>= y 50))
                          do (format ps "50 ~d moveto (~a) show~%" y
                                     (escape-ps-string line))
                             (decf y line-height)))
                  (format ps "showpage~%"))))
            (uiop:run-program (list "ps2pdf" ps-path pdf-path)
                              :output nil :ignore-error-status t)
            (ignore-errors (delete-file ps-path))
            (uiop:run-program (if queue (list "lp" "-d" queue pdf-path) (list "lp" pdf-path))
                              :output nil :ignore-error-status t)
            (ignore-errors (delete-file pdf-path)))))))

(defun populate-echo-print-menu (&optional frame (command-table 'echo-print-to-menu))
  "Populate echo-print-to-menu with discovered printers.
   Falls back to captured text printing when frame has no pdf-function.
   When COMMAND-TABLE is specified, populate that table instead."
  (declare (ignore frame))
  (ignore-errors
   (clim:remove-menu-item-from-command-table command-table "No printers found")
   (clim:remove-menu-item-from-command-table command-table "Default Printer (lpr)"))
  (ensure-printer-scavenger-is-running)
  (if *ipp-printer-registry*
      (dolist (printer *ipp-printer-registry*)
        (let* ((struct (cdr printer))
               (display (ipp-name struct)))
          (clim:add-menu-item-to-command-table
           command-table display :command
           `(com-print-to-printer ,struct)
           :after :end)))
      (clim:add-menu-item-to-command-table
       command-table "Default Printer (lpr)" 
       :command
       '(com-print-to-printer nil)
       :after :end))
  ;; Define the print command dynamically
  (unless (fboundp 'com-print-to-printer)
    (clim:define-command (com-print-to-printer
                          :command-table clim-internals::global-command-table
                          :menu nil :name t)
        ((printer t))
      (%print-echo-to-printer printer))))

;;  Run function 

(defun run-in-simple-echo (function &key (width 800)
                                         (height 400)
                                         port
                                         frame-manager
                                         pdf-function
                                         (process-name (format nil "Echo from ~s" function))
                                         (window-title process-name)
                                         menu-bar)
  (let* ((fm (or frame-manager (find-frame-manager :port (or port (clim:find-port)))))
         (pipe function)
         (frame (make-application-frame 'simple-echo
                                        :name "Skyline-Tool"
                                        :pretty-name window-title
                                        :function function
                                        :frame-manager fm
                                        :pipe pipe
                                        :pdf-function pdf-function
                                        :menu-bar (or menu-bar 'echo-menu-bar)
                                        :width width
                                        :height height)))
    (clim-sys:make-process (lambda ()
                             (run-frame-top-level frame))
                           :name process-name)))

;;  Display function with output capture 

(defclass capturing-stream (fundamental-character-output-stream)
  ((target :initarg :target :reader capturing-target)
   (capture :initarg :capture :reader capturing-capture))
  (:default-initargs :capture (make-string-output-stream)))

(defmethod stream-write-char ((s capturing-stream) c)
  (write-char c (slot-value s 'target))
  (write-char c (slot-value s 'capture))
  (when (or (char= c #\Newline) (char= c #\Return))
    (force-output (slot-value s 'target))))

(defmethod stream-write-string ((s capturing-stream) string
                                &optional (start 0) (end (length string)))
  (loop for i from start below end
        do (stream-write-char s (char string i))))

(defmethod stream-force-output ((s capturing-stream))
  (force-output (slot-value s 'target)))

(defmethod stream-fresh-line ((s capturing-stream))
  (fresh-line (slot-value s 'target))
  (fresh-line (slot-value s 'capture)))

(defmethod stream-terpri ((s capturing-stream))
  (terpri (slot-value s 'target))
  (terpri (slot-value s 'capture)))

;; Delegate CLIM output recording to the target (pane) stream
(defmethod clim:invoke-with-output-to-output-record
    ((stream capturing-stream) continuation record-type &key parent)
  (clim:invoke-with-output-to-output-record (slot-value stream 'target)
                                            continuation record-type :parent parent))

(defmethod clim:stream-add-output-record ((stream capturing-stream) record)
  (clim:stream-add-output-record (slot-value stream 'target) record))

(defmethod clim:stream-output-history ((stream capturing-stream))
  (clim:stream-output-history (slot-value stream 'target)))

(defmethod clim:window-clear ((stream capturing-stream))
  (clim:window-clear (slot-value stream 'target)))

(defmethod clim-internals::invoke-with-sheet-medium (continuation
                                                     (stream capturing-stream))
  ;; bordered-output and other CLIM infrastructure may need the sheet medium;
  ;; delegate to the target (pane) stream which has a proper medium.
  (clim-internals::invoke-with-sheet-medium continuation (slot-value stream 'target)))

;; Delegate common CLIM stream queries to the target pane
(defmethod clim:stream-cursor-position ((stream capturing-stream))
  (clim:stream-cursor-position (slot-value stream 'target)))

(defmethod clim:stream-set-cursor-position ((stream capturing-stream) x y)
  (clim:stream-set-cursor-position (slot-value stream 'target) x y))

(defmethod stream-line-height ((stream capturing-stream) &key text-style)
  (stream-line-height (slot-value stream 'target) :text-style text-style))

(defmethod clim-internals::stream-write-object ((stream capturing-stream) object)
  (clim-internals::stream-write-object (slot-value stream 'target) object))

(defmethod clim:stream-recording-p ((stream capturing-stream))
  (clim:stream-recording-p (slot-value stream 'target)))

;; Delegate with-drawing-options to the target pane's medium
(defmethod clim:invoke-with-drawing-options ((stream capturing-stream) continuation &rest drawing-options)
  (apply #'clim:invoke-with-drawing-options (slot-value stream 'target) continuation drawing-options))

(defmethod clim-internals::stream-write-object ((stream capturing-stream) object)
  (clim-internals::stream-write-object (slot-value stream 'target) object))

;; Expose the real target pane for code that needs direct CLIM drawing
(defvar *echo-pane* nil
  "Bound to the actual CLIM pane during echo-echo display functions.
   Use this instead of *standard-output* for CLIM operations that
   require a real pane (drawing, surrounding-output-with-border, etc.).")

(defun echo-echo (frame pane)
  ;; Save scroll position before clearing
  (let ((old-x 0) (old-y 0))
    (multiple-value-setq (old-x old-y)
      (ignore-errors (clim:window-viewport-position pane)))
    (clim:window-clear pane)
    (let* ((capture (make-string-output-stream))
           (capturing-stream (make-instance 'capturing-stream
                                            :target pane :capture capture)))
      (let ((*standard-output* capturing-stream)
            (*trace-output* *standard-output*)
            (*error-output* *standard-output*)
            (*echo-pane* pane))
        (funcall (frame-pipe frame)))
      (setf (frame-captured-text frame) (get-output-stream-string capture))
      (force-output pane)
      ;; Restore scroll position — on first call old-x/old-y are (0,0), fine.
      (ignore-errors (setf (clim:window-viewport-position pane) (values old-x old-y))))))


