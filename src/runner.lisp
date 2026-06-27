(in-package :skyline-tool)
(cl:defvar *application-frame*)

(defun load-dump-into-mem (&optional (dump-file #p"/tmp/dump"))
  (let ((mem (make-array (expt 2 16) :element-type '(unsigned-byte 8))))
    (with-input-from-file (dump dump-file :element-type '(unsigned-byte 8))
      (loop for byte = (read-byte dump nil nil)
            for i from 0 below #x10000
            while byte
            do (setf (aref mem i) byte)))
    mem))

(defvar *run-script-frame* nil)

(clim:define-command-table script-list-save-as-menu
  :menu (("Text..." :command com-save-script-list-text)
         ("PDF..." :command com-save-script-list-pdf)))

(clim:define-command-table script-list-scripts-menu
  :menu (("Save As" :menu script-list-save-as-menu)
         (nil :divider :line)
         ("Close" :command com-close-frame)))

(clim:define-command-table script-list-edit-menu
  :menu (("Copy List" :command com-copy-script-list)))

(clim:define-command-table script-list-help-menu
  :menu (("Skyline-Tool Developers' Guide" :command com-open-dev-guide)
         ("Fountain Scripting Language Manual" :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool" :command com-about-skyline-tool)))

(clim:define-command-table script-list-menu-bar
  :menu (("Scripts" :menu script-list-scripts-menu)
         ("Edit" :menu script-list-edit-menu)
         ("Help" :menu script-list-help-menu)))

(clim:define-application-frame run-script-frame ()
  ((%decal-index :initform 0 :accessor decal-index :initarg :index))
  (:panes (script-list-pane :application :height 700 :width 450
                                         :display-function 'display-script-list)
          (interactor :interactor :height 125 :width 450))
  (:menu-bar script-list-menu-bar)
  (:layouts (default (clim:vertically () script-list-pane interactor))))

(defun all-script-names (&key (reloadp nil))
  (when reloadp
    (setf *assets-list* nil
          *asset-ids-seen* nil))
  (sort
   (remove-if-not #'script-asset-p
                  (copy-list (hash-table-keys (read-assets-list))))
   #'string<))

(clim:define-presentation-type script-name () :inherit-from 'string)

(clim:define-presentation-method clim:accept
    ((type script-name) stream view &key)
  (values (clim:completing-from-suggestions (stream)
            (dolist (script-name (all-script-names))
              (clim:suggest (subseq script-name (1+ (position #\/ script-name)))
                            script-name)))))

(clim:define-presentation-method clim:present
    (script-full-name (type script-name) stream view &key)
  (clim:with-text-face (stream (if (or (search "Global/" script-full-name)
                                       (search "Testing" script-full-name))
                                   :bold
                                   :roman))
    (destructuring-bind (area name)
        (split-sequence #\/ (subseq script-full-name (1+ (position #\/ script-full-name))))
      (format stream "~4t~a: “~a”"
              (cl-change-case:title-case area)
              (cl-ppcre:regex-replace "\\bDont\\b"
                                      (cl-change-case:title-case name)
                                      "Don't")))))

(defun run-script-in-playtest (script-full-name)
  (clim-sys:make-process (lambda ()
                           (uiop:run-program
                            (list "ptyxis"
                                  "-s"
                                  "--title" (format nil "Running ~a: ~a"
                                                    (cl-change-case:title-case *game-title*)
                                                    script-full-name)
                                  "--" "bin/playtest" 
                                  (format nil "NEWGAME=~a"
                                          (subseq script-full-name
                                                  (1+ (position #\/ script-full-name)))))))
                         :name (format nil "Running ~a" script-full-name)))

(define-run-script-frame-command (com-save-script-list-text :name t) ()
  "Save the script list as plain text."
  (let* ((text (with-output-to-string (s)
                 (let ((last-area nil))
                   (dolist (sn (all-script-names :reloadp t))
                     (terpri s)
                     (let ((area (let ((parts (split-sequence #\/ sn)))
                                   (elt parts (- (length parts) 2)))))
                       (unless (string-equal area last-area)
                         (format s "~%~a~%" (cl-change-case:title-case area))
                         (setf last-area area)))
                     (format s "  ~a~%" (cl-ppcre:regex-replace "\\bDont\\b"
                                                                (cl-change-case:title-case
                                                                 (subseq sn (1+ (position #\/ sn))))
                                                                "Don't"))))))
         (path (prompt-save-pathname "ScriptList.txt" "txt")))
    (when path
      (with-open-file (f path :direction :output :if-exists :supersede
                              :external-format :utf-8)
        (princ text f))
      (format *query-io* "~&Saved ~a~%" (namestring path)))))

(define-run-script-frame-command (com-save-script-list-pdf :name t) ()
  "Save the script list as a PDF via simple PostScript."
  (let* ((text (with-output-to-string (s)
                 (let ((last-area nil))
                   (dolist (sn (all-script-names :reloadp t))
                     (terpri s)
                     (let ((area (let ((parts (split-sequence #\/ sn)))
                                   (elt parts (- (length parts) 2)))))
                       (unless (string-equal area last-area)
                         (format s "~%~a~%" (cl-change-case:title-case area))
                         (setf last-area area)))
                     (format s "  ~a~%" (cl-ppcre:regex-replace "\\bDont\\b"
                                     (cl-change-case:title-case
                                      (subseq sn (1+ (position #\/ sn))))
                                     "Don't"))))))
         (path (prompt-save-pathname "ScriptList.pdf" "pdf")))
    (when path
      (let* ((ps-path (make-pathname :type "ps" :defaults path))
             (lines (count #\Newline text))
             (total-pages (max 1 (ceiling lines (/ (- 700 50) 10)))))
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
        (uiop:run-program (list "ps2pdf" (namestring ps-path) (namestring path))
                          :output nil :ignore-error-status t)
        (ignore-errors (delete-file ps-path))
        (format *query-io* "~&Saved ~a~%" (namestring path))))))

(define-run-script-frame-command (com-copy-script-list :name t) ()
  "Copy the script list to the clipboard as text."
  (let ((text (with-output-to-string (s)
                (let ((last-area nil))
                  (dolist (sn (all-script-names :reloadp t))
                    (terpri s)
                    (let ((area (let ((parts (split-sequence #\/ sn)))
                                  (elt parts (- (length parts) 2)))))
                      (unless (string-equal area last-area)
                        (format s "~%~a~%" (cl-change-case:title-case area))
                        (setf last-area area)))
                    (format s "  ~a~%" (cl-ppcre:regex-replace "\\bDont\\b"
                                                               (cl-change-case:title-case
                                                                (subseq sn (1+ (position #\/ sn))))
                                                               "Don't")))))))
    (when (> (length text) 0)
      (clime:publish-selection (clim:find-pane-named *application-frame* 'interactor)
                               :clipboard text 'string)
      (format *query-io* "~&Copied ~d characters to clipboard.~%" (length text)))))

;; Context menu commands for scripts

(define-run-script-frame-command (com-run-script-in-emulator :name t)
    ((script-full-name 'script-name :gesture :select :menu nil))
  (run-script script-full-name))

(define-run-script-frame-command (com-play-on-atarivox :name t)
    ((script-full-name 'script-name :gesture :select :menu nil))
  (if (fboundp 'read-script-interactive)
      (read-script-interactive
       (if (search "Scripts/" script-full-name)
           script-full-name
           (format nil "Scripts/~a" script-full-name)))
      (format *query-io* "~&Read Aloud requires AtariVox support.~%")))

(define-run-script-frame-command (com-edit-script-in-climacs :name t)
    ((script-full-name 'script-name :gesture :select
                                    :menu nil))
  (clim-sys:make-process
   (lambda ()
     (climacs:edit-file (format nil "Source/~a.fountain" script-full-name)
                        :process-name (format nil "Editing ~a" script-full-name)))
   :name (format nil "Edit ~a with Climacs" script-full-name)))

(define-run-script-frame-command (com-edit-script-in-emacs :name t)
    ((script-full-name 'script-name :gesture :select
                                     :menu nil))
  (clim-sys:make-process
   (lambda ()
     (uiop:run-program
      (list "emacsclient" "-n" (format nil "Source/~a.fountain" script-full-name))
      :output nil :ignore-error-status t))
   :name (format nil "Edit ~a with Emacs" script-full-name)))

(define-run-script-frame-command (com-edit-script-in-thiefmd :name t)
    ((script-full-name 'script-name :gesture :select
                                    :menu nil))
  (clim-sys:make-process
   (lambda ()
     (uiop:run-program
      (list "thiefmd" (format nil "Source/~a.fountain" script-full-name))
      :output nil :ignore-error-status t))
   :name (format nil "Edit ~a with ThiefMD" script-full-name)))

(define-run-script-frame-command (com-save-script-as-pdf-quick :name t)
    ((script-full-name 'script-name :gesture :select :menu nil))
  (let* ((default-name (format nil "~a.pdf"
                                (substitute #\_ #\/ script-full-name)))
         (save-dir (if (find-package :clim-simple-echo)
                       (clim-simple-echo::default-save-directory)
                       (merge-pathnames #p"Documents/" (user-homedir-pathname))))
         (default-full (namestring (merge-pathnames default-name save-dir)))
         (zenity-out (uiop:run-program
                      (list "zenity" "--file-selection" "--save"
                            (format nil "--filename=~a" default-full)
                            "--title=Save Script As PDF...")
                      :output :string :ignore-error-status t)))
    (when (and zenity-out (plusp (length (string-trim '(#\Newline #\Space) zenity-out))))
      (let ((path (string-trim '(#\Newline #\Space #\Tab) zenity-out)))
        (handler-case
            (progn
              (fountain->pdf script-full-name path)
              (format *query-io* "~&Saved ~a~%" path)
              (uiop:run-program (list "xdg-open" path) :output nil :ignore-error-status t))
          (error (e)
            (format *query-io* "~&PDF export error: ~a~%" e)))))))

(define-run-script-frame-command (com-print-script :name t)
    ((script-full-name 'script-name :gesture :select :menu nil))
  (let* ((pdf-path (format nil "/tmp/skyline-tool-print-~a.pdf"
                           (substitute #\_ #\/ script-full-name)))
         (printers (and (fboundp 'discover-printers) (discover-printers))))
    (unless printers
      (format *query-io* "~&No printers discovered.~%")
      (return-from com-print-script))
    (handler-case
        (fountain->pdf script-full-name pdf-path)
      (error (e)
        (format *query-io* "~&PDF generation error: ~a~%" e)
        (return-from com-print-script)))
    (format *query-io* "~&Select printer (1-~d):~%" (length printers))
    (dotimes (i (length printers))
      (format *query-io* "  ~d. ~a~%" (1+ i) (elt printers i)))
    (force-output *query-io*)
    (let* ((choice (clim:accept 'integer :prompt "Printer number :" :default 1))
           (printer (elt printers (1- choice))))
      (format *query-io* "~&Printing to ~a...~%" printer)
      (force-output)
      (uiop:run-program (list "lp" "-d" printer pdf-path)
                        :output nil :ignore-error-status t)
      (ignore-errors (delete-file pdf-path))
      (format *query-io* "~&Sent to printer.~%"))))

;; Print submenu populated at menu-display time
(defun %print-text-to-lp (printer-name text)
  "Convert TEXT to a PDF via ps2pdf and send it to PRINTER-NAME via lp."
  (let* ((base (format nil "EchoOutput-~d" (get-universal-time)))
         (ps-path (format nil "~a.ps" base))
         (pdf-path (format nil "~a.pdf" base))
         (lines (count #\Newline text))
         (total-pages (max 1 (ceiling lines (- 700 50)))))
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
    (format *query-io* "~&Sent ~a to ~a~%" pdf-path printer-name)))

(defun %print-menu-populate (command-table)
  "Populate COMMAND-TABLE with menu items for each discovered CUPS printer.
   Shows display names in the menu, but sends to the queue name via lp."
  ;; Remove old items from previous calls
  (ignore-errors
    (clim:remove-menu-item-from-command-table command-table "Select Printer...")
    (clim:remove-menu-item-from-command-table command-table "No printers found")
    (clim:remove-menu-item-from-command-table command-table "Default Printer (lpr)")
    (dolist (p (discover-printers))
      (ignore-errors
         (clim:remove-menu-item-from-command-table command-table p)))))
  ;; Refresh printer list (cache managed by discover-printers)
  (let* ((queues (discover-printers))
         (printers (when queues (discover-printers-with-names))))
    (if (null printers)
        (clim:add-menu-item-to-command-table
         command-table "Default Printer (lpr)" :function
         (lambda (g n)
           (declare (ignore g n))
           (%print-text-to-lp "lpr"
             (or (ignore-errors
                   (when (boundp '*application-frame*)
                     (typecase *application-frame*
                       (clim-simple-echo::simple-echo
                        (clim-simple-echo::frame-captured-text *application-frame*))
                       (run-script-frame
                        (format nil "Script: ~a"
                                (ignore-errors
                                  (clim:frame-pretty-name *application-frame*)))))))
                 (format nil "Skyline-Tool print at ~a~%" (get-universal-time))))))
        (dolist (pair printers)
          (let ((queue-name (car pair))
                (display-name (cdr pair)))
            (clim:add-menu-item-to-command-table
             command-table display-name :function
             (lambda (gesture numeric-arg)
               (declare (ignore gesture numeric-arg))
               (%print-text-to-lp queue-name
                 (or (ignore-errors
                       (when (boundp '*application-frame*)
                         (typecase *application-frame*
                           (clim-simple-echo::simple-echo
                            (clim-simple-echo::frame-captured-text *application-frame*))
                           (run-script-frame
                            (format nil "Script: ~a"
                                    (ignore-errors
                                       (clim:frame-pretty-name *application-frame*)))))))
                     (format nil "Skyline-Tool print at ~a~%" (get-universal-time)))))
              :after :end)))))

(defmethod display-script-list (frame (pane clim:pane))
  (clim:with-text-face (pane :bold)
    (format pane "Click a script to run it in playtest mode~2%"))
  (let ((last-area nil))
    (dolist (script-name (all-script-names :reloadp t))
      (terpri pane)
      (let ((area (let ((parts (split-sequence #\/ script-name)))
                    (elt parts (- (length parts) 2)))))
        (unless (string-equal area last-area)
          (clim:with-text-face (pane :bold)
            (clim:with-text-size (pane :large)
              (format pane "~%~a~%" (cl-change-case:title-case area))))
          (setf last-area area)))
      (clim:present script-name 'script-name :stream pane))
    (format pane "~2%")))

(defun run-script (&optional SCRIPT-TO-RUN)
  "Choose SCRIPT-TO-RUN from a menu and launch playtest.

@table @asis
@item SCRIPT-TO-RUN
Optional script full name (e.g. @code{\"Scripts/Global/Welcome\"}).
When omitted or NIL, opens a CLIM frame for interactive selection.
@item Side Effects
Launches an emulator playtest session for the specified script.
@end table"
  (if SCRIPT-TO-RUN
      (progn
        (when *run-script-frame*
          (clim:frame-exit *run-script-frame*))
        (run-script-in-playtest
         (if (search "Scripts/" SCRIPT-TO-RUN)
             SCRIPT-TO-RUN
             (format nil "Scripts/~a" SCRIPT-TO-RUN))))
      
      (let ((frame (clim:make-application-frame 'run-script-frame)))
        (let ((*run-script-frame* frame))
          (setf (clim:frame-pretty-name frame)
                (window-title "Script Runner"))
          (clim-sys:make-process (lambda () (clim:run-frame-top-level frame))
                                 :name "Script Runner (launcher)")))))

;;; ============================================================
;;; Hollywood-format screenplay PDF generation (Fountain → PDF)
;;; ============================================================

(defun %fountain-script-path (script-full-name)
  "Return the absolute pathname for a script name like \"Scripts/Global/Welcome\"."
  (merge-pathnames (make-pathname :directory '(:relative "Source")
                                  :name (format nil "~a.fountain" script-full-name))
                   (uiop:getcwd)))

(defun %read-fountain-lines (pathname)
  "Read all lines from a fountain file, preserving blank lines."
  (with-open-file (f pathname :external-format :utf-8)
    (loop for line = (read-line f nil nil) while line collect line)))

(defparameter +uppercase-letters+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defun %fountain-element-type (line in-notes prev-type)
  "Classify a Fountain LINE. Returns (values type stripped-text in-notes)."
  (flet ((all-caps-p (s)
           (and (> (length s) 1)
                (every (lambda (c) (or (char= c #\Space) (find c +uppercase-letters+)))
                       (string-trim '(#\Space #\Tab) s))))
         (scene-p (s)
           (and (>= (length s) 3)
                (member (subseq s 0 3) '("INT" "EXT") :test #'string-equal))))
    (cond (in-notes
           (if (search "]]" line)
               (values :action (subseq line 0 (search "]]" line)) nil)
               (values :notes line t)))
          ((and (>= (length line) 2) (string= (subseq line 0 2) "[["))
           (let ((rest (subseq line 2)))
             (if (search "]]" rest)
                 (values :notes (subseq rest 0 (search "]]" rest)) nil)
                 (values :notes (subseq line 2) t))))
          ((scene-p line)
           (values :scene-heading (string-upcase line) nil))
          ((and (> (length line) 1) (char= (char line 0) #\>))
           (values :transition (string-upcase (string-trim '(#\Space) (subseq line 1))) nil))
          ((and (all-caps-p line) (member prev-type '(:blank :action :scene-heading)))
           (values :character (string-trim '(#\Space #\Tab) line) nil))
          ((and (> (length line) 0) (char= (char line 0) #\())
           (values :parenthetical line nil))
          ((and (> (length line) 2) (string= (subseq line 0 2) "=="))
           (values :page-break line nil))
          ((and (>= (length line) 2) (string= (subseq line 0 2) "**")
                (string= (subseq line (1- (length line)) (length line)) "**")
                (>= (length line) 5))
           (values :centered (string-trim '(#\*) line) nil))
          ((zerop (length line))
           (values :blank "" nil))
          (t
           (values :action line nil)))))

(defun %fountain-parse (lines)
  "Parse a list of Fountain lines into a list of element plists."
  (let ((elements nil)
        (in-notes nil))
    (dolist (line lines)
      (let ((prev (if elements (getf (car elements) :type) :blank)))
        (multiple-value-bind (type text new-in-notes)
            (%fountain-element-type line in-notes prev)
          (setf in-notes new-in-notes)
          ;; Merge consecutive :action lines
          (if (and (eq type :action) (eq prev :action))
              (setf (getf (car elements) :text)
                    (concatenate 'string (getf (car elements) :text) " " text))
              (push (list :type type :text text) elements)))))
    (nreverse elements)))

(defun %ps-escape-screenplay (str)
  "Escape special PostScript characters in STR."
  (with-output-to-string (out)
    (loop for c across str
          do (case c
               ((#\( #\) #\\) (princ "\\" out) (princ c out))
               (#\© (princ "(c)" out))
               (#\— (princ "--" out))
               (#\… (princ "..." out))
               (#\" (princ "\\\"" out))
               ((#\™) (princ "TM" out))
               (t (princ c out))))))

(defun %fountain->ps (ps elements title-text date-str author-str pdf-path)
  "Write a Hollywood-format screenplay PostScript to PS stream.
   Format: US Letter, Courier 12pt, standard screenplay margins."
  (let* ((page-w 612) (page-h 792)
         (lm 108)       ; left margin 1.5"
         (rm 72)        ; right margin 1"
         (tm 72)        ; top margin 1"
         (bm 72)        ; bottom margin 1"
         (font-size 12)
         (lh 14.4)      ; line height (~12pt leading)
         (cw (* font-size 0.6))  ; approx Courier char width
         (body-cw (floor (- page-w lm rm) cw))
         (char-x 266)   ; character name at 3.7"
         (dialogue-x 180)  ; dialogue at 2.5"
         (dialogue-cw (floor (- page-w dialogue-x rm) cw))
         (paren-x 223)   ; parenthetical at 3.1"
         (y (- page-h tm lh))
         (page-num 0)
         (scene-heading nil)
         (last-character ""))

    (labels ((header (&optional extra)
               (format ps "gsave~%")
               (format ps "newpath 0 ~d moveto ~d ~d lineto stroke~%"
                       (- page-h tm 14) (- page-w lm) (- page-h tm 14))
               (skyline-tool::write-ps-header-icon ps)
               (format ps " 56 22 moveto /Helvetica-Bold-ISOLatin1 findfont 10 scalefont setfont 0.2 0.2 0.25 setrgbcolor (~a) show~%"
                       (skyline-tool::escape-ps-string title-text))
               ;; Page number top right
               (format ps " /Helvetica-ISOLatin1 findfont 10 scalefont setfont 0.4 0.4 0.45 setrgbcolor~%")
               (format ps " ~d ~d moveto (~d.) show~%" (- page-w rm 30) (- page-h tm 12) page-num)
               (when extra (princ extra ps))
               (format ps "grestore~%"))
             (footer ()
               (format ps "gsave~%")
               (format ps " /Helvetica-ISOLatin1 findfont 7 scalefont setfont 0.6 0.6 0.6 setrgbcolor~%")
               (format ps " 50 15 moveto (Page ~d -- Exported by Skyline-Tool on ~a) show~%"
                       page-num (skyline-tool::escape-ps-string date-str))
               (format ps "grestore~%"))
             (new-page ()
               (footer)
               (format ps "showpage~%")
               (incf page-num)
               (setf y (- page-h tm lh))
               (header))
             (emit (x text &optional (size font-size))
               (let ((esc (%ps-escape-screenplay text)))
                 (format ps "~d ~d moveto /Courier-ISOLatin1 findfont ~d scalefont setfont (~a) show~%"
                         x y size esc)))
             (need-lines (n)
               (unless (>= y (+ bm (* n lh)))
                 (new-page)))
             (emit-dialogue-line (text)
               (need-lines 1)
               (when (>= y bm)
                 (emit dialogue-x text)))
             (emit-action (text)
               (dolist (line (%reflow-text text body-cw))
                 (need-lines 1)
                 (when (>= y bm)
                   (emit lm line))))
             (%reflow-text (text chars-per-line &optional (prefix ""))
               "Reflow TEXT to fit CHARS-PER-LINE, returning a list of strings."
               (let ((words (split-sequence #\Space text))
                     (lines nil)
                     (line prefix))
                 (dolist (word words)
                   (if (>= (+ (length line) (length word) 1) chars-per-line)
                       (progn (push (string-trim " " line) lines)
                              (setf line (concatenate 'string prefix word)))
                       (setf line (concatenate 'string " " line " " word))))
                 (unless (zerop (length line))
                   (push (string-trim " " line) lines))
                 (nreverse lines))))
      ;; --- Title Page ---
      (header)
      (format ps "/Courier-ISOLatin1 findfont 12 scalefont setfont~%")
      (let ((title-y (- (/ page-h 2) 48)))
        (dolist (line (%reflow-text title-text 50))
          (format ps "~d ~d moveto (~a) show~%" (/ page-w 2) title-y
                  (string-upcase (%ps-escape-screenplay line)))
          (decf title-y lh))
        (when (and author-str (> (length author-str) 0))
          (format ps "~d ~d moveto (~a) show~%" (/ page-w 2) (- title-y 24)
                  (%ps-escape-screenplay (format nil "by ~a" author-str)))))
      (format ps "showpage~%")
      (setf y (- page-h tm lh) page-num 1)

      ;; --- Body pages ---
      (header)
      (dolist (elem elements)
        (let* ((type (getf elem :type))
               (text (getf elem :text)))
          (case type
            (:blank
             (decf y lh))
            (:page-break
             (new-page))
            (:notes
             (need-lines 1)
             (when (>= y bm)
               (format ps "~d ~d moveto /Times-Italic-ISOLatin1 findfont 11 scalefont setfont (~a) show~%"
                       lm y (%ps-escape-screenplay text)))
             (decf y lh))
            (:centered
             (need-lines 1)
             (when (>= y bm)
               (let ((cx (/ (- page-w lm rm) 2)))
                 (format ps "~d ~d moveto (~a) show~%" cx y (%ps-escape-screenplay text))))
             (decf y lh))
            (:scene-heading
             (need-lines 2)
             (setf scene-heading elem
                   last-character "")
             (when (>= y bm)
               (format ps "gsave /Courier-ISOLatin1 findfont 12 scalefont setfont~%")
               (format ps "~d ~d moveto (~a) show grestore~%" lm y
                       (%ps-escape-screenplay text)))
             (decf y lh))
            (:transition
             (need-lines 2)
             (when (>= y bm)
               (let ((tx (- page-w rm (* (length text) cw))))
                 (format ps "gsave /Courier-ISOLatin1 findfont 12 scalefont setfont~%")
                 (format ps "~d ~d moveto (~a) show grestore~%" tx y
                         (%ps-escape-screenplay text))))
             (decf y lh))
            (:character
             (setf last-character text)
             (need-lines 2)
             (when (>= y bm)
               (format ps "gsave /Courier-ISOLatin1 findfont 12 scalefont setfont~%")
               (format ps "~d ~d moveto (~a) show grestore~%" char-x y
                       (%ps-escape-screenplay text)))
             (decf y lh))
            (:parenthetical
             (need-lines 1)
             (when (>= y bm)
               (format ps "gsave /Courier-ISOLatin1 findfont 12 scalefont setfont~%")
               (format ps "~d ~d moveto (~a) show grestore~%" paren-x y
                       (%ps-escape-screenplay text)))
             (decf y lh))
            (:dialogue
             (dolist (line (%reflow-text text dialogue-cw))
               (emit-dialogue-line line)))
             (:action
               (emit-action text)))))

      ;; Last page footer
      (footer)
      (format ps "showpage~%"))))

(defun %collect-script-metadata (elements)
  "Extract title, author, and date from parsed fountain metadata."
  (let ((title "") (author "") (date ""))
    (dolist (e elements (values title author date))
      (when (eq (getf e :type) :metadata)
        (let ((k (getf e :key)) (v (getf e :value)))
          (cond ((member k '("title") :test 'string=) (setf title v))
                ((member k '("author") :test 'string=) (setf author v))
                ((member k '("date") :test 'string=) (setf date v))))))))

(defun %find-meta (elements key)
  "Find metadata VALUE for KEY in parsed elements."
  (dolist (e elements)
    (when (and (eq (getf e :type) :metadata)
               (string-equal (getf e :key) key))
      (return (getf e :value)))))

(defun fountain->pdf (script-full-name pdf-pathname)
  "Generate a Hollywood-format screenplay PDF from a Fountain file at PDF-PATHNAME."
  (let* ((src-path (%fountain-script-path script-full-name))
         (lines (%read-fountain-lines src-path))
         (elements (%fountain-parse lines))
         (meta-title (getf (find-if (lambda (e) (eq (getf e :type) :title)) elements) :text))
         (meta-author (%find-meta elements "author"))
         (game-name (if (boundp '*game-title*) (string-capitalize *game-title*) "Skyline-Tool"))
         (script-title (or meta-title
                           (format nil "~a: ~a" game-name script-full-name)))
         (author (or meta-author (user-real-name)))
         (date-str (multiple-value-bind (s m h d mo y) (get-decoded-time)
                     (declare (ignore s))
                     (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" y mo d h m)))
         (ps-path (make-pathname :type "ps" :defaults pdf-pathname)))
    (with-open-file (ps ps-path :direction :output :if-exists :supersede
                                :external-format :utf-8)
      (format ps "%!PS-Adobe-3.0~%")
      (format ps "<< /PageSize [612 792] >> setpagedevice~%")
      (skyline-tool::write-ps-font-encodings ps)
      (%fountain->ps ps elements script-title date-str author pdf-pathname))
    (uiop:run-program (list "ps2pdf" (namestring ps-path) pdf-pathname)
                      :output nil :ignore-error-status t)
    (ignore-errors (delete-file ps-path))
    (when (probe-file pdf-pathname)
      (format *query-io* "~&Saved ~a~%" pdf-pathname))))

(defmethod clim:text-size ((stream swank/gray::slime-output-stream) size &rest _)
  (declare (ignore _)))

(defmethod clim:stream-vertical-spacing ((stream swank/gray::slime-output-stream)) 1)

(defmethod clim:stream-cursor-position ((stream swank/gray::slime-output-stream)) 1)

(defmethod clim:invoke-with-output-recording-options
    ((stream swank/gray::slime-output-stream) continuation _ __)
  (funcall continuation stream))

(defmethod clim-internals::invoke-with-pristine-viewport
    ((stream swank/gray::slime-output-stream) continuation)
  (funcall continuation stream))

(defmethod (setf clim::.stream-cursor-position-star.)
    (value _ (stream swank/gray::slime-output-stream)))

(defmethod clim:sheet-direct-mirror ((stream swank/gray::slime-output-stream)))

(defmethod clim:sheet-native-transformation ((stream swank/gray::slime-output-stream)))

(defmethod clim:untransform-region (_ (stream swank/gray::slime-output-stream)))

(defmethod clim:stream-close-text-output-record ((stream swank/gray::slime-output-stream)))

(defmethod clim:stream-drawing-p ((stream swank/gray::slime-output-stream)) nil)

(defmethod clim-internals::sheet-native-region* ((stream swank/gray::slime-output-stream))
  stream)

(defmethod clim:invoke-with-new-output-record ((stream swank/gray::slime-output-stream)
                                               continuation record-type &rest initargs)
  (funcall continuation stream (apply #'make-instance record-type initargs))))

