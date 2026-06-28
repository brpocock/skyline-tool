;;; Phantasia SkylineTool/src/launcher.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC.

(in-package :skyline-tool)

;;; Exported variables for use across the toolchain
(defvar *launcher-frame* nil "Reference to the launcher frame.")

;; --- Command Tables (defined first, just store symbol references) ---

(clim:define-command-table launcher-tool-menu
  :menu (("Edit Project..." :command com-edit-project.json :shortcut :ctrl-x-p)
         ("Edit Preferences..." :command com-edit-skyline-config-prefs :shortcut :ctrl-comma)
         (nil :divider :line)
         ("All Resources..." :command com-show-all-resources :shortcut :ctrl-o)
         ("Run in Emulator" :command com-about-skyline-tool :shortcut :ctrl-p)
         (nil :divider :line)
         ("Quit" :command com-quit-skyline-tool :shortcut :ctrl-x-c)))

(clim:define-command-table launcher-help-menu
   :menu (("How to Use the Launcher" :command com-help-for-window)
          ("Skyline-Tool Developers' Guide" :command com-open-dev-guide)
          ("Skyline-Tool Scripting Guide" :command com-open-scripting-guide)
          (nil :divider :line)
           ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table launcher-menu-bar
  :menu (("Skyline-Tool" :menu launcher-tool-menu)
         ("Help" :menu launcher-help-menu)))

(clim:define-command-table edit-menu
  :menu (("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table help-menu
  :menu (("How To Do Things In This Window..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-scripting-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

;; --- Launcher Frame ---

(clim:define-application-frame launcher-frame ()
  ((%decal-index :initform 0 :accessor decal-index :initarg :index)
   (%saved-scroll-y :initform nil :accessor launcher-saved-scroll-y))
  (:panes (menu-list-pane :application :height 700 :width 450
                                       :display-function 'display-launcher-menu)
          (interactor :interactor :height 125 :width 450
                                  :max-height 125))
  (:menu-bar launcher-menu-bar)
  (:icon (skyline-tool-icon))
  (:layouts (default (clim:vertically () menu-list-pane interactor))))

;; --- Launcher Commands (after frame so define-launcher-frame-command is available) ---

(define-launcher-frame-command (com-quit-skyline-tool :menu t :name t) ()
   (bye))

(define-launcher-frame-command (com-edit-project.json :menu t :name t) ()
  (if (boundp '*machine*)
      (let* ((machine-dir (machine-directory-name))
             (project-file (format nil "Project.~a.json" machine-dir))
             (project-path (or (probe-file project-file)
                               (probe-file (make-pathname :directory ".." :name project-file :type "json"))
                               (probe-file (make-pathname :directory '(:relative "..") :name project-file :type "json")))))
        (if project-path
            (clim-sys:make-process
             (lambda ()
               (handler-case (climacs:edit-file (namestring project-path)
                                                :process-name (format nil "Editing Project (~a)" machine-dir))
                 (error (e)
                   (format *query-io* "~&Climacs error: ~a~%" e))))
             :name (format nil "Editing Project (~a)" machine-dir))
            (format *query-io* "~&Project file not found: ~a~%" project-file)))
      (load-project.json nil #'(lambda () (funcall (symbol-function 'com-edit-project.json))))))

(define-launcher-frame-command (com-edit-skyline-config-prefs :menu t :name t) ()
  (let* ((prefs-file (make-pathname :directory (list :relative ".config" "Skyline-Tool"
                                                     *game-title*
                                                     (machine-directory-name))
                                    :defaults (user-homedir-pathname)
                                    :name "Preferences" :type "lisp")))
    (clim-sys:make-process
     (lambda ()
       (error "Should open in Emacs ... not workding yet"))
     :name "Editing Skyline-Tool Preferences")))

;; --- Shared Commands (in global command table so all frames can use them) ---

(clim:define-command (com-cut :command-table clim-internals::global-command-table) ()
  (if (not (boundp '*application-frame*))
      (error  "~&Nothing to cut.~%")
      (error  "~&Cut is not yet implemented. Use Copy then delete manually.~%")))
(clim:define-command (com-copy :command-table clim-internals::global-command-table) ()
  "Copy: publish selection via the resource-specific clipboard system."
  (let* ((frame (or (and (boundp '*application-frame*) *application-frame*)
                    (let ((found nil))
                      (ignore-errors
                       (clim:map-over-frames (lambda (f)
                                               (when (typep f 'clim:application-frame)
                                                 (setf found f)))
                                             :port (clim:find-port)))
                      found)))
         (*application-frame* frame))
    (if frame
        (handler-case (publish-current-resource)
          (error (e)
            (error  "~&Copy error: ~a~%" e)))
        (error  "~&Nothing to copy.~%"))))
(clim:define-command (com-paste :command-table clim-internals::global-command-table) ()
  "Paste: request :clipboard as 'string."
  (if (not (boundp '*application-frame*))
      (error  "~&No active frame to paste into.~%")
      (handler-case
          (let* ((frame *application-frame*)
                 (interactor (or (clim:find-pane-named frame 'interactor)
                                 *standard-input*)))
            (multiple-value-bind (string type)
                (clime:request-selection interactor :clipboard 'string)
              (declare (ignore type))
              (if string
                  (error  "~&Paste: got ~d chars.~%" (length string))
                  (error  "~&Paste: clipboard empty.~%"))))
        (error (e)
          (error  "~&Paste error: ~a~%" e)))))
(clim:define-command (com-find :command-table clim-internals::global-command-table) ()
  (error  "~&Find is not yet implemented.~%"))
;; --- About / Dev Guide ---
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +compile-date+
    (multiple-value-bind (s m h d mo y) (get-decoded-time)
      (declare (ignore s))
      (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" y mo d h m)))
  (defparameter +compile-user+ (ignore-errors (user-real-name)))
  (defparameter +compile-machine+ (ignore-errors (machine-instance)))
  (defparameter +compile-machine-type+ (ignore-errors (machine-type)))
  (defparameter +compile-lisp-type+ (ignore-errors (lisp-implementation-type)))
  (defparameter +compile-lisp-version+ (ignore-errors (lisp-implementation-version)))
  (defparameter +compile-software-type+ (ignore-errors (software-type)))
  (defparameter +compile-software-version+ (ignore-errors (software-version)))
  (defparameter +compile-site-name+ (ignore-errors (short-site-name)))
  (defparameter +compile-long-site-name+ (ignore-errors (long-site-name))))

(clim:define-command (com-open-dev-guide :command-table clim-internals::global-command-table) ()
  (let ((html-index (asdf:system-relative-pathname
                     :skyline-tool
                     #p"../Dist/7800/PhantasiaDevGuide-html/index.html")))
    (if (probe-file html-index)
        (uiop:run-program (list "xdg-open" (namestring html-index)) :output nil)
        (progn
          (error  "~&The Developers' Guide has not been built.~%")
          (if (clim:accept 'boolean :prompt "Build it now" :default t)
              (progn
                (error  "~&Building documentation…~%")
                (force-output *query-io*)
                (clim-sys:make-process
                 (lambda ()
                   (uiop:run-program (list "ptyxis" "-s" "--title" "Building Dev Guide"
                                           "--" "make" "doc")
                                     :output nil :ignore-error-status t))
                 :name "Building Dev Guide"))
              (error  "~&Run ‘make doc’ in a terminal when ready.~%"))))))

(clim:define-command (com-open-fountain-manual :command-table clim-internals::global-command-table) ()
  (let ((pdf-path (asdf:system-relative-pathname
                   :skyline-tool
                   #p"../Manual/FountainScripting.pdf")))
    (if (probe-file pdf-path)
        (uiop:run-program (list "xdg-open" (namestring pdf-path)) :output nil)
        (error  "~&Fountain Scripting Language Manual not found. Build it from Manual/FountainScripting.tex~%"))))

(defvar *devguide-html-dir* nil
  "Cached absolute path to the Developer Guide HTML directory.")

(defun devguide-html-page (node-name)
  "Open the DEV Guide HTML page for NODE-NAME (a string like \"Tools-GUI-Launcher\")."
  (let* ((html-dir (or *devguide-html-dir*
                       (setf *devguide-html-dir*
                             (namestring
                              (asdf:system-relative-pathname
                               :skyline-tool
                               #p"../Dist/7800/PhantasiaDevGuide-html/")))))
         (page (format nil "~a~a.html" html-dir node-name)))
    (if (probe-file page)
        (uiop:run-program (list "xdg-open" page) :output nil)
        (error  "~&Dev Guide HTML not found; run ‘make doc’ first.~%"))))

(clim:define-command (com-help-for-window :command-table clim-internals::global-command-table) ()
   "Open the Developer Guide section relevant to the current window."
   (if (boundp '*application-frame*)
       (let ((frame *application-frame*))
         (devguide-html-page
          (typecase frame
            (launcher-frame "Tools-GUI-Launcher")
            ((or anim-seq-editor-frame anim-seq-assign-frame anim-seq-assigns-frame
                 show-tileset-frame choose-sequence-frame anim-buffer-frame show-decal-frame)
             "Tools-GUI-Animation-Editor")
            (read-script-frame "Tools-GUI-All-Resources")
            (otherwise
             (cond ((and (find-package :clim-simple-echo)
                         (typep frame (find-class 'clim-simple-echo::simple-echo nil)))
                    "Tools-GUI-Output-Windows")
                   ((typep frame 'clim-debugger::clim-debugger)
                    "Tests-Crash-Detection-and-Core-Dumps")
                   (t "Tools-Skyline-Tool-GUI"))))))
       (error  "~&No active window.~%")))

(clim:define-command (com-open-scripting-guide :command-table clim-internals::global-command-table) ()
  "Open the Skyline-Tool Scripting Guide (Fountain) PDF."
  (let ((pdf-path (asdf:system-relative-pathname
                   :skyline-tool
                   #p"../Manual/FountainScripting.pdf")))
    (if (probe-file pdf-path)
        (uiop:run-program (list "xdg-open" (namestring pdf-path)) :output nil)
        (error "Unimplemented: compile fountain scripting gudie"))))

(defun %about-data ()
  "Return a plist of About dialog data for display and PDF export."
  (let* ((tz-str (multiple-value-bind (s m h d mo y tz dst-p tz-name) (get-decoded-time)
                   (declare (ignore s m h d mo y tz-name))
                   (let* ((total-offset (abs tz))
                          (tz-h (floor total-offset 3600))
                          (tz-m (round (/ (mod total-offset 3600) 60)))
                          (tz-label (cond ((and dst-p (<= -28800 tz -25200)) "PDT")
                                          ((<= -28800 tz -25200) "PST")
                                          (dst-p "DST")
                                          (t ""))))
                     (format nil "UTC~:[+~;-~]~d:~2,'0d~@[ (~a)~]"
                             (minusp tz) tz-h tz-m
                             (not (emptyp tz-label)) tz-label))))
         (version (or (ignore-errors (asdf:component-version (asdf:find-system :skyline-tool))) "0.9.1"))
         (compiled (format nil "Compiled ~a by ~a on ~a~@[ at ~a~]"
                           (or (ignore-errors +compile-date+) (format nil "~A" (get-universal-time)))
                           (or (ignore-errors +compile-user+) (user-login-name))
                           (or (ignore-errors +compile-machine+) (machine-instance))
                           (unless (string-equal (or (ignore-errors +compile-site-name+) "")
                                                 (or (ignore-errors +compile-long-site-name+) ""))
                             (or (ignore-errors +compile-long-site-name+) "")))))
    (list :version version :compiled compiled :timestamp
          (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d ~a"
                  (nth-value 5 (get-decoded-time)) (nth-value 4 (get-decoded-time))
                  (nth-value 3 (get-decoded-time)) (nth-value 2 (get-decoded-time))
                  (nth-value 1 (get-decoded-time)) tz-str)
          :user (user-real-name) :machine (machine-instance)
          :cpu (format nil "~a — ~a" (machine-type) (machine-version)) :os (format nil "~a ~a" (software-type) (software-version))
          :lisp (format nil "~a ~a" (lisp-implementation-type) (lisp-implementation-version))
          :site (or (ignore-errors +compile-long-site-name+) "Unknown"))))

(defun %about-pdf (ps-path)
  "Write PostScript for the About dialog to PS-PATH."
  (let ((data (%about-data))
        (page-width 612) (page-height 792) (icon-size 72))
    (with-open-file (ps ps-path :direction :output :if-exists :supersede :external-format :utf-8)
      (format ps "%!PS-Adobe-3.0~%")
      (write-ps-docinfo ps "About Skyline-Tool" "Skyline-Tool"
                        (format nil "~a on ~a" (getf data :user) (getf data :machine)))
      (format ps "<< /PageSize [~d ~d] >> setpagedevice~%" page-width page-height)
      (write-ps-font-encodings ps)
      (format ps "%%Page: 1 1~%")
      ;; Header bar
      (write-ps-header-bar ps "About Skyline-Tool" "" "" "Skyline-Tool" 1 1)
      ;; Icon at large size (72pt) with "Skyline-Tool" in 72-point Royal Blue beside it
      (let ((icon-path (asdf:system-relative-pathname :skyline-tool "../Tools/skyline-tool-icon-64.png")))
        (when (probe-file icon-path)
          (format ps "gsave 72 620 translate ~d ~d scale~%" (/ icon-size 64) (/ icon-size 64))
          (ignore-errors
            (let* ((png (png-read:read-png-file (namestring icon-path)))
                   (w (png-read:width png)) (h (png-read:height png))
                   (data (png-read:image-data png))
                   (dims (array-dimensions data))
                   (bpp (if (= (length dims) 3) (third dims) 4))
                   (rgb (make-array (list w h 3) :element-type '(unsigned-byte 8))))
              (dotimes (y h)
                (dotimes (x w)
                  (if (= (length dims) 3)
                      (progn
                        (setf (aref rgb x y 0) (aref data x y 0)
                              (aref rgb x y 1) (aref data x y 1)
                              (aref rgb x y 2) (aref data x y 2)))
(let ((idx (aref data x y)))
                         (when idx
                           (setf (aref rgb x y 0) idx
                                 (aref rgb x y 1) idx
                                 (aref rgb x y 2) idx))))))
              (write-ps-image ps rgb w h w h)))
          (format ps "grestore~%"))
        (format ps "gsave 72 620 translate
  64 64 scale
  0 0 0.5 setrgbcolor
  newpath 0 0 64 64 rectfill
  1 1 1 setrgbcolor
  /Times-Bold-ISOLatin1 findfont 12 scalefont setfont
  8 24 moveto (ST) show
  grestore~%"))
      (format ps "/Times-Bold-ISOLatin1 findfont 72 scalefont setfont~%")
      (format ps "0.0 0.2 0.6 setrgbcolor~%")
      (format ps "168 650 moveto (Skyline-Tool) show~%")
      (format ps "0 0 0 setrgbcolor~%")
      ;; Body text
      (let ((y 540) (lh 16))
        (format ps "/Times-Roman-ISOLatin1 findfont 11 scalefont setfont~%")
        (format ps "72 ~d moveto (Version ~a) show~%" y (getf data :version)) (decf y lh) (decf y lh)
        (format ps "72 ~d moveto (Copyright ) show~%" y)
        (format ps "/Times-Roman-ISOLatin1 findfont 11 scalefont setfont")
        (format ps " (\\251 2014-2024 Bruce-Robert Pocock) show~%") (decf y lh)
        (format ps "/Times-Roman-ISOLatin1 findfont 11 scalefont setfont")
        (format ps "72 ~d moveto (Copyright \\251 2024-2026 Interworldly Adventuring, LLC) show~%" y)
        (decf y lh) (decf y lh)
        (format ps "72 ~d moveto (~a) show~%" y (escape-ps-string (getf data :compiled)))
        (decf y lh) (decf y lh)
        ;; Tabular data: bold keys, values right-aligned
        (format ps "/Times-Bold-ISOLatin1 findfont 10 scalefont setfont~%")
        (let ((x-label 84) (x-value 200))
          (flet (($ (label value)
                   (format ps "72 ~d moveto (~a) show~%" y (escape-ps-string (format nil "~a:" label)))
                   (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont
  ~d ~d moveto (~a) show~%" x-value y (escape-ps-string (or value "?")))
                   (format ps "/Times-Bold-ISOLatin1 findfont 10 scalefont setfont~%")
                   (decf y lh)))
            ($ "Currently" (getf data :timestamp))
            ($ "User" (getf data :user))
            ($ "Machine" (getf data :machine))
            ($ "CPU" (getf data :cpu))
            ($ "OS" (getf data :os))
            ($ "Lisp" (getf data :lisp))
            ($ "Site" (getf data :site))))
        (format ps "showpage~%")))))

(clim:define-command (com-about-skyline-tool :command-table clim-internals::global-command-table) ()
  (clim-simple-echo:run-in-simple-echo
   (lambda ()
     (let* ((data (%about-data))
            (pane clim-simple-echo::*echo-pane*)
            (pane-width (if pane (clim:bounding-rectangle-width (clim:sheet-region pane)) 600)))
       ;; Skyline-Tool heading: icon + royal blue title (large, beside icon)
       (clim:with-room-for-graphics (*standard-output* :height 80)
         (let ((icon-path (asdf:system-relative-pathname :skyline-tool "../Tools/skyline-tool-icon-64.png")))
           (when (probe-file icon-path)
             (clim:draw-pattern* *standard-output*
                                 (clim:make-pattern-from-bitmap-file icon-path) 10 4)))
(clim:with-text-face (*standard-output* :bold)
           (clim:with-text-size (*standard-output* :large)
             (clim:with-drawing-options (*standard-output* :ink (clim:make-rgb-color 0 0.2 0.6))
               (format *standard-output* "     Skyline-Tool")))))
       ;; Horizontal royal-blue rule below icon area
       (terpri) (terpri)
       (clim:draw-rectangle* pane 0 (nth-value 1 (clim:stream-cursor-position *standard-output*))
                             (- pane-width 10) (1+ (nth-value 1 (clim:stream-cursor-position *standard-output*)))
                             :ink (clim:make-rgb-color 0 0.2 0.6) :filled t)
       (terpri) (terpri)
       ;; Version + copyright + compilation info
       (format t "  Version ~a~%~%" (getf data :version))
       (format t "  Copyright © 2014-2024 Bruce-Robert Pocock~%")
       (format t "  Copyright © 2024-2026 Interworldly Adventuring, LLC~%~%")
       (format t "  ~a~%~%" (getf data :compiled))
       ;; Two-column table using CLIM formatting-table
       (clim:formatting-table (*standard-output*)
         (flet ((r (label value)
                  (clim:formatting-row (*standard-output*)
                    (clim:formatting-cell (*standard-output*)
                      (clim:with-text-face (*standard-output* :bold)
                        (format *standard-output* "~a" (string label))))
                    (clim:formatting-cell (*standard-output*)
                      (format *standard-output* "~a" (or value "?")))))))
         (r "Currently:" (getf data :timestamp))
         (r "User:" (getf data :user))
         (r "Machine:" (getf data :machine))
         (r "CPU:" (getf data :cpu))
         (r "OS:" (getf data :os))
         (r "Lisp:" (getf data :lisp))
         (r "Site:" (getf data :site))))
     (format t "~%")))
  :window-title "About Skyline-Tool"
  :width 600 :height 500
  :pdf-function '%about-pdf)

;; --- Climacs configuration ---

(defun configure-climacs ()
  "Set up Climacs text styles and themes for proper display."
  ;; Use a visible fixed-width font with appropriate size
  (setf climacs::*climacs-text-style*
        (clim:make-text-style :fix :roman :normal))
  ;; Info and mini-buffer colors
  (setf climacs::*info-bg-color*   (clim:make-gray-color 0.85))
  (setf climacs::*info-fg-color*   (clim:make-gray-color 0.0))
  (setf climacs::*mini-bg-color*   (clim:make-gray-color 1.0))
  (setf climacs::*mini-fg-color*   (clim:make-gray-color 0.0))
  ;; Ensure foreground/background inks are contrasting
  (setf clim::*foreground-ink* (clim:make-gray-color 0.0))  ; black
  (setf clim::*background-ink* (clim:make-gray-color 1.0))) ; white

;; Configure on load
(eval-when (:load-toplevel :execute)
  (ignore-errors (configure-climacs)))

;; --- Climacs-based editing ---

(clim:define-command (com-edit-file :command-table clim-internals::global-command-table
                                    ) ()
  "Open a file in the Climacs text editor."
  (let* ((default-path (merge-pathnames "Source/" (uiop:getcwd)))
         (path (clim:accept 'pathname :prompt "File to edit" :default default-path)))
    (when path
      (clim-sys:make-process
       (lambda ()
         (handler-case (climacs:edit-file (namestring path)
                                          :process-name (format nil "Editing ~a" path))
           (error (e)
             (error  "~&Climacs error: ~a~%" e))))
       :name (format nil "Editing ~a" path)))))

(clim:define-command (com-close-frame :command-table clim-internals::global-command-table) ()
  (let ((frame (when (boundp '*application-frame*) *application-frame*)))
    (unless frame
      ;; Fallback: try to find the frame from the port
      (ignore-errors
       (clim:map-over-frames (lambda (f)
                               (when (typep f 'clim:application-frame)
                                 (setf frame f)))
                             :port (clim:find-port))))
    (if frame
        (clim:frame-exit frame)
        (error  "~&Nothing to close.~%"))))

;; --- Global keyboard shortcuts with frame-type dispatch ---

(clim:define-command (com-save-default :command-table clim-internals::global-command-table
                                       ) ()
  "Save in default format (JSON for resource frames)."
  (if (boundp '*application-frame*)
      (let ((frame *application-frame*))
        (typecase frame
          (skyline-tool::anim-seq-editor-frame
           (skyline-tool::com-save-animation-seq-as-json))
          (skyline-tool::run-script-frame
           (skyline-tool::com-save-script-as-json))
          (otherwise
           (if (find-package :clim-simple-echo)
               (let ((class (find-class 'clim-simple-echo::simple-echo nil)))
                 (when (and class (typep frame class))
                   (clim-simple-echo::com-save-text)))
               (error  "~&Save: nothing to save in this window.~%")))))
      (error  "~&No active frame.~%")))

(clim:define-command (com-print-default :command-table clim-internals::global-command-table
                                        ) ()
  "Print/save as PDF for the current frame."
  (if (boundp '*application-frame*)
      (let ((frame *application-frame*))
        (if (and (find-package :clim-simple-echo)
                 (typep frame (find-class 'clim-simple-echo::simple-echo nil)))
            (clim-simple-echo::com-print-pdf)
            (typecase frame
              (skyline-tool::anim-seq-editor-frame
               (skyline-tool::com-save-animation-seq-as-pdf))
              (skyline-tool::run-script-frame
               (skyline-tool::com-save-script-as-pdf))
              (otherwise
               (error  "~&Print: no PDF export for this window.~%")))))
      (error  "~&No active frame.~%")))

(clim:define-command (com-open-go-to :command-table clim-internals::global-command-table
                                     ) ()
  "Open or go to a specific resource."
  (if (boundp '*application-frame*)
      (let ((frame *application-frame*))
        (typecase frame
          (skyline-tool::anim-seq-editor-frame
           (skyline-tool::com-switch-to-sequence))
          (skyline-tool::run-script-frame
           (skyline-tool::com-go-to-script))
          (skyline-tool::anim-seq-assign-frame
           (skyline-tool::com-find))
          (skyline-tool::anim-seq-assigns-frame
           (skyline-tool::com-find))
          (otherwise
           (error  "~&Open: not available in this window.~%"))))
      (error  "~&No active frame.~%")))

(clim:define-command (com-create-new-resource :command-table clim-internals::global-command-table
                                              ) ()
  "Create a new resource appropriate for the current window."
  (if (boundp '*application-frame*)
      (let ((frame *application-frame*))
        (typecase frame
          (skyline-tool::anim-seq-editor-frame
           (skyline-tool::com-create-new-sequence))
          (skyline-tool::run-script-frame
           (skyline-tool::com-new-script))
          (skyline-tool::anim-seq-assign-frame
           (error  "~&New: not available in this window.~%"))
          (skyline-tool::anim-seq-assigns-frame
           (error  "~&New: not available in this window.~%"))
          (otherwise
           (error  "~&New: not available in this window.~%"))))
      (error  "~&No active frame.~%")))

(defun run-tiled ()
  "Open the project in Tiled"
  (clim-sys:make-process
   (lambda ()
     (let ((title (format nil "~a: make pngs" (title-case *game-title*))))
       (error  "~&Building PNG assets (terminal: ~a)...~%" title)
       (force-output *query-io*)
       (uiop:run-program (list "ptyxis" "-s" "--title" title
                               "--" "make" "pngs")
                         :output nil :ignore-error-status t)
       (uiop:run-program (list "tiled" (format nil "Source/Maps/~a.tiled-project" *game-title*))
                         :output nil :ignore-error-status t)))
   :name "Run Tiled"))

  (defun open-file-manager ()
    "Open your File Manager in the project folder"
    (uiop:run-program (list "xdg-open" "./")))

  (define-constant +launcher-entries+
      '(skyline-tool
        (files
         show-all-resources
         show-rom-budget
         open-file-manager)
        (animation-editor
         assign-animation-sequences
         edit-animation-sequence)
        (7800-game-drive
         push-binary-to-7800-game-drive
         shove-binary-into-running-7800-game-drive)
        (core-dump-display
         show-dll-from-dump
         show-other-dll-from-dump
         copy-dump-as-dump2
         compare-dlls-from-dumps
         show-animation-buffer
         show-decal)
        (core-dump-general
         analyze-faults-from-dump
         show-dialogue-buffers
         show-map
         show-sound-system-info
         show-all-stacks
         show-forth-stack)
        (core-dump-objects
         show-player-object
         show-self-object
         show-all-objects
         show-room-for-objects)
        (lisp
         run-repl
         show-lisp-room
         reload-skyline-tool-from-sources))
    :test 'equalp)

  (defun display-launcher-menu-item (entry pane)
    (if (consp entry)
        (progn
          (clim:with-text-face (pane :bold)
            (clim:with-text-size (pane :larger)
              (format pane "~2%~a~%"
                      (title-case (string (first entry))))))
          (clim:with-text-size (pane :smaller)
            (dolist (sub-entry (rest entry))
              (display-launcher-menu-item sub-entry pane))))
        (clim:present entry 'nullary-function-name :stream pane)))

  (defmethod display-launcher-menu ((frame launcher-frame) (pane clim:pane))
    ;; Save the scroll Y before loading (which may trigger redisplay)
    (let ((scroll-y (launcher-saved-scroll-y frame)))
      (unless scroll-y
        (setf scroll-y (nth-value 1 (ignore-errors (clim:window-viewport-position pane)))))
      (load-project.json)
      (clim:with-text-size (pane :larger)
        (display-launcher-menu-item (copy-list +launcher-entries+) pane))
      (clim:with-text-size (pane :small)
        (format pane "~3%Click the name of any function to launch it"))
      ;; Restore scroll position to keep the user's view stable after commands
      (when scroll-y
        (ignore-errors (setf (clim:window-viewport-position pane)
                             (values 0 scroll-y)))
        (setf (launcher-saved-scroll-y frame) scroll-y))))

  (clim:define-presentation-type nullary-function-name () :inherit-from 'symbol)

  (clim:define-presentation-method clim:present
      (function-name (type nullary-function-name) stream view &key)
    (clim:with-text-size (stream :larger)
      (format stream "~%~4t~a" (title-case (string function-name))))
    (when-let (doc (documentation function-name 'function))
      (format stream "~%~a" (first-line doc)))
    (terpri stream))

  (define-launcher-frame-command (com-run-nullary-function :menu nil :name t)
      ((function-name 'nullary-function-name :gesture :select))
    (when (boundp '*application-frame*)
      (let* ((pane (clim:find-pane-named *application-frame* 'menu-list-pane))
             (scroll-y (and pane (nth-value 1 (ignore-errors (clim:window-viewport-position pane))))))
        (when scroll-y
          (setf (launcher-saved-scroll-y *application-frame*) scroll-y))))
    (clim-sys:make-process (lambda () (funcall function-name))
                           :name (title-case (string function-name))))

  (defun show-dll-from-dump ()
    "Show the decoded Display List List from the core dump"
    (clim-simple-echo:run-in-simple-echo #'decode-dll-from-dump
                                         :process-name "Display List List decoded"
                                         :height 768))

  (defun show-other-dll-from-dump ()
    "Show the decoded back buffer Display List List from the core dump"
    (clim-simple-echo:run-in-simple-echo (lambda () (decode-dll-from-dump
                                                     #p"/tmp/dump"
                                                     (logxor #x80 (detect-active-dll #p"/tmp/dump"))))
                                         :process-name "Back Buffer Display List List decoded"
                                         :height 768))

  ;; ============================================================
  ;; Unified Assets Index — replaces "Check for Absent Assets"
  ;; and "Show Assets Index".
  ;; ============================================================



  ;; --- Asset action menu (both left and right click) ---

  (defun %show-asset-context-menu (moniker builds kind-name asset-id hex-str present-p full-path)
    (flet ((run (cmd &rest a)
             (uiop:run-program (cons cmd a) :output nil :ignore-error-status t))
           (script-src ()
             (format nil "Source/~a.fountain" moniker))
           (item (label fn)
             (list label fn)))
      (let* ((parts (split-sequence #\/ moniker))
             (basename (car (last parts)))
             (type-key (ignore-errors (kind-by-name kind-name)))
             (file-path (or full-path (asset-index->filesystem-path moniker)))
             (dir-path (when file-path
                         (namestring (make-pathname :defaults file-path :name nil :type nil))))
             (moniker-full (if (search "Scripts/" moniker) moniker
                               (format nil "Scripts/~a" moniker)))
             (label (format nil "~a ~a~@[  $~a~]"
                            kind-name
                            (cl-change-case:title-case
                             (cl-ppcre:regex-replace "\\bDont\\b" basename "Don't"))
                            hex-str))
             (items nil))
        (macrolet (($ (label . body) `(push (item ,label (lambda () ,@body)) items))
                   (--- () '(push nil items)))
          ;; Editor entries per type
          (case type-key
            (:script
             ($ "Run in playtest emulator" (run-script moniker-full))
             ($ "Read aloud on AtariVox"
                (if (fboundp 'read-script-interactive)
                    (read-script-interactive moniker-full)
                    (error  "~&AtariVox not loaded.~%")))
             (---)
             ($ "Edit in Climacs"
                (clim-sys:make-process
                 (lambda () (climacs:edit-file (script-src)
                                               :process-name (format nil "Editing ~a" moniker)))
                 :name (format nil "Edit ~a" moniker)))
             ($ "Open in Emacs"
                (clim-sys:make-process
                 (lambda () (run "emacsclient" "-n" (script-src)))
                 :name (format nil "Edit ~a" moniker)))
             ($ "Open in ThiefMD"
                (clim-sys:make-process
                 (lambda () (run "thiefmd" (script-src)))
                 :name (format nil "Edit ~a" moniker)))
             (---)
             ($ "Save as PDF"
                (let* ((def (format nil "~a.pdf" (substitute #\_ #\/ moniker)))
                       (dir (if (find-package :clim-simple-echo)
                                (clim-simple-echo::default-save-directory)
                                (merge-pathnames #p"Documents/" (user-homedir-pathname))))
                       (full (namestring (merge-pathnames def dir)))
                       (path (string-trim '(#\Newline #\Space)
                                          (run "zenity" "--file-selection" "--save"
                                               (format nil "--filename=~a" full)
                                               "--title=Save Script As PDF..."))))
                  (when (and path (> (length path) 0))
                    (handler-case
                        (progn (fountain->pdf moniker path)
                               (error  "~&Saved ~a~%" path))
                      (error (e) (error  "~&PDF error: ~a~%" e))))))
             ($ "Print"
                (let* ((pdf (format nil "/tmp/st-~a.pdf" (substitute #\_ #\/ moniker)))
                       (printers (ignore-errors (discover-printers))))
                  (unless printers
                    (return-from %show-asset-context-menu
                      (error  "~&No printers.~%")))
                  (handler-case (fountain->pdf moniker pdf)
                    (error (e) (return-from %show-asset-context-menu
                                 (error  "~&PDF error: ~a~%" e))))
                  (let* ((names (mapcar (lambda (p) (if (consp p) (cdr p) p)) printers))
                         (choice (clim:menu-choose
                                  (mapcar (lambda (n) (list n n)) names)
                                  :label "Select printer")))
                    (when choice
                      (let ((q (if (consp (find choice names :test #'equal
                                                             :key (lambda (x)
                                                                    (if (consp x) (cdr x) x))))
                                   (car (find choice names :test #'equal
                                                           :key (lambda (x)
                                                                  (if (consp x) (cdr x) x))))
                                   choice)))
                        (error  "~&Printing to ~a...~%" choice)
                        (run "lp" "-d" q pdf)
                        (ignore-errors (delete-file pdf))
                        (error  "~&Sent.~%")))))))
            (:blob ($ "Open in Gimp..." (run "gimp" (or file-path moniker))))
            (:map
             ($ "Inspect...")
             ($ "Open in Tiled..." (run "tiled" (or file-path moniker))))
            (:song ($ "Open in MuseScore" (run "musescore" (or file-path moniker)))
             ($ "View Score as PDF"
                (let* ((src (or file-path moniker))
                       (pdf (format nil "/tmp/~a.pdf"
                                    (pathname-name (pathname src)))))
                  (run "musescore" src "-o" pdf)
                  (run "xdg-open" pdf)))
             ($ "Play as MIDI..."
                (let* ((src (or file-path moniker))
                       (mid (format nil "/tmp/~a.mid"
                                    (pathname-name (pathname src)))))
                  (run "musescore" src "-o" mid)
                  (run "xdg-open" mid)))
             ($ "Play as Ogg Vorbis..."
                (let* ((src (or file-path moniker))
                       (ogg (format nil "/tmp/~a.ogg"
                                    (pathname-name (pathname src)))))
                  (run "musescore" src "-o" ogg)
                  (run "xdg-open" ogg)))
             ($ "Play as FLAC..."
                (let* ((src (or file-path moniker))
                       (flac (format nil "/tmp/~a.flac"
                                     (pathname-name (pathname src)))))
                  (run "musescore" src "-o" flac)
                  (run "xdg-open" flac))))
            (:tileset
             ($ "Inspect...")
             ($ "Open in Gimp..." (run "gimp" (or file-path moniker)))
             ($ "Open in Tiled..." (run "tiled" (or file-path moniker))))
            (:sprite-sheet
             ($ "Open in Gimp" (run "gimp" (or file-path moniker))))
            (:object-prototype
             ($ "Inspect..."
                (if (fboundp 'edit-object-prototypes)
                    (edit-object-prototypes file-path)
                    (error  "~&edit-object-prototypes not available.~%")))
             ($ "Open in Emacs..."
                (clim-sys:make-process
                 (lambda () (run "emacsclient" "-n" (or file-path moniker)))
                 :name (format nil "Edit ~a" moniker))))
            (:class
             ($ "Inspect..."
                ))
            (:routine
             ($ "Open in Emacs"
                (clim-sys:make-process
                 (lambda () (run "emacsclient" "-n" (or file-path moniker)))
                 :name (format nil "Edit ~a" moniker))))
            (:character
             ($ "Open Character Inspector"
                (open-character-inspector (string-capitalize basename)))
             ($ "Edit Prototype JSON"
                (let ((p (merge-pathnames
                          (make-pathname :name basename :type "json")
                          (merge-pathnames "Source/Objects/" (uiop:getcwd)))))
                  (when (probe-file p)
                    (run "emacsclient" "-n" (namestring p))))))
            (:boat
             ($ "Inspect..."
                (let ((path (prompt-save-pathname
                             (format nil "~a.txt" basename) :type "txt")))
                  (when path
                    (with-open-file (s path :direction :output :if-exists :supersede)
                      (dolist (boat (load-boats))
                        (destructuring-bind (name class notes) boat
                          (format s "~a: ~a (~a)~%" name class notes)))))))))
          ;; Toggle build flags
          
          (---)
          ($ "Toggle Demo" (%toggle-build-flag moniker builds #\D))
          ($ "Toggle Public" (%toggle-build-flag moniker builds #\P))
          ($ "Toggle AtariAge" (%toggle-build-flag moniker builds #\A))
          (---)
          ($ "Open Containing Folder" (run "xdg-open" (or dir-path ".")))
          (---)
          ;; Global actions
          
          ($ "Save List as Text"
             (let* ((text (%script-list-text))
                    (path (prompt-save-pathname "ScriptList.txt" :type "txt")))
               (when path
                 (with-open-file (f path :direction :output :if-exists :supersede
                                         :external-format :utf-8)
                   (princ text f))
                 (error  "~&Saved ~a~%" (namestring path)))))
          ($ "Save List as PDF"
             (let* ((text (%script-list-text))
                    (path (prompt-save-pathname "ScriptList.pdf" :type "pdf"))
                    (lines (count #\Newline text))
                    (pages (max 1 (ceiling lines (- 700 50))))
                    (game-title (string-capitalize *game-title*))
                    (title (format nil "Skyline-Tool for ~a" game-title))
                    (author (user-real-name))
                    (date-str (multiple-value-bind (s m h d mo y) (get-decoded-time)
                                (declare (ignore s))
                                (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" y mo d h m))))
               (when path
                 (let ((ps (make-pathname :type "ps" :defaults path)))
                   (with-open-file (f ps :direction :output :if-exists :supersede)
                     (format f "%!PS-Adobe-3.0~%")
                     (write-ps-docinfo f title "Skyline-Tool"
                                       (format nil "~a on ~a" author (machine-instance)))
                     (format f "<< /PageSize [612 792] >> setpagedevice~%")
                     (write-ps-font-encodings f)
                     (with-input-from-string (s text)
                       (dotimes (p pages)
                         (format f "%%Page: ~d ~d~%" (1+ p) pages)
                         (let ((y 700))
                           (loop for line = (read-line s nil nil)
                                 while (and line (>= y 50))
                                 do (format f "50 ~d moveto (~a) show~%" y
                                            (escape-ps-string line))
                                    (decf y 10)))
                         (write-ps-footer f (1+ p) pages (string-capitalize *game-title*) nil author (machine-instance))
                         (format f "showpage~%"))))
                   (run "ps2pdf" (namestring ps) (namestring path))
                   (ignore-errors (delete-file ps))
                   (error  "~&Saved ~a~%" (namestring path))))))
          ($ "Copy List"
             (let ((text (%script-list-text)))
               (clim:with-application-frame (frame)
                 (setf (clim-simple-echo::frame-captured-text frame) text))
               (if (clim-simple-echo::%clipboard-copy text)
                   (error  "~&Copied ~:d char~:p to clipboard.~%" (length text))
                   (error  "~&Clipboard copy requires wl-copy or xclip.~%")))
             (---)
             ($ "Save Assets Index" (write-sorted-all-resources))
             (---)
             ($ "Redisplay"
                (clim:redisplay-frame-panes *application-frame* :force-p t)))
          ;; Show the menu
          
          (setf items (nreverse items))
          (let ((choice (clim:menu-choose
                         (mapcar (lambda (item)
                                   (if (null item)
                                       '(nil :divider :line)
                                       (list (first item) (first item))))
                                 items)
                         :label label)))
            (when choice
              (let ((fn (second (find choice items :key #'first :test #'equal))))
                (when fn (funcall fn)))))))))

  (defun %script-list-text ()
    "Return a plain-text listing of all script names grouped by area."
    (with-output-to-string (s)
      (let ((last-area nil))
        (dolist (sn (all-script-names :reloadp t))
          (terpri s)
          (let* ((parts (split-sequence #\/ sn))
                 (area (elt parts (- (length parts) 2))))
            (unless (string-equal area last-area)
              (format s "~%~a~%" (cl-change-case:title-case area))
              (setf last-area area)))
          (format s "  ~a~%" (cl-ppcre:regex-replace "\\bDont\\b"
                                                     (cl-change-case:title-case
                                                      (subseq sn (1+ (position #\/ sn)))) "Don't"))))))

  (defun %toggle-build-flag (moniker builds flag-char)
    "Toggle a build flag (D, P, or A) for MONIKER and rewrite Assets.index."
    (let* ((build-name (case flag-char (#\D "Demo") (#\P "Public") (#\A "AA") (t "")))
           (has-flag (and builds (member build-name builds :test #'string-equal)))
           (new-builds (if has-flag
                           (remove build-name builds :test #'string-equal)
                           (sort (cons build-name (copy-list builds)) #'string-lessp)))
           (lines (with-open-file (f #p"Source/Assets.index")
                    (loop for l = (read-line f nil nil) while l collect l)))
           (old-build-str (when builds
                            (format nil "~{~a~}" (mapcar (lambda (b) (case b ("Demo" "D") ("Public" "P") ("AA" "A"))) builds))))
           (new-build-str (format nil "~{~a~}" (mapcar (lambda (b) (case b ("Demo" "D") ("Public" "P") ("AA" "A"))) new-builds)))
           (old-line (format nil "~a~@[ ~a~]" moniker old-build-str))
           (new-line (format nil "~a~@[ ~a~]" moniker new-build-str)))
      (with-open-file (f #p"Source/Assets.index" :direction :output :if-exists :supersede
                                                 :external-format :utf-8)
        (dolist (l lines)
          (write-line (if (string= (string-trim " " l) (string-trim " " old-line))
                          new-line l) f)))
      (error  "~&Toggled ~a for ~a: ~:[(none)~;~a~]~%"
              flag-char moniker (not (emptyp new-builds)) new-build-str))
    (ignore-errors (clim:redisplay-frame-panes *application-frame* :force-p t)))

  ;; --- Save Assets Index (sorted, blank lines between directories) ---

  (defun write-sorted-all-resources ()
    "Rewrite Assets.index in alphabetical order with blank lines between
   directory sections. Preserves header comments."
    (let* ((path #p"Source/Assets.index")
           (lines (with-open-file (f path)
                    (loop for l = (read-line f nil nil) while l collect l)))
           (headers nil)
           (entries nil))
      ;; Split into header comments and sorted entries
      (dolist (l lines)
        (if (or (zerop (length (string-trim " " l)))
                (char= (char (string-trim " " l) 0) #\;))
            (push l headers)
            (push (string-trim " " l) entries)))
      (setf headers (nreverse headers)
            entries (sort (nreverse entries) #'string-lessp))
      ;; Group entries by first directory component (Scripts, Songs, Maps, Blobs)
      (let ((grouped (make-hash-table :test 'equal))
            (order nil))
        (dolist (e entries)
          (let* ((parts (split-sequence #\/ e))
                 (dir (first parts)))
            (unless (gethash dir grouped)
              (push dir order))
            (push e (gethash dir grouped))))
        (setf order (nreverse order))
        (with-open-file (f path :direction :output :if-exists :supersede
                                :external-format :utf-8)
          (dolist (h headers) (write-line h f))
          (terpri f)
          (loop for dir in order
                for group = (sort (gethash dir grouped) #'string-lessp)
                do (dolist (e group) (write-line e f))
                   (terpri f))))
      (error  "~&Assets.index rewritten in sorted order with blank lines between directories.~%")
      ;; Redisplay
      (ignore-errors
       (clim:redisplay-frame-panes *application-frame* :force-p t))))

  ;; --- Launcher wrappers ---

  (defun edit-all-resources ()
    "Open the unified Assets Index in a simple-echo window."
    (clim-simple-echo:run-in-simple-echo #'show-all-resources
                                         :process-name "Assets Index"
                                         :width 450 :height 700))

  (defun show-all-resources ()
    "Open the unified All Resources browser."
    (clim-simple-echo:run-in-simple-echo #'show-all-resources-internal
                                         :process-name "All Resources"
                                         :width 600 :height 845
                                         :menu-bar 'clim-simple-echo::echo-resource-menu-bar))

  (defun check-for-absent-assets-in-project-folder ()
    "Open the unified Assets Index (includes absent-asset detection)."
    (edit-all-resources))

  (defun show-lisp-room ()
    "Check how much room (in memory) this Lisp image is using"
    (clim-simple-echo:run-in-simple-echo (lambda ()
                                           (format t "~&Running a full garbage collection…")
                                           (force-output)
                                           (handler-case (sb-ext:gc :full t)
                                             (error (e) (format t "~&GC error: ~a~%" e)))
                                           (format t "~&Ready.  Memory usage:~%")
                                           (handler-case (room)
                                             (error (e) (format t "~&(room) error: ~a~%" e))))
                                         :process-name "Room"))

  (defun push-binary-to-7800-game-drive ()
    "Push the latest binary to the 7800GD over its serial (debug) port"
    (clim-simple-echo:run-in-simple-echo
     (lambda ()
       (let ((rom-path (format nil "Dist/~a.Public.NTSC.bin" *game-title*)))
         (unless (probe-file rom-path)
           (format t "~&Binary not found. Running make to build it...~%")
           (force-output)
           (uiop:run-program (list "make" (format nil "Dist/~a.Public.NTSC.a78" *game-title*))
                             :output t :ignore-error-status t))
         (push-7800gd-bin rom-path)))
     :process-name "Push binary"))

  (defun shove-binary-into-running-7800-game-drive ()
    "Update the running game with a new binary image
 
Should try to just reload the current scene, if there were no code (only
asset) changes, this may work."
    (clim-simple-echo:run-in-simple-echo
     (lambda ()
       (push-7800gd-bin-no-execute (format nil "Dist/~a.Public.NTSC.bin" *game-title*)
                                   (find-7800gd-serial-port) t))
     :process-name "Shove binary into running system"))

  (defun explain-error-code (error-code)
    (let ((codes-file (make-pathname :directory (list :relative "Dist" "7800")
			       :name (format nil "~a.error.codes" *game-title*)
                                     :type "tsv")))
      (unless (probe-file codes-file)
        (uiop:run-program (list "make" (enough-namestring codes-file))))
      (with-input-from-file (codes codes-file)
        (loop for line = (read-line codes nil nil)
              while line
              do (destructuring-bind (code text source) (split-sequence #\Tab line)
                   (when (string-equal code error-code)
                     (format t "~&~10tCode: “~@:(~a~)” — “~a”~%~14tFound in: ~a"
                             code text source)
                     (return-from explain-error-code t))))))
    (format t "~&~10t(code not identified)"))

  (defun analyze-dump-faults (&optional (dump-pathname #p"/tmp/dump"))
    "Report on the fault codes logged in a core dump at DUMP-PATHNAME"
    (let ((mem (load-dump-into-mem dump-pathname))
          (break-signal (find-label-from-files "BreakSignal"))
          (minor-fault-count (find-label-from-files "MinorFaultCount"))
          (last-minor-fault (find-label-from-files "LastMinorFault")))
      (format t "~&Analyzing faults found in dump …")
      (let ((break-bytes (list (dump-peek break-signal mem)
                               (dump-peek (+ 1 break-signal) mem)
                               (dump-peek (+ 2 break-signal) mem)
                               (dump-peek (+ 3 break-signal) mem))))
        (unless (every #'zerop break-bytes)
          (let ((break-code (minifont->unicode break-bytes :replace #\?)))
            (format t "~2%A DebugBreak occurred, which will result in the crash screen.
The signal code was ~a" break-code)
            (explain-error-code break-code))))
      (let ((faults-count (+ (dump-peek minor-fault-count mem)
                             (* #x100 (dump-peek (+ 1 minor-fault-count) mem)))))
        (unless (zerop faults-count)
          (format t "~2%There have been ~:d minor fault~:p reported" faults-count)))
      (let ((fault-bytes (vector (dump-peek last-minor-fault mem) 
                                 (dump-peek (+ 1 last-minor-fault) mem) 
                                 (dump-peek (+ 2 last-minor-fault) mem) 
                                 (dump-peek (+ 3 last-minor-fault) mem))))
        (unless (every #'zerop fault-bytes)
          (let ((fault-code (minifont->unicode fault-bytes :replace #\?)))
            (format t "~2%The last minor fault signal code was ~a" fault-code)
            (explain-error-code fault-code))))
      (terpri) (terpri) (force-output))) 

  (defun analyze-faults-from-dump (&optional (dump-pathname #p"/tmp/dump"))
    "Report on the fault codes logged in a core dump at DUMP-PATHNAME in a new window"
    (clim-simple-echo:run-in-simple-echo (lambda ()
                                           (analyze-dump-faults dump-pathname))
                                         :process-name "Analyze Dump Faults"))

  (defun reload-skyline-tool-from-sources ()
    "Recompile and reload this utility from the current sources on disk."
    (clim-simple-echo:run-in-simple-echo (lambda ()
                                           (when *launcher-frame*
                                             (clim:frame-exit *launcher-frame*))
                                           (recompile-tool))
                                         :process-name "Recompile Skyline-Tool"))

  (defun read-asset-bank-size (bank build region)
    (with-input-from-file (size-file (allocation-size-name bank build region))
      (loop for line = (read-line size-file)
            when (let ((at-pos (position #\@ line))
                       (tab-pos (position #\Tab line)))
                   (and at-pos (zerop at-pos) tab-pos (= 1 tab-pos)))
              do (return-from read-asset-bank-size (parse-integer line :start 2))))
    (error "Could not figure out size of bank $~2,'0x for ~a ~a" bank build region))

  (defun make-progress-bar (fraction width)
    "Return a text string representing a progress bar FRACTION (0-1) of WIDTH characters."
    (let* ((fill (round (* fraction width)))
           (buf (make-string width :initial-element #\space)))
      (dotimes (i (min fill width))
        (setf (char buf i) #\#))
      buf))

  (defun emit-clim-progress-bar (stream fraction &key (width nil) (height nil) (label ""))
    "Draw a progress bar in the CLIM stream STREAM at cursor position.
 Light blue background + black outline, navy fill portion.
 Width defaults to ~1/3 pane width, height defaults to ex-height.
 Falls back to text \"[NN%]\" when STREAM does not support CLIM drawing."
    (if (ignore-errors (clim:stream-drawing-p stream))
        (let* ((pane-width (ignore-errors
                            (clim:bounding-rectangle-width (clim:sheet-region stream))))
               (bar-width (or width (if pane-width (round (/ pane-width 3)) 200)))
               (bar-height (or height 12)))
          (let ((fill-w (max 1 (round (* fraction bar-width)))))
            ;; Light blue background fill
            (clim:draw-rectangle* stream 0 0 bar-width bar-height
                                  :filled t :ink (clim:make-rgb-color 0.7 0.85 1.0))
            ;; Black outline
            (clim:draw-rectangle* stream 0 0 bar-width bar-height
                                  :filled nil :ink (clim:make-rgb-color 0 0 0))
            ;; Navy fill portion
            (clim:draw-rectangle* stream 0 0 fill-w bar-height
                                  :filled t :ink (clim:make-rgb-color 0.0 0.0 0.5))))
        (format stream "[~3d%]" (round (* 100 fraction)))))

  (defun compute-bank-size (bank build region)
    "Return the size in bytes used by BANK for this BUILD/REGION, or #x4000 on error."
    (cond
      ((< bank (first-assets-bank build))
       ;; Low bank (Stagehand low code)
       (let* ((size-path (make-pathname :directory '(:relative "Source" "Generated")
                                        :type "size"
                                        :name (format nil "Bank~(~2,'0x~).~a.~a"
                                                      bank build (string-upcase region))))
              (base-size (ignore-errors
                          (parse-integer
                           (remove-if-not #'digit-char-p
                                          (read-file-into-string size-path))))))
         (or base-size
             (progn (format t "~%Bank $~2,'0x size file not found" bank) #x4000))))
      ((= bank #x3e)
       #x4000)
      ((= bank #x3f)
       ;; StagehandHigh .o size
       (let ((o-path (merge-pathnames
                      (make-pathname :directory (list :relative "Object" (machine-directory-name))
                                     :name "StagehandHigh" :type "o")
                      (uiop:getcwd))))
         (if (probe-file o-path)
             (with-open-file (o-file o-path :direction :input :element-type '(unsigned-byte 8))
               (file-length o-file))
             (progn (format t "~%Bank $3F: StagehandHigh.o not found") #x4000))))
      (t
       ;; Asset bank
       (or (ignore-errors (read-asset-bank-size bank build region))
           (progn (format t "~%Bank $~2,'0x size file not found" bank) #x4000)))))

  (defun rom-budget (&optional (build "Public") (region :ntsc))
    "Generate a ROM budget report for BUILD and REGION, with visual usage bars.
   BUILD: \"Demo\", \"Public\", or \"Publisher\".
   REGION: :ntsc or :pal.
   Returns (VALUES bank-data-list total-sum total-banks)."
    (format t "(generating size files …")
    (force-output)
    (write-master-makefile)
    (uiop:run-program (list "make" "-j4" "-s"
                            "Source/Generated/Makefile")
                      :output :string :ignore-error-status t)
    (format t "~2&Build: ~a ~20tRegion: ~a" build (string-upcase region))
    (let ((sum 0)
          (bank-count 0)
          (bank-data (make-array 0 :fill-pointer t :adjustable t))
          (total-banks #x40))
      (dotimes (bank total-banks)
        (let* ((size (compute-bank-size bank build region))
               (pct (round (/ size 163.84)))
               (bar-text (make-progress-bar (/ size #x4000) 24))
               (label (format nil "Bank $~2,'0x" bank)))
          (incf sum size)
          (incf bank-count)
          (vector-push-extend (list :bank bank :label label :size size :pct pct :bar-text bar-text)
                              bank-data)
          (if (= bank #x3e)
              (format t "~%Bank $3E — unavailable on 7800GD~35t[░░░░░░░░░░░░░░░░░░░░] 100%")
              (format t "~%Bank $~2,'0x — $~4,'0x (~:d)~35t~a ~d%"
                      bank size size bar-text pct))))
      (let ((total-pct (round (* 100 (/ sum (* total-banks #x4000))))))
        (format t "~2% … total for ~a ~a: $~6,'0x = ~:d = ~:d kiB (~d%)~%"
                build (string-upcase region) sum sum (floor sum 1024)
                total-pct)
        (let ((bar-width 600) (bar-height 24))
          (format t "~%~%     ")
          (emit-clim-progress-bar *standard-output* (/ sum (* total-banks #x4000))
                                  :width bar-width :height bar-height)
          (format t " ~d% (~:d / ~:d bytes)~%~%" total-pct sum (* total-banks #x4000))))
      (values bank-data sum total-banks)))

  ;; --- ROM Budget two-pane CLIM frame ---

  (clim:define-application-frame rom-budget-frame (clim:standard-application-frame)
    ((bank-data :initform nil :accessor rb-bank-data)
     (total-sum :initform 0 :accessor rb-total-sum)
     (total-banks :initform #x40 :accessor rb-total-banks)
     (build :initform "Public" :accessor rb-build)
     (region :initform :ntsc :accessor rb-region))
    (:panes
     (bank-list-pane :application :scroll-bars t
                                  :height 550 :width 800
                                  :display-function 'display-rom-budget-bank-list)
     (summary-pane :application :height 100 :width 800
                                :display-function 'display-rom-budget-summary))
    (:layouts
     (default (clim:vertically () bank-list-pane summary-pane)))
    (:menu-bar rom-budget-menu-bar)
    (:icon (skyline-tool-icon)))

  (clim:define-command-table rom-budget-file-menu
    :menu (("Close" :command com-close-rom-budget)))

  (clim:define-command-table rom-budget-regen-menu
    :menu (("NTSC Demo" :command com-rb-regen-ntsc-demo)
           ("NTSC Public" :command com-rb-regen-ntsc-public)
           ("NTSC Publisher" :command com-rb-regen-ntsc-publisher)
           (nil :divider :line)
           ("PAL Demo" :command com-rb-regen-pal-demo)
           ("PAL Public" :command com-rb-regen-pal-public)
           ("PAL Publisher" :command com-rb-regen-pal-publisher)))

  (clim:define-command-table rom-budget-menu-bar
    :menu (("Report" :menu rom-budget-file-menu)
           ("Recompile" :menu rom-budget-regen-menu)))

  (clim:define-command (com-close-rom-budget :menu nil :name t) ()
    (clim:frame-exit *application-frame*))

  ;; --- ROM Budget regenerate commands ---

  (defun %regen-rom-budget (frame build region)
    "Helper: run rom-budget for BUILD/REGION and update FRAME slots, then redisplay."
    (with-slots (bank-data total-sum total-banks) frame
      (multiple-value-bind (bd ts tb) (rom-budget build region)
        (setf bank-data bd total-sum ts total-banks tb
              (rb-build frame) build (rb-region frame) region))
      (clim:redisplay-frame-panes frame :force-p t)))

  (clim:define-command (com-rb-regen-ntsc-demo :menu nil :name t)
      ((frame 'clim:application-frame))
    (%regen-rom-budget frame "Demo" :ntsc) nil)

  (clim:define-command (com-rb-regen-ntsc-public :menu nil :name t)
      ((frame 'clim:application-frame))
    (%regen-rom-budget frame "Public" :ntsc) nil)

  (clim:define-command (com-rb-regen-ntsc-publisher :menu nil :name t)
      ((frame 'clim:application-frame))
    (%regen-rom-budget frame "Publisher" :ntsc) nil)

  (clim:define-command (com-rb-regen-pal-demo :menu nil :name t)
      ((frame 'clim:application-frame))
    (%regen-rom-budget frame "Demo" :pal) nil)

  (clim:define-command (com-rb-regen-pal-public :menu nil :name t)
      ((frame 'clim:application-frame))
    (%regen-rom-budget frame "Public" :pal) nil)

  (clim:define-command (com-rb-regen-pal-publisher :menu nil :name t)
      ((frame 'clim:application-frame))
    (%regen-rom-budget frame "Publisher" :pal) nil)

  ;; --- ROM Budget launcher entry functions ---

  (defun %open-rom-budget-frame (build region)
    "Create and run a rom-budget-frame for the given BUILD and REGION."
    (let* ((fm (clim:find-frame-manager :port (or (clim:find-port) (clim:find-port :server-path :x))))
           (frame (clim:make-application-frame 'rom-budget-frame
                                               :build build :region region
                                               :frame-manager fm
                                               :width 820 :height 680)))
      (clim-sys:make-process
       (lambda ()
         (let ((*application-frame* frame))
           (multiple-value-bind (bd ts tb) (rom-budget build region)
             (setf (rb-bank-data frame) bd
                   (rb-total-sum frame) ts
                   (rb-total-banks frame) tb))
           (clim:run-frame-top-level frame)))
       :name (format nil "ROM Budget ~a ~a"
                     (string-capitalize build)
                     (if (eq region :ntsc) "NTSC" "PAL")))))

  (defun show-rom-budget ()
    "Open ROM Budget for the default build variant (Public NTSC)."
    (%open-rom-budget-frame "Public" :ntsc))

  (defun show-rom-budget-DEMO-NTSC () (%open-rom-budget-frame "Demo" :ntsc))
  (defun show-rom-budget-PUBLIC-NTSC () (%open-rom-budget-frame "Public" :ntsc))
  (defun show-rom-budget-PUBLISHER-NTSC () (%open-rom-budget-frame "Publisher" :ntsc))
  (defun show-rom-budget-DEMO-PAL () (%open-rom-budget-frame "Demo" :pal))
  (defun show-rom-budget-PUBLIC-PAL () (%open-rom-budget-frame "Public" :pal))
  (defun show-rom-budget-PUBLISHER-PAL () (%open-rom-budget-frame "Publisher" :pal))

  (defun display-rom-budget-bank-list (frame pane)
    "Display per-bank entries from ROM Budget data."
    (clim:window-clear pane)
    (with-slots (bank-data build region) frame
      (format pane "~2&Build: ~a ~20tRegion: ~a~2%" build (string-upcase region))
      (dotimes (i (length bank-data))
        (let ((entry (aref bank-data i)))
          (format pane "~&~a — $~4,'0x (~:d)~24t~a ~d%"
                  (getf entry :label) (getf entry :size) (getf entry :size)
                  (getf entry :bar-text) (getf entry :pct))))))

  (defun display-rom-budget-summary (frame pane)
    "Display the overall progress bar from ROM Budget data."
    (clim:window-clear pane)
    (with-slots (total-sum total-banks build region) frame
      (let ((total-pct (round (* 100 (/ total-sum (* total-banks #x4000))))))
        (format pane "~%~%     ")
        (emit-clim-progress-bar pane (/ total-sum (* total-banks #x4000))
                                :width 600 :height 24)
        (format pane " ~d% (~:d / ~:d bytes)~%" total-pct total-sum
                (* total-banks #x4000)))))
  
  (defun launcher ()
    "Open the Skyline Tool launcher (main menu)"
    (let ((frame (clim:make-application-frame 'launcher-frame :name "Skyline-Tool")))
      (if (boundp '*machine*)
          (launcher-body frame)
          (load-project.json nil (lambda () (launcher-body frame))))))

  (defun launcher-body (frame)
    (let ((*launcher-frame* frame))
      (setf (clim:frame-pretty-name frame) (window-title)
            *default-pathname-defaults*
            (let ((skyline-dir (asdf:system-source-directory 
                                (asdf:find-system :skyline-tool))))
              (make-pathname :defaults skyline-dir
                             :directory (butlast (pathname-directory skyline-dir)))))
      (sb-posix:chdir (namestring *default-pathname-defaults*))
      (clim:run-frame-top-level frame)))

  (defun run-launcher ()
    "Open the Skyline Tool launcher (main menu) in its own thread"
    (let ((*trace-output* (make-synonym-stream '*trace-output*)))
      (handler-case
          (clim-sys:make-process #'launcher
                                 :name "Skyline Tool GUI Launcher")
        (xlib:window-error ()
          (invoke-restart 'restart-event-loop) ))))

  (defun cl-user::skyline-tool ()
    "Open the Skyline Tool launcher (main menu)"
    (run-launcher))

;;; ============================================================
;;; Clipboard support — selection translators for format negotiation
;;; ============================================================

  (defun extract-maria-pixels (dump mode address width)
    "Decode Maria 7800 pixel data into a 2D array (pixels-wide × 16)."
    (flet ((peek (offset)
             (if (< (+ address offset) (length dump))
                 (aref dump (+ address offset))
                 #xff)))
      (let* ((pix-per-byte (ecase mode (:160a 4) (:160b 2)))
             (pix-width (* width pix-per-byte))
             (pixels (make-array (list pix-width 16) :element-type '(unsigned-byte 8))))
        (dotimes (y 16)
          (dotimes (byte width)
            (let ((bits (peek (+ (* (- #x0f y) #x100) byte))))
              (ecase mode
                (:160a
                 (setf (aref pixels (+ 0 (* 4 byte)) y) (ash (logand #b11000000 bits) -6)
                       (aref pixels (+ 1 (* 4 byte)) y) (ash (logand #b00110000 bits) -4)
                       (aref pixels (+ 2 (* 4 byte)) y) (ash (logand #b00001100 bits) -2)
                       (aref pixels (+ 3 (* 4 byte)) y) (logand #b00000011 bits)))
                (:160b
                 (let ((left-c (ash (logand #b11000000 bits) -6))
                       (right-c (ash (logand #b00110000 bits) -4))
                       (left-p (ash (logand #b00001100 bits) -2))
                       (right-p (logand #b00000011 bits)))
                   (setf (aref pixels (+ 0 (* 2 byte)) y) (logior (ash left-p 2) left-c)
                         (aref pixels (+ 1 (* 2 byte)) y) (logior (ash right-p 2) right-c))))))))
        pixels)))

  (defun pixels-to-xpm (pixels &optional (palette nil))
    "Convert a 2D pixel array (color indices) to XPM format string.
   PALETTE is a list of Atari color register values (for color table).
   Returns the XPM text as a string."
    (let* ((width (array-dimension pixels 0))
           (height (array-dimension pixels 1))
           (ncolors (1+ (loop for i below width maximize
                                                (loop for j below height maximize (aref pixels i j)))))
           (color-chars " .XoO+@#$%&*=-:;,"))
      (with-output-to-string (xpm)
        (format xpm "/* XPM */~%static char *viewport[] = {~%")
        (format xpm "\"~d ~d ~d 1\",~%" width height ncolors)
        (dotimes (c ncolors)
          (let ((char (if (< c (length color-chars))
                          (aref color-chars c)
                          (code-char (+ 33 c)))))
            (format xpm "\"~c c ~a\",~%" char
                    (if palette
                        (format nil "#~2,'0x~2,'0x~2,'0x"
                                (floor c 6) (floor (mod c 6) 2) (mod c 12))
                        (format nil "#~2,'0x~2,'0x~2,'0x"
                                (ash c 4) (ash c 4) (ash c 4))))))
        (dotimes (j height)
          (format xpm "\"")
          (dotimes (i width)
            (let ((c (aref pixels i j)))
              (write-char (if (< c (length color-chars))
                              (aref color-chars c)
                              (code-char (+ 33 c)))
                          xpm)))
          (format xpm (if (< j (1- height)) "\",~%" "\"~%"))
          (values))
        (format xpm "};~%"))))

  (defun sequence-property-text (seq)
    "Return a plain-text property listing for an animation sequence."
    (with-output-to-string (s)
      (format s "Animation Sequence #~d~%" (simple-animation-sequence-index seq))
      (format s "  Label:       ~a~%" (or (simple-animation-sequence-label seq) "(none)"))
      (format s "  Major Kind:  ~a~%" (simple-animation-sequence-major-kind seq))
      (format s "  Decal Kind:  ~a~%" (simple-animation-sequence-decal-kind seq))
      (format s "  Body:        ~d~%" (simple-animation-sequence-decal-body seq))
      (format s "  Tile Sheet:  ~a~%" (simple-animation-sequence-tile-sheet seq))
      (format s "  Write Mode:  ~a~%" (simple-animation-sequence-write-mode seq))
      (format s "  Bytes Wide:  ~d~%" (simple-animation-sequence-bytes-width seq))
      (format s "  Frame Count: ~d~%" (simple-animation-sequence-frame-count seq))
      (format s "  Frame Rate:  ~d~%" (simple-animation-sequence-frame-rate-scalar seq))
      (format s "~%Frames:~%")
      (dotimes (i (simple-animation-sequence-frame-count seq))
        (let ((frame (aref (simple-animation-sequence-frames seq) i)))
          (format s "  [~d] tile reference ~d~%" i frame)))))

  (defun sequence-alist (seq)
    "Return the animation sequence as an alist for JSON serialization."
    (list (cons "index" (simple-animation-sequence-index seq))
          (cons "label" (or (simple-animation-sequence-label seq) ""))
          (cons "majorKind" (string-downcase (simple-animation-sequence-major-kind seq)))
          (cons "decalKind" (string-downcase (simple-animation-sequence-decal-kind seq)))
          (cons "body" (simple-animation-sequence-decal-body seq))
          (cons "tileSheet" (simple-animation-sequence-tile-sheet seq))
          (cons "writeMode" (string-downcase (simple-animation-sequence-write-mode seq)))
          (cons "bytesWidth" (simple-animation-sequence-bytes-width seq))
          (cons "frameCount" (simple-animation-sequence-frame-count seq))
          (cons "frameRateScalar" (simple-animation-sequence-frame-rate-scalar seq))
          (cons "frames"
                (loop for i below (simple-animation-sequence-frame-count seq)
                      collect (aref (simple-animation-sequence-frames seq) i)))))

  (defun sequence-to-json (seq)
    "Return a compact JSON string for an animation sequence."
    (json:encode-json-to-string (sequence-alist seq)))

  (defun sequence-from-json (json-string)
    "Parse a JSON string into a simple-animation-sequence instance."
    (let ((data (json:decode-json-from-string json-string)))
      (flet ((v (key) (cdr (assoc key data :test #'string=))))
        (make-instance 'simple-animation-sequence
                       :index (v "index")
                       :label (let ((l (v "label"))) (if (emptyp l) nil l))
                       :major-kind (make-keyword (string-upcase (v "majorKind")))
                       :decal-kind (make-keyword (string-upcase (v "decalKind")))
                       :decal-body (v "body")
                       :tile-sheet (v "tileSheet")
                       :write-mode (make-keyword (string-upcase (v "writeMode")))
                       :bytes-width (v "bytesWidth")
                       :frame-count (v "frameCount")
                       :frame-rate-scalar (v "frameRateScalar")
                       :frames (coerce (v "frames") 'vector)))))

  ;; --- CLIPBOARD PRESENTATION TYPES ---
  ;; These types tag clipboard data so request-selection can find translators.

  (clim:define-presentation-type clipboard-text () :inherit-from 'string)
  (clim:define-presentation-type clipboard-json () :inherit-from 'string)
  (clim:define-presentation-type clipboard-xpm () :inherit-from 'string)

  ;; Wrapper class for animation sequence clipboard data
  (defclass animation-sequence-clipboard ()
    ((sequence :initarg :sequence :reader clipboard-sequence)
     (dump :initarg :dump :reader clipboard-dump :initform nil)
     (address :initarg :address :reader clipboard-address :initform 0)
     (mode :initarg :mode :reader clipboard-mode :initform :160a)
     (palette :initarg :palette :reader clipboard-palette :initform nil)
     (width :initarg :width :reader clipboard-width :initform 2)))

  (clim:define-presentation-type animation-sequence-clipboard-data ()
    :inherit-from 'animation-sequence-clipboard)

  ;; --- SELECTION TRANSLATORS: animation sequence → clipboard formats ---
  ;; Use define-presentation-translator with :translator-class selection-translator
  ;; so that request-selection will find them during format negotiation.

  (clim:define-presentation-translator seq->text
      (animation-sequence-clipboard-data clipboard-text clim-internals::global-command-table
                                         :gesture :select
                                         :tester-definitive t
                                         :translator-class clim-internals::selection-translator)
      (data)
    (sequence-property-text (clipboard-sequence data)))

  (clim:define-presentation-translator seq->json
      (animation-sequence-clipboard-data clipboard-json clim-internals::global-command-table
                                         :gesture :select
                                         :tester-definitive t
                                         :translator-class clim-internals::selection-translator)
      (data)
    (sequence-to-json (clipboard-sequence data)))

  (clim:define-presentation-translator seq->xpm
      (animation-sequence-clipboard-data clipboard-xpm clim-internals::global-command-table
                                         :gesture :select
                                         :tester-definitive t
                                         :translator-class clim-internals::selection-translator)
      (data)
    (if-let ((dump (clipboard-dump data)))
      (let ((pixels (extract-maria-pixels dump (clipboard-mode data)
                                          (clipboard-address data)
                                          (clipboard-width data))))
        (pixels-to-xpm pixels (clipboard-palette data)))
      "/* XPM: no pixel data available */"))

  ;; --- COPY: publish current resource as clipboard data ---

  (defun publish-current-resource ()
    "Publish the currently-edited resource to :clipboard with type
   animation-sequence-clipboard-data (or appropriate type)."
    (let ((frame (and (boundp '*application-frame*) *application-frame*)))
      (unless frame
        (return-from publish-current-resource
          (error  "~&No active frame to copy from.~%")))
      (typecase frame
        (anim-seq-editor-frame
         (let* ((seq (anim-seq-editor-sequence frame))
                (dump (ignore-errors (anim-seq-editor-cached-tile-dump frame)))
                (data (make-instance 'animation-sequence-clipboard
                                     :sequence seq
                                     :dump dump
                                     :mode (simple-animation-sequence-write-mode seq)
                                     :width (simple-animation-sequence-bytes-width seq)
                                     :address (or (ignore-errors
                                                   (anim-seq-editor-index frame)) 0))))
           (clime:publish-selection
            (clim:find-pane-named frame 'interactor)
            :clipboard data 'animation-sequence-clipboard-data)
           (error  "~&Copied animation sequence ~d to clipboard.~%"
                   (simple-animation-sequence-index seq))))
        (run-script-frame
         (let ((text (with-output-to-string (s)
                       (clim:with-text-face (s :bold)
                         (format s "Scripts for playtest~2%"))
                       (let ((last-area nil))
                         (dolist (sn (all-script-names :reloadp t))
                           (terpri s)
                           (let* ((parts (split-sequence #\/ sn))
                                  (area (elt parts (- (length parts) 2))))
                             (unless (string-equal area last-area)
                               (format s "~%~a~%~%" (cl-change-case:title-case area))
                               (setf last-area area)))
                           (format s "~a~%" (cl-ppcre:regex-replace
                                             "\\bDont\\b"
                                             (cl-change-case:title-case
                                              (subseq sn (1+ (position #\/ sn))))
                                             "Don't")))))))
           (clime:publish-selection
            (clim:find-pane-named frame 'interactor)
            :clipboard text 'clipboard-text)
           (error  "~&Copied script list (~d chars) to clipboard.~%" (length text))))
        (otherwise
         (let ((text (format nil "~a — ~a"
                             (or (ignore-errors (clim:frame-pretty-name frame)) "Skyline-Tool")
                             (multiple-value-bind (s m h d mo y) (get-decoded-time)
                               (declare (ignore s))
                               (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" y mo d h m)))))
           (clime:publish-selection
            (clim:find-pane-named frame 'interactor)
            :clipboard text 'clipboard-text)
           (error  "~&Copied frame info (~d chars) to clipboard.~%" (length text)))))))

;;; ============================================================
;;; CLIM debugger: copy full error text + backtrace to clipboard
;;; ============================================================

  (defun find-debugger-frame ()
    "Find the active clim-debugger frame on any port, or NIL."
    (clim:map-over-frames (lambda (f)
                            (when (typep f 'clim-debugger::clim-debugger)
                              (return-from find-debugger-frame f))))
    nil)

  (clim:define-command-table debugger-edit-menu
    :menu (("Copy Error Text" :command com-copy-error-text)))

  ;; Add an "Edit" menu to the debugger's command table containing the Copy command
  (eval-when (:load-toplevel :execute)
    (when (find-package :clim-debugger)
      (ignore-errors
       (clim:add-menu-item-to-command-table
        'clim-debugger::clim-debugger "Edit" :menu 'debugger-edit-menu))))

  (clim:define-command (com-copy-error-text
                        :command-table clim-debugger::clim-debugger)
      ()
    (block nil
      (let* ((frame (find-debugger-frame))
             (pane (and frame (clim:find-pane-named frame 'clim-debugger::debugger-pane)))
             (interactor (or (and frame (clim:find-pane-named frame 'interactor))
                             pane))
             (info (when pane (clim-debugger::condition-info pane))))
        (unless info
          (error  "~&No debugger info available.~%")
          (return))
        (let* ((type (clim-debugger::type-of-condition info))
               (message (clim-debugger::condition-message info))
               (condition (clim-debugger::the-condition info))
               (backtrace (clim-debugger::backtrace info))
               (restarts (clim-debugger::restarts info))
               (text (with-output-to-string (s)
                       (format s "Error: ~a~%" type)
                       (format s "Message: ~a~%" message)
                       (when condition
                         (format s "Condition: ~s~%" condition))
                       (format s "~%Backtrace:~%")
                       (dolist (backtrace-frame backtrace)
                         (let ((frame-text (clim-debugger::frame-string backtrace-frame)))
                           (format s "  ~d: ~a~%"
                                   (clim-debugger::frame-no backtrace-frame)
                                   frame-text)))
                       (format s "~%Restarts:~%")
                       (dolist (r restarts)
                         (format s "  ~a~%" (restart-name r))))))
          (clime:publish-selection (or interactor pane)
                                   :clipboard text 'string)
          (error  "~&Error text copied to clipboard (~d bytes).~%"
                  (length text))))))

;;; ============================================================
;;; Keyboard shortcuts (Ctrl + key) — added after command definition
;;; using add-keystroke-to-command-table so each gesture is
;;; normalized individually.
;;; ============================================================

  (clim:define-command (com-redisplay :command-table clim-internals::global-command-table) ()
    "Clear and redisplay the current window's panes."
    (when (boundp '*application-frame*)
      (clim:redisplay-frame-panes *application-frame* :force-p t)))

  (clim:define-command (com-duplicate :command-table clim-internals::global-command-table) ()
    "Duplicate the current resource (not yet implemented)."
    (error  "~&Duplicate is not yet implemented.~%"))

  (clim:define-command (com-undo :command-table clim-internals::global-command-table) ()
    "Undo last action (not yet implemented)."
    (error  "~&Undo is not yet implemented.~%"))

  (defmethod initialize-instance :after ((frame launcher-frame) &key)
    "Bind keyboard shortcuts when the launcher frame is created."
    (flet ((bind (command-table gesture command)
             (ignore-errors
              (clim:add-keystroke-to-command-table
               command-table gesture :command command))))
      ;; Global shortcuts — work in all frames via global-command-table
      (let ((gct 'clim-internals::global-command-table))
        (bind gct '(#\s :control) 'com-save-default)
        (bind gct '(#\p :control) 'com-print-default)
        (bind gct '(#\w :control) 'com-close-frame)
        (bind gct '(#\o :control) 'com-open-go-to)
        (bind gct '(#\d :control) 'com-duplicate)
        (bind gct '(#\f :control) 'com-find)
        (bind gct '(#\l :control) 'com-redisplay)
        (bind gct '(#\z :control) 'com-undo)
        (bind gct '(#\x :control) 'com-cut)
        (bind gct '(#\c :control) 'com-copy)
        (bind gct '(#\v :control) 'com-paste))
      ;; Launcher-specific: C-q Quit
      (bind 'launcher-frame '(#\q :control) 'com-quit-skyline-tool)
      ;; C-e Open for editing (also in global table)
      (bind 'clim-internals::global-command-table '(#\e :control) 'com-edit-file)))
