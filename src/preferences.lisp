(in-package :skyline-tool)

(defvar *prefs-cache* nil
  "Cached preference plist loaded from the prefs file, or NIL if not yet loaded.")

(defun write-json-pretty (data stream &optional (depth 0))
  "Write DATA as pretty-printed JSON to STREAM using cl-json library."
  (write-string (json:encode-json-to-string data) stream))

(defun prefs-pathname ()
  "Return the pathname for the preferences file.
   Constructs ~/.config/Skyline-Tool/<GAME-TITLE>/<PORT>.lisp
   using *game-title* (Header-Case) and machine-directory-name."
  (make-pathname
   :directory (append (pathname-directory (user-homedir-pathname))
                      '(".config" "Skyline-Tool")
                      (list (header-case (if (boundp '*game-title*) *game-title* "Game"))))
   :name (machine-directory-name)
   :type "lisp"))

(defun load-prefs ()
  "Read the preferences JSON file and return a plist.
   Returns NIL if the file does not exist."
  (let ((path (prefs-pathname)))
    (when (probe-file path)
      (let ((alist (cl-json:decode-json-from-string (uiop:read-file-string path))))
        (loop for (key . value) in alist
              append (list (intern (string-upcase key) :keyword) value))))))

(defun save-prefs (plist)
  "Write PLIST as pretty-printed JSON to the preferences file.
   Creates the directory if it does not exist."
  (let ((path (prefs-pathname)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede
                            :external-format :utf-8)
      (write-json-pretty (loop for (key value) on plist by #'cddr
                               collect (cons (string-downcase (symbol-name key)) value))
                         s))))

(defun get-pref (key &optional default)
  "Read a preference value from the cached prefs.
   Returns DEFAULT (default NIL) if KEY is not found."
  (unless *prefs-cache*
    (setf *prefs-cache* (load-prefs)))
  (if *prefs-cache*
      (getf *prefs-cache* key default)
      default))

(defun set-pref (key value)
  "Set a preference value, update the cache, and save the file."
  (unless *prefs-cache*
    (setf *prefs-cache* (load-prefs)))
  (setf (getf *prefs-cache* key) value)
  (save-prefs *prefs-cache*)
  value)

(defvar *last-save-directory* nil
  "Last directory used by prompt-save-pathname for Save As dialogs.")

(defparameter *paper-sizes*
  '(("US-Letter" :width 612 :height 792 :units :points)
    ("A4" :width 595 :height 842 :units :points)
    ("Legal" :width 612 :height 1008 :units :points)
    ("Tabloid" :width 792 :height 1008 :units :points))
  "Supported paper sizes for PostScript output.")

(defun get-paper-size-prefs ()
  "Get paper size preferences, defaulting to US-Letter."
  (or (get-pref :paper-size "US-Letter")
      "US-Letter"))

(defun get-paper-dimensions (&optional (paper-name (get-paper-size-prefs)))
  "Return (width height) in points for PAPER-NAME."
  (let ((size (assoc (string-downcase paper-name) *paper-sizes* :test #'string-equal)))
    (if size
        (list (getf size :width) (getf size :height))
        (list 612 792)))) ; Default to US-Letter

(defun set-paper-size (paper-name)
  "Set the paper size preference."
  (set-pref :paper-size paper-name))

(defun find-save-directory ()
  "Return the best default directory for Save As dialogs.
   Checks *last-save-directory*, then saved preferences,
   then ~/work/, ~/Work/, ~/Documents/."
  (or *last-save-directory*
      (let ((saved (get-pref :last-save-directory)))
        (and saved (probe-file (pathname saved)) (pathname saved)))
      (let ((home (user-homedir-pathname)))
        (or (some (lambda (d) (let ((p (merge-pathnames d home)))
                                (when (probe-file p) p)))
                  '("work/" "Work/" "Documents/" "./"))))))

(defun prompt-save-pathname (default-name
                             &key (type (pathname-type default-name))
                                  prefs-key)
  "Prompt the user for a save pathname, trying zenity first then CLIM dialog.
   DEFAULT-NAME is the suggested filename (e.g. \"Sequence-5.json\").
   PREFS-KEY is a keyword used to persist the chosen directory in preferences.
   Returns the chosen pathname, or NIL if cancelled."
  (let* ((dir (find-save-directory))
         (default (merge-pathnames default-name dir)))
    ;; Try zenity for native Gnome dialog
    (or (ignore-errors
         (let* ((out (string-trim '(#\Newline #\Space)
                                  (uiop:run-program
                                   (list "zenity" "--file-selection" "--save"
                                         (format nil "--filename=~a" (namestring default))
                                         "--title=Save As...")
                                   :output :string :ignore-error-status t)))
                (path (when (and out (> (length out) 0)) (pathname out))))
           (when path
             (let ((dir (make-pathname :name nil :type nil :defaults path)))
               (setf *last-save-directory* dir)
               (when prefs-key
                 (set-pref prefs-key (namestring dir))
                 (set-pref :last-save-directory (namestring dir)))
               path))))
        ;; Fallback to file dialog
        (let ((path (run-text-input-dialog "Save As (enter path):" :initial-value (namestring default) :title "Save As")))
          (when path
            (let ((pathname-path (pathname path))
                  (dir (make-pathname :name nil :type nil :defaults (pathname path))))
              (setf *last-save-directory* dir)
              (when prefs-key
                (set-pref prefs-key (namestring dir))
                (set-pref :last-save-directory (namestring dir)))
              pathname-path))))))
