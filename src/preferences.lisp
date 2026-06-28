(in-package :skyline-tool)

(defvar *last-save-directory* nil
  "Last directory used by prompt-save-pathname for Save As dialogs.")

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
                  '("work/" "Work/" "Documents/"))
            (merge-pathnames "Work/" home)))))

(defun prompt-save-pathname (default-name &optional (type "txt") &key prefs-key)
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
        ;; Fallback to CLIM pathname prompter
        (let ((path (clim:accept 'pathname :prompt "Save As" :default default)))
          (when path
            (let ((dir (make-pathname :name nil :type nil :defaults path)))
              (setf *last-save-directory* dir)
              (when prefs-key
                (set-pref prefs-key (namestring dir))
                (set-pref :last-save-directory (namestring dir)))
              path))))))

(defvar *prefs-cache* nil
  "Cached preference plist loaded from the prefs file, or NIL if not yet loaded.")

(defun prefs-pathname ()
  "Return the pathname for the preferences file.
   Constructs ~/.config/Skyline-Tool/<GAME-TITLE>/<PORT>.prefs.json
   using *game-title* (capitalized) and machine-directory-name."
  (let ((game (string-capitalize (if (boundp '*game-title*) *game-title* "Game")))
        (port (ignore-errors (machine-directory-name))))
    (merge-pathnames
     (make-pathname :directory (list :relative ".config" "Skyline-Tool" game)
                    :name port
                    :type "prefs.json")
     (user-homedir-pathname))))

(defun load-prefs ()
  "Read the preferences JSON file and return a plist.
   Returns NIL if the file does not exist."
  (let ((path (prefs-pathname)))
    (when (probe-file path)
      (let ((alist (cl-json:decode-json-from-string (uiop:read-file-string path))))
        (loop for (key . value) in alist
              append (list (intern (string-upcase key) :keyword) value))))))

(defun write-json-pretty (data stream &optional (depth 0))
  "Write DATA as pretty-printed JSON to STREAM.
   DATA is an alist (→ object), list (→ array), string, number, or null.
   DEPTH controls indentation — start at 0."
  (labels ((indent (d) (format stream "~%~v@t" (* d 2)))
           (out (obj d)
             (etypecase obj
               (null (princ "null" stream))
               (string (format stream "~s" obj))
               (integer (princ obj stream))
               (float (format stream "~f" obj))
               (cons
                (if (and (car obj) (consp (car obj)))
                    (progn
                      (princ "{" stream)
                      (loop for (key . value) in obj
                            for sep = "" then ","
                            do (princ sep stream) (indent (1+ d))
                               (format stream "~s: " (string key))
                               (out value (1+ d)))
                      (when obj (indent d))
                      (princ "}" stream))
                    (progn
                      (princ "[" stream)
                      (loop for item in obj
                            for sep = "" then ", "
                            do (princ sep stream)
                               (if (and (consp item) (consp (car item)))
                                   (progn (indent (1+ d))
                                          (out item (1+ d))
                                          (indent d))
                                   (out item d)))
                      (princ "]" stream)))))))
    (out data depth)))

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
