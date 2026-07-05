(in-package :skyline-tool)

(defun prompt-save-pathname (default-name &key prefs-key)
  "Prompt user for a pathname to save to, returning a pathname or NIL if cancelled.
   DEFAULT-NAME is the suggested filename. PREF-KW is a key for remembering the last used directory."
  (declare (ignore prefs-key))
  (format *query-io* "~&Save as (default ~a): " default-name)
  (let ((input (read-line *query-io*)))
    (when (and input (plusp (length input)))
      (let ((path (enough-namestring input)))
        (when (probe-file path)
          (format *query-io* "File exists, overwrite? (y/n): ")
          (unless (char-equal #\y (read-char *query-io*))
            (return-from prompt-save-pathname nil)))
        path))))

(defun generate-game-title ()
  "Generate a game title based on current project configuration."
  (if (and (boundp '*game-title*) *game-title*)
      (string-capitalize *game-title*)
      "Skyline-Tool Game"))

(defun escape-ps-string (string)
  "Escape special characters in STRING for PostScript output."
  (declare (type string string))
  (with-output-to-string (s)
    (loop for char across string
          do (case char
               (#\\ (princ "\\\\" s))
               (#\( (princ "\\(" s))
               (#\) (princ "\\)" s))
               (#\  (princ "\\ " s))
               (otherwise (princ char s))))))