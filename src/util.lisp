(in-package :skyline-tool)

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
