(in-package :skyline-tool)

(defun parse-ods (pathname)
  "Read an OpenDocument Spreadsheet (.ods) file.
Returns list of worksheets/sheets and processed rows."""
  (let ((raw (read-ods-into-lists pathname)))
    (values raw (ss->lol (first raw))))

(defun parse-keys (pathname)
  "Parse a simple keys text file into a list of strings."
  (with-open-file (stream pathname :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          for trimmed = (string-trim '(#\Space #\Tab #\Return #\Newline) line)
          if (plusp (length trimmed))
            collect trimmed into result
          finally (return (nreverse result)))))