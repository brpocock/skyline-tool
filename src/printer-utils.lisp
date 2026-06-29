(in-package :skyline-tool)

(defvar *printer-cache* nil
  "Cached list of (queue-name . display-name) printer pairs, or NIL if no cache.")
(defvar *printer-cache-time* 0
  "Universal time when *printer-cache* was last refreshed.")

(defun discover-printers (&optional force)
  "Return a list of CUPS printer queue names (strings).
   Uses Drakma to fetch printer list from CUPS web interface.
   Results are cached for 30 seconds unless FORCE is true."
  (let ((now (get-universal-time)))
    (unless (and *printer-cache* (> (- now *printer-cache-time*) 30))
      (setf *printer-cache* nil
            *printer-cache-time* 0)))
  (when (or force (not *printer-cache*))
    (let ((html (ignore-errors
                   (drakma:http-request "http://localhost:631/printers/"
                                        :method :get
                                        :ignore-ssl-errors t
                                        :timeout 5))))
      (when html
        (setf *printer-cache*
              (sort (delete ""
                            (mapcar (lambda (s) (string-trim '(#\Space #\Tab)))
                                    (ppcre:split "\\s+" html))
                            :test #'string=)
                    #'string-lessp))
       *printer-cache-time* (get-universal-time)))
  *printer-cache*)

(defun discover-printers-with-names ()
  "Return a list of (queue-name . display-name) for CUPS printers.
   Display names come from CUPS HTML or lpstat descriptions."
  (flet ((trim (s) (string-trim '(#\Space #\Tab #\Newline) s)))
    (let ((queues (discover-printers))
          (result nil))
      (dolist (q queues)
        (let ((display q))
          ;; Try CUPS HTTP API for display name
          (ignore-errors
            (let* ((url (format nil "http://localhost:631/printers/~a" q))
                   (html (uiop:run-program
                          (list "curl" "-s" "--connect-timeout" "2" url)
                          :output :string :ignore-error-status t)))
              (when html
                ;; Look for "printer-info" or "printer-make-and-model" in IPP attrs
                (let ((m (ppcre:scan-to-strings
                          "printer-make-and-model[^>]*>([^<]+)"
                          html)))
                  (when (and m (aref m 0) (> (length (aref m 0)) 0))
                    (setf display (trim (aref m 0))))))))
          ;; Fallback: lpstat description
          (when (string= display q)
            (ignore-errors
              (let ((detail (uiop:run-program (list "lpstat" "-l" "-p" q)
                                               :output :string :ignore-error-status t)))
                (when (and detail (search "Description:" detail))
                  (let ((start (+ 12 (search "Description:" detail)))
                        (end (position #\Newline detail :start (search "Description:" detail))))
                    (let ((desc (trim (subseq detail start end))))
                      (when (> (length desc) 0) (setf display desc))))))))
          (push (cons q display) result)))
      (sort result #'string-lessp :key #'cdr))))
