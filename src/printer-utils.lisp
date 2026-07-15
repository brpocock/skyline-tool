(in-package :skyline-tool)

;; Whitespace characters for string trimming
(define-constant +whitespace+ (coerce #(#\Space #\Tab #\Page #\Return #\Linefeed
                                         #\Non-Breaking_Space)
                                       'string)
  :test #'string-equal)

(defvar *printer-cache* nil
  "Cached list of (queue-name . display-name) printer pairs, or NIL if no cache.")
(defvar *printer-cache-time* 0
  "Universal time when *printer-cache* was last refreshed.")
(defvar *cups-port* nil
  "Cached CUPS port number from libc services database, or 631 if not found.")

(defvar *printer-discovery-thread* nil
  "Background thread that periodically discovers printers and publishes changes.")
(defvar *printer-discovery-running-p* nil
  "Flag indicating whether the printer discovery thread is running.")

(defun log-printer-status (msg)
  "Log a printer-related status message to *trace-output*."
  (format *trace-output* "~&[Printer] ~a~%" msg))

(defun get-cups-port ()
  "Get CUPS port number from libc services database at runtime, defaulting to 631.
   Uses get-service-port from libc-services which wraps getservbyname."
  (or (ignore-errors (get-service-port "ipp" "tcp"))
      631))

(defun discover-printers (&optional force)
  "Return a list of CUPS printer queue names using IPP.
   Uses IPP Get-Printers operation to fetch printer list from CUPS server.
   Results are cached for 30 seconds unless FORCE is true.
   Does NOT use lpstat - pure IPP implementation."
  (let ((now (get-universal-time)))
    (unless (and *printer-cache* (> (- now *printer-cache-time*) 30))
      (setf *printer-cache* nil
            *printer-cache-time* 0))
    (when (or force (not *printer-cache*))
      (handler-case
          (let* ((port (get-cups-port))
                 (url (format nil "http://localhost:~a/printers" port))
                 (response (drakma:http-request url
                                                :method :get
                                                :accept "application/ipp")))
            (if (and response (plusp (length response)))
                (let ((printers (parse-ipp-response response)))
                  (setf *printer-cache* printers
                        *printer-cache-time* (get-universal-time)))
                (progn
                  (log-printer-status "CUPS IPP request returned no data")
                  (setf *printer-cache* nil))))
        (error (e)
          (log-printer-status (format nil "CUPS discovery failed: ~A" e))
          (setf *printer-cache* nil
                *printer-cache-time* 0)
          (error e)))
      *printer-cache*)))

(defun parse-ipp-response (data)
  "Parse IPP response to extract printer names.
   NO FALLBACK - pure IPP parsing only.
   Returns list of printer queue names."
  (let (printers)
    ;; Parse IPP response for printer names
    (loop for line in (split-sequence:split-sequence #\Newline data)
          when (and line (search "printer-name" line :test #'char-equal))
            do (let ((name-start (search "printer-name" line :test #'char-equal)))
                 (when name-start
                   (let ((after-name (subseq line (+ name-start 12))))
                     (let ((name (string-trim '(#\Space #\Tab #\Newline) after-name)))
                       (when (plusp (length name))
                         (push name printers)))))))
    (when printers
      (nreverse printers))))

(defun discover-printers-with-names ()
  "Return a list of (queue-name . display-name) for CUPS printers.
   Uses IPP for discovery and IPP Get-Printer-Attributes for names."
  (let ((queues (discover-printers))
        (result nil))
    (when queues
      (dolist (q queues)
        (let ((display q)) ; Default to queue name
          ;; Try to get printer description via IPP Get-Printer-Attributes
          (ignore-errors
           (let* ((port (get-cups-port))
                  (url (format nil "http://localhost:~d/printers/~a" port q))
                  (response (with-timeout (10)
                              (drakma:http-request url
                                                   :method :get
                                                   :accept "application/ipp"))))
             (when (and response (plusp (length response)))
               (let ((desc (parse-ipp-printer-attributes-response response)))
                 (when (and desc (plusp (length desc)))
                   (setf display desc))))))
          (push (cons q display) result)))
      (sort result #'string-lessp :key #'cdr))))

(defun parse-ipp-printer-attributes-response (data)
  "Parse IPP Get-Printer-Attributes response to extract printer description.
   Returns description string or NIL if not found."
  (loop for line in (split-sequence:split-sequence #\Newline data)
        when (and line (search "printer-name" line :test #'char-equal))
          do (let ((name-start (search "printer-name" line :test #'char-equal)))
               (when name-start
                 (let ((after-name (subseq line (+ name-start 12))))
                   (let ((name (string-trim '(#\Space #\Tab #\Newline) after-name)))
                     (when (plusp (length name))
                       (return name))))))))

;; 
;; Event‑bus publishing for printer changes
;; 
(defvar *printer-event-handlers* nil
  "List of symbols that are bound to a function that will be called
   when the printer list changes.  The symbol must be a *command* name
   that accepts one argument – the alist returned by DISCOVER‑PRINTERS‑WITH‑NAMES.")

(defun subscribe-printer-change (command-symbol)
  "Bind COMMAND-SYMBOL to be called with the current printer alist
   whenever the list changes.  Use UNINITIALIZE-SUBSCRIBER to remove."
  (push command-symbol *printer-event-handlers*))

(defun publish-printer-change (printer-alist)
  "Invoke every handler bound via SUBSRIBE-PRINTER-CHANGE.
   The handlers are expected to be commands that take one argument –
   the printer alist."
  (dolist (fn *printer-event-handlers*)
    (when (fboundp fn)
      (funcall fn printer-alist))))

;; 
;; Background discovery thread
;; 
(defun printer-discovery-loop ()
  "Loop that periodically discovers printers and publishes changes."
  (loop while *printer-discovery-running-p*
        do (let ((new-list (ignore-errors (discover-printers-with-names))))
             (unless (equal new-list *printer-cache*)
               (setf *printer-cache* new-list)
               (publish-printer-change new-list)))
           (sleep 5))) ; poll every 5 seconds

(defun start-printer-discovery-thread ()
  "Start the background printer discovery thread if it is not already running."
  (unless *printer-discovery-running-p*
    (setf *printer-discovery-running-p* t)
    (setf *printer-discovery-thread*
          (make-thread #'printer-discovery-loop :name "Printer Discovery Thread"))))

(defun stop-printer-discovery-thread ()
  "Stop the background printer discovery thread."
  (when *printer-discovery-thread*
    (bt:destroy-thread *printer-discovery-thread*)
    (setf *printer-discovery-thread* nil
          *printer-discovery-running-p* nil)))

(defun ensure-printer-discovery-started ()
  "Start the background printer discovery thread if not already running.
   Called when a printable window is opened."
  (unless *printer-discovery-running-p*
    (start-printer-discovery-thread)))
