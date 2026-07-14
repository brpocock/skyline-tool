(in-package :skyline-tool)

(defvar *avahi-browse-data* nil
  "Thread-local storage for browse results.")

(defvar *avahi-services-cache* nil
  "Cached list of discovered services.")
(defvar *avahi-services-cache-time* 0
  "Universal time when *avahi-services-cache* was last refreshed.")

(defun log-avahi-status (msg)
  "Log an Avahi-related status message to *trace-output*."
  (format *trace-output* "~&[Avahi] ~a~%" msg))

(defun discover-avahi-services (&optional force)
  "Discover services on the LAN using Avahi mDNS/Bonjour.
     Returns a list of (service-type . service-info) for discovered services.
     Service types include: :printer, :skyline-tool, :sftp."
  (let ((now (get-universal-time)))
    (unless (and *avahi-services-cache* (> (- now *avahi-services-cache-time*) 30))
      (setf *avahi-services-cache* nil
            *avahi-services-cache-time* 0)))
  (when (or force (not *avahi-services-cache*))
    (handler-case
        (let ((services '()))
          ;; Discover IPP printers
          (let ((printers (discover-ipp-printers)))
            (dolist (p printers)
              (push (cons :printer p) services)))
          ;; Discover Skyline-Tool instances
          (let ((skyline-instances (discover-skyline-tool-instances)))
            (dolist (s skyline-instances)
              (push (cons :skyline-tool s) services)))
          ;; Discover SFTP servers
          (let ((sftp-servers (discover-sftp-servers)))
            (dolist (f sftp-servers)
              (push (cons :sftp f) services)))
          ;; Discover pending offers
          (let ((offers (discover-offers)))
            (dolist (o offers)
              (push (cons :offer o) services)))
          (setf *avahi-services-cache* services
                *avahi-services-cache-time* (get-universal-time)))
      (error (e)
        (log-avahi-status (format nil "Avahi discovery failed: ~A" e))
        (setf *avahi-services-cache* nil
              *avahi-services-cache-time* 0)))
    *avahi-services-cache*))

(defun discover-ipp-printers ()
  "Discover CUPS/IPP printers using Avahi browse."
  (ignore-errors
    (let ((output (uiop:run-program
                   (list "avahi-browse" "-t" "_ipp._tcp" "-l" "-p")
                   :output :string :ignore-error-status t)))
      (when output
        (loop for line in (split-sequence:split-sequence #\Newline output)
              when (and line (search "+;" line))
              collect (parse-avahi-browse-line line :printer))))))

(defun discover-skyline-tool-instances ()
  "Discover other Skyline-Tool instances on the LAN."
  (ignore-errors
    (let ((output (uiop:run-program
                   (list "avahi-browse" "-t" "_skyline-tool._tcp" "-l" "-p")
                   :output :string :ignore-error-status t)))
      (when output
        (loop for line in (split-sequence:split-sequence #\Newline output)
              when (and line (search "+;" line))
              collect (parse-avahi-browse-line line :skyline-tool))))))

(defun discover-sftp-servers ()
  "Discover SFTP/SSH servers on the LAN."
  (ignore-errors
    (let ((output (uiop:run-program
                   (list "avahi-browse" "-t" "_ssh._tcp" "-l" "-p")
                   :output :string :ignore-error-status t)))
      (when output
        (loop for line in (split-sequence:split-sequence #\Newline output)
              when (and line (search "+;" line))
              collect (parse-avahi-browse-line line :sftp))))))

(defun discover-offers ()
  "Discover pending resource offers via DNS-SD (_skyline-offer._tcp).
   Returns a list of (offer-id . offer-info) pairs."
  (ignore-errors
    (let ((output (uiop:run-program
                   (list "avahi-browse" "-t" "_skyline-offer._tcp" "-l" "-p")
                   :output :string :ignore-error-status t)))
      (when output
        (loop for line in (split-sequence:split-sequence #\Newline output)
              when (and line (search "+;" line))
              collect (parse-avahi-browse-line line :offer))))))

(defun parse-avahi-browse-line (line &optional service-type)
  "Parse an Avahi browse output line into service info.
   SERVICE-TYPE is the type of service (:skyline-tool, :sftp, :offer, etc.)."
  (let ((parts (split-sequence:split-sequence #\; line)))
    (when (>= (length parts) 12)
      (let ((name (string-trim '(#\Space #\Tab) (nth 7 parts)))
            (domain (nth 11 parts))
            (host (nth 9 parts)))
        (when (plusp (length name))
          (ecase service-type
            (:skyline-tool (list :name name :domain domain :host host))
            (:sftp (list :name name :domain domain :host host))
            (:offer (list :offer-id name :domain domain :host host
                          :resource-type (nth 12 parts)
                          :from-host host))))))))

(defun get-recipient-list ()
  "Get a formatted list of recipients for Send menu.
     Returns a list of strings like 'User Name on Host Display Name'."
  (let ((services (discover-avahi-services))
        (recipients '()))
    (dolist (service services)
      (let ((type (first service))
            (info (rest service)))
        (case type
          (:skyline-tool
           (push (format nil "~a on ~a" 
                        (get-user-full-name) 
                        (getf info :name)) recipients))
          (:sftp
           (push (format nil "SFTP: ~a" (getf info :name)) recipients)))))
    recipients))

(defun get-printer-list ()
  "Get a formatted list of printers for Print To menu.
     Returns a list of strings like 'Printer Display Name'."
  (let ((services (discover-avahi-services))
        (printers '()))
    (dolist (service services)
      (when (eq (first service) :printer)
        (let ((info (rest service)))
          (push (getf info :name) printers))))
    printers))

(defun get-user-full-name ()
  "Get the user's full name from GECOS or environment."
  (or (ignore-errors
        (uiop:run-program (list "getent" "passwd" (uiop:getenv "USER"))
                         :output :string :error-output nil))
      (uiop:getenv "USER")
      "Unknown User"))

(defun get-host-display-name ()
  "Get the host display name."
  (ignore-errors
    (uiop:run-program (list "hostname")
                     :output :string :error-output nil)))

(defun refresh-avahi-services ()
  "Force refresh of the Avahi services cache."
  (discover-avahi-services t))
