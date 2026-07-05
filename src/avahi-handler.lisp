(in-package :skyline-tool)

;; mDNS Service Discovery Module
;; Provides service discovery via Avahi/Bonjour for Skyline-Tool instances,
;; CUPS printers, and SFTP servers.

(defvar *service-cache* (make-hash-table :test 'equal))
(defvar *cache-time* 0)

(defun discover-skyline-tool-instances ()
  "Discover nearby Skyline-Tool instances using mDNS.
   Returns a list of (name . host:port) pairs."
  (let ((services (ignore-errors (discover-avahi-services))))
    (when services
      (loop for (type info) in services
            when (eq type :skyline-tool)
            collect (cons (getf info :name)
                          (format nil "~a:~a" (getf info :host) (getf info :port)))))))

(defun discover-printers-service ()
  "Discover CUPS printers via mDNS/Bonjour.
   Returns a list of (name . queue-name) pairs."
  (let ((services (ignore-errors (discover-avahi-services))))
    (when services
      (loop for (type info) in services
            when (eq type :printer)
            collect (cons (getf info :name)
                          (getf info :name))))))

(defun discover-sftp-servers ()
  "Find SFTP/SSH servers advertising Skyline-Tool services.
   Returns a list of (name . host:port) pairs."
  (let ((services (ignore-errors (discover-avahi-services))))
    (when services
      (loop for (type info) in services
            when (eq type :sftp)
            collect (cons (getf info :name)
                          (format nil "~a:~a" (getf info :host) (getf info :port)))))))

(defun refresh-services ()
  "Force refresh of all service discoveries."
  (setf *service-cache* nil
        *cache-time* (get-universal-time))
  (discover-services))

(defun discover-services ()
  "Discover all services and populate cache."
  (let ((skyline (discover-skyline-tool-instances))
        (printers (discover-printers-service))
        (sftp (discover-sftp-servers)))
    (setf *service-cache* (list :skyline skyline
                                :printers printers
                                :sftp sftp)
          *cache-time* (get-universal-time))
    *service-cache*))

(defun get-matching-services (service-type)
  "Get formatted service list for menu display.
   Returns a list of (display-name . value) pairs for the given service type."
  (let ((now (get-universal-time)))
    (when (> (- now *cache-time*) 30)
      (discover-services))
    (let* ((cache *service-cache*)
           (services (ecase service-type
                       (:skyline-tool (getf cache :skyline))
                       (:printer (getf cache :printers))
                       (:sftp (getf cache :sftp)))))
      (mapcar (lambda (pair)
                (cons (car pair) (cdr pair)))
              services))))

;; Menu integration
(defun get-services-for-menu (menu-type)
  "Get services formatted for CLIM menu.
   MENU-TYPE can be :skyline-tool, :printer, or :sftp.
   Returns list of (display-name . value) pairs."
  (get-matching-services menu-type))

