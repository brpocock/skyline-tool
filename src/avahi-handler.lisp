(in-package :skyline-tool)

;; mDNS Service Discovery Module
;; Provides service discovery via Avahi/Bonjour for Skyline-Tool instances,
;; CUPS printers, and SFTP servers.

(defvar *service-cache* (make-hash-table :test 'equal))
(defvar *cache-time* 0)
(defvar *offers-cache* nil
  "Cached list of pending offers from other Skyline-Tool instances.")
(defvar *offers-cache-time* 0
  "Universal time when offers cache was last refreshed.")

(defun discover-skyline-tool-instances ()
  "Discover nearby Skyline-Tool instances using mDNS.
   Returns a list of (name . host:port) pairs."
  (let ((services (error "unimplemented")))
    (when services
      (loop for info in services
            collect (cons (getf info :name)
                          (format nil "~a:~a" (getf info :host) (getf info :port)))))))

(defun discover-printers-service ()
  "Discover CUPS printers via mDNS/Bonjour.
   Returns a list of (name . queue-name) pairs."
  (let ((services (ignore-errors (discover-ipp-printers))))
    (when services
      (loop for info in services
            collect (cons (getf info :name)
                          (getf info :name))))))

(defun discover-sftp-servers ()
  "Find SFTP/SSH servers advertising Skyline-Tool services.
   Returns a list of (name . host:port) pairs."
  (let ((services (ignore-errors (discover-sftp-servers))))
    (when services
      (loop for info in services
            collect (cons (getf info :name)
                          (format nil "~a:~a" (getf info :host) (getf info :port)))))))

(defun discover-offers ()
  "Discover pending offers from other Skyline-Tool instances via DNS-SD.
   Returns a list of (offer-id . offer-info) pairs."
  (let ((now (get-universal-time)))
    (unless (and *offers-cache* (> (- now *offers-cache-time*) 30))
      (setf *offers-cache* nil
            *offers-cache-time* 0))
    (when (or (> (- now *offers-cache-time*) 30) (null *offers-cache*))
      (setf *offers-cache*
            (ignore-errors
              (let ((services (discover-skyline-offers)))
                (when services
                  (loop for info in services
                        collect (cons (getf info :offer-id) info)))))
            *offers-cache-time* (get-universal-time)))
    *offers-cache*))

(defun publish-offer (offer-id resource-info)
  "Advertise an offer via DNS-SD for other instances to discover.
   OFFER-ID is a unique identifier for this offer.
   RESOURCE-INFO is an alist describing the resource being shared."
  (declare (ignore resource-info))
  (error "Offer publishing via DNS-SD is not yet implemented. Use a file-based share or email instead."))

(defun retract-offer (offer-id)
  "Remove an offer from DNS-SD discovery."
  (declare (ignore offer-id))
  (error "Offer retraction via DNS-SD is not yet implemented."))

(defun refresh-services ()
  "Force refresh of all service discoveries."
  (setf *service-cache* nil
        *cache-time* (get-universal-time)
        *offers-cache* nil
        *offers-cache-time* 0)
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

(defun get-offers ()
  "Get list of pending offers from other instances.
   Returns a list of (offer-id . offer-info) pairs."
  (discover-offers))

(defun get-offer-display-name (offer-info)
  "Format an offer for display in the Send menu."
  (format nil "~a offers ~a"
          (getf offer-info :from-host "Unknown")
          (getf offer-info :resource-type "resource")))

(defun get-services-for-menu (menu-type)
  "Get services formatted for CLIM menu.
   MENU-TYPE can be :skyline-tool, :printer, :sftp, or :offers.
   Returns list of (display-name . value) pairs."
  (case menu-type
    (:offers
     (let ((offers (get-offers)))
       (mapcar (lambda (o)
                 (cons (get-offer-display-name (cdr o))
                       (car o)))
               offers)))
    (otherwise (get-matching-services menu-type))))

