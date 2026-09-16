;;; Skyline-Tool src/p2p-sharing.lisp
;;; Peer-to-peer resource sharing infrastructure
;;; Updated with HMAC offer IDs, resource locking, transfer tracking

(in-package :skyline-tool)

;; Secret key management for HMAC
(defvar *p2p-hmac-secret* nil
  "HMAC secret key auto-generated if nil or after network errors")

;; Resource locking for transfer safety
(defvar *resource-locks* (make-hash-table :test 'equal)
  "Locks for resources during transfer")

(defstruct resource-lock
  (id nil :type string)
  (holder nil :type string)
  (timestamp 0 :type integer)
  (expires-at 0 :type integer))

;; Transfer tracking
(defvar *active-transfers* (make-hash-table :test 'equal)
  "Active transfer states")

(defstruct active-transfer
  (id nil :type string)
  (resource nil :type game-resource)
  (from-host nil :type string)
  (to-host nil :type string)
  (start-time 0 :type integer)
  (status :pending :type symbol))

;; HMAC key management
(defun ensure-p2p-secret-key ()
  "Generate or reset HMAC secret key"
  (when (null *p2p-hmac-secret*)
    (setf *p2p-hmac-secret* (generate-random-key))))

(defun generate-random-key ()
  "Generate cryptographically random key using ironclad"
  (let* ((bytes (ironclad:random-data 48)) ; 48 bytes -> 96 hex chars
         (hex-string (ironclad:byte-array-to-hex-string bytes)))
    (format nil "~a" hex-string)))



;; HMAC-based offer ID generation
(defun generate-hmac-offer-id (resource recipient)
  "Generate collision-resistant offer ID using HMAC-SHA256"
  (ensure-p2p-secret-key)
  (let* ((timestamp (get-universal-time))
         (data (format nil "~a-~a-~a-~a" 
                       (game-resource-title resource)
                       recipient
                       timestamp
                       (random 1000000)))
         (hmac-digest (ironclad:hmac-digest
                        (ironclad:update-hmac
                         (ironclad:make-hmac (ironclad:ascii-string-to-byte-array *p2p-hmac-secret*)
                                            :sha256)
                         (ironclad:ascii-string-to-byte-array data))))
         (hmac (ironclad:byte-array-to-hex-string hmac-digest)))
    (format nil "offer-~a-~a" hmac timestamp)))

;; Resource locking
(defun acquire-resource-lock (resource-id holder)
  "Acquire lock for resource during transfer"
  (let* ((now (get-universal-time))
         (expires (+ now 1800))) ; 30 minute expiry
    (setf (gethash resource-id *resource-locks*)
          (make-resource-lock
           :id resource-id
           :holder holder
           :timestamp now
           :expires-at expires))))

(defun release-resource-lock (resource-id)
  "Release resource lock after transfer"
  (remhash resource-id *resource-locks*))

(defun check-lock-expiry (resource-id)
  "Check if resource lock has expired"
  (let ((lock (gethash resource-id *resource-locks*)))
    (and lock (> (getf lock :expires-at) (get-universal-time)))))

;; Transfer tracking
(defun start-transfer-tracking (offer-id resource from-host to-host)
  "Begin tracking a resource transfer"
  (ensure-p2p-secret-key)
  (setf (gethash offer-id *active-transfers*)
        (make-active-transfer
         :id offer-id
         :resource resource
         :from-host from-host
         :to-host to-host
         :start-time (get-universal-time)
         :status :transferring)))

(defun complete-transfer-tracking (offer-id)
  "Mark transfer as completed"
  (let ((transfer (gethash offer-id *active-transfers*)))
    (when transfer
      (setf (active-transfer-status transfer) :completed)
      (remhash offer-id *active-transfers*))))

;; Restartable send resource
(defun send-resource-to-peer (resource host port)
  "Send RESOURCE to peer with restartable error handling"
  (restart-case
      (progn
        (ensure-p2p-secret-key)
        (drakma:http-request
         (format nil "http://~a:~d/skyline-tool/resource" host port)
         :method :post
         :content-type "application/json"
         :content (cl-json:encode-json-to-string (resource-to-json resource))))
    (retry-connection ()
      :report "Retry connection to ~a:~d" host port
      (send-resource-to-peer resource host port))
    (abort-send ()
      :report "Abort sending resource to ~a:~d" host port
      (values nil :aborted))))

;; Send with full P2P integration
(defun send-to (resource recipient)
  "Send RESOURCE to RECIPIENT with HMAC offer ID and tracking"
  (let* ((parts (split-sequence #\: recipient))
         (host (first parts))
         (port (parse-integer (second parts) :junk-allowed t))
         (offer-id (generate-hmac-offer-id resource recipient)))
    (unless (and host port)
      (error "Invalid recipient format: ~a (expected host:port)" recipient))
    (ensure-peer-sharing-started)
    (restart-case
        (progn
          (acquire-resource-lock (game-resource-title resource) offer-id)
          (start-transfer-tracking offer-id resource host recipient)
          (send-resource-to-peer resource host port)
          (complete-transfer-tracking offer-id)
          (release-resource-lock (game-resource-title resource))
          offer-id)
      (retry-send ()
        :report "Retry sending to ~a:~d" host port
        (send-to resource recipient))
      (abort-send ()
        :report "Abort sending to ~a:~d" host port
        (values nil :aborted)))))

;; Accept offer with restart cases
(defun accept-from (offer-id)
  "Accept an incoming offer by OFFER-ID with restartable error handling"
  (restart-case
      (let ((offer-info (find-offer-by-id offer-id)))
        (unless offer-info
          (error "Offer not found: ~a" offer-id))
        (let ((resource (receive-shared-resource (getf offer-info :json))))
          (complete-transfer-tracking offer-id)
          resource))
    (retry-accept ()
      :report "Retry accepting offer ~a" offer-id
      (accept-from offer-id))
    (reject-offer ()
      :report "Reject offer ~a" offer-id
      (values nil :rejected))))

;; Discover peers via Avahi
(defun discover-p2p-recipients ()
  "Return list of (name . \"host:port\") for discovered P2P peers"
  (mapcar (lambda (service)
            (cons (getf service :name)
                  (format nil "~a:~d" (getf service :host) (getf service :port))))
          (discover-skyline-tool-instances)))

;; Friendly name generation using get-pref
(defun generate-sharing-name (mode)
  "Generate friendly name based on get-pref format string"
  (format nil "~a on ~a — ~a ~a"
          (user-real-name)
          (machine-instance)
          *game-title*
          (machine-directory-name)))

;; mDNS service registration
(defun advertise-sharing-service (mode port)
  "Advertise sharing service via Avahi with OS-assigned port"
  (when (get-pref (intern (format nil "~a-ADVERTISE" mode) :keyword))
    (publish-skyline-tool-service
     :name (generate-sharing-name mode)
     :port (if (= port 0) nil port)  ; port 0 means OS auto-assign
     :type (intern (format nil "~a._tcp" mode) :keyword))))

;; Server initialization
(defun start-resource-sharing-server ()
  "Start resource sharing server with OS-assigned port"
  (setf *resource-sharing-acceptor*
        (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 0)))
  (let ((port (hunchentoot:local-port *resource-sharing-acceptor*)))
    (publish-skyline-tool-service
     :port port
     :name (generate-sharing-name :resource-sharing))))

(defun ensure-peer-sharing-started ()
  "Ensure resource sharing server is running"
  (unless (and *resource-sharing-acceptor*
               (hunchentoot:started-p *resource-sharing-acceptor*))
    (start-resource-sharing-server)))

;; Offer management
(defvar *pending-offers* (make-hash-table :test 'equal)
  "Hash table of pending offers")

(defun find-offer-by-id (offer-id)
  "Find an offer by its ID"
  (gethash offer-id *pending-offers*))

(defun register-offer (offer-id json-data)
  "Register a pending offer"
  (setf (gethash offer-id *pending-offers*)
        (list :id offer-id :json json-data :timestamp (get-universal-time))))

;; Network error handling
(defvar *network-error-flag* nil
  "Flag to trigger HMAC key reset on network errors")

(defun handle-network-error ()
  "Clear HMAC key to force regeneration after errors"
  (setf *network-error-flag* t
        *p2p-hmac-secret* nil))

;; Utility for debugging
(defun list-active-transfers ()
  "Return list of active transfers"
  (let ((transfers '()))
    (maphash (lambda (_ transfer)
               (declare (ignore _))
               (push transfer transfers))
             *active-transfers*)
    transfers))

(defun list-resource-locks ()
  "Return list of current resource locks"
  (let ((locks '()))
    (maphash (lambda (_ lock)
               (declare (ignore _))
               (push lock locks))
             *resource-locks*)
    locks))
