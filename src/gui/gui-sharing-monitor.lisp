;;; Skyline-Tool src/gui/gui-sharing-monitor.lisp
;;; Sharing Monitor - comprehensive P2P monitoring for all three services

(in-package :skyline-tool)

;; =============================================================================
;; Data structures for three P2P services
;; =============================================================================

(defstruct p2p-recipient
  "Information about a remote P2P peer"
  (name "" :type string)              ; "Bruce-Robert Pocock on Hermes"
  (host "" :type string)              ; "hermes.local"
  (ip "" :type string)                ; "10.80.20.182"
  (username "" :type string)          ; "brpocock" or "anonymous"
  (service-type :resource :type keyword) ; :resource, :dist-webdav, :music-dlna
  (ident-resolved-p nil :type boolean)
  (active-transfers nil :type list)   ; List of active-transfer structs
  (pending-offers nil :type list))    ; List of pending-offer structs

(defstruct active-transfer
  "An actively transferring resource"
  (offer-id "" :type string)
  (resource nil :type game-resource)
  (resource-title "" :type string)
  (resource-kind "" :type string)
  (bytes-transferred 0 :type integer)
  (total-bytes 0 :type integer)
  (start-time 0 :type integer)
  (last-speed-sample nil :type list)) ; Rolling 1-minute speed samples

(defstruct pending-offer
  "A sent offer not yet accepted"
  (offer-id "" :type string)
  (resource nil :type game-resource)
  (resource-title "" :type string)
  (resource-kind "" :type string)
  (sent-time 0 :type integer))

;; Global state for monitoring
(defvar *p2p-recipients* (make-hash-table :test 'equal)
  "Hash table of known recipients by name")

(defvar *sharing-monitor-frame* nil
  "Current sharing monitor frame instance")

;; =============================================================================
;; Identd resolution with anonymous fallback
;; =============================================================================

(defvar *ident-cache* (make-hash-table :test 'equal :weakness :value)
  "Cache of identd query results: host -> username")

(defun resolve-username-via-identd (host &optional (port 113))
  "Resolve username via identd (RFC 1413). Returns username or \"anonymous\"."
  (let ((cached (gethash host *ident-cache*)))
    (when cached
      (return-from resolve-username-via-identd cached)))
  
  (handler-case
      (let* ((socket (usocket:socket-connect host port :protocol :stream))
             (stream (usocket:socket-stream socket))
             (local-port (get-pref :p2p-resource-sharing-port 0))
             (remote-port local-port)) ; Same port for P2P
        (format stream "~d, ~d~%" remote-port local-port)
        (force-output stream)
        (let ((response (read-line stream nil "")))
          (usocket:socket-close socket)
          (let ((parts (split-sequence #\: response)))
            (when (and (= (length parts) 6)
                       (string-equal (string-trim " " (third parts)) "USERID")
                       (string-equal (string-trim " " (fourth parts)) "UNIX"))
              (let ((username (string-trim " " (fifth parts))))
                (setf (gethash host *ident-cache*) username)
                (return-from resolve-username-via-identd username))))))
    (error (c)
      (declare (ignore c))
      (setf (gethash host *ident-cache*) "anonymous")
      "anonymous")))

(defun get-recipient-username (recipient)
  "Get username for recipient, using identd if not yet resolved"
  (unless (p2p-recipient-ident-resolved-p recipient)
    (setf (p2p-recipient-username recipient)
          (resolve-username-via-identd (p2p-recipient-host recipient)))
    (setf (p2p-recipient-ident-resolved-p recipient) t))
  (p2p-recipient-username recipient)

;; =============================================================================
;; Service monitoring functions
;; =============================================================================

(defun get-resource-sharing-recipients ()
  "Get list of recipients for resource sharing (mDNS/HTTP)"
  (let ((recipients nil))
    (maphash (lambda (name info)
               (declare (ignore name))
               (push (make-p2p-recipient
                      :name (getf info :name)
                      :host (getf info :host)
                      :ip (getf info :ip)
                      :service-type :resource
                      :active-transfers (get-active-transfers-for-host (getf info :host))
                      :pending-offers (get-pending-offers-for-host (getf info :host)))
                     recipients))
             (gethash "_skyline-tool-service._tcp" *discovered-services*))
    (nreverse recipients)))

(defun get-dist-webdav-recipients ()
  "Get list of recipients for Dist/ WebDAV sharing"
  (let ((recipients nil))
    (maphash (lambda (name info)
               (declare (ignore name))
               (push (make-p2p-recipient
                      :name (getf info :name)
                      :host (getf info :host)
                      :ip (getf info :ip)
                      :service-type :dist-webdav
                      :active-transfers (get-webdav-transfers-for-host (getf info :host))
                      :pending-offers nil) ; WebDAV doesn't use offer system
                     recipients))
             (gethash "_webdav._tcp" *discovered-services*))
    (nreverse recipients)))

(defun get-dlna-recipients ()
  "Get list of recipients for DLNA music sharing"
  (let ((recipients nil))
    (maphash (lambda (name info)
               (declare (ignore name))
               (push (make-p2p-recipient
                      :name (getf info :name)
                      :host (getf info :host)
                      :ip (getf info :ip)
                      :service-type :music-dlna
                      :active-transfers nil
                      :pending-offers nil)
                     recipients))
             (gethash "_http._tcp" *discovered-services*)) ; DLNA uses HTTP
    (nreverse recipients))

(defun get-all-recipients ()
  "Get combined list of all recipients across all three services"
  (append (get-resource-sharing-recipients)
          (get-dist-webdav-recipients)
          (get-dlna-recipients)))

;; =============================================================================
;; Transfer tracking helpers
;; =============================================================================

(defun get-active-transfers-for-host (host)
  "Get active transfers for a specific host"
  (let ((transfers nil))
    (maphash (lambda (offer-id transfer)
               (when (string= (active-transfer-to-host transfer) host)
                 (push (make-active-transfer
                        :offer-id offer-id
                        :resource (active-transfer-resource transfer)
                        :resource-title (game-resource-title (active-transfer-resource transfer))
                        :resource-kind (game-resource-kind (active-transfer-resource transfer))
                        :bytes-transferred (active-transfer-bytes-transferred transfer)
                        :total-bytes (active-transfer-total-bytes transfer)
                        :start-time (active-transfer-start-time transfer)
                        :last-speed-sample (active-transfer-last-speed-sample transfer))
                       transfers)))
             *active-transfers*)
    (nreverse transfers)))

(defun get-pending-offers-for-host (host)
  "Get pending offers for a specific host"
  (let ((offers nil))
    (maphash (lambda (offer-id offer-info)
               (when (string= (getf offer-info :host) host)
                 (let ((resource (getf offer-info :resource)))
                   (push (make-pending-offer
                          :offer-id offer-id
                          :resource resource
                          :resource-title (game-resource-title resource)
                          :resource-kind (game-resource-kind resource)
                          :sent-time (getf offer-info :timestamp))
                         offers))))
             *pending-offers*)
    (nreverse offers))

(defun get-webdav-transfers-for-host (host)
  "Get active WebDAV transfers for a host"
  ;; WebDAV transfers tracked separately
  (let ((transfers nil))
    (maphash (lambda (key transfer)
               (when (string= (getf transfer :host) host)
                 (push (make-active-transfer
                        :offer-id (format nil "webdav-~a" key)
                        :resource nil
                        :resource-title (getf transfer :path)
                        :resource-kind "File"
                        :bytes-transferred (getf transfer :bytes-sent)
                        :total-bytes (getf transfer :total-size)
                        :start-time (getf transfer :start-time))
                       transfers)))
             *webdav-transfers*)
    (nreverse transfers))

;; WebDAV transfer tracking
(defvar *webdav-transfers* (make-hash-table :test 'equal)
  "Active WebDAV file transfers")

;; =============================================================================
;; Progress calculations
;; =============================================================================

(defun calculate-transfer-progress (transfer)
  "Calculate progress percentage for a transfer"
  (if (and (> (active-transfer-total-bytes transfer) 0)
           (> (active-transfer-bytes-transferred transfer) 0))
      (min 100
           (floor (* 100 (active-transfer-bytes-transferred transfer)
                     (active-transfer-total-bytes transfer)))
      0))

(defun calculate-overall-progress (recipient)
  "Calculate overall progress across all transfers for a recipient"
  (let* ((transfers (p2p-recipient-active-transfers recipient))
         (total-bytes (reduce #'+ transfers :key #'active-transfer-total-bytes))
         (transferred (reduce #'+ transfers :key #'active-transfer-bytes-transferred)))
    (if (> total-bytes 0)
        (floor (* 100 transferred total-bytes))
        0)))

(defun estimate-time-remaining (transfer)
  "Estimate time remaining based on last minute's average speed"
  (let* ((samples (active-transfer-last-speed-sample transfer))
         (bytes-left (- (active-transfer-total-bytes transfer)
                        (active-transfer-bytes-transferred transfer)))
         (avg-speed (when samples
                      (floor (/ (reduce #'+ samples) (length samples))))))
    (when (and avg-speed (> avg-speed 0))
      (floor (/ bytes-left avg-speed)))))

(defun format-time-remaining (seconds)
  "Format seconds as H:MM:SS or M:SS"
  (cond
    ((>= seconds 3600)
     (format nil "~d:~2,'0d:~2,'0d"
             (floor seconds 3600)
             (floor (mod seconds 3600) 60)
             (mod seconds 60)))
    (t
     (format nil "~d:~2,'0d"
             (floor seconds 60)
             (mod seconds 60)))))

;; =============================================================================
;; Sharing Monitor Frame
;; =============================================================================

(clim:define-application-frame sharing-monitor-frame (uniform-inspector-frame)
  ((selected-recipient :initform nil :accessor frame-selected-recipient)
   (details-expanded-p :initform nil :accessor frame-details-expanded-p)
   (auto-refresh-timer :initform nil :accessor frame-auto-refresh-timer))
  (:panes
   (service-tabs :application
                 :display-function 'display-service-tabs
                 :scroll-bars nil
                 :height 40 :width 800)
   (recipient-list :application
                   :display-function 'display-recipient-list
                   :scroll-bars :vertical
                   :height 300 :width 800)
   (details-pane :application
                 :display-function 'display-recipient-details
                 :scroll-bars :vertical
                 :height 300 :width 800
                 :visible t)
   (status-bar :application
               :display-function 'display-monitor-status
               :height 30 :width 800))
  (:layouts
   (default (clim:vertically ()
              service-tabs
              recipient-list
              details-pane
              status-bar)))
  (:menu-bar sharing-monitor-menu-bar)
  (:pretty-name "Sharing Monitor")
  (:icon (skyline-tool-icon :resource :sharing-monitor)))

;; =============================================================================
;; Command Tables
;; =============================================================================

(clim:define-command-table sharing-monitor-file-menu
  :menu (("Refresh Now" :command com-refresh-sharing-monitor)
         (nil :divider :line)
         ("Auto-Refresh" :command com-toggle-auto-refresh :toggle t)
         (nil :divider :line)
         ("Close" :command com-close-sharing-monitor)))

(clim:define-command-table sharing-monitor-recipient-menu
  :menu (("View Details" :command com-toggle-recipient-details)
         ("Cancel All Transfers" :command com-cancel-all-transfers)
         ("Cancel All Offers" :command com-cancel-all-offers)
         (nil :divider :line)
         ("Copy Recipient Info" :command com-copy-recipient-info)))

(clim:define-command-table sharing-monitor-transfer-menu
  :menu (("Cancel Transfer" :command com-cancel-selected-transfer)
         ("Retry Transfer" :command com-retry-selected-transfer)
         (nil :divider :line)
         ("Copy Transfer ID" :command com-copy-transfer-id)))

(clim:define-command-table sharing-monitor-offer-menu
  :menu (("Cancel Offer" :command com-cancel-selected-offer)
         ("Resend Offer" :command com-resend-selected-offer)
         (nil :divider :line)
         ("Copy Offer ID" :command com-copy-offer-id)))

(clim:define-command-table sharing-monitor-menu-bar
  :menu (("File" :menu sharing-monitor-file-menu)
         ("Recipient" :menu sharing-monitor-recipient-menu)
         ("Transfer" :menu sharing-monitor-transfer-menu)
         ("Offer" :menu sharing-monitor-offer-menu)
         ("Help" :menu inspector-help-menu)))

;; =============================================================================
;; Display Functions
;; =============================================================================

(defun display-service-tabs (frame pane)
  (let ((*standard-output* pane))
    (clim:formatting-table (pane :x-spacing 20)
      (clim:formatting-row (pane)
        (loop for (service label) in
              '((:resource "Resource Sharing")
                (:dist-webdav "Dist File Sharing")
                (:music-dlna "Media Sharing"))
              do (clim:formatting-cell (pane)
                   (clim:with-text-face (pane :bold)
                     (let ((count (count-service-recipients service)))
                       (format pane "~a: ~d" label count))))))))

(defun count-service-recipients (service)
  (count service (get-all-recipients) :key #'p2p-recipient-service-type))

(defun display-recipient-list (frame pane)
  (let ((*standard-output* pane)
        (recipients (get-all-recipients)))
    (clim:formatting-table (pane :x-spacing 15)
      ;; Headers
      (clim:formatting-row (pane)
        (loop for header in '("User" "Host" "IP" "Active" "Offered")
              do (clim:formatting-cell (pane)
                   (clim:with-text-face (pane :bold)
                     (format pane "~a" header)))))
      
      ;; Rows
      (dolist (recipient recipients)
        (clim:formatting-row (pane)
          (clim:formatting-cell (pane)
            (let ((username (get-recipient-username recipient)))
              (format pane "~a~@[ (~a)~]"
                      (p2p-recipient-name recipient)
                      username)))
          (clim:formatting-cell (pane)
            (format pane "~a" (p2p-recipient-host recipient)))
          (clim:formatting-cell (pane)
            (format pane "~a" (p2p-recipient-ip recipient)))
          (clim:formatting-cell (pane)
            (format pane "~d" (length (p2p-recipient-active-transfers recipient))))
          (clim:formatting-cell (pane)
            (format pane "~d" (length (p2p-recipient-pending-offers recipient)))))))))

(defun display-recipient-details (frame pane)
  (let ((*standard-output* pane)
        (recipient (frame-selected-recipient frame)))
    (when recipient
      (clim:with-text-face (pane :bold)
        (format pane "Transfers to ~a~%" (p2p-recipient-name recipient))
        (terpri pane))
      
      ;; Overall progress bar
      (let ((overall-progress (calculate-overall-progress recipient)))
        (clim:formatting-table (pane :x-spacing 10)
          (clim:formatting-row (pane)
            (clim:formatting-cell (pane)
              (format pane "Overall Progress: ~3d%%" overall-progress))
            (clim:formatting-cell (pane)
              (let ((time-est (estimate-overall-time-remaining recipient)))
                (when time-est
                  (format pane "Time remaining: ~a" (format-time-remaining time-est)))))))
        (terpri pane)
        
        ;; Progress bar visual
        (clim:with-text-style (pane (clim:make-text-style :fix :roman :normal))
          (let ((bar-width 50)
                (filled (floor (* bar-width overall-progress) 100)))
            (format pane "[~a~a]~%"
                    (make-string filled :initial-element #\#)
                    (make-string (- bar-width filled) :initial-element #\Space)))))
      
      (terpri pane)
      
      ;; Details expander
      (let ((expanded (frame-details-expanded-p frame)))
        (clim:with-text-face (pane :bold)
          (format pane "~a Details~%" (if expanded "▼" "▶")))
        (when expanded
          (terpri pane)
          
          ;; Active Transfers section
          (when (p2p-recipient-active-transfers recipient)
            (clim:with-text-face (pane :bold)
              (format pane "Resources sending:~%"))
            (clim:formatting-table (pane :x-spacing 15)
              (clim:formatting-row (pane)
                (loop for header in '("Resource Title" "Kind" "Progress" "")
                      do (clim:formatting-cell (pane)
                           (clim:with-text-face (pane :bold)
                             (format pane "~a" header)))))
              (dolist (transfer (p2p-recipient-active-transfers recipient))
                (clim:formatting-row (pane)
                  (clim:formatting-cell (pane)
                    (format pane "~a" (active-transfer-resource-title transfer)))
                  (clim:formatting-cell (pane)
                    (format pane "~a" (active-transfer-resource-kind transfer)))
                  (clim:formatting-cell (pane)
                    (let ((pct (calculate-transfer-progress transfer)))
                      (format pane "[~a~a] ~3d%%"
                              (make-string (floor (* 30 pct) 100) :initial-element #\#)
                              (make-string (- 30 (floor (* 30 pct) 100)) :initial-element #\Space)
                              pct)))
                  (clim:formatting-cell (pane)
                    (clim:with-text-face (pane :bold)
                      (format pane "(X)"))))))
          
          (terpri pane)
          
          ;; Pending Offers section
          (when (p2p-recipient-pending-offers recipient)
            (clim:with-text-face (pane :bold)
              (format pane "Resources offered:~%"))
            (clim:formatting-table (pane :x-spacing 15)
              (clim:formatting-row (pane)
                (loop for header in '("Resource Title" "Kind" "")
                      do (clim:formatting-cell (pane)
                           (clim:with-text-face (pane :bold)
                             (format pane "~a" header)))))
              (dolist (offer (p2p-recipient-pending-offers recipient))
                (clim:formatting-row (pane)
                  (clim:formatting-cell (pane)
                    (format pane "~a" (pending-offer-resource-title offer)))
                  (clim:formatting-cell (pane)
                    (format pane "~a" (pending-offer-resource-kind offer)))
                  (clim:formatting-cell (pane)
                    (clim:with-text-face (pane :bold)
                      (format pane "(X)"))))))))))

(defun display-monitor-status (frame pane)
  (let ((*standard-output* pane)
        (recipients (get-all-recipients))
        (total-active (reduce #'+ recipients :key (lambda (r) (length (p2p-recipient-active-transfers r))))
        (total-offered (reduce #'+ recipients :key (lambda (r) (length (p2p-recipient-pending-offers r))))))
    (format pane "Recipients: ~d | Active transfers: ~d | Pending offers: ~d"
            (length recipients) total-active total-offered)))

;; =============================================================================
;; Commands
;; =============================================================================

(clim:define-command (com-refresh-sharing-monitor :command-table sharing-monitor-file-menu
                                                   :menu t :name t) ()
  (clim:redisplay-frame-panes clim:*application-frame* :force-p t))

(clim:define-command (com-toggle-auto-refresh :command-table sharing-monitor-file-menu
                                               :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (if (frame-auto-refresh-timer frame)
        (progn
          (ignore-errors (bt:destroy-thread (frame-auto-refresh-timer frame)))
          (setf (frame-auto-refresh-timer frame) nil))
        (setf (frame-auto-refresh-timer frame)
              (bt:make-thread
               (lambda ()
                 (loop
                   (sleep 5)
                   (when (and (typep frame 'sharing-monitor-frame)
                              (clim:frame-manager frame))
                     (clim:execute-frame-command
                      frame (list 'com-refresh-sharing-monitor)))))
               :name "Sharing Monitor Auto-Refresh")))))

(clim:define-command (com-close-sharing-monitor :command-table sharing-monitor-file-menu
                                                 :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (when (frame-auto-refresh-timer frame)
      (ignore-errors (bt:destroy-thread (frame-auto-refresh-timer frame))))
  (clim:frame-exit clim:*application-frame*))

(clim:define-command (com-toggle-recipient-details :command-table sharing-monitor-recipient-menu
                                                    :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (setf (frame-details-expanded-p frame) (not (frame-details-expanded-p frame)))
    (clim:redisplay-frame-panes frame :force-p t)))

(clim:define-command (com-cancel-all-transfers :command-table sharing-monitor-recipient-menu
                                                :menu t :name t) ()
  (let ((recipient (frame-selected-recipient clim:*application-frame*)))
    (when recipient
      (dolist (transfer (p2p-recipient-active-transfers recipient))
        (cancel-transfer (active-transfer-offer-id transfer)))
      (clim:redisplay-frame-panes clim:*application-frame* :force-p t))))

(clim:define-command (com-cancel-all-offers :command-table sharing-monitor-recipient-menu
                                             :menu t :name t) ()
  (let ((recipient (frame-selected-recipient clim:*application-frame*)))
    (when recipient
      (dolist (offer (p2p-recipient-pending-offers recipient))
        (cancel-offer (pending-offer-offer-id offer)))
      (clim:redisplay-frame-panes clim:*application-frame* :force-p t))))

(clim:define-command (com-copy-recipient-info :command-table sharing-monitor-recipient-menu
                                               :menu t :name t) ()
  (let ((recipient (frame-selected-recipient clim:*application-frame*)))
    (when recipient
      (clim:with-text-style (pane (clim:make-text-style :fix :roman :normal))
        (format t "~&Recipient: ~a~%Host: ~a~%IP: ~a~%"
                (p2p-recipient-name recipient)
                (p2p-recipient-host recipient)
                (p2p-recipient-ip recipient))))))

;; Transfer commands
(clim:define-command (com-cancel-selected-transfer :command-table sharing-monitor-transfer-menu
                                                    :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (recipient (frame-selected-recipient frame))
         (transfer (first (p2p-recipient-active-transfers recipient)))) ; Simplified
    (when transfer
      (cancel-transfer (active-transfer-offer-id transfer))
      (clim:redisplay-frame-panes frame :force-p t))))

(clim:define-command (com-retry-selected-transfer :command-table sharing-monitor-transfer-menu
                                                   :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (recipient (frame-selected-recipient frame))
         (transfer (first (p2p-recipient-active-transfers recipient))))
    (when transfer
      (retry-transfer (active-transfer-offer-id transfer))
      (clim:redisplay-frame-panes frame :force-p t))))

(clim:define-command (com-copy-transfer-id :command-table sharing-monitor-transfer-menu
                                            :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (recipient (frame-selected-recipient frame))
         (transfer (first (p2p-recipient-active-transfers recipient))))
    (when transfer
      (format t "~&Transfer ID: ~a~%" (active-transfer-offer-id transfer)))))

;; Offer commands
(clim:define-command (com-cancel-selected-offer :command-table sharing-monitor-offer-menu
                                                 :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (recipient (frame-selected-recipient frame))
         (offer (first (p2p-recipient-pending-offers recipient))))
    (when offer
      (cancel-offer (pending-offer-offer-id offer))
      (clim:redisplay-frame-panes frame :force-p t))))

(clim:define-command (com-resend-selected-offer :command-table sharing-monitor-offer-menu
                                                 :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (recipient (frame-selected-recipient frame))
         (offer (first (p2p-recipient-pending-offers recipient))))
    (when offer
      (resend-offer (pending-offer-offer-id offer))
      (clim:redisplay-frame-panes frame :force-p t))))

(clim:define-command (com-copy-offer-id :command-table sharing-monitor-offer-menu
                                         :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (recipient (frame-selected-recipient frame))
         (offer (first (p2p-recipient-pending-offers recipient))))
    (when offer
      (format t "~&Offer ID: ~a~%" (pending-offer-offer-id offer)))))

;; =============================================================================
;; Entry Points
;; =============================================================================

(defun open-sharing-monitor ()
  "Open the Sharing Monitor window."
  (setf *sharing-monitor-frame*
        (clim:make-application-frame 'sharing-monitor-frame))
  (clim:run-frame-top-level *sharing-monitor-frame*))

(clim:define-command (com-sharing-monitor :command-table clim-internals::global-command-table
                                           :menu t :name t) ()
  "Open the Sharing Monitor."
  (open-sharing-monitor))

;; =============================================================================
;; Helper Functions for Transfer Management
;; =============================================================================

(defun cancel-transfer (offer-id)
  "Cancel an active transfer by offer ID"
  (let ((transfer (gethash offer-id *active-transfers*)))
    (when transfer
      (journal:journaled (p2p-transfer-cancelled
                          :log-record *worker-journal*
                          :args (list :thread (list :id (thread-os-tid (current-thread))
                                                    :name (thread-name (current-thread)))
                                      :offer-id offer-id
                                      :status :cancelled)))
      (remhash offer-id *active-transfers*))))

(defun retry-transfer (offer-id)
  "Retry a failed transfer"
  (let ((transfer (gethash offer-id *active-transfers*)))
    (when transfer
      (setf (active-transfer-status transfer) :retrying)
      ;; Re-initiate the send
      (send-resource-to-peer (active-transfer-resource transfer)
                             (active-transfer-to-host transfer)
                             (active-transfer-to-port transfer)))))

(defun cancel-offer (offer-id)
  "Cancel a pending offer"
  (remhash offer-id *pending-offers*)

(defun resend-offer (offer-id)
  "Resend a pending offer"
  (let ((offer (gethash offer-id *pending-offers*)))
    (when offer
      (send-to (getf offer :resource) (getf offer :recipient)))))

(defun estimate-overall-time-remaining (recipient)
  "Estimate overall time remaining for all transfers to a recipient"
  (let ((transfers (p2p-recipient-active-transfers recipient)))
    (when transfers
      (let ((estimates (remove nil (mapcar #'estimate-time-remaining transfers))))
        (when estimates
          (apply #'max estimates))))))

;; =============================================================================
;; Integration with Inspector Menu
;; =============================================================================

(defun refresh-inspector-resource-offers-menu ()
  "Update the Resource Offers submenu in All Resources inspector"
  (let ((recipients (get-all-recipients)))
    (dolist (recipient recipients)
      (when (or (p2p-recipient-active-transfers recipient)
                (p2p-recipient-pending-offers recipient))
        (add-recipient-to-offers-menu recipient)))))

(defun add-recipient-to-offers-menu (recipient)
  "Add a recipient to the Resource Offers menu"
  (let ((menu-name (format nil "~a (~a)" (p2p-recipient-name recipient)
                           (p2p-recipient-host recipient))))
    (clim:add-menu-item-to-command-table
     'inspector-send-to-menu menu-name
     :command `(com-open-recipient-monitor ,(p2p-recipient-name recipient))
     :after :end)))

(clim:define-command (com-open-recipient-monitor :command-table clim-internals::global-command-table
                                                  :menu t :name t)
    ((recipient-name 'string :prompt "Recipient"))
  "Open sharing monitor for specific recipient"
  (setf (frame-selected-recipient *sharing-monitor-frame*)
        (find recipient-name (get-all-recipients)
              :key #'p2p-recipient-name :test #'string=))
  (when (frame-selected-recipient *sharing-monitor-frame*)
    (setf (frame-details-expanded-p *sharing-monitor-frame*) t)
    (clim:redisplay-frame-panes *sharing-monitor-frame* :force-p t)))