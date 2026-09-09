;;; Skyline-Tool src/p2p-avahi.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

;;; CFFI bindings for Avahi mDNS/DNS-SD service discovery

(in-package :skyline-tool)

;; Load Avahi library via CFFI (CFFI declared as system dependency)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library libavahi-common
    (:unix (:or "libavahi-common.so.0" "libavahi-common.so"))
    (t (:default "libavahi-common")))
  (cffi:define-foreign-library libavahi-client
    (:unix (:or "libavahi-client.so.0" "libavahi-client.so"))
    (t (:default "libavahi-client"))))

;; Avahi simple poll
(cffi:defcfun ("avahi_simple_poll_new" avahi-simple-poll-new) :pointer)
(cffi:defcfun ("avahi_simple_poll_free" avahi-simple-poll-free) :void
  (poll :pointer))
(cffi:defcfun ("avahi_simple_poll_get" avahi-simple-poll-get) :pointer
  (poll :pointer))
(cffi:defcfun ("avahi_simple_poll_loop" avahi-simple-poll-loop) :int
  (poll :pointer))
(cffi:defcfun ("avahi_simple_poll_quit" avahi-simple-poll-quit) :void
  (poll :pointer))

;; Avahi client
(cffi:defcfun ("avahi_client_new" avahi-client-new) :pointer
  (poll-api :pointer) (flags :int) (user-data :pointer) (callback :pointer))

(cffi:defcfun ("avahi_client_free" avahi-client-free) :void
  (client :pointer))

;; Avahi service browser
(cffi:defcfun ("avahi_service_browser_new" avahi-service-browser-new) :pointer
  (client :pointer) (flags :int) (interface :int32) (protocol :int32)
  (domain :string) (type :string) (callback :pointer) (user-data :pointer))

(cffi:defcfun ("avahi_service_browser_free" avahi-service-browser-free) :void
  (browser :pointer))

;; Avahi service resolver
(cffi:defcfun ("avahi_service_resolver_new" avahi-service-resolver-new) :pointer
  (client :pointer) (flags :int) (interface :int32) (protocol :int32)
  (name :string) (type :string) (domain :string) (callback :pointer) (user-data :pointer))

(cffi:defcfun ("avahi_service_resolver_free" avahi-service-resolver-free) :void
  (resolver :pointer))

;; Avahi string list
(cffi:defcfun ("avahi_string_list_get_next" avahi-string-list-get-next) :pointer
  (list :pointer))
(cffi:defcfun ("avahi_string_list_get_text" avahi-string-list-get-text) :string
  (list :pointer))
(cffi:defcfun ("avahi_string_list_add" avahi-string-list-add) :pointer
  (list :pointer) (text :string))
(cffi:defcfun ("avahi_string_list_free" avahi-string-list-free) :void
  (list :pointer))

;; Entry group API for publishing services
(cffi:defcfun ("avahi_entry_group_new" avahi-entry-group-new) :pointer
  (client :pointer) (callback :pointer) (user-data :pointer))

(cffi:defcfun ("avahi_entry_group_free" avahi-entry-group-free) :void
  (group :pointer))

(cffi:defcfun ("avahi_entry_group_reset" avahi-entry-group-reset) :int
  (group :pointer))

(cffi:defcfun ("avahi_entry_group_add_service" avahi-entry-group-add-service) :int
  (group :pointer) (flags :int) (interface :int32) (protocol :int32)
  (name :string) (type :string) (domain :string)
  (host :string) (port :uint16)
  (txt :pointer))

(cffi:defcfun ("avahi_entry_group_commit" avahi-entry-group-commit) :int
  (group :pointer))

(cffi:defcfun ("avahi_entry_group_is_empty" avahi-entry-group-is-empty) :int
  (group :pointer))

;; Constants
(defconstant +avahi-client-no-fail+ 1)
(defconstant +avahi-if-unspec+ -1)
(defconstant +avahi-proto-unspec+ -1)
(defconstant +avahi-lookup-use-wide-area+ 2)
(defconstant +avahi-protocol-ipv4+ 0)
(defconstant +avahi-protocol-ipv6+ 1)

(defconstant +avahi-resolver-found+ 1)
(defconstant +avahi-resolver-new+ 1)

;; State variables
(defvar *avahi-poll* nil "Avahi simple poll object.")
(defvar *avahi-client* nil "Active Avahi client connection.")
(defvar *avahi-entry-group* nil "Active entry group for publishing.")
(defvar *avahi-browsers* nil "Active service browsers and resolvers.")
(defvar *discovered-services* (make-hash-table :test 'equal) "Hash of discovered services by type.")
(defvar *avahi-thread* nil "Background thread running Avahi poll loop.")

;; Client callback
(cffi:defcallback avahi-client-callback :void
    ((client :pointer) (state :int) (user-data :pointer))
  (journal:journaled (avahi-client-state
                      :log-record *worker-journal*
                      :args (list :thread (list :id (thread-os-tid (current-thread))
                                                :name (thread-name (current-thread)))
                                   :avahi (list :client client
                                                :state state
                                                :user-data user-data)))))

;; Service browser callback
(cffi:defcallback avahi-service-browser-callback :void
    ((browser :pointer) (interface :int32) (protocol :int32)
                        (event :int) (name :string) (type :string) (domain :string) (user-data :pointer))
  (journal:journaled
      (avahi-service-browse
       :log-record *worker-journal*
       :args (list :thread
                   (list :id (thread-os-tid (current-thread))
                         :name (thread-name (current-thread)))
                   :avahi (list :browser browser
                                :interface interface
                                :protocol protocol
                                :event event
                                :name name
                                :type type
                                :domain domain
                                :user-data user-data))))
  (case event
    ((#.+avahi-resolver-new+)
     (let ((resolver (avahi-service-resolver-new *avahi-client*
                                                 0 interface protocol
                                                 name type domain
                                                 (cffi:callback avahi-service-resolver-callback)
                                                 (cffi:null-pointer))))
       (push resolver *avahi-browsers*)))))

;; Service resolver callback
(cffi:defcallback avahi-service-resolver-callback :void
    ((resolver :pointer) (interface :int32) (protocol :int32)
                         (event :int) (name :string) (type :string) (domain :string)
                         (host :string) (aprotocol :int32) (address :pointer) (port :uint16)
                         (txt :pointer) (flags :int) (user-data :pointer))
  (journal:journaled
      (avahi-service-resolved
       :log-record *worker-journal*
       :args (list :thread (list :id (thread-os-tid (current-thread))
                                 :name (thread-name (current-thread)))
                   :avahi (list :resolver resolver
                                :address address
                                :flags flags
                                :interface interface
                                :protocol protocol
                                :event event
                                :name name
                                :type type
                                :domain domain
                                :host host
                                :aprotocol aprotocol
                                :port port
                                :txt txt
                                :resolver resolver
                                :user-data user-data))))
  (case event
    (#.+avahi-resolver-found+
     (let ((txt-plist nil))
       (when (cffi:pointerp txt)
         (loop for txt-ptr = txt then (avahi-string-list-get-next txt-ptr)
               while (cffi:pointerp txt-ptr)
               do (when-let (text (the string (avahi-string-list-get-text txt-ptr)))
                    (when (find #\= text)
                      (let ((kv (split-sequence:split-sequence #\= text)))
                        (push (cons (intern (string-upcase (first kv)) :keyword)
                                    (second kv))
                              txt-plist))))))
       (let ((service-info (list :name name
                                 :host host
                                 :port port
                                 :type type
                                 :domain domain
                                 :txt txt-plist)))
         (push service-info (gethash type *discovered-services*)))))))

;; Entry group state callback
(cffi:defcallback avahi-entry-group-callback :void
    ((group :pointer) (state :int) (user-data :pointer))
  (let ((state-name (ecase state
                      (0 :uncommitted)
                      (1 :registering)
                      (2 :established)
                      (3 :collision)
                      (4 :failure))))
    (journal:journaled (avahi-entry-group-state
                        :log-record *worker-journal*
                        :args (list :thread (list :id (thread-os-tid (current-thread))
                                                  :name (thread-name (current-thread)))
                                    :avahi (list :group group
                                                 :state state
                                                 :state-name state-name
                                                 :user-data user-data))))
    (when (or (eql state-name :established) (eql state-name :failure))
      (journal:journaled (avahi-entry-group-error
                          :log-record *worker-journal*
                          :args (list :thread (list :id (thread-os-tid (current-thread))
                                                    :name (thread-name (current-thread)))
                                      :avahi (list :group group
                                                   :state state
                                                   :state-name state-name
                                                   :user-data user-data)))))))

;; Publish our own Skyline-Tool service
(defun publish-skyline-tool-service (&key (domain "local."))
  "Publish Skyline-Tool resource discovery service via Avahi entry group."
  (unless *avahi-client*
    (error "Avahi client not initialized"))
  
  ;; Reset or create entry group
  (unless *avahi-entry-group*
    (setf *avahi-entry-group* (avahi-entry-group-new *avahi-client*
                                                     (cffi:callback avahi-entry-group-callback)
                                                     (cffi:null-pointer))))
  
  ;; Reset the entry group
  (avahi-entry-group-reset *avahi-entry-group*)
  
  ;; Build TXT record string list
  (let ((txt-record (format nil "game=~a/machine=~d/user=~a"
                            *game-title*
                            *machine*
                            (user-real-name)))
        (txt-list nil))
    ;; Build string list for TXT record (key=value pairs)
    (loop for part in (split-sequence:split-sequence #\Space txt-record)
          when (search "=" part)
            do (let ((kv (split-sequence:split-sequence #\= part)))
                 (when (>= (length kv) 2)
                   (push (format nil "~a=~a" (first kv) (second kv)) txt-list))))
    
    ;; Add service to entry group
    (let ((result (avahi-entry-group-add-service *avahi-entry-group*
                                                 0 +avahi-if-unspec+ +avahi-proto-unspec+
                                                 (machine-instance)
                                                 "_skyline-tool-resource._tcp"
                                                 domain
                                                 "" ; default host
                                                 0 ; automatic port
                                                 (if txt-list
                                                     (let ((head (car txt-list)))
                                                       (let ((list head))
                                                         (dolist (item (cdr txt-list))
                                                           (setf (cdr list) (avahi-string-list-add head item))
                                                           (setf list (cdr list)))
                                                         head))
                                                     (cffi:null-pointer)))))
      (when (< result 0)
        (journal:journaled (avahi-service-publish-failure
                            :log-record *worker-journal*
                            :args (list :thread (list :id (thread-os-tid (current-thread))
                                                      :name (thread-name (current-thread)))
                                        :avahi (list :result result :domain domain))))
        (return-from publish-skyline-tool-service))
      
      ;; Commit changes
      (let ((commit-result (avahi-entry-group-commit *avahi-entry-group*)))
        (when (< commit-result 0)
          (journal:journaled (avahi-service-publish-failure
                              :log-record *worker-journal*
                              :args (list :thread (list :id (thread-os-tid (current-thread))
                                                        :name (thread-name (current-thread)))
                                          :avahi (list :commit-result commit-result
                                                       :domain domain)))))))))

;; Public API functions

(defun discover-skyline-tool-instances ()
  "Discover Skyline-Tool instances via native Avahi.
   Returns a list of plists with :name, :host, :port."
  (let ((results nil))
    (maphash (lambda (name info)
               (declare (ignore name))
               (push info results))
             (gethash "_skyline-tool-service._tcp" *discovered-services*))
    (nreverse results)))

(defun discover-ipp-printers ()
  "Discover CUPS/IPP printers via native Avahi.
   Returns a list of plists with :name."
  (let ((results nil))
    (maphash (lambda (name info)
               (declare (ignore name))
               (push (list :name (getf info :name)) results))
             (gethash "_ipp._tcp" *discovered-services*))
    (nreverse results)))

(defun discover-sftp-servers ()
  "Discover SFTP/SSH servers via native Avahi.
   Returns a list of plists with :name, :host, :port."
  (let ((results nil))
    (maphash (lambda (name info)
               (declare (ignore name))
               (push info results))
             (gethash "_ssh._tcp" *discovered-services*))
    (nreverse results)))

(defun discover-skyline-offers ()
  "Discover pending resource offers via native Avahi.
   Returns a list of plists with :offer-id and other offer details."
  (let ((results nil))
    (maphash (lambda (name info)
               (declare (ignore name))
               (push info results))
             (gethash "_skyline-offer._tcp" *discovered-services*))
    (nreverse results)))

;; Resource Offering (All parameters logged)
(defun offer-resource (resource-type resource-path &key destination)
  "Offer a resource to another Skyline-Tool instance."
  (journal:journaled (avahi-resource-offer
                      :log-record *worker-journal*
                      :args (list :thread (list :id (thread-os-tid (current-thread))
                                                :name (thread-name (current-thread)))
                                   :avahi (list :resource-type resource-type
                                                :resource-path resource-path
                                                :destination destination
                                                :status :offered)))))

;; Resource Accepting (All parameters logged)
(defun accept-resource-offer (offer-id offer-path)
  "Accept an offered resource."
  (journal:journaled (avahi-resource-accept
                      :log-record *worker-journal*
                      :args (list :thread (list :id (thread-os-tid (current-thread))
                                                :name (thread-name (current-thread)))
                                   :avahi (list :offer-id offer-id
                                                :offer-path offer-path
                                                :status :accepted)))))

;; Start native Avahi browsers
(defun start-native-avahi-browsers ()
  "Initialize Avahi client and start service browsers for relevant types."
  (when *avahi-poll*
    (return-from start-native-avahi-browsers))
  
  (setf *avahi-poll* (avahi-simple-poll-new))
  (setf *avahi-client* (avahi-client-new (avahi-simple-poll-get *avahi-poll*)
                                         +avahi-client-no-fail+
                                         (cffi:null-pointer)
                                         (cffi:callback avahi-client-callback)))
  
  ;; Browse for service types we care about
  (dolist (type '("_ipp._tcp" "_skyline-tool-resource._tcp"
                  "_ssh._tcp" "_skyline-tool-service._tcp"))
    (let ((browser (avahi-service-browser-new *avahi-client*
                                              0 +avahi-if-unspec+ +avahi-proto-unspec+
                                              "" type
                                              (cffi:callback avahi-service-browser-callback)
                                              (cffi:null-pointer))))
      (push browser *avahi-browsers*)))
  
  ;; Start poll loop in background thread
  (setf *avahi-thread* (bt:make-thread
                        (lambda ()
                          (avahi-simple-poll-loop *avahi-poll*))
                        :name "Avahi Poll Loop"))
  (journal:journaled (avahi-browsers-started
                      :log-record *worker-journal*
                      :args (list :thread (list :id (thread-os-tid (current-thread))
                                                :name (thread-name (current-thread)))
                                   :avahi (list :browsers *avahi-browsers*)))))

(defun stop-native-avahi-browsers ()
  "Stop Avahi browsers and clean up."
  (when-let (poll *avahi-poll*)
    (setf *avahi-poll* nil)
    (avahi-simple-poll-quit poll))
  (when-let (th *avahi-thread*)
    (setf *avahi-thread* nil)
    (bt:join-thread th))
  (dolist (browser *avahi-browsers*)
    (avahi-service-browser-free browser))
  (setf *avahi-browsers* nil)
  (when-let (client *avahi-client*)
    (setf *avahi-client* nil)
    (avahi-client-free client))
  (when-let (poll *avahi-poll*)
    (setf *avahi-poll* nil)
    (avahi-simple-poll-free poll))
  (journal:journaled (avahi-browsers-stopped
                      :log-record *worker-journal*
                      :args (list :thread (list :id (thread-os-tid (current-thread))
                                                :name (thread-name (current-thread)))
                                  :avahi (list :status :stopped)))))
