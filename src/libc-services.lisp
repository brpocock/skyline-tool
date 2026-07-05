(in-package :skyline-tool)

(cffi:defcstruct (servent
    :conc-name "servent-")
  (s-name :pointer)
  (s-aliases :pointer)
  (s-port :short)
  (s-protocol :pointer))

(cffi:defcfun ("getservbyname" %getservbyname) :pointer
  (name :string) (proto :string))

(cffi:defcfun ("getservbyport" %getservbyport) :pointer
  (port :int) (proto :string))

(cffi:defcfun ("setservent" %setservent) :void
  (stay-open :int))

(cffi:defcfun ("endservent" %endservent) :void)

(defun make-servent-accessor (ptr)
  "Convert a pointer to a servent struct into a plist for easy access."
  (when (cffi:null-pointer-p ptr)
    (return-from make-servent-accessor nil))
  (let ((name-ptr (cffi:foreign-slot-value ptr 'servent 's-name))
        (aliases-ptr (cffi:foreign-slot-value ptr 'servent 's-aliases))
        (port (cffi:foreign-slot-value ptr 'servent 's-port))
        (proto-ptr (cffi:foreign-slot-value ptr 'servent 's-protocol)))
    (list :name (when name-ptr (cffi:foreign-string-to-lisp name-ptr))
          :port (when (plusp port) port)
          :protocol (when proto-ptr (cffi:foreign-string-to-lisp proto-ptr))
          :aliases (when aliases-ptr
                      (loop for alias-ptr = aliases-ptr then (cffi:inc-pointer alias-ptr (cffi:foreign-type-size :pointer))
                            for alias = (cffi:mem-ref alias-ptr :pointer)
                            while (not (cffi:null-pointer-p alias))
                            collect (cffi:foreign-string-to-lisp alias))))))

(defun get-service-port (service-name protocol)
  "Get the port number for a given service by name using libc getservbyname.
   Returns the port number as an integer, or NIL if not found."
  (let ((ptr (%getservbyname service-name protocol)))
    (when ptr
      (let ((serv (make-servent-accessor ptr)))
        (getf serv :port)))))

(defun get-service-name (port protocol)
  "Get the service name for a given port and protocol using libc getservbyport.
   Returns the service name as a string, or NIL if not found."
  (let ((ptr (%getservbyport port protocol)))
    (when ptr
      (let ((serv (make-servent-accessor ptr)))
        (getf serv :name)))))

(defun find-service-port (service-name &optional (protocols '("tcp" "udp")))
  "Try to find the port for SERVICE-NAME by trying each PROTOCOL in turn.
   Returns the first valid port found, or NIL."
  (loop for proto in protocols
        for port = (get-service-port service-name proto)
        when port return port))

(defun query-services-by-port (target-port &optional (protocols '("tcp" "udp")))
  "Find all services that use TARGET-PORT (an integer) across given protocols.
   Returns a list of (service-name protocol) pairs."
  (loop for proto in protocols
        append (loop
                  (setservent 1)
                  (loop for ptr = (%getservbyname "" proto) then (%getservbyname "" proto)
                        while (and ptr (not (cffi:null-pointer-p ptr)))
                        do
                          (let ((serv (make-servent-accessor ptr)))
                            (when (and (getf serv :port) (eql (getf serv :port) target-port))
                              (return (list (getf serv :name) proto))))))))

(defun list-all-services (&optional (protocols '("tcp" "udp")))
  "Return a list of all known services from /etc/services database.
   Each entry is a plist: (:name :port :protocol :aliases)."
  (loop for proto in protocols
        append (loop
                  (setservent 1)
                  (loop for ptr = (%getservbyname "" proto) then (%getservbyname "" proto)
                        while (and ptr (not (cffi:null-pointer-p ptr)))
                        collect (make-servent-accessor ptr)))))

(export 'get-service-port
      'get-service-name
      'find-service-port
      'query-services-by-port
      'list-all-services)
