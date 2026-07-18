;;; Skyline-Tool src/p2p-dlna.lisp
;;; DLNA/UPnP media sharing infrastructure

(in-package :skyline-tool)

;; Global state for DLNA server
(defvar *dlna-server-process* nil
  "Process object for the running DLNA server.")
(defvar *dlna-server-port* 8200
  "Default port for DLNA server.")

;; DLNA Music Sharing using minidlna with OS-assigned port
(defun share-music-over-dlna (&key (directory #p"Source/Songs/")
                                   (friendly-name (format nil "Songs from ~a (~a@~a)"
                                                          *game-title*
                                                          (user-real-name)
                                                          (machine-instance))))
  "Share music over DLNA using minidlna."
  (let ((absolute-dir (uiop:ensure-absolute-pathname (namestring directory))))
    (unless (uiop:directory-exists-p absolute-dir)
      (error "Music directory does not exist: ~a" absolute-dir))
    
    ;; Check if minidlna is available via which command
    (unless (zerop (ignore-errors (uiop:run-program "which minidlnad"
                                                    :ignore-error-status t
                                                    :output nil)))
      (error "minidlna is not installed or not in PATH"))
    
    ;; Create a temporary configuration file for minidlna
    (let* ((conf-dir (uiop:getcwd))
           (conf-file (merge-pathnames
                       (make-pathname :name "minidlna-" :type ".conf")
                       (uiop:ensure-directory-pathname conf-dir)))
           (cmd (list "minidlnad" "-f" (namestring conf-file))))
      (with-open-file (stream conf-file
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (format stream "media_dir=~a~%" (namestring absolute-dir))
        (format stream "friendly_name=~a~%" friendly-name))
      
      ;; Start minidlna as a background process
      (setf *dlna-server-process*
            (uiop:launch-program cmd
                                 :output nil
                                 :error-output nil
                                 :ignore-error-status t))
      
      ;; Wait a moment for the process to start
      (sleep 2)
      
      ;; Check if the process is still running
      (let ((process *dlna-server-process*))
        (unless process
          (error "Failed to start minidlna server"))
        (unless (uiop:process-alive-p process)
          (error "minidlna process terminated unexpectedly")))
      
      ;; Use port 8200 as minidlna default
      (setf *dlna-server-port* 8200)
      
      ;; Register the service with Avahi for discovery
      (register-dlna-service-with-avahi absolute-dir friendly-name)
      t)))

(defun stop-music-over-dlna ()
  "Stop the DLNA music sharing server."
  (when *dlna-server-process*
    (uiop:terminate-process *dlna-server-process*)
    (setf *dlna-server-process* nil)))

(defun register-dlna-service-with-avahi (directory friendly-name)
  "Register DLNA service with Avahi for discovery."
  (when *avahi-entry-group*
    (let ((txt-record (format nil "path=~a name=~a"
                              (uiop:native-namestring directory)
                              friendly-name)))
      (avahi-entry-group-add-service *avahi-entry-group*
                                     0 +avahi-if-unspec+ +avahi-proto-unspec+
                                     (machine-instance)
                                     "_http._tcp"
                                     "" *dlna-server-port* 0
                                     txt-record)))
  (journal:journaled (dlna-service-registered
                      :args (list :thread (list :id (thread-os-tid (current-thread))
                                                :name (thread-name (current-thread)))
                                  :dlna (list :directory (namestring directory)
                                              :friendly-name friendly-name
                                              :port *dlna-server-port*
                                              :status :registered)))))
