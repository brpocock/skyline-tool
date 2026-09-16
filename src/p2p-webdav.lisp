;;; Skyline-Tool src/p2p-webdav.lisp
;;; WebDAV sharing infrastructure

(in-package :skyline-tool)

;; Global state for WebDAV server
(defvar *webdav-acceptor* nil
  "Hunchentoot acceptor for WebDAV server.")

(defun share-dist-over-webdav (&key (dist-directory "Dist/"))
  "Share Dist/ directory over WebDAV. Port assigned by OS."
  (let ((absolute-dir (uiop:ensure-absolute-pathname (namestring dist-directory))))
    (unless (uiop:directory-exists-p absolute-dir)
      (ensure-directories-exist absolute-dir))
    
    (when (and (null *webdav-acceptor*)
               (or (not *webdav-acceptor*)
                   (not (hunchentoot:started-p *webdav-acceptor*))))
      (setf *webdav-acceptor*
            (make-instance 'hunchentoot:taskmaster-acceptor
                           :port 0
                           :access-log-destination nil
                           :message-log-destination nil))
      
      (let ((started (hunchentoot:start *webdav-acceptor*)))
        (when (null started)
          (error "Failed to start WebDAV server"))
        
        (let ((actual-port (hunchentoot:local-port *webdav-acceptor*)))
          (unless actual-port
            (error "Could not determine assigned port"))
          
          ;; WebDAV handler
          (hunchentoot:define-easy-handler (webdav-handler :uri "/") ()
            (let* ((root-dir absolute-dir)
                   (path (hunchentoot:script-name hunchentoot:*request*))
                   (full-path (merge-pathnames
                               (uiop:ensure-absolute-pathname
                                (namestring path))
                               root-dir)))
              (cond
                ((not (uiop:file-exists-p full-path))
                 (error 'hunchentoot:+http-not-found+))
                ((uiop:directory-pathname-p full-path)
                 (list-directory-contents full-path hunchentoot:*request*))
                (t
                 (serve-file full-path hunchentoot:*request*)))))
          
          ;; Register with Avahi
          (register-webdav-service-with-avahi absolute-dir actual-port))))
    
    (when *webdav-acceptor*
      t)))

(defun register-webdav-service-with-avahi (directory port)
  "Register WebDAV service with Avahi."
  (when *avahi-entry-group*
    (let ((txt-record (format nil "path=~a port=~d"
                              (uiop:native-namestring directory)
                              port)))
      (avahi-entry-group-add-service *avahi-entry-group*
                                     0 +avahi-if-unspec+ +avahi-proto-unspec+
                                     (machine-instance)
                                     "_webdav._tcp"
                                     "" "" port
                                     txt-record))))

(defun stop-dist-over-webdav ()
  "Stop the WebDAV sharing server."
  (when *webdav-acceptor*
    (hunchentoot:stop *webdav-acceptor*)
    (setf *webdav-acceptor* nil)))

(defun list-directory-contents (dir request)
  "List directory contents for WebDAV."
  (declare (ignore request))
  (let ((entries (uiop:directory-files dir)))
    (with-output-to-string (stream)
      (format stream "<!DOCTYPE html>
<html>
<head>
  <title>Index of ~a</title>
</head>
<body>
  <h1>Index of ~a</h1>
  <ul>"
              (pathname-name dir)
              (namestring dir))
      (dolist (entry entries)
        (let ((name (file-namestring entry))
              (is-dir (uiop:directory-pathname-p entry)))
          (format stream "<li><a href=\"~a\">~a~a</a></li>"
                  name
                  (pathname-name entry)
                  (if is-dir "/" ""))))
      (format stream "</ul></body></html>"))))

(defun guess-content-type (file)
  "Guess content type from file extension."
  (string-case (pathname-type file)
    ("bin" "application/octet-stream")
    (otherwise (error "unidentified file type"))))

(defun serve-file (file request)
  "Serve a file for WebDAV download."
  (declare (ignore request))
  (let ((content-type (guess-content-type file))
        (buf-size (file-length file)))
    (setf (hunchentoot:header-out "Content-Type") content-type
          (hunchentoot:header-out "Content-Length") buf-size)
    (with-open-file (stream file
                            :direction :input
                            :element-type '(unsigned-byte 8))
      (let ((buffer (make-array buf-size :element-type '(unsigned-byte 8))))
        (read-sequence buffer stream)
        buffer))))
