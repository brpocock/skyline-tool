;;; src/version-control/fsmonitor.lisp
;;; Filesystem monitoring for version control status updates

(in-package :skyline-tool.version-control)

(defclass inotify-monitor ()
  ((path :initarg :path :reader monitor-path)
   (callback :initarg :callback :reader monitor-callback)
   (backend :initarg :backend :reader monitor-backend)
   (watch-descriptor :initform nil :accessor watch-descriptor)
   (active-p :initform nil :accessor monitor-active-p)
   (thread :initform nil :accessor monitor-thread)))

(defun start-fs-monitor (backend path callback)
  "Start monitoring PATH for changes, calling CALLBACK on events.
Returns a monitor object that can be stopped with STOP-FS-MONITOR."
  (let ((monitor (make-instance 'inotify-monitor
                                :path path
                                :callback callback
                                :backend backend)))
    (setf (watch-descriptor monitor)
          (inotify:add-watch path
                             :mask (logior inotify:+in-modify+
                                           inotify:+in-create+
                                           inotify:+in-delete+
                                           inotify:+in-moved-to+
                                           inotify:+in-moved-from+)))
    (setf (monitor-active-p monitor) t)
    (setf (monitor-thread monitor)
          (bt:make-thread
           (lambda ()
             (loop while (monitor-active-p monitor)
                   do (let ((events (inotify:read-events (watch-descriptor monitor))))
                        (dolist (event events)
                          (funcall callback (event-mask event) (event-name event))))))
           :name "fs-monitor-thread"))
    monitor))

(defmethod stop-fs-monitor ((monitor inotify-monitor))
  "Stop filesystem monitoring and clean up resources."
  (setf (monitor-active-p monitor) nil)
  (when (watch-descriptor monitor)
    (inotify:rm-watch (watch-descriptor monitor)))
  (when (monitor-thread monitor)
    (bt:join-thread (monitor-thread monitor))))

(defmacro with-fs-monitor ((monitor-var backend path callback) &body body)
  "Execute BODY with filesystem monitoring active."
  `(let ((,monitor-var (start-fs-monitor ,backend ,path ,callback)))
     (unwind-protect (progn ,@body)
       (when ,monitor-var (stop-fs-monitor ,monitor-var)))))