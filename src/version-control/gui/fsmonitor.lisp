;;; src/version-control/gui/fsmonitor.lisp
;;; Inotify-based filesystem monitoring

(in-package :skyline-tool.version-control)

;; Inotify monitoring class
(defclass inotify-monitor (fs-monitor)
  ())

;; Initialize inotify monitoring
(defmethod start-fs-monitor ((backend git-backend) path callback)
  "Start inotify monitoring for VERSION-CONTROL backend"
  (let ((monitor (make-instance 'inotify-monitor :path path :callback callback)))
    (setf (monitor-thread monitor) (inotify:add-watch monitor path
                                                      (or (:watch-mask backend)
                                                          #x1000)))
    (setf (monitor-active-p monitor) t)
    (inotify:add-watch callback monitor-path monitor-uid)
    monitor))

;; Inotify watch callback
(defmethod inotify-monitor-callback ((monitor inotify-monitor) event-files)
  "Handle inotify events and invoke callback"
  (funcall (monitor-callback monitor) :changed event-files))

;; Fallback polling
(defun fallback-poll-loop (monitor)
  "Polling loop for systems without inotify"
  (loop while (monitor-active-p monitor)
        do (sleep 2)
        (check-for-changes monitor)))

;; Check for changes
(defun check-for-changes (monitor)
  "Check for changes and invoke callback"
  (let ((changes (find-changed-files (inotify-monitor-path monitor))))
    (when changes
      (funcall (inotify-monitor-callback monitor) :changed changes))))

;; Find changed files (inotify-aware)
(defun find-changed-files (path)
  "Return changed files tracked by inotify"
  (inotify:read-events path))
