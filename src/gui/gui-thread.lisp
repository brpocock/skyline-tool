;;; Skyline-Tool src/gui/gui-thread.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC
;;;
;;; Shared thread management for all GUI windows.
;;; Provides thread creation, file watcher management, and window lifecycle helpers.

(in-package :skyline-tool)

(defun make-window-thread (name function)
  "Create a named thread for running a window top-level.
NAME is a string for the thread name.
FUNCTION is a thunk that runs the window.
Preserves +basic-dynamics-list+ bindings in the new thread."
  (let* ((vars +basic-dynamics-list+)
         (vals (mapcar (lambda (sym)
                         (if (boundp sym) (symbol-value sym) nil))
                       vars)))
    (make-thread (lambda ()
                   (progv vars vals
                     (funcall function)))
                 :name name)))

(defun start-file-watcher (frame)
  "Start a background thread that monitors the resource's file for changes
and updates the inspector when the modification time changes.
Returns nil when FRAME has no game-resource or no watchable paths."
  (let ((resource (ignore-errors (slot-value frame 'frame-resource))))
    (unless (typep resource 'game-resource)
      (return-from start-file-watcher nil))
    (let ((paths (remove-if-not #'probe-file
                                (remove-if-not #'identity
                                               (game-resource-pathnames resource)))))
      (unless paths
        (return-from start-file-watcher nil))
      (submit-task
       (lambda ()
         (inotify:with-inotify (inot (mapcar (lambda (pathname)
                                               (list pathname inotify:in-all-events))
                                             paths))
           (loop for ev = (inotify:read-events inot)
                 do (ignore-errors
                     (clim:redisplay-frame-panes frame)))))))))

(defun stop-file-watcher (frame)
  "Stop the file watcher thread if running."
  (let ((thread (ignore-errors (frame-watcher-thread frame))))
    (when (and thread (thread-alive-p thread))
      (destroy-thread thread))))

(defun setup-frame-eventbus-subscriptions (frame event-types handler)
  "Subscribe FRAME to EVENT-TYPES on the global eventbus.
EVENT-TYPES is a list of keywords.
HANDLER is a function of one argument (the event payload).
Returns a list of subscription tokens for later cleanup.

All subscribers are wrapped in an ignore-errors and redisplay-frame-panes."
  (let ((wrapped (lambda (event)
                   (declare (ignore event))
                   (ignore-errors
                    (clim:redisplay-frame-panes frame :force-p t)))))
    (dolist (event-type event-types)
      (subscribe event-type wrapped))
    wrapped))

(defun teardown-frame-eventbus-subscriptions (event-types subscriber)
  "Remove SUBSCRIBER from all EVENT-TYPES."
  (dolist (event-type event-types)
    (unsubscribe event-type subscriber)))

(defun %frame-thread-name (frame base-name)
  "Generate a consistent thread name for FRAME.
Includes the window pretty-name and resource title when available."
  (let* ((pretty (ignore-errors (clim:frame-pretty-name frame)))
         (resource (ignore-errors (inspector-resource frame)))
         (title (when resource (ignore-errors (game-resource-title resource)))))
    (format nil "~a~@[: ~a~] (~a)" base-name title (or pretty "Window"))))
