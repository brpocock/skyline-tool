;;; src/version-control/gui/status-pane.lisp
;;; UI panes for version‑control and issue status in the All Resources window

(in-package :skyline-tool.version-control)

;; -----------------------------------------------------------------------------
;; VC status pane
;; -----------------------------------------------------------------------------

(defun draw-vc-pane (frame pane)
  "Draw a small status line for the current project repository.
   Shows branch name and a colour‑coded indicator of repository cleanliness.
   Uses the generic VC helpers to work with any backend (Git/SVN)."
  (declare (ignore frame))
  (let ((repo (uiop:getcwd))
        (msg "VC: No repository detected"))
    (handler-case
        (let ((backend (detect-vc-backend repo)))
          (if backend
              (let ((client (ecase backend
                              (:git (make-git-backend repo))
                              (:svn (make-svn-backend repo)))))
                (setf msg (format nil "VC: ~a | Branch: ~a | ~a"
                                  (vc-name client)
                                  (or (vc-branch client) "-")
                                  (if (vc-status client nil) "Dir" "Clean"))))))
      (error () (setf msg "VC: Error detecting status")))
    (clim:with-text-style (pane (clim:make-text-style :fix :roman :normal))
      (format pane "~a" msg))))

;; -----------------------------------------------------------------------------
;; Issues pane – shows configured tracker counts
;; -----------------------------------------------------------------------------

(defun draw-issues-pane (frame pane)
  "Draw a simple one‑line summary of issues status from configured trackers."
  (declare (ignore frame))
  (let ((msg "Issues: 0 (configure in Preferences)"))
    (handler-case
        (let ((trackers (list-configured-trackers)))
          (when trackers
            (setf msg (format nil "Issues: ~{~a~}" 
                              (mapcar #'format-tracker-info trackers)))))
      (error () (setf msg "Issues: Error")))
    (clim:with-text-style (pane (clim:make-text-style :fix :roman :normal))
      (format pane "~a" msg))))

(defun list-configured-trackers ()
  "Return list of configured issue tracker clients."
  ;; Stub – to be implemented with actual tracker configuration
  nil)

(defun format-tracker-info (tracker)
  "Format tracker information for display."
  (format nil "~a:~d " (tracker-name tracker)
          (tracker-open-count tracker)))

(defgeneric tracker-name (tracker)
  (:method (tracker) (if (slot-boundp tracker 'name) 
                         (slot-value tracker 'name) 
                       "-")))

(defgeneric tracker-open-count (tracker)
  (:method (tracker) (if (slot-boundp tracker 'open-count)
                         (slot-value tracker 'open-count)
                       0)))

(export '(draw-vc-pane draw-issues-pane))
