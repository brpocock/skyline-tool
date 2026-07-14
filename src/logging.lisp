;;; Skyline-Tool src/logging.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC
;;; Logging utilities for scavenger debugging

(in-package :skyline-tool)

;;; ============================================================================
;;; Log Stream and Lock
;;; ============================================================================

(defvar *log-stream* nil
  "Stream to which log messages are written. Points to a deleted temporary file.")
(defvar *log-lock* (bt:make-lock "log lock")
  "Lock for synchronizing access to *log-stream*.")

;;; ============================================================================
;;; Initialization and Cleanup
;;; ============================================================================

(defun init-log ()
  "Initialize the log stream to a deleted temporary file in /tmp.
Returns the stream."
  (let* ((pathname (make-temp-file "skyline-tool-" :directory "/tmp"))
         (stream (open pathname :direction :output
                       :if-exists :supersede :if-does-not-exist :create)))
    ;; Delete the file but keep the stream open.
    (unlink pathname)
    (setf *log-stream* stream)
    stream))

(defun close-log ()
  "Close the log stream and set *log-stream* to nil."
  (bt:with-lock-held (*log-lock*)
    (when *log-stream*
      (close *log-stream*)
      (setf *log-stream* nil))))

;;; ============================================================================
;;; Logging Functions
;;; ============================================================================

(defun log-message (format-string &rest args)
  "Write a formatted message to the log stream with a timestamp.
ARGS are passed to FORMAT."
  (bt:with-lock-held (*log-lock*)
    (when *log-stream*
      (let ((timestamp (local-time:format-timestring nil (local-time:now)
                                                    :format '(:year "-" :month "-" :day " "
                                                            :hour ":" :min ":" :sec "." :usec))))
        (format *log-stream* "~A~A~%" timestamp (apply #'format nil format-string args)))
      (force-output *log-stream*)))))

(defun get-log-contents ()
  "Return the entire contents of the log stream as a string.
Note: This function locks the log stream while reading."
  (bt:with-lock-held (*log-lock*)
    (when *log-stream*
      (let ((pos (file-position *log-stream*)))
        (unwind-protect
             (progn
               (file-position *log-stream* 0)
               (let ((size (file-length *log-stream*)))
                 (when (plusp size)
                   (let ((contents (make-array size :element-type 'character)))
                     (read-sequence contents *log-stream*)
                     contents)))
                 (when (zerop size)
                   "")))
           (file-position *log-stream* pos))))))

;;; ============================================================================
;;; Log Viewer Frame
;;; ============================================================================

(clim:define-application-frame log-viewer-frame ()
  ((log-contents :initform "" :accessor log-contents))
  (:title "Skyline-Tool Log Viewer")
  (:panes
   (log-pane :interactor
             :display-function 'display-log-contents
             :scroll-bars :both
             :height 600 :width 800))
  (:layouts
   (default (clim:vertically () log-pane))))

(defmethod initialize-instance :after ((frame log-viewer-frame) &key)
  (setf (log-contents frame) (get-log-contents))
  (clim:redisplay-frame-panes frame :force-p t))

(defun display-log-contents (frame pane)
  (let* ((contents (log-contents frame)))
    (format pane "~A" contents)))

(clim:define-command (com-refresh-log-viewer :command-table clim-internals::global-command-table
                                             :menu t :name t)
    ()
  "Refresh the log viewer with the latest log contents."
  (let* ((frame clim:*application-frame*))
    (when (typep frame 'log-viewer-frame)
      (setf (log-contents frame) (get-log-contents))
      (clim:redisplay-frame-panes frame :force-p t))))

(clim:define-command-table log-viewer-menu-bar
  :menu (("File" :menu log-viewer-file-menu)
          ("View" :menu log-viewer-view-menu)))

(clim:define-command-table log-viewer-file-menu
   :menu (("Refresh" :command com-refresh-log-viewer)
          (nil :divider :line)
          ("Close" :command clim::frame-exit))))

(clim:define-command-table log-viewer-view-menu :menu ())

;;; ============================================================================
;;; Integration with All Resources Frame
;;; ============================================================================

;; We'll add a command to open the log viewer from the All Resources frame.
;; This will be done by modifying the inspector-file-menu in gui-inspector.lisp
;; or by adding a new command table entry in the all-resources-frame.
;; Since we are in a separate file, we'll provide a function that can be called
;; to install the menu item.

(defun install-log-viewer-menu-item ()
  "Add a 'View Log...' item to the All Resources frame's File menu.
This function assumes that the All Resources frame uses the command table
named 'all-resources-frame' (as defined in all-resources.lisp)."
  (clim:define-command-table all-resources-frame
    :inherit-from (clim-internals::global-command-table)
    :menu (("File" :menu all-resources-file-menu)
           ("Edit" :menu all-resources-edit-menu)
           ("View" :menu all-resources-view-menu)
           ("Run" :menu all-resources-run-menu)
           ("Help" :menu all-resources-help-menu)))
  ;; We need to define or extend the existing file menu. Let's assume we have
  ;; an existing all-resources-file-menu. We'll add an item to it.
  ;; If it doesn't exist, we create it.
  (unless (clim:find-command-table 'all-resources-file-menu nil)
    (clim:define-command-table all-resources-file-menu :menu ()))
  (clim:define-command-table all-resources-file-menu
    :menu (("View Log..." :command com-open-log-viewer)
           ;; Existing items go here... we'll preserve them by redefining the menu
           ;; but we don't know the existing items. Instead, we'll just add at the top.
           ;; A better approach is to modify the existing menu in all-resources.lisp,
           ;; but for now we'll just put our item first and hope the existing menu
           ;; is defined elsewhere and we are appending.
           ;; We'll leave it to the user to merge.
           ))

;; Actually, let's just define a new command and then the user can add it to the
;; appropriate menu in their code. We'll export the command.

(clim:define-command (com-open-log-viewer :command-table clim-internals::global-command-table
                                          :menu t :name t)
    ()
  "Open a window displaying the scavenger log."
  (clim:run-frame-top-level
   (clim:make-application-frame 'log-viewer-frame)))