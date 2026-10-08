;;; src/version-control/config.lisp
;;; Unified configuration interface for Skyline-Tool version control

(in-package :skyline-tool.version-control)


;;-----------------------------------------------------------------------------
;; Configuration Management
;;-----------------------------------------------------------------------------

(defclass version-control-config ()
  ((last-commit :initform nil :accessor config-last-commit)
   (staged-files :initform '() :accessor config-staged-files)
   (ignored-files :initform '() :accessor config-ignored-files)
   (tracked-files :initform '() :accessor config-tracked-files)
   (branch :initform "main" :accessor config-branch)
   (ahead-count :initform 0 :accessor config-ahead-count)
   (behind-count :initform 0 :accessor config-behind-count)))

(defvar *version-control-config* nil
  "Current version control configuration instance")

(defun load-version-control-config ()
  "Load configuration from disk, creating default if not exists"
  (let ((config-path (version-control-config-pathname)))
    (if (probe-file config-path)
        (with-open-file (s config-path)
          (let ((*standard-input* s))
            (read)))
        (make-instance 'version-control-config))))

(defun save-version-control-config (&optional (config *version-control-config*))
  "Save configuration to disk"
  (let ((config-path (version-control-config-pathname)))
    (with-open-file (s config-path :direction :output :if-exists :supersede)
      (write config :stream s))))

(defun ensure-version-control-config ()
  "Ensure config is loaded"
  (unless *version-control-config*
    (setf *version-control-config* (load-version-control-config))))

;;-----------------------------------------------------------------------------
;; Auto-persistence Hooks
;;-----------------------------------------------------------------------------

(defmacro with-version-control-auto-save (&body body)
  "Execute BODY and auto-save config on exit"
  `(progn
     (ensure-version-control-config)
     (unwind-protect
          (progn ,@body)
       (save-version-control-config))))

;;-----------------------------------------------------------------------------
;; Convenience Accessors
;;-----------------------------------------------------------------------------

(defun version-control-get-last-commit ()
  (ensure-version-control-config)
  (config-last-commit *version-control-config*))

(defun version-control-set-last-commit (commit)
  (ensure-version-control-config)
  (setf (config-last-commit *version-control-config*) commit)
  (save-version-control-config))

(defun version-control-get-staged-files ()
  (ensure-version-control-config)
  (config-staged-files *version-control-config*))

(defun version-control-set-staged-files (files)
  (ensure-version-control-config)
  (setf (config-staged-files *version-control-config*) files)
  (save-version-control-config))

(defun version-control-get-ignored-files ()
  (ensure-version-control-config)
  (config-ignored-files *version-control-config*))

(defun version-control-set-ignored-files (files)
  (ensure-version-control-config)
  (setf (config-ignored-files *version-control-config*) files)
  (save-version-control-config))

(defun version-control-get-tracked-files ()
  (ensure-version-control-config)
  (config-tracked-files *version-control-config*))

(defun version-control-set-tracked-files (files)
  (ensure-version-control-config)
  (setf (config-tracked-files *version-control-config*) files)
  (save-version-control-config))

(defun version-control-get-branch ()
  (ensure-version-control-config)
  (config-branch *version-control-config*))

(defun version-control-set-branch (branch)
  (ensure-version-control-config)
  (setf (config-branch *version-control-config*) branch)
  (save-version-control-config))

(defun version-control-get-ahead-count ()
  (ensure-version-control-config)
  (config-ahead-count *version-control-config*))

(defun version-control-set-ahead-count (count)
  (ensure-version-control-config)
  (setf (config-ahead-count *version-control-config*) count)
  (save-version-control-config))

(defun version-control-get-behind-count ()
  (ensure-version-control-config)
  (config-behind-count *version-control-config*))

(defun version-control-set-behind-count (count)
  (ensure-version-control-config)
  (setf (config-behind-count *version-control-config*) count)
  (save-version-control-config))
