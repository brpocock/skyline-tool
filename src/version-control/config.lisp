;;; src/version-control/config.lisp
;;; Unified configuration interface for Skyline-Tool version control

(in-package :skyline-tool.version-control)


;;-----------------------------------------------------------------------------
;; Configuration Management
;;-----------------------------------------------------------------------------

(defclass vc-config ()
  ((last-commit :initform nil :accessor config-last-commit)
   (staged-files :initform '() :accessor config-staged-files)
   (ignored-files :initform '() :accessor config-ignored-files)
   (tracked-files :initform '() :accessor config-tracked-files)
   (branch :initform "main" :accessor config-branch)
   (ahead-count :initform 0 :accessor config-ahead-count)
   (behind-count :initform 0 :accessor config-behind-count)))

(defvar *vc-config* nil
  "Current version control configuration instance")

(defun load-vc-config ()
  "Load configuration from disk, creating default if not exists"
  (let ((config-path (vc-config-pathname)))
    (if (probe-file config-path)
        (with-open-file (s config-path)
          (let ((*standard-input* s))
            (read)))
        (make-instance 'vc-config))))

(defun save-vc-config (&optional (config *vc-config*))
  "Save configuration to disk"
  (let ((config-path (vc-config-pathname)))
    (with-open-file (s config-path :direction :output :if-exists :supersede)
      (write config :stream s))))

(defun ensure-vc-config ()
  "Ensure config is loaded"
  (unless *vc-config*
    (setf *vc-config* (load-vc-config))))

;;-----------------------------------------------------------------------------
;; Auto-persistence Hooks
;;-----------------------------------------------------------------------------

(defmacro with-vc-auto-save (&body body)
  "Execute BODY and auto-save config on exit"
  `(progn
     (ensure-vc-config)
     (unwind-protect
          (progn ,@body)
       (save-vc-config))))

;;-----------------------------------------------------------------------------
;; Convenience Accessors
;;-----------------------------------------------------------------------------

(defun vc-get-last-commit ()
  (ensure-vc-config)
  (config-last-commit *vc-config*))

(defun vc-set-last-commit (commit)
  (ensure-vc-config)
  (setf (config-last-commit *vc-config*) commit)
  (save-vc-config))

(defun vc-get-staged-files ()
  (ensure-vc-config)
  (config-staged-files *vc-config*))

(defun vc-set-staged-files (files)
  (ensure-vc-config)
  (setf (config-staged-files *vc-config*) files)
  (save-vc-config))

(defun vc-get-ignored-files ()
  (ensure-vc-config)
  (config-ignored-files *vc-config*))

(defun vc-set-ignored-files (files)
  (ensure-vc-config)
  (setf (config-ignored-files *vc-config*) files)
  (save-vc-config))

(defun vc-get-tracked-files ()
  (ensure-vc-config)
  (config-tracked-files *vc-config*))

(defun vc-set-tracked-files (files)
  (ensure-vc-config)
  (setf (config-tracked-files *vc-config*) files)
  (save-vc-config))

(defun vc-get-branch ()
  (ensure-vc-config)
  (config-branch *vc-config*))

(defun vc-set-branch (branch)
  (ensure-vc-config)
  (setf (config-branch *vc-config*) branch)
  (save-vc-config))

(defun vc-get-ahead-count ()
  (ensure-vc-config)
  (config-ahead-count *vc-config*))

(defun vc-set-ahead-count (count)
  (ensure-vc-config)
  (setf (config-ahead-count *vc-config*) count)
  (save-vc-config))

(defun vc-get-behind-count ()
  (ensure-vc-config)
  (config-behind-count *vc-config*))

(defun vc-set-behind-count (count)
  (ensure-vc-config)
  (setf (config-behind-count *vc-config*) count)
  (save-vc-config))
