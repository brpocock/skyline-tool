;;; src/version-control/backends/backend-rcs.lisp
;;; RCS backend implementation for Skyline-Tool version control
;;; Pure procedural implementation - no classes

(in-package :skyline-tool.version-control)

;; RCS backend protocol implementations

(defmethod version-control-backend ((backend (eql :rcs)) &key)
  "Returns the backend identifier for RCS"
  :rcs)

(defmethod version-control-name ((backend (eql :rcs)))
  "Return the name of the version control system"
  "rcs")

(defmethod version-control-version ((backend (eql :rcs)))
  "Return the version string of rcs"
  (or (ignore-errors
        (uiop:run-program (list "rcs" "-V") :output :string))
      "5.10"))

(defmethod version-control-available-p ((backend (eql :rcs)))
  "Check if rcs is available on the system"
  (ignore-errors
    (zerop (uiop:run-program (list "rcs" "-V") :output nil))))

(defmethod version-control-init ((backend (eql :rcs)) path &key)
  "Initialize RCS repository at PATH"
  (let ((rcs-dir (merge-pathnames "RCS" (make-pathname :directory path))))
    (unless (probe-file rcs-dir)
      (ensure-directories-exist rcs-dir))
    (make-rcs-backend path)))

(defmethod version-control-clone ((backend (eql :rcs)) url path &key)
  "Copy RCS files from URL to PATH"
  (uiop:run-program (list "cp" "-r" url path))
  (make-rcs-backend path))

(defmethod version-control-status ((backend (eql :rcs)) path &key)
  "Return status of RCS files"
  (let ((output (ignore-errors 
                  (uiop:run-program (append (list "rcs" "-l") (list path)) :output :string))))
    (when output (parse-rcs-status output))))

(defun parse-rcs-status (output)
  "Parse rcs status output into status plist"
  (let ((modified '()) (locked '()))
    (dolist (line (split-sequence #\newline output))
      (when (plusp (length line))
        (cond
          ((search "locked" line) (push :locked modified))
          ((search "revision" line) (push :modified modified)))))
    (append (when modified '(:modified . t))
            (when locked '(:staged . t)))))

(defmethod version-control-add ((backend (eql :rcs)) paths &key)
  "Create new RCS file for PATHS"
  (dolist (path paths)
    (uiop:run-program (list "rcs" "-i" path))))

(defmethod version-control-commit ((backend (eql :rcs)) message &key)
  "Check in changes with MESSAGE"
  (uiop:run-program (append (list "rcs") (list "-m" message) (list "."))))

(defmethod version-control-reset ((backend (eql :rcs)) paths &key soft mixed hard)
  "Revert or rollback changes"
  (declare (ignore soft mixed hard))
  (uiop:run-program (append (list "rcs" "-u") paths)))

(defmethod version-control-checkout ((backend (eql :rcs)) target &key create-branch)
  "Check out a specific revision"
  (let ((cmd (list "co")))
    (when create-branch (append cmd (list "-r" target)))
    (append cmd (list "."))
    (uiop:run-program (nreverse cmd))))

(defmethod version-control-push ((backend (eql :rcs)) remote branch &key force-with-lease)
  "RCS doesn't have push - it's a local system"
  (declare (ignore remote branch force-with-lease))
  (error "RCS is a local revision system - no push operation"))

(defmethod version-control-pull ((backend (eql :rcs)) remote branch &key rebase)
  "RCS doesn't have pull - it's a local system"
  (declare (ignore remote branch rebase))
  (error "RCS is a local revision system - no pull operation"))

(defmethod version-control-fetch ((backend (eql :rcs)) &key remote all tags prune)
  "RCS doesn't have fetch - it's a local system"
  (declare (ignore remote all tags prune))
  (error "RCS is a local revision system - no fetch operation"))

(defmethod version-control-log ((backend (eql :rcs)) path &key limit since until author)
  "Return revision history"
  (let ((cmd (list "rlog" path)))
    (when limit (push (format nil "-l~a" limit) cmd))
    (ignore-errors (uiop:run-program (nreverse cmd) :output :string))))

(defmethod version-control-diff ((backend (eql :rcs)) path &key cached name_only)
  "Return diff for RCS file"
  (let ((cmd (list "rcsdiff")))
    (when cached (push "-r1.1" cmd))
    (when path (append cmd (list path)))
    (ignore-errors (uiop:run-program (nreverse cmd) :output :string))))

(defmethod version-control-difftool ((backend (eql :rcs)) path &key base target (tool "meld"))
  "Launch external diff tool"
  (declare (ignore tool))
  (let ((cmd (list "rcsdiff" path)))
    (when base (append cmd (list "-r" base)))
    (when target (append cmd (list "-r" target)))
    (uiop:run-program (nreverse cmd) :output nil :ignore-errors t)))

(defmethod version-control-branch ((backend (eql :rcs)) &key list create delete)
  "RCS uses symbolic names for branches"
  (cond
    (list (ignore-errors (uiop:run-program (list "rlog" "-h") :output :string)))
    (create (uiop:run-program (append (list "rcs" "-n" create) (list "."))))
    (delete (uiop:run-program (append (list "rcs" "-n" (format nil "%s=:" delete)) (list "."))))))

(defmethod version-control-merge ((backend (eql :rcs)) source &key)
  "Merge revisions"
  (uiop:run-program (append (list "rcs" "-m" source) (list "."))))

(defmethod version-control-rebase ((backend (eql :rcs)) target &key interactive)
  "RCS doesn't support rebase"
  (declare (ignore target interactive))
  (error "RCS doesn't support rebase operation"))

(defmethod version-control-stash ((backend (eql :rcs)) action &rest args)
  "RCS doesn't have stashing"
  (declare (ignore action args))
  (error "RCS doesn't support stashing operation"))

(defmethod version-control-tag ((backend (eql :rcs)) &key list create delete)
  "Manage symbolic tags"
  (cond
    (list (ignore-errors (uiop:run-program (list "rlog" "-h") :output :string)))
    (create (uiop:run-program (append (list "rcs" "-n" create) (list "."))))
    (delete (uiop:run-program (append (list "rcs" "-n" (format nil "%s=:" delete)) (list "."))))))

(defmethod version-control-config-get ((backend (eql :rcs)) key &key global local)
  "RCS config is stored in RCS files"
  (declare (ignore global local))
  (ignore-errors (uiop:run-program (list "rcsco" "-p" "-q" key))))

(defmethod version-control-config-set ((backend (eql :rcs)) key value &key global local)
  "Set RCS config"
  (declare (ignore global local))
  (uiop:run-program (append (list "rcsco" "-m" (format nil "~a=~a" key value)) (list key))))

(defmethod version-control-user-name ((backend (eql :rcs)) &key global)
  "Get user name from environment"
  (declare (ignore global))
  (user-real-name))

(defmethod version-control-user-email ((backend (eql :rcs)) &key global)
  "Get user email from environment"
  (declare (ignore global))
  (format nil "~a@~a" (user-real-name) (machine-instance)))

(defmethod version-control-set-user ((backend (eql :rcs)) name email &key global)
  "Set user info in environment"
  (declare (ignore global))
  (version-control-config-set backend "USER" name)
  (version-control-config-set backend "EMAIL" email))

(defun make-rcs-backend (path)
  "Create an RCS backend instance"
  (declare (ignore path))
  'rcs-backend)
