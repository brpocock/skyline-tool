;;; src/version-control/backends/backend-cvs.lisp
;;; CVS backend implementation for Skyline-Tool version control
;;; Pure procedural implementation - no classes

(in-package :skyline-tool.version-control)

;; CVS backend protocol implementations

(defmethod version-control-backend ((backend (eql :cvs)) &key)
  "Returns the backend identifier for CVS"
  :cvs)

(defmethod version-control-name ((backend (eql :cvs)))
  "Return the name of the version control system"
  "cvs")

(defmethod version-control-version ((backend (eql :cvs)))
  "Return the version string of cvs"
  (or (ignore-errors
        (uiop:run-program (list "cvs" "--version") :output :string))
      "unknown"))

(defmethod version-control-available-p ((backend (eql :cvs)))
  "Check if cvs is available on the system"
  (ignore-errors
    (zerop (uiop:run-program (list "cvs" "--version") :output nil))))

(defmethod version-control-init ((backend (eql :cvs)) path &key)
  "Initialize a new CVS repository at PATH"
  (declare (ignore path))
  (error "CVS initialization requires manual CVSROOT setup. Use 'cvs -d <path> init' manually."))

(defmethod version-control-clone ((backend (eql :cvs)) url path &key)
  "Checkout module from URL to PATH"
  (uiop:run-program (append (list "cvs" "-d" url) (list "checkout" path)))
  (make-cvs-backend path))

(defmethod version-control-status ((backend (eql :cvs)) path &key)
  "Return status of working files"
  (let ((output (ignore-errors 
                  (uiop:run-program (list "cvs" "status" path) :output :string))))
    (when output (parse-cvs-status output))))

(defun parse-cvs-status (output)
  "Parse cvs status output into status plist"
  (let ((staged '()) (modified '()) (untracked '()))
    (dolist (line (split-sequence #\newline output))
      (when (plusp (length line))
        (cond
          ((search "Status: Up-to-date" line) nil)
          ((search "Status: Locally Modified" line) (push :modified modified))
          ((search "Status: Needs Checkout" line) (push :staged staged))
          ((search "Needs Merge" line) (push :modified modified))
          ((search "Unresolved Conflict" line) (push :modified modified))
          ((search "Unknown" line) (push :untracked untracked)))))
    (append (when staged '(:staged . t))
            (when modified '(:modified . t))
            (when untracked '(:untracked . t)))))

(defmethod version-control-add ((backend (eql :cvs)) paths &key)
  "Add files to CVS"
  (uiop:run-program (append (list "cvs" "add") paths)))

(defmethod version-control-commit ((backend (eql :cvs)) message &key)
  "Commit with MESSAGE"
  (uiop:run-program (append (list "cvs" "commit" "-m" message) (list "."))))

(defmethod version-control-reset ((backend (eql :cvs)) paths &key soft mixed hard)
  "Revert changes"
  (declare (ignore soft mixed hard))
  (uiop:run-program (append (list "cvs" "update" "-C") paths)))

(defmethod version-control-checkout ((backend (eql :cvs)) target &key create-branch)
  "Checkout module/tag"
  (declare (ignore create-branch))
  (uiop:run-program (append (list "cvs" "checkout") (list target))))

(defmethod version-control-push ((backend (eql :cvs)) remote branch &key force-with-lease)
  "CVS doesn't have push - changes are committed directly to server"
  (declare (ignore remote branch force-with-lease))
  (error "CVS doesn't support push/pull operations - commits go directly to repository"))

(defmethod version-control-pull ((backend (eql :cvs)) remote branch &key rebase)
  "Update working copy"
  (declare (ignore remote branch rebase))
  (uiop:run-program (list "cvs" "update")))

(defmethod version-control-fetch ((backend (eql :cvs)) &key remote all tags prune)
  "Update working copy"
  (declare (ignore remote all tags prune))
  (uiop:run-program (list "cvs" "update")))

(defmethod version-control-log ((backend (eql :cvs)) path &key limit since until author)
  "Return commit log"
  (let ((cmd (list "cvs" "log")))
    (when limit (push (format nil "-l~a" limit) cmd))
    (when since (push (format nil "-d\">~a\"" since) cmd))
    (when until (push (format nil "-d\"<~a\"" until) cmd))
    (ignore-errors (uiop:run-program cmd :output :string))))

(defmethod version-control-diff ((backend (eql :cvs)) path &key cached name-only)
  "Return diff against repository"
  (let ((cmd (list "cvs" "diff")))
    (when cached (push "-rBASE" cmd))
    (when name-only (push "-c" cmd))
    (when path (append cmd (list path)))
    (ignore-errors (uiop:run-program cmd :output :string))))

(defmethod version-control-difftool ((backend (eql :cvs)) path &key base target (tool "meld"))
  "Launch external diff tool"
  (let ((cmd (list "cvs" "diff")))
    (when base (append cmd (list "-r" base)))
    (when target (append cmd (list "-r" target)))
    (when tool (push (format nil "--tool=~a" tool) cmd))
    (when path (append cmd (list path)))
    (uiop:run-program cmd :output nil :ignore-errors t)))

(defmethod version-control-branch ((backend (eql :cvs)) &key list create delete)
  "CVS uses tags for branches"
  (cond
    (list (ignore-errors (uiop:run-program (list "cvs" "rtag") :output :string)))
    (create (uiop:run-program (list "cvs" "rtag" "-b" create)))
    (delete (uiop:run-program (list "cvs" "rtag" "-d" delete)))))

(defmethod version-control-merge ((backend (eql :cvs)) source &key)
  "Merge changes from source tag/branch"
  (uiop:run-program (append (list "cvs" "update" "-j" source) (list "."))))

(defmethod version-control-rebase ((backend (eql :cvs)) target &key interactive)
  "CVS doesn't have rebase"
  (declare (ignore target interactive))
  (error "CVS doesn't support rebase operation"))

(defmethod version-control-stash ((backend (eql :cvs)) action &rest args)
  "CVS doesn't have native stashing"
  (declare (ignore action args))
  (error "CVS doesn't support stashing operation"))

(defmethod version-control-tag ((backend (eql :cvs)) &key list create delete)
  "Manage tags"
  (cond
    (list (ignore-errors (uiop:run-program (list "cvs" "rtag") :output :string)))
    (create (uiop:run-program (list "cvs" "tag" create)))
    (delete (uiop:run-program (list "cvs" "tag" "-d" delete)))))

(defmethod version-control-config-get ((backend (eql :cvs)) key &key global local)
  "CVS config is stored in CVSROOT/config"
  (declare (ignore global local))
  (uiop:run-program (list "cvs" "cvsadmin" "-s" key) :output :string))

(defmethod version-control-config-set ((backend (eql :cvs)) key value &key global local)
  "Set CVS config"
  (declare (ignore global local))
  (uiop:run-program (list "cvs" "cvsadmin" "-s" (format nil "~a=~a" key value))))

(defmethod version-control-user-name ((backend (eql :cvs)) &key global)
  "Get user name from CVS"
  (declare (ignore global))
  (user-real-name))

(defmethod version-control-user-email ((backend (eql :cvs)) &key global)
  "Get user email from CVS"
  (declare (ignore global))
  (format nil "~a@~a" (user-real-name) (machine-instance)))

(defmethod version-control-set-user ((backend (eql :cvs)) name email &key global)
  "Set user info in CVS"
  (declare (ignore global))
  ;; CVS derives user info from environment
  (version-control-config-set backend "cvsroot" (format nil "~a ~a" name email)))

(defun make-cvs-backend (path)
  "Create a CVS backend instance"
  (declare (ignore path))
  'cvs-backend)