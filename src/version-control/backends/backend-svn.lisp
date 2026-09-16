;;; src/version-control/backends/backend-svn.lisp
;;; SVN backend implementation for Skyline-Tool version control
;;; Pure procedural implementation - no classes

(in-package :skyline-tool.version-control)

;; SVN backend protocol implementations

(defmethod version-control-backend ((backend (eql :svn)) &key)
  "Returns the backend identifier for Subversion"
  :svn)

(defmethod version-control-name ((backend (eql :svn)))
  "Return the name of the version control system"
  "svn")

(defmethod version-control-version ((backend (eql :svn)))
  "Return the version string of svn"
  (uiop:run-program (list "svn" "--version") :output :string))

(defmethod version-control-available-p ((backend (eql :svn)))
  "Check if svn is available on the system"
  (uiop:run-program (list "svn" "--version") :output nil))

(defmethod version-control-init ((backend (eql :svn)) path &key)
  "Initialize a new svn repository at PATH"
  (uiop:run-program (list "svnadmin" "create" path)))

(defmethod version-control-clone ((backend (eql :svn)) url path &key)
  "Checkout repository from URL to PATH"
  (uiop:run-program (list "svn" "checkout" url path)))

(defmethod version-control-status ((backend (eql :svn)) path &key)
  "Return status of working tree"
  (let ((output (uiop:run-program (list "svn" "status" path) :output :string)))
    (when (not (emptyp (string-trim skyline-tool::+whitespace+ output))) (parse-svn-status output))))

(defun parse-svn-status (output)
  "Parse svn status output into status plist"
  (let (result)
    (dolist (line (split-sequence #\newline output))
      (when (plusp (length line))
        (let ((status-char (char line 0)))
          (cond
            ((char= status-char #\A) (push :staged result))  ; Added
            ((char= status-char #\M) (push :modified result))  ; Modified
            ((char= status-char #\?) (push :untracked result))  ; Untracked
            ((char= status-char #\+) (push :staged result))  ; In conflict
            ((char= status-char #\-) (push :staged result)))))  ; Removed
      result)))

(defmethod version-control-add ((backend (eql :svn)) paths &key)
  "Add PATHS for commit"
  (uiop:run-program (append (list "svn" "add") paths)))

(defmethod version-control-commit ((backend (eql :svn)) message &key)
  "Commit with MESSAGE"
  (uiop:run-program (list "svn" "commit" "-m" message)))

(defmethod version-control-reset ((backend (eql :svn)) paths &key soft mixed hard)
  "Revert PATHS"
  (declare (ignore soft mixed hard))
  (uiop:run-program (append (list "svn" "revert") paths)))

(defmethod version-control-log ((backend (eql :svn)) path &key limit since until author)
  "Return commit log"
  (let ((cmd (list "svn" "log")))
    (when limit
      (appendf cmd (list "-l" limit)))
    (uiop:run-program (append cmd (list (or path (version-control-config-get backend "svn:repository-root"))))
                      :output :string)))

(defmethod version-control-push ((backend (eql :svn)) remote branch &key force-with-lease-p)
  "Commit and push changes"
  (declare (ignore force-with-lease-p))
  (uiop:run-program (list "svn" "commit" "-m" (format nil "Pushing to ~a branch ~a" remote branch))))

(defmethod version-control-pull ((backend (eql :svn)) remote branch &key rebase)
  "Update from remote"
  (declare (ignore rebase))
  (uiop:run-program (list "svn" "update" branch)))

(defmethod version-control-fetch ((backend (eql :svn)) &key remote all tags prune)
  "Update working copy"
  (declare (ignore remote all tags prune))
  (uiop:run-program (list "svn" "update")))

(defmethod version-control-remote-add ((backend (eql :svn)) name url)
  "Add svn remote"
  (uiop:run-program (list "svn" "propset" "svn:externals" name url ".")))

(defmethod version-control-remote-list ((backend (eql :svn)))
  "List svn information"
  (uiop:run-program (list "svn" "info" "--show-item" "url") :output :string))

(defmethod version-control-submodule-add ((backend (eql :svn)) url path &key branch)
  "Add svn:externals (closest equivalent to submodules)"
  (declare (ignore branch))
  (uiop:run-program (list "svn" "propset" "svn:externals" path url ".")))

(defmethod version-control-submodule-update ((backend (eql :svn)) &key init recursive remote)
  "Update externals"
  (declare (ignore init recursive remote))
  (uiop:run-program (list "svn" "update")))

(defmethod version-control-submodule-status ((backend (eql :svn)))
  "Check externals status"
  (uiop:run-program (list "svn" "status" "--show-ipc") :output :string))

(defmethod version-control-stash ((backend (eql :svn)) action &rest args)
  "SVN doesn't have native stashing - use shelve or revert"
  (declare (ignore action args))
  (error "Stashing not natively supported in SVN. Use shelving or revert."))

(defmethod version-control-tag ((backend (eql :svn)) &key list create delete annotate)
  "SVN uses tags as branches"
  (cond
    (list (ignore-errors (uiop:run-program (list "svn" "list" "svn://trunk/tags") :output :string)))
    (create (uiop:run-program (list "svn" "copy" "HEAD" (format nil "svn://trunk/tags/~a" create))))
    (delete (uiop:run-program (list "svn" "delete" (format nil "svn://trunk/tags/~a" delete))))
    (annotate (uiop:run-program (list "svn" "copy" "HEAD" (format nil "svn://trunk/tags/~a" annotate)
                                      "-m" "Annotated tag")))))

(defmethod version-control-config-get ((backend (eql :svn)) key &key global local)
  "Get svn config"
  (declare (ignore global local))
  (uiop:run-program (list "svn" "config" "get" (format nil "~a" key)) :output :string))

(defmethod version-control-config-set ((backend (eql :svn)) key value &key global local)
  "Set svn config"
  (declare (ignore global local))
  (uiop:run-program (list "svn" "config" "set" (format nil "~a" key) value)))

(defmethod version-control-user-name ((backend (eql :svn)) &key global)
  "Get svn user name"
  (declare (ignore global))
  (uiop:run-program (list "svn" "config" "get" "user-name") :output :string))

(defmethod version-control-user-email ((backend (eql :svn)) &key global)
  "Get svn user email"
  (declare (ignore global))
  (uiop:run-program (list "svn" "config" "get" "user-email") :output :string))

(defmethod version-control-set-user ((backend (eql :svn)) name email &key global)
  "Set svn user name and email"
  (declare (ignore global))
  (uiop:run-program (list "svn" "config" "set" "user-name" name))
  (uiop:run-program (list "svn" "config" "set" "user-email" email)))
