;;; src/version-control/backends/backend-mercurial.lisp
;;; Mercurial backend implementation for Skyline-Tool version control
;;; Pure procedural implementation - no classes

(in-package :skyline-tool.version-control)

;; Mercurial backend protocol implementations

(defmethod version-control-backend ((backend (eql :mercurial)) &key)
  "Returns the backend identifier for Mercurial"
  :mercurial)

(defmethod version-control-name ((backend (eql :mercurial)))
  "Return the name of the version control system"
  "mercurial")

(defmethod version-control-version ((backend (eql :mercurial)))
  "Return the version string of hg"
  (or (ignore-errors
        (uiop:run-program (list "hg" "--version") :output :string))
      "unknown"))

(defmethod version-control-available-p ((backend (eql :mercurial)))
  "Check if hg is available on the system"
  (ignore-errors
    (zerop (uiop:run-program (list "hg" "--version") :output nil))))

(defmethod version-control-init ((backend (eql :mercurial)) path &key)
  "Initialize a new Mercurial repository at PATH"
  (when (probe-file path)
    (uiop:run-program (list "hg" "init" path)))
  (make-mercurial-backend path))

(defmethod version-control-clone ((backend (eql :mercurial)) url path &key)
  "Clone repository from URL to PATH"
  (uiop:run-program (list "hg" "clone" url path))
  (make-mercurial-backend path))

(defmethod version-control-status ((backend (eql :mercurial)) path &key)
  "Return status of working tree"
  (let ((output (ignore-errors 
                  (uiop:run-program (list "hg" "status" path) :output :string))))
    (when output (parse-hg-status output))))

(defun parse-hg-status (output)
  "Parse hg status output into status plist"
  (let ((staged '()) (modified '()) (untracked '()))
    (dolist (line (split-sequence #\newline output))
      (when (plusp (length line))
        (let ((status-char (char line 0)))
          (cond
            ((char= status-char #\A) (push :staged staged))  ; Added
            ((char= status-char #\M) (push :modified modified))  ; Modified
            ((char= status-char #\?) (push :untracked untracked))  ; Untracked
            ((char= status-char #\R) (push :staged staged))  ; Removed
            ((char= status-char #\!) (push :staged staged)))))  ; Missing
      (append (when staged '(:staged . t))
              (when modified '(:modified . t))
              (when untracked '(:untracked . t))))))

(defmethod version-control-add ((backend (eql :mercurial)) paths &key)
  "Add PATHS for commit"
  (uiop:run-program (append (list "hg" "add") paths)))

(defmethod version-control-commit ((backend (eql :mercurial)) message &key amend signoff author)
  "Commit with MESSAGE"
  (let ((cmd (list "hg" "commit" "-m" message)))
    (when amend (push "--amend" cmd))
    (when signoff (push "--signoff" cmd))
    (when author (append cmd (list "--user" author)))
    (uiop:run-program (nreverse cmd))))

(defmethod version-control-reset ((backend (eql :mercurial)) paths &key soft mixed hard)
  "Revert or reset PATHS"
  (cond
    (hard (uiop:run-program (append (list "hg" "revert" "--all" "--no-backup") paths)))
    (t (uiop:run-program (append (list "hg" "revert") paths)))))

(defmethod version-control-checkout ((backend (eql :mercurial)) target &key create-branch)
  "Checkout/Update to TARGET branch or commit"
  (let ((cmd (list "hg" "update")))
    (when create-branch (append cmd (list "--new-branch" target)))
    (append cmd (list target))
    (uiop:run-program (nreverse cmd))))

(defmethod version-control-push ((backend (eql :mercurial)) remote branch &key force-with-lease)
  "Push to REMOTE"
  (declare (ignore force-with-lease))
  (let ((cmd (list "hg" "push")))
    (when branch (append cmd (list "-r" branch)))
    (when remote (append cmd (list remote)))
    (uiop:run-program (nreverse cmd))))

(defmethod version-control-pull ((backend (eql :mercurial)) remote branch &key rebase)
  "Pull from REMOTE"
  (let ((cmd (list "hg" "pull")))
    (when rebase (push "--rebase" cmd))
    (when branch (append cmd (list "-r" branch)))
    (when remote (append cmd (list remote)))
    (uiop:run-program (nreverse cmd))))

(defmethod version-control-fetch ((backend (eql :mercurial)) &key remote all tags prune)
  "Fetch from REMOTE(s)"
  (declare (ignore all tags prune))
  (let ((cmd (list "hg" "incoming")))
    (when remote (append cmd (list remote)))
    (uiop:run-program (nreverse cmd))))

(defmethod version-control-log ((backend (eql :mercurial)) path &key limit since until author)
  "Return commit log"
  (let ((cmd (list "hg" "log")))
    (when limit (append cmd (list "-l" (write-to-string limit))))
    (when since (append cmd (list "--date" (format nil ">~a" since))))
    (when until (append cmd (list "--date" (format nil "<~a" until))))
    (when author (append cmd (list "--user" author)))
    (ignore-errors (uiop:run-program (nreverse cmd) :output :string))))

(defmethod version-control-diff ((backend (eql :mercurial)) path &key cached name-only)
  "Return diff for PATH"
  (let ((cmd (list "hg" "diff")))
    (when cached (push "--rev" cmd))
    (when name-only (push "--stat" cmd))
    (when path (append cmd (list path)))
    (ignore-errors (uiop:run-program (nreverse cmd) :output :string))))

(defmethod version-control-difftool ((backend (eql :mercurial)) path &key base target (tool "meld"))
  "Launch external diff tool"
  (let ((cmd (list "hg" "diff")))
    (when base (append cmd (list "--rev" base)))
    (when target (append cmd (list "--rev" target)))
    (when tool (push (format nil "--tool=~a" tool) cmd))
    (when path (append cmd (list path)))
    (uiop:run-program (nreverse cmd) :output nil :ignore-errors t)))

(defmethod version-control-branch ((backend (eql :mercurial)) &key list all create delete)
  "Branch operations"
  (let ((cmd (list "hg" "branches")))
    (cond
      (list (push (if all "--all" "") cmd))
      (create (uiop:run-program (append (list "hg" "branch") (list create))))
      (delete (uiop:run-program (append (list "hg" "branch" "--close") (list delete)))))
    (when (or list all)
      (let ((output (ignore-errors (uiop:run-program (nreverse cmd) :output :string))))
        (when output
          (split-sequence #\newline output :remove-empty-subseqs t))))))

(defmethod version-control-merge ((backend (eql :mercurial)) source &key)
  "Merge SOURCE into current branch"
  (uiop:run-program (list "hg" "merge" source)))

(defmethod version-control-rebase ((backend (eql :mercurial)) target &key interactive)
  "Rebase onto TARGET"
  (declare (ignore interactive))
  (uiop:run-program (list "hg" "rebase" "--onto" target)))

(defmethod version-control-stash ((backend (eql :mercurial)) action &rest args)
  "Manage shelves (hg shelve)"
  (let ((cmd (list "hg" "shelve")))
    (ecase action
      (:push (push "--message" cmd) (push (first args) cmd))
      (:pop (push "--delete" cmd))
      (:list (push "--list" cmd))
      (:drop (push "--delete" cmd) (push (first args) cmd))
      (:apply (push "--apply" cmd) (push (first args) cmd)))
    (uiop:run-program (nreverse cmd) :output :string :ignore-errors t)))

(defmethod version-control-tag ((backend (eql :mercurial)) &key list create delete)
  "Manage tags"
  (cond
    (list (ignore-errors (uiop:run-program (list "hg" "tags") :output :string)))
    (create (uiop:run-program (list "hg" "tag" create)))
    (delete (uiop:run-program (list "hg" "tag" "--remove" delete)))))

(defmethod version-control-config-get ((backend (eql :mercurial)) key &key global local)
  "Get hg config"
  (declare (ignore global local))
  (uiop:run-program (list "hg" "config" key) :output :string))

(defmethod version-control-config-set ((backend (eql :mercurial)) key value &key global local)
  "Set hg config"
  (declare (ignore global local))
  (uiop:run-program (list "hg" "config" key value)))

(defmethod version-control-user-name ((backend (eql :mercurial)) &key global)
  "Get user name from hg config"
  (declare (ignore global))
  (version-control-config-get backend "ui.username"))

(defmethod version-control-user-email ((backend (eql :mercurial)) &key global)
  "Get user email from hg config"
  (declare (ignore global))
  (version-control-config-get backend "ui.username"))

(defmethod version-control-set-user ((backend (eql :mercurial)) name email &key global)
  "Set user name in hg config"
  (declare (ignore global))
  (version-control-config-set backend "ui.username" name)
  (version-control-config-set backend "ui.username" email))

(defun make-mercurial-backend (path)
  "Create a Mercurial backend instance"
  (declare (ignore path))
  'mercurial-backend)
