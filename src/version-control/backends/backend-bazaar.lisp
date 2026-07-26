;;; src/version-control/backends/backend-bazaar.lisp
;;; Bazaar backend implementation for Skyline-Tool version control
;;; Pure procedural implementation - no classes

(in-package :skyline-tool.version-control)

;; Bazaar backend protocol implementations

(defmethod version-control-backend ((backend (eql :bazaar)) &key)
  "Returns the backend identifier for Bazaar"
  :bazaar)

(defmethod version-control-name ((backend (eql :bazaar)))
  "Return the name of the version control system"
  "bazaar")

(defmethod version-control-version ((backend (eql :bazaar)))
  "Return the version string of bzr"
  (or (ignore-errors
        (uiop:run-program (list "bzr" "--version") :output :string))
      "unknown"))

(defmethod version-control-available-p ((backend (eql :bazaar)))
  "Check if bzr is available on the system"
  (ignore-errors
    (zerop (uiop:run-program (list "bzr" "--version") :output nil))))

(defmethod version-control-init ((backend (eql :bazaar)) path &key)
  "Initialize a new Bazaar repository at PATH"
  (when (probe-file path)
    (uiop:run-program (list "bzr" "init" path)))
  (make-bazaar-backend path))

(defmethod version-control-clone ((backend (eql :bazaar)) url path &key)
  "Clone repository from URL to PATH"
  (uiop:run-program (list "bzr" "branch" url path))
  (make-bazaar-backend path))

(defmethod version-control-status ((backend (eql :bazaar)) path &key)
  "Return status of working tree"
  (let ((output (ignore-errors 
                  (uiop:run-program (list "bzr" "status" path) :output :string))))
    (when output (parse-bzr-status output))))

(defun parse-bzr-status (output)
  "Parse bzr status output into status plist"
  (let ((staged '()) (modified '()) (untracked '()))
    (dolist (line (split-sequence #\newline output))
      (when (plusp (length line))
        (let ((status (subseq line 0 1)))
          (cond
            ((string= status "added") (push :staged staged))
            ((string= status "modified") (push :modified modified))
            ((string= status "unknown") (push :untracked untracked))
            ((string= status "deleted") (push :staged staged))
            ((string= status "renamed") (push :staged staged))))))
    (append (when staged '(:staged . t))
            (when modified '(:modified . t))
            (when untracked '(:untracked . t)))))

(defmethod version-control-add ((backend (eql :bazaar)) paths &key)
  "Add PATHS for commit"
  (uiop:run-program (append (list "bzr" "add") paths)))

(defmethod version-control-commit ((backend (eql :bazaar)) message &key amend signoff author)
  "Commit with MESSAGE"
  (let ((cmd (list "bzr" "commit" "-m" message)))
    (when amend (push "--amend" cmd))
    (when signoff (push "--signoff" cmd))
    (when author (append cmd (list "--author" author)))
    (uiop:run-program (nreverse cmd))))

(defmethod version-control-reset ((backend (eql :bazaar)) paths &key soft mixed hard)
  "Revert changes"
  (cond
    (hard (uiop:run-program (append (list "bzr" "revert" "--force") paths)))
    (t (uiop:run-program (append (list "bzr" "revert") paths)))))

(defmethod version-control-checkout ((backend (eql :bazaar)) target &key create-branch)
  "Checkout/Update to TARGET branch"
  (let ((cmd (list "bzr" "checkout")))
    (when create-branch (append cmd (list "--create-branch" target)))
    (append cmd (list target))
    (uiop:run-program (nreverse cmd))))

(defmethod version-control-push ((backend (eql :bazaar)) remote branch &key force-with-lease)
  "Push to REMOTE"
  (let ((cmd (list "bzr" "push")))
    (when force-with-lease (push "--force" cmd))
    (when branch (append cmd (list "-r" branch)))
    (when remote (append cmd (list remote)))
    (uiop:run-program (nreverse cmd))))

(defmethod version-control-pull ((backend (eql :bazaar)) remote branch &key rebase)
  "Pull from REMOTE"
  (let ((cmd (list "bzr" "pull")))
    (when rebase (push "--rebase" cmd))
    (when branch (append cmd (list "-r" branch)))
    (when remote (append cmd (list remote)))
    (uiop:run-program (nreverse cmd))))

(defmethod version-control-fetch ((backend (eql :bazaar)) &key remote all tags prune)
  "Fetch from REMOTE(s)"
  (let ((cmd (list "bzr" "fetch")))
    (when remote (append cmd (list remote)))
    (when all (push "--all" cmd))
    (when tags (push "--tags" cmd))
    (when prune (push "--prune" cmd))
    (uiop:run-program (nreverse cmd))))

(defmethod version-control-log ((backend (eql :bazaar)) path &key limit since until author)
  "Return commit log"
  (let ((cmd (list "bzr" "log")))
    (when limit (append cmd (list "-l" (write-to-string limit))))
    (when since (append cmd (list "--since" (write-to-string since))))
    (when until (append cmd (list "--until" (write-to-string until))))
    (when author (append cmd (list "--author" author)))
    (ignore-errors (uiop:run-program (nreverse cmd) :output :string))))

(defmethod version-control-diff ((backend (eql :bazaar)) path &key cached name_only)
  "Return diff for PATH"
  (let ((cmd (list "bzr" "diff")))
    (when cached (push "--old" cmd))
    (when name_only (push "--stat" cmd))
    (when path (append cmd (list path)))
    (ignore-errors (uiop:run-program (nreverse cmd) :output :string))))

(defmethod version-control-difftool ((backend (eql :bazaar)) path &key base target (tool "meld"))
  "Launch external diff tool"
  (let ((cmd (list "bzr" "diff")))
    (when base (append cmd (list "-r" base)))
    (when target (append cmd (list "-r" target)))
    (when tool (push (format nil "--tool=~a" tool) cmd))
    (when path (append cmd (list path)))
    (uiop:run-program (nreverse cmd) :output nil :ignore-errors t)))

(defmethod version-control-branch ((backend (eql :bazaar)) &key list all create delete)
  "Branch operations"
  (let ((cmd (list "bzr" "branch")))
    (cond
      (list (push (if all "--all" "") cmd))
      (create (append cmd (list create)))
      (delete (append cmd (list "delete" delete))))
    (when (or list all)
      (let ((output (ignore-errors (uiop:run-program (nreverse cmd) :output :string))))
        (when output
          (split-sequence #\newline output :remove-empty-subseqs t))))))

(defmethod version-control-merge ((backend (eql :bazaar)) source &key)
  "Merge SOURCE into current branch"
  (uiop:run-program (list "bzr" "merge" source)))

(defmethod version-control-rebase ((backend (eql :bazaar)) target &key interactive)
  "Rebase onto TARGET"
  (declare (ignore interactive))
  (uiop:run-program (list "bzr" "rebase" "--onto" target)))

(defmethod version-control-stash ((backend (eql :bazaar)) action &rest args)
  "Manage shelves"
  (let ((cmd (list "bzr" "shelve")))
    (ecase action
      (:push (push "--message" cmd) (push (first args) cmd))
      (:pop (push "--delete" cmd))
      (:list (push "--list" cmd))
      (:drop (push "--delete" cmd) (push (first args) cmd))
      (:apply (push "--apply" cmd) (push (first args) cmd)))
    (uiop:run-program (nreverse cmd) :output :string :ignore-errors t)))

(defmethod version-control-tag ((backend (eql :bazaar)) &key list create delete)
  "Manage tags"
  (cond
    (list (ignore-errors (uiop:run-program (list "bzr" "tags") :output :string)))
    (create (uiop:run-program (list "bzr" "tag" create)))
    (delete (uiop:run-program (list "bzr" "tag" "--delete" delete)))))

(defmethod version-control-config-get ((backend (eql :bazaar)) key &key global local)
  "Get bzr config"
  (declare (ignore global local))
  (uiop:run-program (list "bzr" "config" key) :output :string))

(defmethod version-control-config-set ((backend (eql :bazaar)) key value &key global local)
  "Set bzr config"
  (declare (ignore global local))
  (uiop:run-program (list "bzr" "config" key value)))

(defmethod version-control-user-name ((backend (eql :bazaar)) &key global)
  "Get user name from bzr config"
  (declare (ignore global))
  (version-control-config-get backend "email"))

(defmethod version-control-user-email ((backend (eql :bazaar)) &key global)
  "Get user email from bzr config"
  (declare (ignore global))
  (version-control-config-get backend "email"))

(defmethod version-control-set-user ((backend (eql :bazaar)) name email &key global)
  "Set user name and email in bzr config"
  (declare (ignore global))
  (version-control-config-set backend "email" email)
  (version-control-config-set backend "email" name))

(defun make-bazaar-backend (path)
  "Create a Bazaar backend instance"
  (declare (ignore path))
  'bazaar-backend)
