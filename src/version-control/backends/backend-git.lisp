;;; src/version-control/backends/backend-git.lisp
;;; Git backend implementation for Skyline-Tool version control
;;; Pure procedural implementation - no classes

(in-package :skyline-tool.version-control)

;; Git backend protocol implementations using EQL dispatch

(defmethod version-control-backend ((backend (eql :git)) &key)
  "Returns the backend identifier for Git"
  :git)

(defmethod version-control-name ((backend (eql :git)))
  "Return the name of the version control system"
  "git")

(defmethod version-control-version ((backend (eql :git)))
  "Return the version string of git"
  (or (ignore-errors
        (uiop:run-program (list "git" "--version") :output :string))
      "unknown"))

(defmethod version-control-available-p ((backend (eql :git)))
  "Check if git is available on the system"
  (ignore-errors
    (zerop (uiop:run-program (list "git" "--version") :output nil))))

(defmethod version-control-init ((backend (eql :git)) path &key)
  "Initialize a new git repository at PATH"
  (when (probe-file path)
    (uiop:run-program (list "git" "init" path)))
  (make-git-backend path))

(defmethod version-control-clone ((backend (eql :git)) url path &key)
  "Clone repository from URL to PATH"
  (uiop:run-program (list "git" "clone" url path))
  (make-git-backend path))

(defmethod version-control-status ((backend (eql :git)) path &key)
  "Return status of working tree as a plist"
  (let ((output (ignore-errors
                  (uiop:run-program (list "git" "-C" path "status" "--porcelain")
                                    :output :string))))
    (when output (parse-git-status output))))

(defun parse-git-status (output)
  "Parse git status --porcelain output into status plist"
  (let ((staged '()) (modified '()) (untracked '()))
    (dolist (line (split-sequence #\newline output))
      (when (plusp (length line))
        (let ((code (string-trim skyline-tool::+whitespace+ (subseq line 0 2))))
          (case (intern (string-upcase code) :keyword)
            (:A (push :staged staged))
            (:M (push :modified modified))
            (:?? (push :untracked untracked)))))
      (append (when staged '(:staged . t))
              (when modified '(:modified . t))
              (when untracked '(:untracked . t))))))

(defmethod version-control-add ((backend (eql :git)) paths &key)
  "Stage PATHS for commit"
  (uiop:run-program (append (list "git" "add") paths)))

(defmethod version-control-commit ((backend (eql :git)) message &key amend signoff author)
  "Commit staged changes with MESSAGE"
  (let ((cmd (list "git" "commit" "-m" message)))
    (when amend (push "--amend" cmd))
    (when signoff (push "--signoff" cmd))
    (when author (append cmd (list "--author" author)))
    (uiop:run-program (nreverse cmd))))

(defmethod version-control-reset ((backend (eql :git)) paths &key soft mixed hard)
  "Reset PATHS in index/working tree"
  (let ((cmd (list "git" "reset")))
    (when soft (push "--soft" cmd))
    (when mixed (push "--mixed" cmd))
    (when hard (push "--hard" cmd))
    (uiop:run-program (append (nreverse cmd) paths))))

(defmethod version-control-checkout ((backend (eql :git)) target &key create-branch)
  "Checkout TARGET branch or commit"
  (let ((cmd (list "git" "checkout")))
    (when create-branch (push "-b" cmd))
    (uiop:run-program (append (nreverse cmd) (list target)))))

(defmethod version-control-push ((backend (eql :git)) remote branch &key force-with-lease)
  "Push BRANCH to REMOTE"
  (let ((cmd (list "git" "push" remote branch)))
    (when force-with-lease (append cmd (list "--force-with-lease")))
    (uiop:run-program cmd)))

(defmethod version-control-pull ((backend (eql :git)) remote branch &key rebase)
  "Pull BRANCH from REMOTE with optional REBASE"
  (let ((cmd (list "git" "pull" remote branch)))
    (when rebase (append cmd (list "--rebase")))
    (uiop:run-program cmd)))

(defmethod version-control-fetch ((backend (eql :git)) &key remote all tags prune)
  "Fetch from REMOTE(s)"
  (let ((cmd (list "git" "fetch")))
    (when all (append cmd (list "--all")))
    (when remote (append cmd (list remote)))
    (when tags (append cmd (list "--tags")))
    (when prune (append cmd (list "--prune")))
    (uiop:run-program (nreverse cmd))))

(defmethod version-control-log ((backend (eql :git)) path &key limit since until author)
  "Return commit log for PATH (or project if nil)"
  (let ((base-path (or path (getf (version-control-config backend) :repo-path))))
    (let ((cmd (list "git" "-C" base-path "log" "--oneline")))
      (when limit (append cmd (list "-n" (write-to-string limit))))
      (when since (append cmd (list "--since" (write-to-string since))))
      (when until (append cmd (list "--until" (write-to-string until))))
      (when author (append cmd (list "--author" (write-to-string author))))
      (let ((output (ignore-errors (uiop:run-program cmd :output :string))))
        (when output (split-sequence #\newline output))))))

(defmethod version-control-diff ((backend (eql :git)) path &key cached name-only)
  "Return diff for PATH or staged if CACHED"
  (let ((cmd (list "git" "diff")))
    (when cached (push "--cached" cmd))
    (when name-only (push "--name-only" cmd))
    (when path (append cmd (list path)))
    (ignore-errors (uiop:run-program (nreverse cmd) :output :string))))

(defmethod version-control-difftool ((backend (eql :git)) path &key base target (tool "meld"))
  "Launch external diff tool for PATH or staged if BASE/TARGET"
  (let ((cmd (list "git" "difftool" "--tool" tool)))
    (when base (append cmd (list base)))
    (when target (append cmd (list target)))
    (when path (append cmd (list path)))
    (uiop:run-program (nreverse cmd) :output nil :ignore-errors t)))

(defmethod version-control-branch ((backend (eql :git)) &key list all create delete rename move)
  "Branch operations: list, create, delete, rename, or move"
  (let ((cmd (list "git" "branch")))
    (cond
      (list (push (if all "-a" "--list") cmd))
      (create (append cmd (list create)))
      (delete (append cmd (list "-d" delete)))
      (rename (append cmd (list "-m" rename move)))
      (move (append cmd (list "-m" move))))
    (let ((output (ignore-errors (uiop:run-program (nreverse cmd) :output :string))))
      (when output
        (mapcar 'string-trim (split-sequence #\newline output :remove-empty-subseqs t))))))

(defmethod version-control-merge ((backend (eql :git)) source &key no-ff fast-forward)
  "Merge SOURCE into current branch"
  (let ((cmd (list "git" "merge")))
    (when no-ff (push "--no-ff" cmd))
    (when (not fast-forward) (push "--ff-only" cmd))
    (append cmd (list source))
    (uiop:run-program (nreverse cmd))))

(defmethod version-control-rebase ((backend (eql :git)) target &key interactive)
  "Rebase current branch onto TARGET"
  (let ((cmd (list "git" "rebase")))
    (when interactive (push "-i" cmd))
    (append cmd (list target))
    (uiop:run-program (nreverse cmd))))

(defmethod version-control-stash ((backend (eql :git)) action &rest args)
  "Manage stashes: push/pop/list/drop"
  (let ((cmd (list "git" "stash")))
    (ecase action
      (:push (push "push" cmd))
      (:pop (push "pop" cmd))
      (:list (push "list" cmd))
      (:drop (push "drop" cmd))
      (:apply (push "apply" cmd)))
    (append cmd args)
    (uiop:run-program cmd :output :string :ignore-errors t)))

(defmethod version-control-tag ((backend (eql :git)) &key list create delete annotate)
  "Manage tags: list/create/delete/annotate"
  (let ((cmd (list "git" "tag")))
    (cond
      (list (push "-l" cmd))
      (create (append cmd (list create))))
    (uiop:run-program (nreverse cmd) :output :string :ignore-errors t)))

(defmethod version-control-config-get ((backend (eql :git)) key &key global local)
  "Get config KEY value (global or local)"
  (let ((cmd (list "git" "config")))
    (when global (push "--global" cmd))
    (when local (push "--local" cmd))
    (push key cmd)
    (ignore-errors (uiop:run-program (nreverse cmd) :output :string))))

(defmethod version-control-config-set ((backend (eql :git)) key value &key global local)
  "Set config KEY to VALUE (global or local)"
  (let ((cmd (list "git" "config")))
    (when global (push "--global" cmd))
    (when local (push "--local" cmd))
    (push key cmd)
    (push value cmd)
    (uiop:run-program (nreverse cmd) :output nil :ignore-errors t)))

(defmethod version-control-user-name ((backend (eql :git)) &key global)
  "Get user name from config"
  (version-control-config-get backend "user.name" :global global))

(defmethod version-control-user-email ((backend (eql :git)) &key global)
  "Get user email from config"
  (version-control-config-get backend "user.email" :global global))

(defmethod version-control-set-user ((backend (eql :git)) name email &key global)
  "Set user name and email in config"
  (version-control-config-set backend "user.name" name :global global)
  (version-control-config-set backend "user.email" email :global global))

(defmethod version-control-remote-add ((backend (eql :git)) name url)
  "Add remote named NAME with URL"
  (uiop:run-program (list "git" "remote" "add" name url)))

(defmethod version-control-remote-list ((backend (eql :git)))
  "List configured remotes"
  (let ((output (ignore-errors (uiop:run-program (list "git" "remote") :output :string))))
    (when output
      (split-sequence #\newline output :remove-empty-subseqs t))))

(defmethod version-control-submodule-add ((backend (eql :git)) url path &key branch)
  "Add submodule from URL at PATH with optional BRANCH"
  (let ((cmd (list "git" "submodule" "add" url path)))
    (when branch (append cmd (list "-b" branch)))
    (uiop:run-program (nreverse cmd))))

(defmethod version-control-submodule-update ((backend (eql :git)) &key init recursive remote)
  "Update submodules with options"
  (let ((cmd (list "git" "submodule" "update")))
    (when init (push "--init" cmd))
    (when recursive (push "--recursive" cmd))
    (when remote (push "--remote" cmd))
    (uiop:run-program (nreverse cmd))))

(defmethod version-control-submodule-status ((backend (eql :git)))
  "Return submodule status"
  (ignore-errors (uiop:run-program (list "git" "submodule" "status") :output :string)))

(defun make-git-backend (&optional (path (uiop:getcwd)))
  "Create a Git backend instance for PATH"
  (declare (ignore path))
  'git-backend)
