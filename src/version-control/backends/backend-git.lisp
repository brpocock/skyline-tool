;;; src/version-control/backends/git.lisp
;;; Git backend implementation for Skyline-Tool version control

(in-package :skyline-tool.version-control)

;;
;; Git Backend Classes
;;

(defclass git-backend (vc-backend)
  ((repo-path :initarg :repo-path :accessor repo-path)
   (branch :initform "main" :accessor git-branch)))

(defun make-git-backend (&optional (path (uiop:getcwd)))
  "Create a Git backend instance for PATH"
  (make-instance 'git-backend :repo-path path))

;;
;; Git Backend Protocol Implementations
;;

(defmethod vc-name ((backend git-backend)) "git")

(defmethod vc-version ((backend git-backend))
  (or (ignore-errors 
       (uiop:run-program (list "git" "--version") :output :string)) 
      "unknown"))

(defmethod vc-available-p ((backend git-backend))
  (ignore-errors 
   (zerop (uiop:run-program (list "git" "--version") :output nil))))

(defmethod vc-init ((backend git-backend) path &key)
  (when (probe-file path)
    (uiop:run-program (list "git" "init" path)))
  (make-instance 'git-backend :repo-path path))

(defmethod vc-clone ((backend git-backend) url path &key)
  (uiop:run-program (list "git" "clone" url path))
  (make-instance 'git-backend :repo-path path))

(defmethod vc-status ((backend git-backend) path &key)
  (let ((output (ignore-errors 
                 (uiop:run-program (append (list "git" "-C" path "status" "--porcelain")
                                           (when (probe-file (merge-pathnames ".git/index" path))
                                             (list "--untracked-files=no")))
                                   :output :string))))
    (parse-git-status output)))

(defun parse-git-status (output)
  "Parse git status --porcelain output into status plist"
  (let ((staged '()) (modified '()) (untracked '()))
    (dolist (line (split-sequence #\newline output))
      (when (plusp (length line))
        (let ((code (subseq line 0 2)))
          (case (intern (string-upcase code) :keyword)
            (:A (push :staged staged))
            (:M (push :modified modified))
            (:?? (push :untracked untracked))))))
    (append (when staged '(:staged . t))
            (when modified '(:modified . t))
            (when untracked '(:untracked . t)))))

(defmethod vc-add ((backend git-backend) paths &key)
  (uiop:run-program (append (list "git" "-C" (repo-path backend) "add") paths)))


;; Enhanced VC commit implementation using secure temporary file and emacsclient
(defmethod vc-commit ((backend git-backend) message &key amend signoff author)
  ;; Handle case where no message is provided (trigger interactive editor)
  (cond
    (message
     ;; Direct commit with provided message
     (let ((cmd (list "git" "-C" (repo-path backend) "commit" "-m" message)))
       (when amend (push "--amend" cmd))
       (when signoff (push "--signoff" cmd))
       (when author (append cmd (list "--author" author)))
       (uiop:run-program cmd)))

    ;; No message provided: use emacsclient with secure temporary file
    (t
     (cl-fad:with-open-temporary-file (temp-file)
       (wait-for-emacs temp-file)
       
       ;; Launch emacsclient editor on temp file
       (lambda ()
         (format t "Launching emacsclient to compose commit message...~%")
         (run-program "emacsclient" "-t"
                      (namestring (truename temp-file)))
         
         ;; Wait for file modification to complete
         (let ((initial-size 0))
           (loop while (< (file-size temp-file) (+ initial-size 1000))
                 do (sleep 0.5)
                 do (setf initial-size (file-size temp-file))
                 when (>= initial-size 1000)
                   do (return)
                 finally (when (>= initial-size 3000) (return)))) ; arbitrary threshold
         
         ;; Read final commit message
         (let ((final-message (read-file-into-string temp-file)))
           (when (emptyp final-message)
             (error "Commit message composition cancelled"))
           
           ;; Perform actual commit with the composed message
           (let ((commit-cmd (list "git" "-C" (repo-path backend) "commit" "-m" final-message)))
             (when amend (push "--amend" commit-cmd))
             (when signoff (push "--signoff" commit-cmd))
             (when author (append commit-cmd (list "--author" author)))
             (uiop:run-program commit-cmd))))))))

(defmethod vc-reset ((backend git-backend) paths &key soft mixed hard)
  (let ((cmd (list "git" "-C" (repo-path backend) "checkout" "--")))
    (when hard (push "--hard" cmd))
    (uiop:run-program (append cmd paths))))

(defmethod vc-checkout ((backend git-backend) target &key create-branch)
  (let ((cmd (list "git" "-C" (repo-path backend) "checkout")))
    (when create-branch (push "-b" cmd))
    (uiop:run-program (append cmd (list target)))))

(defmethod vc-push ((backend git-backend) remote branch &key force-with-lease)
  (let ((cmd (list "git" "-C" (repo-path backend) "push" remote branch)))
    (when force-with-lease (push "--force-with-lease" cmd))
    (uiop:run-program cmd)))

(defmethod vc-pull ((backend git-backend) remote branch &key rebase)
  (let ((cmd (list "git" "-C" (repo-path backend) "pull" remote branch)))
    (when rebase (push "--rebase" cmd))
    (uiop:run-program cmd)))

(defmethod vc-fetch ((backend git-backend) &key remote all tags prune)
  (let ((cmd (list "git" "-C" (repo-path backend) "fetch")))
    (when all (push "--all" cmd))
    (when remote (push remote cmd))
    (when prune (push "--prune" cmd))
    (uiop:run-program cmd)))

(defmethod vc-log ((backend git-backend) path &key limit since until author)
  (let ((cmd (list "git" "-C" (or path (repo-path backend)) "log" "--oneline")))
    (when limit (append cmd (list "-n" (format nil "~a" limit))))
    (when since (append cmd (list "--since" since)))
    (when until (append cmd (list "--until" until)))
    (when author (append cmd (list "--author" author)))
    (let ((output (ignore-errors (uiop:run-program cmd :output :string))))
      (split-sequence #\newline output))))

(defmethod vc-diff ((backend git-backend) path &key cached name-only)
  (let ((cmd (list "git" "-C" (repo-path backend) "diff")))
    (when cached (push "--cached" cmd))
    (when name-only (push "--name-only" cmd))
    (ignore-errors (uiop:run-program cmd :output :string))))

(defmethod vc-difftool ((backend git-backend) path &key base target (tool "meld"))
  (let ((cmd (list "git" "-C" (repo-path backend) "difftool")))
    (when base (push base cmd))
    (when target (push target cmd))
    (when (string= tool "meld") (push "--tool" cmd))
    (uiop:run-program (append cmd (list path)) :output nil :ignore-errors t)))

(defmethod vc-branch ((backend git-backend) &key list all create delete rename move)
  (let ((cmd (list "git" "-C" (repo-path backend) "branch")))
    (cond
      (list (push (if all "-a" "--list") cmd))
      (create (push create cmd))
      (delete (push "-d" cmd) (push delete cmd))
      (rename (push "-m" cmd) (push rename cmd) (push move cmd))
      (move (push "-m" cmd) (push move cmd)))
    (let ((output (ignore-errors (uiop:run-program cmd :output :string))))
      (when output
        (mapcar (curry #'string-trim skyline-tool::+whitespace+)
                (split-sequence #\newline output :remove-empty-subseqs t))))))

(defmethod vc-checkout ((backend git-backend) target &key create-branch)
  (let ((cmd (list "git" "-C" (repo-path backend) "checkout")))
    (when create-branch (push "-b" cmd))
    (push target cmd)
    (uiop:run-program cmd :output nil :ignore-errors t)))

(defmethod vc-merge ((backend git-backend) source &key no-ff fast-forward)
  (let ((cmd (list "git" "-C" (repo-path backend) "merge")))
    (when no-ff (push "--no-ff" cmd))
    (when (not fast-forward) (push "--ff-only" cmd))
    (push source cmd)
    (uiop:run-program cmd :output nil :ignore-errors t)))

(defmethod vc-rebase ((backend git-backend) target &key interactive)
  (let ((cmd (list "git" "-C" (repo-path backend) "rebase")))
    (when interactive (push "-i" cmd))
    (push target cmd)
    (uiop:run-program cmd :output nil :ignore-errors t)))

(defmethod vc-push ((backend git-backend) remote branch &key force-with-lease)
  (let ((cmd (list "git" "-C" (repo-path backend) "push")))
    (when force-with-lease (push "--force-with-lease" cmd))
    (push remote cmd)
    (when branch (push branch cmd))
    (uiop:run-program cmd :output nil :ignore-errors t)))

(defmethod vc-pull ((backend git-backend) remote branch &key rebase)
  (let ((cmd (list "git" "-C" (repo-path backend) "pull")))
    (when rebase (push "--rebase" cmd))
    (push remote cmd)
    (when branch (push branch cmd))
    (uiop:run-program cmd :output nil :ignore-errors t)))

(defmethod vc-fetch ((backend git-backend) &key remote all tags prune)
  (let ((cmd (list "git" "-C" (repo-path backend) "fetch")))
    (when remote (push remote cmd))
    (when all (push "--all" cmd))
    (when tags (push "--tags" cmd))
    (when prune (push "--prune" cmd))
    (uiop:run-program cmd :output nil :ignore-errors t)))

(defmethod vc-remote-add ((backend git-backend) name url)
  (uiop:run-program (list "git" "-C" (repo-path backend) "remote" "add" name url)
                    :output nil :ignore-errors t))

(defmethod vc-remote-list ((backend git-backend))
  (let ((output (ignore-errors (uiop:run-program (list "git" "-C" (repo-path backend) "remote")
                                                 :output :string))))
    (when output
      (split-sequence #\newline output :remove-empty-subseqs t))))

(defmethod vc-submodule-add ((backend git-backend) url path &key branch)
  (let ((cmd (list "git" "-C" (repo-path backend) "submodule" "add" url path)))
    (when branch (push "-b" cmd) (push branch cmd))
    (uiop:run-program cmd :output nil :ignore-errors t)))

(defmethod vc-submodule-update ((backend git-backend) &key init recursive remote)
  (let ((cmd (list "git" "-C" (repo-path backend) "submodule" "update")))
    (when init (push "--init" cmd))
    (when recursive (push "--recursive" cmd))
    (when remote (push "--remote" cmd))
    (uiop:run-program cmd :output nil :ignore-errors t)))

(defmethod vc-submodule-status ((backend git-backend))
  (ignore-errors (uiop:run-program (list "git" "-C" (repo-path backend) "submodule" "status")
                                   :output :string)))

(defmethod vc-stash ((backend git-backend) action &rest args)
  (let ((cmd (list "git" "-C" (repo-path backend) "stash")))
    (ecase action
      (:push (push "push" cmd))
      (:pop (push "pop" cmd))
      (:list (push "list" cmd))
      (:drop (push "drop" cmd))
      (:apply (push "apply" cmd)))
    (append cmd args)
    (uiop:run-program cmd :output :string :ignore-errors t)))

(defmethod vc-tag ((backend git-backend) &key list create delete annotate)
  (let ((cmd (list "git" "-C" (repo-path backend) "tag")))
    (cond
      (list (push "-l" cmd))
      (create (push create cmd))
      (delete (push "-d" cmd) (push delete cmd))
      (annotate (push "-a" cmd) (push annotate cmd)))
    (uiop:run-program cmd :output :string :ignore-errors t)))

(defmethod vc-config-get ((backend git-backend) key &key global local)
  (let ((cmd (list "git" "-C" (repo-path backend) "config")))
    (when global (push "--global" cmd))
    (when local (push "--local" cmd))
    (push key cmd)
    (ignore-errors (uiop:run-program cmd :output :string))))

(defmethod vc-config-set ((backend git-backend) key value &key global local)
  (let ((cmd (list "git" "-C" (repo-path backend) "config")))
    (when global (push "--global" cmd))
    (when local (push "--local" cmd))
    (push key cmd)
    (push value cmd)
    (uiop:run-program cmd :output nil :ignore-errors t)))

(defmethod vc-user-name ((backend git-backend) &key global)
  (vc-config-get backend "user.name" :global global))

(defmethod vc-user-email ((backend git-backend) &key global)
  (vc-config-get backend "user.email" :global global))

(defmethod vc-set-user ((backend git-backend) name email &key global)
  (vc-config-set backend "user.name" name :global global)
  (vc-config-set backend "user.email" email :global global))
