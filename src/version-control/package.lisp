;;; src/version-control/package.lisp
;;; Package definition for Skyline-Tool version control

(defpackage :skyline-tool.version-control
  (:use :cl :alexandria :serapeum)
  (:import-from :uiop
                #:run-program
                #:directory-exists-p)
  (:export
   ;; Backend protocol
   #:vc-name
   #:vc-version
   #:vc-available-p
   #:vc-init
   #:vc-clone
   #:vc-status
   #:vc-add
   #:vc-commit
   #:vc-reset
   #:vc-log
   #:vc-diff
   #:vc-difftool
   #:vc-branch
   #:vc-checkout
   #:vc-merge
   #:vc-rebase
   #:vc-push
   #:vc-pull
   #:vc-fetch
   #:vc-remote-add
   #:vc-remote-list
   #:vc-submodule-add
   #:vc-submodule-update
   #:vc-submodule-status
   #:vc-stash
   #:vc-tag
   #:vc-config-get
   #:vc-config-set
   #:vc-user-name
   #:vc-user-email
   #:vc-set-user
   #:detect-vc-backend
   #:list-available-backends
   ;; Filesystem monitoring
   #:start-fs-monitor
   #:stop-fs-monitor
   #:with-fs-monitor
   #:vc-status-changed-p
   ;; Configuration interface (DRY - single pathname definition)
   #:vc-config-pathname
   #:load-vc-config
   #:save-vc-config
   #:ensure-vc-config
   #:with-vc-auto-save
   #:vc-get-last-commit
   #:vc-set-last-commit
   #:vc-get-staged-files
   #:vc-set-staged-files
   #:vc-get-ignored-files
   #:vc-set-ignored-files
   #:vc-get-tracked-files
   #:vc-set-tracked-files
   #:vc-get-branch
   #:vc-set-branch
   #:vc-get-ahead-count
   #:vc-set-ahead-count
   #:vc-get-behind-count
   #:vc-set-behind-count
;; Status presentation utilities
    #:vc-status-icon
    #:vc-status-text
    #:vc-file-status
    #:draw-vc-status-icon
    #:draw-vc-status-bar
    #:present-vc-status-icon
    #:present-vc-status-bar
    #:vc-menu-commands
    #:vc-get-tracked-status
    #:vc-set-tracked-status
    #:vc-get-ignored-status
    #:vc-set-ignored-status
    ;; Backend implementations
    #:make-git-backend
    #:make-svn-backend))

(in-package :skyline-tool.version-control)

;;; Protocol definition
(define-condition version-control-error (error)
  ((backend :initarg :backend :reader vc-error-backend)
   (message :initarg :message :reader vc-error-message))
  (:report (lambda (c s)
             (format s "VC Error (~a): ~a" (vc-error-backend c) (vc-error-message c)))))

;; Backend base class
(defclass vc-backend ()
  ((name :reader vc-name :initarg :name)
   (version :reader vc-version :initarg :version :initform "unknown")))

;; Generic functions for version control operations
(defgeneric vc-available-p (backend)
  (:documentation "Return T if the backend executable is available on the system."))

(defgeneric vc-init (backend path &key)
  (:documentation "Initialize a new repository at PATH. Returns a new backend instance."))

(defgeneric vc-clone (backend url path &key)
  (:documentation "Clone repository from URL to PATH. Returns a new backend instance."))

(defgeneric vc-status (backend path &key)
  (:documentation "Return status of working tree as a plist with :staged, :modified, :untracked keys."))

(defgeneric vc-add (backend paths &key)
  (:documentation "Stage PATHS for commit."))

(defgeneric vc-commit (backend message &key)
  (:documentation "Commit staged changes with MESSAGE."))

(defgeneric vc-reset (backend paths &key soft mixed hard)
  (:documentation "Reset PATHS in index/working tree."))

(defgeneric vc-log (backend path &key limit since until author)
  (:documentation "Return commit log for PATH (or project if nil)."))

(defgeneric vc-diff (backend path &key cached name-only)
  (:documentation "Return diff for PATH or staged if CACHED."))

(defgeneric vc-branch (backend &key list all create delete rename move)
  (:documentation "Manage branches: list/create/delete/rename/move."))

(defgeneric vc-checkout (backend target &key create-branch)
  (:documentation "Checkout TARGET branch or commit."))

(defgeneric vc-merge (backend source &key no-ff fast-forward)
  (:documentation "Merge SOURCE into current branch."))

(defgeneric vc-rebase (backend target &key interactive)
  (:documentation "Rebase current branch onto TARGET."))

(defgeneric vc-push (backend remote branch &key force-with-lease)
  (:documentation "Push BRANCH to REMOTE."))

(defgeneric vc-pull (backend remote branch &key rebase)
  (:documentation "Pull BRANCH from REMOTE with optional REBASE."))

(defgeneric vc-fetch (backend &key remote all tags prune)
  (:documentation "Fetch from REMOTE(s)."))

(defgeneric vc-remote-add (backend name url)
  (:documentation "Add remote named NAME with URL."))

(defgeneric vc-remote-list (backend)
  (:documentation "List configured remotes."))

(defgeneric vc-submodule-add (backend url path &key branch)
  (:documentation "Add submodule from URL at PATH with optional BRANCH."))

(defgeneric vc-submodule-update (backend &key init recursive remote)
  (:documentation "Update submodules with options."))

(defgeneric vc-submodule-status (backend)
  (:documentation "Return submodule status list."))

(defgeneric vc-stash (backend action &rest args)
  (:documentation "Manage stashes: push/pop/list/drop."))

(defgeneric vc-tag (backend &key list create delete annotate)
  (:documentation "Manage tags: list/create/delete/annotate."))

(defgeneric vc-config-get (backend key &key global local)
  (:documentation "Get config KEY value (global or local)."))

(defgeneric vc-config-set (backend key value &key global local)
  (:documentation "Set config KEY to VALUE (global or local)."))

(defgeneric vc-user-name (backend &key global)
  (:documentation "Get user name from config."))

(defgeneric vc-user-email (backend &key global)
  (:documentation "Get user email from config."))

(defgeneric vc-set-user (backend name email &key global)
  (:documentation "Set user name and email in config."))

;; Backend detection
(defgeneric detect-vc-backend (path)
  (:documentation "Detect version control backend from directory PATH. Returns :git, :svn, or NIL."))

(defgeneric list-available-backends ()
  (:documentation "List all version control backends available on the system."))

;; Filesystem monitoring
(defgeneric start-fs-monitor (backend path callback)
  (:documentation "Start monitoring PATH for changes. CALLBACK is a function of (event-type path)."))

(defgeneric stop-fs-monitor (monitor)
  (:documentation "Stop filesystem monitoring."))

(defmacro with-fs-monitor ((monitor-var backend path callback) &body body)
  "Execute BODY with filesystem monitoring active."
  `(let ((,monitor-var (start-fs-monitor ,backend ,path ,callback)))
     (unwind-protect (progn ,@body)
       (when ,monitor-var (stop-fs-monitor ,monitor-var)))))

(defgeneric vc-status-changed-p (backend path)
  (:documentation "Check if working tree status has changed since last check."))

;; Diff tool for external diff programs
(defgeneric vc-difftool (backend path &key base target tool)
  (:documentation "Launch external diff tool for PATH or staged if BASE/TARGET.
Optional KEYWORDS: BASE (base branch), TARGET (target branch),
and TOOL (tool name, defaults to \"meld\")."))