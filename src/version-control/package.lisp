;;; src/version-control/package.lisp
;;; Package definition for Skyline-Tool version control
;;; Properly exported symbols with version-control-* naming

(defpackage :skyline-tool.version-control
  (:use :cl :alexandria :serapeum)
  (:import-from :uiop
                #:run-program
                #:directory-exists-p)
  (:export
   ;; Backend protocol symbols
   :version-control-backend
   :version-control-name
   :version-control-version
   :version-control-available-p
   :version-control-init
   :version-control-clone
   :version-control-status
   :version-control-add
   :version-control-commit
   :version-control-reset
   :version-control-checkout
   :version-control-push
   :version-control-pull
   :version-control-fetch
   :version-control-log
   :version-control-diff
   :version-control-difftool
   :version-control-branch
   :version-control-merge
   :version-control-rebase
   :version-control-stash
   :version-control-tag
   :version-control-config-get
   :version-control-config-set
   :version-control-user-name
   :version-control-user-email
   :version-control-set-user
   :version-control-remote-add
   :version-control-remote-list
   :version-control-submodule-add
   :version-control-submodule-update
   :version-control-submodule-status
   
   ;; Filesystem monitoring
   :start-fs-monitor
   :stop-fs-monitor
   :with-fs-monitor
   :version-control-status-changed-p
   
   ;; Configuration interface
   :version-control-config-pathname
   :load-version-control-config
   :save-version-control-config
   :ensure-version-control-config
   :with-version-control-auto-save
   
   ;; Commit utilities
   :version-control-get-last-commit
   :version-control-set-last-commit
   :version-control-get-staged-files
   :version-control-set-staged-files
   :version-control-get-ignored-files
   :version-control-set-ignored-files
   :version-control-get-tracked-files
   :version-control-set-tracked-files
   :version-control-get-branch
   :version-control-set-branch
   :version-control-get-ahead-count
   :version-control-set-ahead-count
   :version-control-get-behind-count
   :version-control-set-behind-count
   
   ;; Status presentation
   :version-control-status-for-file
   :version-control-status-icon
   :version-control-status-text
   :version-control-file-status
   :draw-version-control-status-icon
   :draw-version-control-status-bar
   :present-version-control-status-icon
   :present-version-control-status-bar
   :version-control-menu-commands
   :version-control-get-tracked-status
   :version-control-set-tracked-status
   :version-control-get-ignored-status
   :version-control-set-ignored-status
   
   ;; Backend implementations
   :make-git-backend
   :make-svn-backend
   :make-bazaar-backend
   :make-cvs-backend
   :make-rcs-backend
   :make-mercurial-backend
   
   ;; Error condition
   :version-control-error
   :version-control-error-backend
   :version-control-error-message))

(in-package :skyline-tool.version-control)

;;; Error conditions

(define-condition version-control-error (error)
  ((backend :initarg :backend :reader version-control-error-backend)
   (message :initarg :message :reader version-control-error-message))
  (:report (lambda (c s)
             (format s "Version Control Error (~a): ~a"
                     (version-control-error-backend c)
                     (version-control-error-message c)))))

;;; Backend base class (if needed for common functionality)

(defclass version-control-backend ()
  ((name :reader version-control-name :initarg :name)
   (version :reader version-control-version :initarg :version :initform "unknown")))

;;; Generic functions for version control operations

(defgeneric version-control-backend (backend &key)
  (:documentation "Return the backend identifier for the given backend."))

(defgeneric version-control-name (backend)
  (:documentation "Return the name of the version control system."))

(defgeneric version-control-version (backend)
  (:documentation "Return the version string of the version control system."))

(defgeneric version-control-available-p (backend)
  (:documentation "Return T if the backend executable is available on the system."))

(defgeneric version-control-init (backend path &key)
  (:documentation "Initialize a new repository at PATH. Returns a new backend instance."))

(defgeneric version-control-clone (backend url path &key)
  (:documentation "Clone repository from URL to PATH. Returns a new backend instance."))

(defgeneric version-control-status (backend path &key)
  (:documentation "Return status of working tree as a plist."))

(defgeneric version-control-add (backend paths &key)
  (:documentation "Stage PATHS for commit."))

(defgeneric version-control-commit (backend message &key)
  (:documentation "Commit staged changes with MESSAGE."))

(defgeneric version-control-reset (backend paths &key)
  (:documentation "Reset PATHS in index/working tree."))

(defgeneric version-control-checkout (backend target &key)
  (:documentation "Checkout TARGET branch or commit."))

(defgeneric version-control-push (backend remote branch &key)
  (:documentation "Push BRANCH to REMOTE."))

(defgeneric version-control-pull (backend remote branch &key)
  (:documentation "Pull BRANCH from REMOTE."))

(defgeneric version-control-fetch (backend &key)
  (:documentation "Fetch from REMOTE(s)."))

(defgeneric version-control-log (backend path &key)
  (:documentation "Return commit log for PATH (or project if nil)."))

(defgeneric version-control-diff (backend path &key)
  (:documentation "Return diff for PATH or staged if CACHED."))

(defgeneric version-control-difftool (backend path &key)
  (:documentation "Launch external diff tool for PATH or staged."))

(defgeneric version-control-branch (backend &key)
  (:documentation "Branch operations: list, create, delete, rename."))

(defgeneric version-control-merge (backend source &key)
  (:documentation "Merge SOURCE into current branch."))

(defgeneric version-control-rebase (backend target &key)
  (:documentation "Rebase current branch onto TARGET."))

(defgeneric version-control-stash (backend action &rest args)
  (:documentation "Manage stashes: push/pop/list/drop."))

(defgeneric version-control-tag (backend &key)
  (:documentation "Manage tags: list/create/delete/annotate."))

(defgeneric version-control-config-get (backend key &key)
  (:documentation "Get config KEY value."))

(defgeneric version-control-config-set (backend key value &key)
  (:documentation "Set config KEY to VALUE."))

(defgeneric version-control-user-name (backend &key)
  (:documentation "Get user name from config."))

(defgeneric version-control-user-email (backend &key)
  (:documentation "Get user email from config."))

(defgeneric version-control-set-user (backend name email &key)
  (:documentation "Set user name and email in config."))

(defgeneric version-control-remote-add (backend name url)
  (:documentation "Add remote named NAME with URL."))

(defgeneric version-control-remote-list (backend)
  (:documentation "List configured remotes."))

(defgeneric version-control-submodule-add (backend url path &key)
  (:documentation "Add submodule from URL at PATH."))

(defgeneric version-control-submodule-update (backend &key)
  (:documentation "Update submodules with options."))

(defgeneric version-control-submodule-status (backend)
  (:documentation "Return submodule status list."))

;;; Backend implementations

(defmethod version-control-backend ((backend (eql :git)) &key)
  :git)

(defmethod version-control-backend ((backend (eql :svn)) &key)
  :svn)

(defmethod version-control-backend ((backend (eql :bazaar)) &key)
  :bazaar)

(defmethod version-control-backend ((backend (eql :cvs)) &key)
  :cvs)

(defmethod version-control-backend ((backend (eql :rcs)) &key)
  :rcs)

(defmethod version-control-backend ((backend (eql :mercurial)) &key)
  :mercurial)

;;; Factory functions

(defun make-git-backend (&optional (path (uiop:getcwd)))
  "Create a Git backend instance for PATH"
  (declare (ignore path))
  'git)

(defun make-svn-backend (path)
  "Create an SVN backend instance"
  (declare (ignore path))
  'svn)

(defun make-bazaar-backend (path)
  "Create a Bazaar backend instance"
  (declare (ignore path))
  'bazaar)

(defun make-cvs-backend (path)
  "Create a CVS backend instance"
  (declare (ignore path))
  'cvs)

(defun make-rcs-backend (path)
  "Create an RCS backend instance"
  (declare (ignore path))
  'rcs)

(defun make-mercurial-backend (path)
  "Create a Mercurial backend instance"
  (declare (ignore path))
  'mercurial)