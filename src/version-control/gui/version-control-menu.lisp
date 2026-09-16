;;; src/version-control/gui/version-control-menu.lisp
;;; Version Control menu commands for resource presentations


;;
;; VC Menu Commands for Resource Presentations
;;

(defun version-control-resource-menu-commands (resource)
  "Return a list of VC menu commands for RESOURCE.
   Each command is a cons of (label . function).
   Used in resource context menus (BLOB | Edit | View | Help -> Version Control > ...)"
  (let* ((file-path (or (game-resource-full-path resource)
                        (game-resource-collective-path resource)))
         (status (when file-path (version-control-file-status file-path)))
         (tracked (when file-path (version-control-get-tracked-status file-path)))
         (ignored (when file-path (version-control-get-ignored-status file-path))))
    (list 
     (cons "Version Control" 
           (list 
            ;; Staged toggle
            (cons (if (eq status :staged) "☑ Staged" "☐ Staged")
                  (lambda () 
                    (when file-path
                      (case status
                        (:staged (version-control-reset (make-git-backend) (list file-path)))
                        ((:modified :untracked) 
                         (version-control-add (make-git-backend) (list file-path))
                         (version-control-commit (make-git-backend) "Staged via menu"))
                        (t (format t "Cannot stage ~a in status ~a~%" file-path status))))))
            (cons "" nil)
            ;; Revert
            (cons "Revert..." 
                  (lambda () 
                    (when file-path
                      (show-version-control-revert-dialog file-path))))
            ;; Compare
            (cons "Compare..." 
                  (lambda () 
                    (when file-path
                      (show-version-control-compare-dialog file-path))))
            ;; Commit
            (cons "Commit..." 
                  (lambda () 
                    (when file-path
                      (show-version-control-commit-dialog file-path))))
            (cons "" nil)
            ;; Tracked toggle
            (cons (if tracked "☑ Tracked" "☐ Tracked")
                  (lambda () 
                    (when file-path
                      (if tracked
                          (confirm-dialog 
                           (format nil "Do you want to remove ~a from Version Control?~%[ Leave It ] [ Remove It ]"
                                   file-path)
                           (lambda () (version-control-set-tracked-status file-path nil))
                           :default-action :leave
                           :danger-action :remove)
                          (version-control-set-tracked-status file-path t)))))
            ;; Ignored toggle
            (cons (if ignored "☐ Ignored" "☐ Ignored")
                  (lambda () 
                    (when (and file-path (eq status :untracked) (not tracked))
                      (version-control-set-ignored-status file-path (not ignored)))
                    (when (and file-path (not (eq status :untracked)))
                      (format t "Can only ignore untracked, untracked files~%")))))))))

;;
;; Dialog Functions
;;

(defun show-version-control-revert-dialog (file-path)
  "Show dialog to revert FILE-PATH to a previous revision"
  (let* ((git (make-git-backend))
         (log (version-control-log git file-path :limit 20)))
    (when log
      (clim:with-output-as-presentation
          (stream (make-version-control-revert-dialog log file-path) 'version-control-revert-dialog)
        (format stream "Select revision to revert ~a to:~%" file-path)
        (dolist (commit log)
          (let ((short-hash (subseq commit 0 8))
                (msg (subseq commit 8)))
            (clim:with-output-as-presentation
                (stream (cons short-hash file-path) 'version-control-revert-target)
              (format stream "  ~a ~a~%" short-hash msg))))))))

(defun show-version-control-compare-dialog (file-path)
  "Show dialog to compare FILE-PATH with a revision"
  (let* ((git (make-git-backend))
         (log (version-control-log git file-path :limit 20)))
    (when log
      (clim:with-output-as-presentation
          (stream (make-version-control-compare-dialog log file-path) 'version-control-compare-dialog)
        (format stream "Select revision to compare ~a with:~%" file-path)
        (dolist (commit log)
          (let ((short-hash (subseq commit 0 8))
                (msg (subseq commit 8))
                (affects-file (version-control-commit-affects-file git commit file-path)))
            (clim:with-drawing-options 
                (stream :ink (if affects-file clim:+black+ clim:+gray+))
              (clim:with-output-as-presentation
                  (stream (cons short-hash file-path) 'version-control-compare-target)
                (format stream "  ~a ~a~%" short-hash msg))
              (when affects-file
                (format stream "    [Difftool] ")))))))))

(defun show-version-control-commit-dialog (file-path)
  "Show dialog to commit staged changes with a message"
  (let ((message (run-text-input-dialog (format nil "Commit message for ~a:" file-path)
                                         :initial-value "Update"
                                         :title "Commit")))
    (when message
      (let ((git (make-git-backend)))
        (version-control-add git (list file-path))
        (version-control-commit git message)))))

;;
;; Helper Functions
;;

(defun version-control-get-tracked-status (file-path)
  (member file-path (version-control-get-tracked-files) :test #'equal))

(defun version-control-set-tracked-status (file-path tracked)
  (let ((files (version-control-get-tracked-files)))
    (if tracked
        (unless (member file-path files :test #'equal)
          (version-control-set-tracked-files (append files (list file-path))))
        (version-control-set-tracked-files (remove file-path files :test #'equal)))))

(defun version-control-get-ignored-status (file-path)
  (member file-path (version-control-get-ignored-files) :test #'equal))

(defun version-control-set-ignored-status (file-path ignored)
  (let ((files (version-control-get-ignored-files)))
    (if ignored
        (unless (member file-path files :test #'equal)
          (version-control-set-ignored-files (append files (list file-path))))
        (version-control-set-ignored-files (remove file-path files :test #'equal)))))

(defun version-control-commit-affects-file (git commit file-path)
  "Check if COMMIT affects FILE-PATH"
  (let ((output (ignore-errors 
                  (uiop:run-program (list "git" "-C" (repo-path git) "show" "--name-only" "--pretty=format:" commit)
                                    :output :string))))
    (when output
      (search (namestring file-path) output))))

(defun confirm-dialog (message confirm-fn &key default-action danger-action)
  "Show a confirmation dialog with default and danger actions"
  (let ((confirmed (run-confirm-dialog message
                                       :default-action default-action
                                       :danger-action danger-action
                                       :title "Confirm")))
    (when confirmed
      (funcall confirm-fn))))

;; Placeholder dialog classes
(defclass version-control-revert-dialog (clim:application-frame)
  ((log :initarg :log :reader version-control-revert-log)
   (file-path :initarg :file-path :reader version-control-revert-file-path)))

(defclass version-control-compare-dialog (clim:application-frame)
  ((log :initarg :log :reader version-control-compare-log)
   (file-path :initarg :file-path :reader version-control-compare-file-path)))

(defun make-version-control-revert-dialog (log file-path)
  (make-instance 'version-control-revert-dialog :log log :file-path file-path))

(defun make-version-control-compare-dialog (log file-path)
  (make-instance 'version-control-compare-dialog :log log :file-path file-path))
