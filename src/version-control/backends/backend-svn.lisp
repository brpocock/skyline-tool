;;; src/version-control/backends/svn.lisp
;;; SVN backend implementation for Skyline-Tool

(in-package :skyline-tool.version-control)

(defclass svn-backend (vc-backend)
  ((repo-path :initarg :repo-path :accessor repo-path)))

(defmethod vc-name ((backend svn-backend))
  "svn")

(defmethod vc-version ((backend svn-backend))
  (or (ignore-errors 
        (uiop:run-program (list "svn" "--version") :output :string)) 
      "unknown"))

(defmethod vc-available-p ((backend svn-backend))
  (ignore-errors 
   (zerop (uiop:run-program (list "svn" "--version") :output nil))))

(defmethod vc-init ((backend svn-backend) path &key)
  (when (probe-file path)
    (uiop:run-program (list "svnadmin" "create" path)))
  (make-instance 'svn-backend :repo-path path))

(defmethod vc-clone ((backend svn-backend) url path &key)
  (uiop:run-program (list "svn" "checkout" url path))
  (make-instance 'svn-backend :repo-path path))

(defmethod vc-status ((backend svn-backend) path &key)
  (let ((output (ignore-errors 
                  (uiop:run-program (list "svn" "status" path) :output :string))))
    (when output (parse-svn-status output))))

(defun parse-svn-status (output)
  (let (result)
    (dolist (line (split-sequence #\newline output))
      (when (plusp (length line))
        (cond
          ((search "A" (subseq line 0 1)) (push :staged result))
          ((search "M" (subseq line 0 1)) (push :modified result))
          ((search "?" (subseq line 0 1)) (push :untracked result)))))
    result))

(defmethod vc-add ((backend svn-backend) paths &key)
  (uiop:run-program (append (list "svn" "add") paths)))

(defmethod vc-commit ((backend svn-backend) message &key)
  (uiop:run-program (list "svn" "commit" "-m" message)))

(defmethod vc-reset ((backend svn-backend) paths &key soft mixed hard)
  (uiop:run-program (append (list "svn" "revert") paths)))

(defmethod vc-log ((backend svn-backend) path &key limit since until author)
  (let ((cmd (list "svn" "log")))
    (when limit (push (format nil "-l ~a" limit) cmd))
    (ignore-errors 
      (uiop:run-program (append cmd (list (or path (repo-path backend)))) :output :string))))

(defmethod vc-push ((backend svn-backend) remote branch &key force-with-lease)
  (uiop:run-program (list "svn" "commit" "-m" (format nil "pushing ~a ~a" remote branch))))

(defmethod vc-pull ((backend svn-backend) remote branch &key rebase)
  (uiop:run-program (list "svn" "update" branch)))

(defmethod vc-fetch ((backend svn-backend) &key remote all tags prune)
  (uiop:run-program (list "svn" "update")))

(defmethod vc-remote-add ((backend svn-backend) name url)
  (uiop:run-program (list "svn" "remote" "add" name url)))

(defmethod vc-remote-list ((backend svn-backend))
  (ignore-errors 
    (uiop:run-program (list "svn" "info" "--show-item" "url") :output :string)))

(defmethod vc-submodule-add ((backend svn-backend) url path &key branch)
  (declare (ignore url path branch))
  (error "Submodules not natively supported in SVN"))

(defmethod vc-submodule-update ((backend svn-backend) &key init recursive remote)
  (declare (ignore init recursive remote))
  (error "Submodules not natively supported in SVN"))

(defmethod vc-submodule-status ((backend svn-backend))
  (declare (ignore backend))
  (error "Submodules not natively supported in SVN"))

(defmethod vc-stash ((backend svn-backend) action &rest args)
  (declare (ignore action args))
  (error "Stashing not natively supported in SVN"))

(defmethod vc-tag ((backend svn-backend) &key list create delete annotate)
  (declare (ignore list create delete annotate))
  (uiop:run-program (list "svn" "copy" "HEAD" "tags/new-tag")))

(defmethod vc-config-get ((backend svn-backend) key &key global local)
  (declare (ignore global local))
  (uiop:run-program (list "svn" "config" "get" (format nil "~a" key)) :output :string))

(defmethod vc-config-set ((backend svn-backend) key value &key global local)
  (declare (ignore global local))
  (uiop:run-program (list "svn" "config" "set" (format nil "~a" key) value)))

(defmethod vc-user-name ((backend svn-backend) &key global)
  (declare (ignore global))
  (uiop:run-program (list "svn" "config" "get" "user-name") :output :string))

(defmethod vc-user-email ((backend svn-backend) &key global)
  (declare (ignore global))
  (uiop:run-program (list "svn" "config" "get" "user-email") :output :string))

(defmethod vc-set-user ((backend svn-backend) name email &key global)
  (declare (ignore global))
  (uiop:run-program (list "svn" "config" "set" "user-name" name))
  (uiop:run-program (list "svn" "config" "set" "user-email" email)))
