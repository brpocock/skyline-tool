(in-package :skyline-tool)

(defun version-control-find-root (file-path)
  "Find the closest version control root for FILE-PATH."
  (let ((start-dir (pathname-directory (truename (etypecase file-path
                                                   (string (parse-namestring file-path))
                                                   (pathname file-path))))))
    (loop for dir = start-dir then (uiop:pathname-directory-pathname dir)
          while dir
          thereis (loop for version-control-dir in '(".git" ".hg" ".svn" ".bzr")
                        thereis (probe-file (merge-pathnames version-control-dir dir))))))

(defun version-control-backend (version-control-dir)
  "Determine the VC backend from the VC directory."
  (cond
    ((probe-file (merge-pathnames ".git/" version-control-dir)) :git)
    ((probe-file (merge-pathnames ".hg/" version-control-dir)) :hg)
    ((probe-file (merge-pathnames ".svn/" version-control-dir)) :svn)
    ((probe-file (merge-pathnames ".bzr/" version-control-dir)) :bzr)
    (t :unknown)))

(defun version-control-file-status (file-path)
  "Return version control status as keyword."
  (let ((version-control-dir (version-control-find-root file-path)))
    (if version-control-dir
        (ecase (version-control-backend version-control-dir)
          (:git (version-control-git-file-status file-path version-control-dir))
          (:hg (version-control-hg-file-status file-path version-control-dir))
          (:svn (version-control-svn-file-status file-path version-control-dir))
          (:bzr (version-control-bzr-file-status file-path version-control-dir))
          (:unknown))
      :unknown)))

(defun version-control-git-file-status (file-path version-control-dir)
  "Get Git status for FILE-PATH relative to VC-DIR."
  (let* ((file-truename (truename file-path))
         (version-control-truename (truename version-control-dir))
         (relative-path (enough-namestring file-truename version-control-truename))
         (output (uiop:run-program (list "git" "status" "--porcelain" "--" relative-path)
                                   :output :string
                                   :ignore-error-status t
                                   :directory-string version-control-dir)))
    (cond
      ((string= output "") :unmodified)
      ((search "??" output) :untracked)
      ((search "M " output) :modified)
      ((search "A " output) :added)
      ((search "D " output) :deleted)
      ((search "!!" output) :ignored)
      (t :modified))))

(defun version-control-hg-file-status (file-path version-control-dir)
  (declare (ignore file-path version-control-dir))
  :unknown)

(defun version-control-svn-file-status (file-path version-control-dir)
  (declare (ignore file-path version-control-dir))
  :unknown)

(defun version-control-bzr-file-status (file-path version-control-dir)
  (declare (ignore file-path version-control-dir))
  :unknown)

(defun present-version-control-status-icon (status)
  "Return string icon for VC status."
  (ecase status
    (:unmodified "✓")
    (:modified "●")
    (:added "+")
    (:deleted "×")
    (:untracked "?")
    (:ignored "!")
    (:unknown "?")))