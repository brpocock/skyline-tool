(in-package :skyline-tool)

(defun vc-find-root (file-path)
  "Find the closest version control root for FILE-PATH."
  (let ((start-dir (pathname-directory (truename (etypecase file-path
                                                  (string (parse-namestring file-path))
                                                  (pathname file-path))))))
    (loop for dir = start-dir then (uiop:pathname-directory-pathname dir)
          while dir
          thereis (loop for vc-dir in '(".git" ".hg" ".svn" ".bzr")
                        thereis (probe-file (merge-pathnames vc-dir dir))))))

(defun vc-backend (vc-dir)
  "Determine the VC backend from the VC directory."
  (cond
    ((probe-file (merge-pathnames ".git/" vc-dir)) :git)
    ((probe-file (merge-pathnames ".hg/" vc-dir)) :hg)
    ((probe-file (merge-pathnames ".svn/" vc-dir)) :svn)
    ((probe-file (merge-pathnames ".bzr/" vc-dir)) :bzr)
    (t :unknown)))

(defun vc-file-status (file-path)
  "Return version control status as keyword."
  (let ((vc-dir (vc-find-root file-path)))
    (if vc-dir
        (ecase (vc-backend vc-dir)
          (:git (vc-git-file-status file-path vc-dir))
          (:hg (vc-hg-file-status file-path vc-dir))
          (:svn (vc-svn-file-status file-path vc-dir))
          (:bzr (vc-bzr-file-status file-path vc-dir))
          (:unknown)))
      :unknown)))

(defun vc-git-file-status (file-path vc-dir)
  "Get Git status for FILE-PATH relative to VC-DIR."
  (let* ((file-truename (truename file-path))
         (vc-truename (truename vc-dir))
         (relative-path (enough-namestring file-truename vc-truename))
         (output (uiop:run-program (list "git" "status" "--porcelain" "--" relative-path)
                                 :output :string
                                 :ignore-error-status t
                                 :directory-string vc-dir)))
    (cond
      ((string= output "") :unmodified)
      ((search "??" output) :untracked)
      ((search "M " output) :modified)
      ((search "A " output) :added)
      ((search "D " output) :deleted)
      ((search "!!" output) :ignored)
      (t :modified))))

(defun vc-hg-file-status (file-path vc-dir)
  (declare (ignore file-path vc-dir))
  :unknown)

(defun vc-svn-file-status (file-path vc-dir)
  (declare (ignore file-path vc-dir))
  :unknown)

(defun vc-bzr-file-status (file-path vc-dir)
  (declare (ignore file-path vc-dir))
  :unknown)

(defun present-vc-status-icon (status)
  "Return string icon for VC status."
  (ecase status
    (:unmodified "✓")
    (:modified "●")
    (:added "+")
    (:deleted "×")
    (:untracked "?")
    (:ignored "!")
    (:unknown "?")))