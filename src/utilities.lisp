(in-package :skyline-tool)

(defun window-title (&optional suffix)
  "Return a window title string.
   Without SUFFIX: \"Skyline-Tool for Phantasia\" (Launcher).
   With SUFFIX:    \"All Resources for Phantasia 7800\"."
  (let ((game (ignore-errors (string-capitalize (or (and (boundp '*game-title*) *game-title*) "Game"))))
        (machine (ignore-errors (machine-directory-name))))
    (if suffix
        (format nil "~a for ~a ~a" suffix game machine)
        (format nil "Skyline-Tool for ~a" game))))

(defun generated-file-path (filename)
  "Return the platform-specific path for a generated file."
  (let ((platform-dir (format nil "~d" *machine*)))
    (merge-pathnames (make-pathname :directory (list :relative "Source" "Generated" platform-dir)
                                    :name (pathname-name filename)
                                    :type (pathname-type filename))
                     (uiop:getcwd))))
