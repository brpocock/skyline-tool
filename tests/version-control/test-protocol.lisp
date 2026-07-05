;;; tests/version-control/test-protocol.lisp
;;; Unit tests for Skyline-Tool version control integration

(defpackage :skyline-tool.test.version-control
  (:use :cl :fiveam :skyline-tool.version-control)
  (:import-from :skyline-tool.version-control.gui
                #:format-vc-window-title))

(in-package :skyline-tool.test.version-control)

(def-suite vc-tests :description "Version Control integration tests")
(in-suite vc-tests)

(test vc-status-icon-test
  "Test that status icons return proper color and symbol for each status"
  (let ((absent (vc-status-icon :absent))
        (current (vc-status-icon :current))
        (staged (vc-status-icon :staged))
        (modified (vc-status-icon :modified))
        (untracked (vc-status-icon :untracked)))
    (is (string= (getf absent :color) "0.8 0 0") "Absent status has red color")
    (is (string= (getf current :color) "0 0.8 0") "Current status has green color")
    (is (string= (getf staged :color) "0 0 0.8") "Staged status has blue color")
    (is (string= (getf modified :color) "0.8 0.8 0") "Modified status has yellow color")
    (is (string= (getf untracked :color) "0.8 0 0.8") "Untracked status has magenta color")
    (is (char= (getf absent :symbol) #\✗) "Absent status has X symbol")
    (is (char= (getf current :symbol) #\✓) "Current status has check symbol")
    (is (char= (getf staged :symbol) #\➕) "Staged status has plus symbol")
    (is (char= (getf modified :symbol) #\✎) "Modified status has pencil symbol")
    (is (char= (getf untracked :symbol) #\✱) "Untracked status has star symbol")))

(test vc-status-text-test
  "Test that status text returns proper description for each status"
  (is (string= (vc-status-text :absent) "Absent from VC"))
  (is (string= (vc-status-text :current) "Current"))
  (is (string= (vc-status-text :staged) "Staged for commit"))
  (is (string= (vc-status-text :modified) "Modified since last commit"))
  (is (string= (vc-status-text :untracked) "Untracked")))

(test vc-config-pathname-test
  "Test that config pathname uses title-case and machine-directory-name"
  (let ((path (vc-config-pathname)))
    (is (search ".config/Skyline-Tool/" (namestring path)) "Config path contains expected directory")
    (is (search "-config.lisp" (namestring path)) "Config filename ends with -config.lisp")))

(test git-backend-test
  "Test Git backend creation and availability check"
  (let ((backend (make-instance 'git-backend)))
    (is (string= (vc-name backend) "git") "Git backend returns correct name")
    (is (vc-available-p backend) "Git is available on system")))

(test svn-backend-test
  "Test SVN backend creation"
  (let ((backend (make-instance 'svn-backend)))
    (is (string= (vc-name backend) "svn") "SVN backend returns correct name")
    ;; SVN may or may not be available depending on system
    ))

(test detect-vc-backend-test
  "Test detection of version control backend from directory"
  (let ((detected (detect-vc-backend (uiop:getcwd)))
        (valid (member detected '(:git :svn :hg nil))))
    (is-valid-backend detected valid)))

(test list-available-backends-test
  "Test listing available backends"
  (let ((available (list-available-backends)))
    (is (listp available) "Returns a list")
    (is (member :git available) "Git is in available list if installed")))

;; Helper for detecting valid backends
(defun is-valid-backend (detected)
  "Return T if detected is a valid backend keyword or NIL."
  (member detected '(:git :svn :hg nil)))