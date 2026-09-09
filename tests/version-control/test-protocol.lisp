;;; tests/version-control/test-protocol.lisp
;;; Unit tests for Skyline-Tool version control integration

(defpackage :skyline-tool.test.version-control
  (:use :cl :fiveam :skyline-tool.version-control))

(in-package :skyline-tool.test.version-control)

(def-suite version-control-tests
  :description "Version Control integration tests")
(in-suite version-control-tests)

(test version-control-status-icon-test
  "Test that status icons return proper color and symbol for each status"
  (let ((absent (version-control-status-icon :absent))
        (current (version-control-status-icon :current))
        (staged (version-control-status-icon :staged))
        (modified (version-control-status-icon :modified))
        (untracked (version-control-status-icon :untracked)))
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

(test version-control-status-text-test
  "Test that status text returns proper description for each status"
  (is (string= (version-control-status-text :absent) "Absent from VC"))
  (is (string= (version-control-status-text :current) "Current"))
  (is (string= (version-control-status-text :staged) "Staged for commit"))
  (is (string= (version-control-status-text :modified) "Modified since last commit"))
  (is (string= (version-control-status-text :untracked) "Untracked")))

(test version-control-backend-factories-test
  "Test that backend factory functions return backend keywords"
  (is (eq (make-git-backend) :git) "make-git-backend returns :git")
  (is (eq (make-svn-backend "/tmp") :svn) "make-svn-backend returns :svn"))

(test version-control-git-backend-test
  "Test Git backend protocol methods"
  (let ((backend (make-git-backend)))
    (is (string= (version-control-name backend) "Git") "Git backend returns correct name")
    (is-true (member (version-control-available-p backend) '(t nil))
             "Git availability should be a boolean")))
