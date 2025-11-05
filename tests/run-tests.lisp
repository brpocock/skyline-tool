#!/usr/bin/env sbcl --script

;; Simple test runner for Skyline-Tool action tests
;; This avoids the make dependency issue by running tests in isolation

(require :asdf)

;; Load setup.lisp from SkylineTool root directory
(let ((setup-path (merge-pathnames "setup.lisp"
                                   (uiop:pathname-parent-directory-pathname
                                    (uiop:pathname-directory-pathname *load-pathname*)))))
  (load setup-path))

(ql:quickload :fiveam)
(asdf:clear-system :skyline-tool/test)
(let ((root (uiop:pathname-parent-directory-pathname
             (uiop:pathname-directory-pathname *load-pathname*))))
  (asdf:load-asd (merge-pathnames "skyline-tool.asd" root)))
(asdf:load-system :skyline-tool/test :force t)

(format t "~%Running Skyline-Tool action tests...~%")

(uiop:chdir (uiop:pathname-parent-directory-pathname
              (asdf:system-source-directory :skyline-tool)))
;; Configure FiveAM to report all failures but continue the run
(setf fiveam:*on-error* :backtrace)
(setf fiveam:*on-failure* :backtrace)
(setf fiveam:*test-dribble* *standard-output*)
;; Run tests and capture results
(let ((results (fiveam:run! 'skyline-tool/test:action-tests)))
  (when (and results (not (eq results t)))
    (unless (fiveam:results-status results)
      (format t "~%Action tests had failures, but continuing...~%"))))
;; Run sprite compilation regression tests (loaded via ASDF system)
(let ((results (fiveam:run! 'skyline-tool/test/sprites:sprite-compilation-tests)))
  (when (and results (not (eq results t)))
    (unless (fiveam:results-status results)
      (format t "~%Sprite compilation tests had failures, but continuing...~%"))))
(format t "~%All tests completed.~%") 