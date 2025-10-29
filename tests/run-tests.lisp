#!/usr/bin/env sbcl --script

;; Simple test runner for Skyline-Tool action tests
;; This avoids the make dependency issue by running tests in isolation

(require :asdf)

(load (make-pathname :defaults *load-pathname*
                     :directory (list :relative "SkylineTool")
                     :name "setup" :type "lisp"))

(ql:quickload :fiveam)
(ql:quickload :skyline-tool/test)

(format t "~%Running Skyline-Tool action tests...~%")

(uiop:chdir (uiop:pathname-parent-directory-pathname
              (asdf:system-source-directory :skyline-tool)))
(fiveam:run! 'skyline-tool/test:action-tests)
;; Load and run sprite compilation regression tests
(load (merge-pathnames "sprite-compilation-tests.lisp" 
                       (directory-namestring *load-pathname*)))
(fiveam:run! 'skyline-tool-tests:sprite-compilation-tests)
(format t "~%All tests completed.~%") 