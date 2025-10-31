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