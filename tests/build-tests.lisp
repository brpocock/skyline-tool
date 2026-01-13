;;; Phantasia SkylineTool/tests/build-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

(defpackage :skyline-tool/build-test
  (:use :cl :fiveam)
  (:export #:build-tests))

(in-package :skyline-tool/build-test)

(def-suite build-tests
  :description "Regression tests for build system and compilation issues")

(in-suite build-tests)

;; Test for syntax errors in Lisp source files
(test lisp-source-syntax-check
  "Check that all Lisp source files can be read without syntax errors"
  (let ((source-files (directory (merge-pathnames "**/*.lisp"
                                                  (asdf:system-source-directory :skyline-tool))))
        (errors '()))
    (dolist (file source-files)
      (let ((filename (pathname-name file))
            (pathstring (namestring file)))
        ;; Skip files that are known to have special syntax or dependencies
        ;; Also skip third-party library files in lib/ directory
        (unless (or (member filename '("run-tests" "test-framework") :test #'string=)
                    (search "lib/" pathstring))
          (handler-case
              (progn
                (when (find-package :skyline-tool)
                  (let ((*package* (find-package :skyline-tool)))
                    (with-open-file (stream file)
                      (let ((eof (gensym)))
                        (loop for form = (read stream nil eof)
                              until (eq form eof)
                              finally (return t)))))))
            (error (e)
              (push (cons (namestring file) e) errors))))))
    (is (null errors)
        "All Lisp source files should be syntactically valid, but found errors in: ~A"
        errors)))


;; Test that ASDF system definition is valid
(test asdf-system-validity
  "Test that the ASDF system definition is valid"
  (let ((system (asdf:find-system :skyline-tool)))
    (is-true system "skyline-tool system should be defined")
    (is (stringp (asdf:system-description system))
        "system should have a description")
    (is (not (null (asdf:component-children system)))
        "system should have components")))

;; Test that all defined packages can be found
(test package-existence
  "Test that all expected packages exist"
  (let ((expected-packages '(:skyline-tool :skyline-tool/test
                             :skyline-tool/build-test)))
    (dolist (pkg expected-packages)
      (is-true (find-package pkg)
               "Package ~A should exist" pkg))))

;; Test that core functions have documentation
(test function-documentation
  "Test that core functions have documentation"
  (let ((core-functions '(skyline-tool:blob-rip-7800
                         skyline-tool:compile-art-7800
                         skyline-tool:compile-map
                         skyline-tool:compile-script)))
    (dolist (func core-functions)
      (when (fboundp func)
        (is (documentation func 'function)
            "~A should have documentation" func)))))

;; Test build system stability
(test build-system-stability
  "Test that the build system components are stable"
  (let ((makefile-path (merge-pathnames "../common.mak"
                                        (asdf:system-source-directory :skyline-tool))))
    (is-true (probe-file makefile-path)
             "Makefile should exist")
    (is (stringp (asdf:system-description (asdf:find-system :skyline-tool)))
        "System description should be a string")))

(defun run-build-tests ()
  "Run all build tests and return results"
  (fiveam:run! 'build-tests))
