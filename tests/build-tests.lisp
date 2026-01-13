;;; Phantasia SkylineTool/tests/build-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

(in-package :skyline-tool/test)

(def-suite build-tests
  :description "Regression tests for build system and compilation issues")

(in-suite build-tests)

;; Helper function to check for unmatched delimiters in a string
(defun check-delimiter-balance (content)
  "Check if parentheses and brackets are balanced in CONTENT.
   Returns (values balanced-p unmatched-opens)."
  (let ((stack '())
        (unmatched-opens 0))
    (loop for char across content
          do (cond ((char= char #\()
                    (push #\) stack))
                   ((char= char #\[)
                    (push #\] stack))
                   ((or (char= char #\)) (char= char #\]))
                    (if (and stack (char= char (car stack)))
                        (pop stack)
                        (incf unmatched-opens)))))
    (values (null stack) unmatched-opens)))

;; Test for syntax errors in Lisp source files
(test lisp-source-syntax-check
  "Check that all Lisp source files can be read without syntax errors"
  (let ((source-files (directory (merge-pathnames "**/*.lisp"
                                                  (asdf:system-source-directory :skyline-tool))))
        (errors '()))
    (dolist (file source-files)
      (handler-case
          (with-open-file (stream file)
            (let ((eof (gensym)))
              (loop for form = (read stream nil eof)
                    until (eq form eof)
                    finally (return t))))
        (error (e)
          (push (cons (namestring file) e) errors))))
    (is (null errors)
        "All Lisp source files should be syntactically valid, but found errors in: ~A"
        errors)))

;; Test for unmatched delimiters in source files
(test source-file-delimiter-balance
  "Check that all Lisp source files have balanced delimiters"
  (let ((source-files (directory (merge-pathnames "**/*.lisp"
                                                  (asdf:system-source-directory :skyline-tool))))
        (unbalanced-files '()))
    (dolist (file source-files)
      (with-open-file (stream file)
        (let ((content (make-string (file-length stream))))
          (read-sequence content stream)
          (multiple-value-bind (balanced-p unmatched-opens)
              (check-delimiter-balance content)
            (when (or (not balanced-p) (> unmatched-opens 0))
              (push (list (namestring file) balanced-p unmatched-opens)
                    unbalanced-files))))))
    (is (null unbalanced-files)
        "All Lisp source files should have balanced delimiters, but found unbalanced files: ~A"
        unbalanced-files)))

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
                             :skyline-tool/graphics-test
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
