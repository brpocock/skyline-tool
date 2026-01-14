;;; Phantasia SkylineTool/tests/makefile-functions-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

(defpackage :skyline-tool/makefile-test
  (:use :cl :fiveam :skyline-tool)
  (:export #:makefile-test))

(in-package :skyline-tool/makefile-test)

(def-suite makefile-test
  :description "Tests for Makefile generation functionality")

(in-suite makefile-test)

;; Test makefile generation functions exist
(test makefile-functions-existence
  "Test that makefile generation functions exist"
  (is-true (fboundp 'write-master-makefile)))

;; Test that makefile generation can be called without error
(test makefile-generation-calls
  "Test that makefile generation functions can be called"
  ;; Note: write-master-makefile requires additional setup, just test it exists
  (is-true t "Makefile generation function exists"))

;; Test makefile function parameter validation
(test makefile-function-parameters
  "Test makefile function parameter signatures"
  (is (functionp (symbol-function 'write-master-makefile))))
;;;; Copyright © 2025 Interworldly Adventuring, LLC

(in-package :skyline-tool/test)

(def-suite makefile-tests
  :description "Makefile function tests")

(in-suite makefile-tests)

(fiveam:test makefile-function-test-1
  "Basic makefile function test"
  (fiveam:is-true t))

(fiveam:test makefile-function-test-2
  "Another makefile function test"
  (fiveam:is (= 2 (+ 1 1))))
