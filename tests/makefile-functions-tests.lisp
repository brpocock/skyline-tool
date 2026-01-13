;;; Phantasia SkylineTool/tests/makefile-functions-tests.lisp
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
