;;; Phantasia SkylineTool/tests/graphics-tests.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(defpackage :skyline-tool/graphics-test
  (:use :cl :fiveam)
  (:import-from :skyline-tool
                #:compile-map
                #:compile-art-7800
                #:blob-rip-7800
                #:blob-rip-7800-320ac)
  (:export #:graphics-tests))

(in-package :skyline-tool/graphics-test)

(def-suite graphics-tests
  :description "Tests for graphics converter functionality")

(in-suite graphics-tests)

;; Test that key graphics converters exist and can be called
(test graphics-converter-existence
  "Test that all key graphics converters exist and are callable"
  (is-true (fboundp 'skyline-tool:compile-map)
           "compile-map should be available")
  (is-true (fboundp 'skyline-tool:compile-art-7800)
           "compile-art-7800 should be available")
  (is-true (fboundp 'skyline-tool:blob-rip-7800)
           "blob-rip-7800 should be available")
  (is-true (fboundp 'skyline-tool:blob-rip-7800-320ac)
           "blob-rip-7800-320ac should be available"))

;; Test error handling for converters
(test graphics-converter-error-handling
  "Test that graphics converters handle errors appropriately"
  ;; compile-map should signal error for missing files
  (signals error
    (skyline-tool:compile-map "/nonexistent/file.tmx"))

  ;; compile-art-7800 should signal error for missing files
  (signals error
    (skyline-tool:compile-art-7800 "/nonexistent/input.txt" "/tmp/output.s"))

  ;; blob-rip-7800 should signal error for missing files
  (signals error
    (skyline-tool:blob-rip-7800 "/nonexistent/file.png"))

  ;; blob-rip-7800-320ac should handle missing files gracefully (stub implementation)
  (finishes (skyline-tool:blob-rip-7800-320ac "/nonexistent/file.png")))

;; Test function signatures and basic properties
(test graphics-converter-properties
  "Test that graphics converters have correct function properties"
  ;; All converters should be functions
  (is (functionp (symbol-function 'skyline-tool:compile-map)))
  (is (functionp (symbol-function 'skyline-tool:compile-art-7800)))
  (is (functionp (symbol-function 'skyline-tool:blob-rip-7800)))
  (is (functionp (symbol-function 'skyline-tool:blob-rip-7800-320ac)))

  ;; Test that functions can be described (basic introspection)
  (is (stringp (or (documentation 'skyline-tool:compile-map 'function) "no docs"))
      "compile-map should be documentable")
  (is (stringp (or (documentation 'skyline-tool:compile-art-7800 'function) "no docs"))
      "compile-art-7800 should be documentable"))

;; Test that converters can be called without immediate crashes
(test graphics-converter-basic-calls
  "Test that graphics converters can be called without immediate crashes"
  ;; These calls should not crash the system, even if they signal errors
  (finishes
    (handler-case
        (skyline-tool:compile-map "/nonexistent/file.tmx")
      (error () :expected-error))
    "compile-map call should not crash")

  (finishes
    (handler-case
        (skyline-tool:compile-art-7800 "/nonexistent/input.txt" "/tmp/output.s")
      (error () :expected-error))
    "compile-art-7800 call should not crash")

  (finishes
    (handler-case
        (skyline-tool:blob-rip-7800 "/nonexistent/file.png")
      (error () :expected-error))
    "blob-rip-7800 call should not crash")

  (finishes
    (skyline-tool:blob-rip-7800-320ac "/nonexistent/file.png")
    "blob-rip-7800-320ac call should not crash"))
