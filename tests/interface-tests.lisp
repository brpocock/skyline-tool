(defpackage :skyline-tool/interface-test
  (:use :cl :fiveam)
  (:import-from :skyline-tool
                #:command
                #:blob-rip-7800
                #:compile-art-7800
                #:compile-map
                #:compile-script
                #:compile-tileset)
  (:export #:interface-tests))

(in-package :skyline-tool/interface-test)

(def-suite interface-tests
  :description "Tests for command-line interface functions")

(in-suite interface-tests)

;; Helper function to capture command output
(defmacro with-captured-output ((&key) &body body)
  "Capture *standard-output* and return it as a string."
  `(with-output-to-string (*standard-output*)
     ,@body))

;; Test blob-rip-7800 interface
(test blob-rip-7800-interface
  "Test that blob-rip-7800 interface function exists and can be called"
  (let ((function (find-symbol "BLOB-RIP-7800" :skyline-tool)))
    (is-true function "blob-rip-7800 function should exist")
    (is (fboundp function) "blob-rip-7800 should be bound to a function")))

;; Test compile-art-7800 interface
(test compile-art-7800-interface
  "Test that compile-art-7800 interface function exists and can be called"
  (let ((function (find-symbol "COMPILE-ART-7800" :skyline-tool)))
    (is-true function "compile-art-7800 function should exist")
    (is (fboundp function) "compile-art-7800 should be bound to a function")))

;; Test compile-map interface
(test compile-map-interface
  "Test that compile-map interface function exists and can be called"
  (let ((function (find-symbol "COMPILE-MAP" :skyline-tool)))
    (is-true function "compile-map function should exist")
    (is (fboundp function) "compile-map should be bound to a function")))

;; Test compile-script interface
(test compile-script-interface
  "Test that compile-script interface function exists and can be called"
  (let ((function (find-symbol "COMPILE-SCRIPT" :skyline-tool)))
    (is-true function "compile-script function should exist")
    (is (fboundp function) "compile-script should be bound to a function")))

;; Test compile-tileset interface
(test compile-tileset-interface
  "Test that compile-tileset interface function exists and can be called"
  (let ((function (find-symbol "COMPILE-TILESET" :skyline-tool)))
    (is-true function "compile-tileset function should exist")
    (is (fboundp function) "compile-tileset should be bound to a function")))

;; Test command dispatch mechanism
(test command-dispatch-mechanism
  "Test that the command dispatch mechanism works"
  (let ((invocation-var (find-symbol "*INVOCATION*" :skyline-tool)))
    (is-true invocation-var "*invocation* variable should exist")
    (is (boundp invocation-var) "*invocation* should be bound")
    (is (hash-table-p (symbol-value invocation-var))
        "*invocation* should be a hash table")))

;; Test help/about functionality
(test help-functionality
  "Test that help/about functions exist"
  (let ((about-function (find-symbol "ABOUT-SKYLINE-TOOL" :skyline-tool)))
    (is-true about-function "about-skyline-tool function should exist")
    (is (fboundp about-function) "about-skyline-tool should be bound to a function")))

;; Test that all interface functions are registered
(test interface-function-registration
  "Test that all expected interface functions are registered in *invocation*"
  (let* ((invocation (symbol-value (find-symbol "*INVOCATION*" :skyline-tool)))
         (expected-commands '(:blob-rip-7800 :compile-art-7800 :compile-map
                             :compile-script :compile-tileset :help :--help :-h)))
    (dolist (cmd expected-commands)
      (is-true (gethash cmd invocation)
               "~A should be registered in *invocation*" cmd))))

;; Test error handling for unknown commands
(test unknown-command-handling
  "Test that unknown commands are handled gracefully"
  (let ((command-function (find-symbol "COMMAND" :skyline-tool)))
    (is-true command-function "command function should exist")
    (is (fboundp command-function) "command should be bound to a function")))

;; Test core compilation functions
(test core-compilation-functions
  "Test that core compilation functions are available"
  (let ((core-functions '(compile-fountain-string collect-strings
                        extract-tileset-palette make-classes-for-oops)))
    (dolist (func-name core-functions)
      (let ((func (find-symbol (string-upcase (string func-name)) :skyline-tool)))
        (is-true func "~A function should exist" func-name)
        (is (fboundp func) "~A should be bound to a function" func-name)))))

;; Test asset collection functionality
(test asset-collection-functions
  "Test that asset collection functions exist"
  (let ((asset-functions '(collect-assets check-for-absent-assets
                        allocate-assets)))
    (dolist (func-name asset-functions)
      (let ((func (find-symbol (string-upcase (string func-name)) :skyline-tool)))
        (is-true func "~A function should exist" func-name)
        (is (fboundp func) "~A should be bound to a function" func-name)))))

;; Test label generation functions
(test label-generation-functions
  "Test that label generation functions exist"
  (let ((label-functions '(labels-to-forth labels-to-mame
                        labels-to-include atari800-label-file)))
    (dolist (func-name label-functions)
      (let ((func (find-symbol (string-upcase (string func-name)) :skyline-tool)))
        (is-true func "~A function should exist" func-name)
        (is (fboundp func) "~A should be bound to a function" func-name)))))

;; Test MIDI/audio functions
(test midi-functions
  "Test that MIDI and audio functions exist"
  (let ((midi-functions '(midi-compile compile-forth)))
    (dolist (func-name midi-functions)
      (let ((func (find-symbol (string-upcase (string func-name)) :skyline-tool)))
        (is-true func "~A function should exist" func-name)
        (is (fboundp func) "~A should be bound to a function" func-name)))))

;; Test that interface functions have proper documentation
(test interface-function-documentation
  "Test that interface functions have docstrings"
  (let ((functions-to-check '(blob-rip-7800 compile-art-7800 compile-map
                            compile-script compile-tileset)))
    (dolist (func-name functions-to-check)
      (let ((func (find-symbol (string-upcase (string func-name)) :skyline-tool)))
        (when (and func (fboundp func))
          (is (documentation func 'function)
              "~A should have documentation" func-name))))))

(defun run-interface-tests ()
  "Run all interface tests and return results"
  (fiveam:run! 'interface-tests))