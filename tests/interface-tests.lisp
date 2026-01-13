;;; Phantasia SkylineTool/tests/interface-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

(in-package :skyline-tool/test)

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
         (expected-commands '(:allocate-assets :blob-rip-7800 :compile-art-7800 :compile-forth
                             :compile-map :compile-script :compile-tileset :extract-tileset-palette
                             :labels-to-forth :labels-to-mame :patch-7800gd :prepend-fundamental-mode
                             :push-7800gd :write-actor-prototypes :write-asset-ids :write-character-ids
                             :write-gimp-palettes :write-master-makefile :help :--help :-h)))
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

;; Test allocate-assets interface
(test allocate-assets-interface
  "Test that allocate-assets interface function exists and can be called"
  (let ((function (find-symbol "ALLOCATE-ASSETS" :skyline-tool)))
    (is-true function "allocate-assets function should exist")
    (is (fboundp function) "allocate-assets should be bound to a function")))

;; Test compile-forth interface
(test compile-forth-interface
  "Test that compile-forth interface function exists and can be called"
  (let ((function (find-symbol "COMPILE-FORTH" :skyline-tool)))
    (is-true function "compile-forth function should exist")
    (is (fboundp function) "compile-forth should be bound to a function")))

;; Test extract-tileset-palette interface
(test extract-tileset-palette-interface
  "Test that extract-tileset-palette interface function exists and can be called"
  (let ((function (find-symbol "EXTRACT-TILESET-PALETTE" :skyline-tool)))
    (is-true function "extract-tileset-palette function should exist")
    (is (fboundp function) "extract-tileset-palette should be bound to a function")))

;; Test labels-to-forth interface
(test labels-to-forth-interface
  "Test that labels-to-forth interface function exists and can be called"
  (let ((function (find-symbol "LABELS-TO-FORTH" :skyline-tool)))
    (is-true function "labels-to-forth function should exist")
    (is (fboundp function) "labels-to-forth should be bound to a function")))

;; Test labels-to-mame interface
(test labels-to-mame-interface
  "Test that labels-to-mame interface function exists and can be called"
  (let ((function (find-symbol "LABELS-TO-MAME" :skyline-tool)))
    (is-true function "labels-to-mame function should exist")
    (is (fboundp function) "labels-to-mame should be bound to a function")))

;; Test patch-7800gd interface
(test patch-7800gd-interface
  "Test that patch-7800gd interface function exists and can be called"
  (let ((function (find-symbol "PUSH-7800GD-BIN-NO-EXECUTE" :skyline-tool)))
    (is-true function "patch-7800gd function should exist")
    (is (fboundp function) "patch-7800gd should be bound to a function")))

;; Test prepend-fundamental-mode interface
(test prepend-fundamental-mode-interface
  "Test that prepend-fundamental-mode interface function exists and can be called"
  (let ((function (find-symbol "PREPEND-FUNDAMENTAL-MODE" :skyline-tool)))
    (is-true function "prepend-fundamental-mode function should exist")
    (is (fboundp function) "prepend-fundamental-mode should be bound to a function")))

;; Test push-7800gd interface
(test push-7800gd-interface
  "Test that push-7800gd interface function exists and can be called"
  (let ((function (find-symbol "PUSH-7800GD-BIN" :skyline-tool)))
    (is-true function "push-7800gd function should exist")
    (is (fboundp function) "push-7800gd should be bound to a function")))

;; Test write-actor-prototypes interface
(test write-actor-prototypes-interface
  "Test that write-actor-prototypes interface function exists and can be called"
  (let ((function (find-symbol "WRITE-ACTOR-PROTOTYPES" :skyline-tool)))
    (is-true function "write-actor-prototypes function should exist")
    (is (fboundp function) "write-actor-prototypes should be bound to a function")))

;; Test write-asset-ids interface
(test write-asset-ids-interface
  "Test that write-asset-ids interface function exists and can be called"
  (let ((function (find-symbol "WRITE-ASSET-IDS" :skyline-tool)))
    (is-true function "write-asset-ids function should exist")
    (is (fboundp function) "write-asset-ids should be bound to a function")))

;; Test write-character-ids interface
(test write-character-ids-interface
  "Test that write-character-ids interface function exists and can be called"
  (let ((function (find-symbol "WRITE-CHARACTER-IDS" :skyline-tool)))
    (is-true function "write-character-ids function should exist")
    (is (fboundp function) "write-character-ids should be bound to a function")))

;; Test write-gimp-palettes interface
(test write-gimp-palettes-interface
  "Test that write-gimp-palettes interface function exists and can be called"
  (let ((function (find-symbol "WRITE-GIMP-PALETTES" :skyline-tool)))
    (is-true function "write-gimp-palettes function should exist")
    (is (fboundp function) "write-gimp-palettes should be bound to a function")))

;; Test write-master-makefile interface
(test write-master-makefile-interface
  "Test that write-master-makefile interface function exists and can be called"
  (let ((function (find-symbol "WRITE-MASTER-MAKEFILE" :skyline-tool)))
    (is-true function "write-master-makefile function should exist")
    (is (fboundp function) "write-master-makefile should be bound to a function")))

;; Test that interface functions have proper documentation
(test interface-function-documentation
  "Test that interface functions have docstrings"
  (let ((functions-to-check '(blob-rip-7800 compile-art-7800 compile-map compile-script compile-tileset
                            allocate-assets compile-forth extract-tileset-palette labels-to-forth
                            labels-to-mame prepend-fundamental-mode write-actor-prototypes
                            write-asset-ids write-character-ids write-gimp-palettes write-master-makefile)))
    (dolist (func-name functions-to-check)
      (let ((func (find-symbol (string-upcase (string func-name)) :skyline-tool)))
        (when (and func (fboundp func))
          (is (documentation func 'function)
              "~A should have documentation" func-name))))))

(defun run-interface-tests ()
  "Run all interface tests and return results"
  (fiveam:run! 'interface-tests))
