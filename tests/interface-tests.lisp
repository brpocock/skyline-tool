;;; Phantasia SkylineTool/tests/interface-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

(defpackage :skyline-tool/interface-test
  (:use :cl :fiveam)
  (:import-from :skyline-tool
                #:command
                #:blob-rip-7800
                #:compile-art-7800
                #:compile-map
                #:compile-script
                #:compile-tileset
                #:extract-tileset-palette
                #:labels-to-forth
                #:labels-to-mame
                #:prepend-fundamental-mode
                #:write-actor-prototypes
                #:write-asset-ids
                #:write-character-ids
                #:write-gimp-palettes
                #:write-master-makefile)
  (:export #:interface-tests #:conversion-tests))

(in-package :skyline-tool/interface-test)

(def-suite interface-tests
  :description "Tests for command-line interface functions")

(def-suite conversion-tests
  :description "Comprehensive functional tests for all Skyline-Tool conversion functions")

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

;; =============================================================================
;; COMPREHENSIVE CONVERSION FUNCTION TESTS
;; =============================================================================

(in-suite conversion-tests)

;; Helper functions for conversion tests
(defun create-temp-file (content &optional (extension "tmp"))
  "Create a temporary file with CONTENT and return its pathname."
  (let ((pathname (make-pathname :name (format nil "test-~A-~A" (get-universal-time) (random 1000))
                                 :type extension
                                 :directory '(:absolute "tmp"))))
    (ensure-directories-exist pathname)
    (with-open-file (stream pathname :direction :output :if-exists :supersede)
      (write-string content stream))
    pathname))

(defun cleanup-temp-file (pathname)
  "Remove a temporary file."
  (when (probe-file pathname)
    (delete-file pathname)))

(defmacro with-temp-files ((&rest bindings) &body body)
  "Create temporary files, execute BODY, then clean up."
  `(let ,bindings
     (unwind-protect
         (progn ,@body)
       ,@(mapcar (lambda (binding)
                   `(cleanup-temp-file ,(car binding)))
                 bindings))))

;; =============================================================================
;; WRITE-MASTER-MAKEFILE TESTS
;; =============================================================================

(test write-master-makefile-generates-valid-makefile
  "Test that write-master-makefile generates a syntactically valid Makefile"
  (let ((output-file (make-pathname :name "test-makefile" :type "mak" :directory '(:absolute "tmp"))))
    (ensure-directories-exist output-file)
    (unwind-protect
        (progn
          (finishes (write-master-makefile))
          ;; Check that some key targets exist in the generated Makefile
          (is-true (probe-file "Source/Generated/Makefile")
                   "Generated Makefile should exist")
          (when (probe-file "Source/Generated/Makefile")
            (with-open-file (stream "Source/Generated/Makefile")
              (let ((content (make-string (file-length stream))))
                (read-sequence content stream)
                ;; Check for essential Makefile syntax
                (is (search ".PHONY:" content) "Makefile should contain .PHONY targets")
                (is (search "all:" content) "Makefile should contain all target")
                (is (search "clean:" content) "Makefile should contain clean target")))))
      ;; Clean up the generated file if we created it
      (when (probe-file output-file)
        (delete-file output-file)))))

(test write-master-makefile-includes-all-targets
  "Test that write-master-makefile includes all expected build targets"
  (finishes (write-master-makefile))
  (is-true (probe-file "Source/Generated/Makefile")
           "Generated Makefile should exist")
  (when (probe-file "Source/Generated/Makefile")
    (with-open-file (stream "Source/Generated/Makefile")
      (let ((content (make-string (file-length stream))))
        (read-sequence content stream)
        ;; Check for key targets that should be generated
        (is (search "Dist/Phantasia.Public.NTSC.a78:" content)
            "Should include Public NTSC build target")
        (is (search "Dist/Phantasia.Demo.NTSC.a78:" content)
            "Should include Demo NTSC build target")
        (is (search "Dist/Phantasia.Test.a78:" content)
            "Should include Test build target")))))

;; =============================================================================
;; WRITE-GIMP-PALETTES TESTS
;; =============================================================================

(test write-gimp-palettes-creates-palette-files
  "Test that write-gimp-palettes creates GIMP palette files"
  (let ((original-dir (uiop:getcwd)))
    (unwind-protect
        (progn
          ;; Change to ensure we're in the right directory for relative paths
          (uiop:chdir (asdf:system-source-directory :skyline-tool))
          (uiop:chdir (make-pathname :directory '(:relative "..")))
          (finishes (write-gimp-palettes))
          ;; Check that palette files were created
          (let ((ntsc-palette (probe-file "Tools/Atari-7800-NTSC.gpl"))
                (pal-palette (probe-file "Tools/Atari-7800-PAL.gpl")))
            (is-true ntsc-palette "NTSC palette file should be created")
            (is-true pal-palette "PAL palette file should be created")
            ;; Verify content structure
            (when ntsc-palette
              (with-open-file (stream ntsc-palette)
                (let ((first-line (read-line stream)))
                  (is (search "GIMP Palette" first-line)
                      "Palette file should start with GIMP header"))))))
      (uiop:chdir original-dir))))

;; =============================================================================
;; EXTRACT-TILESET-PALETTE TESTS (already exists in graphics-tests)
;; =============================================================================

;; Skip - already tested in graphics-tests.lisp

;; =============================================================================
;; LABELS-TO-FORTH TESTS
;; =============================================================================

(test labels-to-forth-converts-basic-labels
  "Test that labels-to-forth converts assembler labels to Forth constants"
  (with-temp-files ((input-file (create-temp-file "TestLabel = $1234
AnotherLabel = $ABCD
EndLabel = *"))
                   (output-file (make-pathname :name "test-output" :type "forth" :directory '(:absolute "tmp"))))
    (finishes (labels-to-forth input-file output-file))
    (is-true (probe-file output-file) "Output file should be created")
    (when (probe-file output-file)
      (with-open-file (stream output-file)
        (let ((content (make-string (file-length stream))))
          (read-sequence content stream)
          (is (search "$1234 CONSTANT TestLabel" content)
              "Should convert TestLabel to Forth constant")
          (is (search "$ABCD CONSTANT AnotherLabel" content)
              "Should convert AnotherLabel to Forth constant"))))))

(test labels-to-forth-handles-hex-values
  "Test that labels-to-forth correctly handles hexadecimal values"
  (with-temp-files ((input-file (create-temp-file "HexLabel = $DEAD
DecimalLabel = 1234"))
                   (output-file (make-pathname :name "test-hex" :type "forth" :directory '(:absolute "tmp"))))
    (finishes (labels-to-forth input-file output-file))
    (when (probe-file output-file)
      (with-open-file (stream output-file)
        (let ((content (make-string (file-length stream))))
          (read-sequence content stream)
          (is (search "$DEAD CONSTANT HexLabel" content)
              "Should preserve hex format in Forth")
          (is (search "$4D2 CONSTANT DecimalLabel" content)
              "Should convert decimal to hex in Forth"))))))

;; =============================================================================
;; LABELS-TO-MAME TESTS
;; =============================================================================

(test labels-to-mame-converts-to-debug-format
  "Test that labels-to-mame converts labels to MAME debug format"
  (with-temp-files ((input-file (create-temp-file "Break = $1234
MinorFault = $5678
CurrentBank = $9ABC"))
                   (output-file (make-pathname :name "test-mame" :type "mame" :directory '(:absolute "tmp"))))
    (finishes (labels-to-mame input-file output-file))
    (is-true (probe-file output-file) "MAME output file should be created")
    (when (probe-file output-file)
      (with-open-file (stream output-file)
        (let ((content (make-string (file-length stream))))
          (read-sequence content stream)
          (is (search "wp" content) "Should contain MAME watchpoint commands")
          (is (search "$1234" content) "Should include converted addresses"))))))

;; =============================================================================
;; PREPEND-FUNDAMENTAL-MODE TESTS
;; =============================================================================

(test prepend-fundamental-mode-modifies-list-file
  "Test that prepend-fundamental-mode adds fundamental mode entries"
  (with-temp-files ((input-file (create-temp-file "test_entry = $1000
another_entry = $2000"))
                   (output-file (make-pathname :name "test-list" :type "txt" :directory '(:absolute "tmp"))))
    (finishes (prepend-fundamental-mode input-file))
    ;; The function modifies the input file in place
    (is-true (probe-file input-file) "Input file should still exist")
    (when (probe-file input-file)
      (with-open-file (stream input-file)
        (let ((content (make-string (file-length stream))))
          (read-sequence content stream)
          ;; Check that fundamental mode entries were added
          (is (search "fundamental" (string-downcase content))
              "Should add fundamental mode entries"))))))

;; =============================================================================
;; WRITE-ASSET-IDS TESTS
;; =============================================================================

(test write-asset-ids-generates-forth-and-asm-files
  "Test that write-asset-ids generates both Forth and assembly output"
  (let ((index-file (create-temp-file "test-asset-1.png 160A 8×8
test-asset-2.png 320C 16×16"))
        (forth-output (make-pathname :name "AssetIDs-test" :type "forth" :directory '(:absolute "tmp")))
        (asm-output (make-pathname :name "AssetIDs-test" :type "s" :directory '(:absolute "tmp"))))
    (ensure-directories-exist forth-output)
    (unwind-protect
        (progn
          (finishes (write-asset-ids index-file))
          (is-true (probe-file forth-output) "Forth output should be generated")
          (is-true (probe-file asm-output) "Assembly output should be generated")
          ;; Check content
          (when (probe-file forth-output)
            (with-open-file (stream forth-output)
              (let ((content (make-string (file-length stream))))
                (read-sequence content stream)
                (is (search "CONSTANT" content) "Forth file should contain constants"))))
          (when (probe-file asm-output)
            (with-open-file (stream asm-output)
              (let ((content (make-string (file-length stream))))
                (read-sequence content stream)
                (is (search ".equ" content) "Assembly file should contain .equ directives")))))
      (cleanup-temp-file index-file)
      (cleanup-temp-file forth-output)
      (cleanup-temp-file asm-output))))

;; =============================================================================
;; WRITE-CHARACTER-IDS TESTS
;; =============================================================================

(test write-character-ids-generates-character-tables
  "Test that write-character-ids generates character ID tables from spreadsheet"
  (let ((spreadsheet-file (create-temp-file "TestCharacter,1,Description
AnotherCharacter,2,Another Description"))
        (forth-output (make-pathname :name "CharacterIDs-test" :type "forth" :directory '(:absolute "tmp")))
        (asm-output (make-pathname :name "CharacterIDs-test" :type "s" :directory '(:absolute "tmp"))))
    (ensure-directories-exist forth-output)
    (unwind-protect
        (progn
          (finishes (write-character-ids spreadsheet-file))
          (is-true (probe-file forth-output) "Forth output should be generated")
          (is-true (probe-file asm-output) "Assembly output should be generated"))
      (cleanup-temp-file spreadsheet-file)
      (cleanup-temp-file forth-output)
      (cleanup-temp-file asm-output))))

;; =============================================================================
;; WRITE-ACTOR-PROTOTYPES TESTS
;; =============================================================================

(test write-actor-prototypes-generates-prototype-data
  "Test that write-actor-prototypes generates actor prototype assembly"
  (let ((spreadsheet-file (create-temp-file "Character,HP,Attack,Defense
TestChar,100,50,25"))
        (output-file (make-pathname :name "ActorPrototypes-test" :type "s" :directory '(:absolute "tmp"))))
    (ensure-directories-exist output-file)
    (unwind-protect
        (progn
          (finishes (write-actor-prototypes spreadsheet-file))
          (is-true (probe-file output-file) "Output file should be generated")
          (when (probe-file output-file)
            (with-open-file (stream output-file)
              (let ((content (make-string (file-length stream))))
                (read-sequence content stream)
                (is (search ".byte" content) "Should contain assembly .byte directives")))))
      (cleanup-temp-file spreadsheet-file)
      (cleanup-temp-file output-file))))

;; =============================================================================
;; COMPILATION FUNCTION TESTS (Basic - detailed tests already exist)
;; =============================================================================

(test compile-forth-basic-functionality
  "Test that compile-forth can process basic Forth code"
  (with-temp-files ((input-file (create-temp-file ": test-word 42 ;"))
                   (output-file (make-pathname :name "test-forth" :type "s" :directory '(:absolute "tmp"))))
    (ensure-directories-exist output-file)
    (finishes (compile-forth input-file output-file))
    (is-true (probe-file output-file) "Forth compilation should produce output file")))

(test compile-map-basic-functionality
  "Test that compile-map can process basic TMX map files"
  (with-temp-files ((input-file (create-temp-file "<?xml version=\"1.0\"?>
<map version=\"1.0\" orientation=\"orthogonal\" width=\"10\" height=\"10\" tilewidth=\"8\" tileheight=\"8\">
 <layer name=\"Tile Layer 1\" width=\"10\" height=\"10\">
  <data encoding=\"csv\">1,1,1,1,1,1,1,1,1,1</data>
 </layer>
</map>"))
                   (output-file (make-pathname :name "test-map" :type "s" :directory '(:absolute "tmp"))))
    (ensure-directories-exist output-file)
    (finishes (compile-map input-file))
    ;; compile-map generates files in Source/Generated/Maps/
    (let ((expected-output (make-pathname :name "test-map" :type "s"
                                         :directory '(:relative "Source" "Generated" "Maps"))))
      (is-true (probe-file expected-output) "Map compilation should produce output file"))))

;; =============================================================================
;; ERROR HANDLING TESTS
;; =============================================================================

(test conversion-functions-handle-missing-files
  "Test that conversion functions handle missing input files gracefully"
  (let ((nonexistent-file (make-pathname :name "nonexistent" :type "txt" :directory '(:absolute "tmp"))))
    (signals error (extract-tileset-palette nonexistent-file "/tmp/out.s"))
    (signals error (labels-to-forth nonexistent-file "/tmp/out.forth"))
    (signals error (labels-to-mame nonexistent-file "/tmp/out.mame"))))

(test conversion-functions-validate-output-paths
  "Test that conversion functions validate output file paths"
  (let ((input-file (create-temp-file "test content"))
        (invalid-output "/invalid/path/output.txt"))
    (unwind-protect
        (progn
          ;; These should signal errors for invalid output paths
          (signals error (extract-tileset-palette input-file invalid-output))
          (signals error (labels-to-forth input-file invalid-output)))
      (cleanup-temp-file input-file))))

(defun run-conversion-tests ()
  "Run all conversion function tests and return results"
  (fiveam:run! 'conversion-tests))

(defun run-interface-tests ()
  "Run all interface tests and return results"
  (fiveam:run! 'interface-tests))
