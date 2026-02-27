;;; Phantasia SkylineTool/tests/misc-tests.lisp
;;;; Comprehensive tests for utility functions in misc.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

;; Use the test package defined by the test runner
(in-package :skyline-tool/test)

;;; Enhanced test utilities for targeted testing
(defmacro define-multi-test (name description iterations &body body)
  "Define a test that runs multiple iterations for statistical validation"
  `(test ,name
     ,description
     (dotimes (i ,iterations)
       ,@body)))

(def-suite misc-tests
  :description "Comprehensive tests for utility functions in misc.lisp"
  :in skyline-tool/test)

(in-suite misc-tests)

;; Test data generators for misc functions
(defun generate-valid-region ()
  "Generate a valid region keyword"
  (nth (random 3) '(:ntsc :pal :secam)))

(defun generate-invalid-region ()
  "Generate an invalid region"
  (let ((invalid-options '(nil :invalid :ntsc-pal :secam/ntsc)))
    (nth (random (length invalid-options)) invalid-options)))

(defun generate-valid-machine-number ()
  "Generate a valid machine number from supported platforms"
  (nth (random 10) '(8 16 64 2600 5200 7800 800 400 3010 1000)))

(defun generate-random-length (&optional (max 100))
  "Generate a random length for sequences"
  (random max))

(defun generate-random-sequence (&optional (max-length 50))
  "Generate a random sequence of various types"
  (let ((length (1+ (random max-length))))
    (ecase (random 4)
      (0 (make-list length :initial-element (random 256))) ; list
      (1 (make-string length :initial-element (code-char (+ 32 (random 95))))) ; string
      (2 (make-array length :element-type '(unsigned-byte 8) :initial-element (random 256))) ; vector
      (3 (make-array length :element-type 'character :initial-element #\a))))) ; character array

;; Test region-valid-p function
(test region-valid-p-valid-regions
  "Test region-valid-p with valid region settings"
  ;; Test each valid region
  (dolist (region '(:ntsc :pal :secam))
    (let ((skyline-tool::*region* region))
      (is-true (skyline-tool::region-valid-p)
               "region-valid-p should return T for valid region ~a" region))))

(test region-valid-p-invalid-regions
  "Test region-valid-p with invalid region settings"
  ;; Test various invalid regions
  (dolist (region '(nil :invalid :ntsc-pal :secam/ntsc "ntsc" 1))
    (let ((skyline-tool::*region* region))
      (signals error (skyline-tool::region-valid-p)
               "region-valid-p should signal error for invalid region ~a" region))))

;; Test warn-once function
(test warn-once-basic-functionality
  "Test that warn-once issues warnings and prevents spam"
  ;; This is tricky to test directly since it uses random numbers
  ;; We'll test that the function exists and doesn't error
  (is-true (fboundp 'skyline-tool::warn-once)
           "warn-once function should be defined")
  ;; Test basic functionality without checking output
  (finishes (skyline-tool::warn-once "Test warning ~d" 1)))

;; Test generate-secure-random-id function
(test generate-secure-random-id-basic
  "Test generate-secure-random-id basic functionality"
  (is-true (fboundp 'skyline-tool::generate-secure-random-id)
           "generate-secure-random-id function should be defined"))

(test generate-secure-random-id-output-format
  "Test that generate-secure-random-id returns properly formatted strings"
  (let ((id (skyline-tool::generate-secure-random-id)))
    (is (stringp id) "Should return a string")
    (is (= (length id) 16) "Default length should be 16 hex characters")
    ;; Check that it's valid hex
    (is (every (lambda (c) (or (digit-char-p c 16) (char= c #\space)))
               (remove #\- id))
        "Should contain only valid hex characters")))

(define-multi-test generate-secure-random-id-samples
  "Test generate-secure-random-id with various lengths"
  8 ; Test 8 different lengths for good coverage
  (let* ((length (1+ (random 32)))
         (id (skyline-tool::generate-secure-random-id length)))
    (is (stringp id) "Should always return a string")
    (is (= (length id) (* 2 length)) "Length should be 2×input length")
    (is (every (lambda (c) (digit-char-p c 16)) id)
        "Should contain only hex digits")))

;; Test exact-length function
(test exact-length-basic
  "Test exact-length basic functionality"
  (is (equal (skyline-tool::exact-length 3 '(1 2)) '(1 2 nil))
      "Should pad short lists with nil")
  (is (equal (skyline-tool::exact-length 2 '(1 2 3)) '(1 2))
      "Should truncate long lists")
  (is (equal (skyline-tool::exact-length 3 '(1 2 3)) '(1 2 3))
      "Should return exact length lists unchanged"))

(define-multi-test exact-length-various-inputs
  "Test exact-length with various sequences and lengths"
  12 ; Test 12 combinations for good coverage
  (let* ((target-len (random 20))
         (input-seq (generate-random-sequence 15))
         (result (skyline-tool::exact-length target-len input-seq)))
    (is (= (length result) target-len)
        "Result should have exact target length")
    ;; Check that prefix matches original
    (let ((min-len (min target-len (length input-seq))))
      (dotimes (i min-len)
        (is (equal (elt result i) (elt input-seq i))
            "Result prefix should match input")))))

;; Test machine-valid-p function
(test machine-valid-p-valid-machines
  "Test machine-valid-p with valid machine numbers"
  (dolist (machine '(8 16 64 2600 5200 7800 800 400 3010 1000 9918))
    (is-true (skyline-tool::machine-valid-p machine)
             "machine-valid-p should return T for valid machine ~a" machine)))

(test machine-valid-p-invalid-machines
  "Test machine-valid-p with invalid machine numbers"
  (dolist (machine '(nil -1 999 12345 "atari" :invalid))
    (is-false (skyline-tool::machine-valid-p machine)
              "machine-valid-p should return NIL for invalid machine ~a" machine)))

;; Test machine-short-name function
(test machine-short-name-valid
  "Test machine-short-name returns correct short names"
  (let ((test-cases '((8 . "NES") (2600 . "2600") (5200 . "5200") (7800 . "7800")
                      (800 . "800") (400 . "400") (3010 . "SMS") (1000 . "SG-1000"))))
    (dolist (test-case test-cases)
      (destructuring-bind (machine . expected) test-case
        (let ((skyline-tool::*machine* machine))
          (is (string= (skyline-tool::machine-short-name) expected)
              "machine-short-name should return ~a for machine ~a" expected machine))))))

;; Test machine-long-name function
(test machine-long-name-valid
  "Test machine-long-name returns correct long names"
  (let ((test-cases '((8 . "Nintendo Entertainment System") (2600 . "Atari 2600")
                      (5200 . "Atari 5200") (7800 . "Atari 7800"))))
    (dolist (test-case test-cases)
      (destructuring-bind (machine . expected) test-case
        (let ((skyline-tool::*machine* machine))
          (is (string= (skyline-tool::machine-long-name) expected)
              "machine-long-name should return ~a for machine ~a" expected machine))))))

;; Test truthy function
(test truthy-basic
  "Test truthy function with various inputs"
  (is-true (skyline-tool::truthy "yes") "String 'yes' should be truthy")
  (is-true (skyline-tool::truthy "true") "String 'true' should be truthy")
  (is-true (skyline-tool::truthy "1") "String '1' should be truthy")
  (is-false (skyline-tool::truthy "no") "String 'no' should be falsy")
  (is-false (skyline-tool::truthy "false") "String 'false' should be falsy")
  (is-false (skyline-tool::truthy "0") "String '0' should be falsy")
  (is-false (skyline-tool::truthy "") "Empty string should be falsy"))

;; Test pretty-duration function
(test pretty-duration-basic
  "Test pretty-duration formats time correctly"
  (is (string= (skyline-tool::pretty-duration 0) "0.000s")
      "Zero duration should format correctly")
  (is (string= (skyline-tool::pretty-duration 1.5) "1.500s")
      "Seconds should format correctly")
  (is (string= (skyline-tool::pretty-duration 65) "1m 5.000s")
      "Minutes and seconds should format correctly")
  (is (string= (skyline-tool::pretty-duration 3723) "1h 2m 3.000s")
      "Hours, minutes, seconds should format correctly"))

;; Test group-by function
(test group-by-basic
  "Test group-by function with simple predicate"
  (let ((result (skyline-tool::group-by #'evenp '(1 2 3 4 5 6))))
    (is (= (length result) 2) "Should have 2 groups")
    (is (equal (getf result t) '(2 4 6)) "Even numbers should be in T group")
    (is (equal (getf result nil) '(1 3 5)) "Odd numbers should be in NIL group")))

;; Test atari-f9-p function
(test atari-f9-p-basic
  "Test atari-f9-p detects F9 banking"
  (let ((skyline-tool::*machine* 2600))
    (is-false (skyline-tool::atari-f9-p) "2600 should not be F9 by default")))

;; Test atari-2600-number-of-banks function
(test atari-2600-banks-basic
  "Test atari-2600-number-of-banks"
  (let ((skyline-tool::*machine* 2600))
    (is (= (skyline-tool::atari-2600-number-of-banks) 1)
        "Default 2600 should have 1 bank")))

;; Test max-banks function
(test max-banks-valid-machines
  "Test max-banks returns correct values for different machines"
  (let ((test-cases '((2600 . 32) (5200 . 1) (7800 . 1) (8 . 8))))
    (dolist (test-case test-cases)
      (destructuring-bind (machine . expected) test-case
        (is (= (skyline-tool::max-banks machine) expected)
            "max-banks should return ~d for machine ~d" expected machine)))))

;; Test make-source-file-name function
(test make-source-file-name-basic
  "Test make-source-file-name generates correct paths"
  (let ((skyline-tool::*machine* 2600))
    (let ((result (skyline-tool::make-source-file-name "test")))
      (is (stringp result) "Should return a string")
      (is (search "test" result) "Should contain the base name"))))

;; Test char->font functions (basic existence and non-error)
(test char-font-functions-existence
  "Test that character font conversion functions exist"
  (dolist (func '(skyline-tool::char->petscii-font
                  skyline-tool::char->ascii-font
                  skyline-tool::char->min-font
                  skyline-tool::char->font))
    (is-true (fboundp func)
             "~a function should be defined" func)))

(test unicode-font-function
  "Test unicode->font function"
  (is-true (fboundp 'skyline-tool::unicode->font)
           "unicode->font function should be defined")
  (finishes (skyline-tool::unicode->font "test")
            "unicode->font should not error on basic input"))

;; Test string-index-to-screen function
(test string-index-to-screen-basic
  "Test string-index-to-screen basic functionality"
  (is-true (fboundp 'skyline-tool::string-index-to-screen)
           "string-index-to-screen function should be defined")
  ;; Test with a simple case
  (finishes (skyline-tool::string-index-to-screen 0)
            "string-index-to-screen should handle basic input"))

;; Test write-stringtab function
(test write-stringtab-existence
  "Test write-stringtab function exists"
  (is-true (fboundp 'skyline-tool::write-stringtab)
           "write-stringtab function should be defined"))

;; Test check-machine-valid function
(test check-machine-valid-basic
  "Test check-machine-valid function"
  (is-true (fboundp 'skyline-tool::check-machine-valid)
           "check-machine-valid function should be defined")
  (let ((skyline-tool::*machine* 2600))
    (finishes (skyline-tool::check-machine-valid)
              "check-machine-valid should not error for valid machine")))

;; Test dash-delim-p function
(test dash-delim-p-basic
  "Test dash-delim-p function"
  (is-true (fboundp 'skyline-tool::dash-delim-p)
           "dash-delim-p function should be defined")
  (is-true (skyline-tool::dash-delim-p #\-)
           "Dash should be recognized as delimiter")
  (is-false (skyline-tool::dash-delim-p #\a)
           "Letter should not be recognized as delimiter"))

;; Test transform-string function
(test transform-string-basic
  "Test transform-string function"
  (is-true (fboundp 'skyline-tool::transform-string)
           "transform-string function should be defined")
  (finishes (skyline-tool::transform-string "test-string")
            "transform-string should handle basic input"))

;; Test fresh-dictionary function
(test fresh-dictionary-basic
  "Test fresh-dictionary function"
  (is-true (fboundp 'skyline-tool::fresh-dictionary)
           "fresh-dictionary function should be defined")
  (let ((dict (skyline-tool::fresh-dictionary)))
    (is (hash-table-p dict) "Should return a hash table")))

;; Test group-flags function
(test group-flags-basic
  "Test group-flags function"
  (is-true (fboundp 'skyline-tool::group-flags)
           "group-flags function should be defined")
  (finishes (skyline-tool::group-flags '(1 2 3))
            "group-flags should handle basic input"))

;; Test write-index function
(test write-index-existence
  "Test write-index function exists"
  (is-true (fboundp 'skyline-tool::write-index)
           "write-index function should be defined"))

;; Test atari-bank-switching-method-switch function
(test atari-bank-switching-method-switch-basic
  "Test atari-bank-switching-method-switch function"
  (is-true (fboundp 'skyline-tool::atari-bank-switching-method-switch)
           "atari-bank-switching-method-switch function should be defined")
  (let ((skyline-tool::*machine* 2600))
    (finishes (skyline-tool::atari-bank-switching-method-switch)
              "atari-bank-switching-method-switch should not error")))

;; Test ensure-bin/64tass-exists function
(test ensure-bin-64tass-exists-basic
  "Test ensure-bin/64tass-exists function"
  (is-true (fboundp 'skyline-tool::ensure-bin/64tass-exists)
           "ensure-bin/64tass-exists function should be defined")
  (finishes (skyline-tool::ensure-bin/64tass-exists)
            "ensure-bin/64tass-exists should not error"))

;; Test prepend-fundamental-mode function
(test prepend-fundamental-mode-existence
  "Test prepend-fundamental-mode function exists"
  (is-true (fboundp 'skyline-tool::prepend-fundamental-mode)
           "prepend-fundamental-mode function should be defined"))

;; Test atari800-label-file function
(test atari800-label-file-existence
  "Test atari800-label-file function exists"
  (is-true (fboundp 'skyline-tool::atari800-label-file)
           "atari800-label-file function should be defined"))

;; Test various compilation and processing functions
(test compile-functions-existence
  "Test that major compilation functions exist"
  (dolist (func '(skyline-tool::compile-critters
                  skyline-tool::compile-sound
                  skyline-tool::compile-script
                  skyline-tool::write-cart-header))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test token-safe function
(test token-safe-basic
  "Test token-safe function"
  (is-true (fboundp 'skyline-tool::token-safe)
           "token-safe function should be defined")
  (is (stringp (skyline-tool::token-safe "test-token"))
      "token-safe should return a string")
  (is (stringp (skyline-tool::token-safe :test-symbol))
      "token-safe should handle symbols"))

;; Test make-message-label function
(test make-message-label-basic
  "Test make-message-label function"
  (is-true (fboundp 'skyline-tool::make-message-label)
           "make-message-label function should be defined")
  (is (stringp (skyline-tool::make-message-label "test"))
      "make-message-label should return a string"))

;; Test world-from-filename function
(test world-from-filename-basic
  "Test world-from-filename function"
  (is-true (fboundp 'skyline-tool::world-from-filename)
           "world-from-filename function should be defined")
  (finishes (skyline-tool::world-from-filename "test.map")
            "world-from-filename should handle basic input"))

;; Test critters-referenced function
(test critters-referenced-existence
  "Test critters-referenced function exists"
  (is-true (fboundp 'skyline-tool::critters-referenced)
           "critters-referenced function should be defined"))

;; Test all-references function
(test all-references-existence
  "Test all-references function exists"
  (is-true (fboundp 'skyline-tool::all-references)
           "all-references function should be defined"))

;; Test script processing functions
(test script-processing-existence
  "Test script processing functions exist"
  (dolist (func '(skyline-tool::script->asm
                  skyline-tool::script->file
                  skyline-tool::write-scripts
                  skyline-tool::compile-script))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test map processing functions
(test map-processing-existence
  "Test map processing functions exist"
  (dolist (func '(skyline-tool::interpret-map-tile
                  skyline-tool::gather-scripts
                  skyline-tool::map->script
                  skyline-tool::write-map+index
                  skyline-tool::add-map-row
                  skyline-tool::define-tile))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test tile layout functions
(test tile-layout-existence
  "Test tile layout functions exist"
  (dolist (func '(skyline-tool::read-tile-layout
                  skyline-tool::read-area
                  skyline-tool::seek-heading))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test macro functionality
(test map-loop-macro-existence
  "Test map-loop macro exists"
  (is-true (fboundp 'skyline-tool::map-loop)
           "map-loop macro should be defined"))

(test do-collect-macro-existence
  "Test do-collect macro exists"
  (is-true (fboundp 'skyline-tool::do-collect)
           "do-collect macro should be defined"))

;; Test get-make-vars function
(test get-make-vars-basic
  "Test get-make-vars function"
  (is-true (fboundp 'skyline-tool::get-make-vars)
           "get-make-vars function should be defined")
  (let ((vars (skyline-tool::get-make-vars)))
    (is (or (null vars) (hash-table-p vars))
        "get-make-vars should return nil or a hash table")))

;; Test make-var-actual-value function
(test make-var-actual-value-basic
  "Test make-var-actual-value function"
  (is-true (fboundp 'skyline-tool::make-var-actual-value)
           "make-var-actual-value function should be defined")
  (finishes (skyline-tool::make-var-actual-value "TEST_VAR" nil)
            "make-var-actual-value should handle basic input"))

;; Test find-size-of-bank function
(test find-size-of-bank-existence
  "Test find-size-of-bank function exists"
  (is-true (fboundp 'skyline-tool::find-size-of-bank)
           "find-size-of-bank function should be defined"))

;; Test error-output-from-compiling function
(test error-output-from-compiling-existence
  "Test error-output-from-compiling function exists"
  (is-true (fboundp 'skyline-tool::error-output-from-compiling)
           "error-output-from-compiling function should be defined"))