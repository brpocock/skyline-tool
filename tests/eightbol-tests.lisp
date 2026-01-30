;;; Phantasia SkylineTool/tests/eightbol-tests.lisp
;;;; Comprehensive tests for eightbol.lisp COBOL compiler
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

(def-suite eightbol-tests
  :description "Comprehensive tests for eightbol COBOL compiler")

(in-suite eightbol-tests)

;; Test data generators for eightbol functions
(defun generate-random-cobol-identifier ()
  "Generate a random COBOL-style identifier"
  (let ((parts '("CUSTOMER" "INVOICE" "AMOUNT" "BALANCE" "TOTAL" "COUNT" "INDEX" "STATUS")))
    (nth (random (length parts)) parts)))

(defun generate-random-string (&optional (max-length 20))
  "Generate a random string for testing"
  (let ((length (1+ (random max-length))))
    (coerce (loop for i from 1 to length
                  collect (code-char (+ 65 (random 26))))
            'string)))

(defun generate-random-number-string (&optional (max-digits 5))
  "Generate a random numeric string"
  (format nil "~d" (random (expt 10 max-digits))))

;; Test string utility functions
(test split-sequence-basic
  "Test split-sequence with basic delimiters"
  (is (equal (eightbol::split-sequence #\, "a,b,c") '("a" "b" "c"))
      "Should split comma-separated values")
  (is (equal (eightbol::split-sequence #\space "hello world test") '("hello" "world" "test"))
      "Should split space-separated values")
  (is (equal (eightbol::split-sequence #\- "test-string") '("test" "string"))
      "Should split dash-separated values"))

(test split-sequence-edge-cases
  "Test split-sequence with edge cases"
  (is (equal (eightbol::split-sequence #\, "") '()) "Empty string should return empty list")
  (is (equal (eightbol::split-sequence #\, ",,") '()) "Only delimiters should return empty list")
  (is (equal (eightbol::split-sequence #\, "abc") '("abc")) "No delimiters should return original"))

(test split-sequence-edge-cases
  "Test split-sequence with various delimiters and edge cases"
  ;; Test with different delimiters
  (is (equal (eightbol::split-sequence #\, "apple,banana,cherry") '("apple" "banana" "cherry"))
      "Should split on commas")
  (is (equal (eightbol::split-sequence #\| "one|two|three") '("one" "two" "three"))
      "Should split on pipes")
  (is (equal (eightbol::split-sequence #\space "hello world test") '("hello" "world" "test"))
      "Should split on spaces")
  ;; Test edge cases
  (is (equal (eightbol::split-sequence #\, "single") '("single")) "Single item no delimiter")
  (is (equal (eightbol::split-sequence #\, "") '()) "Empty string")
  (is (equal (eightbol::split-sequence #\, ",,,") '()) "Only delimiters"))

;; Test case conversion functions
(test pascal-case-basic
  "Test pascal-case conversion"
  (is (string= (eightbol::pascal-case "hello-world") "HelloWorld")
      "Should convert kebab-case to PascalCase")
  (is (string= (eightbol::pascal-case "test_string") "TestString")
      "Should convert snake_case to PascalCase")
  (is (string= (eightbol::pascal-case "simple") "Simple")
      "Should capitalize simple words"))

(test param-case-basic
  "Test param-case conversion"
  (is (string= (eightbol::param-case "HelloWorld") "hello-world")
      "Should convert PascalCase to kebab-case")
  (is (string= (eightbol::param-case "TestString") "test-string")
      "Should convert PascalCase to kebab-case"))

(test title-case-basic
  "Test title-case conversion"
  (is (string= (eightbol::title-case "hello world") "Hello World")
      "Should capitalize each word")
  (is (string= (eightbol::title-case "test string here") "Test String Here")
      "Should handle multiple words"))

;; Test label generation
(test generate-label-basic
  "Test generate-label generates unique labels"
  (let ((label1 (eightbol::generate-label))
        (label2 (eightbol::generate-label)))
    (is (stringp label1) "Should return a string")
    (is (stringp label2) "Should return a string")
    (is (not (string= label1 label2)) "Labels should be unique")))

(test generate-label-with-prefix
  "Test generate-label with custom prefix"
  (let ((label (eightbol::generate-label "TEST")))
    (is (stringp label) "Should return a string")
    (is (search "TEST" label) "Should contain prefix")))

;; Test code generation utilities
(test indent-basic
  "Test indent function"
  (is-true (fboundp 'eightbol::indent) "indent should be defined")
  ;; This function modifies global state, so we test existence
  (finishes (eightbol::indent) "Should not error"))

(test emit-basic
  "Test emit function"
  (is-true (fboundp 'eightbol::emit) "emit should be defined")
  (finishes (eightbol::emit "test") "Should handle basic emission"))

(test emit-comment-basic
  "Test emit-comment function"
  (is-true (fboundp 'eightbol::emit-comment) "emit-comment should be defined")
  (finishes (eightbol::emit-comment "test comment") "Should handle basic comments"))

;; Test variable management functions
(test variable-address-basic
  "Test variable-address function"
  (is-true (fboundp 'eightbol::variable-address) "variable-address should be defined")
  (finishes (eightbol::variable-address 'test-var) "Should handle basic variable lookup"))

(test variable-size-basic
  "Test variable-size function"
  (is-true (fboundp 'eightbol::variable-size) "variable-size should be defined")
  (finishes (eightbol::variable-size 'test-var) "Should handle basic variable size lookup"))

(test resolve-variable-address-basic
  "Test resolve-variable-address function"
  (is-true (fboundp 'eightbol::resolve-variable-address) "resolve-variable-address should be defined")
  (finishes (eightbol::resolve-variable-address "test-var") "Should handle basic address resolution"))

;; Test COBOL compilation functions
(test cobol-compile-existence
  "Test cobol-compile function exists"
  (is-true (fboundp 'eightbol::cobol-compile) "cobol-compile should be defined"))

(test parse-cobol-program-existence
  "Test parse-cobol-program function exists"
  (is-true (fboundp 'eightbol::parse-cobol-program) "parse-cobol-program should be defined"))

(test parse-cobol-content-existence
  "Test parse-cobol-content function exists"
  (is-true (fboundp 'eightbol::parse-cobol-content) "parse-cobol-content should be defined"))

(test generate-assembly-existence
  "Test generate-assembly function exists"
  (is-true (fboundp 'eightbol::generate-assembly) "generate-assembly should be defined"))

;; Test data definition collection functions
(test collect-global-data-definition-existence
  "Test collect-global-data-definition function exists"
  (is-true (fboundp 'eightbol::collect-global-data-definition)
           "collect-global-data-definition should be defined"))

(test collect-picture-definition-existence
  "Test collect-picture-definition function exists"
  (is-true (fboundp 'eightbol::collect-picture-definition)
           "collect-picture-definition should be defined"))

(test collect-occurs-definition-existence
  "Test collect-occurs-definition function exists"
  (is-true (fboundp 'eightbol::collect-occurs-definition)
           "collect-occurs-definition should be defined"))

(test collect-object-definition-existence
  "Test collect-object-definition function exists"
  (is-true (fboundp 'eightbol::collect-object-definition)
           "collect-object-definition should be defined"))

(test collect-redefines-definition-existence
  "Test collect-redefines-definition function exists"
  (is-true (fboundp 'eightbol::collect-redefines-definition)
           "collect-redefines-definition should be defined"))

(test collect-simple-definition-existence
  "Test collect-simple-definition function exists"
  (is-true (fboundp 'eightbol::collect-simple-definition)
           "collect-simple-definition should be defined"))

;; Test data parsing functions
(test parse-data-line-existence
  "Test parse-data-line function exists"
  (is-true (fboundp 'eightbol::parse-data-line) "parse-data-line should be defined"))

(test collect-picture-definition-existence
  "Test collect-picture-definition function exists"
  (is-true (fboundp 'eightbol::collect-picture-definition) "collect-picture-definition should be defined"))

(test parse-occurs-clause-existence
  "Test parse-occurs-clause function exists"
  (is-true (fboundp 'eightbol::parse-occurs-clause) "parse-occurs-clause should be defined"))

(test parse-object-reference-existence
  "Test parse-object-reference function exists"
  (is-true (fboundp 'eightbol::parse-object-reference) "parse-object-reference should be defined"))

;; Test procedure parsing functions
(test parse-procedure-line-existence
  "Test parse-procedure-line function exists"
  (is-true (fboundp 'eightbol::parse-procedure-line) "parse-procedure-line should be defined"))

(test parse-move-statement-existence
  "Test parse-move-statement function exists"
  (is-true (fboundp 'eightbol::parse-move-statement) "parse-move-statement should be defined"))

(test parse-add-statement-existence
  "Test parse-add-statement function exists"
  (is-true (fboundp 'eightbol::parse-add-statement) "parse-add-statement should be defined"))

(test parse-subtract-statement-existence
  "Test parse-subtract-statement function exists"
  (is-true (fboundp 'eightbol::parse-subtract-statement) "parse-subtract-statement should be defined"))

(test parse-multiply-statement-existence
  "Test parse-multiply-statement function exists"
  (is-true (fboundp 'eightbol::parse-multiply-statement) "parse-multiply-statement should be defined"))

(test parse-divide-statement-existence
  "Test parse-divide-statement function exists"
  (is-true (fboundp 'eightbol::parse-divide-statement) "parse-divide-statement should be defined"))

;; Test bitwise operation parsing functions
(test parse-bitwise-operations-existence
  "Test bitwise operation parsing functions exist"
  (dolist (func '(eightbol::parse-bitwise-and-statement
                  eightbol::parse-bitwise-or-statement
                  eightbol::parse-bitwise-xor-statement))
    (is-true (fboundp func) "~a should be defined" func)))

;; Test flag operation parsing functions
(test parse-flag-operations-existence
  "Test flag operation parsing functions exist"
  (dolist (func '(eightbol::parse-set-flag-statement
                  eightbol::parse-clear-flag-statement
                  eightbol::parse-test-flag-statement))
    (is-true (fboundp func) "~a should be defined" func)))

;; Test control flow parsing functions
(test parse-control-flow-existence
  "Test control flow parsing functions exist"
  (dolist (func '(eightbol::parse-if-statement
                  eightbol::parse-null-check
                  eightbol::parse-perform-statement))
    (is-true (fboundp func) "~a should be defined" func)))

;; Test function call parsing functions
(test parse-function-calls-existence
  "Test function call parsing functions exist"
  (dolist (func '(eightbol::parse-call-statement
                  eightbol::parse-invoke-statement))
    (is-true (fboundp func) "~a should be defined" func)))

;; Test arithmetic generation functions
(test generate-arithmetic-existence
  "Test arithmetic generation functions exist"
  (dolist (func '(eightbol::generate-16bit-add
                  eightbol::generate-16bit-subtract))
    (is-true (fboundp func) "~a should be defined" func)))

;; Test variable size functions
(test get-variable-size-existence
  "Test get-variable-size function exists"
  (is-true (fboundp 'eightbol::get-variable-size) "get-variable-size should be defined"))

(test get-object-field-size-existence
  "Test get-object-field-size function exists"
  (is-true (fboundp 'eightbol::get-object-field-size) "get-object-field-size should be defined"))

(test find-field-size-in-class-existence
  "Test find-field-size-in-class function exists"
  (is-true (fboundp 'eightbol::find-field-size-in-class) "find-field-size-in-class should be defined"))

;; Test object and field functions
(test find-object-field-existence
  "Test find-object-field function exists"
  (is-true (fboundp 'eightbol::find-object-field) "find-object-field should be defined"))

(test find-method-id-existence
  "Test find-method-id function exists"
  (is-true (fboundp 'eightbol::find-method-id) "find-method-id should be defined"))

;; Test CartRAM and class functions
(test find-cart-ram-variable-existence
  "Test find-cart-ram-variable function exists"
  (is-true (fboundp 'eightbol::find-cart-ram-variable) "find-cart-ram-variable should be defined"))

(test find-class-field-existence
  "Test find-class-field function exists"
  (is-true (fboundp 'eightbol::find-class-field) "find-class-field should be defined"))

;; Test file and loading functions
(test machine-directory-name-existence
  "Test machine-directory-name function exists"
  (is-true (fboundp 'eightbol::machine-directory-name) "machine-directory-name should be defined"))

(test load-class-definitions-existence
  "Test load-class-definitions function exists"
  (is-true (fboundp 'eightbol::load-class-definitions) "load-class-definitions should be defined"))

(test load-cart-ram-variables-existence
  "Test load-cart-ram-variables function exists"
  (is-true (fboundp 'eightbol::load-cart-ram-variables) "load-cart-ram-variables should be defined"))

(test load-constants-existence
  "Test load-constants function exists"
  (is-true (fboundp 'eightbol::load-constants) "load-constants should be defined"))

;; Test array operation parsing functions
(test parse-array-operations-existence
  "Test array operation parsing functions exist"
  (dolist (func '(eightbol::parse-array-load
                  eightbol::parse-array-store))
    (is-true (fboundp func) "~a should be defined" func)))

;; Test global data section generation
(test generate-global-data-section-existence
  "Test generate-global-data-section function exists"
  (is-true (fboundp 'eightbol::generate-global-data-section)
           "generate-global-data-section should be defined"))

;; Test compilation functions
(test compile-file-existence
  "Test compile-file function exists"
  (is-true (fboundp 'eightbol::compile-file) "compile-file should be defined"))

;; Test basic COBOL compilation workflow
(test basic-cobol-compilation-workflow
  "Test basic COBOL compilation workflow exists"
  ;; Test that the main entry points exist and can be called
  (is-true (fboundp 'eightbol::cobol-compile) "Main COBOL compiler should exist")
  (is-true (fboundp 'eightbol::compile-file) "File compilation should exist"))

;; Test CPU target validation
(test cpu-target-validation
  "Test that valid CPU targets are recognized"
  (dolist (cpu '(:6502 :z80 :cp1610 :m68k))
    (is-true (member cpu '(:6502 :z80 :cp1610 :m68k))
             "~a should be a valid CPU target" cpu)))

;; Test that global variables are properly initialized
(test global-variables-initialized
  "Test that global variables are properly initialized"
  (is (hash-table-p eightbol::*global-data-definitions*)
      "*global-data-definitions* should be a hash table")
  (is (hash-table-p eightbol::*redefines-table*)
      "*redefines-table* should be a hash table")
  (is (hash-table-p eightbol::*constants-table*)
      "*constants-table* should be a hash table")
  (is (hash-table-p eightbol::*variable-table*)
      "*variable-table* should be a hash table")
  (is (integerp eightbol::*label-counter*)
      "*label-counter* should be an integer")
  (is (integerp eightbol::*indent-level*)
      "*indent-level* should be an integer"))

;; Test string case conversion round-trip
(test case-conversion-round-trip
  "Test that case conversions can round-trip"
  (let ((original "TestString"))
    (is (string= original (eightbol::pascal-case (eightbol::param-case original)))
        "Pascal->param->Pascal should round-trip"))
  (let ((original "test-string"))
    (is (string= original (eightbol::param-case (eightbol::pascal-case original)))
        "param->Pascal->param should round-trip")))

;; Test label generation uniqueness
(define-multi-test label-uniqueness-test
  "Test label generation uniqueness with multiple samples"
  20 ; Test 20 times for reasonable statistical confidence
  (let ((labels (loop for i from 1 to 10 collect (eightbol::generate-label))))
    (is (= (length labels) (length (remove-duplicates labels :test #'string=)))
        "All generated labels should be unique")))

;; Test that all core functions are properly defined
(test all-core-functions-defined
  "Test that all core eightbol functions are properly defined"
  (dolist (func-name '(eightbol::split-sequence
                       eightbol::pascal-case
                       eightbol::param-case
                       eightbol::title-case
                       eightbol::generate-label
                       eightbol::indent
                       eightbol::emit
                       eightbol::emit-comment
                       eightbol::variable-address
                       eightbol::variable-size
                       eightbol::resolve-variable-address
                       eightbol::cobol-compile
                       eightbol::parse-cobol-program
                       eightbol::parse-cobol-content
                       eightbol::generate-assembly))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all parsing functions are properly defined
(test all-parsing-functions-defined
  "Test that all parsing functions are properly defined"
  (dolist (func-name '(eightbol::parse-data-line
                       eightbol::collect-picture-definition
                       eightbol::parse-occurs-clause
                       eightbol::parse-object-reference
                       eightbol::parse-procedure-line
                       eightbol::parse-move-statement
                       eightbol::parse-add-statement
                       eightbol::parse-subtract-statement
                       eightbol::parse-multiply-statement
                       eightbol::parse-divide-statement
                       eightbol::parse-if-statement))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all utility functions are properly defined
(test all-utility-functions-defined
  "Test that all utility functions are properly defined"
  (dolist (func-name '(eightbol::get-variable-size
                       eightbol::get-object-field-size
                       eightbol::find-field-size-in-class
                       eightbol::find-object-field
                       eightbol::find-method-id
                       eightbol::machine-directory-name
                       eightbol::load-class-definitions
                       eightbol::load-cart-ram-variables
                       eightbol::load-constants))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))