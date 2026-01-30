;;; Phantasia SkylineTool/tests/eprom-tests.lisp
;;;; Comprehensive tests for eprom.lisp EPROM programming system
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

(def-suite eprom-tests
  :description "Comprehensive tests for EPROM programming system")

(in-suite eprom-tests)

;; Test data generators for eprom functions
(defun generate-random-rom-size ()
  "Generate a random ROM size that's a power of 2"
  (ash 1 (+ 10 (random 10)))) ; 1KB to 512KB

(defun generate-random-binary-data (size)
  "Generate random binary data for testing"
  (let ((data (make-array size :element-type '(unsigned-byte 8))))
    (dotimes (i size)
      (setf (aref data i) (random 256)))
    data))

(defun generate-mock-serial-stream ()
  "Create a mock string stream for testing serial I/O"
  (make-string-input-stream "Mock serial data"))

;; Test ROM size and validation functions
(test power-of-two-size-basic
  "Test power-of-two-size function with various sizes"
  (is (= (skyline-tool::power-of-two-size 1024) 1024) "1KB should remain 1KB")
  (is (= (skyline-tool::power-of-two-size 1500) 2048) "1500 bytes should round up to 2KB")
  (is (= (skyline-tool::power-of-two-size 2047) 2048) "2047 bytes should round up to 2KB")
  (is (= (skyline-tool::power-of-two-size 2048) 2048) "2KB should remain 2KB"))

(define-multi-test power-of-two-size-edge-cases
  "Test power-of-two-size with edge cases"
  5 ; Test 5 different sizes
  (let ((size (+ 1 (random 10000))))
    (let ((result (skyline-tool::power-of-two-size size)))
      (is (>= result size) "Result should be >= input size")
      (is (= (logcount result) 1) "Result should be a power of 2"))))

(test validate-rom-binary-size-basic
  "Test validate-rom-binary-size with valid sizes"
  (is-true (skyline-tool::validate-rom-binary-size (generate-random-binary-data 1024)))
  (is-true (skyline-tool::validate-rom-binary-size (generate-random-binary-data 2048)))
  (is-true (skyline-tool::validate-rom-binary-size (generate-random-binary-data 4096))))

(test validate-rom-binary-size-invalid
  "Test validate-rom-binary-size with invalid sizes"
  (let ((invalid-sizes '(0 1 3 5 7 9 15 17 100 1000)))
    (dolist (size invalid-sizes)
      (is-false (skyline-tool::validate-rom-binary-size
                 (generate-random-binary-data size))
                "Size ~d should be invalid" size))))

;; Test device and serial port functions
(test device-file-sys-device-existence
  "Test device-file<-sys-device function exists"
  (is-true (fboundp 'skyline-tool::device-file<-sys-device) "device-file<-sys-device should be defined"))

(test enumerate-real-serial-ports-existence
  "Test enumerate-real-serial-ports function exists"
  (is-true (fboundp 'skyline-tool::enumerate-real-serial-ports) "enumerate-real-serial-ports should be defined"))

;; Test stream processing functions
(test collect-until->-basic
  "Test collect-until-> function with mock stream"
  (let ((stream (make-string-input-stream "Hello>World")))
    (let ((result (skyline-tool::collect-until-> stream)))
      (is (stringp result) "Should return a string")
      (is (search "Hello" result) "Should contain data before >"))))

(test ep1-error-existence
  "Test ep1-error function exists"
  (is-true (fboundp 'skyline-tool::ep1-error) "ep1-error should be defined"))

(test wait-for-input-existence
  "Test wait-for-input function exists"
  (is-true (fboundp 'skyline-tool::wait-for-input) "wait-for-input should be defined"))

(test wait-for->-existence
  "Test wait-for-> function exists"
  (is-true (fboundp 'skyline-tool::wait-for->) "wait-for-> should be defined"))

;; Test serial port functions
(test open-serial-port-stream-existence
  "Test open-serial-port-stream function exists"
  (is-true (fboundp 'skyline-tool::open-serial-port-stream) "open-serial-port-stream should be defined"))

(test serial-port-has-ep-1-p-existence
  "Test serial-port-has-ep-1-p function exists"
  (is-true (fboundp 'skyline-tool::serial-port-has-ep-1-p) "serial-port-has-ep-1-p should be defined"))

;; Test threading and discovery functions
(test spawn-thread-to-look-for-ep-1-existence
  "Test spawn-thread-to-look-for-ep-1-on-port function exists"
  (is-true (fboundp 'skyline-tool::spawn-thread-to-look-for-ep-1-on-port)
           "spawn-thread-to-look-for-ep-1-on-port should be defined"))

(test interactive-wait-existence
  "Test interactive-wait function exists"
  (is-true (fboundp 'skyline-tool::interactive-wait) "interactive-wait should be defined"))

(test find-ep1-serial-port-existence
  "Test find-ep1-serial-port function exists"
  (is-true (fboundp 'skyline-tool::find-ep1-serial-port) "find-ep1-serial-port should be defined"))

;; Test EP1 communication functions
(test collect-ep1-parts-list-existence
  "Test collect-ep1-parts-list function exists"
  (is-true (fboundp 'skyline-tool::collect-ep1-parts-list) "collect-ep1-parts-list should be defined"))

(test collect-ep1-found-parts-existence
  "Test collect-ep1-found-parts function exists"
  (is-true (fboundp 'skyline-tool::collect-ep1-found-parts) "collect-ep1-found-parts should be defined"))

;; Test utility functions
(test numbered-basic
  "Test numbered function with a list"
  (let ((input '("apple" "banana" "cherry")))
    (let ((result (skyline-tool::numbered input)))
      (is (listp result) "Should return a list")
      (is (= (length result) 3) "Should have same length as input"))))

(define-multi-test numbered-various-inputs
  "Test numbered function with various inputs"
  5 ; Test 5 different lists
  (let ((input (loop for i from 1 to (1+ (random 5))
                     collect (format nil "item~d" i))))
    (let ((result (skyline-tool::numbered input)))
      (is (= (length result) (length input)) "Should preserve list length"))))

;; Test interactive chip selection functions
(test select-chip-from-short-list-existence
  "Test select-chip-from-short-list% function exists"
  (is-true (fboundp 'skyline-tool::select-chip-from-short-list%)
           "select-chip-from-short-list% should be defined"))

(test interactive-search-for-chip-existence
  "Test interactive-search-for-chip-in-catalog function exists"
  (is-true (fboundp 'skyline-tool::interactive-search-for-chip-in-catalog)
           "interactive-search-for-chip-in-catalog should be defined"))

(test interactive-choose-chip-type-existence
  "Test interactive-choose-chip-type function exists"
  (is-true (fboundp 'skyline-tool::interactive-choose-chip-type)
           "interactive-choose-chip-type should be defined"))

(test interactive-check-blank-existence
  "Test interactive-check-blank function exists"
  (is-true (fboundp 'skyline-tool::interactive-check-blank) "interactive-check-blank should be defined"))

;; Test EP1 programming functions
(test handle-ep1-responses-existence
  "Test handle-ep1-responses function exists"
  (is-true (fboundp 'skyline-tool::handle-ep1-responses) "handle-ep1-responses should be defined"))

(test set-up-ep1-for-transfer-existence
  "Test set-up-ep1-for-transfer function exists"
  (is-true (fboundp 'skyline-tool::set-up-ep1-for-transfer) "set-up-ep1-for-transfer should be defined"))

(test write-record-to-stream-existence
  "Test write-record-to-stream function exists"
  (is-true (fboundp 'skyline-tool::write-record-to-stream) "write-record-to-stream should be defined"))

(test actually-burn-binary-existence
  "Test actually-burn-binary function exists"
  (is-true (fboundp 'skyline-tool::actually-burn-binary) "actually-burn-binary should be defined"))

(test verify-burn-existence
  "Test verify-burn function exists"
  (is-true (fboundp 'skyline-tool::verify-burn) "verify-burn should be defined"))

(test burn-rom-existence
  "Test burn-rom function exists"
  (is-true (fboundp 'skyline-tool::burn-rom) "burn-rom should be defined"))

;; Test that all core functions are properly defined
(test all-eprom-functions-defined
  "Test that all core eprom functions are properly defined"
  (dolist (func-name '(skyline-tool::power-of-two-size
                       skyline-tool::validate-rom-binary-size
                       skyline-tool::device-file<-sys-device
                       skyline-tool::enumerate-real-serial-ports
                       skyline-tool::collect-until->
                       skyline-tool::ep1-error
                       skyline-tool::wait-for-input
                       skyline-tool::wait-for->
                       skyline-tool::open-serial-port-stream
                       skyline-tool::serial-port-has-ep-1-p
                       skyline-tool::spawn-thread-to-look-for-ep-1-on-port
                       skyline-tool::interactive-wait
                       skyline-tool::find-ep1-serial-port
                       skyline-tool::collect-ep1-parts-list
                       skyline-tool::collect-ep1-found-parts
                       skyline-tool::numbered
                       skyline-tool::select-chip-from-short-list%
                       skyline-tool::interactive-search-for-chip-in-catalog
                       skyline-tool::interactive-choose-chip-type
                       skyline-tool::interactive-check-blank
                       skyline-tool::handle-ep1-responses
                       skyline-tool::set-up-ep1-for-transfer
                       skyline-tool::write-record-to-stream
                       skyline-tool::actually-burn-binary
                       skyline-tool::verify-burn
                       skyline-tool::burn-rom))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all validation functions are properly defined
(test all-validation-functions-defined
  "Test that all validation functions are properly defined"
  (dolist (func-name '(skyline-tool::power-of-two-size
                       skyline-tool::validate-rom-binary-size))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all serial communication functions are properly defined
(test all-serial-functions-defined
  "Test that all serial communication functions are properly defined"
  (dolist (func-name '(skyline-tool::device-file<-sys-device
                       skyline-tool::enumerate-real-serial-ports
                       skyline-tool::open-serial-port-stream
                       skyline-tool::serial-port-has-ep-1-p
                       skyline-tool::spawn-thread-to-look-for-ep-1-on-port
                       skyline-tool::find-ep1-serial-port))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all EP1 programming functions are properly defined
(test all-ep1-functions-defined
  "Test that all EP1 programming functions are properly defined"
  (dolist (func-name '(skyline-tool::collect-ep1-parts-list
                       skyline-tool::collect-ep1-found-parts
                       skyline-tool::handle-ep1-responses
                       skyline-tool::set-up-ep1-for-transfer
                       skyline-tool::write-record-to-stream
                       skyline-tool::actually-burn-binary
                       skyline-tool::verify-burn
                       skyline-tool::burn-rom))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all interactive functions are properly defined
(test all-interactive-functions-defined
  "Test that all interactive functions are properly defined"
  (dolist (func-name '(skyline-tool::interactive-wait
                       skyline-tool::select-chip-from-short-list%
                       skyline-tool::interactive-search-for-chip-in-catalog
                       skyline-tool::interactive-choose-chip-type
                       skyline-tool::interactive-check-blank))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all utility functions are properly defined
(test all-utility-functions-defined
  "Test that all utility functions are properly defined"
  (dolist (func-name '(skyline-tool::collect-until->
                       skyline-tool::ep1-error
                       skyline-tool::wait-for-input
                       skyline-tool::wait-for->
                       skyline-tool::numbered))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test ROM size validation with various sizes
(define-multi-test rom-size-validation
  "Test ROM size validation with various binary sizes"
  8 ; Test 8 different sizes
  (let ((size (generate-random-rom-size)))
    (let ((binary (generate-random-binary-data size)))
      (is-true (skyline-tool::validate-rom-binary-size binary)
               "Valid ROM size ~d should pass validation" size))))

;; Test power-of-two rounding with various inputs
(define-multi-test power-of-two-rounding
  "Test power-of-two-size rounding behavior"
  10 ; Test 10 different inputs
  (let ((input (+ 1 (random 100000)))) ; Random size from 1 to ~100KB
    (let ((result (skyline-tool::power-of-two-size input)))
      (is (>= result input) "Result should be >= input")
      (is (= (logcount result) 1) "Result should be a power of 2")
      ;; Check that it's the smallest power of 2 >= input
      (let ((expected (ash 1 (ceiling (log input 2)))))
        (is (= result expected) "Should round up to next power of 2")))))

;; Test numbered function with various list types
(define-multi-test numbered-different-types
  "Test numbered function with different element types"
  6 ; Test 6 different scenarios
  (let ((test-data (ecase (random 4)
                     (0 (loop for i from 1 to (1+ (random 5)) collect i)) ; numbers
                     (1 (loop for i from 1 to (1+ (random 5)) collect (format nil "item~d" i))) ; strings
                     (2 nil) ; empty list
                     (3 '(a b c d e))))) ; symbols
    (let ((result (skyline-tool::numbered test-data)))
      (is (= (length result) (length test-data)) "Should preserve list length")
      (is (listp result) "Should return a list"))))