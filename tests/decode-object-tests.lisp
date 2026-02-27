;;; Phantasia SkylineTool/tests/decode-object-tests.lisp
;;;; Comprehensive tests for decode-object.lisp object decoding and debugging
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

(def-suite decode-object-tests
  :description "Comprehensive tests for decode-object object decoding and debugging"
  :in skyline-tool/test)

(in-suite decode-object-tests)

;; Test data generators for decode-object functions
(defun generate-random-memory-address ()
  "Generate a random memory address"
  (random #x10000)) ; 64KB address space

(defun generate-random-class-id ()
  "Generate a random class ID"
  (random 256))

(defun generate-random-bam-block ()
  "Generate a random BAM block number"
  (random 256))

(defun generate-random-inventory-number ()
  "Generate a random inventory item number"
  (random 256))

(defun generate-mock-memory-dump (&optional (size 1024))
  "Generate a mock memory dump for testing"
  (let ((dump (make-array size :element-type '(unsigned-byte 8))))
    (dotimes (i size)
      (setf (aref dump i) (random 256)))
    dump))

;; Test class and method loading functions
(test read-class-methods-from-file-existence
  "Test read-class-methods-from-file function exists"
  (is-true (fboundp 'skyline-tool::read-class-methods-from-file) "read-class-methods-from-file should be defined"))

(test read-class-ids-from-file-existence
  "Test read-class-ids-from-file function exists"
  (is-true (fboundp 'skyline-tool::read-class-ids-from-file) "read-class-ids-from-file should be defined"))

(test dereference-class-existence
  "Test dereference-class function exists"
  (is-true (fboundp 'skyline-tool::dereference-class) "dereference-class should be defined"))

(test read-class-fields-from-defs-existence
  "Test read-class-fields-from-defs function exists"
  (is-true (fboundp 'skyline-tool::read-class-fields-from-defs) "read-class-fields-from-defs should be defined"))

;; Test inventory management functions
(test load-inventory-items-existence
  "Test load-inventory-items function exists"
  (is-true (fboundp 'skyline-tool::load-inventory-items) "load-inventory-items should be defined"))

(test inventory-item-name-existence
  "Test inventory-item-name function exists"
  (is-true (fboundp 'skyline-tool::inventory-item-name) "inventory-item-name should be defined"))

;; Test numeric conversion functions
(test 8.8-float-existence
  "Test 8.8-float function exists"
  (is-true (fboundp 'skyline-tool::8.8-float) "8.8-float should be defined"))

(test print-field-value-existence
  "Test print-field-value function exists"
  (is-true (fboundp 'skyline-tool::print-field-value) "print-field-value should be defined"))

;; Test object decoding functions
(test decode-object-existence
  "Test decode-object function exists"
  (is-true (fboundp 'skyline-tool::decode-object) "decode-object should be defined"))

(test decode-object-at-existence
  "Test decode-object-at function exists"
  (is-true (fboundp 'skyline-tool::decode-object-at) "decode-object-at should be defined"))

(test decode-all-objects-existence
  "Test decode-all-objects function exists"
  (is-true (fboundp 'skyline-tool::decode-all-objects) "decode-all-objects should be defined"))

;; Test address conversion functions
(test object-address-to-bam-block-basic
  "Test object-address->bam-block function"
  (is (= (skyline-tool::object-address->bam-block 0) 0) "Address 0 should map to BAM block 0")
  (is (= (skyline-tool::object-address->bam-block #x1000) 1) "Address 0x1000 should map to BAM block 1")
  (is (= (skyline-tool::object-address->bam-block #x2000) 2) "Address 0x2000 should map to BAM block 2"))

(define-multi-test address-bam-conversion
  "Test address to BAM block conversion with various addresses"
  10 ; Test 10 different addresses
  (let ((address (* (random 256) #x1000))) ; Valid BAM block addresses
    (let ((bam-block (skyline-tool::object-address->bam-block address)))
      (is (= bam-block (/ address #x1000)) "Address should convert to correct BAM block"))))

(test bam-block-to-object-address-basic
  "Test bam-block->object-address function"
  (is (= (skyline-tool::bam-block->object-address 0) 0) "BAM block 0 should map to address 0")
  (is (= (skyline-tool::bam-block->object-address 1) #x1000) "BAM block 1 should map to address 0x1000")
  (is (= (skyline-tool::bam-block->object-address 2) #x2000) "BAM block 2 should map to address 0x2000"))

(define-multi-test bam-address-conversion
  "Test BAM block to address conversion with various blocks"
  8 ; Test 8 different BAM blocks
  (let ((bam-block (random 256)))
    (let ((address (skyline-tool::bam-block->object-address bam-block)))
      (is (= address (* bam-block #x1000)) "BAM block should convert to correct address"))))

;; Test object size and analysis functions
(test size-of-object-at-existence
  "Test size-of-object-at function exists"
  (is-true (fboundp 'skyline-tool::size-of-object-at) "size-of-object-at should be defined"))

(test mark-object-visited-existence
  "Test mark-object-visited function exists"
  (is-true (fboundp 'skyline-tool::mark-object-visited) "mark-object-visited should be defined"))

(test mark-and-sweep-objects-existence
  "Test mark-and-sweep-objects function exists"
  (is-true (fboundp 'skyline-tool::mark-and-sweep-objects) "mark-and-sweep-objects should be defined"))

(test room-for-objects-existence
  "Test room-for-objects function exists"
  (is-true (fboundp 'skyline-tool::room-for-objects) "room-for-objects should be defined"))

;; Test special object decoding functions
(test decode-self-object-existence
  "Test decode-self-object function exists"
  (is-true (fboundp 'skyline-tool::decode-self-object) "decode-self-object should be defined"))

(test decode-player-object-existence
  "Test decode-player-object function exists"
  (is-true (fboundp 'skyline-tool::decode-player-object) "decode-player-object should be defined"))

;; Test display/debugging functions
(test show-self-object-existence
  "Test show-self-object function exists"
  (is-true (fboundp 'skyline-tool::show-self-object) "show-self-object should be defined"))

(test show-all-objects-existence
  "Test show-all-objects function exists"
  (is-true (fboundp 'skyline-tool::show-all-objects) "show-all-objects should be defined"))

(test show-player-object-existence
  "Test show-player-object function exists"
  (is-true (fboundp 'skyline-tool::show-player-object) "show-player-object should be defined"))

(test show-room-for-objects-existence
  "Test show-room-for-objects function exists"
  (is-true (fboundp 'skyline-tool::show-room-for-objects) "show-room-for-objects should be defined"))

;; Test Forth stack functions
(test echo-forth-stack-existence
  "Test echo-forth-stack function exists"
  (is-true (fboundp 'skyline-tool::echo-forth-stack) "echo-forth-stack should be defined"))

(test show-forth-stack-existence
  "Test show-forth-stack function exists"
  (is-true (fboundp 'skyline-tool::show-forth-stack) "show-forth-stack should be defined"))

;; Test dialogue functions
(test decode-dialogue-existence
  "Test decode-dialogue function exists"
  (is-true (fboundp 'skyline-tool::decode-dialogue) "decode-dialogue should be defined"))

(test show-dialogue-buffers-existence
  "Test show-dialogue-buffers function exists"
  (is-true (fboundp 'skyline-tool::show-dialogue-buffers) "show-dialogue-buffers should be defined"))

;; Test address range validation
(define-multi-test address-range-validation
  "Test that address conversion functions handle valid ranges"
  12 ; Test 12 different addresses
  (let ((address (generate-random-memory-address)))
    (let ((bam-block (skyline-tool::object-address->bam-block address))
          (back-to-address (skyline-tool::bam-block->object-address
                           (skyline-tool::object-address->bam-block address))))
      (is (<= 0 bam-block 255) "BAM block should be in valid range")
      (is (= (mod address #x1000) (mod back-to-address #x1000))
          "Address conversion should preserve offset within BAM block"))))

;; Test inventory item name generation
(define-multi-test inventory-item-names
  "Test inventory-item-name with various item numbers"
  8 ; Test 8 different item numbers
  (let ((item-number (generate-random-inventory-number)))
    (finishes (skyline-tool::inventory-item-name item-number)
              "Should handle various item numbers")))

;; Test 8.8 fixed-point conversion
(define-multi-test 8.8-fixed-point-conversion
  "Test 8.8-float conversion with various values"
  10 ; Test 10 different values
  (let ((value (random 65536))) ; 16-bit range
    (finishes (skyline-tool::8.8-float value)
              "Should handle various 16-bit values")))

;; Test class ID dereferencing
(define-multi-test class-id-dereferencing
  "Test dereference-class with various IDs"
  6 ; Test 6 different class IDs
  (let ((class-id (generate-random-class-id)))
    (finishes (skyline-tool::dereference-class class-id)
              "Should handle various class IDs")))

;; Test that all core functions are properly defined
(test all-decode-object-functions-defined
  "Test that all core decode-object functions are properly defined"
  (dolist (func-name '(skyline-tool::read-class-methods-from-file
                       skyline-tool::read-class-ids-from-file
                       skyline-tool::dereference-class
                       skyline-tool::read-class-fields-from-defs
                       skyline-tool::load-inventory-items
                       skyline-tool::inventory-item-name
                       skyline-tool::8.8-float
                       skyline-tool::print-field-value
                       skyline-tool::decode-object
                       skyline-tool::decode-object-at
                       skyline-tool::decode-all-objects
                       skyline-tool::object-address->bam-block
                       skyline-tool::bam-block->object-address
                       skyline-tool::size-of-object-at
                       skyline-tool::mark-object-visited
                       skyline-tool::mark-and-sweep-objects
                       skyline-tool::room-for-objects
                       skyline-tool::decode-self-object
                       skyline-tool::decode-player-object
                       skyline-tool::show-self-object
                       skyline-tool::show-all-objects
                       skyline-tool::show-player-object
                       skyline-tool::show-room-for-objects
                       skyline-tool::echo-forth-stack
                       skyline-tool::show-forth-stack
                       skyline-tool::decode-dialogue
                       skyline-tool::show-dialogue-buffers))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all file I/O functions are properly defined
(test all-file-io-functions-defined
  "Test that all file I/O functions are properly defined"
  (dolist (func-name '(skyline-tool::read-class-methods-from-file
                       skyline-tool::read-class-ids-from-file
                       skyline-tool::load-inventory-items))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all decoding functions are properly defined
(test all-decoding-functions-defined
  "Test that all decoding functions are properly defined"
  (dolist (func-name '(skyline-tool::decode-object
                       skyline-tool::decode-object-at
                       skyline-tool::decode-all-objects
                       skyline-tool::decode-self-object
                       skyline-tool::decode-player-object
                       skyline-tool::decode-dialogue))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all display/debugging functions are properly defined
(test all-display-functions-defined
  "Test that all display/debugging functions are properly defined"
  (dolist (func-name '(skyline-tool::show-self-object
                       skyline-tool::show-all-objects
                       skyline-tool::show-player-object
                       skyline-tool::show-room-for-objects
                       skyline-tool::echo-forth-stack
                       skyline-tool::show-forth-stack
                       skyline-tool::show-dialogue-buffers))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all memory analysis functions are properly defined
(test all-memory-functions-defined
  "Test that all memory analysis functions are properly defined"
  (dolist (func-name '(skyline-tool::size-of-object-at
                       skyline-tool::mark-object-visited
                       skyline-tool::mark-and-sweep-objects
                       skyline-tool::room-for-objects))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all conversion functions are properly defined
(test all-conversion-functions-defined
  "Test that all conversion functions are properly defined"
  (dolist (func-name '(skyline-tool::object-address->bam-block
                       skyline-tool::bam-block->object-address
                       skyline-tool::8.8-float
                       skyline-tool::dereference-class
                       skyline-tool::inventory-item-name))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test BAM block conversion round-trip
(define-multi-test bam-conversion-round-trip
  "Test that BAM block conversions are reversible"
  15 ; Test 15 different values
  (let ((original-address (generate-random-memory-address)))
    (let* ((bam-block (skyline-tool::object-address->bam-block original-address))
           (back-to-address (skyline-tool::bam-block->object-address bam-block)))
      (is (= (floor original-address #x1000) bam-block)
          "Address should convert to correct BAM block")
      (is (= (* bam-block #x1000) back-to-address)
          "BAM block should convert back to base address"))))

;; Test that functions handle mock data appropriately
(define-multi-test mock-data-handling
  "Test functions with mock memory dump data"
  5 ; Test 5 different scenarios
  (let ((mock-dump (generate-mock-memory-dump)))
    (finishes (skyline-tool::decode-object-at mock-dump 0)
              "Should handle mock memory dump")
    (finishes (skyline-tool::size-of-object-at mock-dump 0)
              "Should handle size calculation on mock data")))

;; Test print-field-value with various data types
(define-multi-test field-value-printing
  "Test print-field-value with various data types and names"
  10 ; Test 10 different combinations
  (let ((field-name (format nil "field~d" (random 100)))
        (field-value (case (random 4)
                       (0 (random 1000))     ; integer
                       (1 (random 1.0))      ; float
                       (2 "test string")      ; string
                       (3 nil))))            ; nil
    (finishes (skyline-tool::print-field-value field-name field-value nil)
              "Should handle various field types")))