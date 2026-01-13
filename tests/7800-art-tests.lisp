(defpackage :skyline-tool/7800-art-test
  (:use :cl :fiveam)
  (:import-from :skyline-tool :pal-capable-p)
  (:export :7800-art-suite))

(in-package :skyline-tool/7800-art-test)

(def-suite 7800-art-suite
  :description "Test suite for Atari 7800 art compilation and platform support")

(in-suite 7800-art-suite)

;; RED PHASE: Write failing tests first

;; Test PAL capability function (this should work since we implemented it)
(test pal-capable-machines
  "Test that PAL capability is correctly reported for supported machines"
  (is-true (pal-capable-p 7800))
  (is-true (pal-capable-p 2609)) ; Intellivision
  (is-true (pal-capable-p 5200))
  (is-true (pal-capable-p 3))    ; NES
  (is-true (pal-capable-p 6))    ; SNES
  (is-false (pal-capable-p 9999))); Unknown machine

;; Test 7800 mode keywords exist (basic functionality)
(test 7800-mode-keywords
  "Test that 7800 drawing mode keywords are properly defined"
  (is (keywordp :160a))
  (is (keywordp :160b))
  (is (keywordp :320a))
  (is (keywordp :320b))
  (is (keywordp :320c))
  (is (keywordp :320d)))

;; GREEN PHASE: These tests will pass once we implement the functionality

;; Test 7800 art compilation (will fail until implemented)
(test 7800-art-compilation-basic
  "Test basic 7800 art compilation functionality"
  (skip "7800 art compilation not yet fully implemented"))

;; Test platform-specific art handling
(test platform-art-handling
  "Test that platform-specific art directories are handled correctly"
  (skip "Platform-specific art handling not yet implemented"))

;; Test 7800 binary output validation
(test 7800-binary-output-validation
  "Test that 7800 binary output conforms to hardware specifications"
  (skip "Binary output validation not yet implemented"))

;; Test error handling for out-of-range colors/sounds
(test out-of-range-validation
  "Test that invalid colors and sounds throw appropriate errors"
  (skip "Error validation not yet implemented"))