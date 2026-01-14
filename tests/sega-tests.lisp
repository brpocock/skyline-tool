;;; Phantasia SkylineTool/tests/sega-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

;; Use the test package defined by the test runner
(in-package :skyline-tool/test)

(def-suite sega-tests
  :description "Tests for Sega SG-1000 and Master System-specific SkylineTool functionality")

(in-suite sega-tests)

;; Test Sega platform constants
(test sega-platform-constants
  "Test that Sega platform constants are properly defined"
  (is-true (member 1000 skyline-tool::*valid-machines*)
           "SG-1000 (machine 1000) should be in valid machines list")
  (is-true (member 3010 skyline-tool::*valid-machines*)
           "Master System (machine 3010) should be in valid machines list"))

;; Test Sega machine code validation
(test sega-machine-codes
  "Test Sega platform machine code validation"
  (is (= 1000 1000) "SG-1000 machine code should be 1000")
  (is (= 3010 3010) "Master System machine code should be 3010")

  ;; Test machine name functions with proper global state
  (let ((skyline-tool::*machine* 1000))
    (is-true (string= (skyline-tool::machine-short-name) "SG-1000")
             "SG-1000 should have correct short name"))

  (let ((skyline-tool::*machine* 3010))
    (is-true (string= (skyline-tool::machine-short-name) "SMS")
             "Master System should have correct short name")))

;; Test Sega music compilation functions
(test sega-music-compilation
  "Test Sega music compilation functions"
  ;; Test SG-1000 music compilation
  (signals error (skyline-tool::compile-music-sg1000 "/tmp/test.s" "/nonexistent.mid")
           "SG-1000 music compilation should signal error for missing MIDI file")

  ;; Test Master System music compilation
  (signals error (skyline-tool::compile-music-sms "/tmp/test.s" "/nonexistent.mid")
           "Master System music compilation should signal error for missing MIDI file"))

;; Test Sega music function existence
(test sega-music-functions-existence
  "Test that Sega music functions exist"
  (is-true (fboundp 'skyline-tool::compile-music-sg1000)
           "compile-music-sg1000 should exist")
  (is-true (fboundp 'skyline-tool::compile-music-sms)
           "compile-music-sms should exist"))

;; Test Sega platform dispatch recognition
(test sega-dispatch-recognition
  "Test that Sega platforms are recognized in dispatch system"
  (is-true (member 1000 skyline-tool::*valid-machines*)
           "SG-1000 should be recognized for general operations")
  (is-true (member 3010 skyline-tool::*valid-machines*)
           "Master System should be recognized for general operations"))

(defun run-sega-tests ()
  "Run all Sega tests and return results"
  (fiveam:run! 'sega-tests))
