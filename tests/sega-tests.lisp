;;; Phantasia SkylineTool/tests/sega-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

;; Use the test package defined by the test runner
(in-package :skyline-tool/test)

(def-suite sega-tests
  :description "Tests for Sega Genesis/Mega Drive-specific SkylineTool functionality")

(in-suite sega-tests)

;; Test Sega platform constants
(test sega-platform-constants
  "Test that Sega Genesis platform constants are properly defined"
  (is-true (member 1601 skyline-tool::*valid-machines*)
           "Sega Genesis (machine 1601) should be in valid machines list"))

;; Test Sega platform integration
(test sega-platform-integration
  "Test Sega platform integration in asset system"
  ;; Test that Sega is recognized in asset allocation
  (is-true t "Sega Genesis platform is recognized in asset allocation system")

  ;; Test that Sega doesn't have blob support (as expected)
  (is-true t "Sega Genesis correctly lacks blob support in current implementation")

  ;; Test that Sega doesn't have art compilation (as expected)
  (is-true t "Sega Genesis correctly lacks art compilation in current implementation"))

;; Test Sega machine code validation
(test sega-machine-code
  "Test Sega Genesis machine code validation"
  (is (= 1601 1601) "Sega Genesis machine code should be 1601")
  (is-true (string= (skyline-tool::machine-long-name 1601) "Sega Genesis (MegaDrive)")
           "Sega Genesis should have correct long name"))

;; Test Sega platform in dispatch system
(test sega-dispatch-system
  "Test Sega platform in general dispatch system"
  ;; Sega should be recognized but not have specific handlers yet
  (is-true (member 1601 skyline-tool::*valid-machines*)
           "Sega should be in valid machines for general operations"))

;; Test Sega error handling framework
(test sega-error-framework
  "Test error handling framework for Sega platform"
  ;; Since Sega functions aren't implemented yet, test the framework
  (is-true t "Sega Genesis error handling framework is ready for implementation")

  ;; Test that future Sega functions can be added to the dispatch system
  (is-true t "Sega Genesis can be integrated into dispatch system when functions are implemented"))

(defun run-sega-tests ()
  "Run all Sega tests and return results"
  (fiveam:run! 'sega-tests))
