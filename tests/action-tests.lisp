(defpackage :skyline-tool/test
  (:use :cl :skyline-tool :fiveam)
  (:export #:action-tests))
(in-package :skyline-tool/test)

#|(shadowing-import '(fiveam:def-suite fiveam:test fiveam:is fiveam:in-suite))|#

(def-suite action-tests
  :description "Tests for character actions: gesture, panic, flying, wave-arms, dancing")

(in-suite action-tests)

;; Smoke test to ensure suite registration works
(test action-suite-smoke
  (is (= 0 0)))

;; Helper function for testing - avoid make dependencies
(defun compile-script-from-string (script-string)
  "Compile a Fountain script from a string and return the Forth output"
  (let* ((root (asdf:system-source-directory :skyline-tool))
         (npc-path (merge-pathnames "tests/data/NPCStats.ods" root))
         (map-source (merge-pathnames "tests/data/AmsCalypso2.tmx" root))
         (map-target (merge-pathnames "Source/Maps/Ships/AmsCalypso2.tmx" root))
         (map-target-dir (uiop:pathname-directory-pathname map-target))
         (created-map nil)
         (created-dir nil))
    (with-output-to-string (output)
      (let ((*standard-output* output)
            (skyline-tool::*common-palette*
              (append '("purple" "peach" "brown" "yellow" "green")
                      skyline-tool::*common-palette*)))
        (let ((*npc-stats* nil))
          (when (probe-file npc-path)
            (skyline-tool::load-npc-stats npc-path))
          (when (probe-file map-source)
            (unless (uiop:directory-exists-p map-target-dir)
              (setf created-dir t)
              (ensure-directories-exist map-target-dir))
            (unless (probe-file map-target)
            (setf created-map t)
              (uiop:copy-file map-source map-target)))
          (unwind-protect
               (skyline-tool::compile-fountain-string script-string)
            (when created-map
              (ignore-errors (delete-file map-target)))
            (when (and created-dir map-target-dir)
              ;; Try to remove directory if empty (ignore errors if not empty or other issues)
              (ignore-errors
                (when (uiop:directory-exists-p map-target-dir)
                  (let ((files (uiop:directory-files map-target-dir)))
                    (when (null files)
                      (uiop:delete-empty-directory map-target-dir))))))))))))


;; Test gesture actions
(test gesture-starts-gesturing
  "Test 'starts gesturing' grammar"
  (let* ((script "INT SHIPS - AMS CALYPSO2

Open on: NORVILLE is at \"ChefCorner\". Then…

NORVILLE starts gesturing.

> THE END")
         (forth-output (compile-script-from-string script)))
    (is (search "ActionGesture" forth-output))
    (is (search "CharacterID_Norville" forth-output))
    (is (search "character-action!" forth-output))))

(test gesture-gestures
  "Test 'gestures' grammar"
  (let* ((script "INT SHIPS - AMS CALYPSO2

Open on: NORVILLE is at \"ChefCorner\". Then…

NORVILLE gestures.

> THE END")
         (forth-output (compile-script-from-string script)))
    (is (search "ActionGesture" forth-output))
    (is (search "CharacterID_Norville" forth-output))
    (is (search "character-action!" forth-output))))

(test gesture-stops-gesturing
  "Test 'stops gesturing' grammar"
  (let* ((script "INT SHIPS - AMS CALYPSO2

Open on: NORVILLE is at \"ChefCorner\". Then…

NORVILLE stops gesturing.

> THE END")
         (forth-output (compile-script-from-string script)))
    (is (search "ActionIdle" forth-output))
    (is (search "CharacterID_Norville" forth-output))
    (is (search "character-action!" forth-output))))

;; Test panic actions
(test panic-starts-panicking
  "Test 'starts panicking' grammar"
  (let* ((script "INT SHIPS - AMS CALYPSO2

Open on: NORVILLE is at \"ChefCorner\". Then…

NORVILLE starts panicking.

> THE END")
         (forth-output (compile-script-from-string script)))
    (is (search "ActionPanic" forth-output))
    (is (search "CharacterID_Norville" forth-output))
    (is (search "character-action!" forth-output))))

(test panic-panics
  "Test 'panics' grammar"
  (let* ((script "INT SHIPS - AMS CALYPSO2

Open on: NORVILLE is at \"ChefCorner\". Then…

NORVILLE panics.

> THE END")
         (forth-output (compile-script-from-string script)))
    (is (search "ActionPanic" forth-output))
    (is (search "CharacterID_Norville" forth-output))
    (is (search "character-action!" forth-output))))

(test panic-stops-panicking
  "Test 'stops panicking' grammar"
  (let* ((script "INT SHIPS - AMS CALYPSO2

Open on: NORVILLE is at \"ChefCorner\". Then…

NORVILLE stops panicking.

> THE END")
         (forth-output (compile-script-from-string script)))
    (is (search "ActionIdle" forth-output))
    (is (search "CharacterID_Norville" forth-output))
    (is (search "character-action!" forth-output))))

;; Test flying actions (using PARROT as instructed)
(test flying-starts-flying
  "Test 'starts flying' grammar"
  (let* ((script "INT SHIPS - AMS CALYPSO2

Open on: PARROT is at \"ChefCorner\". Then…

PARROT starts flying.

> THE END")
         (forth-output (compile-script-from-string script)))
    (is (search "ActionFlying" forth-output))
    (is (search "CharacterID_Parrot" forth-output))
    (is (search "character-action!" forth-output))))

(test flying-flies
  "Test 'flies' grammar"
  (let* ((script "INT SHIPS - AMS CALYPSO2

Open on: PARROT is at \"ChefCorner\". Then…

PARROT flies.

> THE END")
         (forth-output (compile-script-from-string script)))
    (is (search "ActionFlying" forth-output))
        (is (search "CharacterID_Parrot" forth-output))
    (is (search "character-action!" forth-output))))

(test flying-stops-flying
  "Test 'stops flying' grammar"
      (let* ((script "INT SHIPS - AMS CALYPSO2

Open on: PARROT is at \"ChefCorner\". Then…

PARROT stops flying.

> THE END")
         (forth-output (compile-script-from-string script)))
    (is (search "ActionIdle" forth-output))
    (is (search "CharacterID_Parrot" forth-output))
    (is (search "character-action!" forth-output))))

;; Test dancing actions
(test dancing-starts-dancing
  "Test 'starts dancing' grammar"
  (let* ((script "INT SHIPS - AMS CALYPSO2

Open on: NORVILLE is at \"ChefCorner\". Then…

NORVILLE starts dancing.

> THE END")
         (forth-output (compile-script-from-string script)))
    (is (search "ActionDance" forth-output))
    (is (search "CharacterID_Norville" forth-output))
    (is (search "character-action!" forth-output))))

(test dancing-dances
  "Test 'dances' grammar"
  (let* ((script "INT SHIPS - AMS CALYPSO2

Open on: NORVILLE is at \"ChefCorner\". Then…

NORVILLE dances.

> THE END")
         (forth-output (compile-script-from-string script)))
    (is (search "ActionDance" forth-output))
    (is (search "CharacterID_Norville" forth-output))
    (is (search "character-action!" forth-output))))

(test dancing-stops-dancing
  "Test 'stops dancing' grammar"
      (let* ((script "INT SHIPS - AMS CALYPSO2

Open on: NORVILLE is at \"ChefCorner\". Then…

NORVILLE stops dancing.

> THE END")
             (forth-output (compile-script-from-string script)))
    (is (search "ActionIdle" forth-output))
    (is (search "CharacterID_Norville" forth-output))
    (is (search "character-action!" forth-output))))

;; Test walking actions
(test walking-starts-walking
  "Test 'starts walking' grammar"
  (let* ((script "INT SHIPS - AMS CALYPSO2

Open on: NORVILLE is at \"ChefCorner\". Then…

NORVILLE starts walking to \"AcrossCounterFromChef\".

> THE END")
         (forth-output (compile-script-from-string script)))
    (is (search "do-walk" forth-output))
    (is (search "CharacterID_Norville" forth-output))
    (is (search "do-walk" forth-output))))

;; Test wave-arms actions
(test wave-arms-starts-waving
  "Test 'starts waving' grammar"
  (let* ((script "INT SHIPS - AMS CALYPSO2

Open on: NORVILLE is at \"ChefCorner\". Then…

NORVILLE starts waving.

> THE END")
         (forth-output (compile-script-from-string script)))
    (is (search "ActionWaveArms" forth-output))
    (is (search "CharacterID_Norville" forth-output))
    (is (search "character-action!" forth-output))))

(test wave-arms-starts-waving-arms
  "Test 'starts waving arms' grammar"
  (let* ((script "INT SHIPS - AMS CALYPSO2

Open on: NORVILLE is at \"ChefCorner\". Then…

NORVILLE starts waving arms.

> THE END")
         (forth-output (compile-script-from-string script)))
    (is (search "ActionWaveArms" forth-output))
    (is (search "CharacterID_Norville" forth-output))
    (is (search "character-action!" forth-output))))

(test wave-arms-starts-waving-his-arms
  "Test 'starts waving his arms' grammar"
  (let* ((script "INT SHIPS - AMS CALYPSO2

Open on: NORVILLE is at \"ChefCorner\". Then…

NORVILLE starts waving his arms.

> THE END")
         (forth-output (compile-script-from-string script)))
    (is (search "ActionWaveArms" forth-output))
    (is (search "CharacterID_Norville" forth-output))
    (is (search "character-action!" forth-output))))

(test wave-arms-waves-arms
  "Test 'waves arms' grammar"
      (let* ((script "INT SHIPS - AMS CALYPSO2

Open on: NORVILLE is at \"ChefCorner\". Then…

NORVILLE waves arms.

> THE END")
         (forth-output (compile-script-from-string script)))
    (is (search "ActionWaveArms" forth-output))
    (is (search "CharacterID_Norville" forth-output))
    (is (search "character-action!" forth-output))))

(test wave-arms-waves-his-arms
  "Test 'waves his arms' grammar"
      (let* ((script "INT SHIPS - AMS CALYPSO2

Open on: NORVILLE is at \"ChefCorner\". Then…

NORVILLE waves his arms.

> THE END")
             (forth-output (compile-script-from-string script)))
    (is (search "ActionWaveArms" forth-output))
    (is (search "CharacterID_Norville" forth-output))
    (is (search "character-action!" forth-output))))

(test wave-arms-waves-her-arms
      "Test 'waves her arms' grammar"
  (let* ((script "INT SHIPS - AMS CALYPSO2

Open on: NORVILLE is at \"ChefCorner\". Then…

NORVILLE waves her arms.

> THE END")
         (forth-output (compile-script-from-string script)))
    (is (search "ActionWaveArms" forth-output))
    (is (search "CharacterID_Norville" forth-output))
    (is (search "character-action!" forth-output))))

(test wave-arms-waves-their-arms
      "Test 'waves their arms' grammar"
      (let* ((script "INT SHIPS - AMS CALYPSO2

Open on: NORVILLE is at \"ChefCorner\". Then…

NORVILLE waves their arms.

> THE END")
         (forth-output (compile-script-from-string script)))
    (is (search "ActionWaveArms" forth-output))
        (is (search "CharacterID_Norville" forth-output))
    (is (search "character-action!" forth-output))))

(test wave-arms-stops-waving
      "Test 'stops waving' grammar"
      (let* ((script "INT SHIPS - AMS CALYPSO2

Open on: NORVILLE is at \"ChefCorner\". Then…

NORVILLE stops waving.

> THE END")
         (forth-output (compile-script-from-string script)))
    (is (search "ActionIdle" forth-output))
    (is (search "CharacterID_Norville" forth-output))
    (is (search "character-action!" forth-output))))

(test wave-arms-stops-waving-arms
      "Test 'stops waving arms' grammar"
      (let* ((script "INT SHIPS - AMS CALYPSO2

Open on: NORVILLE is at \"ChefCorner\". Then…

NORVILLE stops waving arms.

> THE END")
             (forth-output (compile-script-from-string script)))
        (is (search "ActionIdle" forth-output))
        (is (search "CharacterID_Norville" forth-output))
        (is (search "character-action!" forth-output))))



(defun run-action-tests ()
  "Run all action tests and return results"
  (fiveam:run! 'action-tests))
