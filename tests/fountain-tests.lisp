;;; Phantasia SkylineTool/tests/fountain-tests.lisp
;;;; Comprehensive tests for fountain.lisp scripting system
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

(def-suite fountain-tests
  :description "Comprehensive tests for fountain scripting system"
  :in skyline-tool/test)

(in-suite fountain-tests)

;; Test data generators for fountain functions
(defun generate-random-direction ()
  "Generate a random cardinal direction"
  (nth (random 4) '(north south east west)))

(defun generate-random-script-text ()
  "Generate random script text"
  (let ((lines '("Hello world" "FADE IN:" "INT. ROOM - DAY" "CHARACTER" "Dialogue here" "ACTION here")))
    (nth (random (length lines)) lines)))

(defun generate-random-expression (&optional (depth 2))
  "Generate a random mathematical expression"
  (if (or (= depth 0) (< (random 10) 7))
      (random 100) ; Return a number
      (let ((op (nth (random 6) '(+ - * / expt sqrt))))
        (list op
              (generate-random-expression (1- depth))
              (generate-random-expression (1- depth))))))

;; Test mathematical expression utilities
(test all-cdrs-basic
  "Test all-cdrs function"
  (is (equal (skyline-tool::all-cdrs '(a b c d)) '(b c d))
      "Should return all cdrs of cons cells")
  (is (equal (skyline-tool::all-cdrs '(a (b c) d)) '((b c) d))
      "Should handle nested lists"))

(test variable-refs-p-basic
  "Test variable-refs-p function"
  (is-true (skyline-tool::variable-refs-p '(+ x y)) "Should detect variables")
  (is-false (skyline-tool::variable-refs-p '(+ 1 2)) "Should not detect variables in constant expressions")
  (is-false (skyline-tool::variable-refs-p '(+ pi e)) "Should ignore pi and e"))

(test eval-expr-basic
  "Test eval-expr function"
  (is (= (skyline-tool::eval-expr '(+ 1 2)) 3) "Should evaluate simple addition")
  (is (= (skyline-tool::eval-expr '(* 3 4)) 12) "Should evaluate multiplication")
  (is (= (skyline-tool::eval-expr '(- 10 3)) 7) "Should evaluate subtraction"))

(test maybe-do-math-basic
  "Test maybe-do-math function"
  (is (= (skyline-tool::maybe-do-math 42) 42) "Should return numbers unchanged")
  (is (= (skyline-tool::maybe-do-math 'pi) pi) "Should handle pi constant")
  (is (= (skyline-tool::maybe-do-math '(+ 1 2)) 3) "Should evaluate constant expressions"))

;; Test direction utilities
(test cross-quarter-direction-basic
  "Test cross-quarter-direction function"
  (is (eq (skyline-tool::cross-quarter-direction 'north 'east) 'northeast)
      "North + East should give Northeast")
  (is (eq (skyline-tool::cross-quarter-direction 'south 'west) 'southwest)
      "South + West should give Southwest"))

;; Test Fountain script parsing
(test regex-match-existence
  "Test regex-match function exists"
  (is-true (fboundp 'skyline-tool::regex-match) "regex-match should be defined"))

(test parse-stage-directions-existence
  "Test parse-stage-directions function exists"
  (is-true (fboundp 'skyline-tool::parse-stage-directions) "parse-stage-directions should be defined"))

(test fountain-lexer-parse-line-existence
  "Test fountain-lexer/parse-line function exists"
  (is-true (fboundp 'skyline-tool::fountain-lexer/parse-line) "fountain-lexer/parse-line should be defined"))

(test format-terminals-for-error-existence
  "Test format-terminals-for-error function exists"
  (is-true (fboundp 'skyline-tool::format-terminals-for-error) "format-terminals-for-error should be defined"))

(test fountain-lexer-existence
  "Test fountain-lexer function exists"
  (is-true (fboundp 'skyline-tool::fountain-lexer) "fountain-lexer should be defined"))

;; Test character classification functions
(test presence-basic
  "Test presence function"
  (is (= (skyline-tool::presence #(1 2 3)) 3) "Should return length of sequence")
  (is (= (skyline-tool::presence "hello") 5) "Should return length of string")
  (is (= (skyline-tool::presence nil) 0) "Should return 0 for nil"))

(test actor-name-char-p-basic
  "Test actor-name-char-p function"
  (is-true (skyline-tool::actor-name-char-p #\A) "Should accept uppercase letters")
  (is-true (skyline-tool::actor-name-char-p #\z) "Should accept lowercase letters")
  (is-false (skyline-tool::actor-name-char-p #\1) "Should reject digits")
  (is-false (skyline-tool::actor-name-char-p #\space) "Should reject spaces"))

(test numeric-char-p-basic
  "Test numeric-char-p function"
  (is-true (skyline-tool::numeric-char-p #\1) "Should accept digits")
  (is-true (skyline-tool::numeric-char-p #\0) "Should accept zero")
  (is-false (skyline-tool::numeric-char-p #\A) "Should reject letters")
  (is-false (skyline-tool::numeric-char-p #\space) "Should reject spaces"))

;; Test lexer functions
(test stage-direction-lexer-existence
  "Test stage-direction-lexer function exists"
  (is-true (fboundp 'skyline-tool::stage-direction-lexer) "stage-direction-lexer should be defined"))

(test make-fountain-lexer-existence
  "Test make-fountain-lexer function exists"
  (is-true (fboundp 'skyline-tool::make-fountain-lexer) "make-fountain-lexer should be defined"))

;; Test dialogue preparation
(test prepare-dialogue-existence
  "Test prepare-dialogue function exists"
  (is-true (fboundp 'skyline-tool::prepare-dialogue) "prepare-dialogue should be defined"))

;; Test AtariVox speech synthesis functions
(test ensure-atarivox-dictionary-existence
  "Test ensure-atarivox-dictionary function exists"
  (is-true (fboundp 'skyline-tool::ensure-atarivox-dictionary) "ensure-atarivox-dictionary should be defined"))

(test phonemes-from-string-existence
  "Test phonemes-from-string function exists"
  (is-true (fboundp 'skyline-tool::phonemes-from-string) "phonemes-from-string should be defined"))

(test load-atarivox-dictionary-existence
  "Test load-atarivox-dictionary function exists"
  (is-true (fboundp 'skyline-tool::load-atarivox-dictionary) "load-atarivox-dictionary should be defined"))

(test reload-atarivox-dictionary-existence
  "Test reload-atarivox-dictionary function exists"
  (is-true (fboundp 'skyline-tool::reload-atarivox-dictionary) "reload-atarivox-dictionary should be defined"))

;; Test speech processing functions
(test speakjet-pause+-basic
  "Test speakjet-pause+ function"
  (is (= (skyline-tool::speakjet-pause+ 10 5) 15) "Should add two values"))

(test log-missing-word-for-speakjet-existence
  "Test log-missing-word-for-speakjet function exists"
  (is-true (fboundp 'skyline-tool::log-missing-word-for-speakjet) "log-missing-word-for-speakjet should be defined"))

(test fixup-exclamations-existence
  "Test fixup-exclamations function exists"
  (is-true (fboundp 'skyline-tool::fixup-exclamations) "fixup-exclamations should be defined"))

(test combine-adjacent-pauses-existence
  "Test combine-adjacent-pauses function exists"
  (is-true (fboundp 'skyline-tool::combine-adjacent-pauses) "combine-adjacent-pauses should be defined"))

(test char-digit-or-comma-p-basic
  "Test char-digit-or-comma-p function"
  (is-true (skyline-tool::char-digit-or-comma-p #\1) "Should accept digits")
  (is-true (skyline-tool::char-digit-or-comma-p #\,) "Should accept commas")
  (is-false (skyline-tool::char-digit-or-comma-p #\A) "Should reject letters")
  (is-false (skyline-tool::char-digit-or-comma-p #\space) "Should reject spaces"))

(test convert-for-atarivox-existence
  "Test convert-for-atarivox function exists"
  (is-true (fboundp 'skyline-tool::convert-for-atarivox) "convert-for-atarivox should be defined"))

;; Test script compilation functions
(test compile-fountain-script-existence
  "Test compile-fountain-script function exists"
  (is-true (fboundp 'skyline-tool::compile-fountain-script) "compile-fountain-script should be defined"))

(test npc-interpret-color-existence
  "Test npc-interpret-color function exists"
  (is-true (fboundp 'skyline-tool::npc-interpret-color) "npc-interpret-color should be defined"))

;; Test actor and location functions
(test load-actor-existence
  "Test load-actor function exists"
  (is-true (fboundp 'skyline-tool::load-actor) "load-actor should be defined"))

(test find-location-existence
  "Test find-location function exists"
  (is-true (fboundp 'skyline-tool::find-location) "find-location should be defined"))

(test interpret-place-existence
  "Test interpret-place function exists"
  (is-true (fboundp 'skyline-tool::interpret-place) "interpret-place should be defined"))

(test color-index-existence
  "Test color-index function exists"
  (is-true (fboundp 'skyline-tool::color-index) "color-index should be defined"))

;; Test character action functions
(test perform-character-action-existence
  "Test perform-character-action function exists"
  (is-true (fboundp 'skyline-tool::perform-character-action) "perform-character-action should be defined"))

;; Test dictionary functions
(test read-entire-dictionary-existence
  "Test read-entire-dictionary function exists"
  (is-true (fboundp 'skyline-tool::read-entire-dictionary) "read-entire-dictionary should be defined"))

;; Test label generation
(test genlabel-basic
  "Test genlabel function generates unique labels"
  (let ((label1 (skyline-tool::genlabel "TEST"))
        (label2 (skyline-tool::genlabel "TEST")))
    (is (stringp label1) "Should return a string")
    (is (stringp label2) "Should return a string")
    (is (not (string= label1 label2)) "Labels should be unique")))

;; Test stage direction functions
(test stage-facing-value-existence
  "Test stage-facing-value function exists"
  (is-true (fboundp 'skyline-tool::stage-facing-value) "stage-facing-value should be defined"))

(test forth-number-existence
  "Test forth-number function exists"
  (is-true (fboundp 'skyline-tool::forth-number) "forth-number should be defined"))

(test stage-constant-value-existence
  "Test stage/constant-value function exists"
  (is-true (fboundp 'skyline-tool::stage/constant-value) "stage/constant-value should be defined"))

(test script-auto-label-basic
  "Test script-auto-label generates unique labels"
  (let ((label1 (skyline-tool::script-auto-label))
        (label2 (skyline-tool::script-auto-label)))
    (is (stringp label1) "Should return a string")
    (is (stringp label2) "Should return a string")
    (is (not (string= label1 label2)) "Labels should be unique")))

;; Test stage direction processing
(test stage-directions-acc-existence
  "Test stage-directions->acc function exists"
  (is-true (fboundp 'skyline-tool::stage-directions->acc) "stage-directions->acc should be defined"))

(test stage-constant-zero-p-existence
  "Test stage/constant-zero-p function exists"
  (is-true (fboundp 'skyline-tool::stage/constant-zero-p) "stage/constant-zero-p should be defined"))

(test invert-branch-existence
  "Test invert-branch function exists"
  (is-true (fboundp 'skyline-tool::invert-branch) "invert-branch should be defined"))

(test stage-directions-test-existence
  "Test stage-directions->test function exists"
  (is-true (fboundp 'skyline-tool::stage-directions->test) "stage-directions->test should be defined"))

(test field-label-existence
  "Test field->label function exists"
  (is-true (fboundp 'skyline-tool::field->label) "field->label should be defined"))

(test stage-directions-code-existence
  "Test stage-directions->code function exists"
  (is-true (fboundp 'skyline-tool::stage-directions->code) "stage-directions->code should be defined"))

;; Test actor management functions
(test find-actor-existence
  "Test find-actor function exists"
  (is-true (fboundp 'skyline-tool::find-actor) "find-actor should be defined"))

(test find-or-load-actor-existence
  "Test find-or-load-actor function exists"
  (is-true (fboundp 'skyline-tool::find-or-load-actor) "find-or-load-actor should be defined"))

(test require-actor-existence
  "Test require-actor function exists"
  (is-true (fboundp 'skyline-tool::require-actor) "require-actor should be defined"))

;; Test scene writing functions
(test fountain-write-scene-start-existence
  "Test fountain/write-scene-start function exists"
  (is-true (fboundp 'skyline-tool::fountain/write-scene-start) "fountain/write-scene-start should be defined"))

(test fountain-write-speech-existence
  "Test fountain/write-speech function exists"
  (is-true (fboundp 'skyline-tool::fountain/write-speech) "fountain/write-speech should be defined"))

(test dialogue-hash-existence
  "Test dialogue-hash function exists"
  (is-true (fboundp 'skyline-tool::dialogue-hash) "dialogue-hash should be defined"))

(test fountain-write-speech-branch-existence
  "Test fountain/write-speech-branch function exists"
  (is-true (fboundp 'skyline-tool::fountain/write-speech-branch) "fountain/write-speech-branch should be defined"))

(test write-off-camera-speaker-existence
  "Test write-off-camera-speaker function exists"
  (is-true (fboundp 'skyline-tool::write-off-camera-speaker) "write-off-camera-speaker should be defined"))

;; Test compilation functions
(test compile-fountain-stream-existence
  "Test compile-fountain-stream function exists"
  (is-true (fboundp 'skyline-tool::compile-fountain-stream) "compile-fountain-stream should be defined"))

(test compile-fountain-string-existence
  "Test compile-fountain-string function exists"
  (is-true (fboundp 'skyline-tool::compile-fountain-string) "compile-fountain-string should be defined"))

(test compile-script-existence
  "Test compile-script function exists"
  (is-true (fboundp 'skyline-tool::compile-script) "compile-script should be defined"))

(test compile-forth-existence
  "Test compile-forth function exists"
  (is-true (fboundp 'skyline-tool::compile-forth) "compile-forth should be defined"))

;; Test NPC and game data functions
(test find-npc-stats-existence
  "Test find-npc-stats function exists"
  (is-true (fboundp 'skyline-tool::find-npc-stats) "find-npc-stats should be defined"))

(test load-npc-stats-existence
  "Test load-npc-stats function exists"
  (is-true (fboundp 'skyline-tool::load-npc-stats) "load-npc-stats should be defined"))

(test load-boats-existence
  "Test load-boats function exists"
  (is-true (fboundp 'skyline-tool::load-boats) "load-boats should be defined"))

(test find-script-id-existence
  "Test find-script-id function exists"
  (is-true (fboundp 'skyline-tool::find-script-id) "find-script-id should be defined"))

;; Test speech support
(test speech-supported-p-existence
  "Test speech-supported-p function exists"
  (is-true (fboundp 'skyline-tool::speech-supported-p) "speech-supported-p should be defined"))

;; Test actor prototype functions
(test print-one-actor-prototype-existence
  "Test print-one-actor-prototype function exists"
  (is-true (fboundp 'skyline-tool::print-one-actor-prototype) "print-one-actor-prototype should be defined"))

(test print-actor-prototypes-existence
  "Test print-actor-prototypes function exists"
  (is-true (fboundp 'skyline-tool::print-actor-prototypes) "print-actor-prototypes should be defined"))

(test write-actor-prototypes-existence
  "Test write-actor-prototypes function exists"
  (is-true (fboundp 'skyline-tool::write-actor-prototypes) "write-actor-prototypes should be defined"))

(test write-character-ids-existence
  "Test write-character-ids function exists"
  (is-true (fboundp 'skyline-tool::write-character-ids) "write-character-ids should be defined"))

;; Test expression evaluation with various inputs
(define-multi-test math-expression-evaluation
  "Test mathematical expression evaluation with various inputs"
  15 ; Test 15 different expressions
  (let ((expr (generate-random-expression)))
    (finishes (skyline-tool::maybe-do-math expr) "Should handle random expressions")))

;; Test direction combinations
(test all-direction-combinations
  "Test cross-quarter-direction with all valid combinations"
  (let ((combinations '((north east northeast)
                        (north west northwest)
                        (south east southeast)
                        (south west southwest))))
    (dolist (combo combinations)
      (destructuring-bind (ns ew expected) combo
        (is (eq (skyline-tool::cross-quarter-direction ns ew) expected)
            "~a + ~a should give ~a" ns ew expected)))))

;; Test label generation uniqueness
(define-multi-test label-generation-uniqueness
  "Test that generated labels are unique"
  20 ; Test 20 times for good statistical confidence
  (let ((labels (loop for i from 1 to 10 collect (skyline-tool::genlabel "TEST"))))
    (is (= (length labels) (length (remove-duplicates labels :test #'string=)))
        "All generated labels should be unique")))

;; Test character classification properties
(define-multi-test character-classification-properties
  "Test character classification functions with various inputs"
  10 ; Test 10 different characters
  (let ((char (code-char (+ 32 (random 95))))) ; Printable ASCII
    ;; Test that functions don't crash and return boolean values
    (is (typep (skyline-tool::actor-name-char-p char) 'boolean)
        "actor-name-char-p should return boolean")
    (is (typep (skyline-tool::numeric-char-p char) 'boolean)
        "numeric-char-p should return boolean")
    (is (typep (skyline-tool::char-digit-or-comma-p char) 'boolean)
        "char-digit-or-comma-p should return boolean")))

;; Test that all core functions are properly defined
(test all-fountain-functions-defined
  "Test that all core fountain functions are properly defined"
  (dolist (func-name '(skyline-tool::all-cdrs
                       skyline-tool::variable-refs-p
                       skyline-tool::eval-expr
                       skyline-tool::maybe-do-math
                       skyline-tool::cross-quarter-direction
                       skyline-tool::regex-match
                       skyline-tool::parse-stage-directions
                       skyline-tool::fountain-lexer/parse-line
                       skyline-tool::fountain-lexer
                       skyline-tool::presence
                       skyline-tool::actor-name-char-p
                       skyline-tool::numeric-char-p
                       skyline-tool::prepare-dialogue
                       skyline-tool::genlabel
                       skyline-tool::script-auto-label
                       skyline-tool::compile-fountain-script))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all speech synthesis functions are properly defined
(test all-speech-functions-defined
  "Test that all speech synthesis functions are properly defined"
  (dolist (func-name '(skyline-tool::ensure-atarivox-dictionary
                       skyline-tool::phonemes-from-string
                       skyline-tool::load-atarivox-dictionary
                       skyline-tool::convert-for-atarivox
                       skyline-tool::speakjet-pause+
                       skyline-tool::combine-adjacent-pauses))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all compilation functions are properly defined
(test all-compilation-functions-defined
  "Test that all compilation functions are properly defined"
  (dolist (func-name '(skyline-tool::compile-fountain-stream
                       skyline-tool::compile-fountain-string
                       skyline-tool::compile-script
                       skyline-tool::compile-forth
                       skyline-tool::compile-fountain-script))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all actor/NPC functions are properly defined
(test all-actor-functions-defined
  "Test that all actor/NPC functions are properly defined"
  (dolist (func-name '(skyline-tool::load-actor
                       skyline-tool::find-actor
                       skyline-tool::find-or-load-actor
                       skyline-tool::require-actor
                       skyline-tool::find-npc-stats
                       skyline-tool::load-npc-stats
                       skyline-tool::npc-interpret-color))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all scene/location functions are properly defined
(test all-scene-functions-defined
  "Test that all scene/location functions are properly defined"
  (dolist (func-name '(skyline-tool::find-location
                       skyline-tool::interpret-place
                       skyline-tool::color-index
                       skyline-tool::fountain/write-scene-start))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all dialogue functions are properly defined
(test all-dialogue-functions-defined
  "Test that all dialogue functions are properly defined"
  (dolist (func-name '(skyline-tool::fountain/write-speech
                       skyline-tool::dialogue-hash
                       skyline-tool::fountain/write-speech-branch
                       skyline-tool::write-off-camera-speaker))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all stage direction functions are properly defined
(test all-stage-direction-functions-defined
  "Test that all stage direction functions are properly defined"
  (dolist (func-name '(skyline-tool::stage-facing-value
                       skyline-tool::stage/constant-value
                       skyline-tool::stage-directions->acc
                       skyline-tool::stage/constant-zero-p
                       skyline-tool::invert-branch
                       skyline-tool::stage-directions->test
                       skyline-tool::stage-directions->code))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all utility functions are properly defined
(test all-utility-functions-defined
  "Test that all utility functions are properly defined"
  (dolist (func-name '(skyline-tool::read-entire-dictionary
                       skyline-tool::load-boats
                       skyline-tool::find-script-id
                       skyline-tool::speech-supported-p
                       skyline-tool::print-one-actor-prototype
                       skyline-tool::print-actor-prototypes
                       skyline-tool::write-actor-prototypes
                       skyline-tool::write-character-ids
                       skyline-tool::perform-character-action
                       skyline-tool::field->label))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))