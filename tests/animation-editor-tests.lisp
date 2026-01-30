;;; Phantasia SkylineTool/tests/animation-editor-tests.lisp
;;;; Comprehensive tests for animation-editor.lisp animation tools
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

(def-suite animation-editor-tests
  :description "Comprehensive tests for animation editor tools")

(in-suite animation-editor-tests)

;; Test data generators for animation editor functions
(defun generate-random-decal-kind ()
  "Generate a random decal kind"
  (nth (random 4) '(:scenery :character :npc :enemy)))

(defun generate-random-body-type ()
  "Generate a random body type"
  (random 10))

(defun generate-random-action ()
  "Generate a random action"
  (nth (random 6) '(:idle :walk :run :attack :defend :die)))

(defun generate-random-facing ()
  "Generate a random facing direction"
  (nth (random 8) '(:north :south :east :west :northeast :northwest :southeast :southwest)))

(defun generate-random-animation-sequence ()
  "Generate a mock animation sequence"
  `(:id ,(random 1000)
    :name ,(format nil "anim_~d" (random 100))
    :frames ,(loop for i from 1 to (1+ (random 8))
                   collect `(:tile ,(random 256) :duration ,(1+ (random 10))))))

;; Test asset management functions
(test all-tilesets-existence
  "Test all-tilesets function exists"
  (is-true (fboundp 'skyline-tool::all-tilesets) "all-tilesets should be defined"))

(test all-scenery-decals-existence
  "Test all-scenery-decals function exists"
  (is-true (fboundp 'skyline-tool::all-scenery-decals) "all-scenery-decals should be defined"))

(test all-npc-art-sheets-existence
  "Test all-npc-art-sheets-for-kind function exists"
  (is-true (fboundp 'skyline-tool::all-npc-art-sheets-for-kind) "all-npc-art-sheets-for-kind should be defined")
  (finishes (skyline-tool::all-npc-art-sheets-for-kind :npc) "Should handle NPC kind"))

(test decal-kind-name-basic
  "Test decal-kind-name function"
  (is (string= (skyline-tool::decal-kind-name :scenery) "scenery") "Should return lowercase string")
  (is (string= (skyline-tool::decal-kind-name :character) "character") "Should handle character kind"))

;; Test animation sequence functions
(test find-animation-sequence-existence
  "Test find-animation-sequence function exists"
  (is-true (fboundp 'skyline-tool::find-animation-sequence) "find-animation-sequence should be defined"))

(test start-preview-animation-existence
  "Test start-preview-animation function exists"
  (is-true (fboundp 'skyline-tool::start-preview-animation) "start-preview-animation should be defined"))

;; Test tile sheet functions
(test load-tile-sheet-object-existence
  "Test load-tile-sheet-object-by-name function exists"
  (is-true (fboundp 'skyline-tool::load-tile-sheet-object-by-name) "load-tile-sheet-object-by-name should be defined"))

(test read-palette-for-tile-sheet-existence
  "Test read-palette-for-tile-sheet function exists"
  (is-true (fboundp 'skyline-tool::read-palette-for-tile-sheet) "read-palette-for-tile-sheet should be defined"))

;; Test animation sequence creation
(test create-new-animation-sequence-existence
  "Test create-new-animation-sequence function exists"
  (is-true (fboundp 'skyline-tool::create-new-animation-sequence) "create-new-animation-sequence should be defined"))

;; Test editing predicates
(test anim-seq-editing-predicates
  "Test animation sequence editing predicates"
  (is-true (fboundp 'skyline-tool::anim-seq-editing-background-p) "anim-seq-editing-background-p should be defined")
  (is-true (fboundp 'skyline-tool::anim-seq-editing-scenery-p) "anim-seq-editing-scenery-p should be defined")
  (finishes (skyline-tool::anim-seq-editing-background-p) "Should handle no arguments")
  (finishes (skyline-tool::anim-seq-editing-scenery-p) "Should handle no arguments"))

;; Test string matching utilities
(test prefix-match-length-basic
  "Test prefix-match-length function"
  (is (= (skyline-tool::prefix-match-length "hello" "hello world") 5) "Should find common prefix")
  (is (= (skyline-tool::prefix-match-length "test" "different") 0) "Should return 0 for no match")
  (is (= (skyline-tool::prefix-match-length "" "anything") 0) "Should handle empty first string")
  (is (= (skyline-tool::prefix-match-length "test" "") 0) "Should handle empty second string"))

(define-multi-test prefix-match-properties
  "Test prefix-match-length properties"
  10 ; Test 10 different string pairs
  (let* ((str1 (format nil "~a" (random 1000)))
         (str2 (format nil "~a~a" str1 (random 1000)))
         (match-len (skyline-tool::prefix-match-length str1 str2)))
    (is (= match-len (length str1)) "Should match the length of the first string")
    (is (<= match-len (length str1)) "Should not exceed first string length")
    (is (<= match-len (length str2)) "Should not exceed second string length")))

;; Test animation sequence parsing
(test parse-animation-sequence-row-existence
  "Test parse-animation-sequence-row-from-ss function exists"
  (is-true (fboundp 'skyline-tool::parse-animation-sequence-row-from-ss) "parse-animation-sequence-row-from-ss should be defined"))

;; Test animation sequence I/O
(test load-all-animation-sequences-existence
  "Test load-all-animation-sequences function exists"
  (is-true (fboundp 'skyline-tool::load-all-animation-sequences) "load-all-animation-sequences should be defined"))

(test save-all-animation-sequences-existence
  "Test save-all-animation-sequences function exists"
  (is-true (fboundp 'skyline-tool::save-all-animation-sequences) "save-all-animation-sequences should be defined"))

(test save-animation-sequence-existence
  "Test save-animation-sequence function exists"
  (is-true (fboundp 'skyline-tool::save-animation-sequence) "save-animation-sequence should be defined"))

;; Test animation editing interface
(test edit-animation-sequence-existence
  "Test edit-animation-sequence function exists"
  (is-true (fboundp 'skyline-tool::edit-animation-sequence) "edit-animation-sequence should be defined"))

;; Test animation assignment functions
(test assign-animation-sequence-existence
  "Test assign-animation-sequence function exists"
  (is-true (fboundp 'skyline-tool::assign-animation-sequence) "assign-animation-sequence should be defined"))

(test body-count-for-decal-kind-existence
  "Test body-count-for-decal-kind function exists"
  (is-true (fboundp 'skyline-tool::body-count-for-decal-kind) "body-count-for-decal-kind should be defined"))

(test find-assigned-animation-sequence-existence
  "Test find-assigned-animation-sequence function exists"
  (is-true (fboundp 'skyline-tool::find-assigned-animation-sequence) "find-assigned-animation-sequence should be defined"))

(test set-animation-sequence-assignment-existence
  "Test set-animation-sequence-assignment function exists"
  (is-true (fboundp 'skyline-tool::set-animation-sequence-assignment) "set-animation-sequence-assignment should be defined"))

(test animation-sequence-compatible-p-existence
  "Test animation-sequence-compatible-p function exists"
  (is-true (fboundp 'skyline-tool::animation-sequence-compatible-p) "animation-sequence-compatible-p should be defined"))

(test find-next-compatible-sequence-existence
  "Test find-next-compatible-sequence function exists"
  (is-true (fboundp 'skyline-tool::find-next-compatible-sequence) "find-next-compatible-sequence should be defined"))

(test find-name-for-body-existence
  "Test find-name-for-body function exists"
  (is-true (fboundp 'skyline-tool::find-name-for-body) "find-name-for-body should be defined"))

;; Test batch assignment functions
(test assign-animation-sequences-existence
  "Test assign-animation-sequences function exists"
  (is-true (fboundp 'skyline-tool::assign-animation-sequences) "assign-animation-sequences should be defined"))

(test accept-chosen-sequence-existence
  "Test accept-chosen-sequence function exists"
  (is-true (fboundp 'skyline-tool::accept-chosen-sequence) "accept-chosen-sequence should be defined"))

;; Test tile selection interface
(test choose-tile-from-set-existence
  "Test choose-tile-from-set function exists"
  (is-true (fboundp 'skyline-tool::choose-tile-from-set) "choose-tile-from-set should be defined"))

;; Test sequence filtering and sorting
(test find-sequences-matching-existence
  "Test find-sequences-matching function exists"
  (is-true (fboundp 'skyline-tool::find-sequences-matching) "find-sequences-matching should be defined"))

(test animation-sequences-natural-order-existence
  "Test animation-sequences-natural-order function exists"
  (is-true (fboundp 'skyline-tool::animation-sequences-natural-order) "animation-sequences-natural-order should be defined"))

;; Test sequence selection interface
(test choose-animation-sequence-existence
  "Test choose-animation-sequence function exists"
  (is-true (fboundp 'skyline-tool::choose-animation-sequence) "choose-animation-sequence should be defined"))

;; Test display functions
(test display-anim-preview-existence
  "Test display-anim-preview function exists"
  (is-true (fboundp 'skyline-tool::display-anim-preview) "display-anim-preview should be defined"))

;; Test sorting functions
(test sort-matching-lists-existence
  "Test sort-matching-lists-by-decal-kind function exists"
  (is-true (fboundp 'skyline-tool::sort-matching-lists-by-decal-kind) "sort-matching-lists-by-decal-kind should be defined"))

(test sort-animation-sequences-existence
  "Test sort-animation-sequences-by-decal-kind function exists"
  (is-true (fboundp 'skyline-tool::sort-animation-sequences-by-decal-kind) "sort-animation-sequences-by-decal-kind should be defined"))

;; Test compilation function
(test compile-animation-sequences-existence
  "Test compile-animation-sequences function exists"
  (is-true (fboundp 'skyline-tool::compile-animation-sequences) "compile-animation-sequences should be defined"))

;; Test decal kind handling
(define-multi-test decal-kind-handling
  "Test decal kind functions with various inputs"
  8 ; Test 8 different decal kinds
  (let ((kind (generate-random-decal-kind)))
    (finishes (skyline-tool::decal-kind-name kind) "Should handle all decal kinds")
    (is (stringp (skyline-tool::decal-kind-name kind)) "Should return string")))

;; Test animation sequence compatibility
(define-multi-test animation-sequence-compatibility
  "Test animation sequence compatibility logic"
  6 ; Test 6 different combinations
  (let ((sequence (generate-random-animation-sequence))
        (kind (generate-random-decal-kind))
        (body (generate-random-body-type)))
    (finishes (skyline-tool::animation-sequence-compatible-p sequence kind body)
              "Should handle compatibility checking")))

;; Test action and facing combinations
(define-multi-test action-facing-combinations
  "Test various action and facing combinations"
  12 ; Test 12 different combinations
  (let ((action (generate-random-action))
        (facing (generate-random-facing)))
    (finishes (skyline-tool::assign-animation-sequence
               (generate-random-decal-kind)
               (generate-random-body-type)
               action facing)
              "Should handle various action/facing combinations")))

;; Test that all core functions are properly defined
(test all-animation-editor-functions-defined
  "Test that all core animation editor functions are properly defined"
  (dolist (func-name '(skyline-tool::all-tilesets
                       skyline-tool::all-scenery-decals
                       skyline-tool::all-npc-art-sheets-for-kind
                       skyline-tool::decal-kind-name
                       skyline-tool::find-animation-sequence
                       skyline-tool::start-preview-animation
                       skyline-tool::load-tile-sheet-object-by-name
                       skyline-tool::read-palette-for-tile-sheet
                       skyline-tool::create-new-animation-sequence
                       skyline-tool::anim-seq-editing-background-p
                       skyline-tool::anim-seq-editing-scenery-p
                       skyline-tool::prefix-match-length
                       skyline-tool::parse-animation-sequence-row-from-ss
                       skyline-tool::load-all-animation-sequences
                       skyline-tool::save-all-animation-sequences
                       skyline-tool::save-animation-sequence
                       skyline-tool::edit-animation-sequence
                       skyline-tool::assign-animation-sequence
                       skyline-tool::body-count-for-decal-kind
                       skyline-tool::find-assigned-animation-sequence
                       skyline-tool::set-animation-sequence-assignment
                       skyline-tool::animation-sequence-compatible-p
                       skyline-tool::find-next-compatible-sequence
                       skyline-tool::find-name-for-body
                       skyline-tool::assign-animation-sequences
                       skyline-tool::accept-chosen-sequence
                       skyline-tool::choose-tile-from-set
                       skyline-tool::find-sequences-matching
                       skyline-tool::animation-sequences-natural-order
                       skyline-tool::choose-animation-sequence
                       skyline-tool::display-anim-preview
                       skyline-tool::sort-matching-lists-by-decal-kind
                       skyline-tool::sort-animation-sequences-by-decal-kind
                       skyline-tool::compile-animation-sequences))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all asset management functions are properly defined
(test all-asset-functions-defined
  "Test that all asset management functions are properly defined"
  (dolist (func-name '(skyline-tool::all-tilesets
                       skyline-tool::all-scenery-decals
                       skyline-tool::all-npc-art-sheets-for-kind
                       skyline-tool::load-tile-sheet-object-by-name
                       skyline-tool::read-palette-for-tile-sheet))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all sequence management functions are properly defined
(test all-sequence-functions-defined
  "Test that all sequence management functions are properly defined"
  (dolist (func-name '(skyline-tool::find-animation-sequence
                       skyline-tool::create-new-animation-sequence
                       skyline-tool::load-all-animation-sequences
                       skyline-tool::save-all-animation-sequences
                       skyline-tool::save-animation-sequence
                       skyline-tool::parse-animation-sequence-row-from-ss))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all UI/interface functions are properly defined
(test all-ui-functions-defined
  "Test that all UI/interface functions are properly defined"
  (dolist (func-name '(skyline-tool::edit-animation-sequence
                       skyline-tool::choose-tile-from-set
                       skyline-tool::choose-animation-sequence
                       skyline-tool::display-anim-preview
                       skyline-tool::start-preview-animation))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all assignment and compatibility functions are properly defined
(test all-assignment-functions-defined
  "Test that all assignment and compatibility functions are properly defined"
  (dolist (func-name '(skyline-tool::assign-animation-sequence
                       skyline-tool::find-assigned-animation-sequence
                       skyline-tool::set-animation-sequence-assignment
                       skyline-tool::animation-sequence-compatible-p
                       skyline-tool::find-next-compatible-sequence
                       skyline-tool::find-name-for-body
                       skyline-tool::body-count-for-decal-kind
                       skyline-tool::assign-animation-sequences
                       skyline-tool::accept-chosen-sequence))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all sorting and filtering functions are properly defined
(test all-sorting-functions-defined
  "Test that all sorting and filtering functions are properly defined"
  (dolist (func-name '(skyline-tool::find-sequences-matching
                       skyline-tool::animation-sequences-natural-order
                       skyline-tool::sort-matching-lists-by-decal-kind
                       skyline-tool::sort-animation-sequences-by-decal-kind))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test that all compilation and I/O functions are properly defined
(test all-compilation-functions-defined
  "Test that all compilation and I/O functions are properly defined"
  (dolist (func-name '(skyline-tool::compile-animation-sequences))
    (is-true (fboundp func-name)
             "Function ~a should be defined" func-name)))

;; Test prefix matching with various string combinations
(define-multi-test prefix-matching-various-strings
  "Test prefix-match-length with various string combinations"
  10 ; Test 10 different string pairs
  (let* ((prefix (format nil "~a" (random 100)))
         (suffix (format nil "~a" (random 100)))
         (str1 (concatenate 'string prefix suffix))
         (str2 (concatenate 'string prefix (format nil "~a" (random 100))))
         (match-len (skyline-tool::prefix-match-length str1 str2)))
    (is (>= match-len 0) "Match length should be non-negative")
    (is (<= match-len (length str1)) "Should not exceed first string")
    (is (<= match-len (length str2)) "Should not exceed second string")))

;; Test decal kind name conversion
(define-multi-test decal-kind-name-conversion
  "Test decal-kind-name with various kinds"
  6 ; Test 6 different kinds
  (let ((kind (generate-random-decal-kind)))
    (let ((name (skyline-tool::decal-kind-name kind)))
      (is (stringp name) "Should return string")
      (is (> (length name) 0) "Should not be empty"))))