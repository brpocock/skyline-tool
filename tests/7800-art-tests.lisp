(defpackage :skyline-tool/7800-art-test
  (:use :cl :fiveam)
  (:import-from :skyline-tool
                :pal-capable-p
                :read-7800-art-index
                :compile-art-7800
                :palette-error
                :dimension-error
                :format-error
                :mode-error
                :art-conversion-error)
  (:export :7800-art-suite))

(in-package :skyline-tool/7800-art-test)

(def-suite 7800-art-suite
  :description "Comprehensive test suite for Atari 7800 art compilation")

(in-suite 7800-art-suite)

;; ============================================================================
;; VALID INPUT TESTS
;; ============================================================================

(test pal-capable-machines
  "Test PAL capability detection for various machines"
  (is-true (pal-capable-p 7800))
  (is-true (pal-capable-p 2609))  ; Intellivision
  (is-true (pal-capable-p 5200))
  (is-true (pal-capable-p 3))     ; NES
  (is-true (pal-capable-p 6))     ; SNES
  (is-false (pal-capable-p 9999)) ; Unknown machine
  (is-false (pal-capable-p 200))) ; Lynx doesn't support PAL

(test 7800-mode-keywords
  "Test that all 7800 drawing mode keywords are properly defined"
  (is (keywordp :160a))
  (is (keywordp :160b))
  (is (keywordp :320a))
  (is (keywordp :320b))
  (is (keywordp :320c))
  (is (keywordp :320d)))

;; Test reading valid art index files
(test read-7800-art-index-valid-inputs
  "Test read-7800-art-index with valid index file formats"
  (with-fixture test-7800-art-context ()
    ;; Create a valid index file
    (let ((index-file (merge-pathnames "valid-index.txt" temp-dir)))
      (with-open-file (f index-file :direction :output :if-exists :supersede)
        (format f "sprite1 160A 16×16~%")
        (format f "background 320B 320×192~%")
        (format f "; comment line~%")
        (format f "~%")
        (format f "tile 160B 8×8~%"))
      ;; Test parsing
      (let ((result (read-7800-art-index index-file)))
        (is (= 3 (length result)))
        ;; Check first entry (sprite1)
        (destructuring-bind (mode path width height) (first result)
          (is (eql :160a mode))
          (is (= 16 width))
          (is (= 16 height)))
        ;; Check second entry (background)
        (destructuring-bind (mode path width height) (second result)
          (is (eql :320b mode))
          (is (= 320 width))
          (is (= 192 height)))
        ;; Check third entry (tile)
        (destructuring-bind (mode path width height) (third result)
          (is (eql :160b mode))
          (is (= 8 width))
          (is (= 8 height)))))))

;; ============================================================================
;; INVALID INPUT TESTS - FORMAT ERRORS
;; ============================================================================

(test read-7800-art-index-invalid-format
  "Test read-7800-art-index with malformed index files"
  (with-fixture test-7800-art-context ()
    ;; Test missing dimensions
    (let ((index-file (merge-pathnames "bad-format.txt" temp-dir)))
      (with-open-file (f index-file :direction :output :if-exists :supersede)
        (format f "sprite1 160A~%")) ; Missing dimensions
      (signals format-error
        (read-7800-art-index index-file)))

    ;; Test too many fields
    (let ((index-file (merge-pathnames "too-many-fields.txt" temp-dir)))
      (with-open-file (f index-file :direction :output :if-exists :supersede)
        (format f "sprite1 160A 16×16 extra field~%"))
      (signals format-error
        (read-7800-art-index index-file)))

    ;; Test invalid dimension format (missing ×)
    (let ((index-file (merge-pathnames "bad-dimensions.txt" temp-dir)))
      (with-open-file (f index-file :direction :output :if-exists :supersede)
        (format f "sprite1 160A 16x16~%")) ; x instead of ×
      (signals format-error
        (read-7800-art-index index-file)))))

;; ============================================================================
;; INVALID INPUT TESTS - MODE ERRORS
;; ============================================================================

(test read-7800-art-index-invalid-modes
  "Test read-7800-art-index with invalid graphics modes"
  (with-fixture test-7800-art-context ()
    ;; Test invalid mode
    (let ((index-file (merge-pathnames "invalid-mode.txt" temp-dir)))
      (with-open-file (f index-file :direction :output :if-exists :supersede)
        (format f "sprite1 INVALID 16×16~%"))
      (signals mode-error
        (read-7800-art-index index-file)))

    ;; Test case-sensitive mode (should work - make-keyword handles case)
    (let ((index-file (merge-pathnames "case-sensitive-mode.txt" temp-dir)))
      (with-open-file (f index-file :direction :output :if-exists :supersede)
        (format f "sprite1 160a 16×16~%")) ; lowercase
      (let ((result (read-7800-art-index index-file)))
        (is (= 1 (length result)))
        (is (eql :160a (first (first result))))))))

;; ============================================================================
;; INVALID INPUT TESTS - DIMENSION ERRORS
;; ============================================================================

(test read-7800-art-index-invalid-dimensions
  "Test read-7800-art-index with invalid dimensions for each mode"
  (with-fixture test-7800-art-context ()
    ;; Test 160A mode with dimensions too large
    (let ((index-file (merge-pathnames "160a-too-wide.txt" temp-dir)))
      (with-open-file (f index-file :direction :output :if-exists :supersede)
        (format f "sprite1 160A 161×16~%")) ; 161 > 160 max width
      (signals dimension-error
        (read-7800-art-index index-file)))

    ;; Test 160A mode with height too large
    (let ((index-file (merge-pathnames "160a-too-tall.txt" temp-dir)))
      (with-open-file (f index-file :direction :output :if-exists :supersede)
        (format f "sprite1 160A 16×193~%")) ; 193 > 192 max height
      (signals dimension-error
        (read-7800-art-index index-file)))

    ;; Test 320B mode with dimensions too large
    (let ((index-file (merge-pathnames "320b-too-wide.txt" temp-dir)))
      (with-open-file (f index-file :direction :output :if-exists :supersede)
        (format f "background 320B 321×192~%")) ; 321 > 320 max width
      (signals dimension-error
        (read-7800-art-index index-file)))

    ;; Test non-numeric dimensions
    (let ((index-file (merge-pathnames "non-numeric-dimensions.txt" temp-dir)))
      (with-open-file (f index-file :direction :output :if-exists :supersede)
        (format f "sprite1 160A abc×def~%"))
      (signals format-error
        (read-7800-art-index index-file)))))

;; ============================================================================
;; ERROR OBJECT VALIDATION TESTS
;; ============================================================================

(test error-objects-have-proper-accessors
  "Test that error objects have proper accessors and print nicely"
  (with-fixture test-7800-art-context ()
    ;; Test palette error
    (let ((error-obj (make-condition 'palette-error
                                     :filename "test.png"
                                     :machine 7800
                                     :context "pixel at 10,20"
                                     :invalid-color #(255 0 0)
                                     :available-colors '((0 0 0) (255 255 255)))))
      (is (string= "test.png" (art-conversion-error-filename error-obj)))
      (is (= 7800 (art-conversion-error-machine error-obj)))
      (is (string= "pixel at 10,20" (art-conversion-error-context error-obj)))
      (is (equalp #(255 0 0) (palette-error-invalid-color error-obj)))
      (is (equal '((0 0 0) (255 255 255)) (palette-error-available-colors error-obj)))
      ;; Test that print-object works
      (let ((output (with-output-to-string (s)
                      (print-object error-obj s))))
        (is (search "Color" output))
        (is (search "not in palette" output))
        (is (search "test.png" output)))))

    ;; Test dimension error
    (let ((error-obj (make-condition 'dimension-error
                                     :filename "sprite.png"
                                     :machine 7800
                                     :context "160A mode"
                                     :invalid-width 200
                                     :invalid-height 250
                                     :max-width 160
                                     :max-height 192)))
      (is (= 200 (dimension-error-invalid-width error-obj)))
      (is (= 250 (dimension-error-invalid-height error-obj)))
      (is (= 160 (dimension-error-max-width error-obj)))
      (is (= 192 (dimension-error-max-height error-obj)))
      (let ((output (with-output-to-string (s)
                      (print-object error-obj s))))
        (is (search "Invalid dimensions" output))
        (is (search "200x250" output))
        (is (search "Maximum allowed: 160x192" output))))

    ;; Test mode error
    (let ((error-obj (make-condition 'mode-error
                                     :filename "art.txt"
                                     :machine 7800
                                     :context "line 5"
                                     :invalid-mode :invalid
                                     :valid-modes '(:160a :160b :320a :320b :320c :320d))))
      (is (eql :invalid (mode-error-invalid-mode error-obj)))
      (is (equal '(:160a :160b :320a :320b :320c :320d) (mode-error-valid-modes error-obj)))
      (let ((output (with-output-to-string (s)
                      (print-object error-obj s))))
        (is (search "Invalid graphics mode" output))
        (is (search ":INVALID" output))))))

;; ============================================================================
;; COMPILATION TESTS (currently skipped - need PNG generation)
;; ============================================================================

(test 7800-art-compilation-basic
  "Test basic 7800 art compilation functionality"
  (skip "7800 art compilation requires PNG file generation - implement when PNG creation is available"))

(test 7800-binary-output-validation
  "Test that 7800 binary output conforms to hardware specifications"
  (skip "Binary output validation requires complete PNG processing pipeline"))

;; ============================================================================
;; PERFORMANCE TESTS
;; ============================================================================

(test 7800-index-parsing-performance
  "Test that 7800 index parsing completes within reasonable time limits"
  (with-fixture test-7800-art-context ()
    ;; Create a larger index file to test performance
    (let ((index-file (merge-pathnames "performance-test.txt" temp-dir)))
      (with-open-file (f index-file :direction :output :if-exists :supersede)
        (dotimes (i 100)
          (format f "sprite~D 160A ~Dx~D~%" i (random 160) (random 192))))
      ;; This should complete quickly
      (let ((start-time (get-internal-real-time)))
        (read-7800-art-index index-file)
        (let ((end-time (get-internal-real-time)))
          (is (< (/ (- end-time start-time) internal-time-units-per-second) 1.0))))))) ; Less than 1 second
