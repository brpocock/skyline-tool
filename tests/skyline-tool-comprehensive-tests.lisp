(in-package :skyline-tool/test)

(def-suite skyline-tool-comprehensive-tests
  :description "Comprehensive tests for all Skyline-Tool subcommands")

(in-suite skyline-tool-comprehensive-tests)

(test compile-2600-playfield-basic-functionality
  "Test basic playfield compilation functionality"
  (let* ((test-png-path (merge-pathnames "test-data/test-playfield-16x16.png"
                                        (asdf:system-source-directory :skyline-tool)))
         (output-path (merge-pathnames "test-output/playfield-test.bas"
                                      (asdf:system-source-directory :skyline-tool))))
    (ensure-directories-exist output-path)
    
    ;; Test with valid 16×16 playfield
    (finishes 
      (skyline-tool::compile-2600-playfield output-path test-png-path "NTSC"))
    
    ;; Verify output file was created and contains expected content
    (is-true (probe-file output-path))
    (let ((content (uiop:read-file-string output-path)))
      (is (search "SetPlayfield" content))
      (is (search "playfield:" content))
      (is (search "pfcolors:" content))
      (is (search "return" content)))))

(test compile-2600-playfield-dimension-validation
  "Test playfield dimension validation"
  (let* ((invalid-png-path (merge-pathnames "test-data/test-playfield-invalid.png"
                                           (asdf:system-source-directory :skyline-tool)))
         (output-path (merge-pathnames "test-output/playfield-invalid.bas"
                                      (asdf:system-source-directory :skyline-tool))))
    (ensure-directories-exist output-path)
    
    ;; Test with invalid dimensions
    (signals error
      (skyline-tool::compile-2600-playfield output-path invalid-png-path "NTSC"))))

(test compile-2600-playfield-tv-standards
  "Test playfield compilation for different TV standards"
  (let* ((test-png-path (merge-pathnames "test-data/test-playfield-32x32.png"
                                        (asdf:system-source-directory :skyline-tool)))
         (ntsc-output (merge-pathnames "test-output/playfield-ntsc.bas"
                                      (asdf:system-source-directory :skyline-tool)))
         (pal-output (merge-pathnames "test-output/playfield-pal.bas"
                                     (asdf:system-source-directory :skyline-tool)))
         (secam-output (merge-pathnames "test-output/playfield-secam.bas"
                                       (asdf:system-source-directory :skyline-tool))))
    (ensure-directories-exist ntsc-output)
    (ensure-directories-exist pal-output)
    (ensure-directories-exist secam-output)
    
    ;; Test NTSC
    (finishes (skyline-tool::compile-2600-playfield ntsc-output test-png-path "NTSC"))
    (is-true (probe-file ntsc-output))
    
    ;; Test PAL
    (finishes (skyline-tool::compile-2600-playfield pal-output test-png-path "PAL"))
    (is-true (probe-file pal-output))
    
    ;; Test SECAM
    (finishes (skyline-tool::compile-2600-playfield secam-output test-png-path "SECAM"))
    (is-true (probe-file secam-output))))

(test compile-2600-font-8x16-basic-functionality
  "Test basic 8×16 font compilation functionality"
  (let* ((test-png-path (merge-pathnames "test-data/test-font-64x16.png"
                                        (asdf:system-source-directory :skyline-tool)))
         (output-path (merge-pathnames "test-output/font-test.bas"
                                      (asdf:system-source-directory :skyline-tool))))
    (ensure-directories-exist output-path)
    
    ;; Test with valid 64×16 font
    (finishes 
      (skyline-tool::compile-2600-font-8x16 output-path test-png-path))
    
    ;; Verify output file was created and contains expected content
    (is-true (probe-file output-path))
    (let ((content (uiop:read-file-string output-path)))
      (is (search "SetFont" content))
      (is (search "data FontData" content))
      (is (search ".byte %" content))
      (is (search "return" content)))))

(test compile-2600-font-8x16-dimension-validation
  "Test font dimension validation"
  (let* ((invalid-png-path (merge-pathnames "test-data/test-font-invalid.png"
                                           (asdf:system-source-directory :skyline-tool)))
         (output-path (merge-pathnames "test-output/font-invalid.bas"
                                      (asdf:system-source-directory :skyline-tool))))
    (ensure-directories-exist output-path)
    
    ;; Test with invalid dimensions
    (signals error
      (skyline-tool::compile-2600-font-8x16 output-path invalid-png-path))))

(test compile-chaos-character-basic-functionality
  "Test basic chaos character compilation functionality"
  (let* ((test-png-path (merge-pathnames "test-data/test-character-64x256.png"
                                        (asdf:system-source-directory :skyline-tool)))
         (output-path (merge-pathnames "test-output/character-test.bas"
                                      (asdf:system-source-directory :skyline-tool))))
    (ensure-directories-exist output-path)
    
    ;; Test with valid 64×256 character
    (finishes 
      (skyline-tool::compile-chaos-character output-path test-png-path))
    
    ;; Verify output file was created and contains expected content
    (is-true (probe-file output-path))
    (let ((content (uiop:read-file-string output-path)))
      (is (search "SetCharacter" content))
      (is (search "data CharacterFrames" content))
      (is (search "data CharacterFrameMap" content))
      (is (search ".byte %" content))
      (is (search "return" content)))))

(test compile-chaos-character-dimension-validation
  "Test character dimension validation"
  (let* ((invalid-png-path (merge-pathnames "test-data/test-character-invalid.png"
                                           (asdf:system-source-directory :skyline-tool)))
         (output-path (merge-pathnames "test-output/character-invalid.bas"
                                      (asdf:system-source-directory :skyline-tool))))
    (ensure-directories-exist output-path)
    
    ;; Test with invalid dimensions
    (signals error
      (skyline-tool::compile-chaos-character output-path invalid-png-path))))

(test compile-2600-special-sprites-basic-functionality
  "Test basic special sprites compilation functionality"
  (let* ((test-png-path (merge-pathnames "test-data/test-special-sprites-24x16.png"
                                        (asdf:system-source-directory :skyline-tool)))
         (output-path (merge-pathnames "test-output/special-sprites-test.bas"
                                      (asdf:system-source-directory :skyline-tool))))
    (ensure-directories-exist output-path)
    
    ;; Test with valid 24×16 special sprites
    (finishes 
      (skyline-tool::compile-2600-special-sprites output-path test-png-path))
    
    ;; Verify output file was created and contains expected content
    (is-true (probe-file output-path))
    (let ((content (uiop:read-file-string output-path)))
      (is (search "SetSpecialSprites" content))
      (is (search "data SpecialSprites" content))
      (is (search ".byte %" content))
      (is (search "return" content)))))

(test compile-2600-special-sprites-dimension-validation
  "Test special sprites dimension validation"
  (let* ((invalid-png-path (merge-pathnames "test-data/test-special-sprites-invalid.png"
                                           (asdf:system-source-directory :skyline-tool)))
         (output-path (merge-pathnames "test-output/special-sprites-invalid.bas"
                                      (asdf:system-source-directory :skyline-tool))))
    (ensure-directories-exist output-path)
    
    ;; Test with invalid dimensions
    (signals error
      (skyline-tool::compile-2600-special-sprites output-path invalid-png-path))))

(test frame-deduplication-functionality
  "Test frame deduplication functionality"
  (let* ((test-frames '(((1 0 1 0 1 0 1 0) (0 1 0 1 0 1 0 1))
                        ((1 0 1 0 1 0 1 0) (0 1 0 1 0 1 0 1))  ; Duplicate
                        ((0 0 0 0 0 0 0 0) (1 1 1 1 1 1 1 1))  ; Different
                        ((1 0 1 0 1 0 1 0) (0 1 0 1 0 1 0 1)))) ; Duplicate again
         (deduplicated (skyline-tool::deduplicate-frames test-frames)))
    
    ;; Should have only 2 unique frames
    (is (= 2 (length deduplicated)))
    
    ;; First frame should be preserved
    (is (equalp (first test-frames) (first deduplicated)))
    
    ;; Third frame should be preserved
    (is (equalp (third test-frames) (second deduplicated)))))

(test blank-row-processing
  "Test blank row processing functionality"
  (let* ((test-frame '((1 0 1 0 1 0 1 0)  ; Valid row
                       (0 0 0 0 0 0 0 0)  ; Blank row
                       (1 1 1 1 1 1 1 1)  ; Valid row
                       (0 0 0 0 0 0 0 0))) ; Blank row
         (processed (skyline-tool::process-blank-rows test-frame)))
    
    ;; Blank rows should be replaced with previous valid row
    (is (equalp (first processed) '(1 0 1 0 1 0 1 0)))  ; First valid row
    (is (equalp (second processed) '(1 0 1 0 1 0 1 0))) ; Blank replaced with first
    (is (equalp (third processed) '(1 1 1 1 1 1 1 1)))  ; Second valid row
    (is (equalp (fourth processed) '(1 1 1 1 1 1 1 1)))) ; Blank replaced with second

(test color-analysis-functionality
  "Test color analysis functionality"
  (let* ((test-palette-pixels (make-array '(32 16) :initial-element 0))
         (test-rgb-colors '((255 0 0) (0 255 0) (255 0 0) (0 0 255))) ; Red, Green, Red, Blue
         (color-counts (skyline-tool::count-rgb-colors test-rgb-colors)))
    
    ;; Red should appear twice, Green and Blue once each
    (is (= 3 (length color-counts)))
    (let ((red-count (cdr (assoc '(255 0 0) color-counts :test #'equalp)))
          (green-count (cdr (assoc '(0 255 0) color-counts :test #'equalp)))
          (blue-count (cdr (assoc '(0 0 255) color-counts :test #'equalp))))
      (is (= 2 red-count))
      (is (= 1 green-count))
      (is (= 1 blue-count)))))

(test xyz-color-averaging
  "Test XYZ color space averaging functionality"
  (let* ((test-colors '((255 0 0) (0 255 0))) ; Red and Green
         (averaged (skyline-tool::average-colors-xyz test-colors)))
    
    ;; Should return a valid RGB triplet
    (is (= 3 (length averaged)))
    (is (every #'numberp averaged))
    (is (every (lambda (x) (and (>= x 0) (<= x 255))) averaged))))

(test palette-color-matching
  "Test palette color matching functionality"
  (let* ((test-rgb '(128 128 128)) ; Gray
         (palette-index (skyline-tool::find-nearest-palette-color test-rgb)))
    
    ;; Should return a valid palette index
    (is (numberp palette-index))
    (is (>= palette-index 0))
    (is (<= palette-index 127))))

(test playfield-x-format-output
  "Test that playfield output uses X/. format"
  (let* ((test-png-path (merge-pathnames "test-data/test-playfield-16x16.png"
                                        (asdf:system-source-directory :skyline-tool)))
         (output-path (merge-pathnames "test-output/playfield-format-test.bas"
                                      (asdf:system-source-directory :skyline-tool))))
    (ensure-directories-exist output-path)
    
    (finishes 
      (skyline-tool::compile-2600-playfield output-path test-png-path "NTSC"))
    
    (let ((content (uiop:read-file-string output-path)))
      ;; Should contain X and . characters, not binary format
      (is (search "X" content))
      (is (search "." content))
      ;; Should not contain binary format
      (is (not (search ".byte %" content))))))

(test font-binary-format-output
  "Test that font output uses binary format"
  (let* ((test-png-path (merge-pathnames "test-data/test-font-64x16.png"
                                        (asdf:system-source-directory :skyline-tool)))
         (output-path (merge-pathnames "test-output/font-format-test.bas"
                                      (asdf:system-source-directory :skyline-tool))))
    (ensure-directories-exist output-path)
    
    (finishes 
      (skyline-tool::compile-2600-font-8x16 output-path test-png-path))
    
    (let ((content (uiop:read-file-string output-path)))
      ;; Should contain binary format
      (is (search ".byte %" content))
      ;; Should not contain X/. format
      (is (not (search "X" content)))
      (is (not (search "." content))))))

(test character-indirection-table
  "Test that character indirection table uses correct formula"
  (let* ((test-png-path (merge-pathnames "test-data/test-character-64x256.png"
                                        (asdf:system-source-directory :skyline-tool)))
         (output-path (merge-pathnames "test-output/character-indirection-test.bas"
                                      (asdf:system-source-directory :skyline-tool))))
    (ensure-directories-exist output-path)
    
    (finishes 
      (skyline-tool::compile-chaos-character output-path test-png-path))
    
    (let ((content (uiop:read-file-string output-path)))
      ;; Should contain indirection table
      (is (search "data CharacterFrameMap" content))
      ;; Should contain frame indices
      (is (search "0" content))
      (is (search "1" content))
      (is (search "2" content)))))

(test proper-indentation-formatting
  "Test that all output uses proper ~10t indentation"
  (let* ((test-png-path (merge-pathnames "test-data/test-playfield-16x16.png"
                                        (asdf:system-source-directory :skyline-tool)))
         (output-path (merge-pathnames "test-output/indentation-test.bas"
                                      (asdf:system-source-directory :skyline-tool))))
    (ensure-directories-exist output-path)
    
    (finishes 
      (skyline-tool::compile-2600-playfield output-path test-png-path "NTSC"))
    
    (let ((content (uiop:read-file-string output-path)))
      ;; Should contain proper indentation (10 spaces)
      (is (search "          playfield:" content))
      (is (search "          pfcolors:" content))
      (is (search "          return" content)))))

(defun run-comprehensive-tests ()
  "Run all comprehensive Skyline-Tool tests"
  (fiveam:run! 'skyline-tool-comprehensive-tests))
