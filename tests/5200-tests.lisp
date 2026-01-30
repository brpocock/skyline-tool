;;; Phantasia SkylineTool/tests/5200-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

;; Use the test package defined by the test runner
(in-package :skyline-tool/test)

(def-suite 5200-tests
  :description "Tests for Atari 5200-specific SkylineTool functionality"
  :in skyline-tool/test)

(in-suite 5200-tests)

;; Test 5200 Mode E bitmap compilation functionality
(test 5200-mode-e-bitmap-compilation
  "Test that 5200 Mode E bitmap compilation produces correct output"
  ;; Test with a simple 8x8 pattern that should produce recognizable output
  (let* ((test-pixels (make-array '(8 8) :element-type '(unsigned-byte 32)
                                  :initial-contents
                                  #(#(0 0 0 0 0 0 0 0)  ; Row 0: all background
                                    #(1 1 1 1 1 1 1 1)  ; Row 1: all foreground
                                    #(0 1 0 1 0 1 0 1)  ; Row 2: alternating pattern
                                    #(1 0 1 0 1 0 1 0)  ; Row 3: inverse alternating
                                    #(0 0 1 1 0 0 1 1)  ; Row 4: 2-pixel pattern
                                    #(1 1 0 0 1 1 0 0)  ; Row 5: inverse 2-pixel
                                    #(0 0 0 0 1 1 1 1)  ; Row 6: half/half
                                    #(1 1 1 1 0 0 0 0)))) ; Row 7: inverse half/half
         (temp-file "Object/5200/tmp5200.s"))

    ;; Compile the test data - this should return a truthy value indicating success
    (is-true (skyline-tool::compile-5200-mode-e-bitmap test-pixels
                                                       :target-dir "Object/5200/")
             "compile-5200-mode-e-bitmap should return truthy value for valid input")

    ;; Verify output file was created
    (is-true (probe-file temp-file)
             "Output file should be created for valid input")

    ;; Verify output contains expected assembly structure
    (when (probe-file temp-file)
      (with-open-file (stream temp-file :direction :input)
        (let ((content (alexandria:read-stream-content-into-string stream)))
          ;; Check for proper assembly file structure
          (is-true (search ";;; -*- fundamental -*-" content)
                   "Output should contain fundamental mode comment")
          (is-true (search ".block" content)
                   "Output should contain .block directive")
          (is-true (search ".bend" content)
                   "Output should contain .bend directive")

          ;; Check for shape data labels
          (is-true (search "Shape:" content)
                   "Output should contain Shape data label")
          (is-true (search "CoLu:" content)
                   "Output should contain color data label")

          ;; Check for valid assembly byte directives
          (is-true (cl-ppcre:scan "\\.byte \\$[0-9a-fA-F]{2}" content)
                   "Output should contain properly formatted .byte directives")

          ;; Verify dimensions are reported correctly
          (is-true (search "Height = 8" content)
                   "Should report correct height")
          (is-true (search "Width = 8" content)
                   "Should report correct width")

          ;; Verify specific byte patterns for known input (Mode E: 2 bits per pixel, 4 pixels per byte)
          ;; Row 0: [0,0,0,0] -> 00 00 00 00 = $00
          ;; Row 1: [1,1,1,1] -> 01 01 01 01 = $55 (not $FF!)
          ;; Row 2: [0,1,0,1] -> 00 01 00 01 = $10
          ;; Row 3: [1,0,1,0] -> 01 00 01 00 = $44
          ;; Row 4: [0,0,1,1] -> 00 00 01 01 = $05
          ;; Row 5: [1,1,0,0] -> 01 01 00 00 = $50
          ;; Row 6: [0,0,0,0] + [1,1,1,1] -> $00, $55
          ;; Row 7: [1,1,1,1] + [0,0,0,0] -> $55, $00
          (is-true (search ".byte $00" content)
                   "All-zero chunks should produce .byte $00")
          (is-true (search ".byte $55" content)
                   "All-ones chunks should produce .byte $55 (not $FF)")
          (is-true (search ".byte $10" content)
                   "Alternating 0,1 pattern should produce .byte $10 (not $AA)")
          (is-true (search ".byte $44" content)
                   "Alternating 1,0 pattern should produce .byte $44 (not $55)")
          (is-true (search ".byte $05" content)
                   "00 00 01 01 pattern should produce .byte $05")
          (is-true (search ".byte $50" content)
                   "01 01 00 00 pattern should produce .byte $50"))))))

;; Test 5200 Mode E bitmap error handling
(test 5200-mode-e-error-handling
  "Test that 5200 Mode E bitmap compilation handles errors properly"
  ;; Test with nil input
  (signals error (skyline-tool::compile-5200-mode-e-bitmap nil)
           "Should signal error for nil input")

  ;; Test with wrong array dimensions
  (signals error (skyline-tool::compile-5200-mode-e-bitmap (make-array '(0 0)))
           "Should signal error for zero-dimension arrays")

  ;; Test with non-array input
  (signals error (skyline-tool::compile-5200-mode-e-bitmap "not an array")
           "Should signal error for non-array input")

  ;; Test with array of wrong element type
  (signals error (skyline-tool::compile-5200-mode-e-bitmap (make-array '(8 8) :element-type 'character))
           "Should signal error for wrong element type"))

;; Test 5200 Mode E bitmap with different sizes
(test 5200-mode-e-different-sizes
  "Test 5200 Mode E bitmap compilation with different valid sizes"
  ;; Test with 16x16 array
  (let ((large-pixels (make-array '(16 16) :element-type '(unsigned-byte 32) :initial-element 0)))
    (is-true (skyline-tool::compile-5200-mode-e-bitmap large-pixels
                                                        :png-file (make-pathname :name "test-16x16" :type "png")
                                                        :target-dir (make-pathname :directory '(:absolute "Object" "5200")))
             "Should handle 16x16 arrays and return truthy value"))

  ;; Test with 4x4 array
  (let ((small-pixels (make-array '(4 4) :element-type '(unsigned-byte 32) :initial-element 1)))
    (is-true (skyline-tool::compile-5200-mode-e-bitmap small-pixels
                                                        :png-file (make-pathname :name "test-4x4" :type "png")
                                                        :target-dir (make-pathname :directory '(:absolute "Object" "5200")))
             "Should handle 4x4 arrays and return truthy value"))

  ;; Test with rectangular array (not square)
  (let ((rect-pixels (make-array '(8 16) :element-type '(unsigned-byte 32) :initial-element 0)))
    (is-true (skyline-tool::compile-5200-mode-e-bitmap rect-pixels :target-dir "Object/5200/")
             "Should handle rectangular arrays and return truthy value")))

;; Test 5200 Mode E palette handling
(test 5200-mode-e-palette-handling
  "Test that 5200 Mode E bitmap compilation handles palette options correctly"
  ;; Test with color-per-line-p = nil (single palette for entire image)
  (let* ((test-pixels (make-array '(8 8) :element-type '(unsigned-byte 32)
                                  :initial-contents
                                  #(#(0 0 0 0 0 0 0 0)  ; Row 0: all color 0
                                    #(1 1 1 1 1 1 1 1)  ; Row 1: all color 1
                                    #(0 1 0 1 0 1 0 1)  ; Row 2: alternating
                                    #(2 2 2 2 2 2 2 2)  ; Row 3: all color 2
                                    #(0 0 1 1 0 0 1 1)  ; Row 4: 2-pixel pattern
                                    #(1 1 2 2 1 1 2 2)  ; Row 5: mixed pattern
                                    #(0 0 0 0 1 1 1 1)  ; Row 6: half/half
                                    #(2 2 2 2 0 0 0 0)))) ; Row 7: inverse half/half
         (temp-file "Object/5200/tmp5200-palette.s"))

    ;; Test with single palette (color-per-line-p = nil)
    (is-true (skyline-tool::compile-5200-mode-e-bitmap test-pixels
                                                       :png-file (make-pathname :name "test-palette" :type "png")
                                                       :target-dir "Object/5200/"
                                                       :color-per-line-p nil)
             "Should handle single palette mode")

    ;; Verify output file was created
    (is-true (probe-file temp-file)
             "Output file should be created for palette test")

    ;; Verify single palette produces consistent colors across all lines
    (when (probe-file temp-file)
      (with-open-file (stream temp-file :direction :input)
        (let ((content (alexandria:read-stream-content-into-string stream)))
          ;; Should have only one set of CoLu data (not per-line)
          (let ((colu-count (count-substring "CoLu:" content)))
            (is (= 1 colu-count)
                "Single palette mode should have only one CoLu section")))))))

;; Test 5200 Mode E base palette functionality
(test 5200-mode-e-base-palette
  "Test that 5200 Mode E bitmap compilation respects base-palette parameter"
  (let* ((test-pixels (make-array '(4 4) :element-type '(unsigned-byte 32)
                                  :initial-contents
                                  #(#(0 1 0 1)  ; Simple pattern
                                    #(1 0 1 0)
                                    #(0 1 0 1)
                                    #(1 0 1 0))))
         (temp-file "Object/5200/tmp5200-base-palette.s"))

    ;; Test with custom base palette
    (is-true (skyline-tool::compile-5200-mode-e-bitmap test-pixels
                                                       :png-file (make-pathname :name "test-base-palette" :type "png")
                                                       :target-dir "Object/5200/"
                                                       :base-palette '(10 20 30 40)
                                                       :color-per-line-p nil)
             "Should handle base-palette parameter")

    ;; Verify output file was created
    (is-true (probe-file temp-file)
             "Output file should be created for base palette test")))

;; Test 5200 Mode E compression functionality
(test 5200-mode-e-compression
  "Test that 5200 Mode E bitmap compilation handles compression correctly"
  ;; Test small image (should not compress by default, threshold is 512 pixels)
  (let* ((small-pixels (make-array '(8 8) :element-type '(unsigned-byte 32) :initial-element 0)) ; 64 pixels
         (small-file "Object/5200/tmp5200-small.s"))

    (is-true (skyline-tool::compile-5200-mode-e-bitmap small-pixels
                                                       :png-file (make-pathname :name "test-small" :type "png")
                                                       :target-dir "Object/5200/")
             "Should handle small image without compression")

    (when (probe-file small-file)
      (with-open-file (stream small-file :direction :input)
        (let ((content (alexandria:read-stream-content-into-string stream)))
          ;; Should indicate uncompressed
          (is-true (search "(stored uncompressed)" content)
                   "Small image should be stored uncompressed")))))

  ;; Test large image (should compress by default)
  (let* ((large-pixels (make-array '(32 32) :element-type '(unsigned-byte 32) :initial-element 0)) ; 1024 pixels
         (large-file "Object/5200/tmp5200-large.s"))

    (is-true (skyline-tool::compile-5200-mode-e-bitmap large-pixels
                                                       :png-file (make-pathname :name "test-large" :type "png")
                                                       :target-dir "Object/5200/")
             "Should handle large image with compression")

    (when (probe-file large-file)
      (with-open-file (stream large-file :direction :input)
        (let ((content (alexandria:read-stream-content-into-string stream)))
          ;; Should indicate compressed
          (is-true (search "(after decompression)" content)
                   "Large image should be stored compressed")))))

  ;; Test explicit compression control
  (let* ((medium-pixels (make-array '(16 16) :element-type '(unsigned-byte 32) :initial-element 0)) ; 256 pixels
         (compress-file "Object/5200/tmp5200-compress.s")
         (nocompress-file "Object/5200/tmp5200-nocompress.s"))

    ;; Force compression on medium image
    (is-true (skyline-tool::compile-5200-mode-e-bitmap medium-pixels
                                                       :png-file (make-pathname :name "test-compress" :type "png")
                                                       :target-dir "Object/5200/"
                                                       :compressp t)
             "Should handle explicit compression request")

    ;; Force no compression on medium image
    (is-true (skyline-tool::compile-5200-mode-e-bitmap medium-pixels
                                                       :png-file (make-pathname :name "test-nocompress" :type "png")
                                                       :target-dir "Object/5200/"
                                                       :compressp nil)
             "Should handle explicit no-compression request")

    ;; Verify compression settings
    (when (and (probe-file compress-file) (probe-file nocompress-file))
      (with-open-file (stream compress-file :direction :input)
        (let ((compress-content (alexandria:read-stream-content-into-string stream)))
          (is-true (search "(after decompression)" compress-content)
                   "Explicit compression should be applied")))

      (with-open-file (stream nocompress-file :direction :input)
        (let ((nocompress-content (alexandria:read-stream-content-into-string stream)))
          (is-true (search "(stored uncompressed)" nocompress-content)
                   "Explicit no-compression should be honored"))))))

;; Test 5200 GTIA player sprite compilation
(test 5200-gtia-player-compilation
  "Test that 5200 GTIA player sprite compilation produces correct output"
  ;; Test with a simple 8x8 sprite pattern
  (let* ((sprite-pixels (make-array '(8 8) :element-type '(unsigned-byte 32)
                                     :initial-contents
                                     #(#(0 1 1 1 1 1 1 0)  ; Row 0: outline
                                       #(1 0 0 0 0 0 0 1)  ; Row 1: inside empty
                                       #(1 0 1 0 1 0 1 0)  ; Row 2: checkerboard
                                       #(1 0 0 0 0 0 0 1)  ; Row 3: inside empty
                                       #(1 0 1 0 1 0 1 0)  ; Row 4: checkerboard
                                       #(1 0 0 0 0 0 0 1)  ; Row 5: inside empty
                                       #(1 0 1 0 1 0 1 0)  ; Row 6: checkerboard
                                       #(0 1 1 1 1 1 1 0)))) ; Row 7: outline
         (temp-file "Object/5200/tmp5200-gtia.s"))

    ;; Compile the sprite data
    (is-true (skyline-tool::compile-gtia-player
              (make-pathname :name "test-sprite" :type "png")
              "Object/5200/"
              8 8 sprite-pixels)
             "compile-gtia-player should return truthy value for valid input")

    ;; Verify output file was created
    (is-true (probe-file temp-file)
             "Output file should be created for GTIA player test")

    ;; Verify output contains expected assembly structure
    (when (probe-file temp-file)
      (with-open-file (stream temp-file :direction :input)
        (let ((content (alexandria:read-stream-content-into-string stream)))
          ;; Check for proper assembly file structure
          (is-true (search ";;; -*- fundamental -*-" content)
                   "Output should contain fundamental mode comment")
          (is-true (search ".block" content)
                   "Output should contain .block directive")
          (is-true (search ".bend" content)
                   "Output should contain .bend directive")

          ;; Check for shape data
          (is-true (search "Shape:" content)
                   "Output should contain Shape data label")

          ;; Check for valid assembly byte directives
          (is-true (cl-ppcre:scan "\\.byte \\$[0-9a-fA-F]{2}" content)
                   "Output should contain properly formatted .byte directives")

          ;; Verify dimensions are reported correctly
          (is-true (search "Height = 8" content)
                   "Should report correct height")
          (is-true (search "Width = 8" content)
                   "Should report correct width")

          ;; Verify specific byte patterns for known input
          ;; Row 0: [0,1,1,1,1,1,1,0] -> bits 01111110 -> $7E
          ;; Row 1: [1,0,0,0,0,0,0,1] -> bits 10000001 -> $81
          ;; Row 2: [1,0,1,0,1,0,1,0] -> bits 10101010 -> $AA
          (is-true (search ".byte $7E" content)
                   "Outline pattern should produce .byte $7E")
          (is-true (search ".byte $81" content)
                   "Inverted outline should produce .byte $81")
          (is-true (search ".byte $AA" content)
                   "Checkerboard pattern should produce .byte $AA"))))))

;; Test 5200 dispatch functionality
(test 5200-dispatch-functionality
  "Test 5200 PNG dispatch functionality"
  ;; Verify dispatch-png% generic function exists
  (is-true (fboundp 'skyline-tool::dispatch-png%)
           "dispatch-png% generic function should exist")

  ;; Verify that there's a method specialized for 5200
  (is-true (find-method #'skyline-tool::dispatch-png% '() (list (list 'eql 5200) t t t t t t t) nil)
           "dispatch-png% should have a method for 5200")

  ;; Test skybox dispatch (256×16)
  (let ((skybox-pixels (make-array '(256 16) :element-type '(unsigned-byte 32) :initial-element 0)))
    (is-true (skyline-tool::dispatch-png% 5200
                                         (make-pathname :name "test-skybox" :type "png")
                                         "Object/5200/"
                                         skybox-pixels 16 256 nil skybox-pixels)
             "Should handle 256×16 skybox images"))

  ;; Test playfield dispatch (160 width)
  (let ((playfield-pixels (make-array '(160 96) :element-type '(unsigned-byte 32) :initial-element 0)))
    (is-true (skyline-tool::dispatch-png% 5200
                                         (make-pathname :name "test-playfield" :type "png")
                                         "Object/5200/"
                                         playfield-pixels 96 160 nil playfield-pixels)
             "Should handle 160-wide playfield images"))

  ;; Test icon dispatch (12×12)
  (let ((icon-pixels (make-array '(12 12) :element-type '(unsigned-byte 32) :initial-element 0)))
    (is-true (skyline-tool::dispatch-png% 5200
                                         (make-pathname :name "test-icon" :type "png")
                                         "Object/5200/"
                                         icon-pixels 12 12 nil icon-pixels)
             "Should handle 12×12 icon images"))

  ;; Test large icon dispatch (256×64)
  (let ((large-icon-pixels (make-array '(256 64) :element-type '(unsigned-byte 32) :initial-element 0)))
    (is-true (skyline-tool::dispatch-png% 5200
                                         (make-pathname :name "test-large-icon" :type "png")
                                         "Object/5200/"
                                         large-icon-pixels 64 256 nil large-icon-pixels)
             "Should handle 256×64 large icon images"))

  ;; Test sprite dispatch (width divisible by 8)
  (let ((sprite-pixels (make-array '(16 8) :element-type '(unsigned-byte 32) :initial-element 0)))
    (is-true (skyline-tool::dispatch-png% 5200
                                         (make-pathname :name "test-sprite" :type "png")
                                         "Object/5200/"
                                         sprite-pixels 8 16 nil sprite-pixels)
             "Should handle sprite images with width divisible by 8"))

  ;; Test unknown dimensions (should error)
  (let ((unknown-pixels (make-array '(10 10) :element-type '(unsigned-byte 32) :initial-element 0)))
    (signals error (skyline-tool::dispatch-png% 5200
                                               (make-pathname :name "test-unknown" :type "png")
                                               "Object/5200/"
                                               unknown-pixels 10 10 nil unknown-pixels)
             "Should signal error for unknown image dimensions")))

;; Test 5200 edge cases and advanced functionality
(test 5200-edge-cases
  "Test 5200 conversion with edge cases and advanced scenarios"
  ;; Test very large image (Mode E territory)
  (let ((large-pixels (make-array '(320 200) :element-type '(unsigned-byte 32) :initial-element 0)))
    (is-true (skyline-tool::compile-5200-mode-e-bitmap large-pixels
                                                       :png-file (make-pathname :name "test-very-large" :type "png")
                                                       :target-dir "Object/5200/")
             "Should handle very large images (320×200)"))

  ;; Test extremely wide image
  (let ((wide-pixels (make-array '(512 4) :element-type '(unsigned-byte 32) :initial-element 0)))
    (is-true (skyline-tool::compile-5200-mode-e-bitmap wide-pixels
                                                       :png-file (make-pathname :name "test-wide" :type "png")
                                                       :target-dir "Object/5200/")
             "Should handle extremely wide images"))

  ;; Test tall narrow image
  (let ((tall-pixels (make-array '(4 192) :element-type '(unsigned-byte 32) :initial-element 0)))
    (is-true (skyline-tool::compile-5200-mode-e-bitmap tall-pixels
                                                       :png-file (make-pathname :name "test-tall" :type "png")
                                                       :target-dir "Object/5200/")
             "Should handle tall narrow images"))

  ;; Test with many colors (palette reduction)
  (let ((colorful-pixels (make-array '(8 8) :element-type '(unsigned-byte 32))))
    ;; Fill with 10 different colors
    (dotimes (y 8)
      (dotimes (x 8)
        (setf (aref colorful-pixels x y) (mod (+ x y) 10))))
    (is-true (skyline-tool::compile-5200-mode-e-bitmap colorful-pixels
                                                       :png-file (make-pathname :name "test-many-colors" :type "png")
                                                       :target-dir "Object/5200/")
             "Should handle images with many colors (palette reduction)"))

  ;; Test color-per-line vs single palette comparison
  (let ((test-pixels (make-array '(8 4) :element-type '(unsigned-byte 32)
                                 :initial-contents
                                 #(#(0 1 0 1 0 1 0 1)
                                   #(1 0 1 0 1 0 1 0)
                                   #(0 2 0 2 0 2 0 2)  ; Introduce color 2
                                   #(2 0 2 0 2 0 2 0))))
        (perline-file "Object/5200/tmp5200-perline.s")
        (single-file "Object/5200/tmp5200-single.s"))

    ;; Test with per-line palette
    (is-true (skyline-tool::compile-5200-mode-e-bitmap test-pixels
                                                       :png-file (make-pathname :name "test-perline" :type "png")
                                                       :target-dir "Object/5200/"
                                                       :color-per-line-p t)
             "Should handle color-per-line palette mode")

    ;; Test with single palette
    (is-true (skyline-tool::compile-5200-mode-e-bitmap test-pixels
                                                       :png-file (make-pathname :name "test-single" :type "png")
                                                       :target-dir "Object/5200/"
                                                       :color-per-line-p nil)
             "Should handle single palette mode")

    ;; Verify different output structures
    (when (and (probe-file perline-file) (probe-file single-file))
      (with-open-file (stream perline-file :direction :input)
        (let ((perline-content (alexandria:read-stream-content-into-string stream)))
          ;; Per-line should have multiple CoLu sections
          (let ((colu-count (count-substring "CoLu:" perline-content)))
            (is (>= colu-count 4)
                "Per-line palette should have CoLu sections for each line"))))

      (with-open-file (stream single-file :direction :input)
        (let ((single-content (alexandria:read-stream-content-into-string stream)))
          ;; Single should have one CoLu section
          (let ((colu-count (count-substring "CoLu:" single-content)))
            (is (= colu-count 1)
                "Single palette should have only one CoLu section")))))))

;; Test 5200 integration - full pipeline validation
(test 5200-integration-pipeline
  "Test that 5200 conversion produces valid assembly that can be parsed"
  ;; Test complete conversion pipeline for different types
  (let ((test-cases '((:name "skybox" :width 256 :height 16 :compress nil :palette nil)
                      (:name "playfield" :width 160 :height 96 :compress t :palette '(0 7 27 83))
                      (:name "icon" :width 12 :height 12 :compress nil :palette '(0 27 83 7))
                      (:name "sprite" :width 16 :height 8 :type :sprite))))

    (dolist (test-case test-cases)
      (destructuring-bind (&key name width height compress palette (type :bitmap)) test-case
        (let* ((pixels (make-array (list width height) :element-type '(unsigned-byte 32) :initial-element 0))
               (png-file (make-pathname :name (format nil "test-integration-~a" name) :type "png"))
               (asm-file (format nil "Object/5200/test-integration-~a.s" name)))

          ;; Fill with some pattern data
          (dotimes (y height)
            (dotimes (x width)
              (setf (aref pixels x y) (mod (+ x y) 4)))) ; Use 4 colors

          ;; Generate assembly
          (if (eq type :sprite)
              (is-true (skyline-tool::compile-gtia-player png-file "Object/5200/" height width pixels)
                       (format nil "Should generate GTIA player assembly for ~a" name))
            (is-true (skyline-tool::compile-5200-mode-e-bitmap pixels
                                                               :png-file png-file
                                                               :target-dir "Object/5200/"
                                                               :compressp compress
                                                               :base-palette palette)
                     (format nil "Should generate bitmap assembly for ~a" name)))

          ;; Verify assembly file was created and has valid structure
          (when (probe-file asm-file)
            (with-open-file (stream asm-file :direction :input)
              (let ((content (alexandria:read-stream-content-into-string stream)))
                ;; Check for basic assembly structure
                (is-true (search ".block" content)
                         (format nil "~a should contain .block directive" name))
                (is-true (search ".bend" content)
                         (format nil "~a should contain .bend directive" name))
                ;; Check for valid byte directives (should not contain malformed ones)
                (is-false (search ".byte $" content)
                          (format nil "~a should not contain malformed .byte directives" name))
                ;; Should have some actual data bytes
                (is-true (cl-ppcre:scan "\\.byte \\$[0-9a-fA-F]{2}" content)
                         (format nil "~a should contain properly formatted byte data" name))))))))))

;; Test 5200 platform validation
(test 5200-platform-validation
  "Test that 5200 is properly recognized as a valid platform"
  (let ((skyline-tool::*machine* 5200))
    ;; Test that machine is recognized as valid
    (is-true (skyline-tool::machine-valid-p)
             "5200 should be recognized as a valid machine")

    ;; Test machine name functions
    (is (stringp (skyline-tool::machine-short-name))
        "machine-short-name should return a string")
    (is (stringp (skyline-tool::machine-long-name))
        "machine-long-name should return a string")))

;; Test 5200 unimplemented functions signal appropriate errors
(test 5200-unimplemented-functions
  "Test that unimplemented 5200 functions signal appropriate errors"
  ;; Test compile-art-5200 (not implemented)
  (signals error (skyline-tool::compile-art-5200 "/fake.in" "/fake.out")
           "compile-art-5200 should signal not-implemented error")

  ;; Test blob ripping functions (not implemented)
  (signals error (skyline-tool::blob-rip-5200-tile "/fake.png")
           "blob-rip-5200-tile should signal error for missing implementation")

  (signals error (skyline-tool::blob-rip-5200-pmg "/fake.png")
           "blob-rip-5200-pmg should signal error for missing implementation")

  ;; Test detect-5200-tile-mode with invalid input
  (signals error (skyline-tool::detect-5200-tile-mode nil)
           "detect-5200-tile-mode should handle nil input"))

(defun run-5200-tests ()
  "Run all 5200 tests and return results"
  (fiveam:run! '5200-tests))
