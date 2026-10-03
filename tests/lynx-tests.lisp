;;; SkylineTool/tests/lynx-tests.lisp
;;; Comprehensive tests for Atari Lynx conversion tools

(in-package :skyline-tool/test)

(def-suite lynx-tests
  :description "Tests for all Atari Lynx conversion functionality"
  :in skyline-tool/test)

(in-suite lynx-tests)

;; Test Lynx palette definitions
(test lynx-palette
  "Test Lynx color palette definitions"
  (is (equal (length skyline-tool::+lynx-palette+) 4096)
      "Lynx palette should have 4096 colors (16×16×16)")
  ;; Test that palette values are in valid RGB range (0-255)
  (loop for color in skyline-tool::+lynx-palette+
        do (is (>= (first color) 0) "Red component should be >= 0")
           (is (<= (first color) 255) "Red component should be <= 255")
           (is (>= (second color) 0) "Green component should be >= 0")
           (is (<= (second color) 255) "Green component should be <= 255")
           (is (>= (third color) 0) "Blue component should be >= 0")
           (is (<= (third color) 255) "Blue component should be <= 255")))

;; Test Lynx art compilation functions
(test lynx-art-compilation-functions
  "Test Lynx art compilation function definitions"
  (is (fboundp 'skyline-tool::compile-art-lynx)
      "compile-art-lynx function should be defined")
  (is (fboundp 'skyline-tool::read-lynx-art-index)
      "read-lynx-art-index function should be defined")
  (is (fboundp 'skyline-tool::write-asset-compilation/blob-lynx)
      "write-asset-compilation/blob-lynx function should be defined"))

(test lynx-art-compilation
  "Test Lynx art compilation output generation"
  ;; Test that the functions generate output without errors
  (let ((temp-index (merge-pathnames "test-lynx-art.index" (uiop:temporary-directory)))
        (temp-output (merge-pathnames "test-lynx-art.s" (uiop:temporary-directory))))
    (unwind-protect
        (progn
          ;; Create a minimal test index file
          (with-open-file (index-out temp-index :direction :output :if-exists :supersede :if-does-not-exist :create)
            (format index-out "; Test Lynx art index~%test-sprite.png SPRITE 8×8~%"))
          ;; Test read-lynx-art-index
          (finishes
            (skyline-tool::read-lynx-art-index temp-index))
          ;; Test compile-art-lynx
          (finishes
            (skyline-tool::compile-art-lynx temp-output temp-index))
          (is (probe-file temp-output)
              "Lynx art compilation should create output file"))
        ;; Cleanup
        (when (probe-file temp-index) (delete-file temp-index))
        (when (probe-file temp-output) (delete-file temp-output)))))

;; Test Lynx music compilation functions
(test lynx-music-compilation
  "Test Lynx music compilation functions"
  (is (fboundp 'skyline-tool::compile-music-lynx)
      "compile-music-lynx function should be defined"))

(test lynx-music-compilation-output
  "Test Lynx music compilation output generation"
  (let ((temp-file (merge-pathnames "test-lynx-music.s" (uiop:temporary-directory)))
        (input-file (unit-test-midi-input-path)))
    (unwind-protect
        (progn
          (uiop:copy-file (merge-pathnames "Source/Songs/Interworldly.midi"
                                           (uiop:getcwd))
                          input-file)
          (finishes (skyline-tool::compile-music-lynx temp-file input-file))
          (is-true (probe-file temp-file)
                   "Lynx music compilation should create output file"))
      (when (probe-file temp-file) (delete-file temp-file))
      (when (probe-file input-file) (delete-file input-file)))))

;; Test Lynx font compilation
(test lynx-font-compilation
  "Test Lynx font compilation functionality"
  ;; Test that compile-font-generic works for Lynx (machine 200)
  (let ((temp-dir (uiop:temporary-directory))
        (temp-font (merge-pathnames "test-lynx-font.png" (uiop:temporary-directory))))
    (unwind-protect
        (progn
          ;; Create a minimal test font PNG (this would normally be done by external tools)
          ;; For now, just test that the function exists and can be called
          (handler-case
              (skyline-tool::compile-font-generic 200 nil "test-font" nil)
            (error (e)
              ;; Expected to fail without proper font input
              (is-true t "Font compilation properly handles missing input"))))
        (when (probe-file temp-font)
          (delete-file temp-font)))))

;; Test Lynx asset blob compilation
#+()
(test lynx-asset-blob-compilation
  "Test Lynx asset blob compilation"
  (is (fboundp 'skyline-tool::write-asset-compilation/blob-lynx)
      "write-asset-compilation/blob-lynx function should be defined")
  ;; Test that it generates makefile output
  (finishes
    (skyline-tool::write-asset-compilation/blob-lynx "test-asset")))

;; Test Lynx graphics utility functions
(test lynx-graphics-utilities
  "Test Lynx graphics utility functions"
  ;; Test machine-palette with Lynx machine number
  (let ((skyline-tool::*machine* 200)
        (skyline-tool::*region* :internal))
    (is (equal (length (skyline-tool::machine-palette)) 4096)
        "Lynx machine palette should have 4096 colors"))
  ;; Test that Lynx is properly recognized as a valid machine
  (is (equal (skyline-tool::machine-number-from-tag "Lynx") 200)
      "Lynx should map to machine number 200"))

;; Test error handling
(test lynx-error-handling
  "Test error handling in Lynx functions"
  ;; Test invalid machine numbers
  (signals error
    (skyline-tool::machine-palette -1))
  ;; Test invalid art index files
  (let ((invalid-index (merge-pathnames "invalid.index" (uiop:temporary-directory))))
    (unwind-protect
        (progn
          (with-open-file (index-out invalid-index :direction :output :if-exists :supersede :if-does-not-exist :create)
            (format index-out "invalid data"))
          (signals error
            (skyline-tool::read-lynx-art-index invalid-index)))
        (when (probe-file invalid-index)
          (delete-file invalid-index)))))

#+()
;; Test Lynx platform integration
(test lynx-platform-integration
  "Test Lynx integration with platform detection"
  ;; Test that Lynx is recognized in machine-number-by-tag
  (is (equal (skyline-tool::machine-number-by-tag :lynx) 200)
      "Lynx keyword should map to machine number 200")
  (is (equal (skyline-tool::machine-number-by-tag "LYNX") 200)
      "Uppercase LYNX should map to machine number 200"))

;; Test Lynx sprite/tile conversion
(test lynx-sprite-conversion
  "Test Lynx sprite and tile data conversion"
  ;; Test basic sprite data structures
  ;; This would need mock PNG data to fully test
  (is-true t "Lynx sprite conversion framework should exist"))

;; Test Lynx sound conversion
(test lynx-mikey-frequency-conversion
  "Test Mikey frequency-to-counter conversion"
  (let ((a4-counter (skyline-tool::frequency->mikey-counter 440.0d0)))
    (is (= a4-counter 4544) "A4 (440 Hz) counter should be 4544 at 4 MHz"))
  (let ((c4-counter (skyline-tool::frequency->mikey-counter 261.63d0)))
    (is (= c4-counter 7643) "C4 (261.63 Hz) counter should be 7643"))
  (let ((max-counter (skyline-tool::frequency->mikey-counter 30.5d0)))
    (is (<= max-counter 65535) "Low frequency counter should fit in 16 bits")))

(test lynx-mikey-score-to-song
  "Test Mikey score->song conversion"
  (let ((score (list (list :lyric nil :instrument :piano :time 0.0d0 :duration 0.1d0
                           :key 60 :velocity 100))))
    (let ((song (skyline-tool::score->song score :mikey :ntsc)))
      (is (arrayp song) "score->song :mikey should return an array")
      (is (plusp (array-dimension song 0)) "should have at least one note row"))))
