;;; Phantasia SkylineTool/tests/asset-allocator-tests.lisp
;;;; Comprehensive tests for asset management and allocation functions
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

(def-suite asset-allocator-tests
  :description "Comprehensive tests for asset management and allocation functions"
  :in skyline-tool/test)

(in-suite asset-allocator-tests)

;; Test data generators for asset allocator functions
(defun generate-random-asset-name ()
  "Generate a random asset name"
  (let ((prefixes '("sprite" "tile" "music" "script" "map" "blob"))
        (suffixes '("01" "02" "abc" "test" "data")))
    (format nil "~a_~a"
            (nth (random (length prefixes)) prefixes)
            (nth (random (length suffixes)) suffixes))))

(defun generate-random-asset-line ()
  "Generate a random asset line for parsing"
  (let ((types '("SPRITE" "TILE" "MUSIC" "SCRIPT" "MAP" "BLOB"))
        (names (loop for i from 1 to 10 collect (generate-random-asset-name))))
    (format nil "~a ~a"
            (nth (random (length types)) types)
            (nth (random (length names)) names))))

(defun generate-random-machine ()
  "Generate a random valid machine number"
  (nth (random 6) '(8 16 2600 5200 7800 800)))

(defun generate-random-file-sizes (count)
  "Generate random file sizes for allocation testing"
  (loop for i from 1 to count
        collect (1+ (random 32767)))) ; 1-32767 bytes

;; Test parse-assets-line function
(test parse-assets-line-basic
  "Test parse-assets-line with basic asset lines"
  (let ((result (skyline-tool::parse-assets-line "SPRITE player_ship")))
    (is (consp result) "Should return a cons cell")
    (is (string= (car result) "SPRITE") "First element should be asset type")
    (is (string= (cdr result) "player_ship") "Second element should be asset name")))

(define-multi-test parse-assets-line-various-formats
  "Test parse-assets-line with various asset line formats"
  10 ; Test 10 different random formats
  (let* ((line (generate-random-asset-line))
         (result (skyline-tool::parse-assets-line line)))
    (is (consp result) "Should always return a cons cell")
    (is (stringp (car result)) "First element should be a string")
    (is (stringp (cdr result)) "Second element should be a string")))

(test parse-assets-line-edge-cases
  "Test parse-assets-line with edge cases"
  ;; Empty line
  (is (null (skyline-tool::parse-assets-line "")) "Empty line should return nil")
  ;; Line with only type
  (let ((result (skyline-tool::parse-assets-line "SPRITE")))
    (is (equal result '("SPRITE" . "")) "Type-only line should have empty name"))
  ;; Line with extra spaces
  (let ((result (skyline-tool::parse-assets-line "  SPRITE   player_ship  ")))
    (is (equal result '("SPRITE" . "player_ship")) "Extra spaces should be handled")))

;; Test kind-by-name function
(test kind-by-name-basic
  "Test kind-by-name with known asset kinds"
  (is (eql (skyline-tool::kind-by-name "SPRITE") :sprite) "SPRITE should map to :sprite")
  (is (eql (skyline-tool::kind-by-name "TILE") :tile) "TILE should map to :tile")
  (is (eql (skyline-tool::kind-by-name "MUSIC") :music) "MUSIC should map to :music")
  (is (eql (skyline-tool::kind-by-name "SCRIPT") :script) "SCRIPT should map to :script")
  (is (eql (skyline-tool::kind-by-name "MAP") :map) "MAP should map to :map")
  (is (eql (skyline-tool::kind-by-name "BLOB") :blob) "BLOB should map to :blob"))

(test kind-by-name-case-insensitive
  "Test kind-by-name is case insensitive"
  (is (eql (skyline-tool::kind-by-name "sprite") :sprite) "Lowercase should work")
  (is (eql (skyline-tool::kind-by-name "Sprite") :sprite) "Mixed case should work"))

(test kind-by-name-unknown
  "Test kind-by-name with unknown types"
  (is (null (skyline-tool::kind-by-name "UNKNOWN")) "Unknown type should return nil")
  (is (null (skyline-tool::kind-by-name "")) "Empty string should return nil"))

;; Test asset-kind/name function
(test asset-kind-name-basic
  "Test asset-kind/name function"
  (let ((asset '(:sprite . "player_ship")))
    (is (eql (skyline-tool::asset-kind/name asset) :sprite) "Should return asset kind"))
  (let ((asset '(:music . "background")))
    (is (eql (skyline-tool::asset-kind/name asset) :music) "Should return music kind")))

;; Test kind-of-asset function
(test kind-of-asset-basic
  "Test kind-of-asset function"
  (is (eql (skyline-tool::kind-of-asset "SPRITE player_ship") :sprite)
      "Should extract kind from asset line")
  (is (eql (skyline-tool::kind-of-asset "MUSIC background") :music)
      "Should extract music kind from asset line"))

;; Test make-seen-ids-table function
(test make-seen-ids-table-basic
  "Test make-seen-ids-table creates a hash table"
  (let ((table (skyline-tool::make-seen-ids-table)))
    (is (hash-table-p table) "Should return a hash table")
    (is (eql (hash-table-test table) 'equal) "Should use equal test")))

;; Test asset predicate functions
(test asset-predicates-basic
  "Test basic asset predicate functions"
  (is-true (skyline-tool::song-asset-p '(:music . "song1")) "Music asset should be song")
  (is-false (skyline-tool::song-asset-p '(:sprite . "ship")) "Sprite asset should not be song")
  (is-true (skyline-tool::script-asset-p '(:script . "dialog")) "Script asset should be script")
  (is-false (skyline-tool::script-asset-p '(:sprite . "ship")) "Sprite asset should not be script")
  (is-true (skyline-tool::map-asset-p '(:map . "level1")) "Map asset should be map")
  (is-false (skyline-tool::map-asset-p '(:sprite . "ship")) "Sprite asset should not be map")
  (is-true (skyline-tool::blob-asset-p '(:blob . "data")) "Blob asset should be blob")
  (is-false (skyline-tool::blob-asset-p '(:sprite . "ship")) "Sprite asset should not be blob"))

;; Test bank-size function
(test bank-size-basic
  "Test bank-size with simple asset size hash"
  (let ((size-hash (make-hash-table)))
    (setf (gethash 'asset1 size-hash) 1000)
    (setf (gethash 'asset2 size-hash) 2000)
    (is (= (skyline-tool::bank-size size-hash) 3000) "Should sum all asset sizes")))

;; Test supported-video-types function
(test supported-video-types-basic
  "Test supported-video-types returns appropriate types"
  (let ((skyline-tool::*machine* 2600))
    (let ((types (skyline-tool::supported-video-types)))
      (is (listp types) "Should return a list")
      (is (member :ntsc types) "Should include NTSC for 2600"))))

;; Test machine-directory-name function
(test machine-directory-name-basic
  "Test machine-directory-name returns correct directory names"
  (let ((skyline-tool::*machine* 2600))
    (is (string= (skyline-tool::machine-directory-name) "2600") "2600 should map to '2600'"))
  (let ((skyline-tool::*machine* 8))
    (is (string= (skyline-tool::machine-directory-name) "NES") "8 should map to 'NES'")))

;; Test machine-number-by-tag function
(test machine-number-by-tag-basic
  "Test machine-number-by-tag function"
  (is (= (skyline-tool::machine-number-by-tag "2600") 2600) "2600 tag should map to 2600")
  (is (= (skyline-tool::machine-number-by-tag "nes") 8) "nes tag should map to 8")
  (is (= (skyline-tool::machine-number-by-tag "5200") 5200) "5200 tag should map to 5200"))

;; Test extract-palette function
(test extract-palette-existence
  "Test extract-palette function exists and handles basic input"
  (is-true (fboundp 'skyline-tool::extract-palette) "extract-palette should be defined")
  (finishes (skyline-tool::extract-palette "dummy-path") "Should not error on basic call"))

;; Test asset->object-name function
(test asset-object-name-basic
  "Test asset->object-name generates correct object names"
  (let ((result (skyline-tool::asset->object-name "SPRITE ship")))
    (is (stringp result) "Should return a string")
    (is (search "ship" result) "Should contain asset name")))

;; Test asset->symbol-name function
(test asset-symbol-name-basic
  "Test asset->symbol-name generates symbol names"
  (let ((result (skyline-tool::asset->symbol-name "SPRITE ship")))
    (is (symbolp result) "Should return a symbol")
    (is (search "SHIP" (symbol-name result)) "Should contain uppercase asset name")))

;; Test asset->source-name function
(test asset-source-name-basic
  "Test asset->source-name generates source names"
  (let ((result (skyline-tool::asset->source-name "SPRITE ship")))
    (is (stringp result) "Should return a string")))

;; Test speech-supported-p function
(test speech-supported-p-basic
  "Test speech-supported-p for different machines"
  (let ((skyline-tool::*machine* 2600))
    (is-false (skyline-tool::speech-supported-p) "2600 should not support speech"))
  (let ((skyline-tool::*machine* 7800))
    (is-true (skyline-tool::speech-supported-p) "7800 should support speech")))

;; Test current-julian-date function
(test current-julian-date-basic
  "Test current-julian-date returns a valid date"
  (let ((date (skyline-tool::current-julian-date)))
    (is (integerp date) "Should return an integer")
    (is (> date 2024000) "Should return a reasonable Julian date")))

;; Test current-year function
(test current-year-basic
  "Test current-year returns current year"
  (let ((year (skyline-tool::current-year)))
    (is (integerp year) "Should return an integer")
    (is (>= year 2024) "Should return current or future year")))

;; Test last-segment function
(test last-segment-basic
  "Test last-segment extracts last path segment"
  (is (string= (skyline-tool::last-segment "path/to/file.txt" #\/) "file.txt")
      "Should extract filename from path")
  (is (string= (skyline-tool::last-segment "simple" #\/) "simple")
      "Should handle simple string"))

;; Test compress-sequential-numbers function
(test compress-sequential-numbers-basic
  "Test compress-sequential-numbers function"
  (is-true (fboundp 'skyline-tool::compress-sequential-numbers)
           "compress-sequential-numbers should be defined")
  (finishes (skyline-tool::compress-sequential-numbers 1 2 3)
            "Should handle basic sequential numbers"))

;; Test collect-assets function
(test collect-assets-existence
  "Test collect-assets function exists"
  (is-true (fboundp 'skyline-tool::collect-assets) "collect-assets should be defined"))

;; Test all-encoded-asset-names function
(test all-encoded-asset-names-existence
  "Test all-encoded-asset-names function exists"
  (is-true (fboundp 'skyline-tool::all-encoded-asset-names)
           "all-encoded-asset-names should be defined"))

;; Test allocation functions
(test allocation-functions-existence
  "Test that allocation functions exist"
  (dolist (func '(skyline-tool::allocate-assets
                  skyline-tool::find-best-allocation
                  skyline-tool::size-of-banks
                  skyline-tool::try-allocation-sequence))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test file processing functions
(test file-processing-functions-existence
  "Test that file processing functions exist"
  (dolist (func '(skyline-tool::asset-file
                  skyline-tool::existing-object-file
                  skyline-tool::generated-path
                  skyline-tool::include-paths-for-current-bank))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test makefile generation functions
(test makefile-functions-existence
  "Test that makefile generation functions exist"
  (dolist (func '(skyline-tool::write-master-makefile
                  skyline-tool::write-assets-makefile
                  skyline-tool::write-bank-makefile
                  skyline-tool::write-makefile-header))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test asset writing functions
(test asset-writing-functions-existence
  "Test that asset writing functions exist"
  (dolist (func '(skyline-tool::write-asset-ids
                  skyline-tool::write-asset-bank
                  skyline-tool::write-asset-source
                  skyline-tool::write-asset-compilation))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test bank management functions
(test bank-functions-existence
  "Test that bank management functions exist"
  (dolist (func '(skyline-tool::number-of-banks
                  skyline-tool::first-assets-bank
                  skyline-tool::bank-source-pathname
                  skyline-tool::last-bank-source-pathname))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test dependency functions
(test dependency-functions-existence
  "Test that dependency functions exist"
  (dolist (func '(skyline-tool::recursive-read-deps
                  skyline-tool::asset->deps-list
                  skyline-tool::find-included-file
                  skyline-tool::find-included-binary-file))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test label functions
(test label-functions-existence
  "Test that label functions exist"
  (dolist (func '(skyline-tool::labels-to-include
                  skyline-tool::labels-to-forth
                  skyline-tool::atari800-label-file))
    (is-true (fboundp func)
             "~a function should be defined" func)))

(test labels-to-include-emits-all-labels-at-same-address
  "labels-to-include must emit every equate in range; duplicate addresses must not collapse.

Unparseable values (floats) must not be forced to address 0 (which overwrote other labels)."
  (uiop:with-temporary-file (:pathname lab :suffix ".LABELS.txt")
    (with-open-file (s lab :direction :output :if-exists :supersede)
      (format s "DupA = $1000~%DupB = $1000~%SkipFloat = 9.5~%Unique = $2000~%"))
    (let ((skyline-tool::*machine* 7800)
          (out (merge-pathnames
                (make-pathname :name "ZZTestDupLabelsInc"
                               :type "s"
                               :directory `(:relative "Source" "Generated" "7800"))
                (skyline-tool::project-root))))
      (unwind-protect
           (progn
             (skyline-tool::labels-to-include lab "0000" "ffff" "ZZTestDupLabelsInc")
             (is-true (probe-file out) "generated include should exist")
             (let ((txt (uiop:read-file-string out)))
               (is (search "DupA" txt) "DupA missing from include")
               (is (search "DupB" txt) "DupB missing from include (duplicate-address bug)")
               (is (search "Unique" txt) "Unique missing from include")
               (is (null (search "SkipFloat" txt)) "unparseable float must not appear")))
        (when (probe-file out)
          (delete-file out))))))

;; Test compilation functions
(test compilation-functions-existence
  "Test that compilation functions exist"
  (dolist (func '(skyline-tool::write-blob-generation
                  skyline-tool::write-art-generation
                  skyline-tool::write-tsx-generation
                  skyline-tool::assemble-with-64tass))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test utility functions
(test utility-functions-existence
  "Test that utility functions exist"
  (dolist (func '(skyline-tool::check-for-absent-assets
                  skyline-tool::skyline-tool-writes-p
                  skyline-tool::write-source-file
                  skyline-tool::recursive-directory))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test asset collection functions
(test asset-collection-functions-existence
  "Test that asset collection functions exist"
  (dolist (func '(skyline-tool::all-portable-assets
                  skyline-tool::all-bare-assets
                  skyline-tool::all-assets-for-build
                  skyline-tool::filter-assets-for-build))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test asset compilation functions
(test asset-compilation-functions-existence
  "Test that asset compilation functions exist"
  (dolist (func '(skyline-tool::asset-compilation-line
                  skyline-tool::write-asset-compilation/music
                  skyline-tool::write-asset-compilation/blob-lynx
                  skyline-tool::compile-enemy-stats))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test name generation functions
(test name-generation-functions-existence
  "Test that name generation functions exist"
  (dolist (func '(skyline-tool::allocation-list-name
                  skyline-tool::allocation-size-name
                  skyline-tool::make-source-file-name))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test included file functions
(test included-file-functions-existence
  "Test that included file functions exist"
  (dolist (func '(skyline-tool::included-file
                  skyline-tool::included-binary-file
                  skyline-tool::makefile-contains-target-p))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test asset loader functions
(test asset-loader-functions-existence
  "Test that asset loader functions exist"
  (dolist (func '(skyline-tool::asset-loaders
                  skyline-tool::read-assets-list
                  skyline-tool::interpret-line-from-assets-list))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test size calculation functions
(test size-functions-existence
  "Test that size calculation functions exist"
  (dolist (func '(skyline-tool::compute-asset-size
                  skyline-tool::assemble-file-for-size
                  skyline-tool::write-assembly-skeleton-for-size))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test RAM bank functions
(test ram-bank-functions-existence
  "Test that RAM bank functions exist"
  (dolist (func '(skyline-tool::write-ram-bank-makefile
                  skyline-tool::write-makefile-test-banks
                  skyline-tool::write-test-header-script))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test header script functions
(test header-script-functions-existence
  "Test that header script functions exist"
  (dolist (func '(skyline-tool::write-header-script
                  skyline-tool::write-makefile-test-target
                  skyline-tool::write-makefile-top-line))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test makefile section functions
(test makefile-section-functions-existence
  "Test that makefile section functions exist"
  (dolist (func '(skyline-tool::write-makefile-for-blobs
                  skyline-tool::write-makefile-for-art
                  skyline-tool::write-makefile-for-tilesets
                  skyline-tool::write-makefile-for-bare-assets))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test asset bank makefile functions
(test asset-bank-makefile-functions-existence
  "Test that asset bank makefile functions exist"
  (dolist (func '(skyline-tool::write-asset-bank-makefile
                  skyline-tool::write-bank-makefile))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test asset source functions
(test asset-source-functions-existence
  "Test that asset source functions exist"
  (dolist (func '(skyline-tool::write-asset-source/blob
                  skyline-tool::write-asset-source/script))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test permutation function
(test best-permutation-existence
  "Test best-permutation function exists"
  (is-true (fboundp 'skyline-tool::best-permutation)
           "best-permutation should be defined"))