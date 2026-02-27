;;; Phantasia SkylineTool/tests/maps-tests.lisp
;;;; Comprehensive tests for maps.lisp level/map system
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

(def-suite maps-tests
  :description "Comprehensive tests for maps level/map system"
  :in skyline-tool/test)

(in-suite maps-tests)

;; Test data generators for maps functions
(defun generate-random-color-list (&optional (count 8))
  "Generate a list of random RGB colors"
  (loop for i from 1 to count
        collect (list (random 256) (random 256) (random 256))))

(defun generate-random-palette (&optional (size 4))
  "Generate a random palette for testing"
  (make-array size :initial-contents (generate-random-color-list size)))

(defun generate-random-tile (&optional (width 8) (height 8))
  "Generate a random tile array"
  (let ((tile (make-array (list width height) :element-type '(unsigned-byte 8))))
    (dotimes (x width)
      (dotimes (y height)
        (setf (aref tile x y) (random 16)))) ; 16 colors max
    tile))

(defun generate-random-grid (&optional (width 4) (height 8))
  "Generate a random grid for TIA testing"
  (let ((grid (make-array (list width height) :element-type '(unsigned-byte 8))))
    (dotimes (x width)
      (dotimes (y height)
        (setf (aref grid x y) (random 256))))
    grid))

(defun generate-random-plist (&optional (size 6))
  "Generate a random property list"
  (let ((plist nil))
    (dotimes (i size)
      (push (intern (format nil "KEY~d" i)) plist)
      (push (random 100) plist))
    plist))

;; Test utility functions
(test assocdr-basic
  "Test assocdr function with basic alists"
  (let ((alist '((:key1 . "value1") (:key2 . "value2") (:key3 . 42))))
    (is (string= (skyline-tool::assocdr :key1 alist) "value1") "Should find string value")
    (is (= (skyline-tool::assocdr :key3 alist) 42) "Should find numeric value")
    (is (null (skyline-tool::assocdr :missing alist)) "Should return nil for missing key")))

(test assocdr-no-error
  "Test assocdr returns nil for missing key (serapeum behavior)"
  (let ((alist '((:key1 . "value1"))))
    (is (null (skyline-tool::assocdr :missing alist)) "Should return nil when key is missing")))

(test pin-basic
  "Test pin function clamps values to range"
  (is (= (skyline-tool::pin 5 0 10) 5) "Value in range should be unchanged")
  (is (= (skyline-tool::pin -5 0 10) 0) "Value below min should be clamped to min")
  (is (= (skyline-tool::pin 15 0 10) 10) "Value above max should be clamped to max"))

(test plist-keys-basic
  "Test plist-keys extracts keys from property list"
  (let ((plist '(:key1 "value1" :key2 42 :key3 "value3")))
    (let ((keys (skyline-tool::plist-keys plist)))
      (is (= (length keys) 3) "Should extract 3 keys")
      (is (member :key1 keys) "Should contain key1")
      (is (member :key2 keys) "Should contain key2")
      (is (member :key3 keys) "Should contain key3"))))

(test plist-values-basic
  "Test plist-values extracts values from property list"
  (let ((plist '(:key1 "value1" :key2 42 :key3 "value3")))
    (let ((values (skyline-tool::plist-values plist)))
      (is (= (length values) 3) "Should extract 3 values")
      (is (member "value1" values :test #'string=) "Should contain value1")
      (is (member 42 values) "Should contain 42")
      (is (member "value3" values :test #'string=) "Should contain value3"))))

;; Test grid class and methods
(test grid-tia-creation
  "Test grid/tia class creation"
  (let ((grid (make-instance 'skyline-tool::grid/tia
                            :tiles (generate-random-grid)
                            :colors (generate-random-color-list 8)
                            :background-color '(0 0 0))))
    (is (typep grid 'skyline-tool::grid/tia) "Should create grid/tia instance")
    (is (arrayp (skyline-tool::grid-tiles grid)) "Should have tiles array")
    (is (= (length (skyline-tool::grid-row-colors grid)) 8) "Should have 8 row colors")))

(test list-grid-row-colors-method
  "Test list-grid-row-colors method"
  (let ((colors (generate-random-color-list 8))
        (grid (make-instance 'skyline-tool::grid/tia
                            :tiles (generate-random-grid)
                            :colors colors
                            :background-color '(0 0 0))))
    (let ((result (skyline-tool::list-grid-row-colors grid)))
      (is (listp result) "Should return a list")
      (is (= (length result) 8) "Should have 8 colors")
      (is (equal result colors) "Should return original colors"))))

(test list-grid-row-palette-colors
  "Test list-grid-row-palette-colors function"
  (let ((colors '((255 0 0) (0 255 0) (0 0 255)))
        (grid (make-instance 'skyline-tool::grid/tia
                            :tiles (generate-random-grid)
                            :colors colors
                            :background-color '(0 0 0))))
    (let ((result (skyline-tool::list-grid-row-palette-colors grid)))
      (is (listp result) "Should return a list")
      (is (= (length result) 3) "Should have 3 palette indices"))))

(test list-grid-tiles-method
  "Test list-grid-tiles method"
  (let ((tiles (make-array '(4 8) :element-type '(unsigned-byte 8) :initial-element 0))
        (grid (make-instance 'skyline-tool::grid/tia
                            :tiles tiles
                            :colors (generate-random-color-list 8)
                            :background-color '(0 0 0))))
    ;; Set some test values
    (setf (aref tiles 0 0) 1)
    (setf (aref tiles 1 1) 2)
    (let ((result (skyline-tool::list-grid-tiles grid)))
      (is (listp result) "Should return a list")
      (is (= (length result) 32) "Should have 4*8 = 32 tiles")
      (is (= (nth 0 result) 1) "Should contain tile values in correct order")
      (is (= (nth 5 result) 2) "Should contain tile values in correct order"))))

;; Test data conversion functions
(test 32-bit-word-basic
  "Test 32-bit-word function"
  (is (= (skyline-tool::32-bit-word 1 2 3 4) #x01020304) "Should combine bytes to 32-bit word"))

(test bytes-to-32-bits-basic
  "Test bytes-to-32-bits function"
  (let ((result (skyline-tool::bytes-to-32-bits #(1 2 3 4 5 6 7 8))))
    (is (arrayp result) "Should return array")
    (is (= (length result) 2) "Should have 2 32-bit words")
    (is (= (aref result 0) #x01020304) "First word should be correct")
    (is (= (aref result 1) #x05060708) "Second word should be correct")))

(test split-grid-to-rows-basic
  "Test split-grid-to-rows function"
  (let ((words #(1 2 3 4)))
    (let ((result (skyline-tool::split-grid-to-rows 2 2 words)))
      (is (arrayp result) "Should return 2D array")
      (is (= (array-dimension result 0) 2) "Should have correct height")
      (is (= (array-dimension result 1) 2) "Should have correct width"))))

;; Test tile and attribute functions
(test object-covers-tile-p-basic
  "Test object-covers-tile-p function"
  (is-true (fboundp 'skyline-tool::object-covers-tile-p) "object-covers-tile-p should be defined")
  (finishes (skyline-tool::object-covers-tile-p 0 0 '(:x 0 :y 0 :width 2 :height 2)) "Should handle basic object"))

(test find-effective-attributes-existence
  "Test find-effective-attributes function exists"
  (is-true (fboundp 'skyline-tool::find-effective-attributes) "find-effective-attributes should be defined"))

(test mark-palette-transitions-existence
  "Test mark-palette-transitions function exists"
  (is-true (fboundp 'skyline-tool::mark-palette-transitions) "mark-palette-transitions should be defined"))

;; Test binary data functions
(test decal-properties-binary-existence
  "Test decal-properties->binary function exists"
  (is-true (fboundp 'skyline-tool::decal-properties->binary) "decal-properties->binary should be defined"))

(test logior-numbers-basic
  "Test logior-numbers function"
  (is (= (skyline-tool::logior-numbers 1 2 4) 7) "Should OR all numbers together")
  (is (= (skyline-tool::logior-numbers) 0) "Should return 0 for no arguments"))

;; Test collection functions
(test collect-decal-object-existence
  "Test collect-decal-object function exists"
  (is-true (fboundp 'skyline-tool::collect-decal-object) "collect-decal-object should be defined"))

(test collect-invisible-decals-existence
  "Test collect-invisible-decals-for-tile function exists"
  (is-true (fboundp 'skyline-tool::collect-invisible-decals-for-tile) "collect-invisible-decals-for-tile should be defined"))

;; Test parsing functions
(test parse-tile-grid-existence
  "Test parse-tile-grid function exists"
  (is-true (fboundp 'skyline-tool::parse-tile-grid) "parse-tile-grid should be defined"))

(test map-layer-depth-existence
  "Test map-layer-depth function exists"
  (is-true (fboundp 'skyline-tool::map-layer-depth) "map-layer-depth should be defined"))

;; Test image processing functions
(test load-tileset-image-existence
  "Test load-tileset-image function exists"
  (is-true (fboundp 'skyline-tool::load-tileset-image) "load-tileset-image should be defined"))

(test extract-8x16-tiles-existence
  "Test extract-8×16-tiles function exists"
  (is-true (fboundp 'skyline-tool::extract-8×16-tiles) "extract-8×16-tiles should be defined"))

(test extract-palettes-basic
  "Test extract-palettes function"
  (is-true (fboundp 'skyline-tool::extract-palettes) "extract-palettes should be defined")
  (finishes (skyline-tool::extract-palettes (generate-random-tile) :count 4) "Should handle basic tile"))

;; Test palette and color functions
(test all-colors-in-tile-basic
  "Test all-colors-in-tile function"
  (let ((tile (generate-random-tile)))
    (let ((colors (skyline-tool::all-colors-in-tile tile)))
      (is (listp colors) "Should return list of colors")
      (is (<= (length colors) 16) "Should not have more than 16 colors"))))

(test tile-fits-palette-p-basic
  "Test tile-fits-palette-p function"
  (let ((tile (generate-random-tile))
        (palette (generate-random-palette 4)))
    (let ((result (skyline-tool::tile-fits-palette-p tile palette)))
      (is (or (null result) (listp result)) "Should return nil or palette mapping"))))

(define-multi-test tile-fits-palette-various
  "Test tile-fits-palette-p with various tiles and palettes"
  8 ; Test 8 combinations
  (let ((tile (generate-random-tile))
        (palette (generate-random-palette (1+ (random 8)))))
    (finishes (skyline-tool::tile-fits-palette-p tile palette) "Should handle various combinations")))

;; Test array conversion functions
(test 2a-to-list-basic
  "Test 2a-to-list function"
  (let ((array (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6)))))
    (let ((result (skyline-tool::2a-to-list array)))
      (is (listp result) "Should return list")
      (is (= (length result) 2) "Should have 2 rows"))))

(test 2a-to-lol-basic
  "Test 2a-to-lol function"
  (let ((array (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6)))))
    (let ((result (skyline-tool::2a-to-lol array)))
      (is (listp result) "Should return list of lists")
      (is (= (length result) 2) "Should have 2 sublists")
      (is (= (length (first result)) 3) "Each sublist should have 3 elements"))))

;; Test color analysis functions
(test region-list-of-colors-existence
  "Test region->list-of-colors function exists"
  (is-true (fboundp 'skyline-tool::region->list-of-colors) "region->list-of-colors should be defined"))

(test color-distance-by-indices-existence
  "Test color-distance-by-indices function exists"
  (is-true (fboundp 'skyline-tool::color-distance-by-indices) "color-distance-by-indices should be defined"))

;; Test palette optimization functions
(test best-palette-existence
  "Test best-palette function exists"
  (is-true (fboundp 'skyline-tool::best-palette) "best-palette should be defined"))

(test split-images-to-palettes-existence
  "Test split-images-to-palettes function exists"
  (is-true (fboundp 'skyline-tool::split-images-to-palettes) "split-images-to-palettes should be defined"))

;; Test tile property functions
(test tile-property-value-existence
  "Test tile-property-value function exists"
  (is-true (fboundp 'skyline-tool::tile-property-value) "tile-property-value should be defined"))

(test tile-collision-p-existence
  "Test tile-collision-p function exists"
  (is-true (fboundp 'skyline-tool::tile-collision-p) "tile-collision-p should be defined"))

;; Test locale and map functions
(test locale-pathname-existence
  "Test locale-pathname function exists"
  (is-true (fboundp 'skyline-tool::locale-pathname) "locale-pathname should be defined"))

(test load-other-map-existence
  "Test load-other-map function exists"
  (is-true (fboundp 'skyline-tool::load-other-map) "load-other-map should be defined"))

(test locale-xml-existence
  "Test locale-xml function exists"
  (is-true (fboundp 'skyline-tool::locale-xml) "locale-xml should be defined"))

;; Test map ID functions
(test read-map-ids-table-existence
  "Test read-map-ids-table function exists"
  (is-true (fboundp 'skyline-tool::read-map-ids-table) "read-map-ids-table should be defined"))

(test find-locale-id-from-xml-existence
  "Test find-locale-id-from-xml function exists"
  (is-true (fboundp 'skyline-tool::find-locale-id-from-xml) "find-locale-id-from-xml should be defined"))

;; Test enemy functions
(test load-enemies-index-existence
  "Test load-enemies-index function exists"
  (is-true (fboundp 'skyline-tool::load-enemies-index) "load-enemies-index should be defined"))

(test find-enemy-id-existence
  "Test find-enemy-id function exists"
  (is-true (fboundp 'skyline-tool::find-enemy-id) "find-enemy-id should be defined"))

;; Test utility functions
(test ensure-number-basic
  "Test ensure-number function"
  (is (= (skyline-tool::ensure-number 42) 42) "Number should pass through")
  (is (= (skyline-tool::ensure-number "42") 42) "String number should convert")
  (is (= (skyline-tool::ensure-number nil) 0) "Nil should become 0"))

;; Test attribute functions
(test add-attribute-values-existence
  "Test add-attribute-values function exists"
  (is-true (fboundp 'skyline-tool::add-attribute-values) "add-attribute-values should be defined"))

(test parse-tile-attributes-existence
  "Test parse-tile-attributes function exists"
  (is-true (fboundp 'skyline-tool::parse-tile-attributes) "parse-tile-attributes should be defined"))

(test tile-effective-palette-existence
  "Test tile-effective-palette function exists"
  (is-true (fboundp 'skyline-tool::tile-effective-palette) "tile-effective-palette should be defined"))

;; Test tileset functions
(test load-tileset-existence
  "Test load-tileset function exists"
  (is-true (fboundp 'skyline-tool::load-tileset) "load-tileset should be defined"))

;; Test RLE compression functions
(test make-byte-array-with-fill-pointer-existence
  "Test make-byte-array-with-fill-pointer function exists"
  (is-true (fboundp 'skyline-tool::make-byte-array-with-fill-pointer) "make-byte-array-with-fill-pointer should be defined"))

(test rle-encode-basic
  "Test rle-encode function"
  (let ((result (skyline-tool::rle-encode 1 2 3)))
    (is (listp result) "Should return a list")
    (is (>= (length result) 2) "Should have at least 2 elements")))

(test rle-expanded-string-existence
  "Test rle-expanded-string function exists"
  (is-true (fboundp 'skyline-tool::rle-expanded-string) "rle-expanded-string should be defined"))

(test rle-compress-segment-existence
  "Test rle-compress-segment function exists"
  (is-true (fboundp 'skyline-tool::rle-compress-segment) "rle-compress-segment should be defined"))

(test rle-compress-existence
  "Test rle-compress function exists"
  (is-true (fboundp 'skyline-tool::rle-compress) "rle-compress should be defined"))

;; Test utility comparison functions
(test shorter-basic
  "Test shorter function"
  (is (eq (skyline-tool::shorter "abc" "de") "de") "Should return shorter string")
  (is (eq (skyline-tool::shorter "ab" "cd") "ab") "Should return first when equal length"))

(test only-best-options-existence
  "Test only-best-options function exists"
  (is-true (fboundp 'skyline-tool::only-best-options) "only-best-options should be defined"))

;; Test hex dump functions
(test hex-dump-comment-existence
  "Test hex-dump-comment function exists"
  (is-true (fboundp 'skyline-tool::hex-dump-comment) "hex-dump-comment should be defined"))

(test hex-dump-bytes-existence
  "Test hex-dump-bytes function exists"
  (is-true (fboundp 'skyline-tool::hex-dump-bytes) "hex-dump-bytes should be defined"))

;; Test XML functions
(test xml-match-existence
  "Test xml-match function exists"
  (is-true (fboundp 'skyline-tool::xml-match) "xml-match should be defined"))

(test xml-matches-existence
  "Test xml-matches function exists"
  (is-true (fboundp 'skyline-tool::xml-matches) "xml-matches should be defined"))

;; Test binary I/O functions
(test write-word-existence
  "Test write-word function exists"
  (is-true (fboundp 'skyline-tool::write-word) "write-word should be defined"))

(test write-dword-existence
  "Test write-dword function exists"
  (is-true (fboundp 'skyline-tool::write-dword) "write-dword should be defined"))

(test write-bytes-existence
  "Test write-bytes function exists"
  (is-true (fboundp 'skyline-tool::write-bytes) "write-bytes should be defined"))

;; Test minifont functions
(test char-minifont-existence
  "Test char->minifont function exists"
  (is-true (fboundp 'skyline-tool::char->minifont) "char->minifont should be defined"))

(test minifont-char-existence
  "Test minifont->char function exists"
  (is-true (fboundp 'skyline-tool::minifont->char) "minifont->char should be defined"))

(test unicode-minifont-existence
  "Test unicode->minifont function exists"
  (is-true (fboundp 'skyline-tool::unicode->minifont) "unicode->minifont should be defined"))

(test minifont-unicode-existence
  "Test minifont->unicode function exists"
  (is-true (fboundp 'skyline-tool::minifont->unicode) "minifont->unicode should be defined"))

;; Test decal functions
(test decal-invisible-p-existence
  "Test decal-invisible-p function exists"
  (is-true (fboundp 'skyline-tool::decal-invisible-p) "decal-invisible-p should be defined"))

;; Test assembly functions
(test assemble-binary-existence
  "Test assemble-binary function exists"
  (is-true (fboundp 'skyline-tool::assemble-binary) "assemble-binary should be defined"))

(test run-commands-content-existence
  "Test run-commands-content-for-map function exists"
  (is-true (fboundp 'skyline-tool::run-commands-content-for-map) "run-commands-content-for-map should be defined"))

;; Test tileset functions
(test tileset-rom-bank-existence
  "Test tileset-rom-bank function exists"
  (is-true (fboundp 'skyline-tool::tileset-rom-bank) "tileset-rom-bank should be defined"))

(test write-binary-animations-existence
  "Test write-binary-animations-list function exists"
  (is-true (fboundp 'skyline-tool::write-binary-animations-list) "write-binary-animations-list should be defined"))

(test map-data-vector-existence
  "Test map-data-vector function exists"
  (is-true (fboundp 'skyline-tool::map-data-vector) "map-data-vector should be defined"))

;; Test compilation functions
(test compile-map-existence
  "Test compile-map function exists"
  (is-true (fboundp 'skyline-tool::compile-map) "compile-map should be defined"))

;; Test tileset processing functions
(test rip-tiles-from-tileset-existence
  "Test rip-tiles-from-tileset function exists"
  (is-true (fboundp 'skyline-tool::rip-tiles-from-tileset) "rip-tiles-from-tileset should be defined"))

(test palette-index-existence
  "Test palette-index function exists"
  (is-true (fboundp 'skyline-tool::palette-index) "palette-index should be defined"))

(test rip-bytes-from-image-existence
  "Test rip-bytes-from-image function exists"
  (is-true (fboundp 'skyline-tool::rip-bytes-from-image) "rip-bytes-from-image should be defined"))

(test compile-tileset-existence
  "Test compile-tileset function exists"
  (is-true (fboundp 'skyline-tool::compile-tileset) "compile-tileset should be defined"))

;; Test color adjustment functions
(test ensure-byte-basic
  "Test ensure-byte function"
  (is (= (skyline-tool::ensure-byte 255) 255) "Valid byte should pass through")
  (is (= (skyline-tool::ensure-byte 300) 44) "Large number should wrap to byte") ; 300 mod 256 = 44
  (is (= (skyline-tool::ensure-byte -5) 251) "Negative should wrap to positive byte"))

(test color-adjustment-functions-existence
  "Test color adjustment functions exist"
  (dolist (func '(skyline-tool::darken-color-in-palette
                  skyline-tool::lighten-color-in-palette
                  skyline-tool::redden-color-in-palette
                  skyline-tool::cyanate-color-in-palette))
    (is-true (fboundp func) "~a should be defined" func)))

(test adjust-palettes-existence
  "Test adjust-palettes function exists"
  (is-true (fboundp 'skyline-tool::adjust-palettes) "adjust-palettes should be defined"))

(test extract-tileset-palette-existence
  "Test extract-tileset-palette function exists"
  (is-true (fboundp 'skyline-tool::extract-tileset-palette) "extract-tileset-palette should be defined"))

;; Test scene object functions
(test find-named-object-in-scene-existence
  "Test find-named-object-in-scene function exists"
  (is-true (fboundp 'skyline-tool::find-named-object-in-scene) "find-named-object-in-scene should be defined"))

;; Test parsing functions
(test parse-tile-animation-set-existence
  "Test parse-tile-animation-set function exists"
  (is-true (fboundp 'skyline-tool::parse-tile-animation-set) "parse-tile-animation-set should be defined"))

(test split-into-bytes-existence
  "Test split-into-bytes function exists"
  (is-true (fboundp 'skyline-tool::split-into-bytes) "split-into-bytes should be defined"))

(test parse-layer-existence
  "Test parse-layer function exists"
  (is-true (fboundp 'skyline-tool::parse-layer) "parse-layer should be defined"))

(test find-tile-by-number-existence
  "Test find-tile-by-number function exists"
  (is-true (fboundp 'skyline-tool::find-tile-by-number) "find-tile-by-number should be defined"))

(test assign-attributes-existence
  "Test assign-attributes function exists"
  (is-true (fboundp 'skyline-tool::assign-attributes) "assign-attributes should be defined"))

(test add-alt-tile-attributes-existence
  "Test add-alt-tile-attributes function exists"
  (is-true (fboundp 'skyline-tool::add-alt-tile-attributes) "add-alt-tile-attributes should be defined"))

(test properties-plist-existence
  "Test properties->plist function exists"
  (is-true (fboundp 'skyline-tool::properties->plist) "properties->plist should be defined"))

(test ensure-minifont-existence
  "Test ensure-minifont function exists"
  (is-true (fboundp 'skyline-tool::ensure-minifont) "ensure-minifont should be defined"))

;; Test exit/entrance functions
(test find-entrance-by-name-existence
  "Test find-entrance-by-name function exists"
  (is-true (fboundp 'skyline-tool::find-entrance-by-name) "find-entrance-by-name should be defined"))

(test assign-exit-existence
  "Test assign-exit function exists"
  (is-true (fboundp 'skyline-tool::assign-exit) "assign-exit should be defined"))

;; Test palette conversion functions
(test palette-to-ansi-pairs-existence
  "Test palette-to-ansi-pairs function exists"
  (is-true (fboundp 'skyline-tool::palette-to-ansi-pairs) "palette-to-ansi-pairs should be defined"))