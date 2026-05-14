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
  "Generate a random asset name component"
  (let ((prefixes '("Hero" "Villain" "Town" "Dungeon" "Forest"))
        (suffixes '("Theme" "Main" "Boss" "Ambient" "Combat")))
    (format nil "~a~a"
            (nth (random (length prefixes)) prefixes)
            (nth (random (length suffixes)) suffixes))))

(defun generate-random-asset-indicator ()
  "Generate a random valid asset indicator string (Kind/Name form)"
  (let ((kinds '("Songs" "Maps" "Scripts" "Blobs")))
    (format nil "~a/~a"
            (nth (random (length kinds)) kinds)
            (generate-random-asset-name))))

(defun generate-random-machine ()
  "Generate a random valid machine number"
  (nth (random 6) '(8 16 2600 5200 7800 800)))

(defun generate-random-file-sizes (count)
  "Generate random file sizes for allocation testing"
  (loop for i from 1 to count
        collect (1+ (random 32767)))) ; 1-32767 bytes

;; Test parse-assets-line function
;;
;; PARSE-ASSETS-LINE returns (LIST asset-string builds-list).
;; The asset-string is the first whitespace-delimited token; the second
;; token (if present) encodes which builds include this asset
;; (A=AtariAge, P=Public, D=Demo).

(test parse-assets-line-basic
  "PARSE-ASSETS-LINE with a bare asset indicator returns a two-element list
where the first element is the asset string and the second is the build list."
  (let ((result (skyline-tool::parse-assets-line "Songs/Title")))
    (is (listp result) "Should return a list")
    (is (string= (first result) "Songs/Title") "First element should be asset indicator")
    (is (listp (second result)) "Second element should be the builds list")
    (is (member "Public" (second result) :test #'string=)
        "Bare line should default to all builds including Public")))

(define-multi-test parse-assets-line-various-formats
  "PARSE-ASSETS-LINE with randomly-generated valid asset indicator lines
always returns a two-element list."
  10
  (let* ((line (generate-random-asset-indicator))
         (result (skyline-tool::parse-assets-line line)))
    (is (listp result) "Should always return a list")
    (is (stringp (first result)) "First element should be a string")
    (is (listp (second result)) "Second element should be a list")))

(test parse-assets-line-edge-cases
  "PARSE-ASSETS-LINE: empty/comment lines return (NIL NIL); a line with
explicit build codes restricts the builds list accordingly."
  ;; Empty line
  (is (equal (skyline-tool::parse-assets-line "") '(nil nil))
      "Empty line should return (nil nil)")
  ;; Comment line
  (is (equal (skyline-tool::parse-assets-line "; comment") '(nil nil))
      "Comment line should return (nil nil)")
  ;; Explicit Public-only build code
  (let ((result (skyline-tool::parse-assets-line "Songs/Title P")))
    (is (listp result) "Should return a list")
    (is (member "Public" (second result) :test #'string=)
        "P code should include Public build")
    (is (not (member "AA" (second result) :test #'string=))
        "P code should not include AA build")))

;; Test kind-by-name function
;;
;; KIND-BY-NAME recognizes the canonical kind strings used in asset
;; identifier paths: "Songs"/"Song", "Art", "Blobs"/"Blob",
;; "Maps"/"Map", "Scripts"/"Script".  It returns the corresponding
;; keyword and signals an error for unrecognized kinds.

(test kind-by-name-basic
  "KIND-BY-NAME maps the canonical kind strings to keywords."
  (is (eql (skyline-tool::kind-by-name "Songs") :song) "Songs → :song")
  (is (eql (skyline-tool::kind-by-name "Song") :song) "Song → :song")
  (is (eql (skyline-tool::kind-by-name "Art") :art) "Art → :art")
  (is (eql (skyline-tool::kind-by-name "Blobs") :blob) "Blobs → :blob")
  (is (eql (skyline-tool::kind-by-name "Blob") :blob) "Blob → :blob")
  (is (eql (skyline-tool::kind-by-name "Maps") :map) "Maps → :map")
  (is (eql (skyline-tool::kind-by-name "Map") :map) "Map → :map")
  (is (eql (skyline-tool::kind-by-name "Scripts") :script) "Scripts → :script")
  (is (eql (skyline-tool::kind-by-name "Script") :script) "Script → :script"))

(test kind-by-name-nil-and-empty
  "KIND-BY-NAME returns NIL for NIL or empty-string input."
  (is (null (skyline-tool::kind-by-name nil)) "nil input should return nil")
  (is (null (skyline-tool::kind-by-name "")) "empty string should return nil"))

(test kind-by-name-unknown-signals-error
  "KIND-BY-NAME signals an error for unrecognized asset kinds — it does
NOT return NIL, because an unknown kind always indicates a programming
error."
  (signals error (skyline-tool::kind-by-name "SPRITE"))
  (signals error (skyline-tool::kind-by-name "Unknown")))

;; Test asset-kind/name function
;;
;; ASSET-KIND/NAME takes a string in "Kind/Name" form and returns
;; (list kind-string name-string).

(test asset-kind-name-basic
  "ASSET-KIND/NAME splits an asset indicator into (kind name) list."
  (let ((skyline-tool::*machine* 7800))
    (let ((result (skyline-tool::asset-kind/name "Songs/Title")))
      (is (listp result) "Should return a list")
      (is (string= (first result) "Songs") "First element should be kind string 'Songs'")
      (is (string= (second result) "Title") "Second element should be name 'Title'"))
    (let ((result (skyline-tool::asset-kind/name "Maps/Solace/Dungeon")))
      (is (string= (first result) "Maps") "Maps indicator kind should be 'Maps'")
      (is (search "Solace" (second result)) "Map name should include path components"))))

;; Test kind-of-asset function
(test kind-of-asset-basic
  "KIND-OF-ASSET returns the keyword for the kind component of an asset indicator."
  (let ((skyline-tool::*machine* 7800))
    (is (eql (skyline-tool::kind-of-asset "Songs/Title") :song)
        "Songs/Title should yield :song")
    (is (eql (skyline-tool::kind-of-asset "Maps/Solace/Dungeon") :map)
        "Maps/… should yield :map")
    (is (eql (skyline-tool::kind-of-asset "Scripts/Intro") :script)
        "Scripts/… should yield :script")
    (is (eql (skyline-tool::kind-of-asset "Blobs/TitleCard") :blob)
        "Blobs/… should yield :blob")))

;; Test make-seen-ids-table function
;;
;; MAKE-SEEN-IDS-TABLE returns an outer hash-table keyed by asset-kind
;; keywords (:song, :map, :script, :blob), each mapping to an inner
;; hash-table.

(test make-seen-ids-table-basic
  "MAKE-SEEN-IDS-TABLE returns an EQL hash table with inner tables for
each asset kind keyword."
  (let ((table (skyline-tool::make-seen-ids-table)))
    (is (hash-table-p table) "Should return a hash table")
    (dolist (kind '(:song :map :script :blob))
      (is (hash-table-p (gethash kind table))
          "Inner table for ~a should be a hash table" kind))))

;; Test asset predicate functions
;;
;; The predicates (SONG-ASSET-P, SCRIPT-ASSET-P, MAP-ASSET-P,
;; BLOB-ASSET-P) all take a string asset indicator in "Kind/Name" form.

(test asset-predicates-basic
  "SONG-ASSET-P, SCRIPT-ASSET-P, MAP-ASSET-P and BLOB-ASSET-P each
accept asset indicator strings and return T/NIL according to kind."
  (let ((skyline-tool::*machine* 7800))
    (is-true  (skyline-tool::song-asset-p "Songs/Title")
              "Songs/… should be a song")
    (is-false (skyline-tool::song-asset-p "Maps/Dungeon")
              "Maps/… should not be a song")
    (is-true  (skyline-tool::script-asset-p "Scripts/Dialogue")
              "Scripts/… should be a script")
    (is-false (skyline-tool::script-asset-p "Songs/Title")
              "Songs/… should not be a script")
    (is-true  (skyline-tool::map-asset-p "Maps/Solace/Dungeon")
              "Maps/… should be a map")
    (is-false (skyline-tool::map-asset-p "Scripts/Intro")
              "Scripts/… should not be a map")
    (is-true  (skyline-tool::blob-asset-p "Blobs/TitleCard")
              "Blobs/… should be a blob")
    (is-false (skyline-tool::blob-asset-p "Maps/Dungeon")
              "Maps/… should not be a blob")))

;; Test bank-size function
;;
;; BANK-SIZE takes a hash-table whose KEYS are asset indicator strings
;; and VALUES are integer byte sizes.  It adds the asset-loader overhead
;; on top of the sum, so the result is always > the raw sum.

(test bank-size-basic
  "BANK-SIZE with a two-song hash returns more than the sum of the raw
sizes (due to loader overhead)."
  (let ((skyline-tool::*machine* 7800)
        (size-hash (make-hash-table :test 'equal)))
    (setf (gethash "Songs/Theme" size-hash) 1000)
    (setf (gethash "Songs/Battle" size-hash) 2000)
    (let ((result (skyline-tool::bank-size size-hash)))
      (is (integerp result) "Should return an integer")
      (is (> result 3000)
          "Should exceed raw sum (3000) due to loader overhead; got ~a" result))))

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
  (is (= (skyline-tool::machine-number-by-tag "5200") 5200) "5200 tag should map to 5200")
  (is (= (skyline-tool::machine-number-by-tag "VCS800") 7850) "VCS800 tag should map to 7850"))

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

;;; ---------------------------------------------------------------------------
;;; ASSET->OBJECT-NAME — must not signal ECASE failure for any registered port.
;;;
;;; Regression target: every machine in MACHINE-DIRECTORY-NAME must be
;;; accepted by ASSET->OBJECT-NAME without an ECASE/SIMPLE-ERROR.
;;; ---------------------------------------------------------------------------

(defmacro with-machine (machine &body body)
  "Evaluate BODY with *MACHINE* bound to MACHINE and *REGION* bound to :NTSC."
  `(let ((skyline-tool::*machine* ,machine)
         (skyline-tool::*region* :ntsc))
     ,@body))

(defun %check-asset->object-name (machine label asset-indicator)
  "Assert ASSET->OBJECT-NAME returns a non-empty string for MACHINE (identified
by LABEL) when applied to ASSET-INDICATOR."
  (with-machine machine
    (let ((result (skyline-tool::asset->object-name asset-indicator :video :ntsc)))
      (is (stringp result)
          "~a (machine ~a): asset->object-name should return a string for ~s, got ~s"
          label machine asset-indicator result)
      (is (plusp (length result))
          "~a (machine ~a): asset->object-name must return a non-empty string for ~s"
          label machine asset-indicator))))

;;; Standard TV-connected machines — video-suffix paths.

(test asset-object-name-standard-machines-songs
  "ASSET->OBJECT-NAME must return a video-suffixed Song path for every
standard TV-connected machine and must not signal ECASE failure."
  (dolist (entry '((7800 "7800") (7850 "VCS800") (5200 "5200")
                   (400 "400") (800 "800")
                   (1 "Oric") (2 "A2") (3 "A3") (8 "NES") (9 "NG")
                   (15 "F") (16 "TG16") (20 "VIC20") (23 "A2e")
                   (81 "ZX81") (88 "SNES") (222 "2gs") (223 "BBC")
                   (264 "C16") (920 "NNG") (1000 "SG1000")
                   (1080 "ST") (1200 "1200") (1601 "SMD") (1624 "32X")
                   (2068 "Spc") (2416 "CDR") (2600 "2600")
                   (3000 "Vx") (3010 "SMS") (4386 "HS") (6122 "Vs")
                   (7600 "O2") (7801 "SC") (8011 "Jag") (9001 "PSX")))
    (destructuring-bind (machine label) entry
      (%check-asset->object-name machine label "Songs/Title"))))

(test asset-object-name-standard-machines-maps
  "ASSET->OBJECT-NAME must return a video-suffixed Map path for every
standard TV-connected machine."
  (dolist (entry '((7800 "7800") (5200 "5200") (8 "NES") (16 "TG16")
                   (88 "SNES") (3010 "SMS") (2 "A2") (3 "A3")
                   (264 "C16") (223 "BBC") (222 "2gs")))
    (destructuring-bind (machine label) entry
      (%check-asset->object-name machine label "Maps/Solace/AncientBurialSite2"))))

(test asset-object-name-standard-machines-video-suffix-present
  "ASSET->OBJECT-NAME Song paths for TV-connected machines must embed
the video-standard token (NTSC or PAL, case-insensitively) so that
NTSC and PAL builds produce distinct Makefile targets."
  (dolist (machine '(7800 8 16 88 3010 2600 5200))
    (with-machine machine
      (let ((ntsc (skyline-tool::asset->object-name "Songs/Title" :video :ntsc))
            (pal  (skyline-tool::asset->object-name "Songs/Title" :video :pal)))
        (is (search "ntsc" ntsc :test #'char-equal)
            "machine ~a Song NTSC path should contain 'ntsc' (case-insensitive), got ~s"
            machine ntsc)
        (is (search "pal" pal :test #'char-equal)
            "machine ~a Song PAL path should contain 'pal' (case-insensitive), got ~s"
            machine pal)
        (is (not (string= ntsc pal))
            "machine ~a NTSC and PAL Song paths must differ" machine)))))

;;; Portable/single-region machines — video-suffix-free paths.

(test asset-object-name-portable-machines-songs
  "ASSET->OBJECT-NAME must return a video-suffix-FREE Song path for every
portable/single-region machine (DMG, CGB, GBA, GG, WS, WSC, VB, Lynx)."
  (dolist (entry '((200 "Lynx") (810 "VB") (837 "GG") (3296 "GBA")
                   (4800 "WS") (6800 "WSC") (20953 "CGB") (35902 "DMG")))
    (destructuring-bind (machine label) entry
      (with-machine machine
        (let ((result (skyline-tool::asset->object-name "Songs/Title" :video :ntsc)))
          (is (stringp result)
              "~a (machine ~a): asset->object-name Songs must return a string" label machine)
          (is (not (search "ntsc" result :test #'char-equal))
              "~a (machine ~a): portable Song path must NOT embed video suffix, got ~s"
              label machine result)
          (is (not (search "pal" result :test #'char-equal))
              "~a (machine ~a): portable Song path must NOT embed video suffix, got ~s"
              label machine result))))))

(test asset-object-name-portable-machines-maps
  "ASSET->OBJECT-NAME must return a video-suffix-free Map path for every
portable/single-region machine."
  (dolist (entry '((200 "Lynx") (810 "VB") (837 "GG") (3296 "GBA")
                   (4800 "WS") (6800 "WSC") (20953 "CGB") (35902 "DMG")))
    (destructuring-bind (machine label) entry
      (with-machine machine
        (let ((result (skyline-tool::asset->object-name
                       "Maps/Solace/AncientBurialSite2" :video :ntsc)))
          (is (stringp result)
              "~a (machine ~a): asset->object-name Maps must return a string" label machine)
          (is (not (search "ntsc" result :test #'char-equal))
              "~a (machine ~a): portable Map path must NOT embed video suffix, got ~s"
              label machine result))))))

(test asset-object-name-portable-machines-blobs-and-scripts
  "ASSET->OBJECT-NAME must place Blobs and Scripts under Source/Generated/
for all portable/single-region machines."
  (dolist (entry '((200 "Lynx") (837 "GG") (3296 "GBA")
                   (20953 "CGB") (35902 "DMG")))
    (destructuring-bind (machine label) entry
      (%check-asset->object-name machine label "Blobs/TitleCard")
      (%check-asset->object-name machine label "Scripts/Title"))))

;;; Special-cased platforms.

(test asset-object-name-intv-songs-are-generated-source
  "Intellivision (machine 2609) Songs must be emitted as .s source files
rather than .o object files, since the Intv assembler is invoked later."
  (with-machine 2609
    (let ((result (skyline-tool::asset->object-name "Songs/Title" :video :ntsc)))
      (is (stringp result) "Intv Song path must be a string")
      (is (search "Source/Generated" result)
          "Intv Song path must be under Source/Generated, got ~s" result)
      (is (search ".s" result)
          "Intv Song path must end with .s, got ~s" result))))

(test asset-object-name-cbm-paths-include-cbm-marker
  "CBM (C64/C128, machines 64 and 128) Song and Map paths must include
the .CBM. marker to disambiguate from standard 6502 builds."
  (dolist (machine '(64 128))
    (with-machine machine
      (let ((song (skyline-tool::asset->object-name "Songs/Title" :video :ntsc))
            (map  (skyline-tool::asset->object-name
                   "Maps/Solace/AncientBurialSite2" :video :ntsc)))
        (is (search "CBM" song)
            "CBM machine ~a Song path must contain 'CBM', got ~s" machine song)
        (is (search "CBM" map)
            "CBM machine ~a Map path must contain 'CBM', got ~s" machine map)))))

(test asset-object-name-clcv-paths-include-clcv-marker
  "ColecoVision (machine 9918) Song and Map paths must include the
.ClcV. marker to disambiguate from standard paths."
  (with-machine 9918
    (let ((song (skyline-tool::asset->object-name "Songs/Title" :video :ntsc))
          (map  (skyline-tool::asset->object-name
                 "Maps/Solace/AncientBurialSite2" :video :ntsc)))
      (is (search "ClcV" song)
          "ClcV Song path must contain 'ClcV', got ~s" song)
      (is (search "ClcV" map)
          "ClcV Map path must contain 'ClcV', got ~s" map))))

;;; SUPPORTED-VIDEO-TYPES — portable machines must return a single-element list.

(test supported-video-types-portable-machines-single-region
  "SUPPORTED-VIDEO-TYPES must return a one-element list for every
portable/single-region machine so that Makefile generation does not
emit duplicate identical targets."
  (dolist (entry '((200 "Lynx") (810 "VB") (837 "GG") (3296 "GBA")
                   (4800 "WS") (6800 "WSC") (20953 "CGB") (35902 "DMG")))
    (destructuring-bind (machine label) entry
      (let ((types (skyline-tool::supported-video-types machine)))
        (is (= 1 (length types))
            "~a (machine ~a): supported-video-types must return exactly one element, got ~s"
            label machine types)
        (is (member :ntsc types)
            "~a (machine ~a): supported-video-types single element must be :ntsc, got ~s"
            label machine types)))))

;; Test asset->symbol-name function
;;
;; ASSET->SYMBOL-NAME returns a STRING (not a symbol) in the form
;; "Kind_Name", dropping the trailing 's' from the kind component.
;; For example, "Songs/Title" → "Song_Title".

(test asset-symbol-name-basic
  "ASSET->SYMBOL-NAME returns a string identifier derived from the
asset indicator, suitable for use as an assembler symbol."
  (let ((result (skyline-tool::asset->symbol-name "Songs/Title")))
    (is (stringp result) "Should return a string")
    (is (search "Title" result) "Should contain the asset name component"))
  (let ((result (skyline-tool::asset->symbol-name "Blobs/TitleCard")))
    (is (stringp result) "Should return a string for Blobs")
    (is (search "TitleCard" result) "Should contain blob name")))

;; Test asset->source-name function
;;
;; ASSET->SOURCE-NAME takes an "Kind/Name" indicator and returns the source
;; path string (e.g. "Songs/Title" → "Source/Songs/Title.midi").
(test asset-source-name-basic
  "ASSET->SOURCE-NAME returns a non-empty string for each known asset kind."
  (let ((skyline-tool::*machine* 7800))
    (let ((result (skyline-tool::asset->source-name "Songs/Title")))
      (is (stringp result) "Songs: should return a string")
      (is (search "Title" result) "Songs: result should contain the asset name")
      (is (search "midi" result) "Songs: result should have .midi extension"))
    (let ((result (skyline-tool::asset->source-name "Maps/Solace/Dungeon")))
      (is (stringp result) "Maps: should return a string")
      (is (search "tmx" result) "Maps: result should have .tmx extension"))
    (let ((result (skyline-tool::asset->source-name "Scripts/Intro")))
      (is (stringp result) "Scripts: result should have .fountain extension")
      (is (search "fountain" result) "Scripts: result should have .fountain extension"))))

;; Test speech-supported-p function
;;
;; SPEECH-SUPPORTED-P returns true for platforms with speech-synthesis
;; hardware: Atari 2600 (AtariVox), 7800 (AtariVox), and Intellivision
;; (IntelliVoice/machine 2609).  All other machines return NIL.

(test speech-supported-p-basic
  "SPEECH-SUPPORTED-P must return true for AtariVox/IntelliVoice
platforms (2600, 7800, 2609) and false for all others."
  (let ((skyline-tool::*machine* 2600))
    (is-true (skyline-tool::speech-supported-p) "2600 supports AtariVox speech"))
  (let ((skyline-tool::*machine* 7800))
    (is-true (skyline-tool::speech-supported-p) "7800 supports AtariVox speech"))
  (let ((skyline-tool::*machine* 2609))
    (is-true (skyline-tool::speech-supported-p) "Intv supports IntelliVoice speech"))
  (let ((skyline-tool::*machine* 8))
    (is-false (skyline-tool::speech-supported-p) "NES does not support speech"))
  (let ((skyline-tool::*machine* 5200))
    (is-false (skyline-tool::speech-supported-p) "5200 does not support speech")))

;; Test current-julian-date function
;;
;; CURRENT-JULIAN-DATE returns the day-of-year (1–366), not an astronomical
;; Julian Day Number.
(test current-julian-date-basic
  "CURRENT-JULIAN-DATE returns an integer day-of-year in the range 1–366."
  (let ((date (skyline-tool::current-julian-date)))
    (is (integerp date) "Should return an integer")
    (is (>= date 1) "Day-of-year should be at least 1")
    (is (<= date 366) "Day-of-year should be at most 366")))

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