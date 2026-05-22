;;; Phantasia SkylineTool/tests/intv-palette-tests.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC.

;; Use the test package defined by the test runner
(in-package :skyline-tool/test)

(def-suite intv-palette-tests
  :description "Tests for Intellivision palette generation (Req-Intv-PaletteGen)"
  :in skyline-tool/test)

(in-suite intv-palette-tests)

;; Test that the generator function is available and exported
(test intv-palette-generator-exists
  "intv-palette-generator function must be defined and exported from skyline-tool"
  (is-true (fboundp 'skyline-tool:intv-palette-generator)
           "intv-palette-generator should be fbound")
  (is-true (find-symbol "INTV-PALETTE-GENERATOR" :skyline-tool)
           "INTV-PALETTE-GENERATOR symbol should exist in package")
  (is (eql (nth-value 1 (find-symbol "INTV-PALETTE-GENERATOR" :skyline-tool))
           :external)
      "intv-palette-generator should be exported"))

;; Test CLI hook registration
(test intv-palette-cli-hook-registered
  "The --intv-palette command must be registered in *invocation* without affecting other commands"
  (is (getf skyline-tool::*invocation* :--intv-palette)
      "--intv-palette must be in *invocation*")
  (is (getf skyline-tool::*invocation* :intv-palette-generator)
      ":intv-palette-generator must also be registered")
  ;; Ensure no side effects: other keys still there
  (is (getf skyline-tool::*invocation* :--help)
      "pre-existing --help hook must still be present")
  (is (getf skyline-tool::*invocation* :write-gimp-palettes)
      "pre-existing write-gimp-palettes must still be present"))

;; Core generator tests for acceptance criteria
(test intv-palette-generator-pure-colors
  "Correct mapping of pure white, black, red, cyan to 5 fade indices"
  ;; Black target (0 0 0) -> all black index 0
  (let ((result (skyline-tool:intv-palette-generator '(0 0 0))))
    (is (= 5 (length result)) "must generate exactly 5 indices")
    (is (every (lambda (i) (= i 0)) result)
        "black target must map to (0 0 0 0 0)"))
  ;; White target (255 255 255) -> (0 8 8 7 7)
  (let ((result (skyline-tool:intv-palette-generator '(255 255 255))))
    (is (= 5 (length result)) "must generate exactly 5 indices")
    (is (equal result '(0 8 8 7 7))
        "white target must map to (0 8 8 7 7)"))
  ;; Red target (255 0 0) -> (0 0 2 2 2)
  (let ((result (skyline-tool:intv-palette-generator '(255 0 0))))
    (is (= 5 (length result)) "must generate exactly 5 indices")
    (is (equal result '(0 0 2 2 2))
        "red target must map to (0 0 2 2 2)"))
  ;; Cyan target (0 255 255) -> (0 4 8 9 9)
  (let ((result (skyline-tool:intv-palette-generator '(0 255 255))))
    (is (= 5 (length result)) "must generate exactly 5 indices")
    (is (equal result '(0 4 8 9 9))
        "cyan target must map to (0 4 8 9 9)")))

(test intv-palette-generator-vector-input
  "Generator accepts vector RGB as well as list"
  (let ((result (skyline-tool:intv-palette-generator #(255 255 255))))
    (is (= 5 (length result)))
    (is (equal result '(0 8 8 7 7)))))

(test intv-palette-generator-intermediates
  "Proper intermediate values are rounded to nearest fixed colour"
  ;; 50% red (127 0 0) should nearest black (index 0)
  (let ((result (skyline-tool:intv-palette-generator '(127 0 0))))
    (is (= 5 (length result)))
    (is (every (lambda (i) (= i 0)) result)))
  ;; 25% white approx (64 64 64) -> black until 64 which is gray 8
  (let ((result (skyline-tool:intv-palette-generator '(64 64 64))))
    (is (= 5 (length result)))
    (is (equal result '(0 0 0 0 8)) "low gray levels nearest to black until threshold")))

(test intv-palette-generator-always-5-indices
  "Generates exactly 5 indices for any target RGB, all valid 0-15"
  (dolist (target '((100 150 200) (0 0 255) (255 255 0) (128 64 32)))
    (let ((result (skyline-tool:intv-palette-generator target)))
      (is (= 5 (length result)) "exactly 5 for ~a" target)
      (is (every (lambda (i) (and (integerp i) (<= 0 i 15))) result)
          "all indices 0-15 for ~a got ~a" target result))))

;; Test the JSON exposure helper if present (for language-agnostic)
(test intv-palette-json-interface
  "Exposes JSON interface for the palette indices (byteArrayToJSON style)"
  (when (fboundp 'skyline-tool::intv-palette-as-json)
    (let ((json-str (skyline-tool::intv-palette-as-json '(255 0 0))))
      (is (stringp json-str))
      (is (search "[0,0,2,2,2]" json-str) "json should contain the indices"))))

;; CLI command functional test using temp files
(test intv-palette-cli-command
  "CLI --intv-palette accepts --input JSON of RGBs and writes .pal with indices bytes"
  (let* ((tmpdir (format nil "/tmp/intv-pal-test-~D" (get-universal-time)))
         (input (merge-pathnames "target-colors.json" tmpdir))
         (output (merge-pathnames "IntvPalette.pal" tmpdir))
         (json-content "[[255,255,255],[0,0,0],[255,0,0],[0,255,255]]"))
    (ensure-directories-exist tmpdir)
    (with-open-file (out input :direction :output :if-exists :supersede)
      (write-string json-content out))
    ;; Call the command function with arg list as CLI would
    (finishes (skyline-tool:intv-palette-command "--input" (namestring input)
                                                 "--output" (namestring output)))
    (is-true (probe-file output) "output .pal file must be created")
    (let ((bytes (with-open-file (in output :element-type '(unsigned-byte 8))
                   (loop for b = (read-byte in nil nil) while b collect b))))
      (is (= (length bytes) 20) "4 targets × 5 indices = 20 bytes")
      (is (equal (subseq bytes 0 5) '(0 8 8 7 7)) "first white")
      (is (equal (subseq bytes 5 10) '(0 0 0 0 0)) "second black")
      (is (equal (subseq bytes 10 15) '(0 0 2 2 2)) "third red")
      (is (equal (subseq bytes 15 20) '(0 4 8 9 9)) "fourth cyan"))
    ;; cleanup
    (ignore-errors (delete-file input))
    (ignore-errors (delete-file output))
    (ignore-errors (uiop:delete-empty-directory tmpdir))))

(defun run-intv-palette-tests ()
  "Run the IntV palette generator tests"
  (fiveam:run! 'intv-palette-tests))
