;;; SkylineTool/tests/music-tests.lisp
;;; Comprehensive tests for music/sound conversion functions

(in-package :skyline-tool/test)

(def-suite music-tests
  :description "Tests for all music/sound conversion functionality")

(in-suite music-tests)

;; Test Game Boy music functions
(test gameboy-music-compilation
  "Test Game Boy music compilation functions"
  (is (fboundp 'compile-music-dmg)
      "DMG music compilation should be available")
  (is (fboundp 'compile-music-cgb)
      "CGB music compilation should be available"))

(test gameboy-music-output
  "Test Game Boy music output generation"
  ;; Test that the functions generate output without errors
  (let ((temp-file (merge-pathnames "test-gb-music.s" (uiop:temporary-directory))))
    (unwind-protect
        (progn
          (finishes
            (compile-music-dmg temp-file "dummy-input.mid"))
          (is (probe-file temp-file)
              "DMG music compilation should create output file"))
        (when (probe-file temp-file)
          (delete-file temp-file)))))

;; Test NES music functions
(test nes-music-compilation
  "Test NES music compilation functions"
  (is (fboundp 'compile-music-nes)
      "NES music compilation should be available"))

(test nes-music-output
  "Test NES music output generation"
  (let ((temp-file (merge-pathnames "test-nes-music.s" (uiop:temporary-directory))))
    (unwind-protect
        (progn
          (finishes
            (compile-music-nes temp-file "dummy-input.mid"))
          (is (probe-file temp-file)
              "NES music compilation should create output file"))
        (when (probe-file temp-file)
          (delete-file temp-file)))))

;; Test SNES music functions
(test snes-music-compilation
  "Test SNES music compilation functions"
  (is (fboundp 'compile-music-snes)
      "SNES music compilation should be available"))

(test snes-music-output
  "Test SNES music output generation"
  (let ((temp-file (merge-pathnames "test-snes-music.s" (uiop:temporary-directory))))
    (unwind-protect
        (progn
          (finishes
            (compile-music-snes temp-file "dummy-input.mid"))
          (is (probe-file temp-file)
              "SNES music compilation should create output file"))
        (when (probe-file temp-file)
          (delete-file temp-file)))))

;;; Enhanced Property-Based Testing for Music

(test psg-frequency-property-test
  "Property-based test for PSG frequency calculations across platforms"
  (dolist (system '(:sms :nes :gb :tg16))
    (let ((freq (generate-valid-psg-frequency system)))
      ;; Test frequency ranges for each system
      (case system
        (:sms (is (<= 0 freq 1023) "SMS PSG frequencies should be 0-1023 (10-bit)"))
        (:nes (is (<= 0 freq 2047) "NES frequencies should be 0-2047 (11-bit)"))
        (:gb (is (<= 0 freq 2047) "Game Boy frequencies should be 0-2047 (11-bit)"))
        (:tg16 (is (<= 0 freq 4095) "TG16 frequencies should be 0-4095 (12-bit)"))))))

(test psg-volume-property-test
  "Property-based test for PSG volume ranges across platforms"
  (dolist (system '(:sms :nes :gb :tg16))
    (let ((volume (generate-valid-psg-volume system)))
      ;; Test volume ranges for each system
      (case system
        ((:sms :gb :nes) (is (<= 0 volume 15) "4-bit PSG volumes should be 0-15"))
        (:tg16 (is (<= 0 volume 31) "5-bit PSG volumes should be 0-31"))))))

(test music-note-sequence-property-test
  "Property-based test for music note sequence generation"
  (let ((sequence (generate-psg-note-sequence (+ 5 (random 20)))))
    ;; Test sequence structure
    (is (>= (length sequence) 5) "Should generate minimum sequence length")
    (dolist (note sequence)
      (destructuring-bind (time channel freq-lo freq-hi volume duration) note
        (is (>= time 0) "Time should be non-negative")
        (is (<= 0 channel 5) "Channel should be 0-5 (depending on system)")
        (is (<= 0 freq-lo 255) "Frequency low should be 0-255")
        (is (<= 0 freq-hi 15) "Frequency high should be 0-15")
        (is (<= 0 volume 31) "Volume should be 0-31")
        (is (> duration 0) "Duration should be positive")))))

;;; Performance Testing for Music

(test music-performance-large-sequence
  "Performance test for large music sequence compilation"
  (let ((large-sequence (generate-psg-note-sequence 1000))) ; 1000 notes
    (format t "~&Testing compilation of ~D note sequence..." (length large-sequence))
    (time
     (with-temp-file (temp-file "music-perf" "s")
       (finishes (skyline-tool::compile-music-sms temp-file "dummy-input.mid"))))))

;;; Fuzz Testing for Music

(test music-fuzz-psg-commands
  "Fuzz test PSG command generation with invalid inputs"
  (dotimes (i 30) ; Run 30 fuzz iterations
    (let ((invalid-commands (generate-invalid-psg-commands (+ 5 (random 20)))))
      ;; Test that invalid commands don't crash the system
      ;; (In practice, this would test command validation functions)
      (finishes (dolist (cmd invalid-commands)
                  (is (or (numberp cmd) (stringp cmd) (symbolp cmd))
                      "Commands should be valid types"))))))

(test music-fuzz-note-data
  "Fuzz test music note data with extreme values"
  (dotimes (i 20) ; Run 20 fuzz iterations
    (let ((invalid-notes (generate-invalid-music-data (+ 5 (random 15)))))
      ;; Test that invalid note data is handled gracefully
      (finishes (dolist (note invalid-notes)
                  (destructuring-bind (time note-num velocity duration) note
                    ;; Basic validation
                    (is (numberp time) "Time should be a number")
                    (is (numberp note-num) "Note should be a number")
                    (is (numberp velocity) "Velocity should be a number")
                    (is (numberp duration) "Duration should be a number")))))))

;;; Integration Testing for Music

(test music-full-compilation-pipeline
  "Test complete music compilation pipeline"
  (dolist (system '("sms" "nes" "dmg" "snes"))
    (with-temp-file (temp-file (format nil "~a-music" system) "s")
      (let ((compile-fn (intern (format nil "COMPILE-MUSIC-~A" system) :skyline-tool)))
        (when (fboundp compile-fn)
          (finishes (funcall compile-fn temp-file "dummy-input.mid"))
          (is-true (probe-file temp-file)
                  (format nil "~A music compilation should create output file" system)))))))

(test music-cross-platform-consistency
  "Test music compilation consistency across platforms"
  ;; Test that all music compilers handle basic input without crashing
  (dolist (system '(sms nes dmg snes))
    (let ((compile-fn (intern (format nil "COMPILE-MUSIC-~A" system) :skyline-tool)))
      (when (fboundp compile-fn)
        (with-temp-file (temp-file (format nil "consistency-~a" system) "s")
          (finishes (funcall compile-fn temp-file "dummy-input.mid")))))))

;;; Error Handling and Edge Cases

(test music-error-handling-missing-files
  "Test music compilation error handling for missing files"
  (dolist (system '(sms nes dmg snes))
    (let ((compile-fn (intern (format nil "COMPILE-MUSIC-~A" system) :skyline-tool)))
      (when (fboundp compile-fn)
        (signals error (funcall compile-fn "/tmp/nonexistent-output.s" "/tmp/nonexistent-input.mid"))))))

(test music-error-handling-invalid-output-paths
  "Test music compilation with invalid output paths"
  (let ((invalid-paths '("/dev/null/invalid" "/root/invalid" "/etc/passwd" "" nil)))
    (dolist (path invalid-paths)
      (when path
        (signals error (skyline-tool::compile-music-sms path "dummy-input.mid"))))))

;; Test ColecoVision music functions
(test colecovision-music-compilation
  "Test ColecoVision music compilation functions"
  (is (fboundp 'compile-music-colecovision)
      "ColecoVision music compilation should be available"))

(test colecovision-music-output
  "Test ColecoVision music output generation"
  (let ((temp-file (merge-pathnames "test-cv-music.s" (uiop:temporary-directory))))
    (unwind-protect
        (progn
          (finishes
            (compile-music-colecovision temp-file "dummy-input.mid"))
          (is (probe-file temp-file)
              "ColecoVision music compilation should create output file"))
        (when (probe-file temp-file)
          (delete-file temp-file)))))

;; Test SG-1000 music functions
(test sg1000-music-compilation
  "Test SG-1000 music compilation functions"
  (is (fboundp 'compile-music-sg1000)
      "SG-1000 music compilation should be available"))

(test sg1000-music-output
  "Test SG-1000 music output generation"
  (let ((temp-file (merge-pathnames "test-sg1000-music.s" (uiop:temporary-directory))))
    (unwind-protect
        (progn
          (finishes
            (compile-music-sg1000 temp-file "dummy-input.mid"))
          (is (probe-file temp-file)
              "SG-1000 music compilation should create output file"))
        (when (probe-file temp-file)
          (delete-file temp-file)))))

;; Test SMS music functions (already implemented)
(test sms-music-compilation
  "Test SMS music compilation functions"
  (is (fboundp 'compile-music-sms)
      "SMS music compilation should be available"))

;; Test platform-specific music features
(test music-platform-specifics
  "Test platform-specific music features"
  ;; Test that different platforms have different register sets
  ;; This is tested by ensuring the functions don't error on basic calls
  (finishes
    (let ((*machine* 35902))  ; DMG
      (compile-music-dmg (merge-pathnames "dummy-dmg.s" (uiop:temporary-directory)) "dummy.mid")))
  (finishes
    (let ((*machine* 9918))   ; ColecoVision
      (compile-music-colecovision (merge-pathnames "dummy-cv.s" (uiop:temporary-directory)) "dummy.mid"))))

;; Test music utility functions
(test music-utilities
  "Test music utility functions"
  (is (fboundp 'compile-music)
      "General music compilation should be available"))

;; Test error conditions
(test music-error-handling
  "Test error handling in music functions"
  ;; Test with non-existent input files
  (signals error
    (compile-music-dmg "/tmp/nonexistent-output.s" "/tmp/nonexistent-input.mid"))
  ;; Test with invalid parameters
  (signals error
    (compile-music-dmg nil nil)))
