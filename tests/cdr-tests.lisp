;;; Phantasia SkylineTool/tests/cdr-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

;; Use the test package defined by the test runner
(in-package :skyline-tool/test)

(def-suite cdr-tests
  :description "Tests for Commander X-16-specific SkylineTool functionality"
  :in skyline-tool/test)

(in-suite cdr-tests)

;; Test Commander X-16 platform support
(test cdr-platform-support
      "Test that Commander X-16 (CDR/2416) is properly supported"
      ;; Test machine number mapping
      (is (= 2416 (skyline-tool::machine-number-by-tag "CDR")))
      (is (= 2416 (skyline-tool::machine-number-by-tag "cdr")))

      ;; Test machine validation
      (let ((skyline-tool::*machine* 2416))
        (is-true (skyline-tool::machine-valid-p))
        (is (string= "CDR" (skyline-tool::machine-directory-name)))
        (is (string= "Commander X-16" (skyline-tool::machine-short-name)))
        (is (string= "Commander X-16" (skyline-tool::machine-long-name)))))

;; Test Commander X-16 graphics dispatch
(test cdr-graphics-dispatch
      "Test Commander X-16 PNG dispatch functionality"
      ;; Verify dispatch-png% method exists
      (is-true (fboundp 'skyline-tool::dispatch-png%))
      (is (not (null (find-method #'skyline-tool::dispatch-png% '() (list (list 'eql 2416) t t t t t t t) nil)))
          "dispatch-png% should have a method for Commander X-16")

      ;; Test text font dispatch (128x64)
      (let ((font-pixels (make-array '(128 64) :element-type '(unsigned-byte 32) :initial-element 0)))
        ;; Create simple font pattern
        (dotimes (char-y 8)
          (dotimes (char-x 16)
            (dotimes (pixel-y 8)
              (dotimes (pixel-x 8)
                (let ((x (+ (* char-x 8) pixel-x))
                      (y (+ (* char-y 8) pixel-y)))
                  (when (and (< x 128) (< y 64))
                    (setf (aref font-pixels x y) (if (< pixel-x 4) 1 0))))))))
        (is-true (skyline-tool::dispatch-png% 2416 "/test-font.png" "Object/CDR/" font-pixels 64 128 nil font-pixels)
                 "Should handle 128x64 text font"))

      ;; Test tileset dispatch
      (let ((tileset-pixels (make-array '(256 128) :element-type '(unsigned-byte 32) :initial-element 0)))
        (is-true (skyline-tool::dispatch-png% 2416 "/test-tiles.png" "Object/CDR/" tileset-pixels 128 256 nil tileset-pixels)
                 "Should handle tileset images"))

      ;; Test bitmap dispatch (320x240)
      (let ((bitmap-pixels (make-array '(320 240) :element-type '(unsigned-byte 32) :initial-element 0)))
        (is-true (skyline-tool::dispatch-png% 2416 "/test-bitmap.png" "Object/CDR/" bitmap-pixels 240 320 nil bitmap-pixels)
                 "Should handle 320x240 bitmap"))

      ;; Test sprite dispatch
      (let ((sprite-pixels (make-array '(64 32) :element-type '(unsigned-byte 32) :initial-element 0)))
        (is-true (skyline-tool::dispatch-png% 2416 "/test-sprite.png" "Object/CDR/" sprite-pixels 32 64 nil sprite-pixels)
                 "Should handle sprite images")))

;; Test Commander X-16 graphics converters
(test cdr-graphics-converters
      "Test Commander X-16 graphics converter functions"
      ;; Test text font compilation
      (let ((font-pixels (make-array '(128 64) :element-type '(unsigned-byte 32) :initial-element 0))
            (temp-file "Object/CDR/test-font.s"))
        ;; Create test font data
        (dotimes (x 128)
          (dotimes (y 64)
            (setf (aref font-pixels x y) (mod (+ x y) 2))))

        (is-true (skyline-tool::compile-cdr-text-font (make-pathname :name "test-font") "Object/CDR/" font-pixels)
                 "Should compile text font successfully")

        (when (probe-file temp-file)
          (with-open-file (stream temp-file :direction :input)
            (let ((content (alexandria:read-stream-content-into-string stream)))
              (is-true (search "Commander X-16 Text Font" content))
              (is-true (search "font_data:" content))
              (is-true (search ".byte $" content))))))

      ;; Test tileset compilation
      (let ((tileset-pixels (make-array '(64 64) :element-type '(unsigned-byte 32) :initial-element 0))
            (temp-file "Object/CDR/test-tileset.s"))
        (is-true (skyline-tool::compile-cdr-tileset (make-pathname :name "test-tileset") "Object/CDR/" 64 64 tileset-pixels)
                 "Should compile tileset successfully")

        (when (probe-file temp-file)
          (with-open-file (stream temp-file :direction :input)
            (let ((content (alexandria:read-stream-content-into-string stream)))
              (is-true (search "Commander X-16 Tileset" content))
              (is-true (search "tileset_data:" content))))))

      ;; Test bitmap compilation
      (let ((bitmap-pixels (make-array '(320 240) :element-type '(unsigned-byte 32) :initial-element 0))
            (temp-file "Object/CDR/test-bitmap.s"))
        (is-true (skyline-tool::compile-cdr-bitmap (make-pathname :name "test-bitmap") "Object/CDR/" 240 320 bitmap-pixels)
                 "Should compile bitmap successfully")

        (when (probe-file temp-file)
          (with-open-file (stream temp-file :direction :input)
            (let ((content (alexandria:read-stream-content-into-string stream)))
              (is-true (search "Commander X-16 Bitmap" content))
              (is-true (search "bitmap_data:" content))))))

      ;; Test sprite compilation
      (let ((sprite-pixels (make-array '(32 32) :element-type '(unsigned-byte 32) :initial-element 0))
            (temp-file "Object/CDR/test-sprite.s"))
        (is-true (skyline-tool::compile-cdr-sprite (make-pathname :name "test-sprite") "Object/CDR/" 32 32 sprite-pixels)
                 "Should compile sprite successfully")

        (when (probe-file temp-file)
          (with-open-file (stream temp-file :direction :input)
            (let ((content (alexandria:read-stream-content-into-string stream)))
              (is-true (search "Commander X-16 Sprite" content))
              (is-true (search "sprite_data:" content)))))))

;; Test Commander X-16 music compilation
(test cdr-music-compilation
      "Test Commander X-16 music compilation"
      ;; Test YM2151 music compilation
      (let ((temp-file "Object/CDR/test-ym2151.s")
            (input-file "test-input.mid"))
        (unwind-protect
             ;; Create minimal test input
             (with-open-file (out input-file :direction :output :if-exists :supersede)
               (write '((:note-on :channel 0 :key 60 :velocity 100 :time 0)
                        (:note-off :channel 0 :key 60 :velocity 0 :time 100)) :stream out :readably t))

          (is-true (skyline-tool::compile-music-for-machine 2416 "YM2151" temp-file input-file "NTSC")
                   "Should compile YM2151 music successfully")

          (when (probe-file temp-file)
            (with-open-file (stream temp-file :direction :input)
              (let ((content (alexandria:read-stream-content-into-string stream)))
                (is-true (search "Commander X-16 YM2151 Music" content))
                (is-true (search "ym2151_init:" content))
                (is-true (search "ym2151_notes:" content))
                (is-true (search "fm_voice_data:" content))
                (is-true (search "play_fm_note:" content))))))

        ;; Cleanup
        (ignore-errors (delete-file temp-file))
        (ignore-errors (delete-file input-file))))

;; Test PSG music compilation
(let ((temp-file "Object/CDR/test-psg.s")
      (input-file "test-input.mid"))
  (unwind-protect
       ;; Create minimal test input
       (with-open-file (out input-file :direction :output :if-exists :supersede)
         (write '((:note-on :channel 0 :key 60 :velocity 100 :time 0)
                  (:note-off :channel 0 :key 60 :velocity 0 :time 100)) :stream out :readably t))

    (is-true (skyline-tool::compile-music-for-machine 2416 "PSG" temp-file input-file "NTSC")
             "Should compile PSG music successfully")
    
    (when (probe-file temp-file)
      (with-open-file (stream temp-file :direction :input)
        (let ((content (alexandria:read-stream-content-into-string stream)))
          (is-true (search "Commander X-16 PSG Music" content))
          (is-true (search "psg_init:" content))
          (is-true (search "psg_notes:" content))
          (is-true (search "play_psg_note:" content))))))
  
  ;; Cleanup
  (ignore-errors (delete-file temp-file))
  (ignore-errors (delete-file input-file))
  
  ;; Test unsupported sound chip
  (let ((temp-file "Object/CDR/test-unknown.s")
        (input-file "test-input.mid"))
    (unwind-protect
         (with-open-file (out input-file :direction :output :if-exists :supersede)
           (write '((:note-on :channel 0 :key 60 :velocity 100 :time 0)) :stream out :readably t))
      
      (is-true (skyline-tool::compile-music-for-machine 2416 "UNKNOWN" temp-file input-file "NTSC")
               "Should handle unknown sound chips gracefully")
      
      (when (probe-file temp-file)
        (with-open-file (stream temp-file :direction :input)
          (let ((content (alexandria:read-stream-content-into-string stream)))
            (is-true (search "Unknown sound chip: UNKNOWN" content))
            (is-true (search "unknown_chip_error:" content))))))
    
    ;; Cleanup
    (ignore-errors (delete-file temp-file))
    (ignore-errors (delete-file input-file))))

;; Test error handling for Commander X-16
(test cdr-error-handling
      "Test error handling for Commander X-16 converters"
      ;; Test invalid dimensions for dispatch
      (let ((invalid-pixels (make-array '(10 10) :element-type '(unsigned-byte 32) :initial-element 0)))
        (signals error (skyline-tool::dispatch-png% 2416 "/test.png" "Object/CDR/" invalid-pixels 10 10 nil invalid-pixels)
                 "Should signal error for unsupported image dimensions"))
      
      ;; Test invalid sound chip
      (let ((temp-file "Object/CDR/test-error.s")
            (input-file "test-input.mid"))
        (unwind-protect
             (with-open-file (out input-file :direction :output :if-exists :supersede)
               (write '((:note-on :channel 0 :key 60 :velocity 100 :time 0)) :stream out :readably t))

          ;; Should handle gracefully (we test this above)
          (finishes (skyline-tool::compile-music-for-machine 2416 "INVALID" temp-file input-file "NTSC")))

        ;; Cleanup
        (ignore-errors (delete-file temp-file))
        (ignore-errors (delete-file input-file))))

(defun run-cdr-tests ()
  "Run all Commander X-16 tests and return results"
  (fiveam:run! 'cdr-tests))
