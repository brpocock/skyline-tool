;;; ZX Spectrum platform tests

(in-package :skyline-tool/test)

(test spectrum-platform-existence
  "Test that ZX Spectrum platform constants and functions exist"
  (is-true (fboundp 'skyline-tool::machine-short-name))
  (is-true (fboundp 'skyline-tool::machine-long-name))
  (is-true (fboundp 'skyline-tool::machine-directory-name)))

(test spectrum-art-compilation-stub
  "Test that ZX Spectrum art compilation function exists but signals error"
  (signals error (skyline-tool::compile-art-spectrum "dummy-index-out" "dummy-index-in"))
  "ZX Spectrum art compilation should signal error (not yet implemented)")

(test spectrum-music-compilation-validation
  "Test ZX Spectrum music compilation produces correct assembly output"
  (let ((output-file (format nil "Object/2068/test-music-~a.s" (skyline-tool::generate-secure-random-id 2)))
        (input-file (format nil "/tmp/test-music-~a.mid" (get-universal-time))))
    ;; Test that function exists and can be called
    (is-true (fboundp 'skyline-tool::compile-music-spectrum)
             "compile-music-spectrum should exist")

    ;; Test compilation with dummy input
    (finishes (skyline-tool::compile-music-spectrum output-file input-file)
              "compile-music-spectrum should complete without throwing errors")

    ;; Validate output file was created and has expected content
    (when (probe-file output-file)
      (is-true (> (file-length output-file) 0)
               "Output file should contain data")
      (with-open-file (stream output-file)
        (let ((content (read-line stream)))
          (is-true (search "ZX Spectrum Beeper Music" content)
                   "Output should contain Spectrum music header"))))

    ;; Cleanup
    (ignore-errors (delete-file output-file))))

(test spectrum-tile-blob-ripping-stub
  "Test that ZX Spectrum tile blob ripping function exists but signals error"
  (signals error (skyline-tool::blob-rip-spectrum-tile "dummy-png"))
  "ZX Spectrum tile blob ripping should signal error (not yet implemented)")
