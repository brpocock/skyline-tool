;;; ZX81 platform tests

(in-package :skyline-tool/test)

(test zx81-platform-existence
  "Test that ZX81 platform constants and functions exist"
  (is-true (fboundp 'skyline-tool::machine-short-name))
  (is-true (fboundp 'skyline-tool::machine-long-name))
  (is-true (fboundp 'skyline-tool::machine-directory-name)))

(test zx81-art-compilation-stub
  "Test that ZX81 art compilation function exists but signals error"
  (signals error (skyline-tool::compile-art-zx81 "dummy-index-out" "dummy-index-in"))
  "ZX81 art compilation should signal error (not yet implemented)")

(test zx81-music-compilation-validation
  "Test ZX81 music compilation produces correct assembly output"
  (let ((output-file (format nil "Object/81/test-music-~a.s" (skyline-tool::generate-secure-random-id 2)))
        (input-file (format nil "/tmp/test-music-~a.mid" (get-universal-time))))
    (ensure-directories-exist (pathname (directory-namestring output-file)))
    ;; Create minimal input file (ZX81 method does not read it, only uses name in header)
    (unless (probe-file input-file)
      (with-open-file (f input-file :direction :output :if-does-not-exist :create
                                 :element-type '(unsigned-byte 8))
        (write-byte 0 f)))
    ;; Test that function exists and can be called
    (is-true (fboundp 'skyline-tool::compile-music-zx81)
             "compile-music-zx81 should exist")

    ;; Test compilation with dummy input
    (finishes (skyline-tool::compile-music-zx81 output-file input-file)
              "compile-music-zx81 should complete without throwing errors")

    ;; Validate output file was created and has expected content
    (when (probe-file output-file)
      (with-open-file (stream output-file)
        (is-true (> (file-length stream) 0)
                 "Output file should contain data")
        (file-position stream 0)
        (let ((content (read-line stream)))
          (is-true (search "ZX81 EAR Music" content)
                   "Output should contain ZX81 music header"))))

    ;; Cleanup
    (ignore-errors (delete-file output-file))))

(test zx81-tile-blob-ripping-stub
  "Test that ZX81 tile blob ripping function exists but signals error"
  (signals error (skyline-tool::blob-rip-zx81-tile "dummy-png"))
  "ZX81 tile blob ripping should signal error (not yet implemented)")
