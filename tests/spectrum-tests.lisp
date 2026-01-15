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

(test spectrum-music-compilation-stub
  "Test that ZX Spectrum music compilation function exists but signals error"
  (signals error (skyline-tool::compile-music-spectrum "dummy-output" "dummy-input"))
  "ZX Spectrum music compilation should signal error (not yet implemented)")

(test spectrum-tile-blob-ripping-stub
  "Test that ZX Spectrum tile blob ripping function exists but signals error"
  (signals error (skyline-tool::blob-rip-spectrum-tile "dummy-png"))
  "ZX Spectrum tile blob ripping should signal error (not yet implemented)")
