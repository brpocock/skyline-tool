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

(test zx81-music-compilation-stub
  "Test that ZX81 music compilation function exists but signals error"
  (signals error (skyline-tool::compile-music-zx81 "dummy-output" "dummy-input"))
  "ZX81 music compilation should signal error (not yet implemented)")

(test zx81-tile-blob-ripping-stub
  "Test that ZX81 tile blob ripping function exists but signals error"
  (signals error (skyline-tool::blob-rip-zx81-tile "dummy-png"))
  "ZX81 tile blob ripping should signal error (not yet implemented)")
