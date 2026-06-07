;;; Phantasia SkylineTool/tests/build-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

(in-package :skyline-tool/test)

(def-suite build-tests
  :description "Regression tests for build system and compilation issues"
  :in skyline-tool/test)

(in-suite build-tests)

;; Test that core functions have documentation
(test function-documentation
  "Test that core functions have documentation"
  (let ((core-functions '(skyline-tool::blob-rip-7800
                          skyline-tool::compile-art-7800
                          skyline-tool::compile-map
                          skyline-tool::compile-script)))
    (dolist (func core-functions)
      (when (fboundp func)
        (is (documentation func 'function)
            "~A should have documentation" func)))))

;; Test build system stability
(test build-system-stability
  "Test that the build system components are stable"
  (is (stringp (asdf:system-description (asdf:find-system :skyline-tool)))
      "System description should be a string"))

#+()

;; Regression tests for build system issues
(test generated-art-assets-exist
  "Test that generated art asset files exist after build"
  ;; This prevents recurrence of missing Art.UI.o assembler errors
  (let ((art-files '("Object/Assets/Art.Font.o" "Object/Assets/Art.UI.o")))
    (dolist (art-file art-files)
      (is-true (probe-file art-file)
               (format nil "Generated art asset ~a should exist" art-file)))))

#+()
(test generated-palette-files-exist
  "Test that generated palette files exist after build"
  ;; This prevents recurrence of missing palette file issues
  (let ((palette-files '("Source/Generated/AncientPalette.s"
                         "Source/Generated/ArturosPalette.s"
                         "Source/Generated/CityscapePalette.s")))
    (dolist (palette-file palette-files)
      (is-true (probe-file palette-file)
               (format nil "Generated palette file ~a should exist" palette-file)))))

#+()
(test generated-makefile-syntax-valid
  "Test that generated Makefiles have valid syntax"
  ;; This prevents recurrence of malformed generated Makefiles
  (let ((makefile-path "Source/Generated/Makefile"))
    (is-true (probe-file makefile-path)
             "Generated Makefile should exist")
    (when (probe-file makefile-path)
      (with-open-file (stream makefile-path)
        (let ((content (make-string (file-length stream))))
          (read-sequence content stream)
          (is (check-delimiter-balance content)
              "Generated Makefile should have balanced delimiters"))))))

#+()
(test build-dependencies-tracked
  "Test that build dependencies are properly tracked"
  ;; This prevents issues where source files change but objects aren't rebuilt
  (let ((source-files '("Source/Art/UI.art" "Source/Art/DrawUI.png"))
        (object-files '("Object/Assets/Art.UI.o")))
    ;; All source files should exist
    (dolist (src source-files)
      (is-true (probe-file src)
               (format nil "Source dependency ~a should exist" src)))
    ;; Object files should exist after build
    (dolist (obj object-files)
      (is-true (probe-file obj)
               (format nil "Object file ~a should be built" obj)))))

(defun run-build-tests ()
  "Run all build tests and return results"
  (fiveam:run! 'build-tests))
