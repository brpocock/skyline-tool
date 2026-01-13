;; Comprehensive Skyline-Tool Converter Test Runner
;; Tests all converter functionality suites

;; Load ASDF and setup
(require :asdf)

;; Load setup script (sets up Quicklisp and loads ASDF system)
(load "SkylineTool/setup.lisp")

;; Ensure ASDF can find the system
(asdf:load-asd "SkylineTool/skyline-tool.asd")

;; Run tests via ASDF test-op
(asdf:test-system :skyline-tool/test)
