(in-package :cl-user)
(require 'asdf)
(asdf:defsystem :skyline-tool
  :description "A tool for building tile-based adventure games for 8-bit systems"
  :author "Bruce-Robert Pocock"
  :version "0.9.0"
  :maintainer "Bruce-Robert Pocock"
  :mailto "brpocock+skyline@star-hope.org"
  :licence "MIT" ; if this poses a problem, ask me for a waiver.
  :long-name "The Skyline tools for building ARPG's for Atari 2600 and more"

  :depends-on ( ;; broken into lines for easier sorting
               :alexandria
               :bordeaux-threads
               :cl-6502
               :cl-base64
               :cl-change-case
               :cl-json
               :cl-ppcre
               :clim-listener
               :clim-debugger
               :clods-export
               :climacs
               :clouseau
               :cserial-port
               :dufy
               :fiveam
               :ironclad
               :mcclim
               :lparallel
               :local-time
               :midi
               :parse-number
               :png-read
               :quicklisp-slime-helper
               :replic
               :split-sequence
               :swank
               :trivial-backtrace
               :trivial-gray-streams
               :xmls
               :yacc
               :zip
               )

  :encoding :utf-8

  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "utils" :depends-on ("package"))
                 (:file "misc" :depends-on ("package"))
                 (:file "clim-simple-echo" :depends-on ("package"))
                 (:file "assembly" :depends-on ("package"))
                 (:file "7800gd-debug" :depends-on ("package"))
                 (:file "7800gd-interface"
                  :depends-on ("7800gd-debug" "eprom" "package"))
                 (:file "music" :depends-on ("package"))
                 (:file "eprom" :depends-on ("package"))
                 (:file "forth" :depends-on ("package"))
                 (:file "fountain" :depends-on ("package"))
                 (:file "graphics" :depends-on ("package"))
                 (:file "maps" :depends-on ("package"))
                 (:file "oops" :depends-on ("package"))
                 (:file "i18n-l10n" :depends-on ("package"))
                 (:file "listings" :depends-on ("package"))
                 (:file "decode-animation-buffers" :depends-on ("package"))
                 (:file "decode-header"
                  :depends-on ("peek" "package"))
                 (:file "decode-decal"
                  :depends-on ("peek" "package"))
                 (:file "decode-map" :depends-on ("package"))
                 (:file "decode-object" :depends-on ("package"))
                 (:file "peek" :depends-on ("package"))
                 (:file "runner" :depends-on ("package"))
                 (:file "animation-editor" :depends-on ("package"))
                 (:file "launcher" :depends-on ("package"))
                 (:file "skylisp" :depends-on ("package"))
                 (:file "tables" :depends-on ("package"))
                 (:file "threed" :depends-on ("package"))
                 (:file "asset-allocator" :depends-on ("package"))
                 (:file "atarivox" :depends-on ("package"))
                 (:file "interface" :depends-on ("package"))))))

;; Separate test system
(asdf:defsystem :skyline-tool/test
  :description "Tests for Skyline-Tool"
  :author "Bruce-Robert Pocock"
  :version "0.9.0"
  :depends-on (:skyline-tool :fiveam)
  :defsystem-depends-on (:asdf)
  :components ((:module "tests"
                :components ((:file "package")  ;; Package definition must be first
                             (:file "action-tests" :depends-on ("package"))
                             (:file "display-list-tests" :depends-on ("package"))
                             (:file "text-transcription-tests" :depends-on ("package"))
                             (:file "animation-preview-tests" :depends-on ("package"))
                             (:file "graphics-tests" :depends-on ("package"))
                             (:file "music-compilation-tests" :depends-on ("package"))
                             (:file "build-tests" :depends-on ("package"))
                             (:file "interface-tests" :depends-on ("package"))
                             (:file "5200-tests" :depends-on ("package"))
                             (:file "7800-tests" :depends-on ("package"))
                             (:file "colecovision-tests" :depends-on ("package"))
                             (:file "intv-gram-tests" :depends-on ("package"))
                             (:file "nes-tests" :depends-on ("package"))
                             (:file "snes-tests" :depends-on ("package")))))
  :perform (asdf:test-op (o c)
             (uiop:symbol-call :fiveam :run! :skyline-tool/test)))
