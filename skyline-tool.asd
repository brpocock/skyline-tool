(in-package :cl-user)
(require 'asdf)
(asdf:defsystem :skyline-tool
  :description "A tool for building tile-based adventure games for 8-bit systems"
  :author "Bruce-Robert Pocock"
  :version "0.9.1"
  :maintainer "Bruce-Robert Pocock"
  :mailto "brpocock+skyline@star-hope.org"
  :licence "MIT" ; if this poses a problem, ask me for a waiver.
  :long-name "The Skyline tools for building ARPG's for various machines"
  
  :depends-on ( ;; broken into lines for easier sorting
               
               :alexandria
               :bordeaux-threads
               :cl-base64
               :cl-change-case
               :cl-json
               :cl-ppcre
               :clim-debugger
               :clim-listener
               :climacs
               :clods-export
               :clouseau
               :cserial-port
               :dufy
               :fiveam
               :ironclad
               :local-time
               :lparallel
               :mcclim
               :midi
               :parse-number
               :png-read
               :quicklisp-slime-helper
               :replic
               :serapeum               
               :swank
               :trivial-backtrace
               :trivial-gray-streams
               :xmls
               :yacc
               :zip
               
               :eightbol
               )
  :encoding :utf-8
  :components
  ((:module "src"
    :components (
                 (:file "7800gd-debug" :depends-on ("package"))
                 (:file "7800gd-interface" :depends-on ("7800gd-debug" "eprom" "package"))
                 (:file "animation-editor" :depends-on ("package" "decode-animation-buffers"))
                 (:file "asset-allocator" :depends-on ("package" "maps"))
                 (:file "cbm-tooling" :depends-on ("package"))
                 (:file "atarivox" :depends-on ("package" "runner"))
                 (:file "clim-simple-echo" :depends-on ("package"))
                 (:file "decode-animation-buffers" :depends-on ("package"))
                 (:file "decode-decal" :depends-on ("peek" "decode-object"))
                 (:file "decode-header" :depends-on ("peek" "package"))
                 (:file "decode-map" :depends-on ("package"))
                 (:file "decode-object" :depends-on ("package" "clim-simple-echo"))
                 (:file "prototypes" :depends-on ("package" "decode-object"))
                 (:file "eprom" :depends-on ("package"))
                 (:file "forth" :depends-on ("package" "fountain" "interface"))
                 (:file "fountain" :depends-on ("package" "maps"))
                 (:module "graphics"
                  :depends-on ("package" "prototypes" "misc" "utils")
                  :components ((:file "palette")
                               (:file "chaos" :depends-on ("palette"))
                               (:file "font-compile" :depends-on ("palette"))
                               (:file "misc-graphics" :depends-on ("palette" "font-compile"))
                               (:file "pixel-utils" :depends-on ("palette" "misc-graphics"))
                               (:file "color-tools" :depends-on ("palette"))
                               (:file "platform-2600" :depends-on ("palette" "misc-graphics" "chaos"))
                               (:file "platform-2600-extras" :depends-on ("palette" "misc-graphics" "platform-2600"))
                               (:file "platform-7800" :depends-on ("palette" "misc-graphics"))
                               (:file "platform-7800-extras" :depends-on ("palette" "misc-graphics" "platform-7800"))
                               (:file "platform-intv" :depends-on ("palette" "misc-graphics"))
                               (:file "platform-intv-extras" :depends-on ("palette" "misc-graphics" "platform-intv"))
                               (:file "platform-vic2" :depends-on ("palette" "misc-graphics"))
                               (:file "platform-lynx" :depends-on ("palette" "misc-graphics"))
                               (:file "platform-snes" :depends-on ("palette" "misc-graphics"))
                               (:file "platform-nes" :depends-on ("palette" "misc-graphics"))
                               (:file "platform-tms9918a" :depends-on ("palette" "misc-graphics"))
                               (:file "platform-tg16" :depends-on ("palette" "misc-graphics"))
                               (:file "platform-sms" :depends-on ("palette" "misc-graphics"))
                               (:file "platform-gb" :depends-on ("palette" "misc-graphics"))
                               (:file "platform-a2gs" :depends-on ("palette" "misc-graphics"))
                               (:file "dispatch" :depends-on ("palette" "font-compile" "misc-graphics" "platform-2600" "platform-2600-extras" "platform-7800" "platform-7800-extras" "platform-intv" "platform-intv-extras" "platform-vic2" "platform-lynx" "platform-snes" "platform-nes" "platform-tms9918a" "platform-tg16" "platform-sms" "platform-gb" "platform-a2gs"))
                               (:file "dispatch-extras" :depends-on ("dispatch"))))
                 (:file "i18n-l10n" :depends-on ("package"))
                 (:file "interface" :depends-on ("package" "asset-allocator" "oops" "cbm-tooling"))
                 (:file "launcher" :depends-on ("package"))
                 (:file "listings" :depends-on ("package"))
                 (:file "maps" :depends-on ("package" "prototypes"))
                 (:file "misc" :depends-on ("package"))
                 (:file "music" :depends-on ("package"))
                 (:file "globals-copybook" :depends-on ("package" "asset-allocator"))
                 (:file "oops" :depends-on ("package" "globals-copybook"))
                 (:file "package")
                 (:file "peek" :depends-on ("package"))
                 (:file "runner" :depends-on ("package"))
                 (:file "sega-constants" :depends-on ("package"))
                 (:file "tables" :depends-on ("package"))
                 (:file "threed" :depends-on ("package"))
                 (:file "utils" :depends-on ("package"))
                 )))
  :in-order-to ((asdf:test-op (asdf:test-op #:skyline-tool/test))))

;; Separate test system
(asdf:defsystem #:skyline-tool/test
  :description "Tests for Skyline-Tool"
  :author "Bruce-Robert Pocock"
  :version "0.9.1"
  :depends-on (:skyline-tool)
  :defsystem-depends-on (:asdf)
  :components ((:module "tests"
                :components (
                             (:file "5200-tests" :depends-on ("package"))
                (:file "7800-tests" :depends-on ("package"))
                             (:file "animation-preview-tests" :depends-on ("package"))
                             (:file "build-tests" :depends-on ("package"))
                             (:file "cbm-tooling-tests" :depends-on ("package"))
                             (:file "cdr-tests" :depends-on ("package"))
                             (:file "colecovision-tests" :depends-on ("package"))
                             (:file "display-list-tests" :depends-on ("package"))
                             (:file "graphics-tests" :depends-on ("package"))
                             (:file "interface-tests" :depends-on ("package"))
                             (:file "intv-gram-tests" :depends-on ("package"))
                             (:file "lynx-tests" :depends-on ("package"))
                             (:file "lynx-cart-tests" :depends-on ("package"))
                             (:file "multiplatform-tests" :depends-on ("package"))
                              (:file "music-compilation-tests" :depends-on ("package"))
                              (:file "music-tests" :depends-on ("package" "test-data-generators"))
                              (:file "music-new-features-tests" :depends-on ("package" "music-compilation-tests"))
                              (:file "near-term-makefile-parse-tests" :depends-on ("package" "interface-tests"))
                             (:file "nes-tests" :depends-on ("package"))
                             (:file "package" :depends-on ("test-data-generators"))
                             (:file "prototype-tests" :depends-on ("package"))
                             (:file "scroll-tests" :depends-on ("package"))
                             (:file "sega-tests" :depends-on ("package" "test-data-generators"))
                             (:file "snes-tests" :depends-on ("package"))
                             (:file "spectrum-tests" :depends-on ("package"))
                             (:file "speech-filter-test" :depends-on ("package"))
                             (:file "test-data-generators")
                             (:file "text-transcription-tests" :depends-on ("package"))
                             (:file "tileset-tests" :depends-on ("package"))
                             (:file "zx81-tests" :depends-on ("package"))
                             )))
  :perform (asdf:test-op (o c)
                         (assert (uiop:symbol-call :fiveam :run-all-tests
                                                   :summary :end))))
