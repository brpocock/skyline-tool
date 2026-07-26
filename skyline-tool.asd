(in-package :cl-user)
(require 'asdf)

(asdf:defsystem :skyline-tool
  :description "A tool for building tile-based adventure games for 8-bit systems"
  :author "Bruce-Robert Pocock"
  :version "0.9.2"
  :maintainer "Bruce-Robert Pocock"
  :mailto "brpocock+skyline@interworldly.com"
  :licence "MIT"
  :long-name "The Skyline tools for building ARPG's for various machines"
  
  :depends-on (
               :alexandria
               :bordeaux-threads
               :cl-base64
               :cl-change-case
               :cl-fad
               :cl-json
               :cl-ppcre
               :clim-debugger
               :clim-listener
               :climacs
               :clods-export
               :clouseau
               :cserial-port
               :cffi
               :drakma
               :dufy
               :eightbol
               :eventbus
               :fiveam
               :hunchentoot  
               :inotify  
               :ironclad
               :local-time
               :lparallel
               :mcclim
               :midi
               :parse-number
               :png-read
               :zpng
               :quicklisp-slime-helper
               :replic
               :serapeum
               :swank
               :trivial-backtrace
               :trivial-gray-streams
               :xmls
               :yacc
               :zip
               :journal
               
               :eightbol
               )
  :encoding :utf-8
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "base91" :depends-on ("package"))
                 (:file "machines" :depends-on ("package"))
                 (:file "7800gd-debug" :depends-on ("package"))
                 (:file "7800gd-interface" :depends-on ("package" "eprom"))
                 (:file "printer-utils" :depends-on ("package"))
                 (:file "p2p-avahi" :depends-on ("package" "thread-pool" "interface"))
                 (:file "p2p-dlna" :depends-on ("package" "p2p-avahi"))
                 (:file "p2p-webdav" :depends-on ("package" "p2p-avahi"))
                 (:file "avahi-handler" :depends-on ("package" "p2p-avahi" "printer-utils"))
                 (:file "animation-editor" :depends-on ("package" "decode-animation-buffers"
                                                                  "printer-utils" "p2p-avahi"))
                 (:file "asset-allocator" :depends-on ("package" "maps" "graphics"
                                                                 "version-control"))
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
                 (:file "eventbus" :depends-on ("package"))
                 (:file "forth" :depends-on ("package" "fountain" "interface"))
                 (:file "fountain" :depends-on ("package" "maps"))
                 (:module "gui"
                  :depends-on ("package" "clim-simple-echo" "ps-utils" "printer-utils"
                                         "eventbus" "game-resource")
                  :components ((:file "gui-thread")
                               (:file "gui-threads")
                               (:file "gui-style")
                               (:file "gui-inspector")
                               (:file "gui-presentations")
                               (:file "gui-dialogs")
                               (:file "gui-atari-vox-dictionary")
                               (:file "gui-basic-routine")
                               (:file "gui-blob")
                               (:file "gui-boat")
                               (:file "gui-character")
                               (:file "gui-class")
                               (:file "gui-cobol-routine")
                               (:file "gui-validation")
                               (:file "gui-flag")
                               (:file "gui-forth-script")
                               (:file "gui-instrument")
                               (:file "gui-intellivoice-dictionary")
                               (:file "gui-item")
                               (:file "gui-key")
                               (:file "gui-magic-desk-dictionary")
                               (:file "gui-map")
                               (:file "gui-object-prototype")
                               (:file "gui-pascal-routine")
                               (:file "gui-phrasebook")
                               (:file "gui-preferences")
                               (:file "gui-project")
                               (:file "gui-script")
                               (:file "gui-song")
                               (:file "gui-sprite-sheet")
                               (:file "gui-terminal-echo")
                               (:file "gui-tileset")
                               (:file "help-about-dialog")
                               (:file "keyboard-shortcuts")))
                 (:module "graphics"
                  :depends-on ("package" "prototypes" "misc" "utils")
                  :components (
                               (:file "dispatch" :depends-on ())
                               (:file "dispatch-extras" :depends-on ("dispatch"))
                               (:file "palette")
                               (:file "chaos" :depends-on ("palette"))
                               (:file "font-compile" :depends-on ("palette"))
                               (:file "misc-graphics" :depends-on ("palette" "font-compile"))
                               (:file "pixel-utils" :depends-on ("palette" "misc-graphics"))
                               (:file "color-tools" :depends-on ("palette"))
                               (:file "platform-2600" :depends-on ("palette" "misc-graphics"
                                                                             "chaos"))
                               (:file "platform-2600-extras" :depends-on ("palette"
                                                                          "misc-graphics"
                                                                          "platform-2600"))
                               (:file "platform-7800" :depends-on ("palette" "misc-graphics"))
                               (:file "platform-7800-extras" :depends-on ("palette"
                                                                          "misc-graphics"
                                                                          "platform-7800"))
                               (:file "platform-intv" :depends-on ("palette" "misc-graphics"))
                               (:file "platform-intv-extras" :depends-on ("palette" "misc-graphics"
                                                                                    "platform-intv"))
                               (:file "platform-vic2" :depends-on ("palette" "misc-graphics"))
                               (:file "platform-lynx" :depends-on ("palette" "misc-graphics"))
                               (:file "platform-snes" :depends-on ("palette" "misc-graphics"))
                               (:file "platform-nes" :depends-on ("palette" "misc-graphics"))
                               (:file "platform-tms9918a" :depends-on ("palette"
                                                                       "misc-graphics"))
                               (:file "platform-tg16" :depends-on ("palette" "misc-graphics"))
                               (:file "platform-sms" :depends-on ("palette" "misc-graphics"))
                               (:file "platform-gb" :depends-on ("palette" "misc-graphics"))
                               (:file "platform-a2gs" :depends-on ("palette" "misc-graphics"))))
                 (:file "i18n-l10n" :depends-on ("package"))
                 
                 (:file "interface" :depends-on ("package" "asset-allocator" "oops" "machines"
                                                           "cbm-tooling" "launcher" "thread-pool"
                                                           "ps-utils"))
                 (:file "preferences" :depends-on ("package"))
                 (:file "game-resource" :depends-on ("package" "version-control"))
                 (:file "local-locale" :depends-on ("package"))
                 (:file "context-menu" :depends-on ("package"))
                 (:file "p2p-sharing" :depends-on ("package"))
                 (:file "all-resources" :depends-on ("package" "asset-allocator" "ps-utils"
                                                               "printer-utils" "clim-simple-echo"
                                                               "game-resource" "thread-pool"
                                                               "context-menu"  "p2p-sharing"
                                                               "gui"))
                 (:file "launcher" :depends-on ("package" "preferences" "all-resources"
                                                          "clim-simple-echo" "ps-utils"
                                                          "printer-utils" "logging"))
                 (:file "listings" :depends-on ("package"))
                 (:file "ps-utils" :depends-on ("package"))
                 (:file "maps" :depends-on ("package" "prototypes"))
                 (:file "misc" :depends-on ("package"))
                 (:file "music" :depends-on ("package"))
                 (:file "globals-copybook" :depends-on ("package" "asset-allocator"))
                 (:file "oops" :depends-on ("package" "globals-copybook"))
                 (:file "peek" :depends-on ("package"))
                 (:file "scavengers" :depends-on ("package" "game-resource" "tables" "logging"))
                 (:file "runner" :depends-on ("package" "ps-utils" "interface"
                                                        "clim-simple-echo"))
                 (:file "sega-constants" :depends-on ("package"))
                 (:file "tables" :depends-on ("package"))
                 (:file "thread-pool" :depends-on ("package"))
                 (:file "thumbnails" :depends-on ("package" "graphics" "maps"))
                 (:file "item-chooser" :depends-on ("package"))
                 (:file "threed" :depends-on ("package"))
                 (:file "utils" :depends-on ("package"))
                 (:file "utilities" :depends-on ("package"))
                 (:file "logging" :depends-on ("package"))
                 (:module "version-control"
                  :depends-on ("package")
                  :components ((:file "package")
                               (:file "config" :depends-on ("package"))
                               (:file "presentation-utils" :depends-on ("package" "config"
                                                                                  "backends"))
                               (:module "backends"
                                :depends-on ("package")
                                :components ((:file "backend-git")
                                             (:file "backend-svn")
                                             (:file "backend-rcs")
                                             (:file "backend-bazaar")
                                             (:file "backend-mercurial")
                                             (:file "backend-cvs")))))
                 (:module "issue-tracking"
                  :depends-on ("package")
                  :components ((:file "package")
                               (:file "embedded-list" :depends-on ("package"))
                               (:module "clients"
                                :components ((:file "bugzilla")
                                             (:file "github")
                                             (:file "gitlab"))))))))
  :in-order-to ((asdf:test-op (asdf:test-op #:skyline-tool/test))))

;; Separate test system

(asdf:defsystem #:skyline-tool/test
  :description "Tests for Skyline-Tool"
  :author "Bruce-Robert Pocock"
  :version "0.9.2"
  :depends-on (:skyline-tool)
  :defsystem-depends-on (:asdf)
  :components
  ((:module "tests"
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
                 (:file "music-new-features-tests"
                  :depends-on ("package" "music-compilation-tests"))
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
                 (:file "zx81-tests" :depends-on ("package")))))
  :perform (asdf:test-op (o c)
                         (assert (uiop:symbol-call :fiveam :run-all-tests :summary :end))))
