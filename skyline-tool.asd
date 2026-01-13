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

  :serial t

  :components
  (;;(:file "gray-streams-pipe")
   (:module "src"
    :components ((:file "package")
                 (:file "utils")
                 (:file "misc")
                 (:file "clim-simple-echo")
                 (:file "assembly")
                 (:file "7800gd-debug")
                 (:file "7800gd-interface"
                  :depends-on ("7800gd-debug" "eprom"))
                 (:file "music")
                 (:file "eprom")
                 (:file "forth")
                 (:file "fountain")
                 (:file "graphics")
                 (:file "maps")
                 (:file "oops")
                 (:file "i18n-l10n")
                 (:file "listings")
                 (:file "decode-animation-buffers")
                 (:file "decode-header"
                  :depends-on ("peek"))
                 (:file "decode-decal"
                  :depends-on ("peek"))
                 (:file "decode-map")
                 (:file "decode-object")
                 (:file "peek")
                 (:file "runner")
                 (:file "animation-editor")
                 (:file "launcher")
                 (:file "skylisp")
                 (:file "tables")
                 (:file "threed")
                 (:file "asset-allocator")
                 (:file "atarivox")
                 (:file "interface")))))

;; Separate test system
(asdf:defsystem :skyline-tool/test
  :description "Tests for Skyline-Tool"
  :author "Bruce-Robert Pocock"
  :version "0.9.0"
  :depends-on (:skyline-tool :fiveam)
  :components ((:module "tests"
                :components ((:file "display-list-tests")
                             (:file "action-tests")
                             (:file "text-transcription-tests")
                             (:file "animation-preview-tests")
                             (:file "graphics-tests")
                             (:file "build-tests")
                             (:file "interface-tests")
                             (:file "5200-tests")
                             (:file "makefile-functions-tests")
                             (:file "music-tests")
                             (:file "lynx-graphics-tests")
                             (:file "basic-tests")))))
