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
  (   (:module "src"
    :components ((:file "package")
                 (:file "utils" :depends-on ("package"))
                 (:file "misc" :depends-on ("package"))
                 (:file "clim-simple-echo" :depends-on ("package"))
                 (:file "assembly" :depends-on ("package"))
                 (:file "7800gd-debug" :depends-on ("package"))
                 (:file "7800gd-interface" :depends-on ("package" "7800gd-debug" "eprom"))
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
                 (:file "decode-header" :depends-on ("package" "peek"))
                 (:file "decode-decal" :depends-on ("package" "peek"))
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
  :version "0.9.1"
  :depends-on (:skyline-tool :fiveam)
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "display-list-tests" :depends-on ("package"))
                             (:file "action-tests" :depends-on ("package"))
                             (:file "text-transcription-tests" :depends-on ("package"))
                             (:file "animation-preview-tests" :depends-on ("package"))
                             (:file "graphics-tests" :depends-on ("package"))
                             (:file "build-tests" :depends-on ("package"))
                             (:file "interface-tests" :depends-on ("package"))
                             (:file "5200-tests" :depends-on ("package"))
                             (:file "makefile-functions-tests" :depends-on ("package"))
                             (:file "music-tests" :depends-on ("package"))
                             (:file "lynx-graphics-tests" :depends-on ("package"))
                             (:file "basic-tests" :depends-on ("package"))
                             (:file "compiler-tests" :depends-on ("package"))
                             (:file "speech-filter-test" :depends-on ("package"))
                             (:file "multiplatform-tests" :depends-on ("package"))
                             (:file "intv-gram-tests" :depends-on ("package")))))
  :perform (asdf:test-op (o c)
             (funcall (intern "RUN!" :fiveam))))
