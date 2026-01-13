(cl:defpackage :skyline-tool
  (:use :cl :alexandria :split-sequence :parse-number :bordeaux-threads)
  (:import-from :uiop
                uiop:run-program
                uiop:split-string)
  (:import-from :cl-change-case
                cl-change-case:camel-case 	cl-change-case:constant-case
                cl-change-case:dot-case 	cl-change-case:header-case
                cl-change-case:lower-case 	cl-change-case:lower-case-first
                cl-change-case:no-case  	cl-change-case:param-case
                cl-change-case:pascal-case 	cl-change-case:path-case
                cl-change-case:sentence-case 	cl-change-case:snake-case
                cl-change-case:string-lower-case-p 	cl-change-case:string-upper-case-p
                cl-change-case:swap-case 	cl-change-case:title-case
                cl-change-case:upper-case 	cl-change-case:upper-case-first)
  (:export #:compile-index
           #:collect-assets
           #:compile-art
           #:compile-critters
           #:compile-map
           #:compile-sound
           #:compile-music
           #:compile-script
           #:midi->note-name
           #:note->midi-note-number
           #:best-pokey-note-for
           #:midi->2600-tia
           #:midi->7800-tia
           #:read-midi
           #:collect-midi-texts
           #:midi-track-notes-count
           #:midi-tracks-with-music
           #:freq<-midi-key
           #:command
           #:build-banking
           #:c
           #:bye
           #:about-skyline-tool
           #:allocate-assets
           #:atari800-label-file
           #:blob-rip-7800
           #:blob-rip-7800-160a
           #:blob-rip-7800-320ac
           #:7800-image-to-160a
           #:7800-image-to-320a
           #:7800-image-to-320c
           #:burn-rom
           #:check-for-absent-assets
           #:compile-animation-sequences
           #:compile-art-7800
           #:compile-art-lynx
           #:compile-code
           #:compile-enemies
           #:compile-font-command
           #:compile-forth
           #:compile-item-drops
           #:compile-midi
           #:midi-compile
           #:compile-obj
           #:compile-shops
           #:compile-tileset
           #:compile-lynx-tileset
           #:compile-lynx-sprite
           #:compile-lynx-blob
           #:dispatch-png
           #:dispatch-png%
           #:collect-strings
           #:extract-tileset-palette
           #:stamp-is-monochrome-p
           #:check-height+width-for-blob
           #:check-height+width-for-blob-320ac
           #:extract-4×16-stamps
           #:blob/write-span-to-stamp-buffer-320ac
           #:blob/write-spans-320ac
           #:gui
           #:zx7-compress
           #:labels-to-forth
           #:labels-to-mame
           #:labels-to-include
           #:make-classes-for-oops
           #:prepend-fundamental-mode
           #:push-7800gd-bin
           #:push-7800gd-bin-no-execute
           #:repl
           #:run-script
           #:play-script-on-atarivox
           #:write-actor-prototypes
           #:write-asset-bank
           #:write-asset-ids
           #:write-cart-header
           #:write-character-ids
           #:write-docks-index
           #:write-equipment-index
           #:write-flags-tables
           #:write-gimp-palettes
           #:write-inventory-tables
           #:write-keys-tables
           #:write-orchestration
           #:write-projection-tables.s
           #:write-sound-effects-file
           #:write-master-makefile
           #:run-for-port
           #:run-gui
           #:run-repl
           #:machine-long-name
           ;; Global variables (for testing)
           #:*machine*
           #:*game-title*
           #:*project.json*
           #:*invocation*
           ;; Display list debugging functions (for testing)
           #:decode-header
           #:header->string
           #:string->hex
           #:decode-dll-entry
           #:decode-dll-hex
           #:decode-display-list
           #:decode-dll-deeply
           #:detect-active-dll
           #:dl-contains-entry-p
           #:dll-can-reach-dl-entry-p))

(in-package :skyline-tool)

(defvar *project.json*
  '#.(json:decode-json-from-source
      (asdf:system-relative-pathname
       :skyline-tool (make-pathname :directory '(:relative :up) :name "Project" :type "json" ))))

(defparameter *game-title* nil)
(defparameter *part-number* nil)
(defparameter *studio* nil)
(defparameter *publisher* nil)
(defparameter *machine* nil)
(defparameter *sound* nil)
(defparameter *common-palette* nil)
(defparameter *default-skin-color* nil)
(defparameter *default-hair-color* nil)
(defparameter *default-clothes-color* nil)

(defvar *region* :ntsc)
