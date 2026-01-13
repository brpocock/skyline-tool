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
           #:blob-rip-5200-tile
           #:blob-rip-5200-pmg
           #:detect-5200-tile-mode
           #:blob-rip-cgb-tile
           #:blob-rip-cgb-sprite
           #:blob-rip-dmg-tile
           #:blob-rip-clc-tile
           #:blob-rip-1000-tile
           #:blob-rip-sms-tile
           #:blob-rip-sgg-tile
           #:blob-rip-nes-tile
           #:blob-rip-snes-tile
           #:blob-rip-bbc-tile
           #:blob-rip-c16-tile
           #:blob-rip-a2-tile
           #:blob-rip-a3-tile
           #:blob-rip-2gs-tile
           #:7800-image-to-160a
           #:7800-image-to-320a
           #:7800-image-to-320c
           #:assemble-intv-rom
           #:burn-rom
           #:check-for-absent-assets
           #:compile-animation-sequences
           #:compile-art-7800
           #:compile-art-5200
           #:compile-art-lynx
           #:compile-art-cgb
           #:compile-art-dmg
           #:compile-art-nes
           #:compile-art-snes
           #:compile-art-colecovision
           #:compile-art-sg1000
           #:compile-art-sms
           #:compile-art-sgg
           #:compile-art-c16
           #:compile-art-a2
           #:compile-art-a3
           #:compile-art-a2gs
           #:compile-art-bbc
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
           ;; Platform-specific compilation functions
           #:compile-music-cgb #:compile-music-dmg #:compile-music-nes #:compile-music-snes
           #:compile-music-colecovision #:compile-music-sg1000 #:compile-music-sms #:compile-music-sgg
           #:compile-music-c16 #:compile-music-a2 #:compile-music-a3 #:compile-music-a2gs #:compile-music-bbc
           ;; Platform-specific blob ripping functions
           #:blob-rip-cgb-tile #:blob-rip-cgb-sprite #:blob-rip-cgb-font
           #:blob-rip-dmg-tile #:blob-rip-dmg-sprite #:blob-rip-dmg-font
           #:blob-rip-nes-tile #:blob-rip-nes-sprite #:blob-rip-nes-font
           #:blob-rip-snes-tile #:blob-rip-snes-sprite #:blob-rip-snes-font
           #:blob-rip-colecovision-tile #:blob-rip-colecovision-sprite #:blob-rip-colecovision-font
           #:blob-rip-sg1000-tile #:blob-rip-sg1000-sprite #:blob-rip-sg1000-font
           #:blob-rip-sms-tile #:blob-rip-sms-sprite #:blob-rip-sms-font
           #:blob-rip-sgg-tile #:blob-rip-sgg-sprite #:blob-rip-sgg-font
           #:blob-rip-c16-tile #:blob-rip-c16-sprite #:blob-rip-c16-font
           #:blob-rip-a2-tile #:blob-rip-a2-sprite #:blob-rip-a2-font
           #:blob-rip-a3-tile #:blob-rip-a3-sprite #:blob-rip-a3-font
           #:blob-rip-a2gs-tile #:blob-rip-a2gs-sprite #:blob-rip-a2gs-font
           #:blob-rip-bbc-tile #:blob-rip-bbc-sprite #:blob-rip-bbc-font
           ;; Platform detection functions
           #:detect-cgb-tile-mode #:detect-dmg-tile-mode #:detect-nes-tile-mode #:detect-snes-tile-mode
           #:detect-colecovision-tile-mode #:detect-sg1000-tile-mode #:detect-sms-tile-mode #:detect-sgg-tile-mode
           #:detect-c16-tile-mode #:detect-a2-tile-mode #:detect-a3-tile-mode #:detect-a2gs-tile-mode #:detect-bbc-tile-mode
           ;; Speech/phoneme compilation functions
           #:compile-speech-7800 #:compile-speech-2600 #:compile-speech-2609
           ;; Forth bytecode compilation functions
           #:compile-forth-6502 #:compile-forth-z80 #:compile-forth-cp1610
           ;; Native bytecode emission functions
           #:emit-6502-bytecode #:emit-6507-bytecode #:emit-6510-bytecode #:emit-8502-bytecode
           #:emit-65c02-bytecode #:emit-65sc02-bytecode #:emit-cp1610-bytecode
           #:emit-z80-bytecode #:emit-z80a-bytecode #:emit-huc6280-bytecode
           ;; Palette generation functions
           #:generate-ntsc-palette #:generate-pal-palette #:generate-secam-palette
           ;; Platform capability predicates
           #:speech-supported-p #:pal-capable-p #:secam-capable-p
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

(defun project-root ()
  "Return the project root directory as a pathname"
  (asdf:system-relative-pathname :skyline-tool #p"../"))

(defparameter *game-title* (cdr (assoc :*game *project.json*)))
(defparameter *part-number*  (cdr (assoc :*part-number *project.json*)))
(defparameter *studio* (cdr (assoc :*studio *project.json*)))
(defparameter *publisher* (cdr (assoc :*publisher *project.json*)))
(defparameter *machine* (cdr (assoc :*machine *project.json*)))
(defparameter *sound* (cdr (assoc :*sound *project.json*)))
(defparameter *common-palette* (mapcar #'intern (cdr (assoc :*common-palette *project.json*))))
(defparameter *default-skin-color* (cdr (assoc :*default-skin-color *project.json*)))
(defparameter *default-hair-color* (cdr (assoc :*default-hair-color *project.json*)))
(defparameter *default-clothes-color* (cdr (assoc :*default-clothes-color *project.json*)))

(defvar *region* :ntsc)
