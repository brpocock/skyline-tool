(cl:defpackage :skyline-tool
  (:use :cl :alexandria :serapeum :split-sequence
   :local-time :cl-change-case :parse-number :bordeaux-threads)
  (:import-from :uiop
                #:run-program
                #:split-string)
  (:shadow #:range)
  (:shadowing-import-from :serapeum #:partition)
  (:export #:compile-index
           #:collect-assets
           #:compile-art
           #:compile-critters
           #:compile-map
           #:compile-sound
           #:compile-music
           #:compile-music-for-machine
           #:freq<-midi-key
           #:key<-midi-key
           #:midi-key<-freq
           #:midi->note-name
           #:note->midi-note-number
           #:midi->7800-tia
           #:array<-7800-tia-notes-list
           #:compile-script
           #:command
           #:build-banking
           #:c
           #:bye
           #:about-skyline-tool
           #:allocate-assets
           #:generate-all-copybooks
           #:generate-constants-copybook
           #:generate-variables-copybook
           #:generate-class-copybook
           #:atari800-label-file
           #:blob-rip-5200-tile
           #:blob-rip-7800
           #:7800-image-to-160a
           #:7800-image-to-320a
           #:7800-image-to-320c
           #:parse-7800-object
           #:write-7800-binary
           #:interleave-7800-bytes
           #:speech-supported-p
           #:simple-animation-sequence
           #:machine-directory-name
           #:generate-secure-random-id
           #:burn-rom
           #:check-for-absent-assets
           #:compile-animation-sequences
           #:compile-art-7800
           #:compile-code
           #:compile-enemies
           #:compile-font-command
           #:compile-forth
           #:compile-item-drops
           #:compile-midi
           #:compile-obj
           #:compile-shops
           #:compile-tileset
           #:collect-strings
           #:extract-tileset-palette
           #:gui
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
(defun project-root ()
  "Return the project root directory as a pathname"
  (asdf:system-relative-pathname :skyline-tool #p"../"))

(defvar *project.json*)
(defvar *game-title*)
(defvar *part-number*)
(defvar *studio*)
(defvar *publisher*)
(defvar *machine*)
(defvar *sound*)
(defvar *common-palette*)
(defvar *default-skin-color*)
(defvar *default-hair-color*)
(defvar *default-clothes-color*)
(defvar *region*)


(defun generated-file-path (filename)
  "Return the platform-specific path for a generated file."
  (let ((platform-dir (format nil "~d" *machine*)))
    (merge-pathnames (make-pathname :directory (list :relative "Source" "Generated" platform-dir)
                                    :name (pathname-name filename)
                                    :type (pathname-type filename))
                     (project-root))))

