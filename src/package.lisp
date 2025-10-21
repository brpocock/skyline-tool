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
           #:command
           #:build-banking
           #:c
           #:bye
           #:about-skyline-tool
           #:allocate-assets
           #:atari800-label-file
           #:blob-rip-7800
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
           #:run-repl))

(in-package :skyline-tool)

(defvar *project.json*
  '#.(json:decode-json-from-source
      (asdf:system-relative-pathname
       :skyline-tool (make-pathname :directory '(:relative :up) :name "Project" :type "json" ))))

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
