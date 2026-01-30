(cl:defpackage :skyline-tool
  (:use :cl :alexandria)
  (:import-from :uiop
                uiop:run-program
                uiop:split-string)
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
           #:generate-all-copybooks
           #:generate-constants-copybook
           #:generate-variables-copybook
           #:generate-class-copybook
           #:atari800-label-file
           #:blob-rip-7800
           #:blob-rip-400-tile
           #:blob-rip-800-tile
           #:burn-rom
           #:check-for-absent-assets
           #:compile-animation-sequences
           #:compile-art-7800
           #:compile-art-400
           #:compile-art-800
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
           #:write-batari-song
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

(defvar *project.json* nil)

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

(defvar *region*)


(defun generated-file-path (filename)
  "Return the platform-specific path for a generated file."
  (let ((platform-dir (format nil "~d" *machine*)))
    (merge-pathnames (make-pathname :directory (list :relative "Source" "Generated" platform-dir)
                                    :name (pathname-name filename)
                                    :type (pathname-type filename))
                     (project-root))))

