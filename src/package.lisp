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

;; Robust JSON key accessor supporting CamelCase (preferred), lowercase, and legacy starred keys
(defun %json-get (table &rest keys)
  (labels ((lookup (k)
             (cond
               ((hash-table-p table)
                (multiple-value-bind (val foundp) (gethash k table)
                  (when foundp val)))
               ((listp table)
                (let ((cell (assoc k table :test #'equal)))
                  (when cell (cdr cell))))
               (t nil))))
    (or (some #'lookup keys)
        (let* ((alts (loop for k in keys append
                            (cond
                              ((stringp k) (list (string-downcase k) (string-upcase k)))
                              ((symbolp k)
                               (let ((s (string k)))
                                 (list (intern s :keyword)
                                       (intern (string-downcase s) :keyword)
                                       (intern (string-upcase s) :keyword)
                                       (string s) (string-downcase s) (string-upcase s))))
                              (t (list k))))))
          (some #'lookup alts)))))

(defparameter *game-title* (%json-get *project.json* :Game :game :*game))
(defparameter *part-number*  (%json-get *project.json* :PartNumber :partnumber :*part-number))
(defparameter *studio* (%json-get *project.json* :Studio :studio :*studio))
(defparameter *publisher* (%json-get *project.json* :Publisher :publisher :*publisher))
(defparameter *machine* (%json-get *project.json* :Machine :machine :*machine))
(defparameter *sound* (%json-get *project.json* :Sound :sound :*sound))
(defparameter *common-palette*
  (let ((v (%json-get *project.json* :CommonPalette :common-palette :*common-palette)))
    (when v (mapcar #'intern v))))
(defparameter *default-skin-color* (%json-get *project.json* :DefaultSkinColor :default-skin-color :*default-skin-color))
(defparameter *default-hair-color* (%json-get *project.json* :DefaultHairColor :default-hair-color :*default-hair-color))
(defparameter *default-clothes-color* (%json-get *project.json* :DefaultClothesColor :default-clothes-color :*default-clothes-color))

(defvar *region* :ntsc)
