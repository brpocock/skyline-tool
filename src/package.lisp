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
           #:command
           #:build-banking
           #:c
           #:bye))

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

(defparameter *all-builds*
  (remove-if #'null
             (list (when *publisher* "AA")
                   (when (assoc :*demo *project.json*) "Demo")
                   "Public")))

(defvar *region* :ntsc)
