(cl:defpackage :skyline-tool
  (:use :cl :alexandria :split-sequence :parse-number :bordeaux-threads)
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

(defvar *region* :ntsc)
