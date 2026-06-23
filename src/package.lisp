(cl:defpackage :skyline-tool
  (:use :cl :alexandria :serapeum :split-sequence
   :local-time :cl-change-case :parse-number :bordeaux-threads)
  (:import-from :uiop
                #:run-program
                #:split-string)
  (:shadow #:range)
  (:shadowing-import-from :serapeum #:partition)
  ;; Export ONLY external call points. Unit tests will have to use ::
  (:export #:command
           #:c
           #:about-skyline-tool
           #:run-for-port
           #:run-gui
           #:run-repl))

(in-package :skyline-tool)

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
(defparameter *region* :ntsc
  "Default TV standard for palette and color conversion (:ntsc, :pal, :secam).")


(defun generated-file-path (filename)
  "Return the platform-specific path for a generated file."
  (let ((platform-dir (format nil "~d" *machine*)))
    (merge-pathnames (make-pathname :directory (list :relative "Source" "Generated" platform-dir)
                                    :name (pathname-name filename)
                                    :type (pathname-type filename))
                     (uiop:getcwd))))

