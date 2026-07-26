;;; SkylineTool src/dynamics.lisp

(in-package :skyline-tool)

;; Global variables set by load-project.json
(defvar *project.json* nil)
(defvar *studio* nil)
(defvar *publisher* nil)
(defvar *part-number* nil)
(defvar *game-title* nil
  "Store current game title for PDF headers and filenames.")
(defvar *machine* nil
  "Current machine identifier.")
(defvar *region* nil
  "Target region")
(defvar *sound* nil
  "Sound configuration")
(defvar *common-palette* nil
  "Common palette colors")
(defvar *default-skin-color* nil
  "Default skin color")
(defvar *default-hair-color* nil
  "Default hair color")
(defvar *default-clothes-color* nil
  "Default clothes color")
(defvar *build* :demo
  "Current build type: :demo, :public, :publisher")
