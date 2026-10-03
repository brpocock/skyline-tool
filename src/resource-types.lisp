;;; Resource Types — Canonical type hierarchy for all resources in Phantasia
;;; Single source of truth for resource classification.

(in-package :skyline-tool)

;; --- Core Structures ---

(defstruct resource-type
  "Base structure for any resource type."
  (name string :read-only t :initarg :name)
  (display-name string :read-only t :initarg :display-name)
  (description string :read-only t :initarg :description)
  (category symbol :read-only t :initarg :category)
  (source-location string :read-only t :initarg :source-location)
  (file-extensions (list string) :read-only t :initarg :file-extensions)
  (has-build-flags boolean :read-only t :initarg :has-build-flags :default nil)
  (validator-function (opt symbol) :read-only t :initarg :validator-function))

(defstruct resource-category
  "Categories of resources (Asset, Spreadsheet-Record, Loose-File-Record, etc.)"
  (name symbol :read-only t :initarg :name)
  (display-name string :read-only t :initarg :display-name)
  (description string :read-only t :initarg :description)
  (resource-types (list resource-type) :read-only t :initarg :resource-types :default nil))

;; --- Category Definitions ---

(defparameter *resource-categories*
  (list
   ;; Assets — have build flags (Demo/Public/AA), compiled into ROM
   (make-resource-category
    :name :asset
    :display-name "Assets"
    :description "Compiled assets with build flags (Demo, Public, Publisher)"
    :resource-types nil) ; populated below

   ;; Spreadsheet Records — loaded from ODS/CSV files, no build flags
   (make-resource-category
    :name :spreadsheet-record
    :display-name "Spreadsheet Records"
    :description "Data records sourced from spreadsheet files (NPCStats.ods, Boats.ods, etc.)"
    :resource-types nil)

   ;; Loose File Records — loaded from individual files in Source/
   (make-resource-category
    :name :loose-file-record
    :display-name "Loose File Records"
    :description "Individual resource files in Source/ directories"
    :resource-types nil)

   ;; Special/Computed Resources — no source file, generated or computed
   (make-resource-category
    :name :special-resource
    :display-name "Special Resources"
    :description "Generated or computed resources with no direct source file"
    :resource-types nil)))

;; --- Asset Types ---

(defparameter *asset-types*
  (list
   (make-resource-type
    :name :map
    :display-name "Map"
    :description "Tiled map files (.tmx) compiled into ROM"
    :category :asset
    :source-location "Source/Maps/<locale>/"
    :file-extensions '("tmx")
    :has-build-flags t
    :validator-function 'validate-map)

   (make-resource-type
    :name :blob
    :display-name "BLOB"
    :description "Binary large object (GIMP .xcf) compiled into ROM"
    :category :asset
    :source-location "Source/Blobs/<machine>/"
    :file-extensions '("xcf")
    :has-build-flags t
    :validator-function 'validate-blob)

   (make-resource-type
    :name :script
    :display-name "Script"
    :description "Fountain/Forth scripts compiled into ROM"
    :category :asset
    :source-location "Source/Scripts/<locale>/"
    :file-extensions '("fountain" "forth")
    :has-build-flags t
    :validator-function 'validate-script)

   (make-resource-type
    :name :song
    :display-name "Song"
    :description "MuseScore music files (.mscz) compiled into ROM"
    :category :asset
    :source-location "Source/Songs/"
    :file-extensions '("mscz")
    :has-build-flags t
    :validator-function 'validate-song)))

;; --- Spreadsheet Record Types ---

(defparameter *spreadsheet-record-types*
  (list
   (make-resource-type
    :name :character
    :display-name "Character"
    :description "NPC character data from NPCStats.ods"
    :category :spreadsheet-record
    :source-location "Source/Tables/NPCStats.ods"
    :file-extensions '("ods")
    :has-build-flags nil
    :validator-function 'validate-character)

   (make-resource-type
    :name :boat
    :display-name "Boat"
    :description "Boat data from Boats.ods"
    :category :spreadsheet-record
    :source-location "Source/Tables/Boats.ods"
    :file-extensions '("ods")
    :has-build-flags nil
    :validator-function 'validate-boat)

   (make-resource-type
    :name :class
    :display-name "Class"
    :description "COBOL class definitions from Classes.ods or Source/Classes/"
    :category :spreadsheet-record
    :source-location "Source/Tables/Classes.ods"
    :file-extensions '("ods" "cob")
    :has-build-flags nil
    :validator-function 'validate-class)

   (make-resource-type
    :name :item
    :display-name "Item"
    :description "Item names from EquipmentIndex.ods"
    :category :spreadsheet-record
    :source-location "Source/Tables/EquipmentIndex.ods"
    :file-extensions '("ods")
    :has-build-flags nil
    :validator-function 'validate-item)

   (make-resource-type
    :name :flag
    :display-name "Flag"
    :description "Flag names from Flags.txt or spreadsheet"
    :category :spreadsheet-record
    :source-location "Source/Tables/Flags.txt"
    :file-extensions '("txt" "ods")
    :has-build-flags nil
    :validator-function 'validate-flag)

   (make-resource-type
    :name :key
    :display-name "Key"
    :description "Key names from Keys.txt or spreadsheet"
    :category :spreadsheet-record
    :source-location "Source/Tables/Keys.txt"
    :file-extensions '("txt" "ods")
    :has-build-flags nil
    :validator-function 'validate-key)

   (make-resource-type
    :name :instrument
    :display-name "Instrument"
    :description "Instrument definitions from Orchestration.ods"
    :category :spreadsheet-record
    :source-location "Source/Tables/Orchestration.ods"
    :file-extensions '("ods")
    :has-build-flags nil
    :validator-function 'validate-instrument)))

;; --- Loose File Record Types ---

(defparameter *loose-file-record-types*
  (list
   (make-resource-type
    :name :routine
    :display-name "Routine"
    :description "Run Commands (COBOL/Pascal/BASIC) and Forth Scripts"
    :category :loose-file-record
    :source-location "Source/Maps/RunCommands/ or Source/Scripts/**/*.forth"
    :file-extensions '("cob" "pas" "bas" "forth")
    :has-build-flags nil
    :validator-function 'validate-routine)

   (make-resource-type
    :name :object-prototype
    :display-name "Object Prototype"
    :description "JSON object prototype definitions"
    :category :loose-file-record
    :source-location "Source/Objects/"
    :file-extensions '("json")
    :has-build-flags nil
    :validator-function 'validate-object-prototype)

   (make-resource-type
    :name :tileset
    :display-name "Tileset"
    :description "Tiled tileset files (.tsx)"
    :category :loose-file-record
    :source-location "Source/Maps/Tiles/"
    :file-extensions '("tsx")
    :has-build-flags nil
    :validator-function 'validate-tileset)

   (make-resource-type
    :name :sprite-sheet
    :display-name "Sprite Sheet"
    :description "Sprite sheet definitions (.art)"
    :category :loose-file-record
    :source-location "Source/Art/"
    :file-extensions '("art")
    :has-build-flags nil
    :validator-function 'validate-sprite-sheet)

   (make-resource-type
    :name :class-source
    :display-name "Class Source"
    :description "COBOL class source files"
    :category :loose-file-record
    :source-location "Source/Classes/"
    :file-extensions '("cob")
    :has-build-flags nil
    :validator-function 'validate-class-source)))

;; --- Special Resource Types ---

(defparameter *special-resource-types*
  (list
   (make-resource-type
    :name :atari-vox-dictionary
    :display-name "AtariVox Dictionary"
    :description "SpeakJet dictionary file"
    :category :special-resource
    :source-location "Source/SpeakJet.dic"
    :file-extensions '("dic")
    :has-build-flags nil
    :validator-function 'validate-atari-vox-dictionary)

   (make-resource-type
    :name :project-config
    :display-name "Project Config"
    :description "Machine-specific project configuration (Project.<machine>.json)"
    :category :special-resource
    :source-location "Source/Project.<machine>.json"
    :file-extensions '("json")
    :has-build-flags nil
    :validator-function 'validate-project-config)

   (make-resource-type
    :name :item-names
    :display-name "Item Names"
    :description "Computed list of all item names"
    :category :special-resource
    :source-location "(computed from EquipmentIndex.ods)"
    :file-extensions nil
    :has-build-flags nil
    :validator-function nil)

   (make-resource-type
    :name :flag-names
    :display-name "Flag Names"
    :description "Computed list of all flag names"
    :category :special-resource
    :source-location "(computed from Flags.txt)"
    :file-extensions nil
    :has-build-flags nil
    :validator-function nil)

   (make-resource-type
    :name :key-names
    :display-name "Key Names"
    :description "Computed list of all key names"
    :category :special-resource
    :source-location "(computed from Keys.txt)"
    :file-extensions nil
    :has-build-flags nil
    :validator-function nil)))

;; --- Aggregate All Types ---

(defparameter *all-resource-types*
  (append *asset-types*
          *spreadsheet-record-types*
          *loose-file-record-types*
          *special-resource-types*))

;; Populate category resource-types lists
(dolist (cat *resource-categories*)
  (setf (resource-category-resource-types cat)
        (remove-if-not (lambda (rt) (eq (resource-type-category rt) (resource-category-name cat)))
                       *all-resource-types*)))

;; --- Lookup Functions ---

(defun find-resource-type (name)
  "Find a resource type by keyword name (e.g., :map, :character)."
  (find name *all-resource-types* :key #'resource-type-name :test #'eq))

(defun find-resource-type-by-display-name (display-name)
  "Find a resource type by display name (e.g., \"Map\", \"Character\")."
  (find display-name *all-resource-types* :key #'resource-type-display-name :test #'string-equal))

(defun resource-types-for-category (category)
  "Return all resource types for a given category keyword (:asset, :spreadsheet-record, etc.)"
  (let ((cat (find category *resource-categories* :key #'resource-category-name :test #'eq)))
    (when cat
      (resource-category-resource-types cat))))

(defun all-asset-types ()
  "Return only asset types (those with build flags)."
  *asset-types*)

(defun all-spreadsheet-record-types ()
  "Return only spreadsheet record types."
  *spreadsheet-record-types*)

(defun all-loose-file-record-types ()
  "Return only loose file record types."
  *loose-file-record-types*)

(defun all-special-resource-types ()
  "Return only special resource types."
  *special-resource-types*)

(defun resource-type-has-build-flags-p (name)
  "Check if a resource type supports Demo/Public/AA build flags."
  (let ((rt (find-resource-type name)))
    (and rt (resource-type-has-build-flags rt))))

(defun resource-type-file-extensions (name)
  "Get file extensions for a resource type."
  (let ((rt (find-resource-type name)))
    (when rt
      (resource-type-file-extensions rt))))

(defun resource-type-source-location (name)
  "Get source location template for a resource type."
  (let ((rt (find-resource-type name)))
    (when rt
      (resource-type-source-location rt))))

;; --- Build Modes ---

(defparameter *build-modes*
  '(:demo :public :publisher)
  "Valid build modes for assets.")

(defparameter *build-mode-display-names*
  '((:demo . "Demo Build")
    (:public . "Public Build")
    (:publisher . "Publisher Build"))
  "Display names for build modes.")

(defun build-mode-display-name (mode)
  "Get display name for a build mode keyword."
  (cdr (assoc mode *build-mode-display-names* :test #'eq)))

(defun valid-build-mode-p (mode)
  "Check if a keyword is a valid build mode."
  (member mode *build-modes* :test #'eq))

;; --- Regions / Frame Rates ---

(defparameter *regions*
  '(:ntsc :pal :secam)
  "All supported regions.")

(defparameter *region-frame-rates*
  '((:ntsc . 60.0)
    (:pal . 50.0)
    (:secam . 50.0))
  "Frame rates (Hz) per region.")

(defparameter *region-display-names*
  '((:ntsc . "NTSC (60 Hz)")
    (:pal . "PAL (50 Hz)")
    (:secam . "SECAM (50 Hz)"))
  "Display names for regions.")

(defun region-frame-rate (region)
  "Get frame rate in Hz for a region keyword."
  (cdr (assoc region *region-frame-rates* :test #'eq)))

(defun region-display-name (region)
  "Get display name for a region keyword."
  (cdr (assoc region *region-display-names* :test #'eq)))

(defun valid-region-p (region)
  "Check if a keyword is a valid region."
  (member region *regions* :test #'eq))

;; --- Machine-dependent region availability ---

(defun regions-for-machine (machine)
  "Return valid regions for a given machine keyword.
   Extend this as machines are added."
  (case machine
    ((:7800 :a2600 :a5200 :a7800 :a800 :xl :xe) '(:ntsc :pal :secam))
    ((:c64 :c128 :vic20 :plus4 :c16) '(:ntsc :pal))
    ((:nes :famicom) '(:ntsc :pal))
    ((:sms :gg :megadrive :genesis) '(:ntsc :pal :secam))
    (t *regions*))) ; default: all regions

;; --- Validators (stubs - implement as needed) ---

(defun validate-map (path) (declare (ignore path)) t)
(defun validate-blob (path) (declare (ignore path)) t)
(defun validate-script (path) (declare (ignore path)) t)
(defun validate-song (path) (declare (ignore path)) t)
(defun validate-character (path) (declare (ignore path)) t)
(defun validate-boat (path) (declare (ignore path)) t)
(defun validate-class (path) (declare (ignore path)) t)
(defun validate-item (path) (declare (ignore path)) t)
(defun validate-flag (path) (declare (ignore path)) t)
(defun validate-key (path) (declare (ignore path)) t)
(defun validate-instrument (path) (declare (ignore path)) t)
(defun validate-routine (path) (declare (ignore path)) t)
(defun validate-object-prototype (path) (declare (ignore path)) t)
(defun validate-tileset (path) (declare (ignore path)) t)
(defun validate-sprite-sheet (path) (declare (ignore path)) t)
(defun validate-class-source (path) (declare (ignore path)) t)
(defun validate-atari-vox-dictionary (path) (declare (ignore path)) t)
(defun validate-project-config (path) (declare (ignore path)) t)

  (defun resource-to-json (resource)
    "Convert resource object to JSON representation"
    (json:encode-json-string resource))

  (defun resource-from-json (json-string)
    "Convert JSON string to resource object"
    (json:decode-json-from-string json-string)) }}

(defun format-resource-type (rt stream)
  "Format a resource type for display."
  (format stream "~a" (resource-type-display-name rt)))

(defun format-build-mode (mode stream)
  "Format a build mode for display."
  (format stream "~a" (build-mode-display-name mode)))

(defun format-region (region stream)
  "Format a region for display."
  (format stream "~a" (region-display-name region)))


