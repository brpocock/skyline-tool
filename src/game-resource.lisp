;;; Skyline-Tool src/game-resource.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

#|

Every  (most-specific subclass)  resource  class  should specialize  the
necessary methods for at a minimum:

- A Start-RESOURCE-Scavenger function which  both finds  resources from
the  filesystem and  any files  necessary (e.g.  spreadsheets or  list
files)  and sents  eventbus notifications  about their  existence, but
also sets up inotify waits to signal eventbus if they change;

- An Open-RESOURCE-Inspector  function which opens the  inspector window
in  editing mode  by  default  for the  specific  RESOURCE kind,  i.e.
with  the required  specific  menu items  and  presentations for  that
RESOURCE class;  if passed a NIL  or no argument, opens  editing a new
resource that will be created if the user selects RESOURCE > Save from
the menu

- Specializations on the icon, title, subheading, locator, as needed;

- Full documentation in the manual  of the Inspector window, all fields,
and cross-references to the canonical explanations of the object being
edited in  the game systems.  This should be on  a new node  with page
breaks, and should explain the functioning of each menu item, shortcut
keys,  editing  gadgets,  validations or  restrictions  upon  editing,
requirements and &c &c

- Any attempt  to reference a "moniker"  on something other than  a game
asset is an error and such code will always be rejected.

|#

(defclass game-resource ()
  ((language :initarg :language :reader game-resource-language :initform :blob))
  (:documentation "Base class for all game resources. Kind is determined by class."))

;; game-resource-moniker is ONLY for game-resource-asset subclasses.
;; Use game-resource-locator for identification of all resource types.

;; Resources that are stored in their own individual files
(defclass game-resource-from-file (game-resource)
  ((full-path :initarg :full-path :reader game-resource-full-path :initform nil)))

;; Resources that are stored as records in a collective file (e.g. Tables)
(defclass game-resource-from-collective-file (game-resource)
  ((collective-path :initarg :collective-path :reader game-resource-collective-path :initform nil)
   (offset :initarg :offset :reader game-resource-offset :initform nil)))

;; Assets are a subset of from-file resources
(defclass game-resource-asset (game-resource-from-file)
  ((moniker :initarg :moniker :reader game-asset-moniker :initform nil)
   (asset-id :initarg :asset-id :reader game-resource-asset-id :initform nil)
   (builds :initarg :builds :reader game-asset-builds :initform nil)
   (language :initarg :language :reader game-resource-language :initform :blob)))

(defmethod game-resource-full-path ((resource game-resource))
  nil)

;; Asset Sub-hierarchy
(defclass game-resource-map (game-resource-asset)
   ((locale :initarg :locale :accessor game-resource-locale)
    (note :initarg :note :accessor game-resource-notes)
    (full-name :initarg :full-name :accessor game-resource-name :initform nil)))

(defclass game-resource-script (game-resource-asset)
  ((locale :initarg :locale :reader game-resource-locale)))

(defclass game-resource-song (game-resource-asset)
  ((mscz-title :initarg :mscz-title :accessor game-resource-song-mscz-title :initform nil)
   (mscz-subtitle :initarg :mscz-subtitle :accessor game-resource-song-mscz-subtitle :initform nil)
   (mscz-composer :initarg :mscz-composer :accessor game-resource-song-mscz-composer :initform nil)
   (mscz-copyright :initarg :mscz-copyright :accessor game-resource-song-mscz-copyright :initform nil)
   (mscz-lyrics :initarg :mscz-lyrics :accessor game-resource-song-mscz-lyrics :initform nil)
   (duration :initarg :duration :accessor game-resource-song-duration :initform nil)))

(defclass game-resource-blob (game-resource-asset) ())

;; Example of other resource types using the new structural classes
(defclass game-resource-boat (game-resource-from-collective-file)
  ((boat-id :initarg :id :accessor game-resource-boat-id)
   (boat-name :initarg :name :accessor game-resource-boat-name)
   (boat-class :initarg :boat-class :accessor game-resource-boat-class)
   (boat-notes :initarg :notes :accessor game-resource-notes)))

(defclass game-resource-instrument (game-resource-from-collective-file)
  ((instrument-id :initarg :instrument-id :reader game-resource-instrument-id)
   (instrument-name :initarg :instrument :accessor game-resource-instrument-name)
   (distortion :initarg :distortion :accessor game-resource-instrument-distortion)
   (attack-addend :initarg :attack-addend :accessor game-resource-instrument-attack-addend)
   (decay-subtrahend :initarg :decay-subtrahend :accessor game-resource-instrument-decay-subtrahend)
   (decay-duration :initarg :decay-duration :accessor game-resource-instrument-decay-duration)
   (release-subtrahend :initarg :release-subtrahend :accessor game-resource-instrument-release-subtrahend)
   (tia-distortion :initarg :tia-distortion :accessor game-resource-instrument-tia-distortion)
   (vibrato :initarg :vibrato :accessor game-resource-instrument-vibrato)
   (tremolo :initarg :tremolo :accessor game-resource-instrument-tremolo)
   (psg-tone :initarg :psg-tone :accessor game-resource-instrument-psg-tone)))

(defclass game-resource-item (game-resource-from-collective-file)
  ((item-id :initarg :item-id :reader game-resource-item-id)
   (name :initarg :name :accessor game-resource-item-name :initform nil)
   (equippable :initarg :equippable :accessor game-resource-item-equippable-p :initform nil)
   (shield :initarg :shield :accessor game-resource-item-shield-p :initform nil)
   (armor :initarg :armor :accessor game-resource-item-armor-p :initform nil)
   (worn :initarg :worn :accessor game-resource-item-worn-p :initform nil)
   (equipment-slot :initarg :slot :accessor game-resource-item-equipment-slot :initform nil)
   (sound :initarg :sound :accessor game-resource-item-sound :initform nil)
   (entity-class :initarg :entity-class :accessor game-resource-item-entity-class :initform nil)
   (entity-prototype :initarg :entity-prototype :accessor game-resource-item-entity-prototype :initform nil)
   (course-class :initarg :course-class :accessor game-resource-course-class :initform nil)
   (course-prototype :initarg :course-prototype :accessor game-resource-course-protoype :initform nil)
   (decal-bank :initarg :decal-bank :accessor game-resource-decal-bank :initform nil)
   (decal-sheet :initarg :decal-sheet :accessor game-resource-decal-sheet :initform nil)
   (decal-up :initarg :decal-up :accessor game-resource-decal-up :initform nil)
   (decal-down :initarg :decal-down :accessor game-resource-decal-down :initform nil)
   (decal-right :initarg :decal-right :accessor game-resource-decal-right :initform nil)
   (decal-left :initarg :decal-left :accessor game-resource-decal-left :initform nil)
   (drawing-mode :initarg :drawing-mode :accessor game-resource-item-drawing-mode :initform nil)
   (palette :initarg :palette :accessor game-resource-item-palette :initform nil)
   (displacement-up :initarg :displacement-up :accessor game-resource-item-displacement-up :initform nil)
   (displacement-down :initarg :displacement-down :accessor game-resource-item-displacement-down :initform nil)
   (displacement-right :initarg :displacement-right :accessor game-resource-item-displacement-right :initform nil)
   (displacement-left :initarg :displacement-left :accessor game-resource-item-displacement-left :initform nil)))

(defclass game-resource-flag (game-resource-from-collective-file)
  ((flag-id :initarg :flag-id :reader game-resource-flag-id)
   (name :initarg :name :accessor game-resource-flag-name)))

(defclass game-resource-key (game-resource-from-collective-file)
  ((key-id :initarg :key-id :reader game-resource-key-id :initform nil)
   (name :initarg :name :accessor game-resource-key-name :initform nil)))

(defclass game-resource-object-prototype (game-resource-from-file) ())

(defclass game-resource-character (game-resource-from-file)
  ((character-name :initarg :name :accessor game-resource-character-name)
   (character-id :initarg :character-id :reader game-resource-character-id)
   (decal :initarg :decal :accessor game-resource-character-decal)
   (gender :initarg :gender :accessor game-resource-character-gender)
   (hp :initarg :hp :accessor game-resource-character-hp)
   (max-hp :initarg :max-hp :accessor game-resource-character-max-hp)
   (ac :initarg :ac :accessor game-resource-character-ac)
   (hair-color :initarg :hair-color :accessor game-resource-character-hair-color)
   (skin-color :initarg :skin-color :accessor game-resource-character-skin-color)
   (clothes-color :initarg :clothes-color :accessor game-resource-character-clothes-color)
   (head :initarg :head :accessor game-resource-character-head)
   (body :initarg :body :accessor game-resource-character-body)
   (speech-pitch :initarg :speech-pitch :accessor game-resource-character-speech-pitch)
   (speech-speed :initarg :speech-speed :accessor game-resource-character-speech-speed)
   (speech-bend :initarg :speech-bend :accessor game-resource-character-speech-bend)
   (speech-color :initarg :speech-color :accessor game-resource-character-speech-color)
   (nicks :initarg :nicks :accessor game-resource-character-nicks)
   (memo :initarg :memo :accessor game-resource-character-memo :initform nil)
   (home :initarg :home :accessor game-resource-character-home)
   (equipment :initarg :equipment :accessor game-resource-character-equipment)
   (shield :initarg :shield :accessor game-resource-character-shield)
   (crowns :initarg :crowns :accessor game-resource-character-crowns)
   (arrows :initarg :arrows :accessor game-resource-character-arrows)
   (potions :initarg :potions :accessor game-resource-character-potions)
   (chalice :initarg :chalice :accessor game-resource-character-chalice)
   (faction :initarg :faction :accessor game-resource-character-faction :initform 0)
   (flags :initarg :flags :accessor game-resource-character-flags :initform 0)
   (course-class :initarg :course-class :accessor game-resource-character-course-class :initform "Course")
   (course-prototype :initarg :course-prototype :accessor game-resource-character-course-prototype :initform nil)
   (keys :initarg :keys :accessor game-resource-character-keys :initform 0 :type 'bit-vector)
   (inventory :initarg :inventory :accessor game-resource-character-inventory :initform 0 :type 'bit-vector)))

(defclass game-resource-translation (game-resource-from-file) ())
(defclass game-resource-phonetic-dictionary (game-resource-translation)
  ((word :initarg :word :accessor game-translation-word)
   (phonetics :initarg :phonetics :accessor game-translation-phonetics)
   (language :initarg :language :accessor game-translation-language)
   (dialect :initarg :dialect :accessor game-translation-dialect)
   (comment-before :initarg :comment-before :accessor game-translation-comments)))

(defclass game-resource-atari-vox-dictionary (game-resource-phonetic-dictionary) ())
(defclass game-resource-intellivoice-dictionary (game-resource-phonetic-dictionary) ())
(defclass game-resource-magic-desk-dictionary (game-resource-phonetic-dictionary) ())
(defclass game-resource-phrasebook (game-resource-translation)
  ((english-key :initarg :english-key :accessor game-translation-english-key)
   (translation :initarg :translation :accessor game-translation-translation)
   (language :initarg :language :accessor game-translation-language)
   (dialect :initarg :dialect :accessor game-translation-dialect)
   (comment-before :initarg :comment-before :accessor game-translation-comments)))

(defclass game-resource-class (game-resource-from-file) ())
(defclass game-resource-class-cobol (game-resource-class) ())
(defclass game-resource-class-basic (game-resource-class) ())
(defclass game-resource-class-fortran (game-resource-class) ())
(defclass game-resource-class-pascal (game-resource-class) ())
(defclass game-resource-class-smalltalk (game-resource-class) ())
(defclass game-resource-class-lingo (game-resource-class) ())
(defclass game-resource-class-lua (game-resource-class) ())
(defclass game-resource-class-objective (game-resource-class) ())

(defclass game-resource-tileset (game-resource-from-file) ())

(defclass game-resource-sprite-sheet (game-resource-from-file) ())

(defclass game-resource-routine (game-resource-from-file) ())
(defclass game-resource-routine-forth-library (game-resource-routine) ())
(defclass game-resource-routine-run-commands (game-resource-routine) ())
(defclass game-resource-routine-rc-cobol (game-resource-routine-run-commands) ())
(defclass game-resource-routine-rc-basic (game-resource-routine-run-commands) ())
(defclass game-resource-routine-rc-pascal (game-resource-routine-run-commands) ())
(defclass game-resource-routine-rc-fortran (game-resource-routine-run-commands) ())
(defclass game-resource-routine-rc-smalltalk (game-resource-routine-run-commands) ())
(defclass game-resource-routine-rc-lingo (game-resource-routine-run-commands) ())
(defclass game-resource-routine-rc-lua (game-resource-routine-run-commands) ())
(defclass game-resource-routine-rc-objective (game-resource-routine-run-commands) ())

(clim:define-presentation-type game-resource-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-reference)))
  (typep object 'game-resource))

(clim:define-presentation-type game-resource-reading ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-reading)))
  (typep object 'game-resource))

(clim:define-presentation-type game-resource-editing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-editing)))
  (typep object 'game-resource))

(defgeneric game-resource-present-title (resource stream)
  (:documentation "Present the title string for reference presentation.")
  (:method ((resource game-resource) stream)
    (princ (game-resource-title resource) stream)))

(defgeneric game-resource-present-subheading (resource stream)
  (:documentation "Present the subheading string for reference presentation.")
  (:method ((resource game-resource) stream)
    (princ (game-resource-subheading resource) stream)))

(defgeneric game-resource-present-icon (resource stream)
  (:documentation "Present the icon for reference presentation.")
  (:method ((resource game-resource) stream)
    (declare (ignore stream))
    nil))

(defgeneric game-resource-present-build-checkboxes (resource stream)
  (:documentation "Present D/P/A build checkboxes for asset resources.")
  (:method ((asset game-resource-asset) stream)
    (format stream " [~:[☐~;☑~]D ~:[☐~;☑~]P ~:[☐~;☑~]A]"
            (game-asset-build-p asset :demo)
            (game-asset-build-p asset :public)
            (game-asset-build-p asset :publisher))))

(defun game-asset-build-p (asset build)
  (let ((builds (game-asset-builds asset)))
    (ecase build
      (:demo (member "Demo" builds :test 'string-equal))
      (:public (member "Public" builds :test 'string-equal))
      (:publisher (member "AA" builds :test 'string-equal)))))

(defgeneric game-resource-present-right-margin (resource stream)
  (:documentation "Present right-margin content for reference presentation (e.g. D/P/A, asset ID).")
  
  (:method ((resource game-resource) stream)
    (let ((version-control-status (version-control-file-status (or (game-resource-full-path resource)
                                                                   (game-resource-collective-path resource)))))
      (format stream "~a~%" (game-resource-locator resource))
      (game-resource-present-build-checkboxes resource stream)
      (when version-control-status
        (let ((*standard-output* stream))
          (present-version-control-status-icon version-control-status))))))

(defmethod game-resource-locator ((resource game-resource))
  ;; For file-based resources without asset IDs, generate a unique hex ID from pathnames
  (when (typep resource 'game-resource-from-file)
    (format nil "~6,'0x"
      (logand #xffffff
        (reduce #'logxor
          (mapcar #'sxhash
            (mapcar #'enough-namestring (game-resource-pathnames resource))))))))

(defmethod game-resource-locator ((resource game-resource-asset))
  (format nil "$~2,'0x" (or (game-resource-asset-id resource) (game-asset-moniker resource))))

(defmethod game-resource-locator ((resource game-resource-script))
  (format nil "$~4,'0x" (or (game-resource-asset-id resource) (sxhash (game-asset-moniker resource)))))

(defun ensure-project.json-loaded ()
  "Ensure *project.json* is loaded from disk if not already bound."
  (unless (and (boundp '*project.json*) *project.json*)
    (let* ((port (or (and (boundp '*machine*) *machine*)
                     7800))
           (json-name (format nil "Project.~a.json" port))
           (cwd (uiop:getcwd))
           (json-path (or (probe-file (merge-pathnames json-name cwd))
                          (probe-file (merge-pathnames json-name
                                                       (make-pathname :directory
                                                                      (butlast (cdr (pathname-directory cwd)))))))))
      (when json-path
        (setf *project.json* (json:decode-json-from-source json-path)))))
  *project.json*)

(defmethod game-resource-locator ((resource game-resource-tileset))
  (let* ((name (when (typep resource 'game-resource-from-file)
                  (pathname-name (game-resource-full-path resource))))
         (name-string (and name (if (symbolp name) (symbol-name name) name)))
         (lower (and name-string (string-downcase name-string))))
    (let* ((tileset-alist (and (ensure-project.json-loaded)
                               (assocdr :tilesets *project.json*)))
           (key (and name (intern (string-upcase name) :keyword)))
           (bank (and key tileset-alist
                      (cdr (assoc key tileset-alist))))
           (address (cond
                      ((and lower (search "decal" lower))
                       #xa000)
                      ((and lower (search "tileset" lower))
                       #x8000)
                      (t #x8000))))
      (format nil "$~2,'0x:~4,'0x" (or bank 0) address))))

(defgeneric present-reference (resource stream)
  (:documentation "Present resource in reference context (icon, title, info, id, build checks).")
  (:method ((resource game-resource) stream)
    (clim:with-output-as-presentation
        (stream resource 'game-resource-reference)
      (clim:formatting-table (stream)
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 0)
            (format stream "~3%"))
          (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 125)
            (game-resource-present-icon resource stream))
          ;; Title
          (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 150)
            (clim:with-text-face (stream :bold)
              (game-resource-present-title resource stream))
            ;; Subheading on next line in small, possibly gray text
            (format stream "~%~5t")
            (clim:with-text-size (stream :smaller)
              (clim:with-drawing-options (stream :ink (clim:make-gray-color 0.75))
                (game-resource-present-subheading resource stream))))
          (clim:formatting-cell (stream :align-x :right :align-y :top :min-height 90 :min-width 125)
            (game-resource-present-right-margin resource stream)))))))

(clim:define-command com-inspect-resource
    ((resource 'game-resource :prompt "Resource to inspect")
     &key
     (view 'symbol :prompt "View type" :default :editing))
  "Inspect a game resource."
  (ecase view
    (:reference (open-resource-inspector resource :reference))
    (:reading (open-resource-inspector resource :reading))
    (:editing (open-resource-inspector resource :editing))))

(defgeneric present-reading (resource stream)
  (:documentation "Present resource in reading context (full view, visually similar to PDF output).")
  (:method ((resource game-resource) stream)
    (let ((display-name (game-resource-title resource))
          (kind (game-resource-kind resource))
          (locator (game-resource-locator resource))
          (full-path (if (typep resource 'game-resource-from-file)
                         (game-resource-full-path resource)
                         nil))
          (asset-id (if (typep resource 'game-resource-asset)
                        (game-resource-asset-id resource)
                        nil))
          (builds (if (typep resource 'game-resource-asset)
                      (game-asset-builds resource)
                      nil))
          (version-control-status (version-control-file-status (or (game-resource-full-path resource)
                                                                   (game-resource-collective-path resource)))))
      (clim:formatting-table (stream)
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream :align-x :right)
            (format stream "Name: "))
          (clim:formatting-cell (stream :align-x :left)
            (princ display-name stream)))
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream :align-x :right)
            (format stream "Kind: "))
          (clim:formatting-cell (stream :align-x :left)
            (princ kind stream)))
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream :align-x :right)
            (format stream "Locator: "))
          (clim:formatting-cell (stream :align-x :left)
            (princ locator stream)))
        (when full-path
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream :align-x :right)
              (format stream "Path: "))
            (clim:formatting-cell (stream :align-x :left)
              (princ full-path stream))))
        (when asset-id
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream :align-x :right)
              (format stream "Asset ID: "))
            (clim:formatting-cell (stream :align-x :left)
              (format stream "$~a" asset-id))))
        (when builds
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream :align-x :right)
              (format stream "Builds: "))
            (clim:formatting-cell (stream :align-x :left)
              (format stream "~{~a~^, ~}" builds))))
        (when version-control-status
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream :align-x :right)
              (format stream "VC Status: "))
            (clim:formatting-cell (stream :align-x :left)
              (princ version-control-status stream))))))))

(defgeneric present-editing (resource stream)
  (:documentation "Present resource in editing context with tabular layout: labels on left, editing gadgets on right.")
  (:method ((resource game-resource) stream)
    (let ((display-name (game-resource-title resource))
          (kind (game-resource-kind resource))
          (locator (game-resource-locator resource))
          (version-control-status (version-control-file-status (or (game-resource-full-path resource)
                                                                   (game-resource-collective-path resource)))))
      (clim:formatting-table (stream)
        ;; Name (editable)
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream :align-x :right)
            (format stream "Name: "))
          (clim:formatting-cell (stream :align-x :left)
            (princ display-name stream)))
        ;; Kind (read-only)
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream :align-x :right)
            (format stream "Kind: "))
          (clim:formatting-cell (stream :align-x :left)
            (format stream "~a (read-only)" kind)))
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream :align-x :right)
            (format stream "Locator: "))
          (clim:formatting-cell (stream :align-x :left)
            (princ locator stream)))
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream :align-x :right)
            (format stream "Path~p: " (length (game-resource-pathnames resource))))
          (clim:formatting-cell (stream :align-x :left)
            (if (typep resource 'game-resource-from-file)
                (princ (game-resource-pathnames resource) stream)
                nil)))
        (when (typep resource 'game-resource-asset)
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream :align-x :right)
              (format stream "Asset ID: "))
            (clim:formatting-cell (stream :align-x :left)
              (if (typep resource 'game-resource-asset)
                  (princ (game-resource-asset-id resource) stream)
                  nil)))
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream :align-x :right)
              (format stream "Builds: "))
            (clim:formatting-cell (stream :align-x :left)
              (format stream "~{~a~^, ~}" (game-asset-builds resource)))))
        (when version-control-status
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream :align-x :right)
              (format stream "VC Status: "))
            (clim:formatting-cell (stream :align-x :left)
              (princ version-control-status stream))))))))

(defun game-resource-to-json (resource)
  "Convert resource to JSON for detail view."
  (let ((obj (make-hash-table :test 'equal)))
    (setf (gethash "name" obj) (game-resource-title resource))
    (setf (gethash "kind" obj) (game-resource-kind resource))
    (when (typep resource 'game-resource-from-file)
      (setf (gethash "path" obj) (game-resource-full-path resource)))
    (when (typep resource 'game-resource-asset)
      (setf (gethash "assetId" obj) (game-resource-asset-id resource))
      (setf (gethash "builds" obj) (game-asset-builds resource)))
    obj))

(defun game-resource-to-postscript (resource)
  "Convert resource to PostScript for detail/reference view."
  (with-output-to-string (s)
    (format s "%% Resource: ~a~%" (game-resource-title resource))
    (format s "%% Kind: ~a~%" (game-resource-kind resource))))

(defun game-resource-to-text (resource)
  "Convert resource to plain text for reference/detail view."
  (game-resource-fulltext resource))

(defgeneric resource-folder (resource)
  (:method ((resource game-resource))
    (error "No folder for ~a" (type-of resource))))

(defgeneric inspector-display-function (resource)
  (:method ((resource game-resource))
    (error "unimplemented")))

;; 
;; Resource Inspector Mixin — shared by all inspector frames
;; 

(defclass resource-inspector-mixin ()
  ((resource :initarg :resource :reader inspector-resource
              :initform nil)
   (view-mode :initform :reference :accessor frame-view-mode)
   (%show-search-bar-p :initform nil :accessor frame-show-search-bar-p)
   (%show-project-bar-p :initform nil :accessor frame-show-project-bar-p))
  (:documentation "Mixin for inspector frames that edit a Game-Resource.
Provides RESOURCE slot (accessed via INSPECTOR-RESOURCE) and VIEW-MODE slot.
Each resource type specializes PRESENT-REFERENCE, PRESENT-DETAIL, and
PRESENT-EDITING on its Game-Resource subclass to render the inspector body."))

(defmethod inspector-resource (frame) nil)

;; 
;; Generic Resource Inspector Frame — fallback for un-specialized resources
;; 

(clim:define-application-frame resource-inspector (resource-inspector-mixin)
  ()
  (:panes
   (content :application
            :display-function 'display-resource-inspector
            :scroll-bars :vertical
            :height 600 :width 800)
   (status-bar :application
               :display-function 'display-resource-status
               :height 30 :width 800))
  (:layouts
   (default (clim:vertically () content status-bar)))
  (:menu-bar resource-inspector-menu-bar))

;; Menu bar for inspector
(clim:define-command-table resource-inspector-menu-bar
  :menu (("File" :menu resource-inspector-file-menu)
         ("Edit" :menu resource-inspector-edit-menu)
         ("View" :menu resource-inspector-view-menu)
         ("Help" :menu resource-inspector-help-menu)))

(clim:define-command-table resource-inspector-file-menu
  :menu (("Save" :command com-inspector-save)
         ("Save As..." :command com-inspector-save-as)
         (nil :divider :line)
         ("Close" :command com-inspector-close)))

(clim:define-command-table resource-inspector-edit-menu
  :menu (("Undo" :command com-inspector-undo)
         ("Redo" :command com-inspector-redo)
         (nil :divider :line)
         ("Preferences..." :command com-edit-skyline-config-prefs)))

(clim:define-command-table resource-inspector-view-menu
  :menu (("Reference View" :command com-inspector-view-reference)
         ("Reading View" :command com-inspector-view-reading)
         ("Editing View" :command com-inspector-view-editing)))

(clim:define-command-table resource-inspector-help-menu
  :menu (("About Skyline-Tool..." :command com-about-skyline-tool)))

(defgeneric display-inspector-content (frame pane)
  (:documentation "Display the inspector content pane for FRAME on PANE.
Specialized by each inspector frame type. Default dispatches on
resource view mode (reference/reading/editing).")
  (:method ((frame resource-inspector-mixin) pane)
    (let ((resource (inspector-resource frame)))
      (when resource
        (let ((*standard-output* pane))
          (ecase (frame-view-mode frame)
            (:reference (present-reference resource pane))
            (:reading   (present-reading resource pane))
            (:editing   (present-editing resource pane))))))))

(defun display-resource-inspector (frame pane)
  "Display the resource inspector content pane."
  (display-inspector-content frame pane))

(defun display-resource-status (frame pane)
  "Display status bar at bottom of inspector."
  (let ((resource (inspector-resource frame)))
    (when resource
      (let ((*standard-output* pane))
        (format pane " ~a | ~a | View: ~a"
                (game-resource-kind resource)
                (game-resource-title resource)
                (string-downcase (symbol-name (frame-view-mode frame))))))))

;; Inspector commands
(clim:define-command (com-inspector-close :command-table resource-inspector-menu-bar
                                           :menu t :name t) ()
  (clim:frame-exit clim:*application-frame*))

(clim:define-command (com-inspector-save :command-table resource-inspector-menu-bar
                                          :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (resource (and frame (inspector-resource frame))))
    (when resource
      (save-resource resource)
      (format t "~&Saved ~a~%" (game-resource-title resource))
      (clim:redisplay-frame-panes frame))))

(clim:define-command (com-inspector-save-as :command-table resource-inspector-menu-bar
                                             :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (resource (and frame (inspector-resource frame))))
    (when resource
      (let ((new-path (clim:accept 'pathname :prompt "Save as:" :default (game-resource-full-path resource))))
        (when new-path
          (save-resource-as resource new-path)
          (format t "~&Saved ~a as ~a~%" (game-resource-title resource) new-path)
          (clim:redisplay-frame-panes frame))))))

(clim:define-command (com-inspector-undo :command-table resource-inspector-menu-bar
                                          :menu t :name t) ()
  (format t "~&Undo not implemented~%"))

(clim:define-command (com-inspector-redo :command-table resource-inspector-menu-bar
                                          :menu t :name t) ()
  (format t "~&Redo not implemented~%"))

(clim:define-command (com-inspector-view-reference :command-table resource-inspector-menu-bar
                                                    :menu t :name t) ()
  (setf (frame-view-mode clim:*application-frame*) :reference)
  (clim:redisplay-frame-panes clim:*application-frame*))

(clim:define-command (com-inspector-view-reading :command-table resource-inspector-menu-bar
                                                  :menu t :name t) ()
  (setf (frame-view-mode clim:*application-frame*) :reading)
  (clim:redisplay-frame-panes clim:*application-frame*))

(clim:define-command (com-inspector-view-editing :command-table resource-inspector-menu-bar
                                                 :menu t :name t) ()
  (setf (frame-view-mode clim:*application-frame*) :editing)
  (clim:redisplay-frame-panes clim:*application-frame*))

;; 
;; Save Functions (to be specialized per resource type)
;; 

(defgeneric save-resource (resource)
  (:documentation "Save resource to its original file.")
  (:method ((resource game-resource))
    (format t "~&Saving ~a... (default implementation - no-op)" (type-of resource))))

(defgeneric save-resource-as (resource new-path)
  (:documentation "Save resource to a new path.")
  (:method ((resource game-resource) new-path)
    (format t "~&Saving ~a as ~a... (default implementation - no-op)" (type-of resource) new-path)))

;;; --- save-resource specializations for all concrete resource classes ---

(defmethod save-resource ((resource game-resource-map))
  (format t "~&Saving ~a to ~a...~%"
          (game-resource-title resource)
          (game-resource-full-path resource))
  (publish-resource-changed resource))

(defmethod save-resource ((resource game-resource-script))
  (format t "~&Saving ~a to ~a...~%"
          (game-resource-title resource)
          (game-resource-full-path resource))
  (publish-resource-changed resource))

(defmethod save-resource ((resource game-resource-song))
  (format t "~&Saving ~a to ~a...~%"
          (game-resource-title resource)
          (game-resource-full-path resource))
  (publish-resource-changed resource))

(defmethod save-resource ((resource game-resource-blob))
  (format t "~&Saving ~a to ~a...~%"
          (game-resource-title resource)
          (game-resource-full-path resource))
  (publish-resource-changed resource))

(defmethod save-resource ((resource game-resource-object-prototype))
  (format t "~&Saving ~a to ~a...~%"
          (game-resource-title resource)
          (game-resource-full-path resource))
  (publish-resource-changed resource))

(defmethod save-resource ((resource game-resource-character))
  (format t "~&Saving ~a to ~a...~%"
          (game-resource-title resource)
          (game-resource-full-path resource))
  (publish-resource-changed resource))

(defmethod save-resource ((resource game-resource-class))
  (format t "~&Saving ~a to ~a...~%"
          (game-resource-title resource)
          (game-resource-full-path resource))
  (publish-resource-changed resource))

(defmethod save-resource ((resource game-resource-tileset))
  (format t "~&Saving ~a to ~a...~%"
          (game-resource-title resource)
          (game-resource-full-path resource))
  (publish-resource-changed resource))

(defmethod save-resource ((resource game-resource-sprite-sheet))
  (format t "~&Saving ~a to ~a...~%"
          (game-resource-title resource)
          (game-resource-full-path resource))
  (publish-resource-changed resource))

(defmethod save-resource ((resource game-resource-translation))
  (format t "~&Saving ~a to ~a...~%"
          (game-resource-title resource)
          (game-resource-full-path resource))
  (publish-resource-changed resource))

(defmethod save-resource ((resource game-resource-atari-vox-dictionary))
  (format t "~&Saving ~a to ~a...~%"
          (game-resource-title resource)
          (game-resource-full-path resource))
  (publish-resource-changed resource))

(defmethod save-resource ((resource game-resource-intellivoice-dictionary))
  (format t "~&Saving ~a to ~a...~%"
          (game-resource-title resource)
          (game-resource-full-path resource))
  (publish-resource-changed resource))

(defmethod save-resource ((resource game-resource-phrasebook))
  (format t "~&Saving ~a to ~a...~%"
          (game-resource-title resource)
          (game-resource-full-path resource))
  (publish-resource-changed resource))

(defmethod save-resource ((resource game-resource-routine))
  (format t "~&Saving ~a to ~a...~%"
          (game-resource-title resource)
          (game-resource-full-path resource))
  (publish-resource-changed resource))

(defmethod save-resource ((resource game-resource-routine-forth-library))
  (format t "~&Saving ~a to ~a...~%"
          (game-resource-title resource)
          (game-resource-full-path resource))
  (publish-resource-changed resource))

(defmethod save-resource ((resource game-resource-routine-run-commands))
  (format t "~&Saving ~a to ~a...~%"
          (game-resource-title resource)
          (game-resource-full-path resource))
  (publish-resource-changed resource))

(defmethod save-resource ((resource game-resource-boat))
  (format t "~&Saving ~a (collective file: ~a)...~%"
          (game-resource-title resource)
          (game-resource-collective-path resource))
  (publish-resource-changed resource))

(defmethod save-resource ((resource game-resource-instrument))
  (format t "~&Saving ~a (collective file: ~a)...~%"
          (game-resource-title resource)
          (game-resource-collective-path resource))
  (publish-resource-changed resource))

(defmethod save-resource ((resource game-resource-item))
  (format t "~&Saving ~a (collective file: ~a)...~%"
          (game-resource-title resource)
          (game-resource-collective-path resource))
  (publish-resource-changed resource))

(defmethod save-resource ((resource game-resource-flag))
  (format t "~&Saving ~a (collective file: ~a)...~%"
          (game-resource-title resource)
          (game-resource-collective-path resource))
  (publish-resource-changed resource))

(defmethod save-resource ((resource game-resource-key))
  (format t "~&Saving ~a (collective file: ~a)...~%"
          (game-resource-title resource)
          (game-resource-collective-path resource))
  (publish-resource-changed resource))

(defun kind->resource-class (kind-name)
  "Map a kind-name string to the appropriate game-resource class."
  (let ((key (ignore-errors (kind-by-name kind-name))))
    (cond
      ((eql key :map) 'game-resource-map)
      ((eql key :script) 'game-resource-script)
      ((eql key :song) 'game-resource-song)
      ((eql key :blob) 'game-resource-blob)
      ((eql key :tileset) 'game-resource-tileset)
      ((eql key :sprite-sheet) 'game-resource-sprite-sheet)
      ((eql key :character) 'game-resource-character)
      ((eql key :class) 'game-resource-class)
      ((eql key :routine) 'game-resource-routine)
      ((eql key :object-prototype) 'game-resource-object-prototype)
      ((eql key :boat) 'game-resource-boat)
      ((eql key :instrument) 'game-resource-instrument)
      ((eql key :item) 'game-resource-item)
      ((eql key :flag) 'game-resource-flag)
      ((eql key :key) 'game-resource-key)
      ((eql key :translation) 'game-resource-translation)
      ((eql key :atari-vox-dictionary) 'game-resource-atari-vox-dictionary)
      ((eql key :intellivoice-dictionary) 'game-resource-intellivoice-dictionary)
      ((eql key :magic-desk-dictionary) 'game-resource-magic-desk-dictionary)
      ((eql key :phrasebook) 'game-resource-phrasebook)
      (t 'game-resource))))

(defgeneric game-resource-pathnames (resource)
  (:documentation "Return a list of filesystem paths for RESOURCE based on its type."))

(defmethod game-resource-pathnames ((resource game-resource))
  "Default: return the full-path if available."
  (when (typep resource 'game-resource-from-file)
    (let ((path (game-resource-full-path resource)))
      (when path
        (list path)))))

(defmethod game-resource-pathnames ((resource game-resource-script))
  (let* ((moniker (game-asset-moniker resource))
         (parts (split-sequence #\/ moniker))
         (region (if (< 1 (length parts))
                     (string-capitalize (second parts))
                     "NTSC"))
         (file-name (namestring (car (last parts)))))
    (list (format nil "Source/Scripts/~a/~a"
                  region
                  (if (search ".fountain" file-name)
                      file-name
                      (format nil "~a.fountain" file-name))))))

(defmethod game-resource-pathnames ((resource game-resource-song))
  (let* ((moniker (game-asset-moniker resource))
         (parts (split-sequence #\/ moniker))
         (file-name (namestring (car (last parts)))))
    (list (format nil "Source/Songs/~a"
                  (if (search ".mscz" file-name)
                      file-name
                      (format nil "~a.mscz" file-name))))))

(defmethod game-resource-pathnames ((resource game-resource-map))
  (let* ((moniker (game-asset-moniker resource))
         (parts (split-sequence #\/ moniker))
         (region (if (< 2 (length parts))
                     (string-capitalize (second parts))
                     "NTSC")))
    (list (format nil "Source/Maps/~a/~a.tmx" region (last parts))
          "Source/Tables/MapsIndex.ods")))

(defmethod game-resource-pathnames ((resource game-resource-tileset))
  (let ((full-path (game-resource-full-path resource)))
    (if full-path
        (list full-path)
        nil)))

(defmethod game-resource-pathnames ((resource game-resource-blob))
  (let* ((moniker (game-asset-moniker resource))
         (parts (split-sequence #\/ moniker)))
    (list (format nil "Source/Blobs/~a/~a.xcf"
                  (machine-directory-name)
                  (last parts)))))

(defmethod game-resource-pathnames ((resource game-resource-sprite-sheet))
  (list (game-resource-full-path resource)))

(defmethod game-resource-pathnames ((resource game-resource-character))
  (list "Source/Tables/NPCStats.ods"))

(defmethod game-resource-pathnames ((resource game-resource-object-prototype))
  (list (game-resource-full-path resource)))

(defmethod game-resource-pathnames ((resource game-resource-class))
  (list (game-resource-full-path resource)))

(defmethod game-resource-pathnames ((resource game-resource-routine))
  (list (game-resource-full-path resource)))

;; Items/Equipment/Flags/Keys all come from the same ODS file
(defmethod game-resource-pathnames ((resource game-resource-item))
  (list (game-resource-collective-path resource)))
(defmethod game-resource-pathnames ((resource game-resource-flag))
  (list (game-resource-collective-path resource)))
(defmethod game-resource-pathnames ((resource game-resource-key))
  (list (game-resource-collective-path resource)))
(defmethod game-resource-pathnames ((resource game-resource-boat))
  (list (game-resource-collective-path resource)))

(defmethod game-resource-pathnames ((resource game-resource-instrument))
  (list (game-resource-collective-path resource)))

(defmethod game-resource-title ((resource game-resource-class))
  (or (game-resource-class-name resource)
      (format nil "Class: ~a" (game-resource-locator resource))))

(defmethod game-resource-subheading ((resource game-resource-class))
  "COBOL Class Definition")

(defmethod game-resource-collective-path ((resource game-resource-class))
  "Source/Classes")

;; Base fallback methods for game-resource
(defmethod game-resource-title ((resource game-resource))
  (title-case (pathname-name (first (game-resource-pathnames resource)))))

(defmethod game-resource-subheading ((resource game-resource))
  "")

;; --- game-resource-locator methods for non-asset types ---

(defmethod game-resource-locator ((resource game-resource-item))
  (let ((item-id (game-resource-item-id resource)))
    (if item-id (format nil "$~2,'0x" item-id) "$00")))

(defmethod game-resource-locator ((resource game-resource-boat))
  (let ((boat-id (game-resource-boat-id resource)))
    (if boat-id (format nil "$~2,'0x" boat-id) "$00")))

(defmethod game-resource-locator ((resource game-resource-instrument))
  (let ((inst-id (game-resource-instrument-id resource)))
    (if inst-id (format nil "$~2,'0x" inst-id) "$00")))

(defmethod game-resource-locator ((resource game-resource-flag))
  (let ((flag-id (game-resource-flag-id resource)))
    (if flag-id (format nil "$~2,'0x" flag-id) "$00")))

(defmethod game-resource-locator ((resource game-resource-key))
  (let ((key-id (game-resource-key-id resource)))
    (if key-id (format nil "$~2,'0x" key-id) "$00")))

(defmethod game-resource-locator ((resource game-resource-character))
  (let ((char-id (game-resource-character-id resource)))
    (if char-id (format nil "$~2,'0x" char-id) "$00")))

;; --- Asset title methods (use game-asset-moniker) ---

(defmethod game-resource-subheading ((resource game-resource-map))
  (game-resource-notes resource))

(defmethod game-resource-subheading ((resource game-resource-song))
  (let ((title (game-resource-song-mscz-title resource))
        (subtitle (game-resource-song-mscz-subtitle resource))
        (composer (game-resource-song-mscz-composer resource)))
    (when (or title subtitle composer)
      (format nil "~@[~a~]~@[ ~a~]~@[ (~a)~]"
              title subtitle composer))))

(defmethod game-resource-title ((resource game-resource-boat))
  (game-resource-boat-name resource))

(defmethod game-resource-subheading ((resource game-resource-boat))
  (game-resource-boat-class resource))

(defmethod game-resource-title ((resource game-resource-instrument))
  (game-resource-instrument-name resource))

(defmethod game-resource-title ((resource game-resource-item))
  (game-resource-item-name resource))

(defmethod game-resource-subheading ((resource game-resource-item))
  (let ((proto (game-resource-item-entity-prototype resource)))
    (when proto (format nil "~a" proto))))

(defmethod game-resource-title ((resource game-resource-flag))
  (game-resource-flag-name resource))

(defmethod (setf game-resource-title) (value (resource game-resource-flag))
  (setf (game-resource-flag-name resource) value))

(defmethod game-resource-title ((resource game-resource-key))
  (game-resource-key-name resource))

(defmethod (setf game-resource-title) (value (resource game-resource-key))
  (setf (game-resource-key-name resource) value))

(defmethod game-resource-title ((resource game-resource-object-prototype))
  (game-resource-locator resource))

(defmethod game-resource-subheading ((resource game-resource-object-prototype))
  "Object Prototype Definition")

(defmethod game-resource-title ((resource game-resource-character))
  (game-resource-character-name resource))

(defmethod game-resource-subheading ((resource game-resource-character))
  (game-resource-character-memo resource))

;; Translation resources

(defmethod game-resource-title ((resource game-resource-atari-vox-dictionary))
  "SpeakJet.dic")

(defmethod game-resource-subheading ((resource game-resource-atari-vox-dictionary))
  "U.S. English")

(defmethod game-resource-title ((resource game-resource-intellivoice-dictionary))
  "IntelliVoice.dic")

(defmethod game-resource-subheading ((resource game-resource-intellivoice-dictionary))
  "U.S. English")

(defmethod game-resource-title ((resource game-resource-phrasebook))
  (game-translation-english-key resource))

(defmethod game-resource-subheading ((resource game-resource-phrasebook))
  (game-translation-language resource))

;; Routine resources
(defmethod game-resource-subheading ((resource game-resource-routine-forth-library))
  "Forth Library")

(defmethod game-resource-subheading ((resource game-resource-routine-rc-cobol))
  "COBOL Run-Commands Routine")

(defmethod game-resource-subheading ((resource game-resource-routine-rc-basic))
  "BASIC Run-Commands Routine")

(defmethod game-resource-subheading ((resource game-resource-routine-rc-pascal))
  "Pascal Run-Commands Routine")

(defmethod game-resource-subheading ((resource game-resource-routine-rc-fortran))
  "FORTRAN Run-Commands Routine")

(defmethod game-resource-subheading ((resource game-resource-routine-rc-smalltalk))
  "SmallTalk Run-Commands Routine")

(defmethod game-resource-subheading ((resource game-resource-routine-rc-lingo))
  "Lingo Run-Commands Routine")

(defmethod game-resource-subheading ((resource game-resource-routine-rc-lua))
  "Lua Run-Commands Routine")

(defmethod game-resource-subheading ((resource game-resource-routine-rc-objective))
  "Objective Run-Commands Routine")

;; Class resources
(defmethod game-resource-subheading ((resource game-resource-class))
  "Class Definition")

(defmethod game-resource-subheading ((resource game-resource-class-cobol))
  "COBOL Class Definition")

(defmethod game-resource-subheading ((resource game-resource-class-basic))
  "BASIC Class Definition")

(defmethod game-resource-subheading ((resource game-resource-class-fortran))
  "FORTRAN Class Definition")

(defmethod game-resource-subheading ((resource game-resource-class-pascal))
  "Pascal Class Definition")

(defmethod game-resource-subheading ((resource game-resource-class-smalltalk))
  "SmallTalk Class Definition")

(defmethod game-resource-subheading ((resource game-resource-class-lingo))
  "Lingo Class Definition")

(defmethod game-resource-subheading ((resource game-resource-class-lua))
  "Lua Class Definition")

(defmethod game-resource-subheading ((resource game-resource-class-objective))
  "Objective Class Definition")

(defgeneric game-resource-title (resource))
(defgeneric game-resource-subheading (resource))
(defun game-resource-fulltext (resource)
  (game-resource-full-text resource))
(defgeneric game-resource-full-text (resource)
  (:method ((resource game-resource))
    (concatenate 'string
                 (game-resource-title resource)
                 " "
                 (game-resource-subheading resource)))
  (:method ((resource game-resource-script))
    (concatenate 'string
                 (call-next-method)
                 " "
                 (reduce (lambda (a b) (concatenate 'string a b))
                         (mapcar #'read-file-into-string (game-resource-pathnames resource)))))
  (:method ((resource game-resource-routine))
    (concatenate 'string
                 (call-next-method)
                 #(#\Newline #\Newline)
                 (reduce (lambda (a b) (concatenate 'string a #(#\Newline #\Newline) b))
                         (mapcar #'read-file-into-string (game-resource-pathnames resource))))))

(defmethod file-name-and-contents ((resource game-resource-from-file))
  (concatenate 'string
               (game-resource-title resource)
               " "
               (read-file-into-string (first (game-resource-pathnames resource)))))

(defmethod game-resource-full-text ((resource game-resource-song))
  (let ((parts (list (game-resource-title resource)
                     (game-resource-subheading resource)
                     (game-resource-song-mscz-composer resource)
                     (game-resource-song-mscz-copyright resource)
                     (game-resource-song-mscz-lyrics resource))))
    (format nil "~{~a~^ ~}" (nreverse parts))))

(defgeneric game-resource-last-updated (resource)
  (:method ((resource game-resource-from-file))
    (file-write-date (first (game-resource-pathnames resource))))
  (:method ((resource game-resource-from-collective-file))
    (file-write-date (first (game-resource-pathnames resource)))))

(defgeneric game-resource-version-control-status (resource))

(defgeneric game-resource-locator (resource))

(defgeneric game-resource-asset-p (resource)
  (:method ((resource game-resource)) nil)
  (:method ((resource game-resource-asset)) t))

(defgeneric game-resource-present-icon (resource stream))

(defgeneric game-resource-present-reference (resource stream))

(defgeneric game-resource-present-editing (resource stream))

(defgeneric game-resource-kind (resource)
  (:documentation "The Kind of a resource is its least-general type.")
  (:method ((resource game-resource-blob)) :blob)
  (:method ((resource game-resource-map)) :map)
  (:method ((resource game-resource-script)) :script)
  (:method ((resource game-resource-song)) :song)
  (:method ((resource game-resource-tileset)) :tileset)
  (:method ((resource game-resource-sprite-sheet)) :sprite-sheet)
  (:method ((resource game-resource-character)) :character)
  (:method ((resource game-resource-object-prototype)) :object-prototype)
  (:method ((resource game-resource-class)) :class)
  (:method ((resource game-resource-routine)) :routine)
  (:method ((resource game-resource-boat)) :boat)
  (:method ((resource game-resource-instrument)) :instrument)
  (:method ((resource game-resource-item)) :item)
  (:method ((resource game-resource-flag)) :flag)
  (:method ((resource game-resource-key)) :key)
  (:method ((resource game-resource-translation)) :translation))

(defgeneric game-resource-depends-upon-resources (resource))

(defvar *resource-scavenger-thread* nil)

(defgeneric resource-scavenger-functions ()
  (:documentation "Each resource scavenger function adds itself to the list by defining
a method on this generic function. This provides the list of all scavengers to be started by the main thread."))

;; Primary method returns nil (no scavengers)
(defmethod resource-scavenger-functions ()
  nil)

(defun resource-scavenger-thread ()
  (let ((scavs (resource-scavenger-functions)))
    (dolist (scav scavs)
      (when (functionp scav)
        (submit-task scav))))
  (loop (sleep 1)
        ;; FIXME ... interact with inotify/fsnotify and also event bus
        ))

(defun ensure-scavenger-thread-running ()
  (unless *resource-scavenger-thread*
    (setf *resource-scavenger-thread*
          (submit-task #'resource-scavenger-thread))))

(defun asset-index-scavenger ()
  "Read Assets.index and publish resource-added events for each entry."
  (read-assets-list)
  (maphash
   (lambda (moniker builds)
     (let* ((kind-name (when (asset-kind/name moniker)
                         (first (asset-kind/name moniker))))
            (kind (when kind-name
                    (kind-by-name kind-name)))
            (asset-type-p (member kind '(:map :script :song :blob)))
            (resource (when kind
                        (apply #'make-instance
                               (ecase kind
                                 (:atari-vox-dictionary 'game-resource-atari-vox-dictionary)
                                 (:blob 'game-resource-blob)
                                 (:boat 'game-resource-boat)
                                 (:character 'game-resource-character)
                                 (:class 'game-resource-class)
                                 (:flags 'game-resource-flag)
                                 (:instruments 'game-resource-instrument)
                                 (:items 'game-resource-item)
                                 (:keys 'game-resource-key)
                                 (:map 'game-resource-map)
                                 (:object-prototype 'game-resource-object-prototype)
                                 (:phrasebook 'game-resource-phrasebook)
                                 (:routine 'game-resource-routine)
                                 (:script 'game-resource-script)
                                 (:song 'game-resource-song)
                                 (:sprite-sheet 'game-resource-sprite-sheet)
                                 (:tileset 'game-resource-tileset))
                               (append
                                (list :kind kind
                                      :asset-id (ignore-errors (get-asset-id kind (second (asset-kind/name moniker))))
                                      :builds builds)
                                (when asset-type-p
                                  (list :moniker moniker)))))))
       (when resource
         (cache-add-resource kind resource)
         (publish-resource-added resource))))
   *assets-list*))

;; Add asset-index-scavenger to the scavenger list via :around method.
(defmethod resource-scavenger-functions :around ()
  (cons #'asset-index-scavenger (call-next-method)))

(defmethod open-resource-inspector ((resource game-resource-routine) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'gui-inspector-frame
                                 :resource (or resource (make-instance 'game-resource-routine))
                                :view-mode mode)))

(defmethod open-resource-inspector ((resource game-resource-routine-forth-library) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'gui-inspector-frame
                                 :resource (or resource (make-instance 'game-resource-routine-forth-library))
                                :view-mode mode)))

(defmethod open-resource-inspector ((resource game-resource-routine-run-commands) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'gui-inspector-frame
                                  :resource (or resource (make-instance 'game-resource-routine-run-commands))
                                 :view-mode mode)))

;; CLIM Display Interface Methods

(defgeneric game-resource-class-name (resource)
  (:documentation "Return CLIM-friendly class name for display")
  (:method ((resource game-resource))
    (title-case (string (class-name (class-of resource))))))

(defmethod game-resource-title ((resource game-resource-from-file))
  (title-case (pathname-name (first (game-resource-pathnames resource)))))

(defmethod game-resource-title ((resource game-resource-map))
  (or (game-resource-name resource)
      (title-case (pathname-name (first (game-resource-pathnames resource))))))

(defmethod game-resource-subheading ((resource game-resource))
  "")

(defmethod game-resource-subheading ((resource game-resource-map))
  (game-resource-notes resource))


