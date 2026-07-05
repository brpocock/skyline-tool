(in-package :skyline-tool)

(defun generate-default-moniker (class)
  "Generate a default moniker from the class name."
  (let ((class-name (symbol-name class)))
    (substitute #\_ #\- (string-downcase class-name))))

(defclass game-resource ()
  ((moniker :initarg :moniker :reader game-resource-moniker :initform nil)
   (kind :initarg :kind :reader game-resource-kind)))

(defmethod initialize-instance :after ((resource game-resource) &key)
  (when (null (slot-boundp resource 'moniker))
    (setf (slot-value resource 'moniker)
          (generate-default-moniker (class-of resource)))))

;; Resources that are stored in their own individual files
(defclass game-resource-from-file (game-resource)
  ((full-path :initarg :full-path :reader game-resource-full-path)))

;; Resources that are stored as records in a collective file (e.g. Tables)
(defclass game-resource-from-collective-file (game-resource)
  ((collective-path :initarg :collective-path :reader game-resource-collective-path)
   (offset :initarg :offset :reader game-resource-offset)))

;; Assets are a subset of from-file resources
(defclass game-resource-asset (game-resource-from-file)
  ((asset-id :initarg :asset-id :reader game-resource-asset-id)
   (builds :initarg :builds :reader game-resource-builds :initform nil)))

;; Asset Sub-hierarchy
(defclass game-resource-map (game-resource-asset)
  ((locale :initarg :locale :reader game-resource-locale)))

(defclass game-resource-script (game-resource-asset)
  ((locale :initarg :locale :reader game-resource-locale)))

(defclass game-resource-song (game-resource-asset) ())

(defclass game-resource-blob (game-resource-asset) ())

;; Example of other resource types using the new structural classes
(defclass game-resource-boat (game-resource-from-collective-file) ())
(defclass game-resource-instrument (game-resource-from-collective-file) ())
(defclass game-resource-item (game-resource-from-collective-file) ())
(defclass game-resource-flag (game-resource-from-collective-file) ())
(defclass game-resource-key (game-resource-from-collective-file) ())

(defclass game-resource-object-prototype (game-resource-from-file) ())

(defclass game-resource-character (game-resource-from-file) ())

(defclass game-resource-translation (game-resource-from-file) ())
(defclass game-resource-phonetic-dictionary (game-resource-translation) ())
(defclass game-resource-atari-vox-dictionary (game-resource-phonetic-dictionary) ())
(defclass game-resource-intellivoice-dictionary (game-resource-phonetic-dictionary) ())
(defclass game-resource-phrasebook (game-resource-translation) ())

(defclass game-resource-class (game-resource-from-file) ())

(defclass game-resource-tileset (game-resource-from-file) ())

(defclass game-resource-sprite-sheet (game-resource-from-file) ())

(defclass game-resource-routine (game-resource-from-file) ())
(defclass game-resource-routine-forth-library (game-resource-routine) ())
(defclass game-resource-routine-run-commands (game-resource-routine) ())
(defclass game-resource-routine-rc-cobol (game-resource-routine-run-commands) ())
(defclass game-resource-routine-rc-basic (game-resource-routine-run-commands) ())
(defclass game-resource-routine-rc-pascal (game-resource-routine-run-commands) ())

(clim:define-presentation-type game-resource-reference ()
  :inherit-from 'game-resource)

(clim:define-presentation-type game-resource-reading ()
  :inherit-from 'game-resource)

(clim:define-presentation-type game-resource-editing ()
  :inherit-from 'game-resource)

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
  (:method ((resource game-resource) stream)
    (terpri stream))
  (:method ((resource game-resource-asset) stream)
    (let ((builds (game-resource-builds resource)))
      (format stream " [~:[☐~;☑~]D ~:[☐~;☑~]P ~:[☐~;☑~]A]"
              (member "Demo" builds :test 'string-equal)
              (member "Public" builds :test 'string-equal)
              (member "AA" builds :test 'string-equal)))))

(defgeneric game-resource-present-right-margin (resource stream)
  (:documentation "Present right-margin content for reference presentation (e.g. D/P/A, asset ID).")
  
  (:method ((resource game-resource) stream)
    (let ((vc-status (vc-file-status (or (game-resource-full-path resource)
                                         (game-resource-collective-path resource)))))
      (format stream "~a~%" (game-resource-locator resource))
      (game-resource-present-build-checkboxes resource stream)
      (when vc-status
        (present-vc-status-icon stream vc-status)))))

(defmethod game-resource-locator ((resource game-resource))
  "")

(defmethod game-resource-locator ((resource game-resource-asset))
  (format nil "$~2,'0x" (game-resource-asset-id resource)))

(defmethod game-resource-locator ((resource game-resource-script))
  (format nil "$~4,'0x" (game-resource-asset-id resource)))

(defmethod game-resource-locator ((resource game-resource-tileset))
  (format nil "$~2,'0x:~4,'0x" :fixme :fixme))

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
          (full-path (if (typep resource 'game-resource-from-file)
                         (game-resource-full-path resource)
                         nil))
          (asset-id (if (typep resource 'game-resource-asset)
                        (game-resource-asset-id resource)
                        nil))
          (builds (if (typep resource 'game-resource-asset)
                      (game-resource-builds resource)
                      nil))
          (vc-status (vc-file-status (or (game-resource-full-path resource)
                                         (game-resource-collective-path resource))))
          (moniker (game-resource-moniker resource)))
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
        (when vc-status
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream :align-x :right)
              (format stream "VC Status: "))
            (clim:formatting-cell (stream :align-x :left)
              (princ vc-status stream))))
        ;; ERROR: ONLY for Assets, not other Resources.
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream :align-x :right)
            (format stream "Moniker: "))
          (clim:formatting-cell (stream :align-x :left)
            (princ moniker stream)))))))

(defgeneric present-editing (resource stream)
  (:documentation "Present resource in editing context with tabular layout: labels on left, editing gadgets on right.")
  (:method ((resource game-resource) stream)
    (let ((display-name (game-resource-title resource))
          (kind (game-resource-kind resource))
          (vc-status (vc-file-status (or (game-resource-full-path resource)
                                         (game-resource-collective-path resource))))
          (moniker (game-resource-moniker resource)))
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
              (format stream "~{~a~^, ~}" (game-resource-builds resource)))))
        (when vc-status
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream :align-x :right)
              (format stream "VC Status: "))
            (clim:formatting-cell (stream :align-x :left)
              (princ vc-status stream))))
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream :align-x :right)
            (format stream "Moniker: "))
          (clim:formatting-cell (stream :align-x :left)
            (princ moniker stream)))))))

(defun game-resource-to-json (resource)
  "Convert resource to JSON for detail view."
  (let ((obj (make-hash-table :test 'equal)))
    (setf (gethash "name" obj) (game-resource-title resource))
    (setf (gethash "kind" obj) (game-resource-kind resource))
    (when (typep resource 'game-resource-from-file)
      (setf (gethash "path" obj) (game-resource-full-path resource)))
    (when (typep resource 'game-resource-asset)
      (setf (gethash "assetId" obj) (game-resource-asset-id resource))
      (setf (gethash "builds" obj) (game-resource-builds resource)))
    obj))

(defun game-resource-to-postscript (resource)
  "Convert resource to PostScript for detail/reference view."
  (with-output-to-string (s)
    (format s "%% Resource: ~a~%" (game-resource-title resource))
    (format s "%% Kind: ~a~%" (game-resource-kind resource))))

(defun game-resource-to-text (resource)
  "Convert resource to plain text for reference/detail view."
  (error "unimplemented"))

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
   (view-mode :initform :reference :accessor frame-view-mode))
  (:documentation "Mixin for inspector frames that edit a Game-Resource.
Provides RESOURCE slot (accessed via INSPECTOR-RESOURCE) and VIEW-MODE slot.
Each resource type specializes PRESENT-REFERENCE, PRESENT-DETAIL, and
PRESENT-EDITING on its Game-Resource subclass to render the inspector body."))

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
                (game-resource-moniker resource)
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
      (format t "~&Saved ~a~%" (game-resource-moniker resource))
      (clim:redisplay-frame-panes frame))))

(clim:define-command (com-inspector-save-as :command-table resource-inspector-menu-bar
                                             :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (resource (and frame (inspector-resource frame))))
    (when resource
      (let ((new-path (clim:accept 'pathname :prompt "Save as:" :default (game-resource-full-path resource))))
        (when new-path
          (save-resource-as resource new-path)
          (format t "~&Saved ~a as ~a~%" (game-resource-moniker resource) new-path)
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
      (t 'game-resource))))

(defun make-game-resource (moniker builds kind-name
                           asset-id hex-str present-p full-path)
  "Create a game-resource object from collected asset data."
  (let* ((class (kind->resource-class kind-name))
         (initargs (list :moniker moniker :kind kind-name)))
    (cond
      ((subtypep class 'game-resource-asset)
       (let ((extra-args '()))
         (when (eql class 'game-resource-map)
           (let ((parts (split-sequence #\/ moniker)))
             (when (> (length parts) 2)
               (setf extra-args (list :locale (second parts))))))
         (when (eql class 'game-resource-script)
           (let ((parts (split-sequence #\/ moniker)))
             (when (> (length parts) 2)
               (setf extra-args (list :locale (second parts))))))
         (when (eql class 'game-resource-blob)
           (setf extra-args (list :machine (machine-directory-name))))
         (let ((base-args (list :full-path full-path
                                :asset-id asset-id
                                :hex-str hex-str
                                :builds builds)))
           (apply #'make-instance class
                  (append initargs base-args extra-args)))))
      ((subtypep class 'game-resource-from-file)
       (make-instance class :moniker moniker :kind kind-name :full-path full-path))
      ((subtypep class 'game-resource-from-collective-file)
        (make-instance class :moniker moniker :kind kind-name :collective-path full-path))
      (t
        (make-instance 'game-resource :moniker moniker :kind kind-name)))))

(defun fixme-interactive-editing-gadget-with-validation (stream resource slot)
  "Fallback: display the slot value as text since interactive editing isn't available."
  (format stream "~a" (slot-value resource slot)))

(defgeneric game-resource-pathnames (resource)
  (:documentation "Return a list of filesystem paths for RESOURCE based on its type."))

(defmethod game-resource-pathnames ((resource game-resource))
  "Default: treat as JSON file."
  (list (format nil "Source/~a.json" (game-resource-moniker resource))))

(defmethod game-resource-pathnames ((resource game-resource-script))
  (let* ((moniker (game-resource-moniker resource))
         (parts (split-sequence #\/ moniker))
         (region (if (< 1 (length parts))
                     (string-capitalize (second parts))
                     "NTSC")))
    (list (format nil "Source/Scripts/~a/~a.fountain"
                  region
                  (last parts)))))

(defmethod game-resource-pathnames ((resource game-resource-song))
  (let ((moniker (game-resource-moniker resource))
        (parts (split-sequence #\/ (game-resource-moniker resource))))
    (list (format nil "Source/Songs/~a.mscz" (last parts)))))

(defmethod game-resource-pathnames ((resource game-resource-map))
  (let* ((moniker (game-resource-moniker resource))
         (parts (split-sequence #\/ moniker))
         (region (if (< 2 (length parts))
                     (string-capitalize (second parts))
                     "NTSC")))
    (list (format nil "Source/Maps/~a/~a.tmx" region (last parts))
          "Source/Tables/MapsIndex.ods")))

(defmethod game-resource-pathnames ((resource game-resource-tileset))
  (let* ((moniker (game-resource-moniker resource))
         (parts (split-sequence #\/ moniker))
         (name (last parts)))
    (list (format nil "Source/Maps/Tiles/~a.tsx" name)
          (format nil "Source/Maps/Tiles/~a/~a.xcf"
                  (machine-directory-name)
                  name))))

(defmethod game-resource-pathnames ((resource game-resource-blob))
  (let ((moniker (game-resource-moniker resource))
        (parts (split-sequence #\/ (game-resource-moniker resource))))
    (list (format nil "Source/Blobs/~a/~a.xcf"
                  (machine-directory-name)
                  (last parts)))))

(defmethod game-resource-pathnames ((resource game-resource-sprite-sheet))
  (let ((moniker (game-resource-moniker resource))
        (parts (split-sequence #\/ (game-resource-moniker resource))))
    (list (format nil "Source/Art/~a.art" (last parts)))))

(defmethod game-resource-pathnames ((resource game-resource-character))
  (list "Source/Tables/NPCStats.ods"))

(defmethod game-resource-pathnames ((resource game-resource-object-prototype))
  (let ((moniker (game-resource-moniker resource))
        (parts (split-sequence #\/ (game-resource-moniker resource))))
    (list (format nil "Source/Objects/~a.json" (last parts)))))

(defmethod game-resource-pathnames ((resource game-resource-class))
  (let ((moniker (game-resource-moniker resource))
        (parts (split-sequence #\/ (game-resource-moniker resource))))
    (list (format nil "Source/Classes/~a.cob" (last parts)))))

(defmethod game-resource-pathnames ((resource game-resource-routine))
  (let* ((moniker (game-resource-moniker resource))
         (parts (split-sequence #\/ moniker))
         (name (last parts)))
    (list (format nil "Source/Routines/~a.bas" name)
          (format nil "Source/Routines/~a.pas" name)
          (format nil "Source/Routines/~a.cob" name))))

;; Items/Equipment/Flags/Keys all come from the same ODS file
(defmethod game-resource-pathnames ((resource game-resource-item))
  (list "Source/Tables/EquipmentIndex.ods"))
(defmethod game-resource-pathnames ((resource game-resource-flag))
  (list "Source/Tables/EquipmentIndex.ods"))
(defmethod game-resource-pathnames ((resource game-resource-key))
  (list "Source/Tables/EquipmentIndex.ods"))
(defmethod game-resource-pathnames ((resource game-resource-boat))
  (list (format nil "Source/SpecialResources/~a" (game-resource-moniker resource))))

(defmethod game-resource-pathnames ((resource game-resource-instrument))
  (list (format nil "Source/SpecialResources/~a" (game-resource-moniker resource))))

(defmethod game-resource-title ((resource game-resource-class))
  (format nil "Class: ~a" (game-resource-moniker resource)))

(defmethod game-resource-subheading ((resource game-resource-class))
  "COBOL Class Definition")

(defmethod game-resource-collective-path ((resource game-resource-class))
  "Source/Classes")

(defgeneric game-resource-title (resource))
(defgeneric game-resource-subheading (resource))
(defgeneric game-resource-full-text (resource)
  (:method ((resource game-resource))
    (concatenate 'string
                 (game-resource-title resource)
                 " "
                 (game-resource-subheading resource))))
(defgeneric game-resource-last-updated (resource))
(defgeneric game-resource-version-control-status (resource))
(defgeneric game-resource-locator (resource))
(defgeneric game-resource-asset-p (resource)
  (:method ((resource game-resource)) nil)
  (:method ((resource game-resource-asset)) t))
(defgeneric game-resource-present-icon (resource stream))
(defgeneric game-resource-present-reference (resource stream))
(defgeneric game-resource-present-editing (resource stream))
(defgeneric game-resource-kind (resource)
  (:method ((resource game-resource))
    (format nil "~{~:(~a~^ ~)~}" (subseq (split-sequence #\- (string (class-name (class-of resource)))) 2))))
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
          (make-thread #'resource-scavenger-thread
                       :name "Resource Scavenger Thread"))))

(defun asset-index-scavenger ()
  "Read Assets.index and publish resource-added events for each entry."
  (read-assets-list)
  (dolist (entry *assets-list*)
    (let* ((moniker (first entry))
           (builds (second entry))
           (kind-name (if (asset-kind/name moniker)
                          (first (asset-kind/name moniker))
                          nil))
           (kind (when kind-name
                   (kind-by-name kind-name)))
           (resource (when kind
                       (make-instance
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
                          (:song 'game-resource-song)
                          (:sprite-sheet 'game-resource-sprite-sheet)
                          (:tileset 'game-resource-tileset)
                          (:script 'game-resource-script))
                        :moniker moniker
                        :kind kind
                        :asset-id (ignore-errors (get-asset-id kind (second (asset-kind/name moniker))))
                        :builds builds))))
      (when resource
        (cache-add-resource kind resource)
        (publish-resource-added resource)))))

;; Add asset-index-scavenger to the scavenger list via :around method.
(defmethod resource-scavenger-functions :around ()
  (cons #'asset-index-scavenger (call-next-method)))

