;;; Skyline-Tool src/gui/gui-resource-chooser.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

;; Group State Persistence
;; Groups default closed; state persists via preferences.

(defvar *chooser-group-states* (make-hash-table :test 'equal)
  "Hash table mapping group paths (lists of strings) to their expanded/collapsed state.")

(defun chooser-group-expanded-p (group-path)
  (gethash group-path *chooser-group-states* nil))

(defun set-chooser-group-expanded-p (group-path expanded)
  (setf (gethash group-path *chooser-group-states*) expanded)
  (persist-chooser-group-states)
  (dolist (frame (clim-frame-list-all-frames 'resource-chooser-frame))
    (clim:redisplay-frame-panes frame :force-p t)))

(defun persist-chooser-group-states ()
  (loop for k being the hash-keys of *chooser-group-states*
        do (setf (get-pref (list :group k)) (gethash k *chooser-group-states*))))

(defun load-chooser-group-states ()
  (let ((saved (get-pref :chooser-group-states)))
    (when saved
      (clrhash *chooser-group-states*)
      (dolist (pair saved)
        (setf (gethash (car pair) *chooser-group-states*) (cdr pair))))))

;; Resource-type proper name helpers

(defun resource-type-name (class)
  (let ((name (string (class-name class))))
    (string-capitalize
     (if (string-prefix-p "GAME-RESOURCE-" name)
         (subseq name (length "GAME-RESOURCE-"))
         name))))

(defun resource-type-icon-key (class)
  (intern (string-upcase (resource-type-name class)) :keyword))

;; Resource Chooser Frame

(clim:define-application-frame resource-chooser-frame (gui-inspector-frame clim:standard-application-frame)
  ((resource-type :initarg :resource-type :reader chooser-resource-type)
   (filter-function :initarg :filter-function :reader chooser-filter-function :initform nil)
   (selection-callback :initarg :selection-callback :reader chooser-selection-callback)
   (event-subscriptions :initform nil :accessor chooser-event-subscriptions))
  (:panes
   (list-pane :application
              :display-function 'display-resource-chooser-list
              :scroll-bars :vertical
              :height 600
              :width 800)
   (status-bar :application
               :display-function 'display-chooser-status
               :height 30
               :width 800))
  (:layouts
   (default (clim:vertically () list-pane status-bar)))
  (:menu-bar chooser-menu-bar))

;; Menu Bar (mirrors inspector standard with build/region sync)

(clim:define-command-table chooser-file-menu
  :menu (("New..." :command com-chooser-new-resource)
         (nil :divider :line)
         ("Close" :command com-close-frame)))

(clim:define-command-table chooser-edit-menu
  :menu (("Find..." :command com-find)))

(clim:define-command-table chooser-view-menu
  :menu (("Project Pane" :command com-toggle-project-pane :toggle t)))

(clim:define-command-table chooser-help-menu
  :menu (("How to Manage Resources..." :command com-help-for-window)
         (nil :divider :line)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table chooser-menu-bar
  :menu (("Resource" :menu chooser-file-menu)
         ("Edit" :menu chooser-edit-menu)
         ("View" :menu chooser-view-menu)
         ("Help" :menu chooser-help-menu)))

;; Frame Lifecycle

(defmethod initialize-instance :after ((frame resource-chooser-frame) &key)
  (load-chooser-group-states)
  (let ((type (chooser-resource-type frame)))
    (setf (clim:frame-pretty-name frame)
          (format nil "~a Chooser — Skyline-Tool"
                  (resource-type-name type)))
    (ignore-errors
     (setf (frame-icon frame)
           (skyline-tool-icon :resource (resource-type-icon-key type)))))
  (setf (chooser-event-subscriptions frame)
        (setup-frame-eventbus-subscriptions
         frame
         '(:resource-added :resource-changed :resource-removed
           :resource-cache-dump :resource-scan-complete)
         (lambda (event)
           (declare (ignore event))
           (ignore-errors
            (clim:redisplay-frame-panes frame :force-p t))))))

(defmethod finalize-instance :after ((frame resource-chooser-frame))
  (teardown-frame-eventbus-subscriptions
   '(:resource-added :resource-changed :resource-removed
     :resource-cache-dump :resource-scan-complete)
   (chooser-event-subscriptions frame)))

;; Generic Functions (designed for any game-resource subclass)

(defgeneric list-available-resources (frame)
  (:documentation "Return a filtered list of available Game-Resource objects."))

(defgeneric group-resources (frame resources)
  (:documentation "Group RESOURCES into a hierarchical alist for display.
Returns a list of (GROUP-NAME . SUBGROUPS) where each SUBGROUP is either
a resource or a nested (SUBGROUP-NAME . items)."))

(defgeneric present-resource-in-chooser (resource stream)
  (:documentation "Present RESOURCE in the chooser list pane."))

;; Display Functions

(defun display-resource-chooser-list (frame pane)
  (let* ((filter (chooser-filter-function frame))
         (raw (list-available-resources frame))
         (resources (if filter (remove-if-not filter raw) raw)))
    (when resources
      (let ((groups (group-resources frame resources)))
        (labels ((present-group (group-path group-name items)
                   (let ((expanded-p (chooser-group-expanded-p group-path)))
                     (clim:with-output-as-presentation
                         (pane (format nil "~{~a~^/~}" group-path)
                               'chooser-group-header)
                       (clim:with-text-face (pane :bold)
                         (format pane "~a ~a~%"
                                 (if expanded-p "▾" "▸") ; spinner arrows
                                 group-name)))
                     (when expanded-p
                       (dolist (item items)
                         (if (consp item)
                             (present-group (append group-path (list (car item)))
                                            (car item)
                                            (cdr item))
                             (present-resource-in-chooser item pane))
                         (terpri pane))))))
          (dolist (top-group groups)
            (present-group (list (car top-group))
                           (car top-group)
                           (cdr top-group))))))))

(defun display-chooser-status (frame pane)
  (let* ((filter (chooser-filter-function frame))
         (raw (list-available-resources frame))
         (resources (if filter (remove-if-not filter raw) raw)))
    (format pane "~a Chooser — ~d resource~:p"
            (resource-type-name (chooser-resource-type frame))
            (length resources))))

;; Presentation Types and Click/Context-Menu Handling

(clim:define-presentation-type chooser-group-header ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'chooser-group-header)))
  (stringp object))

(clim:define-presentation-type chooser-resource ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'chooser-resource)))
  (typep object 'game-resource))

(clim:define-presentation-method clim:present
    ((resource game-resource) (type chooser-resource) stream view &key)
  (declare (ignore view))
  (present-resource-in-chooser resource stream))

;; Left-click selects and closes
(clim:define-presentation-to-command-translator click-to-choose-resource
    (chooser-resource com-chooser-select-current resource-chooser-frame
     :gesture :select :documentation "Choose this resource")
    (resource)
  (list resource))

;; Right-click context menu: first item "Inspect..."
(clim:define-presentation-to-command-translator click-to-context-menu-resource
    (chooser-resource com-chooser-resource-context-menu resource-chooser-frame
     :gesture :menu :documentation "Context menu")
    (resource)
  (list resource))

;; Click on group header toggles expansion
(clim:define-presentation-to-command-translator click-to-toggle-group
    (chooser-group-header com-chooser-toggle-group resource-chooser-frame
     :gesture :select :documentation "Toggle group expansion")
    (group-path)
  (list group-path))

;; Right-click on group header also shows context menu
(clim:define-presentation-to-command-translator click-to-group-context-menu
    (chooser-group-header com-chooser-group-context-menu resource-chooser-frame
     :gesture :menu :documentation "Group context menu")
    (group-path)
  (list group-path))

;; Commands

(clim:define-command (com-chooser-select-current :command-table clim-internals::global-command-table
                                                 :menu t :name t)
    ((resource 'chooser-resource))
  ()
  (let ((callback (chooser-selection-callback clim:*application-frame*)))
    (when callback
      (funcall callback resource))
    (clim:frame-exit clim:*application-frame*)))

(clim:define-command (com-chooser-resource-context-menu :command-table clim-internals::global-command-table
                                                        :menu t :name t)
    ((resource 'chooser-resource))
  ()
  (let ((menu
         (list
          (list "Inspect..."
                (lambda ()
                  (open-resource-inspector resource :mode :editing)))
          (list "Open in File Manager..."
                (lambda ()
                  (let ((path (first (game-resource-pathnames resource))))
                    (when path
                      (uiop:run-program
                       (list "xdg-open" (namestring (make-pathname :name nil :type nil :defaults path)))
                       :output nil))))))))
    (let ((choice (clim:menu-choose
                   (mapcar (lambda (item)
                             (list (first item) :value (second item)))
                           menu)
                   :label "Actions")))
      (when choice (funcall choice)))))

(clim:define-command (com-chooser-toggle-group :command-table clim-internals::global-command-table
                                               :menu t :name t)
    ((group-path 'chooser-group-header))
  ()
  (set-chooser-group-expanded-p group-path
                                (not (chooser-group-expanded-p group-path))))

(clim:define-command (com-chooser-group-context-menu :command-table clim-internals::global-command-table
                                                     :menu t :name t)
    ((group-path 'chooser-group-header))
  ()
  (let ((menu
         (list
          (list (format nil "~:[Expand~;Collapse~] ~a"
                        (chooser-group-expanded-p group-path)
                        (first (last group-path)))
                (lambda ()
                  (set-chooser-group-expanded-p group-path
                                                (not (chooser-group-expanded-p group-path))))))))
    (let ((choice (clim:menu-choose
                   (mapcar (lambda (item)
                             (list (first item) :value (second item)))
                           menu)
                   :label "Group")))
      (when choice (funcall choice)))))

;; New resource command — creates a new instance of the chooser's type
(clim:define-command (com-chooser-new-resource :command-table clim-internals::global-command-table
                                               :menu t :name t)
    ()
  ()
  (let* ((frame clim:*application-frame*)
         (type (chooser-resource-type frame))
         (class (if (symbolp type)
                    (find-class type)
                    type)))
    (open-resource-inspector (make-instance class) :mode :editing)))

;; Generic open-resource-chooser
;; Creates a chooser for any game-resource subclass, runs in its own thread.

(defgeneric open-resource-chooser (resource-type &key filter-function selection-callback title)
  (:documentation
   "Open a resource chooser for RESOURCE-TYPE (a class or keyword).
FILTER-FUNCTION (if provided) is called on each resource to decide inclusion.
SELECTION-CALLBACK receives the chosen resource.
TITLE overrides the default window title."))

(defmethod open-resource-chooser (resource-type &key
                                                  (filter-function nil)
                                                  (selection-callback #'identity)
                                                  (title nil))
  (let* ((class (if (symbolp resource-type)
                    (find-class resource-type)
                    resource-type))
         (display-name (resource-type-name class))
         (frame-title (or title (format nil "~a Chooser" display-name)))
         (frame (clim:make-application-frame
                 'resource-chooser-frame
                 :resource-type class
                 :filter-function filter-function
                 :selection-callback selection-callback
                 :pretty-name (format nil "~a — Skyline-Tool" frame-title))))
    (make-window-thread (format nil "Chooser: ~a" display-name)
                        (lambda ()
                          (clim:run-frame-top-level frame)))
    frame))

;; Resource-Listing Methods (cached from the global resource cache)

(defmethod list-available-resources ((frame resource-chooser-frame))
  (let* ((type (chooser-resource-type frame))
         (class (if (symbolp type) (find-class type) type)))
    (loop for k being the hash-keys of *all-resources-cache*
          nconc (remove-if-not (lambda (r) (typep r class))
                               (gethash k *all-resources-cache*)))))

(defmethod group-resources ((frame resource-chooser-frame) resources)
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (r resources)
      (let ((kind (game-resource-kind r)))
        (push r (gethash kind groups))))
    (let (result)
      (maphash (lambda (kind items)
                 (push (cons (string-capitalize (string kind))
                             (sort (copy-list items) #'string<
                                   :key (lambda (r)
                                          (or (ignore-errors (game-resource-title r))
                                              (princ-to-string r)))))
                       result))
               groups)
      (sort result #'string-lessp :key #'car))))

(defmethod present-resource-in-chooser ((resource game-resource) stream)
  (clim:with-output-as-presentation
      (stream resource 'chooser-resource)
    (format stream "~4t~a" (game-resource-title resource))
    (let ((locator (ignore-errors (game-resource-locator resource))))
      (when locator
        (clim:with-text-face (stream :roman)
          (format stream "  [~a]" locator))))))

;; Convenience Constructors for Common Resource Types
;; Each runs in its own thread and supports event-bus live updates.

(defun open-character-chooser (&key (callback #'identity) (title "Character Chooser"))
  (open-resource-chooser 'game-resource-character
                         :filter-function (lambda (r) (typep r 'game-resource-character))
                         :selection-callback callback
                         :title title))

(defun open-script-chooser (&key (callback #'identity) (title "Script Chooser"))
  (open-resource-chooser 'game-resource-script
                         :filter-function (lambda (r) (typep r 'game-resource-script))
                         :selection-callback callback
                         :title title))

(defun open-song-chooser (&key (callback #'identity) (title "Song Chooser"))
  (open-resource-chooser 'game-resource-song
                         :filter-function (lambda (r) (typep r 'game-resource-song))
                         :selection-callback callback
                         :title title))

(defun open-map-chooser (&key (callback #'identity) (title "Map Chooser"))
  (open-resource-chooser 'game-resource-map
                         :filter-function (lambda (r) (typep r 'game-resource-map))
                         :selection-callback callback
                         :title title))

(defun open-blob-chooser (&key (callback #'identity) (title "BLOB Chooser"))
  (open-resource-chooser 'game-resource-blob
                         :filter-function (lambda (r) (typep r 'game-resource-blob))
                         :selection-callback callback
                         :title title))

(defun open-sprite-sheet-chooser (&key (callback #'identity) (title "Sprite Sheet Chooser"))
  (open-resource-chooser 'game-resource-sprite-sheet
                         :filter-function (lambda (r) (typep r 'game-resource-sprite-sheet))
                         :selection-callback callback
                         :title title))

(defun open-tileset-chooser (&key (callback #'identity) (title "Tileset Chooser"))
  (open-resource-chooser 'game-resource-tileset
                         :filter-function (lambda (r) (typep r 'game-resource-tileset))
                         :selection-callback callback
                         :title title))

(defun open-class-chooser (&key (callback #'identity) (title "Class Chooser"))
  (open-resource-chooser 'game-resource-class
                         :filter-function (lambda (r) (typep r 'game-resource-class))
                         :selection-callback callback
                         :title title))

(defun open-boat-chooser (&key (callback #'identity) (title "Boat Chooser"))
  (open-resource-chooser 'game-resource-boat
                         :filter-function (lambda (r) (typep r 'game-resource-boat))
                         :selection-callback callback
                         :title title))

(defun open-instrument-chooser (&key (callback #'identity) (title "Instrument Chooser"))
  (open-resource-chooser 'game-resource-instrument
                         :filter-function (lambda (r) (typep r 'game-resource-instrument))
                         :selection-callback callback
                         :title title))

(defun open-item-chooser (&key (callback #'identity) (title "Item Chooser"))
  (open-resource-chooser 'game-resource-item
                         :filter-function (lambda (r) (typep r 'game-resource-item))
                         :selection-callback callback
                         :title title))

(defun open-flag-chooser (&key (callback #'identity) (title "Flag Chooser"))
  (open-resource-chooser 'game-resource-flag
                         :filter-function (lambda (r) (typep r 'game-resource-flag))
                         :selection-callback callback
                         :title title))

(defun open-key-chooser (&key (callback #'identity) (title "Key Chooser"))
  (open-resource-chooser 'game-resource-key
                         :filter-function (lambda (r) (typep r 'game-resource-key))
                         :selection-callback callback
                         :title title))

(defun open-object-prototype-chooser (&key (callback #'identity) (title "Object Prototype Chooser"))
  (open-resource-chooser 'game-resource-object-prototype
                         :filter-function (lambda (r) (typep r 'game-resource-object-prototype))
                         :selection-callback callback
                         :title title))

(defun open-routine-chooser (&key (callback #'identity) (title "Routine Chooser"))
  (open-resource-chooser 'game-resource-routine
                         :filter-function (lambda (r) (typep r 'game-resource-routine))
                         :selection-callback callback
                         :title title))

(defun open-phrasebook-chooser (&key (callback #'identity) (title "Translation Chooser"))
  (open-resource-chooser 'game-resource-phrasebook
                         :filter-function (lambda (r) (typep r 'game-resource-phrasebook))
                         :selection-callback callback
                         :title title))

(defun open-atari-vox-chooser (&key (callback #'identity) (title "AtariVox Dictionary Chooser"))
  (open-resource-chooser 'game-resource-atari-vox-dictionary
                         :filter-function (lambda (r) (typep r 'game-resource-atari-vox-dictionary))
                         :selection-callback callback
                         :title title))

(defun open-intellivoice-chooser (&key (callback #'identity) (title "IntelliVoice Dictionary Chooser"))
  (open-resource-chooser 'game-resource-intellivoice-dictionary
                         :filter-function (lambda (r) (typep r 'game-resource-intellivoice-dictionary))
                         :selection-callback callback
                         :title title))
