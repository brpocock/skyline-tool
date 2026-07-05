;;; Skyline-Tool src/gui/gui-resource-chooser.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

;; ============================================================================
;; Global state for synchronizing group expansion across all chooser windows
;; ============================================================================

(defvar *chooser-group-states* (make-hash-table :test 'equal)
  "Hash table mapping group paths to their expanded/collapsed state.")

(defun chooser-group-expanded-p (group-path)
  "Check if GROUP-PATH is currently expanded."
  (gethash group-path *chooser-group-states* t))

(defun set-chooser-group-expanded-p (group-path expanded)
  "Set the expansion state for GROUP-PATH."
  (setf (gethash group-path *chooser-group-states*) expanded)
  ;; Notify all open choosers to refresh
  (dolist (frame (clim:frame-list-all-frames 'resource chooser-frame))
    (clim:redisplay-frame-panes frame :force-p t)))

;; ============================================================================
;; Resource Chooser Frame Definition
;; ============================================================================

(clim:define-application-frame resource-chooser-frame (gui-inspector-frame)
  ((resource-type :initarg :resource-type :reader chooser-resource-type)
   (filter-function :initarg :filter-function :reader chooser-filter-function)
   (selection-callback :initarg :selection-callback :reader chooser-selection-callback)
   (scavenger :initform (make-hash-table :test 'equal) :accessor chooser-scavenger))
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
  (:layouts (default (clim:vertically () list-pane status-bar)))
  (:menu-bar chooser-menu-bar))

;; ============================================================================
;; Menu Definitions
;; ============================================================================

(clim:define-command-table chooser-menu-bar
  :menu (("File" :menu chooser-file-menu)
         ("Edit" :menu chooser-edit-menu)
         ("View" :menu chooser-view-menu)
         ("Help" :menu chooser-help-menu)))

(clim:define-command-table chooser-file-menu
  :menu (("Select" :command com-chooser-select-current)
         (nil :divider :line)
         ("Close" :command com-close-frame)))

(clim:define-command-table chooser-edit-menu
  :menu (("Cut" :command chooser-cut)
         ("Copy" :command chooser-copy)
         ("Paste" :command chooser-paste)))

(clim:define-command-table chooser-view-menu
  :menu (("Expand All" :command com-chooser-expand-all)
         ("Collapse All" :command com-chooser-collapse-all)))

(clim:define-command-table chooser-help-menu
  :menu (("About Resource Chooser" :command com-chooser-about)))

;; ============================================================================
;; Generic Functions for Subclasses to Override
;; ============================================================================

(defgeneric list-available-resources (frame)
  (:documentation "Return a list of available Game-Resource objects for FRAME."))

(defgeneric group-resources (resources)
  (:documentation "Group RESOURCES into a hierarchical structure.
Returns a list of (GROUP-NAME . SUBGROUPS/RESOURCES) alists."))

(defgeneric present-resource-in-chooser (resource stream)
  (:documentation "Present RESOURCE in the chooser list pane."))

;; ============================================================================
;; Display Functions
;; ============================================================================

(defun display-resource-chooser-list (frame pane)
  "Display the hierarchical resource list with groupings."
  (let ((resources (list-available-resources frame)))
    (when resources
      (let ((groups (group-resources resources)))
        (labels ((present-group (group-path group-name items)
                   (let ((expanded-p (chooser-group-expanded-p group-path)))
                     (clim:with-text-face (pane :bold)
                       (format pane "~a~%" 
                               (if expanded-p "▼ " "► ")
                               group-name))
                     (when expanded-p
                       (dolist (item items)
                         (if (consp item)
                             (present-group (cons group-path (car item))
                                           (car item)
                                           (cdr item))
                             (present-resource-in-chooser item pane))
                         (terpri pane))))))
          (dolist (top-group groups)
            (present-group (list (car top-group))
                          (car top-group)
                          (cdr top-group))))))))

(defun display-chooser-status (frame pane)
  "Display status line with resource count."
  (let ((resources (list-available-resources frame)))
    (format pane "Resources: ~d | Type: ~a"
            (length resources)
            (chooser-resource-type frame))))

;; ============================================================================
;; Presentation Methods for Click Handling
;; ============================================================================

(clim:define-presentation-type chooser-resource ()
  :inherit-from 'game-resource)

(clim:define-presentation-method clim:present
    ((resource game-resource) (type chooser-resource) stream view &key)
  (declare (ignore view))
  (present-resource-in-chooser resource stream))

(clim:define-presentation-to-command-translator click-to-select-resource
    (chooser-resource com-chooser-select-current resource-chooser-frame
     :gesture :select :documentation "Choose this resource")
    (resource)
  (list resource))

(clim:define-presentation-to-command-translator click-to-edit-resource
    (chooser-resource com-chooser-edit-current resource-chooser-frame
     :gesture :menu :documentation "Edit this resource")
    (resource)
  (list resource))

;; ============================================================================
;; Commands
;; ============================================================================

(clim:define-command (com-chooser-select-current :command-table clim-internals::global-command-table
                                               :menu t :name t)
    ((resource 'chooser-resource))
  ()
  "Select RESOURCE and close the chooser window."
  (let ((callback (chooser-selection-callback clim:*application-frame*)))
    (when callback
      (funcall callback resource))
    (clim:frame-exit clim:*application-frame*)))

(clim:define-command (com-chooser-edit-current :command-table clim-internals::global-command-table
                                               :menu t :name t)
    ((resource 'chooser-resource))
  ()
  "Edit RESOURCE in an inspector window."
  (open-resource-inspector resource :mode :editing))

(clim:define-command (com-chooser-expand-all :command-table clim-internals::global-command-table
                                             :menu t :name t)
    ()
  ()
  "Expand all groups in the resource list."
  (labels ((expand-all-groups (groups path-prefix)
             (dolist (group groups)
               (let ((full-path (cons path-prefix (list (car group)))))
                 (set-chooser-group-expanded-p full-path t)
                 (expand-all-groups (cdr group) full-path)))))
    (expand-all-groups (group-resources (list-available-resources clim:*application-frame*)) nil))
  (clim:redisplay-frame-panes clim:*application-frame* :force-p t))

(clim:define-command (com-chooser-collapse-all :command-table clim-internals::global-command-table
                                               :menu t :name t)
    ()
  ()
  "Collapse all groups in the resource list."
  (clrhash *chooser-group-states*)
  (clim:redisplay-frame-panes clim:*application-frame* :force-p t))

(clim:define-command (com-chooser-about :command-table clim-internals::global-command-table
                                       :menu t :name t)
    ()
  ()
  "Show information about the Resource Chooser.")

;; ============================================================================
;; Scavenger Integration
;; ============================================================================

(defun start-chooser-scavenger (frame)
  "Start monitoring for resource changes and updating the chooser."
  (let ((resources (list-available-resources frame)))
    (make-thread
     (lambda ()
       (loop
         (sleep 2)
         (let ((new-resources (list-available-resources frame)))
           (unless (equal new-resources resources)
             (setf resources new-resources)
             (clim:redisplay-frame-panes frame :force-p t)))))
     :name "chooser-scavenger")))

;; ============================================================================
;; Convenience Constructors
;; ============================================================================

(defun open-script-chooser (&key (callback #'identity) (title "Script Chooser"))
  "Open a chooser window for scripts.
CALLBACK receives the selected script resource.
TITLE is the window title."
  (let ((frame (clim:make-application-frame 'script-chooser-frame
                                          :selection-callback callback
                                          :filter-function (lambda (r) (typep r 'game-resource-script)))))
    (setf (clim:frame-pretty-name frame) title)
    (clim:run-frame-top-level frame)))

(defun open-song-chooser (&key (callback #'identity) (title "Song Chooser"))
  "Open a chooser window for songs."
  (let ((frame (clim:make-application-frame 'song-chooser-frame
                                          :selection-callback callback
                                          :filter-function (lambda (r) (typep r 'game-resource-song)))))
    (setf (clim:frame-pretty-name frame) title)
    (clim:run-frame-top-level frame)))

;; ============================================================================
;; Script Chooser (specialized from Resource Chooser)
;; ============================================================================

(clim:define-application-frame script-chooser-frame (resource-chooser-frame)
  ()
  (:default-initargs
   :resource-type 'game-resource-script
   :title "Script Chooser"))

(defmethod list-available-resources ((frame script-chooser-frame))
  "List all game-resource-script objects, filtered and sorted."
  (let ((scripts (remove-if-not #'game-resource-script-p
                                (copy-list (hash-table-keys (read-assets-list))))))
    (sort scripts #'string< :key #'game-resource-moniker)))

(defmethod group-resources ((frame script-chooser-frame) resources)
  "Group scripts by locale (area)."
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (script resources)
      (let* ((moniker (game-resource-moniker script))
             (parts (split-sequence #\/ moniker))
             (locale (if (> (length parts) 2)
                         (string-capitalize (second parts))
                         "Global")))
        (push script (gethash locale groups))))
    (let (result)
      (maphash (lambda (locale scripts)
                 (push (cons locale (nreverse scripts)) result))
               groups)
      (sort result #'string-lessp :key #'car))))

(defmethod present-resource-in-chooser ((resource game-resource-script) stream)
  "Present a script in the chooser list."
  (clim:with-text-face (stream (if (search "Global/" (game-resource-moniker resource))
                                  :bold :roman))
    (format stream "~4t~a: "~ (cl-change-case:title-case (get-script-area resource)))
    (format stream "~a" (cl-change-case:title-case (get-script-name resource)))))

;; ============================================================================
;; Song Chooser (specialized from Resource Chooser)
;; ============================================================================

(clim:define-application-frame song-chooser-frame (resource-chooser-frame)
  ()
  (:default-initargs
   :resource-type 'game-resource-song
   :title "Song Chooser"))

(defmethod list-available-resources ((frame song-chooser-frame))
  "List all game-resource-song objects."
  (let ((songs (remove-if-not #'game-resource-song-p
                              (copy-list (hash-table-keys (read-assets-list))))))
    (sort songs #'string< :key #'game-resource-moniker)))

(defmethod group-resources ((frame song-chooser-frame) resources)
  "Group songs by type (BGM, SFX, etc.)."
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (song resources)
      (let ((type (or (game-resource-song-type song) "Unknown")))
        (push song (gethash type groups))))
    (let (result)
      (maphash (lambda (type songs)
                 (push (cons type (nreverse songs)) result))
               groups)
      (sort result #'string-lessp :key #'car))))

(defmethod present-resource-in-chooser ((resource game-resource-song) stream)
  "Present a song in the chooser list."
  (format stream "~4t~a (~a)"
          (cl-change-case:title-case (game-resource-title resource))
          (game-resource-song-duration resource)))

;; ============================================================================
;; Helper Functions for Script Parsing
;; ============================================================================

(defun get-script-area (script)
  "Extract the area name from a script's moniker."
  (let ((parts (split-sequence #\/ (game-resource-moniker script))))
    (if (> (length parts) 2)
        (second parts)
        "Global")))

(defun get-script-name (script)
  "Extract the script name from its moniker."
  (let ((parts (split-sequence #\/ (game-resource-moniker script))))
    (if (> (length parts) 2)
        (first (last parts))
        (first parts))))

(defun game-resource-script-p (resource)
  "Check if RESOURCE is a game-resource-script."
  (typep resource 'game-resource-script))

(defun game-resource-song-p (resource)
  "Check if RESOURCE is a game-resource-song."
  (typep resource 'game-resource-song))