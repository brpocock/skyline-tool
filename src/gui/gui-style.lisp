;;; Skyline-Tool src/gui/gui-style.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

;;; Common UI widgets, presentation methods, and style constants for all
;;; gui-* inspector and window frames.
;;; Provides shared colors, text styles, layout helpers, and the standard
;;; window title format.

(in-package :skyline-tool)

;; Shared Color Constants

(defparameter +navy-blue+ (clim:make-rgb-color 0 0 0.5)
  "Navy blue used for headings and branding.")

(defparameter +royal-blue+ (clim:make-rgb-color 0 0.2 0.6)
  "Royal blue used for the Skyline-Tool brand name.")

(defparameter +dark-gray+ (clim:make-gray-color 0.25)
  "75%% gray for subheadings and metadata text.")

(defparameter +missing-red+ (clim:make-rgb-color 0.8 0 0)
  "Red for resource titles missing from builds.")

;; Standard Window Title Format

(defun format-window-title (resource-name)
  "Format a window title bar as: $(Resource Name) — $(Game-Title) $(Machine-Directory-Name)
RESOURCE-NAME is a string; GAME-TITLE and MACHINE-DIRECTORY-NAME come from globals."
  (format nil "~a — ~a (~a)"
          resource-name
          (string-capitalize *game-title*)
          (machine-directory-name)))

(defun format-window-title-from-resource (resource)
  "Format a window title from a RESOURCE object.
Returns just the resource title when RESOURCE is non-nil, or a fallback string."
  (if resource
      (format-window-title (game-resource-title resource))
      "Skyline-Tool Resource Inspector"))

(defun format-vc-window-title (thing)
  "Format a version-control window title as: $(Thing) - Skyline-Tool for *Game-Title* (Machine-Directory-Name)"
  (format nil "~a - Skyline-Tool for ~a (~a)"
          thing
          (string-capitalize *game-title*)
          (machine-directory-name)))

;; Presentation helper functions

(defmacro with-face ((stream face &body body) &aux (var (gensym "face-var")))
  "Macro to apply a text face for the duration of BODY to STREAM.
   VAR is an implementation-detail used to avoid compile-time evals."
  `(let* ((,var ,face))
     (clim:with-text-face (,stream ,var)
       ,@body)))

(defun print-line-with-face (stream face label value)
  "Print LABEL and VALUE on STREAM with FACE.
   Used by many gui-*-inspector display functions to keep formatting consistent."
  (clim:with-text-face (stream face)
    (format stream "~&~a: " label)
    (clim:with-text-face (stream :roman)
      (format stream "~a" value))))

(defun list-values (stream title items)
  "Display a titled list of items on STREAM.
   Used for displaying collections (e.g., 'In Bag', 'Keys', 'Available') in inspectors."
  (clim:with-text-face (stream :bold)
    (format stream "~%~a" (or title "Items")))
  (cond
    (items
     (loop for i from 0 below (length items)
           do (clim:with-output-as-presentation (stream i 'integer)
                                            (format stream "~&  ~a" (elt items i)))))
    (t (format stream "~&  (empty)"))))

(defun present-resource-title (stream resource)
  "Present the resource title with optional red highlight when missing from builds."
  (let ((builds (when (typep resource 'game-resource-asset)
                  (game-resource-builds resource))))
    (clim:with-drawing-options (stream :ink (if (and builds (zerop builds))
                                                +missing-red+
                                                clim:+black+))
(clim:with-text-face (stream :roman)
         (format stream "~a" (game-resource-title resource))))))

(defun present-resource-subheading (stream resource)
  "Present the resource subheading (kind / locator) in 75%% gray."
  (clim:with-drawing-options (stream :ink +dark-gray+)
    (clim:with-text-size (stream :smaller)
      (format stream "~a" (game-resource-kind resource))
      (let ((locator (ignore-errors (game-resource-locator resource))))
        (when locator
          (format stream " | ~a" locator))))))

;; Presentation helper macros

(defmacro present-as-keyed-list (stream table)
  "Format a simple key:value presentation for INSPECTOR-TYPE.
   TABLE is a property list of (KEY . VALUE) pairs."
  (with-gensyms (k)
    `(progn
       (fresh-line ,stream)
       (clim:with-text-size (,stream :larger)
         (format ,stream "~a" ,table))
       (clim:formatting-table (,stream)
         (dolist (,(car table) ,table)
           (let ((,k (string-downcase (string (car ,table)))))
             (clim:formatting-row (,stream)
               (clim:formatting-cell (,stream)
                 (format ,stream "~a:"
                         (or (getf *translation-table* ,k)
                             (string-capitalize (string-trim +whitespace+ ,k)))))
               (clim:formatting-cell (,stream)
                 (format ,stream "~a" (cdr ,table)))))))
       (fresh-line ,stream))))

(defun make-status-bar-text (frame pane resource)
  "Build a standardized status bar line for an inspector frame.
Format: 'Kind | Title | VC: status | View: mode'"
  (let ((vc-status (remove-duplicates (sort (mapcar #'vc-file-status
                                                    (game-resource-pathnames resource))
                                            #'string-lessp)
                                      :test #'string-equal)))
    (format nil "~a | ~a | VC: ~a | View: ~a"
            (game-resource-kind resource)
            (game-resource-title resource)
            (or vc-status "unknown")
            (string-downcase (symbol-name (frame-view-mode frame))))))
