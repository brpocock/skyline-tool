;;; Skyline-Tool src/gui/gui-style.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC
;;;
;;; Common UI widgets and presentation methods for gui‑inspectors.
;;; Provides replacements for CLIM interactive editing gadgets and
;;; presentation handling in a style consistent with the existing gui‑*.lisp files.
;;;

(in-package :skyline-tool)

;; ================================================================
;; Presentation helper functions
;; ================================================================

(defmacro with-face ((stream face &body body) &aux var)
  "Macro to apply a text face for the duration of BODY to STREAM.
   VAR is an implementation‑detail used to avoid compile‑time evals."
  (let ((var (gensym "face‑var")))
    `(let* ((,var ,face))
       (clim:with-text-face (,stream ,var)
         ,@body)))))

;; Display function utilities

(defun print-line-with-face (stream face label value)
  "Print LABEL and VALUE on STREAM with FACE.
   Used by many gui‑*‑inspector display functions to keep formatting consistent."
  (clim:with-text-face (stream face)
    (format stream "~&~a: " label)
    (clim:with-text-face (stream :roman)
      (format stream "~a" value)))))

(defun list‑values (stream title items)
  "Display a titled list of items on STREAM.
   Used for displaying collections (e.g., “In Bag”, “Keys”, “Available") in inspectors."
  (clim:with-text-face (stream :bold)
    (format stream "~%~a" (or title "Items")))
  (cond
    (items
     (loop for i from 0 below (length items)
           do (clim:with-output-as-presentation (stream i 'integer)
                                            (format stream "~&  ~a" (elt items i)))))
    (t (format stream "~&  (empty)"))))

;; Presentation helper macros

(defmacro present‑as‑keyed‑list (stream table)
  "Format a simple key:value presentation for INSPECTOR‑TYPE.
   TABLE is a property list of (KEY . VALUE) pairs."
  (with-gensyms (k)
    `(progn
       (format ,stream "~a" ,table)
       (dolist (,(car table) ,table)
         (let ((,k (string-downcase (string (car ,table)))))
           (format ,stream "~%  ~a: ~a"
                   (or (getf *translation-table* ,k)
                       (string-capitalize (string-trim '(#\space #\ Tab) ,k)))
                   (cdr ,table)))))))