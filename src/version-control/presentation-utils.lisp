;;; src/version-control/presentation-utils.lisp
;;; Utilities for presenting version control status in Skyline-Tool presentations

(in-package :skyline-tool.version-control)

;;
;; Status Detection
;;

(defun version-control-file-status (file-path)
  "Return the version control status of FILE-PATH.
Returns a keyword: :absent, :current, :staged, :modified, :untracked, :ignored.
Uses the detected backend (Git preferred)."
  (let ((backend (detect-version-control-backend file-path)))
    (cond
      ((null backend) :absent) ; No VC detected
      ((eq backend :git)
       (version-control-git-file-status file-path))
      ((eq backend :hg)
       (version-control-hg-file-status file-path))
      ((eq backend :svn)
       (version-control-svn-file-status file-path))
      ((eq backend :bzr)
       (version-control-bzr-file-status file-path))
      (t :absent))))

;;
;; Status Icons (small, for Reference form)
;;

(defun version-control-status-icon (status)
  "Return a plist with :color, :shape and :symbol for STATUS.
Symbol uses Unicode characters for visual cue."
  (ecase status
    (:absent  '(:color "0.8 0 0" :shape :x      :symbol #\✗))   ; red X
    (:current '(:color "0 0.8 0" :shape :check  :symbol #\✓))   ; green check
    (:staged  '(:color "0 0 0.8" :shape :plus   :symbol #\➕))   ; blue plus
    (:modified '(:color "0.8 0.8 0" :shape :pencil :symbol #\✎))   ; yellow pencil
    (:untracked '(:color "0.8 0 0.8" :shape :circle :symbol #\✱)))) ; magenta star

(defun version-control-draw-status-icon (pane status x y size)
  "Draw a small status indicator at (X,Y) with given SIZE.
Uses the color and shape from version-control-status-icon."
  (let* ((icon (version-control-status-icon status))
         (color (getf icon :color))
         (shape (getf icon :shape)))
    (clim:with-drawing-options (pane :ink (apply #'clim:make-rgb-color 
                                                  (read-from-string (concatenate 'string "(" color ")")))
                                      :foreground t)
      (ecase shape
        (:x (clim:draw-line* pane (- x size) (- y size) (+ x size) (+ y size))
         (clim:draw-line* pane (- x size) (+ y size) (+ x size) (- y size)))
        (:check (clim:draw-line* pane (- x size) y x (+ y size))
         (clim:draw-line* pane x (+ y size) (+ x size) (- y size)))
        (:plus (clim:draw-line* pane x (- y size) x (+ y size))
         (clim:draw-line* pane (- x size) y (+ x size) y))
        (:pencil (clim:draw-polygon* pane 
                                     (list (clim:make-point (- x size) y)
                                           (clim:make-point x (+ y size))
                                           (clim:make-point (+ x size) y))))
        (:circle (clim:draw-ellipse* pane x y size size 0 0))))))

;;
;; Status Text (full, for Detailed/Editing layouts)
;;

(defun version-control-status-text (status)
  "Return a string describing the STATUS for full display."
  (ecase status
    (:absent  "Absent from VC")
    (:current "Current")
    (:staged  "Staged for commit")
    (:modified "Modified since last commit")
    (:untracked "Untracked")
    (:ignored "Ignored")))

(defun version-control-draw-status-bar (pane status width height)
  "Draw a status bar across the bottom of a pane of given WIDTH and HEIGHT."
  (let* ((y-offset (- height 10)) ; 10 pixels from bottom
         (text (version-control-status-text status))
         (color (case status
                  (:absent '(.8 0 0))
                  (:current '(0 .8 0))
                  (:staged  '(0 0 .8))
                  (:modified '(.8 .8 0))
                  (:untracked '(.8 0 .8))
                  (:ignored '(.5 .5 .5)))))
    (clim:with-drawing-options
        (pane :ink (apply #'clim:make-rgb-color color))
      (clim:draw-rectangle* pane 0 y-offset width height :filled t)
      (clim:with-drawing-options (pane :ink (clim:make-rgb-color 1 1 1))
        (clim:stream-set-cursor-position pane 5 (- y-offset 2))
        (write-string text pane)))))

;;
;; Integration with filesystem monitor
;;

(defvar *version-control-file-status-cache* (make-hash-table :test 'equal)
  "Cache of file paths to their last known VC status.")

(defun version-control-file-status-changed-p (file-path)
  "Return T if the VC status of FILE-PATH has changed since last check.
Uses the configuration cache."
  (let ((current-status (version-control-file-status file-path))
        (cached-status (gethash file-path *version-control-file-status-cache*)))
    (when (or (null cached-status)
              (not (eq current-status cached-status)))
      (setf (gethash file-path *version-control-file-status-cache*) 
            (cons (get-universal-time) current-status))
      t)))

;;
;; Menu commands for version control (to be added to resource menus)
;;

(defun version-control-menu-commands (file-path)
  "Return a list of menu commands for version control operations on FILE-PATH.
Each command is a cons of (label . function)."
  (let* ((backend-type (detect-version-control-backend file-path))
         (status (version-control-file-status file-path)))
    (when (and backend-type (not (eq backend-type :absent)))
      (let ((backend (make-version-control-backend)))
        (list 
         (cons "Stage/Unstage" 
               (lambda () 
                 (if (member status '(staged modified untracked))
                     (progn
                       (version-control-add backend (list file-path))
                       (version-control-commit backend "Staging via menu"))
                   (version-control-add backend (list file-path)))
               (clim:run-frame-top-level
                (clim:make-application-frame 'compare-frame
                                               :width 600 :height 400
                                               :title "Compare"))))
          
         (cons "Commit..." 
               (lambda () 
                 (version-control-show-commit-dialog file-path backend)))
          
         (cons "Ignore" 
               (lambda () 
                 (if (eq status :untracked)
                     (progn
                       (version-control-set-ignored-file file-path t)
                       (version-control-set-ignored-status file-path t))
                   (format t "Can only ignore untracked files~%")))))
          
         (cons "Tracked" 
               (lambda () 
                 (if (member status '(staged modified untracked))
                     (version-control-set-tracked-file file-path t)
                   (version-control-set-tracked-file file-path nil))))
          
         (cons "Version Control Settings" 
               (lambda () 
                 (version-control-show-settings)))))))

;;
;; Dialog implementations
;;

(clim:define-application-frame compare-frame ()
  ((file-path :initarg :file-path :accessor compare-file-path))
  (:panes
   (content :application
            :display-function 'draw-compare-dialog
            :scroll-bars nil)
   (command-line :interactor))
  (:layouts
   (default (clim:vertically () content command-line)))
  (:menu-bar t))

(defun draw-compare-dialog (frame pane)
  (clim:stream-set-cursor-position pane 10 50)
  (write-string "Compare Dialog" pane)
  (clim:stream-set-cursor-position pane 10 70)
  (format pane "File: ~a~%" (compare-file-path frame)))

;; VC commit dialog
(clim:define-application-frame commit-frame ()
  ((file-path :initarg :file-path :accessor commit-file-path)
   (message :initarg :message :accessor commit-message :initform ""))
  (:panes
   (content :application
            :display-function 'draw-commit-dialog
            :scroll-bars nil)
   (command-line :interactor))
  (:layouts
   (default (clim:vertically () content command-line)))
  (:menu-bar t))

(defun draw-commit-dialog (frame pane)
  (clim:stream-set-cursor-position pane 10 50)
  (write-string "Commit Dialog" pane)
  (clim:stream-set-cursor-position pane 10 70)
  (write-string "Message: " pane)
  (clim:with-drawing-options (pane :ink clim:+black+)
    (clim:stream-set-cursor-position pane 30 70)
    (write-string (commit-message frame) pane))
  (clim:stream-set-cursor-position pane 10 90)
  (write-string "Amend commit? [ ]" pane)
  (clim:stream-set-cursor-position pane 10 110)
  (write-string "Sign commit? [ ]" pane))

;; VC Settings dialog
(clim:define-application-frame version-control-settings-frame ()
  ((current-category :initarg :current-category :accessor version-control-settings-category :initform :general))
  (:panes
   (content :application
            :display-function 'draw-version-control-settings-dialog
            :scroll-bars :vertical)
   (command-line :interactor))
  (:layouts
   (default (clim:vertically () content command-line)))
  (:menu-bar t))

(defun draw-version-control-settings-dialog (frame pane)
  (clim:stream-set-cursor-position pane 10 50)
  (ecase (version-control-settings-category frame)
    (:general (write-string "General Settings" pane))
    (:backends (write-string "Backend Settings" pane))
    (:monitoring (write-string "Monitoring Settings" pane))
    (:config (write-string "Configuration Settings" pane))))

;; Helper functions for tracked/ignored status (using config)
(defun version-control-get-tracked-status (file-path)
  (member file-path (version-control-get-tracked-files) :test #'equal))

(defun version-control-set-tracked-status (file-path tracked)
  (let ((files (version-control-get-tracked-files)))
    (if tracked
        (unless (member file-path files :test #'equal)
          (version-control-set-tracked-files (append files (list file-path))))
        (version-control-set-tracked-files (remove file-path files :test #'equal)))))

(defun version-control-get-ignored-status (file-path)
  (member file-path (version-control-get-ignored-files) :test #'equal))

(defun version-control-set-ignored-status (file-path ignored)
  (let ((files (version-control-get-ignored-files)))
    (if ignored
        (unless (member file-path files :test #'equal)
          (version-control-set-ignored-files (append files (list file-path))))
        (version-control-set-ignored-files (remove file-path files :test #'equal)))))

;; Dialog invocation functions
(defun version-control-show-compare-dialog (&optional (file-path (uiop:getcwd)))
  "Show the compare dialog for FILE-PATH."
  (clim:run-frame-top-level 
    (clim:make-application-frame 'compare-frame
      :width 600 :height 400
      :title "Compare"
      :file-path file-path)))

(defun version-control-show-commit-dialog (&optional (file-path (uiop:getcwd)) backend)
  "Show the commit dialog for FILE-PATH using BACKEND.
If BACKEND is not provided, a Git backend is used for FILE-PATH."
  (when (null backend)
    (setf backend (make-version-control-backend file-path)))
  (let ((message (run-text-input-dialog (format nil "Commit message for ~a:" file-path)
                                          :initial-value "Update"
                                          :title "Commit")))
    (when message
      (version-control-add backend (list file-path))
      (version-control-commit backend message))))

(defun version-control-show-settings ()
  "Show the version control settings window."
  (clim:run-frame-top-level 
    (clim:make-application-frame 'version-control-settings-frame
      :width 600 :height 400
      :title "VC Settings")))