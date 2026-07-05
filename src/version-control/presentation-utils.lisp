;;; src/version-control/presentation-utils.lisp
;;; Utilities for presenting version control status in Skyline-Tool presentations

(in-package :skyline-tool.version-control)

;;
;; Status Detection
;;

(defun vc-file-status (file-path)
  "Return the version control status of FILE-PATH.
   Returns a keyword: :absent, :current, :staged, :modified, :untracked.
   Uses the detected backend (Git preferred)."
  (let ((backend (detect-vc-backend file-path)))
    (cond
      ((null backend) :absent) ; No VC detected
      ((eq backend :git)
       (let* ((git-backend (make-git-backend))
              (status (vc-status git-backend file-path)))
         (cond
           ((member :staged status) :staged)
           ((member :modified status) :modified)
           ((member :untracked status) :untracked)
           (t :current))))
      ((eq backend :svn)
       (let* ((svn-backend (make-instance 'svn-backend))
              (status (vc-status svn-backend file-path)))
         (cond
           ((member :staged status) :staged)
           ((member :modified status) :modified)
           ((member :untracked status) :untracked)
           (t :current))))
      (t :absent))))

;;
;; Status Icons (small, for Reference form)
;;

(defun vc-status-icon (status)
  "Return a plist with :color, :shape and :symbol for STATUS.
   Symbol uses Unicode characters for visual cue."
  (ecase status
    (:absent  '(:color "0.8 0 0" :shape :x      :symbol #\✗))   ; red X
    (:current '(:color "0 0.8 0" :shape :check  :symbol #\✓))   ; green check
    (:staged  '(:color "0 0 0.8" :shape :plus   :symbol #\➕))   ; blue plus
    (:modified '(:color "0.8 0.8 0" :shape :pencil :symbol #\✎))   ; yellow pencil
    (:untracked '(:color "0.8 0 0.8" :shape :circle :symbol #\✱)))) ; magenta star

(defun draw-vc-status-icon (pane status x y size)
  "Draw a small status indicator at (X,Y) with given SIZE.
   Uses the color and shape from vc-status-icon."
  (let* ((icon (vc-status-icon status))
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
        (:circle (clim:draw-ellipse* pane x y size size size size))))))

;;
;; Status Text (full, for Detailed/Editing layouts)
;;

(defun vc-status-text (status)
  "Return a string describing the STATUS for full display."
  (ecase status
    (:absent  "Absent from VC")
    (:current "Current")
    (:staged  "Staged for commit")
    (:modified "Modified since last commit")
    (:untracked "Untracked")))

(defun draw-vc-status-bar (pane status width height)
  "Draw a status bar across the bottom of a pane of given WIDTH and HEIGHT."
  (let* ((y-offset (- height 10)) ; 10 pixels from bottom
         (text (vc-status-text status))
         (color (case status
                  (:absent '(.8 0 0))
                  (:current '(0 .8 0))
                  (:staged  '(0 0 .8))
                  (:modified '(.8 .8 0))
                  (:untracked '(.8 0 .8)))))
    (clim:with-drawing-options
        (pane :ink (apply #'clim:make-rgb-color color))
      (clim:draw-rectangle* pane 0 y-offset width height :filled t)
      (clim:with-drawing-options (pane :ink (clim:make-rgb-color 1 1 1))
        (clim:stream-set-cursor-position pane 5 (- y-offset 2))
        (write-string text pane)))))

;;
;; Integration with filesystem monitor
;;

(defun vc-file-status-changed-p (file-path)
  "Return T if the VC status of FILE-PATH has changed since last check.
   Uses the configuration cache."
  (let ((current-status (vc-file-status file-path))
        (cached-status (gethash file-path *vc-file-status-cache*)))
    (when (or (null cached-status)
              (not (eq current-status cached-status)))
      (setf (gethash file-path *vc-file-status-cache*) current-status)
      t)))

(defvar *vc-file-status-cache* (make-hash-table :test 'equal)
  "Cache of file paths to their last known VC status.")

;;
;; Menu commands for version control (to be added to resource menus)
;;

(defun vc-menu-commands (file-path)
  "Return a list of menu commands for version control operations on FILE-PATH.
   Each command is a cons of (label . function)."
  (let ((status (vc-file-status file-path)))
    (list (cons "Stage/Unstage" 
                (lambda () 
                  (if (member status '(staged modified untracked))
                      (progn
                        (vc-add (make-git-backend) (list file-path))
                        (vc-commit (make-git-backend) "Staging via menu"))
                      (vc-add (make-git-backend) (list file-path)))
                  (clim:run-frame-top-level
                   (clim:make-application-frame 'compare-frame
                                                :width 600 :height 400
                                                :title "Compare"))))
          
          (cons "Commit..." 
                (lambda () 
                  (show-vc-commit-dialog)))
          
          (cons "Ignore" 
                (lambda () 
                  (if (eq status :untracked)
                      (progn
                        (vc-set-ignored-file file-path t))
                      (format t "Can only ignore untracked files~%")))))
    
    (cons "Tracked" 
          (lambda () 
            (if (member status '(staged modified untracked))
                (vc-set-tracked-file file-path t)
                (vc-set-tracked-file file-path nil))))
    
    (cons "Version Control Settings" 
          (lambda () 
            (show-vc-settings)))))
  ;; 
  ;; Dialog implementations
  
  ;; Compare dialog
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
(clim:define-application-frame vc-settings-frame ()
  ((current-category :initarg :current-category :accessor vc-settings-category :initform :general))
  (:panes
   (content :application
            :display-function 'draw-vc-settings-dialog
            :scroll-bars :vertical)
   (command-line :interactor))
  (:layouts
   (default (clim:vertically () content command-line)))
  (:menu-bar t))

(defun draw-vc-settings-dialog (frame pane)
  (clim:stream-set-cursor-position pane 10 50)
  (ecase (vc-settings-category frame)
    (:general (write-string "General Settings" pane))
    (:backends (write-string "Backend Settings" pane))
    (:monitoring (write-string "Monitoring Settings" pane))
    (:config (write-string "Configuration Settings" pane))))

;; Helper functions for tracked/ignored status (using config)
(defun vc-get-tracked-status (file-path)
  (member file-path (vc-get-tracked-files) :test #'equal))

(defun vc-set-tracked-status (file-path tracked)
  (let ((files (vc-get-tracked-files)))
    (if tracked
        (unless (member file-path files :test #'equal)
          (vc-set-tracked-files (append files (list file-path))))
        (vc-set-tracked-files (remove file-path files :test #'equal)))))

(defun vc-get-ignored-status (file-path)
  (member file-path (vc-get-ignored-files) :test #'equal))

(defun vc-set-ignored-status (file-path ignored)
  (let ((files (vc-get-ignored-files)))
    (if ignored
        (unless (member file-path files :test #'equal)
          (vc-set-ignored-files (append files (list file-path))))
        (vc-set-ignored-files (remove file-path files :test #'equal)))))

;; Dialog invocation functions
(defun show-vc-compare-dialog (&optional (file-path (uiop:getcwd)))
  "Show the compare dialog for FILE-PATH."
  (clim:run-frame-top-level 
   (clim:make-application-frame 'compare-frame
     :width 600 :height 400
     :title "Compare"
     :file-path file-path)))

(defun show-vc-commit-dialog (&optional (file-path (uiop:getcwd)))
  "Show the commit dialog."
  (clim:run-frame-top-level 
   (clim:make-application-frame 'commit-frame
     :width 600 :height 400
     :title "Commit"
     :file-path file-path)))

(defun show-vc-settings ()
  "Show the version control settings window."
  (clim:run-frame-top-level 
   (clim:make-application-frame 'vc-settings-frame
     :width 600 :height 400
     :title "VC Settings")))
