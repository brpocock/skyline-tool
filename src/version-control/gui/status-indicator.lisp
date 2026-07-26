;;; src/version-control/gui/status-indicator.lisp
;;; Status indicator component for CLIM panes

(in-package :skyline-tool.version-control)

;;
;; Status Indicator Presentation Type
;;

(clim:define-presentation-type version-control-status-indicator () )

;;
;; Status Indicator Command Table
;;

(clim:define-command-table version-control-status-menu)

(clim:define-command (com-version-control-stage :command-table version-control-status-menu :menu t :name t)
    ((file 'pathname :gesture :select))
  (let ((status (skyline-tool.version-control:version-control-file-status file)))
    (case status
      (:staged nil) ; Already staged
      ((:modified :untracked) 
       (skyline-tool.version-control:version-control-add (skyline-tool.version-control:make-git-backend) (list file))
       (skyline-tool.version-control:version-control-commit (skyline-tool.version-control:make-git-backend) "Staged via indicator"))
      (t (format t "Cannot stage ~a in status ~a~%" file status)))))

(clim:define-command (com-version-control-revert :command-table version-control-status-menu :menu t :name t)
    ((file 'pathname :gesture :select))
  (skyline-tool.version-control:version-control-reset (skyline-tool.version-control:make-git-backend) (list file)))

(clim:define-command (com-version-control-compare :command-table version-control-status-menu :menu t :name t)
    ((file 'pathname :gesture :select))
  (show-version-control-diff-dialog file))

;;
;; Status Indicator Presentation
;;

(defun present-version-control-status (stream file-path &key (style ':compact))
  "Present VC status indicator for FILE-PATH.
   STYLE can be :compact (small icon bottom-right) or :full (bottom bar)."
  (let ((status (skyline-tool.version-control:version-control-file-status file-path)))
    (clim:with-output-as-presentation
        (stream (cons :version-control-status file-path) 'version-control-status-indicator)
      (case style
        (:compact (present-version-control-status-icon stream status))
        (:full (present-version-control-status-bar stream status))))))

(defun present-version-control-status-icon (stream status)
  "Present compact status icon"
  (let ((icon (skyline-tool.version-control:version-control-status-icon status)))
    (clim:stream-set-cursor-position stream 10 -15) ; Bottom right
    (clim:with-drawing-options (stream :ink (clim:make-rgb-color 
                                               (parse-color-string (getf icon :color))))
      (clim:draw-rectangle* stream 0 0 10 10 :filled t)
      (clim:stream-set-cursor-position stream 2 2)
      (format stream "~a" (status-symbol status)))))

(defun present-version-control-status-bar (stream status)
  "Present full status bar at bottom of pane"
  (let ((text (skyline-tool.version-control:version-control-status-text status))
        (color (case status
                 (:absent  (clim:make-rgb-color 0.8 0 0))
                 (:current (clim:make-rgb-color 0 0.8 0))
                 (:staged  (clim:make-rgb-color 0 0 0.8))
                 (:modified (clim:make-rgb-color 0.8 0.8 0))
                 (:untracked (clim:make-rgb-color 0.8 0 0.8)))))
    (clim:with-drawing-options (stream :ink color)
      (clim:draw-rectangle* stream 0 -20 200 10 :filled t)
      (clim:with-drawing-options (stream :ink clim:+white+)
        (clim:stream-set-cursor-position stream 5 (- (clim:stream-cursor-position stream) 0 15))
        (format stream "~a" text)))))

(defun status-symbol (status)
  "Return a single character symbol for STATUS"
  (case status
    (:absent #\?)             ; Use question mark instead of #?
    (:current #\✓)
    (:staged #\+)
    (:modified #\~)
    (:untracked #\*)))

(defun parse-color-string (color-str)
  "Parse a color string like '0.8 0 0' into RGB values"
  (let ((parts (split-sequence #\Space color-str)))
    (values (parse-number:parse-number (pop parts))
            (parse-number:parse-number (pop parts))
            (parse-number:parse-number (pop parts)))))
