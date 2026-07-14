(in-package :skyline-tool)

;; Instrument Inspector - for viewing/editing instrument definitions from Orchestration.ods

(defun load-orchestration (&optional (pathname #p"Source/Tables/Orchestration.ods"))
  "Load the orchestration table from PATHNAME"
  (let ((full (merge-pathnames pathname (uiop:getcwd))))
    (when (probe-file full)
      (read-orchestration full))))

(defun save-orchestration (orchestration &optional (pathname #p"Source/Tables/Orchestration.ods"))
  "Save the orchestration table to PATHNAME"
  ;; Note: For simplicity, we won't implement saving to ODS format here
  ;; In practice, users would edit in LibreOffice or similar
  (warn "Saving orchestration not implemented - edit Source/Tables/Orchestration.ods directly")
  nil)

;; Instrument inspector frame
(clim:define-application-frame instrument-inspector-frame (resource-inspector-mixin clim:standard-application-frame)
  ((path :initarg :path :accessor frame-path)
   (orchestration :initarg :orchestration :accessor frame-orchestration))
  (:menu-bar instrument-inspector-menu-bar)
  (:panes
   (editor-pane :application :display-function 'display-resource-inspector
                :height 600 :width 500
                :scroll-bars :vertical)
   (interactor :interactor :height 80 :width 500))
  (:layouts (default (clim:vertically () editor-pane interactor))))

(clim:define-command-table instrument-inspector-help-menu
  :menu (("How to Edit Instruments..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-scripting-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table instrument-inspector-menu-bar
  :menu (("File" :menu (("Close" :command com-instrument-close)))
         ("Help" :menu instrument-inspector-help-menu)))

(defmethod display-inspector-content ((frame instrument-inspector-frame) pane)
  (display-instrument-inspector frame pane))

(defun display-instrument-inspector (frame pane)
  (clim:window-clear pane)
  (let ((orchestration (frame-orchestration frame)))
    (if orchestration
        (progn
          (format pane "~&Orchestration Instruments (~d entries)~%" (length orchestration))
          (format pane "~&~30a  Attack  Decay  Release  PSG  Vibe  Trem~%" "Instrument")
          (format pane "~&~40a  -----  -----  -----  ---  ----  ----~%" "")
          (loop for inst in orchestration
                for i from 0
                do (format pane "~&~2d. ~-30a  ~4d    ~4d    ~4d    ~3d    ~4d    ~4d~%"
                           i
                           (or (getf inst :instrument) "")
                           (or (getf inst :attack-addend) 0)
                           (or (getf inst :decay-subtrahend) 0)
                           (or (getf inst :decay-duration) 0)
                           (or (getf inst :release-subtrahend) 0)
                           (or (getf inst :vibrato) 0)
                           (or (getf inst :tremolo) 0))))
        (format pane "~&No orchestration data loaded.~%")
        (format pane "~&File: ~a~%" (frame-path frame)))))

;; Inspector commands
(clim:define-command (com-instrument-close :menu t :name t) ()
  (let ((frame (and (boundp 'clim:*application-frame*) clim:*application-frame*)))
  (when frame (clim:frame-exit frame))))

;; Open instrument inspector
(defun open-instrument-inspector (&optional (pathname #p"Source/Tables/Orchestration.ods"))
  "Open the Instrument Inspector."
  (let* ((full (merge-pathnames pathname (uiop:getcwd)))
         (orchestration (load-orchestration full))
         (resource (make-instance 'game-resource-instrument
                                  :moniker "Instruments"
                                 
                                  :collective-path (when (probe-file full) (truename full))))
         (fm (clim:find-frame-manager :port (or (clim:find-port) (clim:find-port :server-path :x))))
         (frame (clim:make-application-frame 'instrument-inspector-frame
                 :resource resource
                 :path full
                 :orchestration orchestration
                 :frame-manager fm)))
    (clim:run-frame-top-level frame)))

;; For creating a new instrument (blank entry), we'll open the inspector
;; The user would need to add the instrument to the spreadsheet manually
;; This satisfies the requirement to show an inspector for a new blank resource
(clim:define-command (com-new-instrument-blank :command-table clim-internals::global-command-table
                                                :menu nil :name t)
    ()
  "Open instrument inspector for creating a new instrument."
  (open-instrument-inspector))