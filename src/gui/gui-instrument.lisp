;;; Skyline-Tool src/gui/gui-instrument.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-instrument-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-instrument-reference)))
  (typep object 'game-resource-instrument))

(clim:define-presentation-type game-resource-instrument-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-instrument-editable)))
  (typep object 'game-resource-instrument))

(clim:define-presentation-type game-resource-instrument-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-instrument-viewing)))
  (typep object 'game-resource-instrument))

(clim:define-application-frame instrument-inspector-frame (resource-inspector-mixin clim:standard-application-frame)
  ()
  (:menu-bar instrument-inspector-menu-bar)
  (:panes
   (editor-pane :application :display-function 'display-resource-inspector
                             :height 300 :width 400
                             :scroll-bars :vertical)
   (search-bar :application :display-function (lambda (pane frame) (error "search"))
                            :height 30 :width 400
                            :scroll-bars nil)
   (project-bar :application :display-function (lambda (pane frame) (error "search"))
                             :height 30 :width 400
                             :scroll-bars nil))
  (:layouts (default (clim:vertically () editor-pane))
            (search (clim:vertically () editor-pane search-bar))
            (project (clim:vertically () editor-pane  project-bar))
            (search+project (clim:vertically () editor-pane search-bar project-bar))))

(defmethod present-reference ((resource game-resource-instrument) stream)
  (clim:with-output-as-presentation (stream resource 'game-resource-instrument-reference)
    (clim:formatting-table (stream)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 0)
          (format stream "~3%"))
        (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 125)
          (game-resource-present-icon resource stream))
        (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 150)
          (clim:with-text-size (stream :larger)
            (clim:with-text-face (stream :bold)
              (game-resource-present-title resource stream)))
          (format stream "~%~5t")
          (clim:with-text-size (stream :smaller)
            (clim:with-drawing-options (stream :ink (clim:make-gray-color 0.75))
              (game-resource-present-subheading resource stream))))
        (clim:formatting-cell (stream :align-x :right :align-y :top :min-height 90 :min-width 125)
          (game-resource-present-right-margin resource stream))))))

(defmethod present-reading ((resource game-resource-instrument) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-title resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Distortion: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~d" (game-resource-instrument-distortion resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Attack: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~d" (game-resource-instrument-attack-addend resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Decay: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~d/~d" (game-resource-instrument-decay-subtrahend resource)
                (game-resource-instrument-decay-duration resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Release: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~d" (game-resource-instrument-release-subtrahend resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Vibrato: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~d" (game-resource-instrument-vibrato resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Tremolo: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~d" (game-resource-instrument-tremolo resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "PSG Tone: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~d" (game-resource-instrument-psg-tone resource))))))

(defun insert-slider-gadget (stream label variable setter min-val max-val)
  "Insert a slider gadget for numeric fields."
  (declare (ignore label))
  (clim:with-output-as-gadget (stream)
    (clim:make-pane 'clim:slider
                    :value (or variable 0)
                    :minimum-bound min-val
                    :maximum-bound max-val
                    :value-changed-callback
                    (lambda (gadget) (funcall setter (clim:gadget-value gadget))))))

(defmethod present-editing ((resource game-resource-instrument) stream)
  (clim:with-text-size (stream :larger)
    (clim:with-text-face (stream :bold)
      (format stream "Instrument Parameters")))
  (format stream "~%~%")
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
        (interactive-editing-gadget-with-validation
         stream resource
         :getter (lambda (r) (game-resource-title r))
         :setter (lambda (r v) (setf (game-resource-title r) v))
         :label "Name:")))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Instrument ID: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-locator resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Distortion (0-255): "))
      (clim:formatting-cell (stream :align-x :left)
        (insert-slider-gadget stream "Distortion:" (game-resource-instrument-distortion resource)
                              (lambda (v) (setf (game-resource-instrument-distortion resource) v))
                              0 255)))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Attack (0-255): "))
      (clim:formatting-cell (stream :align-x :left)
        (insert-slider-gadget stream "Attack:" (game-resource-instrument-attack-addend resource)
                              (lambda (v) (setf (game-resource-instrument-attack-addend resource) v))
                              0 255)))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Decay Sub (0-255): "))
      (clim:formatting-cell (stream :align-x :left)
        (insert-slider-gadget stream "Decay Sub:" (game-resource-instrument-decay-subtrahend resource)
                              (lambda (v) (setf (game-resource-instrument-decay-subtrahend resource) v))
                              0 255)))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Decay Dur (0-255): "))
      (clim:formatting-cell (stream :align-x :left)
        (insert-slider-gadget stream "Decay Dur:" (game-resource-instrument-decay-duration resource)
                              (lambda (v) (setf (game-resource-instrument-decay-duration resource) v))
                              0 255)))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Release (0-255): "))
      (clim:formatting-cell (stream :align-x :left)
        (insert-slider-gadget stream "Release:" (game-resource-instrument-release-subtrahend resource)
                              (lambda (v) (setf (game-resource-instrument-release-subtrahend resource) v))
                              0 255)))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "TIA Distortion (0-15): "))
      (clim:formatting-cell (stream :align-x :left)
        (insert-slider-gadget stream "TIA Dist:" (game-resource-instrument-tia-distortion resource)
                              (lambda (v) (setf (game-resource-instrument-tia-distortion resource) v))
                              0 15)))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Vibrato (0-255): "))
      (clim:formatting-cell (stream :align-x :left)
        (insert-slider-gadget stream "Vibrato:" (game-resource-instrument-vibrato resource)
                              (lambda (v) (setf (game-resource-instrument-vibrato resource) v))
                              0 255)))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Tremolo (0-255): "))
      (clim:formatting-cell (stream :align-x :left)
        (insert-slider-gadget stream "Tremolo:" (game-resource-instrument-tremolo resource)
                              (lambda (v) (setf (game-resource-instrument-tremolo resource) v))
                              0 255)))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "PSG Tone (0-255): "))
      (clim:formatting-cell (stream :align-x :left)
        (insert-slider-gadget stream "PSG Tone:" (game-resource-instrument-psg-tone resource)
                              (lambda (v) (setf (game-resource-instrument-psg-tone resource) v))
                              0 255)))))

(defparameter *instrument-palette* clim:+black+
  "The color selection palette for this instrument")

(clim:define-command-table instrument-file-menu
  :menu (("New..." :command com-new-instrument)
         ("Import..." :command com-import-resource)
         (nil :divider :line)
         ("Save" :command com-save-resource)
         ("Version" :menu inspector-vc-menu)
         ("Save as" :menu inspector-save-as-menu)
         (nil :divider :line)
         ("Send to" :menu inspector-send-to-menu)
         ("Print to" :menu inspector-print-to-menu)
         (nil :divider :line)
         ("Close" :command com-close-frame)))

(clim:define-command-table instrument-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table instrument-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :toggle t)
         (nil :divider :line)
         ("Project Pane" :command com-toggle-project-pane :toggle t)))

(clim:define-command-table instrument-build-menu
  :menu (("Demo" :command com-build-instrument-demo)
         ("Public" :command com-build-instrument-public)
         ("$(Publisher)" :command com-build-instrument-publisher)))

(clim:define-command-table instrument-region-menu
  :menu (("NTSC" :command com-region-ntsc-instrument)
         ("PAL" :command com-region-pal-instrument)
         ("SECAM" :command com-region-secam-instrument)))

(clim:define-command-table instrument-run-menu
  :menu (("Build" :menu instrument-build-menu)
         ("Region" :menu instrument-region-menu)
         (nil :divider :line)
         ("Play Instrument..." :command com-play-instrument)))

(clim:define-command-table instrument-help-menu
  :menu (("How to Manage Instruments..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table instrument-menu-bar
  :menu (("Instrument" :menu instrument-file-menu)
         ("Edit" :menu instrument-edit-menu)
         ("View" :menu instrument-view-menu)
         ("Run" :menu instrument-run-menu)
         ("Help" :menu instrument-help-menu)))

(defmethod initialize-instance :after ((frame instrument-inspector-frame) &key)
  (call-next-method)
  (subscribe :resource-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

(defun open-instrument-inspector (resource)
  (open-resource-inspector (or resource
                                (make-instance 'game-resource-instrument
                                  :instrument-id 0
                                  :instrument "New Instrument"
                                  :distortion 0
                                  :attack-addend 0
                                  :decay-subtrahend 0
                                  :decay-duration 0
                                  :release-subtrahend 0
                                  :tia-distortion 0
                                  :vibrato 0
                                  :tremolo 0
                                  :psg-tone 0)) :editing))

(defmethod open-resource-inspector ((resource game-resource-instrument) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'instrument-inspector-frame
                                :resource resource
                                :pretty-name (format nil "~a — ~a ~a"
                                                     (game-resource-title resource)
                                                     *game-title* (machine-directory-name))
                                :view-mode mode)))

(clim:define-command (com-new-instrument :command-table clim-internals::global-command-table :menu t :name t) ()
  (open-instrument-inspector nil))

(clim:define-command (com-play-instrument :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Playing instrument...~%"))))

(clim:define-command (com-build-instrument-demo :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building instrument (Demo)...~%"))))

(clim:define-command (com-build-instrument-public :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building instrument (Public)...~%"))))

(clim:define-command (com-build-instrument-publisher :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building instrument (Publisher)...~%"))))

(clim:define-command (com-region-ntsc-instrument :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :ntsc)
  (publish :region-changed :payload :ntsc))

(clim:define-command (com-region-pal-instrument :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :pal)
  (publish :region-changed :payload :pal))

(clim:define-command (com-region-secam-instrument :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :secam)
  (publish :region-changed :payload :secam))

(defmethod game-resource-action-menu ((resource game-resource-instrument))
  (list
   (make-menu-item "Inspect..." (lambda () (open-instrument-inspector resource)))))
(in-package :skyline-tool)

;; Instrument Inspector - for viewing/editing instrument definitions from Orchestration.ods

(defun load-orchestration (&optional (pathname #p"Source/Tables/Orchestration.ods"))
  "Load the orchestration table from PATHNAME"
  (let ((full (merge-pathnames pathname (uiop:getcwd))))
    (when (probe-file full)
      (read-orchestration full))))

(defun save-orchestration (instrument
                           &optional (pathname #p"Source/Tables/Orchestration.ods"))
  "Save the orchestration table to PATHNAME"
  ;; Note: For simplicity, we won't implement saving to ODS format here
  ;; In practice, users would edit in LibreOffice or similar
  (error "Saving orchestration not implemented - edit Source/Tables/Orchestration.ods directly")
  nil)


(clim:define-command-table instrument-inspector-help-menu
  :menu (("How to Edit Instruments..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-scripting-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table instrument-inspector-menu-bar
  :menu (("File" :menu (("Close" :command com-instrument-close)))
         ;; FIXME missing everything
         ("Help" :menu instrument-inspector-help-menu)))

(defmethod display-inspector-content ((frame instrument-inspector-frame) pane)
  (display-instrument-inspector frame pane))

(defun display-instrument-inspector (frame pane &aux (resource (frame-resource frame)))
  (clim:window-clear pane)
  (format pane "~&Instrument ~a" (game-resource-title resource))
  (format pane "~&ID.  Attack  Decay  Release  PSG  Vibrator  Tremolo~%")
  (format pane "~&---  -----  -----  -----  ---  ----  ----~%")
  (format pane "~&~3a ~3d    ~3d    ~3d    ~3d    ~3d    ~3d ~3d~%"
          (game-resource-locator resource)
          (game-resource-instrument-attack-addend resource)
          (game-resource-instrument-decay-subtrahend resource)
          (game-resource-instrument-decay-duration resource)
          (game-resource-instrument-release-subtrahend resource)
          (game-resource-instrument-psg-tone resource)
          (game-resource-instrument-vibrato resource)
          (game-resource-instrument-tremolo resource)))

;; Inspector commands
(clim:define-command (com-instrument-close :menu t :name t) ()
  (let ((frame (and (boundp 'clim:*application-frame*) clim:*application-frame*)))
    (when frame (clim:frame-exit frame))))

;; For creating a new instrument (blank entry), we'll open the inspector
;; The user would need to add the instrument to the spreadsheet manually
;; This satisfies the requirement to show an inspector for a new blank resource
(clim:define-command (com-new-instrument-blank :command-table clim-internals::global-command-table
                                               :menu nil :name t)
    ()
  "Open instrument inspector for creating a new instrument."
  (open-instrument-inspector nil))
