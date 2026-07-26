;;; Skyline-Tool src/gui/gui-atari-vox-dictionary.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-atari-vox-dictionary-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-atari-vox-dictionary-reference)))
  (typep object 'game-resource-atari-vox-dictionary))

(clim:define-presentation-type game-resource-atari-vox-dictionary-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-atari-vox-dictionary-editable)))
  (typep object 'game-resource-atari-vox-dictionary))

(clim:define-presentation-type game-resource-atari-vox-dictionary-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-atari-vox-dictionary-viewing)))
  (typep object 'game-resource-atari-vox-dictionary))

(defmethod present-reference ((resource game-resource-atari-vox-dictionary) stream)
  (clim:with-output-as-presentation (stream resource 'game-resource-atari-vox-dictionary-reference)
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

(defmethod present-reading ((resource game-resource-atari-vox-dictionary) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Word: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-title resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Phonetics: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-translation-phonetics resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Language: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-translation-language resource))))
    (when (typep resource 'game-resource-from-file)
      (let ((path (game-resource-full-path resource)))
        (when (and path (probe-file path))
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream :align-x :right)
              (format stream "Path: "))
            (clim:formatting-cell (stream :align-x :left)
              (format stream "~a" path))))))))

(defmethod present-editing ((resource game-resource-atari-vox-dictionary) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Word: "))
      (clim:formatting-cell (stream :align-x :left)
        (interactive-editing-gadget-with-validation
         stream resource
         (lambda (r) (game-resource-title r))
         (lambda (r v) (setf (game-resource-title r) v))
         :label "Word:"
         :validator #'validate-asset-name
         :max-length 200)))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Phonetics: "))
      (clim:formatting-cell (stream :align-x :left)
        (insert-gadget stream :label nil
                       :variable (game-translation-phonetics resource)
                       :activation-callback
                       (lambda (g)
                         (setf (game-translation-phonetics resource)
                               (clim:gadget-value g))))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Language: "))
      (clim:formatting-cell (stream :align-x :left)
        (insert-gadget stream :label nil
                       :variable (game-translation-language resource)
                       :activation-callback
                       (lambda (g)
                         (setf (game-translation-language resource)
                               (clim:gadget-value g))))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Kind: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a (read-only)" (game-resource-kind resource))))))

(clim:define-command-table atari-vox-file-menu
  :menu (("New..." :command com-new-atari-vox-word)
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

(clim:define-command-table atari-vox-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table atari-vox-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :toggle t)
         (nil :divider :line)
         ("Project Pane" :command com-toggle-project-pane :toggle t)))

(clim:define-command-table atari-vox-build-menu
  :menu (("Demo" :command com-build-atari-vox-demo)
         ("Public" :command com-build-atari-vox-public)
         ("$(Publisher)" :command com-build-atari-vox-publisher)))

(clim:define-command-table atari-vox-region-menu
  :menu (("NTSC" :command com-region-ntsc-atari-vox)
         ("PAL" :command com-region-pal-atari-vox)
         ("SECAM" :command com-region-secam-atari-vox)))

(clim:define-command-table atari-vox-run-menu
  :menu (("Build" :menu atari-vox-build-menu)
         ("Region" :menu atari-vox-region-menu)
         (nil :divider :line)
         ("Speak on AtariVox..." :command com-speak-atari-vox)))

(clim:define-command-table atari-vox-help-menu
  :menu (("How to Edit AtariVox Words..." :command com-help-for-window)
         ("SpeakJet Phoneme Documentation..." :command com-open-fountain-manual)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table atari-vox-menu-bar
  :menu (("AtariVox" :menu atari-vox-file-menu)
         ("Edit" :menu atari-vox-edit-menu)
         ("View" :menu atari-vox-view-menu)
         ("Run" :menu atari-vox-run-menu)
         ("Help" :menu atari-vox-help-menu)))

(clim:define-application-frame atari-vox-dictionary-inspector-frame (gui-inspector-frame clim:standard-application-frame)
  ((resource :initarg :resource :reader frame-resource))
  (:menu-bar atari-vox-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "AtariVox Dictionary Inspector"))

(defmethod initialize-instance :after ((frame atari-vox-dictionary-inspector-frame) &key)
  (call-next-method)
  (subscribe :resource-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

(defun open-atari-vox-dictionary-inspector (resource)
  (open-resource-inspector (or resource (make-instance 'game-resource-atari-vox-dictionary
                                                        :kind "AtariVox Dictionary"
                                                        :moniker "AtariVox Dictionary/SpeakJet.dic")) :editing))

(defmethod open-resource-inspector ((resource game-resource-atari-vox-dictionary) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'atari-vox-dictionary-inspector-frame
                                :resource resource
                                :pretty-name (format nil "~a — ~a ~a"
                                                     (game-resource-title resource)
                                                     *game-title* (machine-directory-name))
                                :view-mode mode)))

(clim:define-command (com-new-atari-vox-word :command-table clim-internals::global-command-table :menu t :name t) ()
  (open-atari-vox-dictionary-inspector nil))

(clim:define-command (com-speak-atari-vox :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Speaking on AtariVox...~%"))))

(clim:define-command (com-build-atari-vox-demo :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building AtariVox dictionary (Demo)...~%"))))

(clim:define-command (com-build-atari-vox-public :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building AtariVox dictionary (Public)...~%"))))

(clim:define-command (com-build-atari-vox-publisher :command-table clim-internals::global-command-table :menu t :name t) ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Building AtariVox dictionary (Publisher)...~%"))))

(clim:define-command (com-region-ntsc-atari-vox :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :ntsc)
  (publish :region-changed :payload :ntsc))

(clim:define-command (com-region-pal-atari-vox :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :pal)
  (publish :region-changed :payload :pal))

(clim:define-command (com-region-secam-atari-vox :command-table clim-internals::global-command-table :menu t :name t) ()
  (setf *region* :secam)
  (publish :region-changed :payload :secam))

(defmethod game-resource-action-menu ((resource game-resource-atari-vox-dictionary))
  (list
   (make-menu-item "Inspect..." (lambda () (open-atari-vox-dictionary-inspector resource)))))
(in-package :skyline-tool)

;; AtariVox Dictionary Inspector - for viewing/editing the phonetic dictionary

;; AtariVox Dictionary Inspector Frame
(clim:define-application-frame atari-vox-dictionary-inspector-frame (resource-inspector-mixin clim:standard-application-frame)
  ((path :initarg :path :accessor frame-path)
   (entries :initarg :entries :accessor frame-entries))
  (:menu-bar atari-vox-dictionary-inspector-menu-bar)
  (:panes
   (editor-pane :application :display-function 'display-resource-inspector
                :height 600 :width 500
                :scroll-bars :vertical)
   (interactor :interactor :height 80 :width 500))
  (:layouts (default (clim:vertically () editor-pane interactor))))

;; Menu definitions
(clim:define-command-table atari-vox-dictionary-inspector-help-menu
  :menu (("How to Edit Dictionary..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-scripting-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table atari-vox-dictionary-inspector-menu-bar
  :menu (("File" :menu (("Close" :command com-atari-vox-dictionary-close)))
         ("Help" :menu atari-vox-dictionary-inspector-help-menu)))

;; Load dictionary entries
(defun load-atari-vox-dictionary (&optional (path "Source/Tables/SpeakJet.dic"))
  "Load dictionary entries from PATH"
  (let ((full (merge-pathnames path (uiop:getcwd))))
    (when (probe-file full)
      (with-open-file (f full)
        (loop for line = (read-line f nil nil)
              while line
              when (and (> (length line) 0)
                        (not (char= #\# (char line 0)))
                        (not (char= #\[ (char line 0))))
              collect line)))))

;; Save dictionary
(defun save-atari-vox-dictionary (entries &optional (path "Source/Tables/SpeakJet.dic"))
  "Save dictionary ENTRIES to PATH"
  (let ((full (merge-pathnames path (uiop:getcwd))))
    (ensure-directories-exist full)
    (with-open-file (f full :direction :output :if-exists :supersede
                      :external-format :utf-8)
      (dolist (entry entries)
        (format f "~a~%
" entry)))))

;; Display function
(defmethod display-inspector-content ((frame atari-vox-dictionary-inspector-frame) pane)
  (display-atari-vox-dictionary-inspector frame pane))

(defun display-atari-vox-dictionary-inspector (frame pane)
  (clim:window-clear pane)
  (let ((entries (frame-entries frame)))
    (format pane "~&AtariVox Phonetic Dictionary (~d entries)~%" (length entries))
    (format pane "~&~-20a  ~a~%" "Word" "Phonetic Code")
    (format pane "~&~40a~%" "----------------------------------------")
    (loop for entry in entries
           for i from 0
          do (let ((parts (split-sequence #\= entry)))
               (when (>= (length parts) 2)
                 (format pane "~&~3d. ~-20a  ~a~%" i (first parts) (second parts)))))))

;; Inspector command
(clim:define-command (com-atari-vox-dictionary-close :menu t :name t) ()
  (let ((frame (and (boundp 'clim:*application-frame*) clim:*application-frame*)))
    (when frame (clim:frame-exit frame))))

;; Open AtariVox dictionary inspector
(defun open-atari-vox-dictionary-inspector (&optional (path "Source/Tables/SpeakJet.dic"))
  "Open the AtariVox Dictionary Inspector."
  (let* ((full (merge-pathnames path (uiop:getcwd)))
         (entries (load-atari-vox-dictionary full))
         (resource (make-instance 'game-resource-phonetic-dictionary
                                  :moniker "AtariVox Dictionary"
                                 
                                  :full-path (when (probe-file full) (truename full))))
         (fm (clim:find-frame-manager))
         (frame (clim:make-application-frame 'atari-vox-dictionary-inspector-frame
                                             :resource resource
                                             :path full
                                             :entries entries
                                             :frame-manager fm)))
    (clim:run-frame-top-level frame)))
