(in-package :skyline-tool)

;; AtariVox Dictionary Inspector - for viewing/editing the phonetic dictionary

;; AtariVox Dictionary Inspector Frame
(clim:define-application-frame atari-vox-dictionary-inspector-frame (resource-inspector-mixin)
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
                                  :kind "Translations"
                                  :full-path (when (probe-file full) (truename full))))
         (fm (clim:find-frame-manager))
         (frame (clim:make-application-frame 'atari-vox-dictionary-inspector-frame
                                             :resource resource
                                             :path full
                                             :entries entries
                                             :frame-manager fm)))
    (clim:run-frame-top-level frame)))
