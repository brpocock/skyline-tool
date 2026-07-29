                                        ; Accessibility Tab - Preferences Inspector
;; Contains theme and keyboard shortcut configuration

(in-package :skyline-tool)

(defun create-theme-radio-group (frame pane label options value-key)
  "Create a radio button group for theme selection"
  (let* ((box (clim:make-pane 'clim:radio-box))
         (current-value (get-pref value-key))
         (radios (loop for (key lbl) in options
                       collect (clim:make-pane 'clim:toggle-button
                                               :label lbl
                                               :value (eq current-value key)
                                               :group box
                                               :value-changed-callback
                                               (lambda (g v)
                                                 (declare (ignore g))
                                                 (when v
                                                   (setf (get-pref value-key) key)
                                                   (clim:redisplay-frame-panes frame :force-p t)))))))
    (clim:formatting-row (pane)
      (clim:formatting-cell (pane) (format pane "~a" label))
      (clim:note-gadget-activated box pane))
    (values box radios)))

(defun display-color-theme-section (frame pane)
  "Display color theme selection radio buttons"
  (clim:note-gadget-activated
   (create-theme-radio-group frame pane "Color Theme" '((:normal "Normal (Light))"
                                                         (:inverted "Inverted (Dark))"
                                                          (:sunrise "Sunrise/Sunset")
                                                          (:desktop "Desktop (Auto)"))))
                             '(:accessibility :color-theme))
   pane)
  (display-sunrise-sunset-section frame pane))

(defun display-keyboard-shortcuts-section (frame pane)
  "Display keyboard shortcuts theme selection"
  (clim:note-gadget-activated
   (create-theme-radio-group frame pane "Keyboard Shortcuts Theme"
                             '(
                               (:gnome "Gnome")
                               (:macos "macOS")
                               (:common "Common (CDE))")
                               (:emacs "Emacs"))
                             '(:accessibility :keybind-theme))
   pane))

(defun display-sunrise-sunset-section (frame pane)
  "Display sunrise/sunset location settings when theme is :sunrise"
  (when (eq (get-pref '(:accessibility :color-theme)) :sunrise)
    (format pane "~2%Sunrise/Sunset Location:")
    (clim:note-gadget-activated
     (clim:make-pane 'clim:text-field
                     :value (get-pref '(:location :latitude) "")
                     :width 80
                     :callback (lambda (gadget value)
                                 (declare (ignore gadget))
                                 (setf (get-pref '(:location :latitude)) value)
                                 (clim:redisplay-frame-panes frame :force-p t))) pane)
    (format pane "°")
    (format pane "FIXME: E/W?")
    (clim:note-gadget-activated
     (clim:make-pane 'clim:text-field
                     :value (get-pref '(:location :longitude) "")
                     :width 80
                     :callback (lambda (gadget value)
                                 (declare (ignore gadget))
                                 (setf (get-pref '(:location :longitude)) value)
                                 (clim:redisplay-frame-panes frame :force-p t))) pane)
    (format pane "°")
    (format pane "FIXME: N/S?")))

(defun macos-p ()
  (or (search "macos" (software-type)) (search "darwin" (software-type))
      (and (probe-file "/System/") (probe-file "/Applications/"))))

(defun display-accessibility-tab (frame pane)
  "Display accessibility settings with modular sections"
  (get-pref '(:accessibility :keybind-theme) (if (macos-p) :macos :gnome))
  (display-color-theme-section frame pane)
  (display-keyboard-shortcuts-section frame pane))
