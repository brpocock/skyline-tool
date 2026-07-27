;; Accessibility Tab - Preferences Inspector
;; Contains theme and keyboard shortcut configuration

(in-package :skyline-tool)

(defun display-accessibility-tab (frame pane)
  "Display the Accessibility tab content"
  (make-section-header pane "Accessibility Settings")
  (make-label-value-row pane "Color Theme"
                        (unless (accessibility-theme-field frame)
                          (setf (accessibility-theme-field frame)
                                (clim:make-pane 'clim:text-field
                                                :value (get-pref '(:accessibility :theme) "default")
                                                :width 200
                                                :activate-callback
                                                (lambda (gadget value)
                                                  (declare (ignore gadget))
                                                  (setf (get-pref '(:accessibility :theme)) value))))))
  (clim:note-gadget-activated (accessibility-theme-field frame) pane)
  
  
  (make-label-value-row pane "Keyboard Shortcuts"
                        (unless (accessibility-shortcut-field frame)
                          (setf (accessibility-shortcut-field frame)
                                (clim:make-pane 'clim:text-field
                                                :value (get-pref '(:accessibility :shortcut) "")
                                                :width 150
                                                :activate-callback
                                                (lambda (gadget value)
                                                  (declare (ignore gadget))
                                                  (setf (get-pref '(:accessibility :shortcut)) value))))))
  (clim:note-gadget-activated (accessibility-shortcut-field frame) pane))
