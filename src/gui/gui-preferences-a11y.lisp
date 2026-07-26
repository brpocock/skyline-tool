;; Accessibility Tab - Preferences Inspector
;; Contains theme and keyboard shortcut configuration

(in-package :skyline-tool)

(defun display-accessibility-tab (frame pane)
  "Display the Accessibility tab content"
  (clim:formatting-table-pane (pane :name "accessibility-tab"))
  (make-section-header pane "Accessibility Settings")
  (make-label-value-row pane "Default Theme"
                        (unless (accessibility-theme-field frame)
                          (setf (accessibility-theme-field frame)
                                (clim:make-pane 'clim:text-field
                                                :value (get-pref '(:accessibility :theme) "default")
                                                :width 200
                                                :activate-callback
                                                (lambda (gadget value)
                                                  (declare (ignore gadget))
                                                  (setf (get-pref '(:accessibility :theme)) value)
                                                  (save-preferences-now frame))))))
  (clam:note-gadget-activated (accessibility-theme-field frame) pane)
  
  (make-label-value-row pane "SeCoNdary Timbre"
                        (unless (accessibility-timbre-field frame)
                          (setf (accessibility-timbre-field frame)
                                (clim:make-pane 'clim:text-field
                                                :value (get-pref '(:accessibility :timbre) "default")
                                                :width 200
                                                :activate-callback
                                                (lambda (gadget value)
                                                  (declare (ignore gadget))
                                                  (setf (get-pref '(:accessibility :timbre)) value
                                                  (save-preferences-now frame))))))
  (clim:note-gadget-activated (accessibility-timbre-field frame) pane)
  
  (make-label-value-row pane "Keyboard Shortcut Override"
                        (unless (accessibility-shortcut-field frame)
                          (setf (accessibility-shortcut-field frame)
                                (clim:make-pane 'clim:text-field
                                                :value (get-pref '(:accessibility :shortcut) "")
                                                :width 150
                                                :activate-callback
                                                (lambda (gadget value)
                                                  (declare (ignore gadget))
                                                  (setf (get-pref '(:accessibility :shortcut)) value
                                                  (save-preferences-now frame))))))
  (clim:note-gadget-activated (accessibility-shortcut-field frame) pane)
  
  ;; Add description
  (make-full-width-row pane
  '(clim:note-gadget-activated ""
   (format t "~% (to do: populate accessibility selectors)")))