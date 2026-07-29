(in-package :skyline-tool)

(define-constant +paper-sizes+
  '((:us-letter "U.S. Letter" 215.9 279.4)
    (:us-legal "U.S. Legal" 215.9 355.6)
    (:a4 "A4" 210.0 297.0))
  :test 'equalp
  :documentation "Paper size options: (key label width-mm height-mm).")

(defun read-default-unit-from-dbus ()
  "Return :mm for metric locales, :in for imperial (like US) via DBus.
   Falls back to :mm."
  :mm)

(defun display-paper-size-tab (frame pane)
  "Display the Paper Size tab content with dynamic unit conversion."
  (let ((current-unit (or (get-pref '(:units :length))
                          (read-default-unit-from-dbus)))
        (current-size (get-pref '(:paper :size) :us-letter)))

    (make-section-header pane "Paper Size")
    
    ;; Main paper size buttons
    (clim:note-gadget-activated
     (clim:make-pane
      'clim:radio-box
      :label "Paper Size:"
      :value current-size
      :callback (lambda (gadget value)
                  (declare (ignore gadget))
                  (setf (get-pref '(:paper :size)) value)
                  (clim:redisplay-frame-panes frame :force-p t))
      :group :paper-sizes
      :items (loop for (key label w h) in +paper-sizes+
                   collect (clim:make-pane
                            'clim:toggle-button
                            :label label
                            :value (eq current-size key)
                            :callback (lambda (g v)
                                        (declare (ignore g))
                                        (when v
                                          (setf (get-pref '(:paper :size)) key)
                                          (clim:redisplay-frame-panes frame :force-p t))))))
     pane)
    
    ;; Custom size section
    (when (eq current-size :custom)
      (make-section-header pane "Custom Size")
      
      ;; Width field with unit dropdown
      (clim:note-gadget-activated
       (clim:make-pane 'clim:text-field
                       :value (format nil "~a" (get-pref '(:paper :width) 215.9))
                       :width 60
                       :activate-callback (lambda (gadget value)
                                            (declare (ignore gadget))
                                            (setf (get-pref '(:paper :width)) (parse-number value))
                                            (clim:redisplay-frame-panes frame :force-p t)))
       pane)
      
      ;; Width unit dropdown
      (let ((box (clim:make-pane 'clim:radio-box)))
        (setf (paper-unit-box frame) box)
        (loop for unit in '(:mm :cm :pt :pc :in)
              collect (clim:make-pane
                       'clim:toggle-button
                       :label (string unit)
                       :value (eq current-unit unit)
                       :group box
                       :callback (lambda (g v)
                                   (declare (ignore g))
                                   (when v
                                     (setf (get-pref '(:units :length)) unit)
                                     (clim:redisplay-frame-panes frame :force-p t)))))
        (clim:note-gadget-activated box pane))
      
      ;; Height field with unit dropdown
      (clim:note-gadget-activated
       (clim:make-pane 'clim:text-field
                       :value (format nil "~a" (get-pref '(:paper :height) 279.4))
                       :width 60
                       :activate-callback (lambda (gadget value)
                                            (declare (ignore gadget))
                                            (setf (get-pref '(:paper :height)) (parse-number value))
                                            (clim:redisplay-frame-panes frame :force-p t)))
       pane)
      
      ;; Height unit dropdown
      (let ((box (clim:make-pane 'clim:radio-box)))
        (setf (paper-unit-box frame) box)
        (loop for unit in '(:mm :cm :pt :pc :in)
              collect (clim:make-pane
                       'clim:toggle-button
                       :label (string unit)
                       :value (eq current-unit unit)
                       :group box
                       :callback (lambda (g v)
                                   (declare (ignore g))
                                   (when v
                                     (setf (get-pref '(:units :length)) unit)
                                     (clim:redisplay-frame-panes frame :force-p t)))))
        (clim:note-gadget-activated box pane)))))

