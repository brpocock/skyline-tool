;; Paper Size Tab - Preferences Inspector
;; Contains paper size selection and unit conversion functionality

(in-package :skyline-tool)

(defconstant +paper-sizes+
  '((:us-letter "U.S. Letter" 215.9 279.4)
    (:us-legal "U.S. Legal" 215.9 355.6)
    (:a4 "A4" 210.0 297.0))
  :test 'equalp
  :documentation "Paper size options: (key label width-mm height-mm).")

(defun display-paper-size-tab (frame pane)
  "Display the Paper Size tab content"
  (clim:formatting-table-pane (pane :name "paper-tab"))
  (make-section-header pane "Paper Size")
  (let ((current-size (get-pref '(:paper :size) :us-letter)))
    (let ((box (clim:make-pane 'clim:radio-box)))
      (setf (paper-size-box frame) box)
      (setf (paper-size-radio frame)
            (loop for (key label w h) in +paper-sizes+
                  collect (clim:make-pane 'clim:toggle-button
                                          :label label
                                          :value (eq current-size key)
                                          :group box
                                          :value-changed-callback
                                          (lambda (g v)
                                            (declare (ignore g))
                                            (when v
                                              (setf (get-pref '(:paper :size)) key)
                                              (when w
                                                (setf (get-pref '(:paper :width)) w)
                                                (setf (get-pref '(:paper :height)) h))
                                              (save-preferences-now frame)
                                              (clim:redisplay-frame-panes frame :force-p t))))))
      (make-full-width-row pane (clim:note-gadget-activated box pane))))

  ;; --- Paper Unit (only when Custom) ---
  (when (eq (get-pref '(:paper :size) :us-letter) :custom)
    (make-section-header pane "Paper Unit")
    (let* ((current-unit (get-pref '(:units :length) :mm))
           (box (clim:make-pane 'clim:radio-box)))
      (setf (paper-unit-box frame) box)
      (setf (paper-unit-radio frame)
            (loop for unit in '(:mm :cm :in :pt)
                  collect (clim:make-pane 'clim:toggle-button
                                          :label (string unit)
                                          :value (eq current-unit unit)
                                          :group box
                                          :value-changed-callback
                                          (lambda (g v)
                                            (declare (ignore g))
                                            (when v
                                              (let ((old-unit (get-pref '(:units :length)))
                                                    (new-unit unit))
                                                (setf (get-pref '(:units :length)) new-unit)
                                                (let ((w (get-pref '(:paper :width)))
                                                      (h (get-pref '(:paper :height))))
                                                  (when (and w h)
                                                    (setf (get-pref '(:paper :width))
                                                          (round (convert-unit w old-unit new-unit) 1))
                                                    (setf (get-pref '(:paper :height))
                                                          (round (convert-unit h old-unit new-unit) 1)))))
                                              (save-preferences-now frame)
                                              (clim:redisplay-frame-panes frame :force-p t))))))
      (make-full-width-row pane (clim:note-gadget-activated box pane))

      (make-label-value-row pane "Width"
                            (unless (paper-width-field frame)
                              (setf (paper-width-field frame)
                                    (clim:make-pane 'clim:text-field
                                                    :value (format nil "~a" (get-pref '(:paper :width) 215.9))
                                                    :width 80
                                                    :activate-callback
                                                    (lambda (gadget value)
                                                      (declare (ignore gadget))
                                                      (setf (get-pref '(:paper :width)) (parse-number value))
                                                      (save-preferences-now frame))))))
      (clim:note-gadget-activated (paper-width-field frame) pane)

      (make-label-value-row pane "Height"
                            (unless (paper-height-field frame)
                              (setf (paper-height-field frame)
                                    (clim:make-pane 'clim:text-field
                                                    :value (format nil "~a" (get-pref '(:paper :height) 279.4))
                                                    :width 80
                                                    :activate-callback
                                                    (lambda (gadget value)
                                                      (declare (ignore gadget))
                                                      (setf (get-pref '(:paper :height)) (parse-number value))
                                                      (save-preferences-now frame))))))
      (clim:note-gadget-activated (paper-height-field frame) pane)))