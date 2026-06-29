;;;; About Skyline-Tool Dialog
;;;; Copyright © 2026 Interworldly Adventuring, LLC.

(in-package :skyline-tool)

;; --- About dialog specific command tables ---

(clim:define-command-table about-file-menu
  :menu (("Save As" :menu about-save-as-menu)
         ("Print To" :menu about-print-to-menu)
         (nil :divider :line)
         ("Close" :command com-close-about)))

(clim:define-command-table about-save-as-menu
  :menu (("Text..." :command com-save-about-text)
         ("JSON..." :command com-save-about-json)
         ("PDF..." :command com-save-about-pdf)))

(clim:define-command-table about-print-to-menu
  :menu (("Choose Printer..." :command com-print-about-to-printer)))

(clim:define-command-table about-menu-bar
  :menu (("About Skyline-Tool" :menu about-file-menu)))

;; --- About dialog commands ---

(clim:define-command (com-close-about :menu nil :name t) ()
  (clim:frame-exit *application-frame*))

(clim:define-command (com-save-about-pdf :menu nil :name t) ()
  (let ((frame *application-frame*))
    (when (and frame (frame-pdf-function frame))
      (funcall (frame-pdf-function frame)
               (format nil "About-Skyline-Tool-~a.pdf"
                       (format-timestring nil (get-universal-time) :format '(:year :month :day :hour :min :sec)))))))

(clim:define-command (com-save-about-text :menu nil :name t) ()
  (let ((frame *application-frame*))
    (when (and frame (clim-simple-echo::frame-captured-text frame))
      (let ((filename (format nil "About-Skyline-Tool-~a.txt"
                              (format-timestring nil (get-universal-time) :format '(:year :month :day :hour :min :sec)))))
        (with-open-file (out filename :direction :output :if-exists :supersede)
          (write-string (clim-simple-echo::frame-captured-text frame) out))
        (format *query-io* "~&Saved to ~a~%" filename)))))

(clim:define-command (com-save-about-json :menu nil :name t) ()
  (let ((frame *application-frame*))
    (when (and frame (clim-simple-echo::frame-captured-text frame))
      (let* ((data (%about-data))
             (filename (format nil "About-Skyline-Tool-~a.json"
                               (format-timestring nil (get-universal-time) :format '(:year :month :day :hour :min :sec))))
             (json (with-output-to-string (s)
                     (json:encode-json-to-string
                      (list* (cons "timestamp" (getf data :timestamp))
                             (cons "user" (getf data :user))
                             (cons "machine" (getf data :machine))
                             (cons "cpu" (getf data :cpu))
                             (cons "os" (getf data :os))
                             (cons "lisp" (getf data :lisp))
                             (cons "site" (getf data :site))
                             (cons "version" (getf data :version))
                             (cons "compiled" (getf data :compiled))
                             (cons "text" (clim-simple-echo::frame-captured-text frame))
                             s)))))
        (with-open-file (out filename :direction :output :if-exists :supersede)
          (write-string json out))
        (format *query-io* "~&Saved to ~a~%" filename)))))

(clim:define-command (com-print-about-to-printer :menu nil :name t) ((printer-name string))
  (let ((frame *application-frame*))
    (when (and frame (frame-pdf-function frame))
      (let ((temp-ps (format nil "/tmp/about-~a.ps" (get-universal-time))))
        (funcall (frame-pdf-function frame) temp-ps)
        (uiop:run-program (list "lp" "-d" printer-name temp-ps)
                          :ignore-error-status t)
        (format *query-io* "~&Sent to printer ~a~%" printer-name)))))

;; Populate the Print To menu with available printers
(defun populate-about-print-to-menu ()
  (let ((ct (clim:find-command-table 'about-print-to-menu)))
    (when ct
      ;; Add printer items directly
      (dolist (printer (discover-printers-with-names))
        (let ((queue (car printer))
              (display (cdr printer)))
          (clim:add-menu-item-to-command-table ct display
            :command 'com-print-about-to-printer
            :arguments (list queue)
            :after :end)))))

;; --- About dialog frame and display ---

(clim:define-application-frame about-skyline-tool-frame ()
  ()
  (:panes (about-pane :application :height 500 :width 600
                                          :display-function 'display-about-skyline-tool))
  (:menu-bar about-menu-bar)
  (:icon (skyline-tool-icon))
  (:layouts (default about-pane))
  (:pdf-function '%about-pdf))

(defun display-about-skyline-tool (frame pane)
  (declare (ignore frame))
  (let* ((data (%about-data))
         (pane-width (clim:bounding-rectangle-width (clim:sheet-region pane))))
    ;; Centered header: icon + royal-blue title
    (let* ((icon-path (asdf:system-relative-pathname :skyline-tool "../Tools/skyline-tool-icon-64.png"))
           (icon-pattern (and (probe-file icon-path)
                              (ignore-errors (clim:make-pattern-from-bitmap-file icon-path)))))
      (when icon-pattern
        (let* ((icon-size 64)
               (title-text "Skyline-Tool")
               (title-style (clim:make-text-style :sans-serif :bold :huge))
               (title-width (nth-value 0 (clim:text-size pane title-text :text-style title-style)))
               (gap 16)
               (total-width (+ icon-size gap title-width))
               (start-x (max 0 (/ (- pane-width total-width) 2)))
               (y 16))
          (clim:draw-pattern* pane icon-pattern start-x y)
          (clim:with-drawing-options (pane :ink (clim:make-rgb-color 0 0.2 0.6))
            (clim:with-text-style (pane title-style)
              (clim:draw-text* pane title-text (+ start-x icon-size gap) (+ y 48))))))))
    ;; Horizontal rule below header
    (clim:draw-line* pane 10 90 (- pane-width 10) 90
                     :ink (clim:make-rgb-color 0 0.2 0.6) :line-thickness 2)
    ;; Advance text cursor below header
    (clim:stream-set-cursor-position *standard-output* 0 110)
    ;; Version / copyright / compilation
    (format *standard-output* "~&  Version ~a~%~%" (getf data :version))
    (format *standard-output* "  Copyright © 2014-2024 Bruce-Robert Pocock~%")
    (format *standard-output* "  Copyright © 2024-2026 Interworldly Adventuring, LLC~%~%")
    (format *standard-output* "  ~a~%~%" (getf data :compiled))
    ;; Two-column data table using fixed-width text
    (flet ((row (label value)
             (format *standard-output* "~&  ~12a ~a~%" label (or value "?"))))
      (row "Currently:" (getf data :timestamp))
      (row "User:" (getf data :user))
      (row "Machine:" (getf data :machine))
      (row "CPU:" (getf data :cpu))
      (row "OS:" (getf data :os))
      (row "Lisp:" (getf data :lisp))
      (row "Site:" (getf data :site)))
    (terpri))

(defun show-about-skyline-tool ()
  "Open the About Skyline-Tool dialog."
  (let* ((fm (clim:find-frame-manager :port (or (clim:find-port) (clim:find-port :server-path :x))))
         (frame (clim:make-application-frame 'about-skyline-tool-frame
                                             :frame-manager fm
                                             :width 620 :height 520)))
    (populate-about-print-to-menu)
    (clim-sys:make-process
     (lambda ()
       (clim:run-frame-top-level frame))
     :name "About Skyline-Tool")))

;; Export the command for global access
(clim:define-command (com-about-skyline-tool :command-table clim-internals::global-command-table) ()
  (show-about-skyline-tool)))
