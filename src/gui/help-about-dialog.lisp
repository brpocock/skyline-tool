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
  :menu ())

(clim:define-command-table about-menu-bar
  :menu (("About Skyline-Tool" :menu about-file-menu)))

;; --- About dialog commands ---

(clim:define-command (com-close-about :command-table clim-internals::global-command-table
                                        :menu t :name t)
    ((frame clim:application-frame))
  (clim:frame-exit (clim:frame-manager frame)))

(clim:define-command (com-save-about-pdf :command-table clim-internals::global-command-table
                                          :menu t :name t)
    ((frame clim:application-frame))
  (when (frame-pdf-function frame)
    (funcall (frame-pdf-function frame)
             (format nil "About-Skyline-Tool-~a.pdf"
                     (format-timestring nil (get-universal-time) :format '(:year :month :day :hour :min :sec))))))

(clim:define-command (com-save-about-text :command-table clim-internals::global-command-table
                                            :menu t :name t)
    ((frame clim:application-frame))
  (when (clim-simple-echo::frame-captured-text frame)
    (let ((filename (format nil "About-Skyline-Tool-~a.txt"
                            (format-timestring nil (get-universal-time) :format '(:year :month :day :hour :min :sec)))))
      (with-open-file (out filename :direction :output :if-exists :supersede)
        (write-string (clim-simple-echo::frame-captured-text frame) out))
      (format *query-io* "~&Saved to ~a~%" filename))))

(clim:define-command (com-save-about-json :command-table clim-internals::global-command-table
                                            :menu t :name t)
    ((frame clim:application-frame))
  (when (clim-simple-echo::frame-captured-text frame)
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
      (format *query-io* "~&Saved to ~a~%" filename))))

(clim:define-command (com-print-about-to-printer :command-table clim-internals::global-command-table
                                                 :menu nil :name t)
    ((frame clim:application-frame) (printer-name string))
  (when (frame-pdf-function frame)
    (let ((temp-ps (format nil "/tmp/about-~a.ps" (get-universal-time))))
      (funcall (frame-pdf-function frame) temp-ps)
      (ensure-thumbnail-kernel)  ; reuse LParallel kernel
      (lparallel:submit-task
       (lambda ()
         (uiop:run-program (list "lp" "-d" printer-name temp-ps)
                           :ignore-error-status t))))))


;; Populate any Print To menu with available printers
;; If no printers are found, adds "Default Printer (lpr)" as fallback
(defun populate-print-menu (command-table-name)
  "Populate the given command table with printer items.
   If CUPS printers are discovered, add each one.
   Otherwise, add 'Default Printer (lpr)' as fallback"
  (when command-table-name
    (let ((printers (ignore-errors (discover-printers-with-names))))
      (if printers
          (dolist (printer printers)
            (let ((queue (car printer))
                  (display (cdr printer)))
              (unless (clim:find-menu-item command-table-name display)
                (clim:add-menu-item-to-command-table command-table-name display
                                                     :command `(run-command-in-terminal-echo ,queue "make test")
                                                     :after :end))))
          (unless (clim:find-menu-item command-table-name "Default Printer (lpr)")
            (clim:add-menu-item-to-command-table command-table-name "Default Printer (lpr)"
                                                 :command `(run-command-in-terminal-echo "make test")
                                                 :after :end))))))

;; Populate the Print To menu in the About dialog
(defun populate-about-print-to-menu ()
  (populate-print-menu 'about-print-to-menu))

;; --- About dialog frame and display ---

(clim:define-command-table about-skyline-tool-commands
  :inherit-from (clim-internals::global-command-table))

(clim:define-application-frame about-skyline-tool-frame (clim-simple-echo::simple-echo)
   ()
   (:command-table (about-skyline-tool-commands))
   (:panes (about-pane :application :height 500 :width 600
                                        :display-function 'display-about-skyline-tool))
   (:menu-bar about-menu-bar)
   (:icon (skyline-tool-icon :resource :about))
   (:layouts (default about-pane)))

(defun display-about-skyline-tool (frame pane)
  (declare (ignore frame))
  (let* ((data (%about-data))
         (pane-width (clim:bounding-rectangle-width (clim:sheet-region pane))))
    (let ((*standard-output* pane))
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
    ;; Position stream cursor below the rule
    (clim:stream-set-cursor-position pane 10 100)
    ;; Version / copyright / compilation
    (format pane "~&~%  Version ~a~%~%" (getf data :version))
    (format pane "  Copyright © 2014-2024 Bruce-Robert Pocock~%")
    (format pane "  Copyright © 2024-2026 Interworldly Adventuring, LLC~%~%")
    (format pane "  ~a~%~%" (getf data :compiled))
    ;; Two-column data rows
    (format pane "~&  Currently: ~a~%~%" (getf data :timestamp))
    (format pane "  User: ~a~%" (getf data :user))
    (format pane "  Machine: ~a~%" (getf data :machine))
    (format pane "  CPU: ~a~%" (getf data :cpu))
    (format pane "  OS: ~a~%" (getf data :os))
    (format pane "  Lisp: ~a~%" (getf data :lisp))
    (format pane "  Site: ~a~%" (getf data :site))
    (terpri)))

(defun show-about-skyline-tool ()
  "Open the About Skyline-Tool dialog."
  (let* ((fm (clim:find-frame-manager :port (or (clim:find-port) (clim:find-port :server-path :x))))
         (frame (clim:make-application-frame 'about-skyline-tool-frame
                                              :pretty-name "About Skyline-Tool"
                                              :frame-manager fm
                                             :width 620 :height 620)))
    (populate-about-print-to-menu)
    (clim-sys:make-process
     (lambda ()
       (clim:run-frame-top-level frame))
     :name "About Skyline-Tool")))

;; Export the command for global access
(clim:define-command (com-about-skyline-tool :command-table clim-internals::global-command-table) ()
  (show-about-skyline-tool))
