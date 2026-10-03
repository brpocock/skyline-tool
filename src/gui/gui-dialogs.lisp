;;; Skyline-Tool src/gui/gui-dialogs.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

;;; Generic dialog frames for user input using CLIM gadgets

(in-package :skyline-tool)

;;; Generic Text Input Dialog

(clim:define-application-frame text-input-dialog ()
  ((prompt :initarg :prompt :accessor dialog-prompt :initform "")
   (initial-value :initarg :initial-value :accessor dialog-initial-value :initform "")
   (callback :initarg :callback :accessor dialog-callback :initform nil)
   (result :accessor dialog-result :initform nil)
   (confirmed :accessor dialog-confirmed :initform nil)
   (text-field :accessor dialog-text-field :initform nil))
  (:panes
   (prompt-pane :application
                :display-function 'display-dialog-prompt
                :height 40 :width 400)
   (input-pane :application
               :display-function 'display-dialog-input
               :height 40 :width 400)
   (button-pane :application
                :display-function 'display-dialog-buttons
                :height 50 :width 400))
  (:layouts
   (default (clim:vertically () prompt-pane input-pane button-pane)))
  (:icon (skyline-tool-icon :resource :dialog))
  (:pretty-name "Text Input"))

(defun display-dialog-prompt (frame pane)
  (clim:stream-set-cursor-position pane 10 10)
  (write-string (dialog-prompt frame) pane))

(defun display-dialog-input (frame pane)
  (unless (dialog-text-field frame)
    (setf (dialog-text-field frame)
          (clim:make-pane 'clim:text-field
                          :value (dialog-initial-value frame)
                          :width 380
                          :activate-callback (lambda (gadget value)
                                               (declare (ignore gadget))
                                               (setf (dialog-result frame) value)))))
  (clim:stream-set-cursor-position pane 10 5)
  (clim:note-gadget-activated (dialog-text-field frame) pane))

(defun display-dialog-buttons (frame pane)
  (clim:stream-set-cursor-position pane 10 5)
  (let ((ok-btn (clim:make-pane 'clim:push-button
                                :label "OK"
                                :activate-callback (lambda (gadget)
                                                     (declare (ignore gadget))
                                                     (setf (dialog-confirmed frame) t)
                                                     (when (dialog-callback frame)
                                                       (funcall (dialog-callback frame) (dialog-result frame)))
                                                     (clim:frame-exit clim:*application-frame*))))
        (cancel-btn (clim:make-pane 'clim:push-button
                                    :label "Cancel"
                                    :activate-callback (lambda (gadget)
                                                         (declare (ignore gadget))
                                                         (setf (dialog-confirmed frame) nil)
                                                         (clim:frame-exit clim:*application-frame*)))))
    (clim:note-gadget-activated ok-btn pane)
    (clim:stream-set-cursor-position pane 120 5)
    (clim:note-gadget-activated cancel-btn pane)))

(defun run-text-input-dialog (prompt &key (initial-value "") (title "Input"))
  "Run a text input dialog and return the entered string, or NIL if cancelled."
  (let ((frame (clim:make-application-frame 'text-input-dialog
                                            :prompt prompt
                                            :initial-value initial-value
                                            :pretty-name title
                                            :width 420 :height 180)))
    (clim:run-frame-top-level frame)
    (when (dialog-confirmed frame)
      (dialog-result frame))))

;;; Generic Number Input Dialog

(clim:define-application-frame number-input-dialog ()
  ((prompt :initarg :prompt :accessor dialog-prompt :initform "")
   (initial-value :initarg :initial-value :accessor dialog-initial-value :initform 0)
   (min-value :initarg :min-value :accessor dialog-min-value :initform 0)
   (max-value :initarg :max-value :accessor dialog-max-value :initform 255)
   (callback :initarg :callback :accessor dialog-callback :initform nil)
   (result :accessor dialog-result :initform nil)
   (confirmed :accessor dialog-confirmed :initform nil)
   (text-field :accessor dialog-text-field :initform nil))
  (:panes
   (prompt-pane :application
                :display-function 'display-dialog-prompt
                :height 40 :width 400)
   (input-pane :application
               :display-function 'display-number-dialog-input
               :height 40 :width 400)
   (button-pane :application
                :display-function 'display-dialog-buttons
                :height 50 :width 400))
  (:layouts
   (default (clim:vertically () prompt-pane input-pane button-pane)))
  (:icon (skyline-tool-icon :resource :dialog))
  (:pretty-name "Number Input"))

(defun display-number-dialog-input (frame pane)
  (unless (dialog-text-field frame)
    (setf (dialog-text-field frame)
          (clim:make-pane 'clim:text-field
                          :value (format nil "~a" (dialog-initial-value frame))
                          :width 380
                          :activate-callback (lambda (gadget value)
                                               (declare (ignore gadget))
                                               (let ((parsed (ignore-errors (parse-integer value :junk-allowed t))))
                                                 (when (and parsed
                                                            (>= parsed (dialog-min-value frame))
                                                            (<= parsed (dialog-max-value frame)))
                                                   (setf (dialog-result frame) parsed)))))))
  (clim:stream-set-cursor-position pane 10 5)
  (clim:note-gadget-activated (dialog-text-field frame) pane))

(defun run-number-input-dialog (prompt &key (initial-value 0) (min-value 0) (max-value 255) (title "Number Input"))
  "Run a number input dialog and return the entered number, or NIL if cancelled."
  (let ((frame (clim:make-application-frame 'number-input-dialog
                                            :prompt prompt
                                            :initial-value initial-value
                                            :min-value min-value
                                            :max-value max-value
                                            :pretty-name title
                                            :width 420 :height 180)))
    (clim:run-frame-top-level frame)
    (when (dialog-confirmed frame)
      (dialog-result frame))))

;;; Generic Multi-Field Input Dialog

(clim:define-application-frame multi-field-input-dialog ()
  ((prompt :initarg :prompt :accessor dialog-prompt :initform "")
   (fields :initarg :fields :accessor dialog-fields :initform nil)
   (callback :initarg :callback :accessor dialog-callback :initform nil)
   (result :accessor dialog-result :initform nil)
   (confirmed :accessor dialog-confirmed :initform nil)
   (field-gadgets :accessor dialog-field-gadgets :initform nil))
  (:panes
   (prompt-pane :application
                :display-function 'display-dialog-prompt
                :height 40 :width 400)
   (fields-pane :application
                :display-function 'display-multi-field-input
                :height 200 :width 400
                :scroll-bars :vertical)
   (button-pane :application
                :display-function 'display-dialog-buttons
                :height 50 :width 400))
  (:layouts
   (default (clim:vertically () prompt-pane fields-pane button-pane)))
  (:icon (skyline-tool-icon :resource :dialog))
  (:pretty-name "Multi Field Input"))

(defun display-multi-field-input (frame pane)
  (let ((fields (dialog-fields frame))
        (y 10))
    (dolist (field fields)
      (let* ((label (getf field :label))
             (init-value (getf field :value ""))
             (field-type (getf field :type 'string))
             (gadget (cond
                       ((eq field-type 'string)
                        (clim:make-pane 'clim:text-field
                                        :value init-value
                                        :width 360))
                       ((eq field-type 'integer)
                        (clim:make-pane 'clim:text-field
                                        :value (format nil "~a" init-value)
                                        :width 360))
                       (t
                        (clim:make-pane 'clim:text-field
                                        :value init-value
                                        :width 360)))))
        (push (cons (getf field :name) gadget) (dialog-field-gadgets frame))
        (clim:stream-set-cursor-position pane 10 y)
        (write-string label pane)
        (clim:stream-set-cursor-position pane 150 y)
        (clim:note-gadget-activated gadget pane)
        (incf y 30)))))

(defun run-multi-field-input-dialog (prompt fields &key (title "Input"))
  "Run a multi-field input dialog. FIELDS is a list of plists with :NAME, :LABEL, :VALUE, :TYPE."
  (let ((frame (clim:make-application-frame 'multi-field-input-dialog
                                            :prompt prompt
                                            :fields fields
                                            :pretty-name title
                                            :width 420 :height 350)))
    (clim:run-frame-top-level frame)
    (when (dialog-confirmed frame)
      (let ((result ()))
        (dolist (pair (dialog-field-gadgets frame))
          (push (cons (car pair) (clim:gadget-value (cdr pair))) result))
        (nreverse result)))))

;;; Generic Selection Dialog (from a list)

(clim:define-application-frame selection-dialog ()
  ((prompt :initarg :prompt :accessor dialog-prompt :initform "")
   (options :initarg :options :accessor dialog-options :initform nil)
   (callback :initarg :callback :accessor dialog-callback :initform nil)
   (result :accessor dialog-result :initform nil)
   (confirmed :accessor dialog-confirmed :initform nil)
   (list-pane :accessor dialog-list-pane :initform nil))
  (:panes
   (prompt-pane :application
                :display-function 'display-dialog-prompt
                :height 40 :width 400)
   (list-pane :application
              :display-function 'display-selection-list
              :height 200 :width 400
              :scroll-bars :vertical)
   (button-pane :application
                :display-function 'display-dialog-buttons
                :height 50 :width 400))
  (:layouts
   (default (clim:vertically () prompt-pane list-pane button-pane)))
  (:icon (skyline-tool-icon :resource :dialog))
  (:pretty-name "Selection"))

(defun display-selection-list (frame pane)
  (unless (dialog-list-pane frame)
    (setf (dialog-list-pane frame)
          (clim:make-pane 'clim:list-pane
                          :items (dialog-options frame)
                          :value-changed-callback (lambda (gadget value)
                                                    (declare (ignore gadget))
                                                    (setf (dialog-result frame) value)))))
  (clim:stream-set-cursor-position pane 10 5)
  (clim:note-gadget-activated (dialog-list-pane frame) pane))

(defun run-selection-dialog (prompt options &key (title "Select") (default nil))
  "Run a selection dialog and return the selected item, or NIL if cancelled."
  (let ((frame (clim:make-application-frame 'selection-dialog
                                            :prompt prompt
                                            :options options
                                            :pretty-name title
                                            :width 420 :height 340)))
    (when default
      (setf (dialog-result frame) default))
    (clim:run-frame-top-level frame)
    (when (dialog-confirmed frame)
      (dialog-result frame))))

;;; Color Selection Dialog

(defun run-color-selection-dialog (prompt &key (current "White") (title "Color Selection"))
  "Run a color selection dialog with Atari color names."
  (let* ((ntsc-names (mapcar (lambda (s) (format nil "NTSC: ~a" s)) +atari-ntsc-color-names+))
         (pal-names (mapcar (lambda (s) (format nil "PAL: ~a" s)) +atari-pal-color-names+))
         (all-colors (remove-duplicates (append ntsc-names pal-names) :test #'string-equal))
         (default (cond
                    ((search "NTSC:" current) (position current all-colors :test #'string-equal))
                    ((search "PAL:" current) (position current all-colors :test #'string-equal))
                    (t (position (format nil "NTSC: ~a" current) all-colors :test #'string-equal)))))
    (run-selection-dialog prompt all-colors :title title :default (when default (nth default all-colors)))))

;;; Confirmation Dialog

(clim:define-application-frame confirm-dialog ()
  ((prompt :initarg :prompt :accessor dialog-prompt :initform "")
   (default-action :initarg :default-action :accessor dialog-default-action :initform "OK")
   (danger-action :initarg :danger-action :accessor dialog-danger-action :initform "Cancel")
   (result :accessor dialog-result :initform nil)
   (confirmed :accessor dialog-confirmed :initform nil))
  (:panes
   (prompt-pane :application
                :display-function 'display-dialog-prompt
                :height 60 :width 400)
   (button-pane :application
                :display-function 'display-confirm-buttons
                :height 50 :width 400))
  (:layouts
   (default (clim:vertically () prompt-pane button-pane)))
  (:icon (skyline-tool-icon :resource :dialog))
  (:pretty-name "Confirm"))

(defun display-confirm-buttons (frame pane)
  (clim:stream-set-cursor-position pane 10 5)
  (let ((ok-btn (clim:make-pane 'clim:push-button
                                :label (dialog-default-action frame)
                                :activate-callback (lambda (gadget)
                                                     (declare (ignore gadget))
                                                     (setf (dialog-confirmed frame) t)
                                                     (setf (dialog-result frame) t)
                                                     (clim:frame-exit clim:*application-frame*))))
        (cancel-btn (clim:make-pane 'clim:push-button
                                    :label (dialog-danger-action frame)
                                    :activate-callback (lambda (gadget)
                                                         (declare (ignore gadget))
                                                         (setf (dialog-confirmed frame) nil)
                                                         (setf (dialog-result frame) nil)
                                                         (clim:frame-exit clim:*application-frame*)))))
    (clim:note-gadget-activated ok-btn pane)
    (clim:stream-set-cursor-position pane 120 5)
    (clim:note-gadget-activated cancel-btn pane)))

(defun run-confirm-dialog (prompt &key (default-action "Execute") (danger-action "Cancel")
                                       (title "Confirm"))
  "Run a confirmation dialog and return T if confirmed, NIL if cancelled."
  (let ((frame (clim:make-application-frame 'confirm-dialog
                                            :prompt prompt
                                            :default-action default-action
                                            :danger-action danger-action
                                            :pretty-name title
                                            :width 420 :height 160)))
    (clim:run-frame-top-level frame)
    (dialog-result frame)))

;;; Boolean Dialog (Yes/No)

(defun run-boolean-dialog (prompt &key (default nil) (title "Confirm"))
  "Run a Yes/No dialog and return T for Yes, NIL for No."
  (run-confirm-dialog prompt :default-action "Yes" :danger-action "No" :title title))

