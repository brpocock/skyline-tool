;;;; Skyline-Tool src/gui/gui-journal.lisp
;;;; Journal Inspector with tag filtering

(in-package :skyline-tool)

(defpackage :skyline-tool.gui-journal
  (:use :cl :clim :clim-lisp :clim-extensions)
  (:export #:run-journal-inspector
           #:journal-inspector-frame
           #:journal-entry-tag
           #:extract-entry-tags))

(in-package :skyline-tool.gui-journal)

;; ──────────────────────────────────────────────────────────────────────
;; Journal Entry Presentation
;; ──────────────────────────────────────────────────────────────────────

(defclass journal-entry-presentation ()
  ((entry   :initarg :entry   :reader presentation-entry)
   (stream  :initarg :stream  :reader presentation-stream)
   (frame   :initarg :frame   :reader presentation-frame)))

(defmethod journal-entry-tag ((entry t))
  "Extract the symbolic tag from a journal event entry.
   Returns a keyword symbol representing the event type."
  (cond
    ((and (listp entry) (getf entry :type))
     (intern (string-upcase (getf entry :type)) :keyword))
    ((and (listp entry) (getf entry :event-type))
     (intern (string-upcase (getf entry :event-type)) :keyword))
    ((and (listp entry) (getf entry :event))
     (intern (string-upcase (getf entry :event)) :keyword))
    ((and (listp entry) (car entry))
     (let ((first (car entry)))
       (if (keywordp first)
           first
           (intern (string-upcase (symbol-name first)) :keyword))))
    (t :unknown)))

(defun extract-entry-tags (entries)
  "Return a list of unique tags present in ENTRIES."
  (remove-duplicates
   (mapcar #'journal-entry-tag entries)
   :test #'eq))

(defun format-journal-entry (entry stream)
  "Format a journal entry for display."
  (let ((tag (journal-entry-tag entry))
        (time (or (getf entry :timestamp) (getf entry :time) 0))
        (message (or (getf entry :message) (getf entry :msg) ""))
        (resource (getf entry :resource))
        (offer-id (getf entry :offer-id)))
    (format stream "[~a] ~a"
            tag
            (format-timestamp time))
    (when resource
      (format stream " Resource: ~a" resource))
    (when offer-id
      (format stream " Offer: ~a" offer-id))
    (when message
      (format stream " ~a" message))
    (terpri stream)))

(defun format-timestamp (universal-time)
  "Format universal time as HH:MM:SS."
  (multiple-value-bind (sec min hour day month year) (decode-universal-time universal-time)
    (declare (ignore day month year))
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour min sec)))

;; ──────────────────────────────────────────────────────────────────────
;; Search Bar Pane
;; ──────────────────────────────────────────────────────────────────────

(define-application-frame journal-search-bar ()
  ((selected-tag :initform :all :accessor search-bar-selected-tag)
   (available-tags :initform '(:all) :accessor search-bar-available-tags)
   (on-filter-change :initarg :on-filter-change :accessor search-bar-on-filter-change))
  (:panes (tag-selector :interactor-pane
                        :height 40
                        :display-function 'display-tag-selector
                        :display-after-commands t))
  (:layouts (default tag-selector)))

(defun display-tag-selector (frame pane)
  "Display the tag filter dropdown."
  (let ((tags (search-bar-available-tags frame))
        (selected (search-bar-selected-tag frame)))
    (formatting-table (pane)
      (formatting-row (pane)
        (formatting-cell (pane :align-x :left)
          (write-string "Tag: " pane))
        (formatting-cell (pane :align-x :left)
          (clim:accepting-values (pane)
            (let ((choice (clim:accept 'tag-symbol
                                      :prompt ""
                                      :default selected
                                      :presentation-type 'tag-symbol
                                      :presentation-type-args tags)))
              (when (and choice (not (eq choice selected)))
                (setf (search-bar-selected-tag frame) choice)
                (when (search-bar-on-filter-change frame)
                  (funcall (search-bar-on-filter-change frame) choice)))))))))

(define-presentation-type tag-symbol (options)
  :inherit-from 'symbol
  :options (options))

(define-presentation-method accept ((type tag-symbol) stream view &key options)
  (let ((choices (or options '(:all))))
    (clim:accepting-values (stream)
      (let ((result (clim:accept 'symbol
                                  :prompt "Tag"
                                  :default (first choices)
                                  :presentation-type `(member ,@choices))))
        (values result t)))))

;; ──────────────────────────────────────────────────────────────────────
;; Journal List Pane
;; ──────────────────────────────────────────────────────────────────────

(define-application-frame journal-list ()
  ((entries :initform nil :accessor journal-list-entries)
   (filtered-entries :initform nil :accessor journal-list-filtered-entries)
   (current-filter :initform :all :accessor journal-list-current-filter)
   (available-tags :initform '(:all) :accessor journal-list-available-tags))
  (:panes (entry-list :application
                      :height 400
                      :width 800
                      :display-function 'display-journal-entries
                      :scroll-bars t
                      :default-text-style (make-text-style :fix :roman :normal)))
  (:layouts (default entry-list)))

(defun display-journal-entries (frame pane)
  "Display the filtered journal entries."
  (let ((entries (journal-list-filtered-entries frame)))
    (if (null entries)
        (write-string "(No journal entries match the current filter.)" pane)
        (loop for entry in entries
              do (format-journal-entry entry pane)))))

(defun apply-journal-filter (frame tag)
  "Apply TAG filter to journal entries."
  (setf (journal-list-current-filter frame) tag)
  (let ((all-entries (journal-list-entries frame)))
    (setf (journal-list-filtered-entries frame)
          (if (eq tag :all)
              all-entries
              (remove-if-not (lambda (e) (eq (journal-entry-tag e) tag))
                             all-entries)))
    (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'entry-list)
                               :force-p t)))

(defun update-journal-tags (frame entries)
  "Update available tags from ENTRIES."
  (let ((tags (extract-entry-tags entries)))
    (setf (journal-list-available-tags frame) (cons :all tags))
    (setf (search-bar-available-tags (clim:find-pane-named frame 'tag-selector))
          (cons :all tags))))

;; ──────────────────────────────────────────────────────────────────────
;; Main Journal Inspector Frame
;; ──────────────────────────────────────────────────────────────────────

(define-command-table journal-file-menu
  :menu (("Close" :command com-close-journal)))

(define-command-table journal-edit-menu
  :menu (("Find..." :command com-find-in-journal)
         ("Copy" :command com-copy-journal)))

(define-command-table journal-help-menu
  :menu (("About Journal Inspector..." :command com-about-journal)))

(define-command-table journal-menu-bar
  :menu (("Journal" :menu journal-file-menu)
         ("Edit" :menu journal-edit-menu)
         ("Help" :menu journal-help-menu)))

(define-application-frame journal-inspector ()
  ((journal-data :initarg :journal-data :accessor journal-inspector-journal-data)
   (worker-journal :initarg :worker-journal :accessor journal-inspector-worker-journal)
   (list-pane :accessor journal-inspector-list-pane)
   (search-bar-pane :accessor journal-inspector-search-bar)
   (all-entries :initform nil :accessor journal-inspector-all-entries)
   (filtered-entries :initform nil :accessor journal-inspector-filtered-entries)
   (current-filter :initform :all :accessor journal-inspector-current-filter)
   (available-tags :initform '(:all) :accessor journal-inspector-available-tags))
  (:panes (main-container :application
                          :display-function 'display-journal-inspector)
          (search-bar :application
                      :height 50
                      :display-function 'display-search-bar))
  (:layouts (default (clim:vertically () main-container search-bar)))
  (:command-table (journal-inspector))
  (:menu-bar journal-menu-bar)
  (:icon (skyline-tool::skyline-tool-icon)))

(defmethod initialize-instance :after ((frame journal-inspector) &key)
  (let ((journal (or (journal-inspector-worker-journal frame) *worker-journal*)))
    (when journal
      (load-journal-data frame journal))))

(defun load-journal-data (frame journal)
  "Load journal events from JOURNAL into FRAME."
  (let ((events (journal:list-events journal)))
    (setf (journal-inspector-all-entries frame) events)
    (setf (journal-inspector-filtered-entries frame) events)
    (setf (journal-inspector-available-tags frame)
          (cons :all (extract-entry-tags events)))
    (clim:redisplay-frame-panes frame)))

(defun display-journal-inspector (frame pane)
  "Display the main journal content area."
  (let ((entries (journal-inspector-filtered-entries frame)))
    (if (null entries)
        (write-string "(No journal entries.)" pane)
        (loop for entry in entries
              do (format-journal-entry entry pane)))))

(defun display-search-bar (frame pane)
  "Display the search/filter bar beneath the main window."
  (formatting-table (pane)
    (formatting-row (pane)
      (formatting-cell (pane :align-x :left :min-width 50)
        (write-string "Tag: " pane))
      (formatting-cell (pane :align-x :left)
        (clim:accepting-values (pane)
          (let* ((tags (journal-inspector-available-tags frame))
                 (selected (journal-inspector-current-filter frame))
                 (choice (clim:accept 'symbol
                                       :prompt ""
                                       :default selected
                                       :presentation-type `(member ,@tags))))
            (when (and choice (not (eq choice selected)))
              (setf (journal-inspector-current-filter frame) choice)
              (apply-journal-filter-to-frame frame choice))))))))

(defun apply-journal-filter-to-frame (frame tag)
  "Apply TAG filter to the journal inspector frame."
  (let ((all-entries (journal-inspector-all-entries frame)))
    (setf (journal-inspector-filtered-entries frame)
          (if (eq tag :all)
              all-entries
              (remove-if-not (lambda (e) (eq (journal-entry-tag e) tag))
                             all-entries)))
    (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'main-container)
                               :force-p t)))

;; ──────────────────────────────────────────────────────────────────────
;; Commands
;; ──────────────────────────────────────────────────────────────────────

(define-journal-inspector-command (com-close-journal :menu t :name t) ()
  "Close the journal inspector window."
  (clim:frame-exit clim:*application-frame*))

(define-journal-inspector-command (com-find-in-journal :menu t :name t) ()
  "Open the search bar for filtering journal entries.
   The search bar is already displayed beneath the main window;
   this command focuses the tag selector."
  (let ((frame clim:*application-frame*))
    (when (typep frame 'journal-inspector)
      (let ((search-pane (clim:find-pane-named frame 'search-bar)))
        (when search-pane
          (clim:stream-set-cursor-position search-pane 0 0)
          (clim:redisplay-frame-pane frame search-pane :force-p t))))))

(define-journal-inspector-command (com-copy-journal :menu t :name t) ()
  "Copy visible journal entries to clipboard."
  (let* ((frame clim:*application-frame*)
         (entries (journal-inspector-filtered-entries frame))
         (output (with-output-to-string (s)
                   (loop for e in entries do (format-journal-entry e s)))))
    (when (and output (plusp (length output)))
      (ignore-errors
        (uiop:run-program '("wl-copy") :input output :output nil))
      (ignore-errors
        (uiop:run-program '("xclip" "-selection" "clipboard") :input output :output nil))
      (format *query-io* "~&Copied ~d entries to clipboard.~%" (length entries)))))

(define-journal-inspector-command (com-about-journal :menu t :name t) ()
  "Show about dialog for Journal Inspector."
  (format *query-io* "~&Journal Inspector — Skyline-Tool~%")
  (format *query-io* "~&View and filter worker journal events by tag.~%"))

;; ──────────────────────────────────────────────────────────────────────
;; Public API
;; ──────────────────────────────────────────────────────────────────────

(defun run-journal-inspector (&key (width 900) (height 600)
                                       journal-data
                                       worker-journal
                                       (frame-manager nil)
                                       (port nil)
                                       (process-name "Journal Inspector"))
  "Launch the Journal Inspector window.
   JOURNAL-DATA: optional pre-loaded journal events list
   WORKER-JOURNAL: optional journal instance (defaults to *worker-journal*)"
  (let* ((fm (or frame-manager (find-frame-manager :port (or port (clim:find-port)))))
         (frame (make-application-frame 'journal-inspector
                                        :name "Skyline-Tool"
                                        :pretty-name "Worker Journal"
                                        :frame-manager fm
                                        :journal-data journal-data
                                        :worker-journal worker-journal
                                        :width width
                                        :height height)))
    (clim-sys:make-process (lambda ()
                             (run-frame-top-level frame))
                           :name process-name)))