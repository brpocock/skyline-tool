;;; Skyline-Tool src/gui/gui-threads.lisp
;;; Lisp Threads Inspector for debugging and monitoring thread activity

(in-package :skyline-tool)

(clim:define-command (com-show-lisp-threads-inspector :command-table clim-internals::global-command-table
                                                       :menu t :name t) ()
  "Show Lisp Threads Inspector with table view, sorting, and actions."
  (show-lisp-threads-inspector))

(clim:define-presentation-type lisp-thread ())

(clim:define-command-table lisp-thread-actions-menu
  :inherit-from (clim-internals::global-command-table)
  :menu (("Interrupt Thread" :command com-interrupt-thread)
         ("Destroy Thread" :command com-destroy-thread)))

(clim:define-command-table lisp-threads-inspector-frame
  :inherit-from (lisp-thread-actions-menu clim-internals::global-command-table))

(clim:define-presentation-to-command-translator lisp-thread-to-actions
    (lisp-thread com-interrupt-thread lisp-thread-actions-menu
     :gesture :menu
     :menu t
     :documentation "Thread actions")
    (thread)
  (list thread))

(clim:define-presentation-to-command-translator lisp-thread-select
    (lisp-thread com-select-thread lisp-threads-inspector-frame
     :gesture :select
     :documentation "Select thread")
    (thread)
  (list thread))

(clim:define-command (com-select-thread :command-table lisp-threads-inspector-frame
                                        :menu nil :name t)
    ((thread 'lisp-thread :gesture :select))
  "Select a thread and show its details."
  (setf (lti-selected-thread clim:*application-frame*) thread)
  (format *query-io* "~&Selected: ~a (TID ~d)~%"
          (thread-name thread) (thread-os-tid thread)))

(defun show-lisp-threads-inspector ()
  "Show Lisp Threads Inspector in a CLIM frame with table view, sorting, and actions."
  (let ((frame (clim:make-application-frame 'lisp-threads-inspector-frame)))
    (clim-sys:make-process (lambda () (clim:run-frame-top-level frame))
                           :name "Lisp Threads Inspector")))

(clim:define-application-frame lisp-threads-inspector-frame ()
  ((refresh-interval :initform 1 :accessor lti-refresh-interval)
   (sort-column :initform :tid :accessor lti-sort-column)
   (sort-reverse :initform nil :accessor lti-sort-reverse)
   (selected-thread :initform nil :accessor lti-selected-thread))
  (:panes (thread-table :application :scroll-bars t
                                     :display-function 'display-lisp-threads-table
                                     :height 600 :width 800))
  (:layouts (default (clim:vertically () thread-table)))
  (:menu-bar lisp-threads-menu-bar)
  (:icon (skyline-tool::skyline-tool-icon))
  (:default-initargs
   :pretty-name "Lisp Threads Inspector — Skyline-Tool"
   :application-frame-name "Lisp Threads Inspector"))

(clim:define-command-table lisp-threads-file-menu
  :inherit-from (clim-internals::global-command-table)
  :menu (("Refresh Now" :command com-refresh-threads)
         (nil :divider :line)
         ("Save As Text..." :command com-save-threads-text)
         ("Close" :command com-close-threads-inspector)))

(clim:define-command-table lisp-threads-edit-menu
  :inherit-from (clim-internals::global-command-table)
  :menu (("Copy" :command com-copy)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table lisp-threads-view-menu
  :inherit-from (clim-internals::global-command-table)
  :menu (("Sort by TID" :command com-sort-threads-by-tid)
         ("Sort by Name" :command com-sort-threads-by-name)
         ("Sort by State" :command com-sort-threads-by-state)
         (nil :divider :line)
         ("Reverse Sort" :command com-reverse-threads-sort :toggle t)
         (nil :divider :line)
         ("Auto Refresh (1s)" :command com-set-refresh-1s :toggle t)
         ("Auto Refresh (5s)" :command com-set-refresh-5s :toggle t)
         ("Pause Auto Refresh" :command com-pause-refresh :toggle t)))

(clim:define-command-table lisp-threads-help-menu
  :inherit-from (clim-internals::global-command-table)
  :menu (("How to Use..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table lisp-threads-menu-bar
  :menu (("File" :menu lisp-threads-file-menu)
         ("Edit" :menu lisp-threads-edit-menu)
         ("View" :menu lisp-threads-view-menu)
         ("Help" :menu lisp-threads-help-menu)))

(clim:define-command (com-close-threads-inspector :menu nil :name t) ()
  (clim:frame-exit clim:*application-frame*))

(clim:define-command (com-refresh-threads :menu t :name t) ()
  "Force an immediate refresh of the thread list."
  (clim:redisplay-frame-panes clim:*application-frame* :force-p t))

(clim:define-command (com-save-threads-text :menu t :name t) ()
  "Save the current thread list as a text file."
  (let* ((frame clim:*application-frame*)
         (default-name (format nil "Lisp-Threads-~a.txt"
                               (format-timestring nil (get-universal-time) :format '(:year :month :day :hour :min :sec))))
         (path (prompt-save-pathname default-name :prefs-key :last-export-directory)))
    (when path
      (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
        (let* ((threads (all-threads))
               (sorted (sort-threads threads (lti-sort-column frame) (lti-sort-reverse frame))))
          (let ((id-column-width (loop for thread on threads
                                       maximize (length (princ-to-string (thread-os-tid thread)))))
                (name-column-width (loop for thread on threads
                                         maximize (length (thread-name thread))))
                (state-column-width (loop for thread on threads
                                          maximize (length (thread-state-string thread)))))
            (format out "~&~va | ~va | ~va"
                    id-column-width "Thread ID"
                    name-column-width "Name"
                    state-column-width "State")
            (dolist (thread sorted)
              (let ((tid (thread-os-tid thread))
                    (name (thread-name thread))
                    (state (thread-state-string thread)))
                (format out "~%~va | ~va | ~va"
                        id-column-width tid
                        name-column-width name
                        state-column-width state)))))
        (fresh-line out)))))

(clim:define-command (com-sort-threads-by-tid :menu t :name t) ()
  "Sort thread list by TID."
  (let ((frame clim:*application-frame*))
    (if (eq (lti-sort-column frame) :tid)
        (setf (lti-sort-reverse frame) (not (lti-sort-reverse frame)))
        (progn
          (setf (lti-sort-column frame) :tid
                (lti-sort-reverse frame) nil)))
    (clim:redisplay-frame-panes frame :force-p t)))

(clim:define-command (com-sort-threads-by-name :menu t :name t) ()
  "Sort thread list by Name."
  (let ((frame clim:*application-frame*))
    (if (eq (lti-sort-column frame) :name)
        (setf (lti-sort-reverse frame) (not (lti-sort-reverse frame)))
        (progn
          (setf (lti-sort-column frame) :name
                (lti-sort-reverse frame) nil)))
    (clim:redisplay-frame-panes frame :force-p t)))

(clim:define-command (com-sort-threads-by-state :menu t :name t) ()
  "Sort thread list by State."
  (let ((frame clim:*application-frame*))
    (if (eq (lti-sort-column frame) :state)
        (setf (lti-sort-reverse frame) (not (lti-sort-reverse frame)))
        (progn
          (setf (lti-sort-column frame) :state
                (lti-sort-reverse frame) nil)))
    (clim:redisplay-frame-panes frame :force-p t)))

(clim:define-command (com-reverse-threads-sort :menu t :name t) ()
  "Reverse the current sort order."
  (let ((frame clim:*application-frame*))
    (setf (lti-sort-reverse frame) (not (lti-sort-reverse frame)))
    (clim:redisplay-frame-panes frame :force-p t)))

(clim:define-command (com-set-refresh-1s :menu t :name t) ()
  "Set auto-refresh interval to 1 second."
  (setf (lti-refresh-interval clim:*application-frame*) 1)
  (format *query-io* "~&Auto-refresh set to 1 second~%"))

(clim:define-command (com-set-refresh-5s :menu t :name t) ()
  "Set auto-refresh interval to 5 seconds."
  (setf (lti-refresh-interval clim:*application-frame*) 5)
  (format *query-io* "~&Auto-refresh set to 5 seconds~%"))

(clim:define-command (com-pause-refresh :menu t :name t) ()
  "Pause auto-refresh."
  (setf (lti-refresh-interval clim:*application-frame*) nil)
  (format *query-io* "~&Auto-refresh paused~%"))

(clim:define-command (com-interrupt-thread :command-table lisp-thread-actions-menu
                                           :menu t :name t)
    ((thread 'lisp-thread :gesture :menu))
  "Interrupt the selected thread."
  (bt:interrupt-thread thread (lambda () (break "Interrupted by user")))
  (clim:redisplay-frame-panes clim:*application-frame* :force-p t))

(clim:define-command (com-destroy-thread :command-table lisp-thread-actions-menu
                                         :menu t :name t)
    ((thread 'lisp-thread :gesture :menu))
  "Destroy the selected thread."
  (bt:destroy-thread thread)
  (clim:redisplay-frame-panes clim:*application-frame* :force-p t))

(defun thread-state-string (thread)
  "Return the state of THREAD as a string."
  (if (thread-alive-p thread)
      "Alive"
      "Dead"))

(defun sort-threads (threads sort-column sort-reverse)
  "Sort THREADS by SORT-COLUMN (:tid, :name, or :state)."
  (let ((sorted (sort (copy-list threads)
                      (case sort-column
                        (:tid (lambda (a b) (< (thread-os-tid a) (thread-os-tid b))))
                        (:name (lambda (a b) (string-lessp (thread-name a) (thread-name b))))
                        (:state (lambda (a b) (string-lessp (thread-state-string a) (thread-state-string b))))))))
    (if sort-reverse (nreverse sorted) sorted)))

(defun sort-indicator (frame column)
  "Return a sort arrow string if COLUMN is the active sort column."
  (if (eq (lti-sort-column frame) column)
      (if (lti-sort-reverse frame) " ^" " v")
      ""))

(defun display-lisp-threads-table (frame pane)
  "Display the Lisp threads table with columns: TID, Name, State."
  (clim:window-clear pane)
  (let* ((threads (all-threads))
         (sorted (sort-threads threads (lti-sort-column frame) (lti-sort-reverse frame))))
    ;; Header row
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane)
          (clim:with-text-face (pane :bold)
            (format pane "Thread ID~a" (sort-indicator frame :tid))))
        (clim:formatting-cell (pane)
          (clim:with-text-face (pane :bold)
            (format pane "Name~a" (sort-indicator frame :name))))
        (clim:formatting-cell (pane)
          (clim:with-text-face (pane :bold)
            (format pane "State~a" (sort-indicator frame :state)))))
      (dolist (thread sorted)
        (let* ((tid (thread-os-tid thread))
               (name (thread-name thread))
               (state (thread-state-string thread)))
          ;; Present thread as clickable for context menu
          (clim:formatting-row (pane)
            (clim:formatting-cell (pane)
              (clim:with-output-as-presentation (pane thread 'lisp-thread)
                (format pane "~10d" tid)))
            (clim:formatting-cell (pane)
              (clim:with-output-as-presentation (pane thread 'lisp-thread)
                (format pane "~a" name)))
            (clim:formatting-cell (pane)
              (clim:with-output-as-presentation (pane thread 'lisp-thread)
                (format pane "~a" state))))))))

  ;; Auto-refresh if interval is set
  (let ((interval (lti-refresh-interval frame)))
    (when interval
      (clim-sys:make-process
       (lambda ()
         (sleep interval)
         (ignore-errors
          (clim:redisplay-frame-panes clim:*application-frame* :force-p t)))
       :name "Lisp Threads Auto-Refresh"))))
