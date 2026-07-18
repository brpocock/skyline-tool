;;; Skyline-Tool src/gui/gui-threads.lisp
;;; Lisp Threads Inspector for debugging and monitoring thread activity

(in-package :skyline-tool)

;; =============================
;; DISPLAY FUNCTIONS
;; =============================

(defun display-lisp-threads-table (frame pane)
  "Display a table of active Lisp threads"
  (declare (ignore frame))
  (let ((threads (ignore-errors (bt:all-threads))))
    (clim:stream-set-cursor-position pane 0 0)
    (format pane "=== LISP THREADS INSPECTOR ===")
    (clim:stream-set-cursor-position pane 0 2)
    (format pane "~8a | ~30a | ~10a | ~10a | ~8a"
            "TID" "Name" "State" "Start Time" "Alive?")
    (clim:stream-set-cursor-position pane 0 3)
    (format pane "---------------------------------------------"
            "--------------------------------------------------------------------"
            "------------"
            "------------"
            "--------")
    (loop for thread in threads
          for y from 4
          for tid = (bt:thread-id thread)
          for name = (bt:thread-name thread)
          for state = (bt:thread-state thread)
          for start-time = (format-timestring nil (bt:thread-start-time thread))
          for alive = (bt:thread-alive-p thread)
          do (clim:stream-set-cursor-position pane 0 y)
             (format pane "~8a | ~30a | ~10a | ~10a | ~8a"
                     tid name state start-time alive))))

;; =============================
;; MENU DEFINITIONS
;; =============================

(clim:define-command-table lisp-threads-file-menu
  :menu (("Close" :command com-close-threads-inspector)))

(clim:define-command-table lisp-threads-edit-menu
  :menu (("Kill Selected Thread" :command com-kill-selected-thread)
         (nil :divider :line)
         ("Kill All Worker Threads" :command com-kill-all-workers)))

(clim:define-command-table lisp-threads-view-menu
  :menu (("[] Auto Refresh" :command com-toggle-auto-refresh :toggle t)
         (nil :divider :line)
         ("Sort by TID" :command com-sort-by-tid)
         ("Sort by Name" :command com-sort-by-name)
         ("Sort by State" :command com-sort-by-state)))

(clim:define-command-table lisp-threads-help-menu
  :menu (("How to Use Threads Inspector..." :command com-help-threads)
         (nil :divider :line)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table lisp-threads-menu-bar
  :menu (("Threads" :menu lisp-threads-file-menu)
         ("Edit" :menu lisp-threads-edit-menu)
         ("View" :menu lisp-threads-view-menu)
         ("Help" :menu lisp-threads-help-menu)))

;; =============================
;; COMMANDS
;; =============================

(clim:define-command (com-close-threads-inspector :command-table clim-internals::global-command-table :menu t :name t)
  ()
  ()
  "Close the Lisp Threads Inspector"
  (clim:frame-exit clim:*application-frame*))

(clim:define-command (com-kill-selected-thread :command-table clim-internals::global-command-table :menu t :name t)
  ()
  ()
  "Kill the selected thread"
  (let ((frame clim:*application-frame*))
    (when frame
      (let ((thread (lti-selected-thread frame)))
        (when thread
          (ignore-errors (bt:destroy-thread thread))
          (clim:redisplay-frame-panes frame :force-p t))))))

(clim:define-command (com-kill-all-workers :command-table clim-internals::global-command-table :menu t :name t)
  ()
  ()
  "Kill all worker threads (non-system threads)"
  (let ((threads (ignore-errors (bt:all-threads))))
    (dolist (thread threads)
      (when (and (not (equal (bt:thread-name thread) "main"))
                 (not (equal (bt:thread-name thread) "Avahi Poll Loop")))
        (ignore-errors (bt:destroy-thread thread))))
    (format *query-io* "~&Killed all worker threads~%")))

(clim:define-command (com-toggle-auto-refresh :command-table clim-internals::global-command-table :menu t :name t)
  ()
  ()
  "Toggle auto-refresh on/off"
  (let ((frame clim:*application-frame*))
    (when frame
      (let ((interval (lti-refresh-interval frame)))
        (setf (lti-refresh-interval frame) (if (= interval 0) 1 0))
        (format *query-io* "~&Auto-refresh ~a~%"
                (if (= (lti-refresh-interval frame) 0) "disabled" "enabled"))))))

(clim:define-command (com-sort-by-tid :command-table clim-internals::global-command-table :menu t :name t)
  ()
  ()
  "Sort threads by Thread ID"
  (setf (lti-sort-column frame) :tid)
  (clim:redisplay-frame-panes frame :force-p t))

(clim:define-command (com-sort-by-name :command-table clim-internals::global-command-table :menu t :name t)
  ()
  ()
  "Sort threads by name"
  (setf (lti-sort-column frame) :name)
  (clim:redisplay-frame-panes frame :force-p t))

(clim:define-command (com-sort-by-state :command-table clim-internals::global-command-table :menu t :name t)
  ()
  ()
  "Sort threads by state"
  (setf (lti-sort-column frame) :state)
  (clim:redisplay-frame-panes frame :force-p t))

(clim:define-command (com-help-threads :command-table clim-internals::global-command-table :menu t :name t)
  ()
  ()
  "Show help for Threads Inspector")

;; =============================
;; FRAME DEFINITION
;; =============================

(clim:define-application-frame lisp-threads-inspector-frame ()
  ((refresh-interval :initform 1 :accessor lti-refresh-interval)
   (sort-column :initform :tid :accessor lti-sort-column)
   (sort-reverse :initform nil :accessor lti-sort-reverse)
   (selected-thread :initform nil :accessor lti-selected-thread)
   (refresh-timer :initform nil :accessor frame-refresh-timer))
  (:panes
   (thread-table :application :scroll-bars t
                 :display-function 'display-lisp-threads-table
                 :height 600 :width 800)
   (status-bar :application
               :display-function 'display-threads-status
               :height 30 :width 800))
  (:layouts
   (default (clim:vertically () thread-table status-bar)))
  (:menu-bar lisp-threads-menu-bar)
  (:icon (skyline-tool-icon :resource :inspector))
  (:pretty-name "Lisp Threads Inspector — Skyline-Tool")
  (:default-initargs
   :application-frame-name "Lisp Threads Inspector"))

;; =============================
;; STATUS DISPLAY
;; =============================

(defun display-threads-status (frame pane)
  "Display status information in the status bar"
  (let ((thread-count (length (ignore-errors (bt:all-threads)))))
    (let ((*standard-output* pane))
      (format pane "Threads: ~a | Auto-refresh: ~as | Click thread to select"
              thread-count
              (lti-refresh-interval frame)))))

;; =============================
;; FRAME LIFECYCLE
;; =============================

(defmethod initialize-instance :after ((frame lisp-threads-inspector-frame) &key)
  "Start the refresh timer"
  (setf (frame-refresh-timer frame)
        (mp:make-timer (lambda ()
                         (when (and (clim:framep frame)
                                    (not (zerop (lti-refresh-interval frame))))
                           (clim:redisplay-frame-panes frame :force-p t))
                         (mp:schedule-timer-relative (frame-refresh-timer frame)
                                                     (lti-refresh-interval frame)))
                     frame)))

(defmethod finalize-instance :after ((frame lisp-threads-inspector-frame) &key)
  "Stop the refresh timer"
  (when (frame-refresh-timer frame)
    (mp:unschedule-timer (frame-refresh-timer frame))))

;; =============================
;; OPEN HELPER
;; =============================

(defun open-threads-inspector ()
  "Open the Lisp Threads Inspector window"
  (clim:make-application-frame 'lisp-threads-inspector-frame
                               :pretty-name "Lisp Threads Inspector — Skyline-Tool")
  (clim:run-frame-top-level *))