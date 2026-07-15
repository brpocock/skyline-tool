(in-package :skyline-tool)

(defvar *machine-memory* nil
  "Memory array for loading core dumps (e.g., fault.core).")

(defvar *machine-memory* nil
  "Memory array for core dump analysis.")

;; Load a core dump into a memory array for analysis
(defun load-dump-into-mem (&optional (dump-file #p"/tmp/dump"))
  (let ((mem (make-array (expt 2 16) :element-type '(unsigned-byte 8))))
    (with-input-from-file (dump dump-file :element-type '(unsigned-byte 8))
      (loop for byte = (read-byte dump nil nil)
            for i from 0 below #x10000
            while byte
            do (setf (aref mem i) byte)))
    mem))

;;  Event Types for Resource Scavenger Events 

(define-constant +resource-event-types+
    '(:resource-added :resource-changed :resource-removed :resource-cache-dump :resource-scan-complete)
  :test 'equalp
  :documentation "Event types published by the resource scavenger system.")

;;  Global Resource Cache 

(defvar *all-resources-cache* (make-hash-table :test 'equal)
  "Global hash table mapping resource kind keywords to lists of game-resource objects.
Keys are kind keywords (:map, :script, :song, etc.), values are lists of game-resource instances.")

(defvar *all-resources-cache-lock* (bt:make-lock "all-resources-cache-lock")
  "Lock for thread-safe access to *all-resources-cache*.")

(defvar *resource-scavenger-thread* nil
  "The background scavenger thread that monitors resource directories.")

(defvar *resource-scavenger-running-p* nil
  "Flag indicating whether the scavenger thread is running.")

(defvar *resource-filesystem-monitors* (make-hash-table :test 'equal)
  "Hash table mapping directory paths to inotify monitor objects.")

(defvar *resource-scavenger-initialized-p* nil
  "Flag indicating whether the scavenger system has been initialized.")

;;  Cache Manipulation Functions 

(defun cache-get-resources (kind)
  "Get all resources of KIND from the global cache. Returns a list of game-resource objects."
  (bt:with-lock-held (*all-resources-cache-lock*)
    (gethash kind *all-resources-cache* nil)))

(defun cache-set-resources (kind resources)
  "Set the resources for KIND in the global cache."
  (bt:with-lock-held (*all-resources-cache-lock*)
    (setf (gethash kind *all-resources-cache*) resources)))

(defun cache-add-resource (kind resource)
  "Add a single RESOURCE to the cache for KIND, skipping duplicates.
Duplicates are identified by the cons pair (type-of resource) and locator."
  (bt:with-lock-held (*all-resources-cache-lock*)
    (let* ((dup-key (cons (type-of resource) (game-resource-locator resource)))
           (current (gethash kind *all-resources-cache* nil)))
      (unless (find dup-key current :key (lambda (r)
                                           (cons (type-of r)
                                                 (game-resource-locator r)))
                           :test #'equalp)
        (setf (gethash kind *all-resources-cache*)
              (cons resource current))))))

(defun cache-remove-resource (kind locator)
  "Remove a resource with LOCATOR from the cache for KIND."
  (bt:with-lock-held (*all-resources-cache-lock*)
    (let ((current (gethash kind *all-resources-cache* nil)))
      (when current
        (setf (gethash kind *all-resources-cache*)
              (remove locator current :key #'game-resource-locator :test #'string-equal))))))

(defun cache-find-resource (kind locator)
  "Find a resource with LOCATOR in the cache for KIND."
  (bt:with-lock-held (*all-resources-cache-lock*)
    (let ((current (gethash kind *all-resources-cache* nil)))
      (find locator current :key #'game-resource-locator :test #'string-equal))))

(defun cache-clear-kind (kind)
  "Clear all resources for KIND from the cache."
  (bt:with-lock-held (*all-resources-cache-lock*)
    (remhash kind *all-resources-cache*)))

(defun cache-get-all-kinds ()
  "Get all kinds currently in the cache."
  (bt:with-lock-held (*all-resources-cache-lock*)
    (loop for k being the hash-keys of *all-resources-cache* collect k)))

(defun cache-dump ()
  "Return a plist of all resources in the cache for dumping to subscribers."
  (bt:with-lock-held (*all-resources-cache-lock*)
    (loop for k being the hash-keys of *all-resources-cache*
          collect (cons k (gethash k *all-resources-cache*)))))

;;  Event Publishing Functions 

(defun publish-resource-added (resource)
  "Publish :resource-added event for RESOURCE."
  (publish :resource-added
           :payload (list :resource resource
                          :kind (game-resource-kind resource)
                          :locator (game-resource-locator resource))))

(defun publish-resource-changed (resource)
  "Publish :resource-changed event for RESOURCE."
  (publish :resource-changed
           :payload (list :resource resource
                          :kind (game-resource-kind resource)
                          :locator (game-resource-locator resource))))

(defun publish-resource-removed (kind moniker)
  "Publish :resource-removed event for a resource identified by KIND and MONIKER."
  (publish :resource-removed
           :payload (list :kind kind :moniker moniker)))

(defun publish-cache-dump ()
  "Publish :resource-cache-dump event with full cache contents."
  (publish :resource-cache-dump
           :payload (list :cache (cache-dump))))

(defun publish-scan-complete (kind count)
  "Publish :resource-scan-complete event for KIND with COUNT resources found."
  (publish :resource-scan-complete
           :payload (list :kind kind :count count)))

(clim:define-application-frame all-resources-frame (clim:standard-application-frame)
  ((scavenger-thread :initform nil :accessor frame-scavenger-thread))
  (:panes
   (main-pane :application :height 800 :width 400
                           :display-function 'display-all-resources
                           :scroll-bars :vertical)
   (find-pane :application :height 30 :width 400
                           :display-function 'display-resource-filter
                           :scroll-bars :vertical)
   (project-pane :application :height 30 :width 400
                              :display-function 'display-versioning-and-issues
                              :scroll-bars :vertical))
  
  (:command-table (all-resources-frame))
  (:menu-bar resource-menu-bar)
  (:icon (skyline-tool-icon))
  (:layouts (default main-pane)
            (search (clim:vertically () main-pane find-pane))
            (project (clim:vertically () main-pane project-pane))
            (project+search (clim:vertically () main-pane find-pane project-pane)))
  (:default-initargs
   :pretty-name (format nil "~a ~a — All Resources — Skyline-Tool"
                        *game-title* (machine-directory-name))
   :application-frame-name (format nil "~a - ~a" *game-title* (machine-directory-name))))


(clim:define-command-table all-resources-frame
  :inherit-from (clim-internals::global-command-table))

;;  All Resources Scavenger 
;; Background thread that monitors resource directories for changes.
;; The canonical start-all-resources-scavengers is in scavengers.lisp.

(defmethod initialize-instance :after ((frame all-resources-frame) &key)
  (start-all-resources-scavengers frame)
  (start-project-scavenger frame)
  (initialize-run-menus))

(defmethod finalize-instance :after ((frame all-resources-frame))
  (when (frame-scavenger-thread frame)
    (bt:destroy-thread (frame-scavenger-thread frame))))

;;  New resource commands (using frame-specific macro) 

(define-all-resources-frame-command (com-new-blob :menu nil :name t) ()
  (open-blob-inspector nil))

(define-all-resources-frame-command (com-new-character :menu nil :name t) ()
  (open-character-inspector nil))

(define-all-resources-frame-command (com-new-map :menu nil :name t) ()
  (open-map-inspector nil))

(define-all-resources-frame-command (com-new-object-prototype :menu nil :name t) ()
  (open-object-prototype-inspector nil))

(define-all-resources-frame-command (com-new-script-from-menu :menu nil :name t) ()
  (open-script-inspector nil))

(define-all-resources-frame-command (com-new-song :menu nil :name t) ()
  (open-song-inspector nil))

(define-all-resources-frame-command (com-new-sprite-sheet :menu nil :name t) ()
  (open-sprite-sheet-inspector))

(define-all-resources-frame-command (com-new-tileset :menu nil :name t) ()
  (open-tileset-inspector nil))

(define-all-resources-frame-command (com-new-boat :menu nil :name t) ()
  (open-boat-inspector nil))

(define-all-resources-frame-command (com-new-class :menu nil :name t) ()
  (open-class-inspector nil))

(define-all-resources-frame-command (com-new-flag :menu nil :name t) ()
  (open-flag-inspector nil))

(define-all-resources-frame-command (com-new-item :menu nil :name t) ()
  (open-item-inspector nil))

(define-all-resources-frame-command (com-new-key :menu nil :name t) ()
  (open-key-inspector nil))

;; FIXME: Instrument and Routine use $f template scaffolding in gui-*.lisp
(define-all-resources-frame-command (com-new-instrument :menu nil :name t) ()
  (open-resource-inspector (make-instance 'game-resource-instrument) :editing))

(define-all-resources-frame-command (com-new-routine :menu nil :name t) ()
  (open-resource-inspector (make-instance 'game-resource-routine) :editing))

(define-all-resources-frame-command (com-new-routine-forth-library :menu nil :name t) ()
  (open-resource-inspector (make-instance 'game-resource-routine-forth-library) :editing))

(define-all-resources-frame-command (com-new-routine-bas :menu nil :name t) ()
  (open-resource-inspector (make-instance 'game-resource-routine-rc-basic) :editing))

(define-all-resources-frame-command (com-new-routine-cob :menu nil :name t) ()
  (open-resource-inspector (make-instance 'game-resource-routine-rc-cobol) :editing))

(define-all-resources-frame-command (com-new-routine-pas :menu nil :name t) ()
  (open-resource-inspector (make-instance 'game-resource-routine-rc-pascal) :editing))

(define-all-resources-frame-command (com-new-phrasebook :menu nil :name t) ()
  (open-resource-inspector (make-instance 'game-resource-phrasebook) :editing))

(define-all-resources-frame-command (com-new-atari-vox-dictionary :menu nil :name t) ()
  (open-resource-inspector (make-instance 'game-resource-atari-vox-dictionary) :editing))

(define-all-resources-frame-command (com-new-intellivoice-dictionary :menu nil :name t) ()
  (open-resource-inspector (make-instance 'game-resource-intellivoice-dictionary) :editing))

(define-all-resources-frame-command (com-new-magic-desk-dictionary :menu nil :name t) ()
  (open-resource-inspector (make-instance 'game-resource-magic-desk-dictionary) :editing))

(define-all-resources-frame-command (com-import-resource-json :menu nil :name t) ()
  (let* ((pathname (clim:accept 'pathname :stream *query-io* :prompt "JSON file to import:"))
         (class-name (clim:accept 'symbol :stream *query-io* :prompt "Resource class:" :default 'game-resource))
         (resource (import-resource-from-json-file pathname class-name)))
    (when resource
      (publish-resource-added resource)
      (format t "~&Imported ~a from ~a~%" (game-resource-title resource) pathname))))

;;  Stub commands for menu items not yet implemented 

(clim:define-command (com-build-release-package :command-table clim-internals::global-command-table
                                                :menu t :name t) ()
  (error "Build a Release Package is not implemented yet.

Build for:
     <o> Demo
     <> Public
     <> $(Publisher)

Last Release: v XX.XX  ( Read Notes ) ( Read Git Change Log )
Release number: v ___.___                [ starts at last release major + (1+ last release minor)

[x] Run all tests first

 <o>Dry Run (do not publish)
 <> Publish after building 

Release notes:   ( Edit in Emacs )
___________________________
___________________________
___________________________
___________________________
___________________________
___________________________

                ( Cancel ) ( Build )
"))

(clim:define-command (com-load-last-minor-fault :command-table clim-internals::global-command-table
                                                 :menu t :name t) ()
  "Load the last minor fault core dump into memory."
  (let ((mem (load-dump-into-mem (merge-pathnames "fault.core" (uiop:getcwd)))))
    (setf *machine-memory* mem)
    (format t "~&Loaded fault.core into memory.~%")))

(clim:define-command (com-load-last-break :command-table clim-internals::global-command-table
                                          :menu t :name t) ()
  (error "~&Load Last Break: not yet available.~%"))

(clim:define-command (com-load-last-failed-test :command-table clim-internals::global-command-table
                                                :menu t :name t) ()
  (error "~&Load Last Failed Test: not yet available.~%"))

;;  New Project commands 

(clim:define-command (com-new-project :command-table clim-internals::global-command-table
                                      :menu t :name t) ()
  (open-project-inspector :project nil))

(clim:define-command (com-new-project-url :command-table clim-internals::global-command-table
                                          :menu t :name t) ()
  (error "~&Project from URL: not yet available.~%"))

(clim:define-command (com-new-project-port :command-table clim-internals::global-command-table
                                           :menu t :name t) ()
  (error "~&Port to Machine: not yet available.~%"))

;;  Save List As commands for all-resources 

(clim:define-command (com-save-as-json :command-table clim-internals::global-command-table
                                       :menu t :name t) ()
  (error "~&Save List As JSON: not yet available.~%"))

(clim:define-command (com-save-text :command-table clim-internals::global-command-table
                                    :menu t :name t) ()
  (error "~&Save List As Text: not yet available.~%"))

(clim:define-command (com-save-spreadsheet :command-table clim-internals::global-command-table
                                           :menu t :name t) ()
  (error "~&Save List As Spreadsheet: not yet available.~%"))

(clim:define-command (com-print-pdf :command-table clim-internals::global-command-table
                                    :menu t :name t) ()
  (error "~&Print List as PDF: not yet available.~%"))

(clim:define-command (com-run-emacs :command-table clim-internals::global-command-table
                                    :menu t :name t) ()
  (uiop:run-program "emacs"))

(clim:define-command (com-run-gimp :command-table clim-internals::global-command-table
                                   :menu t :name t) ()
  (uiop:run-program "gimp"))

(clim:define-command (com-run-libre-calc :command-table clim-internals::global-command-table
                                         :menu t :name t) ()
  (uiop:run-program '("libreoffice" "-calc")))

(clim:define-command (com-run-muse-score :command-table clim-internals::global-command-table
                                         :menu t :name t) ()
  (uiop:run-program "mscore"))

(clim:define-command (com-run-thief-md :command-table clim-internals::global-command-table
                                       :menu t :name t) ()
  (uiop:run-program '("flatpak" "run" "com.github.kmwallio.thiefmd")))

(clim:define-command (com-resource-copy-list :command-table clim-internals::global-command-table
                                             :menu t :name t) ()
  (error "~&Copy List: not yet available.~%"))

(clim:define-command (com-paste-as-resource :command-table clim-internals::global-command-table
                                            :menu t :name t) ()
  (error "~&Paste JSON as Resource: not yet available.~%"))

(clim:define-command (com-preferences-inspector :command-table clim-internals::global-command-table
                                                :menu t :name t) ()
  (open-preferences-inspector))

(clim:define-command (com-project-inspector :command-table clim-internals::global-command-table
                                            :menu t :name t) ()
  (open-project-inspector))

(defvar *all-resources-sort-mode* :alpha
  "Sort mode for All Resources: :alpha (alphabetical) or :numeric (by asset ID).")

(clim:define-command (com-set-sort-alpha :command-table clim-internals::global-command-table
                                         :menu t :name t) ()
  "Sort All Resources alphabetically by moniker within each kind."
  (setf *all-resources-sort-mode* :alpha)
  (when (boundp 'clim:*application-frame*)
    (clim:redisplay-frame-panes clim:*application-frame* :force-p t)))

(clim:define-command (com-set-sort-numeric :command-table clim-internals::global-command-table
                                           :menu t :name t) ()
  "Sort All Resources numerically by asset ID within each kind."
  (setf *all-resources-sort-mode* :numeric)
  (when (boundp 'clim:*application-frame*)
    (clim:redisplay-frame-panes clim:*application-frame* :force-p t)))

(clim:define-command (com-resource-close-all :command-table clim-internals::global-command-table
                                             :menu t :name t) ()
  "Collapse all resource groups in All Resources."
  (dolist (k '("Scripts" "Songs" "Maps" "Characters" "Boats" "Blobs"
               "Tilesets" "Sprite Sheets" "Object Prototypes" "Classes" "Routines"
               "Instruments" "Items" "Flags" "Keys" "AtariVox Dictionary"))
    (setf (gethash k *all-resources-collapsed*) t))
  (set-pref :all-resources-collapsed
            (loop for k being the hash-keys of *all-resources-collapsed*
                  collect (cons k (if (gethash k *all-resources-collapsed*) "true" "false"))))
  (when (boundp 'clim:*application-frame*)
    (clim:redisplay-frame-panes clim:*application-frame* :force-p t)))

(clim:define-command (com-resource-open-all :command-table clim-internals::global-command-table
                                            :menu t :name t) ()
  "Expand all resource groups in All Resources."
  (dolist (k '("Scripts" "Songs" "Maps" "Characters" "Boats" "Blobs"
               "Tilesets" "Sprite Sheets" "Object Prototypes" "Classes" "Routines"
               "Instruments" "Items" "Flags" "Keys" "AtariVox Dictionary"))
    (setf (gethash k *all-resources-collapsed*) nil))
  (set-pref :all-resources-collapsed
            (loop for k being the hash-keys of *all-resources-collapsed*
                  collect (cons k (if (gethash k *all-resources-collapsed*) "true" "false"))))
  (when (boundp 'clim:*application-frame*)
    (clim:redisplay-frame-panes clim:*application-frame* :force-p t)))

;;  Migrated Launcher commands 

(clim:define-command (com-edit-project.json :command-table clim-internals::global-command-table
                                            :menu t :name t) ()
  "Edit the Project.<machine>.json configuration file in Emacs."
  (open-in-emacs (make-pathname :name (format nil "Project.~a" (machine-directory-name))
                                :type "json")))

(clim:define-command (com-edit-skyline-config-prefs
                      :command-table clim-internals::global-command-table
                      :menu t :name t) ()
  "Edit Skyline-Tool preferences file in Emacs."
  (let* ((prefs-file (prefs-pathname)))
    (open-in-emacs prefs-file)))

(clim:define-command (com-quit-skyline-tool :command-table clim-internals::global-command-table
                                            :menu t :name t) ()
  "Exit Skyline-Tool."
  (if (pending-outbound-offers-p)
      (cerror "Quit Anyway"
              "You still have these offers outbound, if you quit, the offers will be retracted? ~
~{~%~:d offer~:p of ~a~}"
              (mapcar
               (lambda (group)
                 (list (length group) (outbound-offer-kind (first group))))
               (group-by #'outbound-offer-kind (pending-outbound-offers)))))
  (bye))

;;  Migrated Lisp / Tools / Debug commands

(defun %menu-run (name fn)
  "Run FN in a new process named NAME."
  (clim-sys:make-process (lambda () (funcall fn)) :name (string-capitalize name)))

(clim:define-command (com-run-repl :command-table clim-internals::global-command-table
                                   :menu t :name t) ()
  "Open a Lisp REPL."
  (run-repl))

(clim:define-command (com-show-all-resources :command-table clim-internals::global-command-table
                                             :menu t :name t) ()
  "Open the All Resources browser."
  (%menu-run "All Resources" #'show-all-resources))

(clim:define-command (com-show-lisp-room :command-table clim-internals::global-command-table
                                         :menu t :name t) ()
  "Show Lisp memory usage."
  (%menu-run "Lisp Room" #'show-lisp-room))

(clim:define-command (com-show-lisp-threads-inspector :command-table clim-internals::global-command-table
                                                      :menu t :name t) ()
  "Show Lisp Threads Inspector with table view, sorting, and actions."
  (show-lisp-threads-inspector))

(clim:define-presentation-type lisp-thread ())

(clim:define-command-table lisp-thread-actions-menu
  :inherit-from (clim-internals::global-command-table)
  :menu (("Interrupt Thread" :command com-interrupt-thread)
         ("Destroy Thread" :command com-destroy-thread)))

(defun show-lisp-threads-inspector ()
  "Show Lisp Threads Inspector in a CLIM frame with table view, sorting, and actions."
  (let ((frame (clim:make-application-frame 'lisp-threads-inspector-frame)))
    (clim:run-frame-top-level frame)))

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

(clim:define-command-table lisp-threads-view-menu
  :inherit-from (clim-internals::global-command-table)
  :menu (("Sort by TID" :command com-sort-threads-by-tid)
         ("Sort by Name" :command com-sort-threads-by-name)
         ("Sort by State" :command com-sort-threads-by-state)
         (nil :divider :line)
         ("Reverse Sort" :command com-reverse-threads-sort)
         (nil :divider :line)
         ("Auto Refresh (1s)" :command com-set-refresh-1s)
         ("Auto Refresh (5s)" :command com-set-refresh-5s)
         ("Pause Auto Refresh" :command com-pause-refresh)))

(clim:define-command-table lisp-threads-help-menu
  :inherit-from (clim-internals::global-command-table)
  :menu (("How to Use..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table lisp-threads-menu-bar
  :menu (("File" :menu lisp-threads-file-menu)
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
         (path (skyline-tool::prompt-save-pathname default-name :prefs-key :last-export-directory)))
    (when path
      (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
        (let* ((threads (bt:all-threads))
               (sorted (sort-threads threads (lti-sort-column frame) (lti-sort-reverse frame))))
          (format out "~&TID | Thread Name | State~%")
          (format out "----|-------------|------~%")
          (dolist (thread sorted)
            (let ((tid (thread-os-tid thread))
                  (name (bt:thread-name thread))
                  (state (thread-state-string thread)))
              (format out "~a | ~a | ~a~%" tid name state)))))
      (format *query-io* "~&Saved thread list to ~a~%" path))))

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

(clim:define-command (com-interrupt-thread :menu t :name t)
    ((thread 'lisp-thread :gesture :menu))
  "Interrupt the selected thread."
  (bt:interrupt-thread thread)
  (format *query-io* "~&Interrupted thread ~a~%" (bt:thread-name thread))
  (clim:redisplay-frame-panes clim:*application-frame* :force-p t))

(clim:define-command (com-destroy-thread :menu t :name t)
    ((thread 'lisp-thread :gesture :menu))
  "Destroy the selected thread."
  (let ((name (bt:thread-name thread)))
    (bt:destroy-thread thread)
    (format *query-io* "~&Destroyed thread ~a~%" name)
    (clim:redisplay-frame-panes clim:*application-frame* :force-p t)))

(defun thread-os-tid (thread)
  "Return the OS thread ID for THREAD."
  (sb-thread:thread-os-tid thread))

(defun thread-state-string (thread)
  "Return the state of THREAD as a string."
  (if (bt:thread-alive-p thread)
      "Alive"
      "Dead"))

(defun sort-threads (threads sort-column sort-reverse)
  "Sort THREADS by SORT-COLUMN (:tid, :name, or :state)."
  (let ((sorted (sort (copy-list threads)
                      (case sort-column
                        (:tid (lambda (a b) (< (thread-os-tid a) (thread-os-tid b))))
                        (:name (lambda (a b) (string-lessp (bt:thread-name a) (bt:thread-name b))))
                        (:state (lambda (a b) (string-lessp (thread-state-string a) (thread-state-string b))))))))
    (if sort-reverse (nreverse sorted) sorted)))

(defun display-lisp-threads-table (frame pane)
  "Display the Lisp threads table with columns: TID, Name, State."
  (clim:window-clear pane)
  (let* ((threads (bt:all-threads))
         (sorted (sort-threads threads (lti-sort-column frame) (lti-sort-reverse frame))))
    ;; Header row
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane)
          (princ "Thread ID" pane))
        (clim:formatting-cell (pane)
          (princ "Name" pane))
        (clim:formatting-cell (pane)
          (princ "State" pane)))
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

(clim:define-command (com-show-clouseau :command-table clim-internals::global-command-table
                                        :menu t :name t) ()
  "Show Clouseau inspector on Skyline-Tool package."
  (show-clouseau))

(clim:define-command (com-reload-skyline-tool-from-sources :command-table clim-internals::global-command-table
                                                            :menu t :name t) ()
  "Recompile and reload Skyline-Tool from sources."
  (%menu-run "Recompile" #'reload-skyline-tool-from-sources))

(clim:define-command (com-show-journal :command-table clim-internals::global-command-table
                                       :menu t :name t) ()
  "Show the worker journal log."
  (clim-simple-echo:run-in-simple-echo
   (lambda ()
     (if *worker-journal*
         (let ((events (journal:list-events *worker-journal*)))
           (if events
               (journal:pprint-events events :stream *standard-output*)
               (princ "(No journal entries yet.)" *standard-output*)))
         (princ "(Worker journal not initialized.)" *standard-output*)))
   :window-title "Worker Journal"))

(clim:define-command (com-show-rom-budget :command-table clim-internals::global-command-table
                                          :menu t :name t) ()
  "Show ROM budget report."
  (show-rom-budget))

(clim:define-command (com-anim-seq-editor :command-table clim-internals::global-command-table
                                          :menu t :name t) ()
  "Open the animation sequence editor."
  (edit-animation-sequence))

(clim:define-command (com-assign-animation-sequences :command-table clim-internals::global-command-table
                                                     :menu t :name t) ()
  "Open the animation sequence assignment window."
  (%menu-run "Animation Assignments" #'assign-animation-sequences))

(clim:define-command (com-run-tiled :command-table clim-internals::global-command-table
                                    :menu t :name t) ()
  "Open the project in Tiled."
  (run-tiled))

(clim:define-command (com-open-file-manager :command-table clim-internals::global-command-table
                                            :menu t :name t) ()
  "Open the file manager in the project folder."
  (open-file-manager))

(clim:define-command (com-push-binary-to-7800gd :command-table clim-internals::global-command-table
                                                :menu t :name t) ()
  "Push the latest binary to the 7800 Game Drive."
  (%menu-run "Push Binary" #'push-binary-to-7800-game-drive))

(clim:define-command (com-shove-binary-into-7800gd :command-table clim-internals::global-command-table
                                                   :menu t :name t) ()
  "Shove binary into a running 7800 Game Drive."
  (%menu-run "Shove Binary" #'shove-binary-into-running-7800-game-drive))

(clim:define-command (com-show-dll-from-dump :command-table clim-internals::global-command-table
                                             :menu t :name t) ()
  "Show decoded Display List List from core dump."
  (%menu-run "DLL from Dump" #'show-dll-from-dump))

(clim:define-command (com-show-buffer-dll :command-table clim-internals::global-command-table
                                          :menu t :name t) ()
  "Show decoded back buffer DLL from core dump."
  (%menu-run "Buffer DLL" #'show-other-dll-from-dump))

(clim:define-command (com-copy-dump-as-dump2 :command-table clim-internals::global-command-table
                                             :menu t :name t) ()
  "Copy dump to dump2 for comparison."
  (copy-dump-as-dump2))

(clim:define-command (com-compare-dlls :command-table clim-internals::global-command-table
                                       :menu t :name t) ()
  "Compare DLLs from two dumps."
  (compare-dlls-from-dumps))

(clim:define-command (com-show-animation-buffer :command-table clim-internals::global-command-table
                                                 :menu t :name t)
  "Show an animation buffer from a core dump."
  (let ((index (clim:accept 'integer :stream *query-io* :prompt "Animation buffer index:" :default 0)))
    (show-animation-buffer index)))

(clim:define-command (com-show-decal :command-table clim-internals::global-command-table
                                      :menu t :name t)
  "Show a decal from a core dump."
  (let ((index (clim:accept 'integer :stream *query-io* :prompt "Decal index:" :default 0)))
    (show-decal index)))

(clim:define-command (com-analyze-faults :command-table clim-internals::global-command-table
                                         :menu t :name t) ()
  "Analyze fault codes from a core dump."
  (%menu-run "Analyze Faults" #'analyze-faults-from-dump))

(clim:define-command (com-show-dialogue-buffers :command-table clim-internals::global-command-table
                                                :menu t :name t) ()
  "Show dialogue buffers from a core dump."
  (%menu-run "Dialogue Buffers" #'show-dialogue-buffers))

(clim:define-command (com-show-map-from-dump :command-table clim-internals::global-command-table
                                             :menu t :name t) ()
  "Show map from a core dump."
  (%menu-run "Map from Dump" #'show-map))

(clim:define-command (com-show-sound-system-info :command-table clim-internals::global-command-table
                                                 :menu t :name t) ()
  "Show sound system info from a core dump."
  (%menu-run "Sound System Info" #'show-sound-system-info))

(clim:define-command (com-show-all-stacks :command-table clim-internals::global-command-table
                                          :menu t :name t) ()
  "Show all stacks from a core dump."
  (%menu-run "All Stacks" #'show-all-stacks))

(clim:define-command (com-show-forth-stack :command-table clim-internals::global-command-table
                                           :menu t :name t) ()
  "Show Forth stack from a core dump."
  (%menu-run "Forth Stack" #'show-forth-stack))

(clim:define-command (com-show-player-object :command-table clim-internals::global-command-table
                                             :menu t :name t) ()
  "Show player object from a core dump."
  (%menu-run "Player Object" #'show-player-object))

(clim:define-command (com-show-self-object :command-table clim-internals::global-command-table
                                           :menu t :name t) ()
  "Show self object from a core dump."
  (%menu-run "Self Object" #'show-self-object))

(clim:define-command (com-show-all-objects :command-table clim-internals::global-command-table
                                           :menu t :name t) ()
  "Show all objects from a core dump."
  (%menu-run "All Objects" #'show-all-objects))

(clim:define-command (com-show-room-for-objects :command-table clim-internals::global-command-table
                                                :menu t :name t) ()
  "Show room for objects from a core dump."
  (%menu-run "Room for Objects" #'show-room-for-objects))

;; --- Asset path resolution ---

(defun asset-index->filesystem-path (&rest _)
  "Obsolete function. Use GAME-RESOURCE-PATHNAMES instead.
   Returns the first filesystem path for a MONIKER, or NIL if not found."
  (declare (ignore _))
  (error "Asset-index->filesystem-path is obsolete. Use (GAME-RESOURCE-PATHNAMES MONIKER) instead."))

;; --- Additional commands for Resource menu ---

(clim:define-command (com-find-in-assets :command-table clim-internals::global-command-table
                                          :menu t :name t)
  "Prompt for a filter string and apply it to the All Resources display."
  (let* ((filter (clim:accept 'string :stream *query-io* :prompt "Filter assets (substring match):" :default *assets-index-filter*)))
    (setf *assets-index-filter* filter)
    (when (boundp 'clim:*application-frame*)
      (clim:redisplay-frame-panes clim:*application-frame* :force-p t))))

;; --- All Resources Frame ---

(clim:define-presentation-type build-checkbox ())

(clim:define-presentation-type unified-asset-entry ())

(clim:define-presentation-type assets-section-header () :inherit-from 'string)

(defun current-uid ()
  "Return the current Unix user ID as an integer."
  #+sbcl (sb-posix:getuid)
  #-sbcl (parse-integer (string-trim '(#\Newline)
                                     (uiop:run-program '("id" "-u") :output :string))))

(defun emacs-server-file ()
  "Return the pathname of the Emacs server auth file, or NIL."
  (or (let ((env (uiop:getenv "EMACS_SERVER_FILE")))
        (when env (probe-file env)))
      (probe-file (merge-pathnames ".emacs.d/server/server" (user-homedir-pathname)))
      (let ((uid (current-uid)))
        (or (probe-file (format nil "/run/user/~d/emacs/server" uid))
            (probe-file (format nil "/tmp/emacs~d/server" uid))
            (probe-file "/tmp/emacs/server")))))

(defun emacs-server-pid ()
  "Return the Emacs server PID from the server auth file, or NIL."
  (let ((file (emacs-server-file)))
    (when file
      (with-open-file (s file)
        (let ((line (read-line s nil nil)))
          (and line (parse-integer line :junk-allowed t)))))))

(defun emacs-server-running-p ()
  "Return T if the Emacs server process appears to be alive.
First tries emacsclient with --eval to check server; falls back to PID check."
  (or
   ;; Quick check: emacsclient with --eval returns 0 if server responds
   (ignore-errors
    (uiop:run-program (list "emacsclient" "--eval" "(+ 1 1)") 
                      :output nil :error-output nil ))
   ;; Fallback: check PID via server file
   (let ((pid (emacs-server-pid)))
     (and pid
          (let ((cmdline (format nil "/proc/~d/cmdline" pid)))
            (and (probe-file cmdline)
                 (search "emacs" (uiop:read-file-string cmdline))))))))

(defun open-in-emacs (path)
  "Open PATH in Emacs using emacsclient. If server is not running, prompt user to start Emacs with server-start."
  (let ((file (namestring path)))
    (if (emacs-server-running-p)
        (uiop:run-program (list "emacsclient" "-n" file)
                          :output nil
                          :error-output nil)
        ;; Prompt user to start Emacs with server-start
        (let ((confirm
                (error "FIXME: Need to start Emacs for me so I can retry")))
          (when confirm
            (uiop:run-program (list "bash" "-c"
                                    (format nil "emacs ~s --eval '(server-start)' > /dev/null 2>&1 & disown" file))
                              :output nil
                              :error-output nil
                              )
            ;; Wait briefly for server to start
            (sleep 2)
            (if (emacs-server-running-p)
                (uiop:run-program (list "emacsclient" "-n" file)
                                  :output nil
                                  :error-output nil)
                (error "~&Emacs server failed to start. Please start Emacs manually with 'server-start'.~%")))))))

(defun reload-assets-index ()
  "Force a reload of Source/Assets.index and the maps index table."
  (setf *assets-list* nil
        *asset-ids-seen* nil
        *maps-ids* nil)
  (read-assets-list)
  (error "~&Assets index reloaded.~%"))

(defun collect-all-resources ()
  "Return  a  list of  game-resource  objects  combining all  entries  from
Assets.index with filesystem assets not yet indexed."
  (ensure-scavenger-thread-running)
  (flatten (hash-table-values *all-resources-cache*)))

(defun color-rgb-for-kind (kind)
  "Return PostScript setrgbcolor values for a KIND-NAME background.
   NIL or \"Special Resources\" gets white (no badge)."
  (ecase (kind)
    ((null) "1.0 1.0 1.0 setrgbcolor")
    (:Script "0.0 0.0 0.502 setrgbcolor")
    (:Song "0.502 0.0 0.0 setrgbcolor")
    (:Map "0.302 0.149 0.0 setrgbcolor")
    (:Blob "0.0 0.302 0.0 setrgbcolor")
    (:Boat "0.0 0.2 0.5 setrgbcolor")
    (:Character "0.502 0.0 0.502 setrgbcolor")
    (:Tileset "0.8 0.4 0.0 setrgbcolor")
    (:Sprite-Sheet "0.0 0.4 0.4 setrgbcolor")
    (:Object-Prototype "0.4 0.0 0.6 setrgbcolor")
    (:Class "0.2 0.3 0.6 setrgbcolor")
    (:Routine "0.3 0.4 0.1 setrgbcolor")
    (:Instrument "0.6 0.2 0.8 setrgbcolor")
    (:Item "0.8 0.6 0.0 setrgbcolor")
    (:Flag "0.9 0.3 0.3 setrgbcolor")
    (:Key "0.3 0.7 0.7 setrgbcolor")
    (:Translation "0.5 0.5 0.9 setrgbcolor")
    (t "0.3 0.3 0.3 setrgbcolor")))

(defun kind-name-display (kind-name)
  "Return a title-case display string for KIND-NAME keyword."
  (case kind-name
    (:blob "Blob")
    (:boat "Boat")
    (:character "Character")
    (:class "Class")
    (:flag "Flag")
    (:instrument "Instrument")
    (:item "Item")
    (:key "Key")
    (:map "Map")
    (:object-prototype "Object Prototype")
    (:routine "Routine")
    (:script "Script")
    (:song "Song")
    (:sprite-sheet "Sprite Sheet")
    (:tileset "Tileset")
    (:translation "Translation")
    (:atari-vox-dictionary "AtariVox Dictionary")
    (:intellivoice-dictionary "IntelliVoice Dictionary")
    (:phonetic-dictionary "Phonetic Dictionary")
    (:phrasebook "Phrasebook")
    (t (if (symbolp kind-name)
           (string-capitalize (string kind-name))
           (string kind-name)))))

(defun kind-name-plural (kind-name)
  "Return the plural display string for KIND-NAME keyword."
  (case kind-name
    (:script "Scripts")
    (:map "Maps")
    (:routine "Routines")
    (:song "Songs")
    (:blob "Blobs")
    (:boat "Boats")
    (:character "Characters")
    (:class "Classes")
    (:flag "Flags")
    (:instrument "Instruments")
    (:item "Items")
    (:key "Keys")
    (:object-prototype "Object Prototypes")
    (:sprite-sheet "Sprite Sheets")
    (:tileset "Tilesets")
    (:translation "Translations")
    (:atari-vox-dictionary "AtariVox Dictionaries")
    (:intellivoice-dictionary "IntelliVoice Dictionaries")
    (:phonetic-dictionary "Phonetic Dictionaries")
    (:phrasebook "Phrasebooks")
    (t (format nil "~as" (kind-name-display kind-name)))))

(defun color-for-asset-kind (kind-name)
  "Return a CLIM color for the KIND-NAME keyword.
   NIL gets white (no badge).
   Otherwise, maps alphabetically across a full H=0-360 rainbow
   with S=0.73, L=0.57 (from Royal Blue HSL)."
  (if (null kind-name)
      (clim:make-rgb-color 1 1 1)
      (let* ((kinds '(:blob :boat :character :class :flag :instrument :item :key :map
                      :object-prototype :phonetic-dictionary :routine :script :song
                      :sprite-sheet :tileset))
             (pos (position kind-name kinds))
             (n (length kinds)))
        (if pos
            (let ((packed (dufy:hsl-to-rgbpack (* pos (/ 360 (1- n))) 0.73 0.57)))
              (clim:make-rgb-color
               (/ (ldb (byte 8 16) packed) 255.0)
               (/ (ldb (byte 8 8) packed) 255.0)
               (/ (ldb (byte 8 0) packed) 255.0)))
            (clim:make-rgb-color 0.3 0.3 0.3)))))

(defun write-all-resources-ps (path)
  "Generate a PostScript document at PATH with the full Assets Index.
   Uses proper kind badges, colored section headings, D/P/A checkboxes,
   typographical quotes, bordered entries, and pagination."
  (let* ((all-resources (let ((*standard-output* (make-string-output-stream))
                              (*trace-output* *standard-output*))
                          (collect-all-resources)))
         (kind-order '(("Blobs" . :BLOB)
                       ("Boats" . :BOAT)
                       ("Characters" . :CHARACTER)
                       ("Classes" . :CLASS)
                       ("Flags" . :FLAG)
                       ("Instruments" . :INSTRUMENT)
                       ("Items" . :ITEM)
                       ("Keys" . :KEY)
                       ("Maps" . :MAP)
                       ("Object Prototypes" . :OBJECT-PROTOTYPE)
                       ("Routines" . :ROUTINE)
                       ("Scripts" . :SCRIPT)
                       ("Songs" . :SONG)
                       ("Sprite Sheets" . :SPRITE-SHEET)
                       ("Tilesets" . :TILESET)
                       ("Translations" . :TRANSLATION)))
         (page-width 612) (page-height 792)
         (margin-left 102) (margin-right 102)
         (page-top 680) (page-bottom 80)
         (line-h 12) (entry-h 14) (heading-h 20)
         (y page-top)
         (page-num 1))
    (declare (ignore line-h heading-h))
    (labels ((next-page (ps)
               ;; Footer for the page we're closing (icon + branding)
               (write-ps-page-footer ps page-num page-num
                                     (string-capitalize *game-title*) nil
                                     (user-real-name) (machine-instance))
               (format ps "showpage~%%%Page: ~d ~d~%" (1+ page-num) (+ page-num 1))
               (incf page-num)
               (setf y page-top)
               (write-ps-header-bar ps (format nil "Assets Index for ~a"
                                               (string-capitalize *game-title*))
                                    "" "" (string-capitalize *game-title*) 1 1)
               (format ps "/Times-Roman-ISOLatin1 findfont 9 scalefont setfont 0 0 0 setrgbcolor~%"))
             (check-space (ps needed)
               (when (< (- y needed) page-bottom)
                 (next-page ps))))
      (with-open-file (ps path :direction :output :if-exists :supersede
                               :external-format :utf-8)
        (format ps "%!PS-Adobe-3.0~%")
        (write-ps-docinfo ps (format nil "Assets Index for ~a" (string-capitalize *game-title*))
                          "Skyline-Tool" (format nil "~a on ~a" (user-real-name) (machine-instance)))
        (format ps "<< /PageSize [~d ~d] >> setpagedevice~%" page-width page-height)
        (write-ps-font-encodings ps)
        (format ps "%%Page: 1 1~%")
        (write-ps-header-bar ps (format nil "All Resources for ~a"
                                        (string-capitalize *game-title*))
                             "" "" (string-capitalize *game-title*) 1 1)
        (format ps "/Times-Roman-ISOLatin1 findfont 9 scalefont setfont 0 0 0 setrgbcolor~%")
        (setf y page-top)
        ;; Group by kind
        (let ((grouped (make-hash-table :test 'equal))
              (locale-groups (make-hash-table :test 'equal)))
          (dolist (resource all-resources)
            (push resource (gethash (game-resource-kind resource) grouped)))
          ;; Iterate kinds in order
          (dolist (kind-pair kind-order)
            (destructuring-bind (kind-text kind) kind-pair
              (let ((entries (reverse (gethash kind grouped nil))))
                (unless entries (return))
                ;; Kind heading
                (check-space ps 30)
                (format ps "gsave
 newpath ~d ~d ~d ~d rectfill
 0.0 0.0 0.0 setrgbcolor
 newpath ~d ~d ~d ~d rectstroke
 grestore
" margin-left (- y 18) (- page-width margin-left margin-right) 18
                        margin-left (- y 18) (- page-width margin-left margin-right) 18)
                (format ps "gsave
 ~a
 /Times-Bold-ISOLatin1 findfont 12 scalefont setfont
 1.0 1.0 1.0 setrgbcolor
 ~d ~d moveto (~a) show
 grestore
"
                        (color-rgb-for-kind kind)
                        (+ margin-left 6) (- y 6)
                        (escape-ps-string kind-text))
                (decf y 24)
                ;; Group by locale for Scripts and Maps
                (clrhash locale-groups)
                (dolist (e entries)
                  (let* ((moniker (if (typep e 'game-resource-asset)
                                      (game-asset-moniker e)
                                      (game-resource-locator e)))
                         (parts (split-sequence #\/ moniker))
                         (locale (if (member kind '("Scripts" "Maps") :test #'string-equal)
                                     (and (> (length parts) 2)
                                          (cl-change-case:title-case (second parts)))
                                     (if (string-equal kind "Characters")
                                         (and (> (length parts) 1)
                                              (cl-change-case:title-case (second parts)))
                                         ""))))
                    (push e (gethash (or locale "") locale-groups))))
                ;; Sort locales alphabetically
                (let ((locale-keys (sort (loop for k being the hash-keys of locale-groups
                                               collect k) #'string-lessp)))
                  (dolist (locale-key locale-keys)
                    (let ((locale-entries (reverse (gethash locale-key locale-groups))))
                      ;; Locale heading
                      (unless (string= locale-key "")
                        (check-space ps 20)
                        (format ps "gsave
 ~a
 /Times-Bold-ISOLatin1 findfont 10 scalefont setfont
 1.0 1.0 1.0 setrgbcolor
 ~d ~d moveto (~a) show
 grestore
" (color-rgb-for-kind kind)
                                (+ margin-left 12) (- y 4) (escape-ps-string locale-key))
                        (decf y 16))
                      ;; Asset entries
                      (dolist (entry locale-entries)
                        (let* ((moniker (if (typep entry 'game-resource-asset)
                                            (game-asset-moniker entry)
                                            (game-resource-locator entry)))
                               (builds (game-resource-builds entry))
                               (kind-name (game-resource-kind entry))
                               (hex-str (when (typep entry 'game-resource-asset)
                                          (let ((aid (game-resource-asset-id entry)))
                                            (when aid
                                              (format nil "$~(~v,'0x~)"
                                                      (if (eql (kind-by-name kind-name) :script) 4 2)
                                                      aid)))))
                               (present-p (some #'probe-file (game-resource-pathnames entry))))
                          (check-space ps entry-h)
                          (let* ((parts (split-sequence #\/ moniker))
                                 (basename (car (last parts)))
                                 (kind-key (kind-by-name kind-name))
                                 (display-name
                                   (case kind-key
                                     ((:script :song :blob)
                                      (format nil "~c~a~c" (code-char #x201C)
                                              (cl-change-case:title-case
                                               (cl-ppcre:regex-replace "\\bDont\\b" basename "Don't"))
                                              (code-char #x201D)))
                                     (:map
                                      (cl-change-case:title-case
                                       (cl-ppcre:regex-replace "\\bDont\\b" basename "Don't")))
                                     (t basename)))
                                 (rgb (if present-p "0 0 0" "0.8 0 0")))
                            (declare (ignore rgb))
                            ;; Border
                            (format ps "gsave
 newpath ~d ~d ~d ~d rectstroke
 grestore
" margin-left (- y entry-h) (- page-width margin-left margin-right) entry-h)
                            ;; Kind badge — golden rectangle (φ ≈ 1.618), full entry height, white text
                            (format ps "gsave
 ~a
 newpath ~d ~d ~d ~d rectfill
 1.0 1.0 1.0 setrgbcolor
 /Times-Bold-ISOLatin1 findfont 7 scalefont setfont
 ~d ~d moveto (~a) show
 grestore
" (color-rgb-for-kind kind-name)
                                    margin-left (- y entry-h) 22 entry-h
                                    (+ margin-left 4) (- y 4) (escape-ps-string kind-name))
                            ;; Asset name
                            (format ps "/Times-Roman-ISOLatin1 findfont 9 scalefont setfont
 ~d ~d moveto (~a) show
" (+ margin-left 66) (- y 4) (escape-ps-string display-name))
                            ;; Map name trailing digits in gray
                            (when (and (eq kind-key :map) (plusp (length display-name))
                                       (digit-char-p (char display-name (1- (length display-name)))))
                              (let* ((str (princ-to-string display-name))
                                     (split (position-if-not #'digit-char-p str :from-end t :end (1- (length str)))))
                                (when split
                                  (let ((digits (subseq str (1+ split))))
                                    (format ps "0.25 0.25 0.25 setrgbcolor
 ~d ~d moveto (~a) show
 0 0 0 setrgbcolor
" (+ margin-left 66 (* 2 (length (subseq str 0 (1+ split))) 4))
                                            (- y 4) (escape-ps-string digits))))))
                            ;; Hex ID
                            (when hex-str
                              (format ps "/Times-Roman-ISOLatin1 findfont 7 scalefont setfont
 0.5 0.5 0.5 setrgbcolor
 ~d ~d moveto (~a) show
 0 0 0 setrgbcolor
" (- page-width margin-right 80) (- y 4) (escape-ps-string hex-str)))
                            ;; D/P/A checkboxes
                            (let* ((x-check (+ (- page-width margin-right) 2))
                                   (checked-d (and builds (member "Demo" builds :test #'string-equal)))
                                   (checked-p (and builds (member "Public" builds :test #'string-equal)))
                                   (checked-a (and builds (member "AA" builds :test #'string-equal))))
                              (format ps "/Times-Bold-ISOLatin1 findfont 8 scalefont setfont
 ~d ~d moveto
" x-check (- y 2))
                              (if checked-d
                                  (format ps "0.0 0.6 0.0 setrgbcolor (D) show ")
                                  (format ps "0.6 0.6 0.6 setrgbcolor ( ) show "))
                              (format ps "~d ~d moveto" (+ x-check 10) (- y 2))
                              (if checked-p
                                  (format ps "0.0 0.6 0.0 setrgbcolor (P) show ")
                                  (format ps "0.6 0.6 0.6 setrgbcolor ( ) show "))
                              (format ps "~d ~d moveto" (+ x-check 20) (- y 2))
                              (if checked-a
                                  (format ps "0.0 0.6 0.0 setrgbcolor (A) show~%")
                                  (format ps "0.6 0.6 0.6 setrgbcolor ( ) show~%")))
                            (decf y entry-h))))))))))
          ;; Page footer and showpage
          (write-ps-page-footer ps page-num page-num
                                (string-capitalize *game-title*) nil
                                (user-real-name) (machine-instance))
          (format ps "showpage~%"))))))

;; --- Collapsible section state ---


(defvar *all-resources-collapsed* (make-hash-table :test 'equal)
  "Hash table mapping section key strings to booleans (t = collapsed).")

(defvar *all-resources-state*
  (list :collapsed *all-resources-collapsed*
        :current-kind nil
        :current-locale nil)
  "Plist holding the assets index display state.")

(defvar *assets-index-filter* nil
  "When non-NIL, a string used to filter All Resources entries by moniker or display name.")

(defvar *all-resources-sort-mode* :alpha
  "Sort mode for All Resources: :alpha (alphabetical) or :numeric (by asset ID).")

(clim:define-presentation-type assets-section-header () :inherit-from 'string)

(clim:define-command (com-toggle-section-header :command-table clim-internals::global-command-table
                                                :menu t :name t)
    ((section 'assets-section-header :gesture :select))
  (setf (gethash section *all-resources-collapsed*)
        (not (gethash section *all-resources-collapsed*)))
  (set-pref :all-resources-collapsed
            (loop for k being the hash-keys of *all-resources-collapsed*
                  collect (cons k (if (gethash k *all-resources-collapsed*) "true" "false"))))
  (when (boundp 'clim:*application-frame*)
    (clim:redisplay-frame-panes clim:*application-frame* :force-p t)))

(clim:define-command (com-section-header-menu :command-table clim-internals::global-command-table
                                              :menu t :name t)
    ((section 'assets-section-header :gesture :menu))
  (let* ((parts (split-sequence #\/ section))
         (kind-name (first parts))
         (kind-key (ignore-errors (kind-by-name kind-name)))
         (locale (when (> (length parts) 1) (second parts)))
         (items nil))
    (flet ((add-item (label fn)
             (push (list label fn) items))
           (add-divider ()
             (push :divider items)))
      (add-item "Open in File Manager..."
                (lambda ()
                  (let ((dir (case kind-key
                               (:script (if locale
                                            (format nil "Source/Scripts/~a/" locale)
                                            "Source/Scripts/"))
                               (:map (if locale
                                         (format nil "Source/Maps/~a/" locale)
                                         "Source/Maps/"))
                               (:song "Source/Songs/")
                               (:character "Source/Objects/")
                               (:class "Source/Classes/")
                               (:tileset "Source/Maps/Tiles/7800/")
                               (:sprite-sheet "Source/Art/7800/")
                               (:blob "Source/Blobs/7800/")
                               (:object-prototype "Source/Objects/")
                               (:routine "Source/Code/")
                               (:boat "Source/Objects/")
                               (t nil))))
                    (when dir
                      (uiop:run-program (list "xdg-open"
                                              (namestring (merge-pathnames dir (uiop:getcwd))))
                                        :output nil )))))
      (when (and (eql kind-key :map) locale)
        (add-divider)
        (add-item (format nil "New Map in ~a..." locale)
                  (lambda () (error "~&New map creation not yet implemented.~%")))
        (add-item (format nil "Edit ~a Atlas..." locale)
                  (lambda ()
                    (let ((path (merge-pathnames
                                 (format nil "Source/Maps/~a/~a.tex" locale locale)
                                 (uiop:getcwd))))
                      (when (probe-file path)
                        (open-in-emacs (truename path)))))))
      (unless locale
        (add-divider)
        (add-item "New..."
                  (lambda () (error "~&New ~a resource creation not yet implemented.~%" kind-name))))
      (clim:menu-choose (nreverse items) :label (format nil "~a" section)))))

(defgeneric compute-resource-display-name (resource basename)
  (:documentation "Return the display name for a resource entry.")
  (:method ((resource game-resource) basename)
    (cl-change-case:title-case
     (cl-ppcre:regex-replace "\\bDont\\b" basename "Don't"))))

(defmethod compute-resource-display-name ((resource game-resource-script) basename)
  (format nil "~c~a~c" (code-char #x201C)
          (cl-change-case:title-case
           (cl-ppcre:regex-replace "\\bDont\\b" basename "Don't"))
          (code-char #x201D)))

(defmethod compute-resource-display-name ((resource game-resource-song) basename)
  (format nil "~c~a~c" (code-char #x201C)
          (cl-change-case:title-case
           (cl-ppcre:regex-replace "\\bDont\\b" basename "Don't"))
          (code-char #x201D)))

(defmethod compute-resource-display-name ((resource game-resource-blob) basename)
  (format nil "~c~a~c" (code-char #x201C)
          (cl-change-case:title-case
           (cl-ppcre:regex-replace "\\bDont\\b" basename "Don't"))
          (code-char #x201D)))

(defmethod compute-resource-display-name ((resource game-resource-map) basename)
  (cl-change-case:title-case
   (cl-ppcre:regex-replace "\\bDont\\b" basename "Don't")))

(defmethod compute-resource-display-name ((resource game-resource-character) basename)
  (cl-change-case:title-case
   (cl-ppcre:regex-replace "\\bDont\\b" basename "Don't")))

(defgeneric present-resource-name (resource stream name red)
  (:documentation "Output NAME on STREAM with font and color appropriate for RESOURCE.")
  (:method ((resource game-resource) stream name red)
    (clim:with-text-style (stream (clim:make-text-style :serif :italic :normal))
      (if red
          (clim:with-drawing-options (stream :ink (clim:make-rgb-color 0.8 0 0))
            (princ name stream))
          (princ name stream)))))

(defmethod present-resource-name ((resource game-resource-map) stream name red)
  (clim:with-text-style (stream (clim:make-text-style :serif :roman :normal))
    (let* ((str (princ-to-string name))
           (len (length str)))
      (if (and (plusp len) (digit-char-p (char str (1- len))))
          (let ((split (position-if-not #'digit-char-p str :from-end t :end (1- len))))
            (if split
                (let ((prefix (subseq str 0 (1+ split)))
                      (digits (subseq str (1+ split))))
                  (if red
                      (clim:with-drawing-options (stream :ink (clim:make-rgb-color 0.8 0 0))
                        (princ prefix stream))
                      (princ prefix stream))
                  (write-string "  " stream)
                  (clim:with-drawing-options (stream :ink (clim:make-gray-color 0.25))
                    (princ digits stream)))
                (progn
                  (write-string "  " stream)
                  (clim:with-drawing-options (stream :ink (clim:make-gray-color 0.25))
                    (princ str stream)))))))))

(defun render-kind-badge (stream the-pane start-y end-y badge-w kind-name)
  "Draw the colored kind badge rectangle full height of entry, with white bold lowercase kind-name text."
  (clim:draw-rectangle* the-pane 0 start-y badge-w end-y
                        :ink (color-for-asset-kind kind-name) :filled t)
  (clim:stream-set-cursor-position stream 8 (+ start-y 10))
  (clim:with-drawing-options (stream :ink (clim:make-rgb-color 1 1 1))
    (clim:with-text-face (stream :bold)
      (princ (if (eql kind-name :phonetic-dictionary)
                 "vox"
                 (string-downcase (kind-name-display kind-name)))
             stream))))

(defun render-build-checkboxes (stream moniker builds kind-name asset-id hex-str present-p full-path)
  "Output D/P/A build checkboxes in right margin under hex ID."
  (clim:with-output-as-presentation
      (stream (list moniker builds kind-name asset-id hex-str present-p full-path #\D)
              'build-checkbox)
    (princ (if (member "Demo" builds :test 'string-equal) "☑D" "☐D") stream))
  (write-string " " stream)
  (clim:with-output-as-presentation
      (stream (list moniker builds kind-name asset-id hex-str present-p full-path #\P)
              'build-checkbox)
    (princ (if (member "Public" builds :test 'string-equal) "☑P" "☐P") stream))
  (write-string " " stream)
  (clim:with-output-as-presentation
      (stream (list moniker builds kind-name asset-id hex-str present-p full-path #\A)
              'build-checkbox)
    (princ (if (member "AA" builds :test 'string-equal) "☑A" "☐A") stream)))

(defun count-kind-assets (kind-name subsection-key all-assets)
  "Count entries matching kind-name and subsection-key."
  (count-if (lambda (e)
              (let ((e-kind (game-resource-kind e)))
                (and (string-equal e-kind kind-name)
                     (or (null subsection-key)
                         (and (typep e 'game-resource-asset)
                              (let ((e-parts (split-sequence #\/ (game-asset-moniker e))))
                                (and (> (length e-parts) 2)
                                     (string-equal (second e-parts) subsection-key))))))))
            all-assets))

(defun render-kind-heading (the-pane pane-width kind-name sk skip-kind all-assets)
  "Render a colored 3-line kind section heading, clickable to collapse.
   When collapsed, shows count of resources in right margin."
  (let ((cursor-y (nth-value 1 (clim:stream-cursor-position *standard-output*)))
        (heading-h 48))
    (clim:with-output-as-presentation (the-pane sk 'assets-section-header)
      (clim:draw-rectangle* the-pane 0 cursor-y pane-width (+ cursor-y heading-h)
                            :ink (color-for-asset-kind kind-name) :filled t))
    (clim:stream-set-cursor-position *standard-output* 8 (+ cursor-y 14))
    (clim:with-drawing-options (*standard-output* :ink (clim:make-rgb-color 1 1 1))
      (clim:with-text-face (*standard-output* :bold)
        (clim:with-text-size (*standard-output* :larger)
          (format *standard-output* "~a ~a" (if skip-kind "▶ " "▼ ") sk))))
    ;; Show count in right margin when collapsed
    (when skip-kind
      (let* ((count (count-kind-assets kind-name nil all-assets))
             (count-str (format nil "(~d)" count))
             (x-pos (- pane-width (clim:text-size the-pane count-str) 16)))
        (clim:stream-set-cursor-position *standard-output* x-pos (+ cursor-y 14))
        (clim:with-drawing-options (*standard-output* :ink (clim:make-rgb-color 1 1 1))
          (princ count-str *standard-output*))))
    (clim:stream-set-cursor-position *standard-output* 0 (+ cursor-y heading-h))))

(defun render-grouping-heading (the-pane pane-width kind-name subsection-key this-grouping skip-grouping all-assets)
  "Render a 1-line subsection heading, clickable to collapse.
   When collapsed, shows count of resources in right margin."
  (let ((cursor-y (nth-value 1 (clim:stream-cursor-position *standard-output*)))
        (heading-h 20))
    (clim:with-output-as-presentation (the-pane subsection-key 'assets-section-header)
      (clim:draw-rectangle* the-pane 0 cursor-y pane-width (+ cursor-y heading-h)
                            :ink (color-for-asset-kind kind-name) :filled t))
    (clim:stream-set-cursor-position *standard-output* 8 (+ cursor-y 2))
    (clim:with-drawing-options (*standard-output* :ink (clim:make-rgb-color 1 1 1))
      (clim:with-text-size (*standard-output* :smaller)
        (princ (if skip-grouping "▶ " "▼ ") *standard-output*)
        (princ this-grouping *standard-output*)))
    ;; Show count in right margin when collapsed
    (when skip-grouping
      (let* ((count (count-kind-assets kind-name this-grouping all-assets))
             (count-str (format nil "(~d)" count))
             (x-pos (- pane-width (clim:text-size the-pane count-str) 16)))
        (clim:stream-set-cursor-position *standard-output* x-pos (+ cursor-y 2))
        (clim:with-drawing-options (*standard-output* :ink (clim:make-rgb-color 1 1 1))
          (princ count-str *standard-output*))))
    (clim:stream-set-cursor-position *standard-output* 0 (+ cursor-y heading-h))))

(defun asset-matches-filter-p (entry filter &key (search-title-p t) search-body-p)
  "Return T if ENTRY (moniker builds kind-name ...) matches FILTER string.
Matches against the full moniker or the asset base name, case-insensitive."
  (let ((needle (remove-if-not #'alphanumericp (string-downcase filter))))
    (or (when search-title-p
          (search needle
                  (remove-if-not #'alphanumericp (string-downcase (game-resource-title entry)))))
        (when search-body-p
          (search needle
                                     (remove-if-not #'alphanumericp (string-downcase (game-resource-full-text entry))))))))

(defun compute-asset-display-name (basename kind-key)
  "Compute a human-readable display name from BASENAME and KIND-KEY.
Converts filenames like \"en_US/gameplay/some-name\" to \"Some Name\"."
  (declare (ignore kind-key))
  (cl-change-case:title-case
   (substitute #\Space #\- (substitute #\Space #\_ basename))))

(defun present-asset-name (stream display-name kind-key red-p)
  "Present DISPLAY-NAME on STREAM, optionally in red when RED-P is true.
KIND-KEY is the keyword asset type for potential per-kind styling."
  (declare (ignore kind-key))
  (when display-name
    (if red-p
        (clim:with-drawing-options (stream :ink clim:+red+)
          (write-string display-name stream))
        (write-string display-name stream))))

(defun show-all-resources-internal ()
  "Display all assets with colored type squares, title-cased names,
    hex IDs, D/P/A checkboxes. Click name for action menu.
    Assets absent from disk appear in red.
    Click kind/locale headings to collapse/expand sections."
  (unless (and *all-resources-state*
               (getf *all-resources-state* :collapsed))
    ;; Restore collapsed state from prefs, or default all to collapsed
    ;; null / true = collapsed, false = expanded
    (let ((saved (get-pref :all-resources-collapsed)))
      ;; Default all sections collapsed
      (dolist (k '("Scripts" "Songs" "Maps" "Characters" "Boats" "Blobs"
                   "Tilesets" "Sprite Sheets" "Object Prototypes" "Classes" "Routines"
                   "Instruments" "Items" "Flags" "Keys" "AtariVox Dictionary"))
        (setf (gethash k *all-resources-collapsed*) t))
      ;; Override with persisted: "true" = collapsed (t), "false" = expanded (nil)
      (when saved
        (dolist (pair saved)
          (setf (gethash (car pair) *all-resources-collapsed*)
                (string-equal (cdr pair) "true")))))
    (setf *all-resources-state*
          (list :collapsed *all-resources-collapsed*
                :current-kind nil
                :current-locale nil)))
  (let* ((all-resources (if (and (stringp *assets-index-filter*)
                                  (plusp (length *assets-index-filter*)))
                             (remove-if-not (lambda (entry)
                                              (asset-matches-filter-p entry *assets-index-filter*))
                                            (collect-all-resources))
                             (collect-all-resources)))
         (all-resources (sort (copy-list all-resources)
                               (lambda (a b)
                                 (let ((ka (game-resource-kind a))
                                       (kb (game-resource-kind b)))
                                   (if (eql ka kb)
                                       (string< (or (if (typep a 'game-resource-asset)
                                                        (game-asset-moniker a)
                                                        (game-resource-locator a))
                                                    "")
                                                (or (if (typep b 'game-resource-asset)
                                                        (game-asset-moniker b)
                                                        (game-resource-locator b))
                                                    ""))
                                       (string< (or (kind-name-display ka) "~")
                                                (or (kind-name-display kb) "~")))))))
         (collapsed (getf *all-resources-state* :collapsed))
         (last-kind nil)
         (last-grouping nil)
         skip-kind
         skip-grouping
         grouping)
    (flet ((section-kind (kn)
             (cond
               ((null kn) nil)
               ((eql kn :phonetic-dictionary) (kind-name-plural :translation))
               (t (kind-name-plural kn))))
           (section-key (kn)
             (cond
               ((null kn) nil)
               ((eql kn :phonetic-dictionary) (kind-name-plural :translation))
               (t (kind-name-plural kn)))))
      (terpri)
      (dolist (resource all-resources)
        (let* ((moniker (if (typep resource 'game-resource-asset)
                            (game-asset-moniker resource)
                            (game-resource-locator resource)))
               (builds (game-resource-builds resource))
               (kind-name (game-resource-kind resource))
               (asset-id (when (typep resource 'game-resource-asset)
                           (game-resource-asset-id resource)))
               (hex-str (when asset-id
                          (format nil "$~(~v,'0x~)"
                                  (if (eql (kind-by-name kind-name) :script) 4 2)
                                  asset-id)))
               (full-path (first (game-resource-pathnames resource)))
               (present-p (some #'probe-file (game-resource-pathnames resource)))
               (sk (section-kind kind-name)))
          ;; --- Kind heading (skip for Special Resources: nil kind-name) ---
          (unless (string-equal sk last-kind)
            (setf last-kind sk
                  last-grouping nil
                  skip-kind (gethash sk collapsed)
                  skip-grouping nil)
            (unless (null kind-name)
              (let* ((the-pane clim-simple-echo::*echo-pane*)
                     (pane-width (clim:bounding-rectangle-width
                                  (clim:sheet-region the-pane))))
                (render-kind-heading the-pane pane-width kind-name sk skip-kind all-resources))))
          ;; --- Skip if kind collapsed ---
          (unless skip-kind
            ;; --- Subgroup header for Scripts, Maps, and Routines ---
            (let* ((parts (split-sequence #\/ moniker))
                   (this-grouping
                    (case kind-name
                      ((:script :map)
                       (let ((kind-dir (if (eql kind-name :script) "Scripts" "Maps")))
                         (let ((idx (position kind-dir parts :test #'string-equal)))
                           (when (and idx (< (1+ idx) (length parts))
                                      (not (string-equal kind-dir (nth (1+ idx) parts))))
                             (cl-change-case:title-case (nth (1+ idx) parts))))))
                      (:routine
                       (cond
                         ((typep resource 'game-resource-routine-forth-library)
                          "Forth Libraries")
                         ((typep resource 'game-resource-routine-run-commands)
                          "Run Commands")
                         (t nil)))
                      (t nil))))
              (when (and this-grouping (not (string-equal this-grouping last-grouping)))
                (setf last-grouping this-grouping)
                (let ((subsection-key (format nil "~a/~a" (section-key kind-name) this-grouping)))
                  (setf skip-grouping (gethash subsection-key collapsed))
                  (let* ((the-pane clim-simple-echo::*echo-pane*)
                         (pane-width (clim:bounding-rectangle-width
                                      (clim:sheet-region the-pane))))
                    (render-grouping-heading the-pane pane-width kind-name subsection-key this-grouping
                                            skip-grouping all-resources)))))
            ;; --- Entry display (skip if grouping collapsed) ---
            (unless skip-grouping
              (let* ((title-str (game-resource-title resource))
                     (subheading (game-resource-subheading resource))
                     (locator (game-resource-locator resource)))
                (clim:with-output-as-presentation
                    (clim-simple-echo::*echo-pane* resource 'game-resource)
                  (clim:surrounding-output-with-border (clim-simple-echo::*echo-pane*)
                    (let* ((the-pane clim-simple-echo::*echo-pane*)
                           (pane-width (clim:bounding-rectangle-width (clim:sheet-region the-pane)))
                           (start-y (nth-value 1 (clim:stream-cursor-position *standard-output*)))
                           (badge-h 72)
                           (badge-w (floor (* badge-h 1.618)))
                           (red (not present-p))
                           (title-x (if kind-name (+ badge-w 8) 4))
                           (target-y (+ start-y badge-h))
                           (id-x (- pane-width 120)))
                      ;; Icon / kind badge - full entry height
                      (when kind-name
                        (render-kind-badge *standard-output* the-pane start-y target-y badge-w kind-name))
                      ;; Title (game-resource-title, red if asset missing from disk)
                      (when kind-name (write-string " " *standard-output*))
                      (clim:stream-set-cursor-position *standard-output* title-x start-y)
                      (present-asset-name *standard-output* title-str kind-name (and red (not (eql kind-name :boat))))
                      ;; Subtitle (game-resource-subheading, small gray)
                      (when (and subheading (plusp (length subheading)))
                        (clim:stream-set-cursor-position *standard-output* (+ title-x 16) (+ start-y 18))
                        (clim:with-text-size (*standard-output* :smaller)
                          (clim:with-drawing-options (*standard-output* :ink (clim:make-gray-color 0.75))
                            (princ subheading *standard-output*))))
                      ;; Right column: locator, build checkboxes, VC status
                      (when locator
                        (clim:stream-set-cursor-position *standard-output* id-x start-y)
                        (princ locator *standard-output*))
                      (when builds
                        (clim:stream-set-cursor-position *standard-output* id-x (+ start-y 16))
                        (render-build-checkboxes *standard-output* moniker builds kind-name
                                                 asset-id hex-str present-p full-path))
                      ;; Minimum 3-line entry height
                      (loop while (< (nth-value 1 (clim:stream-cursor-position *standard-output*))
                                     target-y)
                            do (terpri))
                      ;; Stretch border to full pane width
                      (clim:stream-set-cursor-position
                       *standard-output*
                       (- pane-width 5)
                       (nth-value 1 (clim:stream-cursor-position *standard-output*))))))))))))))

;; --- Asset action menu command ---

(clim:define-command (com-asset-action-menu :command-table clim-internals::global-command-table
                                            :menu t :name t)
    ((resource 'game-resource :gesture :select))
  (%show-resource-context-menu resource))

(clim:define-command (com-asset-context-menu :command-table clim-internals::global-command-table
                                             :menu t :name t)
    ((resource 'game-resource :gesture :select))
  (%show-resource-context-menu resource))

(defun %toggle-build-flag (moniker builds flag-char)
  "Toggle a build flag (D/P/A) for MONIKER in Assets.index.
When all three flags are cleared, the entry is commented out with #.
When all three flags are set, the entry has no letters in Assets.index."
  (read-assets-list)
  (let* ((entry (gethash moniker *assets-list*))
         (new-builds (cond
                       ((eql flag-char #\D)
                        (if (member "Demo" builds :test #'string-equal)
                            (remove "Demo" builds :test #'string-equal)
                            (cons "Demo" builds)))
                       ((eql flag-char #\P)
                        (if (member "Public" builds :test #'string-equal)
                            (remove "Public" builds :test #'string-equal)
                            (cons "Public" builds)))
                       ((eql flag-char #\A)
                        (if (member "AA" builds :test #'string-equal)
                            (remove "AA" builds :test #'string-equal)
                            (cons "AA" builds)))
                       (t builds))))
    (if entry
        (setf (gethash moniker *assets-list*) new-builds)
        (setf (gethash moniker *assets-list*) new-builds))
    ;; Write back to Assets.index
    (let ((index-path (merge-pathnames "Source/Assets.index" (uiop:getcwd))))
      (with-open-file (out index-path :direction :output :if-exists :supersede
                                      :external-format :utf-8)
        (format out "# Assets.index for Phantasia~%")
        (format out "# Format: moniker D|P|A~%")
        (format out "# D=Demo, P=Public, A=AA (AtariAge)~%")
        (format out "# If no letters, all three are enabled~%")
        (format out "# If line starts with #, asset is disabled~%~%")
        (maphash (lambda (m bs)
                   (let ((comment (if (and bs (zerop (length bs))) "# " "")))
                     (format out "~a~a~@[ ~a~]~%" comment m
                             (when bs
                               (let ((str (with-output-to-string (s)
                                            (when (member "Demo" bs :test #'string-equal)
                                              (princ "D" s))
                                            (when (member "Public" bs :test #'string-equal)
                                              (princ "P" s))
                                            (when (member "AA" bs :test #'string-equal)
                                              (princ "A" s)))))
                                 (if (zerop (length str))
                                     ""
                                     str))))))
                 *assets-list*)))
    (clim:redisplay-frame-panes clim:*application-frame* :force-p t)))


;; --- All Resources Frame ---

(clim:define-command-table resource-edit-menu
  :menu (("Copy List" :command com-resource-copy-list)
         ("Paste JSON as Resource" :command com-paste-as-resource)
         (nil :divider :line)
         ("Find..." :command com-resource-focus-search-filters)
         (nil :divider :line)
         ("Project..." :command com-project-inspector)
         ("Preferences..." :command com-preferences-inspector)))

(clim:define-command-table resource-new-menu
  :menu (("BLOB..." :command com-new-blob)
         ("Boat..." :command com-new-boat)
         ("Character..." :command com-new-character)
         ("Class..." :command com-new-class)
         ("Flag..." :command com-new-flag)
         ("Instrument..." :command com-new-instrument)
         ("Item..." :command com-new-item)
         ("Key..." :command com-new-key)
         ("Map..." :command com-new-map)
         ("Object Prototype..." :command com-new-object-prototype)
         ("Routine" :menu routine-new-submenu)
         ("Script..." :command com-new-script-from-menu)
         ("Song..." :command com-new-song)
         ("Sprite Sheet..." :command com-new-sprite-sheet)
         ("Tileset..." :command com-new-tileset)
         ("Translation" :menu translation-new-submenu)
         (nil :divider :line)
         ("Project..." :command com-new-project)
         ("Project from URL..." :command com-new-project-url)
         ("Port to Machine..." :command com-new-project-port)))

(clim:define-command-table routine-new-submenu
  :menu (("Forth Library..." :command com-new-routine-forth-library)
         (nil :divider :line)
         ("BASIC Run Commands..." :command com-new-routine-bas)
         ("COBOL Run Commands..." :command com-new-routine-cob)
         ("Pascal Run Commands..." :command com-new-routine-pas)))

(clim:define-command-table translation-new-submenu
  :menu (("Phonetic Dictionary" :menu phonetic-dict-new-submenu)
         ("Phrasebook..." :command com-new-phrasebook)))

(clim:define-command-table phonetic-dict-new-submenu
  :menu (("AtariVox..." :command com-new-atari-vox-dictionary)
         ("IntelliVoice..." :command com-new-intellivoice-dictionary)
         ("Magic Desk..." :command com-new-magic-desk-dictionary)))

(clim:define-command-table resource-save-list-as-menu
  :menu (("JSON..." :command com-save-as-json)
         ("Text..." :command com-save-text)
         ("Spreadsheet..." :command com-save-spreadsheet)
         ("PDF..." :command com-print-pdf)))

(clim:define-command-table resource-lisp-menu
  :inherit-from (clim-internals::global-command-table)
  :menu (("Run REPL..." :command com-run-repl)
         (nil :divider :line)
         ("Show Lisp Room..." :command com-show-lisp-room)
         ("Show Lisp Threads..." :command com-show-lisp-threads-inspector)
         ("Show Lisp Journal..." :command com-show-journal)
         ("Show Clouseau..." :command com-show-clouseau)
         (nil :divider :line)
         ("Reload from Source..." :command com-reload-skyline-tool-from-sources)))

;; --- Dynamic Build and Region menus ---

(defun clear-command-table-menu (command-table)
  "Remove all menu items from COMMAND-TABLE (symbol or command table object).
   McCLIM's map-over-command-table-menu-items passes keyword-prefixed lists
   (:item name ...) (:menu name ...) (:divider name ...); we extract the name."
  (let ((ct (if (symbolp command-table)
                (clim:find-command-table command-table)
                command-table)))
    (when ct
      (handler-case
          (let ((names nil))
            (clim:map-over-command-table-menu-items
             ct
             (lambda (item)
               (when (and (listp item) (stringp (second item)))
                 (push (second item) names))))
            (dolist (name names)
              (ignore-errors
                (clim:remove-menu-item-from-command-table ct name))))
        (error (c)
          (declare (ignore c))
          nil)))))

;; --- Dynamic Build and Region menus ---

(defun get-current-build ()
  "Return current build from preferences."
  (or (get-pref :build) "Public"))

(defun get-current-region ()
  "Return current region from preferences."
  (or (get-pref :region) "NTSC"))

(defun supported-regions-for-machine ()
  "Return list of (label command enabled) tuples for regions supported by current machine."
  (list
   (list "NTSC" 'com-set-region-ntsc t)
   (list "PAL" 'com-set-region-pal t)
   (list "SECAM" 'com-set-region-secam (not (= *machine* 7800)))))

(defun supported-builds-for-machine ()
  "Return list of (label command enabled) tuples for builds supported by current machine."
  (list
   (list "Demo" 'com-set-build-demo t)
   (list "Public" 'com-set-build-public t)
   (list "Publisher" 'com-set-build-publisher t)))

(defun populate-region-menu ()
  "Populate the resource-region-menu with current region checked."
  (let ((current (get-current-region)))
    (clear-command-table-menu 'resource-region-menu)
    (dolist (item (supported-regions-for-machine))
      (destructuring-bind (label cmd enabled) item
        (let ((checked (string-equal label current)))
          (handler-case
            (clim:add-menu-item-to-command-table
             'resource-region-menu
             (format nil "~:[☐~;☑~] ~a" checked label)
             :command (if enabled cmd nil)
             :after :end)
            (clim:command-already-present ())))))))

(defun populate-build-menu ()
  "Populate the resource-build-menu with current build checked."
  (let ((current (get-current-build)))
    (clear-command-table-menu 'resource-build-menu)
    (dolist (item (supported-builds-for-machine))
      (destructuring-bind (label cmd enabled) item
        (let ((checked (string-equal label current)))
          (handler-case
            (clim:add-menu-item-to-command-table
             'resource-build-menu
             (format nil "~:[☐~;☑~] ~a" checked label)
             :command (if enabled cmd nil)
             :after :end)
            (clim:command-already-present ())))))))

;; Commands for setting build
(clim:define-command (com-set-build-demo :command-table clim-internals::global-command-table
                                         :menu t :name t) ()
  (set-pref :build "Demo")
  (populate-build-menu)
  (when (boundp 'clim:*application-frame*)
    (clim:redisplay-frame-panes clim:*application-frame* :force-p t)))

(clim:define-command (com-set-build-public :command-table clim-internals::global-command-table
                                           :menu t :name t) ()
  (set-pref :build "Public")
  (populate-build-menu)
  (when (boundp 'clim:*application-frame*)
    (clim:redisplay-frame-panes clim:*application-frame* :force-p t)))

(clim:define-command (com-set-build-publisher :command-table clim-internals::global-command-table
                                              :menu t :name t) ()
  (set-pref :build "Publisher")
  (populate-build-menu)
  (when (boundp 'clim:*application-frame*)
    (clim:redisplay-frame-panes clim:*application-frame* :force-p t)))

;; Commands for setting region
(clim:define-command (com-set-region-ntsc :command-table clim-internals::global-command-table
                                          :menu t :name t) ()
  (set-pref :region "NTSC")
  (populate-region-menu)
  (when (boundp 'clim:*application-frame*)
    (clim:redisplay-frame-panes clim:*application-frame* :force-p t)))

(clim:define-command (com-set-region-pal :command-table clim-internals::global-command-table
                                         :menu t :name t) ()
  (set-pref :region "PAL")
  (populate-region-menu)
  (when (boundp 'clim:*application-frame*)
    (clim:redisplay-frame-panes clim:*application-frame* :force-p t)))

(clim:define-command (com-set-region-secam :command-table clim-internals::global-command-table
                                           :menu t :name t) ()
  (set-pref :region "SECAM")
  (populate-region-menu)
  (when (boundp 'clim:*application-frame*)
    (clim:redisplay-frame-panes clim:*application-frame* :force-p t)))

;; Initialize menus when frame is created
(defun initialize-run-menus ()
  (populate-build-menu)
  (populate-region-menu))

;; --- Build and Region command tables (dynamic) ---

(clim:define-command-table resource-region-menu
  :inherit-from (clim-internals::global-command-table)
  :menu ())

(clim:define-command-table resource-build-menu
  :inherit-from (clim-internals::global-command-table)
  :menu ())

(clim:define-command-table resource-tools-menu
  :inherit-from (clim-internals::global-command-table)
  :menu (("Show ROM Budget..." :command com-show-rom-budget)
         (nil :divider :line)
         ("Animation Sequence Editor..." :command com-anim-seq-editor)
         ("Assign Animation Sequences..." :command com-assign-animation-sequences)
         (nil :divider :line)
         ("Open Emacs..." :command com-run-emacs)
         ("Open Files..." :command com-open-file-manager)
         ("Open Gimp..." :command com-run-gimp)
         ("Open LibreCalc..." :command com-run-libre-calc)
         ("Open MuseScore..." :command com-run-muse-score)
         ("Open ThiefMD..." :command com-run-thief-md)
         ("Open Tiled..." :command com-run-tiled)))

(clim:define-command-table resource-run-menu
  :menu (("Build" :menu resource-build-menu)
         ("Region" :menu resource-region-menu)
         (nil :divider :line)
         ("Run in A7800..." :command com-run-in-a7800)
         ("Run in js7800..." :command com-run-in-js7800)
         (nil :divider :line)
         ("Send to SD Card..." :command com-send-to-sd)
         ("Send to 7800GD Debug Port..." :command com-push-binary-to-7800gd)
         (nil :divider :line)
         ("Make" :menu resource-make-menu)
         (nil :divider :line)
         ("Build a Release Package..." :command com-build-release-package)))

;; --- Machine to emulator binary extension mapping ---

(defun emulator-binary-extension (machine)
  "Return the emulator binary extension for MACHINE.
   Used for running in emulators (e.g., .a78 for 7800, .a26 for 2600, etc.)"
  (case machine
    (:atari-2600 "a26")
    (:atari-5200 "a52")
    (:atari-7800 "a78")
    (:intellivision "int")
    (:colecoVision "col")
    (:nes "nes")
    (:commodore64 "d64")
    (:commodore128 "d64")
    (:vic20 "d64")
    (otherwise "a78")))

(defun game-emulator-binary-pathname (build region)
  "Return the pathname for the GAME binary with BUILD and REGION, using emulator extension."
  (let* ((machine (machine-directory-name))
         (ext (emulator-binary-extension machine))
         (build-str (string-downcase (or build "public")))
         (region-str (string-downcase (or region "ntsc")))
         (name (format nil "Phantasia.~a.~a.~a" build-str region-str ext)))
    (merge-pathnames (make-pathname :directory '(:relative "Dist" machine) :name name)
                     (uiop:getcwd))))

;; --- Build commands ---

(clim:define-command (com-make-test :command-table clim-internals::global-command-table
                                    :menu t :name t) ()
  "Build and run tests in a terminal window."
  (run-command-in-terminal-echo (list "make" "test")
                                :title "Running Tests"))

(clim:define-command (com-make-documentation :command-table clim-internals::global-command-table
                                             :menu t :name t) ()
  "Build documentation (PDF and HTML) in a terminal window."
  (run-command-in-terminal-echo (list "make" "doc")
                                :title "Building Documentation"))

(clim:define-command (com-make-game :command-table clim-internals::global-command-table
                                    :menu t :name t) ()
  "Build the game binary for the current machine/region in a terminal window."
  (let* ((build (or (get-pref :build) "Public"))
         (region (or (get-pref :region) "NTSC"))
         (machine (machine-directory-name))
         (target (format nil "Dist/~a/Phantasia.~a.~a.bin" machine build region)))
    (run-command-in-terminal-echo (list "make" target)
                                  :title (format nil "Building ~a" target))))

(clim:define-command (com-make-all :command-table clim-internals::global-command-table
                                   :menu t :name t) ()
  "Build everything: demo game, documentation, packages, music-book, map-book, cd-ready in a terminal window."
  (run-command-in-terminal-echo (list "make")
                                :title "Building All"))

;; --- Make menu ---

(clim:define-command-table resource-make-menu
  :menu (("Test..." :command com-make-test)
         ("Documentation..." :command com-make-documentation)
         ("Game..." :command com-make-game)
         (nil :divider :line)
         ("All..." :command com-make-all)))

(clim:define-command (com-send-to-sd :command-table clim-internals::global-command-table
                                     :menu t :name t) ()
  "Send the built binary to SD card for hardware testing."
  (let* ((build (or (get-pref :build) "Public"))
         (region (or (get-pref :region) "NTSC"))
         (bin-path (game-emulator-binary-pathname build region)))
    (if (probe-file bin-path)
        (progn
          (error "~&Sending ~a to SD card...~%" bin-path)
          (uiop:run-program (list "cp" bin-path "/media/*/SDCARD/" bin-path)
                            :output *standard-output* :error-output *standard-output*))
        (error "Binary not found: ~a. Build it first with Make > Game..." bin-path))))

(clim:define-command-table resource-core-dump-menu
  :menu (("Load /tmp/dump" :command com-load-dump-default)
         ("Load /tmp/dump2" :command com-load-dump2)
         (nil :divider :line)
         ("Load Last Minor Fault" :command com-load-last-minor-fault)
         ("Load Last Break" :command com-load-last-break)
         ("Load Last Failed Test" :command com-load-last-failed-test)
         (nil :divider :line)
         ("Load from file..." :command com-load-dump-from-file)))

(clim:define-command-table resource-core-display-menu
  :menu (("Show DLL from Dump..." :command com-show-dll-from-dump)
         ("Show Back Buffer DLL..." :command com-show-buffer-dll)
         ("Copy Dump as Dump2..." :command com-copy-dump-as-dump2)
         ("Compare DLLs from Dumps..." :command com-compare-dlls)
         (nil :divider :line)
         ("Show Animation Buffer..." :command com-show-animation-buffer)
         ("Show Decal..." :command com-show-decal)))

(clim:define-command-table resource-core-analysis-menu
  :menu (("Analyze Faults from Dump..." :command com-analyze-faults)
         ("Show Dialogue Buffers..." :command com-show-dialogue-buffers)
         ("Show Map..." :command com-show-map-from-dump)
         ("Show Sound System Info..." :command com-show-sound-system-info)
         ("Show All Stacks..." :command com-show-all-stacks)
         ("Show Forth Stack..." :command com-show-forth-stack)))

(clim:define-command-table resource-core-object-menu
  :menu (("Show Player Object..." :command com-show-player-object)
         ("Show Self Object..." :command com-show-self-object)
         ("Show All Objects..." :command com-show-all-objects)
         (nil :divider :line)
         ("Object Instance Inspector..." :command com-instance-inspector)
         ("Show Room for Objects..." :command com-show-room-for-objects)))

(clim:define-command-table resource-debug-menu
  :inherit-from (clim-internals::global-command-table)
  :menu (("Core Dump" :menu resource-core-dump-menu)
         ("Display" :menu resource-core-display-menu)
         ("Analysis" :menu resource-core-analysis-menu)
         ("Objects" :menu resource-core-object-menu)))

(clim:define-command-table resource-menu
  :menu (("New" :menu resource-new-menu)
         ("Accept from" :menu resource-accept-from-menu)
         ("Import JSON..." :command com-import-resource-json)
         (nil :divider :line)
         ("Save List as" :menu resource-save-list-as-menu)
         ("Send List to" :menu p2p-sharing-menu)
         ("Print List to" :menu printer-menu)
         (nil :divider :line)
         ("Quit" :command com-quit-skyline-tool :shortcut :ctrl-q)))

(clim:define-command-table resource-view-sort-menu
  :menu (("☐ Alphabetically" :command com-set-sort-alpha)
         ("☐ Numerically" :command com-set-sort-numeric)))

(clim:define-command-table resource-view-menu
  :menu (("Close all Groups" :command com-resource-close-all)
         ("Open all Groups" :command com-resource-open-all)
         (nil :divider :line)
         ("Sort By" :menu resource-view-sort-menu)
         (nil :divider :line)
         ("☐ Project Bar" :command com-toggle-project-bar)))

(clim:define-command-table resource-menu-bar
  :menu (("Resource" :menu resource-menu)
         ("Edit" :menu resource-edit-menu)
         ("Tools" :menu resource-tools-menu)
         ("Run" :menu resource-run-menu)
         ("Debug" :menu resource-debug-menu)
         ("Lisp" :menu resource-lisp-menu)
         ("View" :menu resource-view-menu)
         ("Help" :menu resource-help-menu)))

(clim:define-command-table resource-help-menu
  :inherit-from (clim-internals::global-command-table)
  :menu (("How to Manage Resources..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Fountain Scripting Manual..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table resource-accept-from-menu
  :inherit-from (clim-internals::global-command-table)
  :menu ())

(defun populate-accept-from-menu ()
  "Populate the Accept From menu with pending offers from each sender."
  #+ () (clim:remove-command-from-command-table 'resource-accept-from-menu :FIXME-command-table)
  (let ((pending-offers (pending-offers)))
    (when (plusp (hash-table-count pending-offers))
      (maphash (lambda (sender offers)
                 (let ((count (length offers)))
                   (clim:add-menu-item-to-command-table
                    'resource-accept-from-menu
                    (format nil "~a (~d)" sender count)
                    :command `(com-show-offers-window ,sender)
                    :after :end)))
               pending-offers))))

(clim:define-command-table p2p-sharing-menu
  :menu ())

(clim:define-command-table printer-menu
  :menu ())

;; --- Frame definition ---



(defun display-all-resources (frame pane)
  (declare (ignore frame))
  (clim:window-clear pane)
  (let ((*standard-output* pane)
        (clim-simple-echo::*echo-pane* pane))
    (show-all-resources-internal)))

(defun populate-resource-print-to-menu ()
  "Populate printer-menu with discovered printers for resource list printing."
  (ignore-errors
   (clim:remove-menu-item-from-command-table 'printer-menu "No printers found")
   (clim:remove-menu-item-from-command-table 'printer-menu "Default Printer (lpr)")
   (dolist (p (ignore-errors (discover-printers)))
     (ignore-errors
      (clim:remove-menu-item-from-command-table 'printer-menu p))))
  (let* ((printers (ignore-errors (discover-printers-with-names))))
    (if printers
        (dolist (pair printers)
          (let ((queue-name (car pair))
                (display-name (cdr pair)))
            (clim:add-menu-item-to-command-table
             'printer-menu display-name :command
             `(com-print-resource-list ,queue-name ,display-name)
             :after :end)))
        (clim:add-menu-item-to-command-table
         'printer-menu "Default Printer (lpr)"
         :command
         `(com-print-resource-list "lpr" "Default Printer")
         :after :end))))

(defun %generate-resource-list-text ()
  "Return a string of the resource list formatted by kind."
  (let* ((all (collect-all-resources))
         (kinds (make-hash-table :test 'equal)))
    (dolist (entry all)
      (let ((moniker (if (typep entry 'game-resource-asset)
                         (game-asset-moniker entry)
                         (game-resource-locator entry)))
            (kind-name (game-resource-kind entry)))
        (push moniker (gethash kind-name kinds))))
    (with-output-to-string (out)
      (dolist (kind '("Scripts" "Songs" "Maps" "Characters" "Boats" "Blobs"
                      "Tilesets" "Sprite Sheets" "Object Prototypes" "Classes"
                      "Routines" "Instruments" "Items" "Flags" "Keys"
                      "Translations"))
        (let ((items (gethash kind kinds)))
          (when items
            (setf items (sort (copy-list items) #'string-lessp))
            (format out "~&~a:~%" kind)
            (dolist (item items)
              (format out "  ~a~%" item))))))))

(clim:define-command (com-print-resource-list
                      :command-table clim-internals::global-command-table
                      :menu nil :name t)
    ((printer-queue 'string) (display-name 'string))
  (declare (ignore display-name))
  (let* ((text (%generate-resource-list-text))
         (lines (count #\Newline text))
         (total-pages (max 1 (ceiling lines (- 700 50))))
         (temp-ps (format nil "/tmp/resource-list-~a.ps" (get-universal-time)))
         (temp-pdf (format nil "/tmp/resource-list-~a.pdf" (get-universal-time)))
         (title (format nil "Resource List - ~a"
                        (string-capitalize
                         (or (ignore-errors (symbol-value '*game-title*)) "Game"))))
         (author (ignore-errors (user-real-name))))
    (when (zerop (length text))
      (format *query-io* "~&No resources to print.~%")
      (return-from com-print-resource-list))
    (with-open-file (ps temp-ps :direction :output :if-exists :supersede)
      (format ps "%!PS-Adobe-3.0~%")
      (write-ps-docinfo ps title "Skyline-Tool"
                        (format nil "~a on ~a" author (machine-instance)))
      (format ps "<< /PageSize [612 792] >> setpagedevice~%")
      (write-ps-font-encodings ps)
      (with-input-from-string (s text)
        (dotimes (page total-pages)
          (format ps "%%Page: ~d ~d~%" (1+ page) total-pages)
          (let ((y 700) (line-height 10))
            (loop for line = (read-line s nil nil)
                  while (and line (>= y 50))
                  do (format ps "50 ~d moveto (~a) show~%" y
                             (escape-ps-string line))
                     (decf y line-height)))
          (format ps "showpage~%"))))
    (uiop:run-program (list "ps2pdf" temp-ps temp-pdf)
                      :output nil :ignore-error-status t)
    (ignore-errors (delete-file temp-ps))
    (uiop:run-program (list "lp" "-d" printer-queue temp-pdf)
                      :output nil :ignore-error-status t)
    (ignore-errors (delete-file temp-pdf))
    (format *query-io* "~&Printed resource list to ~a~%" printer-queue)))
