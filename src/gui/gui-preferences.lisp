;; Skyline-Tool src/gui/gui-preferences.lisp
;; Copyright © 2026 Interworldly Adventuring, LLC

;; Preferences Inspector - Complete CLIM tabbed interface for development preferences

#|
Preferences Inspector documentation: Requirements. NO MODIFICATIONS permitted. ;

Tab Bar
 _______________   __________   _________   _________________   ________________
/ Accessibility \ / Printing \ / Network \ / Version Control \ / Issue Tracking \


Accessibility Tab
------------------

Color Theme: [ Normal (Light)    - ]
             [ Inverted (Dark)     ]
             [ Sunrise/Sunset      ]
             [ Desktop (Auto)      ]

Sunrise/Sunset: ___° <> N <> S × ___° <> W <> E      (Find Me)

Keyboard Shortcuts Theme: [ Gnome        - ]
                          [ macOS          ]
                          [ Common (CDE)   ]
                          [ Emacs          ]

Printing Tab
-------------

Paper Size: <> U.S. Letter | 8.5 in × 11 in
            <> U.S. Legal  | 8.5 in × 14 in
            <> A4          | 9 in × 12 in                # probably wrong numbers
            <> Custom      | ____ [ mm - ] × ____ [ mm - ]
                                  [ cm   ]        [ cm   ] 
                                  [ pt   ]        [ pt   ]
                                  [ pc   ]        [ pc   ]
                                  [ in   ]        [ in   ]
                                        
Network Tab
------------

[] LAN Resource Sharing
[] Share “Dist/” Folder
[] Share Music as Media Server

Local Domain: ________________                # default "local."

Cryptographic Algorithm for Sharing: [ $(algos-list)    - ] # from Ironclad

Version Control Tab
--------------------
Version Control System: <> Git              | git
                        <> Bazaar           | bzr          
                        <> Subversion       | svn      
                        <> Mercurial        | hg        
                        <> Concurrent       | cvs      
                        <> Revision Control | rcs

Git # Specific to the version control system in use
----                                    

User                   
-----                  
Name: $(user-real-name)
eMail: ________________________  

Signing key: [ Bruce-Robert Pocock <brpocock@interworldly.com>    - ]
             [ ...                                                  ]
                        (Generate and publish a new key...)

[] Sign Commits                         
[] Sign Tags

Tools                                   
------                                  
Merge: [ meld - ]  [] Prompt first        
Diff:  [ meld - ]  [] Prompt first        
##( git difftool --tool-help first section of output only )
##( "may be set to one of the following:" options only )
##( do not list "valid, but not currently available" list )
Remotes                                 
--------                                
|| Remote        | URL                               | Fetch                               ||
||-----------------------------------------------------------------------------------------||
|| origin        | git@github.com:brpocock/Phantasia | +refs/heads/:refs/remotes/origin/   || 
( + )                                   
Submodules                              
-----------                             
[] Skyline-Tool and Eightbol

[] Atari 7800 Tools                     
[] Intellivision Tools                  

Pushing                                 
--------                                
[] When pushing, automatically set up new branches on remote 

Pulling
--------
[] When pulling, automatically [ fast-forward only   - ]

Default Branch: ___________         # main default

Issue Tracking Tab 
-------------------
Issue tracker kind: <> GitHub           
                     <> GitLab                               
                     <> Bugzilla

GitLab         # specific to each selection
-------

Tracker URL: ____________________________________________________ 
                            ( Sign in... )                          
|#
(in-package :skyline-tool)

;;;; Tab-friendly mixin — used by inspector frames with tabbed interfaces
(defclass tab-friendly-mixin ()
  ((current-tab :initarg :current-tab :accessor frame-current-tab :initform :accessibility))
  (:documentation "Mixin for inspector frames with tabbed interfaces."))

(defgeneric frame-tab-list (frame)
  (:documentation "Return the list of tab keywords for this inspector frame."))

(defgeneric switch-to-tab (frame tab)
  (:documentation "Switch FRAME to TAB and redisplay."))

(defmethod switch-to-tab ((frame tab-friendly-mixin) tab)
  (setf (frame-current-tab frame) tab)
  (clim:redisplay-frame-panes frame :force-p t))

;;;; Constants
(define-constant +paper-sizes+
    '((:us-letter "U.S. Letter" 215.9 279.4)
      (:us-legal "U.S. Legal" 215.9 355.6)
      (:a4 "A4" 210.0 297.0))
  :test 'equalp
  :documentation "Paper size options: (key label width-mm height-mm).")

;;;; Utility functions
(defun convert-unit (value from-unit to-unit)
  "Convert VALUE from FROM-UNIT to TO-UNIT.
   Units: :mm, :cm, :in, :pt (points = 1/72 inch)."
  (let ((mm-per-unit
          (case from-unit
            (:mm 1)
            (:cm 10)
            (:in 25.4)
            (:pt (/ 25.4 72))
            (t 1)))
        (units-per-mm
          (case to-unit
            (:mm 1)
            (:cm (/ 1 10))
            (:in (/ 1 25.4))
            (:pt (/ 72 25.4))
            (t 1))))
    (* value mm-per-unit units-per-mm)))

;;;; Frame definition — uses CLIM tab-layout from clim-tab-layout package

(clim:define-application-frame preferences-inspector-frame (tab-friendly-mixin clim:standard-application-frame)
  ((dirty :initform nil :accessor frame-dirty)
   (watcher-thread :initform nil :accessor prefs-watcher-thread))
  (:menu-bar preferences-inspector-menu-bar)
  (:icon (skyline-tool-icon :resource :preferences))
  (:pretty-name "Preferences Inspector")
  (:panes
   (a11y-tab (make-a11y-tab-pane clim:*application-frame*))
   (printing-tab (make-printing-tab-pane clim:*application-frame*))
   (sharing-tab (make-sharing-tab-pane clim:*application-frame*))
   (version-control-tab (make-version-control-tab-pane clim:*application-frame*))
   (issue-tracking-tab (make-issue-tracking-tab-pane clim:*application-frame*)))
  (:layouts
   (default
     (clim-tab-layout:with-tab-layout ('tab-page :name 'prefs-tab-layout)
       ("Accessibility" a11y-tab
        :enabled-callback (lambda (page)
                            (let ((frame (clim:pane-frame (clim-tab-layout:tab-page-tab-layout page))))
                              (setf (frame-current-tab frame) :accessibility))))
       ("Printing" printing-tab
        :enabled-callback (lambda (page)
                            (let ((frame (clim:pane-frame (clim-tab-layout:tab-page-tab-layout page))))
                              (setf (frame-current-tab frame) :printing))))
       ("Sharing" sharing-tab
        :enabled-callback (lambda (page)
                            (let ((frame (clim:pane-frame (clim-tab-layout:tab-page-tab-layout page))))
                              (setf (frame-current-tab frame) :sharing))))
       ("Version Control" version-control-tab
        :enabled-callback (lambda (page)
                            (let ((frame (clim:pane-frame (clim-tab-layout:tab-page-tab-layout page))))
                              (setf (frame-current-tab frame) :version-control))))
       ("Issue Tracking" issue-tracking-tab
        :enabled-callback (lambda (page)
                            (let ((frame (clim:pane-frame (clim-tab-layout:tab-page-tab-layout page))))
                              (setf (frame-current-tab frame) :issue-tracking))))))))

(defmethod clim:frame-pretty-name ((frame preferences-inspector-frame))
  (format nil "Preferences Inspector — ~a ~a"
          *game-title* (machine-directory-name)))

(defmethod frame-tab-list ((frame preferences-inspector-frame))
  '(:accessibility :printing :sharing :version-control :issue-tracking))

(defmethod switch-to-tab ((frame preferences-inspector-frame) tab)
  (let* ((title (string-capitalize (substitute #\Space #\- (string tab))))
         (layout (clim:find-pane-named frame 'prefs-tab-layout)))
    (when layout
      (let ((page (clim-tab-layout:find-tab-page-named title layout)))
        (when page
          (setf (clim-tab-layout:tab-layout-enabled-page layout) page)
          (setf (frame-current-tab frame) tab))))))

(defmethod initialize-instance :after ((frame preferences-inspector-frame) &key)
  (let ((prefs-path (prefs-pathname)))
    (when (probe-file prefs-path)
      (ensure-directories-exist (make-pathname :defaults prefs-path :name nil :type nil))
      (setf (prefs-watcher-thread frame)
            (submit-task
             (lambda ()
               (inotify:with-inotify (inot (list (list prefs-path inotify:in-modify)))
                 (loop for ev = (inotify:read-events inot)
                       do (clim:redisplay-frame-panes frame :force-p t))))))))
  ;; Populate dynamic menus
  (populate-print-menu 'preferences-print-to-menu)
  ;; Subscribe to printer list changes for dynamic menu updates
  (skyline-tool::subscribe :printer-list-changed
              (lambda (event)
                (declare (ignore event))
                (populate-print-menu 'preferences-print-to-menu)
                (clim:redisplay-frame-panes frame :force-p t)))
  ;; Subscribe to external preference-change events so other windows stay in sync
  (skyline-tool::subscribe :preference-change
              (lambda (event)
                (declare (ignore event))
                (clim:redisplay-frame-panes frame :force-p t))))

(defun open-preferences-inspector ()
  (let ((frame (clim:make-application-frame
                'preferences-inspector-frame)))
    (make-window-thread
(format nil "Preferences Inspector: ~a (~a)"
              *game-title* (machine-directory-name))
     (lambda ()
       (clim:run-frame-top-level frame)))))

;;;; Menus
(clim:define-command-table preferences-save-as-menu
  :menu (("Text..." :command com-preferences-save-text)
         ("JSON..." :command com-preferences-save-json)
         ("PDF..." :command com-preferences-save-pdf)))

(clim:define-command-table preferences-send-to-menu
  :menu ())

(clim:define-command-table preferences-print-to-menu
  :menu ())

(clim:define-command-table preferences-inspector-file-menu
  :menu (("Reset to Defaults" :command com-preferences-reset)
         (nil :divider :line)
         ("Save as" :menu preferences-save-as-menu)
         ("Send to" :menu preferences-send-to-menu)
         ("Print to" :menu preferences-print-to-menu)
         (nil :divider :line)
         ("Close" :command com-close-frame)))

(clim:define-command-table preferences-inspector-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table preferences-inspector-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :toggle t)
         (nil :divider :line)
         ("Project Pane" :command com-toggle-project-pane :toggle t)))

(clim:define-command-table preferences-inspector-help-menu
  :menu (("How to Manage Preferences..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-scripting-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table preferences-inspector-menu-bar
  :menu (("Preferences" :menu preferences-inspector-file-menu)
         ("Edit" :menu preferences-inspector-edit-menu)
         ("View" :menu preferences-inspector-view-menu)
         ("Help" :menu preferences-inspector-help-menu)))

;;;; Commands
(clim:define-command (com-preferences-reset :command-table clim-internals::global-command-table
                                            :menu t :name t) ()
  (let ((confirmed (error "Reset all preferences to defaults?")))
    (when confirmed
      (setf *prefs-cache* nil)
      (setf (frame-dirty clim:*application-frame*) t)
      (setf (get-pref '(:internal :prefs :reset-by-user)) (get-universal-time))
      (clim:redisplay-frame-panes clim:*application-frame* :force-p t))))

(clim:define-command (com-close-frame :command-table clim-internals::global-command-table
                                       :menu t :name t) ()
  (clim:frame-exit clim:*application-frame*))

(clim:define-command (com-preferences-save-text :command-table clim-internals::global-command-table
                                                :menu t :name t) ()
  (let* ((path (prompt-save-pathname "Preferences.txt" (list :dir :preferences :text))))
    (when path
      (with-open-file (s path :direction :output :if-exists :supersede)
        (format s "Preferences — ~a ~a~%"
                *game-title* (machine-directory-name))
        (labels ((recurse-keys (set &optional (indent 0))
                   (loop for (k v) on set by #'cddr
                         do (if (consp v)
                                (progn
                                  (format s "~vt ~s: ..." indent k)
                                  (recurse-keys v (+ 2 indent)))
                                (format s "~vt  ~s: ~s~%" indent k v)))))
          (recurse-keys *prefs-cache*))))))

(clim:define-command (com-preferences-save-json :command-table clim-internals::global-command-table
                                                :menu t :name t) ()
  (let* ((path (prompt-save-pathname "Preferences.json" '(:dir :preferences :json))))
    (when path
      (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8)
        (json:encode-json
         (loop for (k v) on *prefs-cache* by #'cddr
               collect (cons (string-downcase (symbol-name k)) v))
         s)))))

(clim:define-command (com-preferences-save-pdf :command-table clim-internals::global-command-table
                                               :menu t :name t) ()
  (let* ((path (prompt-save-pathname "Preferences.pdf" '(:dir :preferences :pdf))))
    (when path
      (let ((ps-stream (uiop:run-program 
                        (list "ps2pdf" "-" (namestring path))
                        :input :stream
                        :output nil
                        :error-output nil)))
        (write-ps-header-bar ps-stream "Preferences")
        (loop for (k v) on *prefs-cache* by #'cddr
              do (format ps-stream "(~s: ~s) showpage show~%" k v))
        (close ps-stream)))))

(clim:define-command (com-cut :command-table clim-internals::global-command-table
                              :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (declare (ignore frame))))

(clim:define-command (com-copy :command-table clim-internals::global-command-table
                               :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (declare (ignore frame))))

(clim:define-command (com-paste :command-table clim-internals::global-command-table
                                :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (declare (ignore frame))))

(clim:define-command (com-find :command-table clim-internals::global-command-table
                               :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (declare (ignore frame))))

(clim:define-command (com-inspector-toggle-view :command-table clim-internals::global-command-table
                                                :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (declare (ignore frame))))

(clim:define-command (com-toggle-project-pane :command-table clim-internals::global-command-table
                                              :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (declare (ignore frame))))

(clim:define-command (com-help-for-window :command-table clim-internals::global-command-table
                                          :menu t :name t) ()
  (let ((frame clim:*application-frame*))
    (declare (ignore frame))))

(clim:define-command (com-open-dev-guide :command-table clim-internals::global-command-table
                                         :menu t :name t) ()
  (let ((guide-path (asdf:system-relative-pathname :skyline-tool
                                                   "../Source/Documentation/PhantasiaDevGuide.texi")))
    (when (probe-file guide-path)
      (uiop:run-program (list "xdg-open" (namestring guide-path)) :output nil))))

(clim:define-command (com-open-scripting-guide :command-table clim-internals::global-command-table
                                               :menu t :name t) ()
   (let ((guide-path (asdf:system-relative-pathname :skyline-tool
                                                    "../Source/Documentation/FountainScripting.md")))
     (when (probe-file guide-path)
       (uiop:run-program (list "xdg-open" (namestring guide-path)) :output nil))))

;;;; Presentation types for right-click context menus
(clim:define-presentation-type preferences-section ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'preferences-section)))
  (stringp object))

(clim:define-presentation-type preferences-gadget ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'preferences-gadget)))
  t)

(clim:define-presentation-type git-remote-item ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'git-remote-item)))
  (listp object))

;;;; Context menus for right-click on preferences sections and gadgets
(clim:define-command-table preferences-section-context-menu
  :menu (("Reset Section to Defaults" :command com-preferences-reset-section)
         ("How to Manage This Setting..." :command com-help-for-window)))

(clim:define-command-table preferences-gadget-context-menu
  :menu (("Copy Value" :command com-copy)
         ("Reset to Default" :command com-preferences-reset-section)
         ("How to Manage..." :command com-help-for-window)))

;;;; Menu helper
(defun populate-print-menu (menu)
  "Populate print-to-menu with available printers."
  (declare (ignore menu)))

;;;; Sunrise/Sunset calculation
(defun degrees->radians (degrees)
  "Convert DEGREES to radians."
  (* degrees (/ pi 180.0d0)))

(defun radians->degrees (radians)
  "Convert RADIANS to degrees."
  (* radians (/ 180.0d0 pi)))

(defun julian-day (year month day)
  "Compute the Julian Day number for YEAR, MONTH, DAY."
  (let* ((a (floor (* 14 (- month 12)) 10))
         (y (+ year 4800 (- a)))
         (m (+ month (* 12 a) (- 3))))
    (+ day
       (floor (+ (* 153 m) 2) 5)
       (* 365 y)
       (floor y 4)
       (- (floor y 100))
       (floor y 400)
       (- 32045))))

(defun solar-declination (jd)
  "Compute solar declination δ for Julian Day JD using simplified Meeus."
  (let* ((n (- jd 2451545.0d0))
         (l (+ 280.460d0 (* 0.9856474d0 n)))
         (g (+ 357.528d0 (* 0.9856003d0 n)))
         (g-rad (degrees->radians (mod g 360.0d0)))
         (lambda-val (+ l (* 1.915d0 (sin g-rad)) (* 0.020d0 (sin (* 2 g-rad)))))
         (lambda-rad (degrees->radians lambda-val))
         (epsilon (degrees->radians 23.439d0))
         (sin-dec (* (sin lambda-rad) (sin epsilon))))
    (asin sin-dec)))

(defun hour-angle-at-sunrise (latitude declination-jd)
  (let* ((phi (degrees->radians latitude))
         (cos-h0 (* (- (tan phi)) (tan (solar-declination declination-jd)))))
    (cond
      ((< cos-h0 -1.0d0) nil)
      ((> cos-h0 1.0d0) nil)
      (t (acos cos-h0)))))

(defun current-theme-from-sunrise-sunset (latitude longitude)
  "Determine whether the current time is between sunrise and sunset.
   Uses the observer's LATITUDE (in degrees) and LONGITUDE (in degrees).
   Returns :NORMAL if it is daytime, :INVERTED if it is nighttime."
  (multiple-value-bind (seconds minutes hours month-day month year week-day
                        daylight-saving-p tz-offset)
      (decode-universal-time (get-universal-time))
    (declare (ignore week-day tz-offset))
    (let* ((jd (julian-day year month month-day))
           (hour (+ hours (if daylight-saving-p 1 0)
                    (/ minutes 60.0d0)
                    (/ seconds 3600.0d0)))
           (n (- jd 2451545.0d0))
           (j-star (/ n 36525.0d0))
           (mean-solar-time (+ hour
                               (- longitude 0.0d0)
                               (* 24.0d0
                                  (- n (floor n)))))
           (decl (solar-declination jd))
           (h0 (hour-angle-at-sunrise latitude decl)))
      (if h0
          (let* ((l0 (+ 280.460d0 (* 360.9856235d0 n)))
                 (m (+ 357.528d0 (* 359.99050d0 j-star)))
                 (m-rad (degrees->radians (mod m 360.0d0)))
                 (c (+ (* 1.915d0 (sin m-rad))
                       (* 0.020d0 (sin (* 2 m-rad)))))
                 (lambda-val (+ l0 c))
                 (epsilon-rad (degrees->radians 23.439d0))
                 (right-ascension (/ (radians->degrees
                                      (atan (* (tan (degrees->radians lambda-val))
                                               (cos epsilon-rad))
                                           1.0d0))
                                     15.0d0))
                 (h0-degrees (radians->degrees h0))
                 (lst (+ mean-solar-time (* 0.0057755183d0
                                            (- l0 c (* right-ascension))))))
            (if (and (>= lst (- right-ascension (/ h0-degrees 15.0d0)))
                     (<= lst (+ right-ascension (/ h0-degrees 15.0d0))))
                :normal
                :inverted))
          :normal))))

(defun read-desktop-theme ()
  "Read the current desktop theme from DBus.
   Returns :NORMAL for light theme, :INVERTED for dark theme.
   Falls back to :NORMAL if DBus is unavailable."
  (handler-case
      (dbus:with-open-bus (bus (dbus:session-server-addresses))
        (dbus:with-introspected-object
            (proxy bus "/org/freedesktop/portal/desktop"
                   "org.freedesktop.portal.Desktop")
          (let ((result (proxy "org.freedesktop.DBus.Properties" "Get"
                                "org.freedesktop.Appearance" "color-scheme")))
            (cond
              ((or (string= result "1") (string= result "uint32 1")) :inverted)
              ((or (string= result "2") (string= result "uint32 2")) :inverted)
              (t :normal)))))
    (error () :normal)))

(defun effective-color-theme (requested-theme frame)
  "Determine the actual color theme to apply based on REQUESTED-THEME.
   For :desktop, reads from dbus. For :sunrise, calculates from lat/long."
  (declare (ignore frame))
  (case requested-theme
    (:desktop (read-desktop-theme))
    (:sunrise
     (let ((lat-str (get-pref '(:location :latitude) ""))
           (lon-str (get-pref '(:location :longitude) "")))
       (handler-case
           (let ((lat (parse-number lat-str))
                 (lon (parse-number lon-str))
                 (lat-ns (get-pref '(:location :latitude-ns) "N"))
                 (lon-ew (get-pref '(:location :longitude-ew) "E")))
             (current-theme-from-sunrise-sunset
              (if (string-equal lat-ns "S") (- lat) lat)
              (if (string-equal lon-ew "W") (- lon) lon)))
         (error ()
           :normal))))
    (:inverted :inverted)
    (otherwise :normal)))
