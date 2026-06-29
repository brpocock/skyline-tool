(defpackage clim-simple-echo
  (:use :clim :clim-lisp :clim-extensions)
  (:export #:run-in-simple-echo
           #:capturing-stream
           #:capturing-target
           #:frame-captured-text))

(in-package :clim-simple-echo)

(defclass echo-view (textual-view) ())

(define-presentation-method present :around
  ((object sequence) (type sequence) stream (view echo-view)
                     &key acceptably for-context-type)
  (present object 'expression :stream stream :view view
                              :acceptably acceptably :for-context-type for-context-type))

;; Macro to delegate a clim-simple-echo command to the same-named
;; command in skyline-tool, without writing package prefixes in source.
(defmacro define-echo-delegate (name &rest args)
  (let ((target (intern (string name) :skyline-tool)))
    `(clim:define-command (,name :command-table clim-internals::global-command-table
                                :menu nil :name t)
         ,(if args
              `(,(first (first args)) ,(second (first args)))
              '())
       (,target ,@(when args `(,(first (first args))))))))

(define-echo-delegate com-run-repl)
(define-echo-delegate com-show-lisp-room)
(define-echo-delegate com-reload-skyline-tool-from-sources)

(define-echo-delegate com-show-rom-budget)
(define-echo-delegate com-anim-seq-editor)
(define-echo-delegate com-assign-animation-sequences)
(define-echo-delegate com-run-tiled)
(define-echo-delegate com-open-file-manager)
(define-echo-delegate com-push-binary-to-7800gd)
(define-echo-delegate com-shove-binary-into-7800gd)

(define-echo-delegate com-show-dll-from-dump)
(define-echo-delegate com-show-buffer-dll)
(define-echo-delegate com-copy-dump-as-dump2)
(define-echo-delegate com-compare-dlls)
(define-echo-delegate com-show-animation-buffer)
(define-echo-delegate com-show-decal)
(define-echo-delegate com-analyze-faults)
(define-echo-delegate com-show-dialogue-buffers)
(define-echo-delegate com-show-map-from-dump)
(define-echo-delegate com-show-sound-system-info)
(define-echo-delegate com-show-all-stacks)
(define-echo-delegate com-show-forth-stack)
(define-echo-delegate com-show-player-object)
(define-echo-delegate com-show-self-object)
(define-echo-delegate com-show-all-objects)
(define-echo-delegate com-show-room-for-objects)

(define-echo-delegate com-edit-project.json)
(define-echo-delegate com-edit-skyline-config-prefs)
(define-echo-delegate com-reload-assets-index)
(define-echo-delegate com-rescan-project-folder)

(define-echo-delegate com-help-for-window)
(define-echo-delegate com-open-scripting-guide)

(define-echo-delegate com-set-region-ntsc)
(define-echo-delegate com-set-region-pal)
(define-echo-delegate com-set-build-demo)
(define-echo-delegate com-set-build-public)
(define-echo-delegate com-set-build-publisher)
(define-echo-delegate com-run-in-a7800)

(define-echo-delegate com-load-dump-default)
(define-echo-delegate com-load-dump2)
(define-echo-delegate com-load-dump-from-file)

(define-command-table echo-save-as-menu
  :menu (("Text..." :command com-save-text)
         ("PDF..." :command com-print-pdf)
         ("JSON..." :command com-save-as-json)))

(define-command-table echo-print-to-menu
  :menu ())

(define-command-table echo-regenerate-builds-menu
  :menu (("NTSC Demo" :command com-regenerate-ntsc-demo)
         ("NTSC Public" :command com-regenerate-ntsc-public)
         ("NTSC Publisher" :command com-regenerate-ntsc-publisher)
         (nil :divider :line)
         ("PAL Demo" :command com-regenerate-pal-demo)
         ("PAL Public" :command com-regenerate-pal-public)
         ("PAL Publisher" :command com-regenerate-pal-publisher)))

(define-command-table echo-file-menu
  :menu (("Save As" :menu echo-save-as-menu)
         ("Print To" :menu echo-print-to-menu)
         (nil :divider :line)
         ("Regenerate" :command com-regenerate)
         (nil :divider :line)
         ("Close" :command com-close-echo)))

(define-command-table echo-edit-menu
  :menu (("Copy" :command com-copy-clipboard)
         (nil :divider :line)
         ("Find..." :command com-find-in-echo)))

(define-command-table echo-help-menu
  :menu (("How to Use This Window" :command com-help-for-window)
         ("Skyline-Tool Developers' Guide" :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide" :command com-open-scripting-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-echo)))

(define-application-frame simple-echo (standard-application-frame)
  ((pipe :initarg :pipe :reader frame-pipe)
   (captured-text :initform nil :accessor frame-captured-text)
   (pdf-function :initform nil :initarg :pdf-function :accessor frame-pdf-function))
  (:panes (echo-container :application :height 1000 :width 1000
                                          :display-function 'echo-echo))
  (:command-table (simple-echo))
  (:menu-bar echo-menu-bar)
  (:icon (skyline-tool::skyline-tool-icon))
  (:layouts (default echo-container)))

(defmethod initialize-instance :after ((frame simple-echo) &key)
  (ignore-errors (populate-echo-print-menu frame)))

(define-command-table simple-echo
  :inherit-from (clim-internals::global-command-table)
  :menu (("Report" :menu echo-file-menu) ("Edit" :menu echo-edit-menu) ("Help" :menu echo-help-menu)))

(define-command-table echo-menu-bar
  :menu (("Report" :menu echo-file-menu) ("Edit" :menu echo-edit-menu) ("Help" :menu echo-help-menu)))

(define-command-table echo-resource-new-menu
  :menu (("BLOB" :command com-new-blob)
         ("Character" :command com-new-character)
         ("Map" :command com-new-map)
         ("Object Prototype" :command com-new-object-prototype)
         ("Script" :command com-new-script-from-menu)
         ("Song" :command com-new-song)
         (nil :divider :line)
         ("Sprite Sheet" :command com-new-sprite-sheet)
         ("Tileset" :command com-new-tileset)))

(define-command-table echo-resource-save-list-as-menu
  :menu (("JSON..." :command com-save-as-json)
         ("Text..." :command com-save-text)
         ("Spreadsheet..." :command com-save-spreadsheet)
         ("PDF..." :command com-print-pdf)))

(define-command-table echo-resource-lisp-menu
  :inherit-from (clim-internals::global-command-table)
  :menu (("Run REPL..." :command com-run-repl)
         ("Show Lisp Room..." :command com-show-lisp-room)
         (nil :divider :line)
         ("Reload Skyline-Tool from Sources..." :command com-reload-skyline-tool-from-sources)))

(define-command-table echo-resource-a7800-region-menu
  :menu (("NTSC" :command com-set-region-ntsc)
         ("PAL" :command com-set-region-pal)))

(define-command-table echo-resource-a7800-build-menu
  :menu (("Demo" :command com-set-build-demo)
         ("Public" :command com-set-build-public)
         ("Publisher" :command com-set-build-publisher)))

(define-command-table echo-resource-a7800-menu
  :menu (("Region" :menu echo-resource-a7800-region-menu)
         ("Build" :menu echo-resource-a7800-build-menu)
         (nil :divider :line)
         ("Run in A7800..." :command com-run-in-a7800)))

(define-command-table echo-resource-tools-menu
  :inherit-from (clim-internals::global-command-table)
  :menu (("Show ROM Budget..." :command com-show-rom-budget)
         ("Animation Sequence Editor..." :command com-anim-seq-editor)
         ("Assign Animation Sequences..." :command com-assign-animation-sequences)
         (nil :divider :line)
         ("Run in Tiled..." :command com-run-tiled)
         ("Open File Manager..." :command com-open-file-manager)
         (nil :divider :line)
         ("A7800" :menu echo-resource-a7800-menu)
         (nil :divider :line)
         ("Push Binary to 7800 Game Drive..." :command com-push-binary-to-7800gd)
         ("Shove into Running 7800 Game Drive..." :command com-shove-binary-into-7800gd)))

(define-command-table echo-resource-core-dump-menu
  :menu (("Load /tmp/dump" :command com-load-dump-default)
         ("Load /tmp/dump2" :command com-load-dump2)
         ("Load from file..." :command com-load-dump-from-file)))

(define-command-table echo-resource-debug-menu
  :inherit-from (clim-internals::global-command-table)
  :menu (("Core Dump" :menu echo-resource-core-dump-menu)
         (nil :divider :line)
         ("Show DLL from Dump..." :command com-show-dll-from-dump)
         ("Show Back Buffer DLL..." :command com-show-buffer-dll)
         ("Copy Dump as Dump2..." :command com-copy-dump-as-dump2)
         ("Compare DLLs from Dumps..." :command com-compare-dlls)
         (nil :divider :line)
         ("Show Animation Buffer..." :command com-show-animation-buffer)
         ("Show Decal..." :command com-show-decal)
         (nil :divider :line)
         ("Analyze Faults from Dump..." :command com-analyze-faults)
         ("Show Dialogue Buffers..." :command com-show-dialogue-buffers)
         ("Show Map..." :command com-show-map-from-dump)
         ("Show Sound System Info..." :command com-show-sound-system-info)
         ("Show All Stacks..." :command com-show-all-stacks)
         ("Show Forth Stack..." :command com-show-forth-stack)
         (nil :divider :line)
         ("Show Player Object..." :command com-show-player-object)
         ("Show Self Object..." :command com-show-self-object)
         ("Show All Objects..." :command com-show-all-objects)
         ("Show Room for Objects..." :command com-show-room-for-objects)))

(define-command-table echo-resource-menu
  :menu (("Edit Project..." :command com-edit-project.json :shortcut :ctrl-x-p)
         ("Edit Preferences..." :command com-edit-skyline-config-prefs :shortcut :ctrl-comma)
         (nil :divider :line)
         ("New" :menu echo-resource-new-menu)
         ("Import JSON..." :command com-import-resource-json)
         (nil :divider :line)
         ("Reload Assets.index" :command com-reload-assets-index)
         ("Rescan Project Folder" :command com-rescan-project-folder)
         (nil :divider :line)
         ("Save List As" :menu echo-resource-save-list-as-menu)
         (nil :divider :line)
         ("Close" :command com-close-echo :shortcut :ctrl-w)))

(define-command-table echo-resource-menu-bar
  :menu (("Resource" :menu echo-resource-menu)
         ("Edit" :menu echo-edit-menu)
         ("Lisp" :menu echo-resource-lisp-menu)
         ("Tools" :menu echo-resource-tools-menu)
         ("Debug" :menu echo-resource-debug-menu)
         ("Help" :menu echo-help-menu)))

;; --- New resource commands ---

(define-simple-echo-command (com-new-blob :menu nil :name t) ()
  (let* ((name (clim:accept 'string :prompt "BLOB name" :default ""))
         (machine (or (ignore-errors (skyline-tool::machine-directory-name)) "7800"))
         (path (make-pathname :directory `(:relative "Source" "Blobs" ,machine)
                              :name name :type "xcf")))
    (when (plusp (length name))
      (ensure-directories-exist path)
      (with-open-file (f path :direction :output :if-exists :supersede)
        (format f ""))
      (format *query-io* "~&Created ~a~%" (namestring (truename path)))
      (uiop:run-program (list "gimp" (namestring (truename path)))
                        :output nil :ignore-error-status t))))

(define-simple-echo-command (com-new-character :menu nil :name t) ()
  (let* ((name (clim:accept 'string :prompt "Character name" :default ""))
         (path (make-pathname :directory '(:relative "Source" "Objects")
                              :name name :type "json")))
    (when (plusp (length name))
      (ensure-directories-exist path)
      (with-open-file (f path :direction :output :if-exists :supersede
                               :external-format :utf-8)
        (format f "~a" (json:encode-json-to-string
                         (list (cons "Class" "Player")
                               (cons "CharacterName" name)))))
    (format *query-io* "~&Created ~a~%" (namestring (truename path)))
    (uiop:run-program (list "emacsclient" "-n" (namestring (truename path)))
                      :output nil :ignore-error-status t))))

(define-simple-echo-command (com-new-map :menu nil :name t) ()
  (let* ((name (clim:accept 'string :prompt "Map name" :default ""))
         (path (make-pathname :directory '(:relative "Source" "Maps")
                              :name name :type "tmx")))
    (when (plusp (length name))
      (ensure-directories-exist path)
      (with-open-file (f path :direction :output :if-exists :supersede
                              :external-format :utf-8)
        (format f "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
        (format f "<map version=\"1.0\" tiledversion=\"1.3.5\" orientation=\"orthogonal\"~%")
        (format f "     renderorder=\"right-down\" width=\"10\" height=\"10\"~%")
        (format f "     tilewidth=\"16\" tileheight=\"16\" infinite=\"0\"~%")
        (format f "     nextlayerid=\"2\" nextobjectid=\"1\">~%")
        (format f " <layer id=\"1\" name=\"Tile Layer 1\" width=\"10\" height=\"10\">~%")
        (format f "  <data encoding=\"csv\">~%")
        (dotimes (y 10)
          (format f "~{0~^,~}~%" (make-list 10 :initial-element 0)))
        (format f "</data>~%")
        (format f " </layer>~%")
        (format f "</map>~%"))
      (format *query-io* "~&Created ~a~%" (namestring (truename path)))
      (uiop:run-program (list "tiled" (namestring (truename path)))
                        :output nil :ignore-error-status t))))

(define-simple-echo-command (com-new-object-prototype :menu nil :name t) ()
  (let* ((name (clim:accept 'string :prompt "Class name" :default ""))
         (path (make-pathname :directory '(:relative "Source" "Objects")
                              :name name :type "json")))
    (when (plusp (length name))
      (ensure-directories-exist path)
(with-open-file (f path :direction :output :if-exists :supersede
                           :external-format :utf-8)
      (format f "~a" (json:encode-json-to-string
                       (list (cons "Class" "Player")
                             (cons "CharacterName" name)))))
      (format *query-io* "~&Created ~a~%" (namestring (truename path)))
      (uiop:run-program (list "emacsclient" "-n" (namestring (truename path)))
                        :output nil :ignore-error-status t))))

(define-simple-echo-command (com-new-script-from-menu :menu nil :name t) ()
  (let* ((name (clim:accept 'string :prompt "Script name" :default ""))
         (path (make-pathname :directory '(:relative "Source" "Scripts")
                              :name name :type "fountain")))
    (when (plusp (length name))
      (ensure-directories-exist path)
      (with-open-file (f path :direction :output :if-exists :supersede
                              :external-format :utf-8)
        (format f ""))
      (format *query-io* "~&Created ~a~%" (namestring (truename path)))
      (uiop:run-program (list "emacsclient" "-n" (namestring (truename path)))
                        :output nil :ignore-error-status t))))

(define-simple-echo-command (com-new-song :menu nil :name t) ()
  (let* ((name (clim:accept 'string :prompt "Song name" :default ""))
         (path (make-pathname :directory '(:relative "Source" "Songs")
                              :name name :type "mscz")))
    (when (plusp (length name))
      (ensure-directories-exist path)
      (let ((tmpdir (format nil "/tmp/mscz-~d" (get-universal-time))))
        (ensure-directories-exist (pathname tmpdir))
        (with-open-file (f (format nil "~a/~a.mscx" tmpdir name)
                           :direction :output :if-exists :supersede
                           :external-format :utf-8)
          (format f "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
          (format f "<museScore version=\"3.02\">~%")
          (format f "  <Score>~%")
          (format f "    <Staff>~%")
          (format f "      <StaffType group=\"pitched\"/>~%")
          (format f "    </Staff>~%")
          (format f "    <trackCount>1</trackCount>~%")
          (format f "  </Score>~%")
          (format f "</museScore>~%"))
        (uiop:run-program (list "zip" "-j" (namestring path)
                                (format nil "~a/~a.mscx" tmpdir name))
                          :output nil :ignore-error-status t)
        (uiop:run-program (list "rm" "-rf" tmpdir)
                          :output nil :ignore-error-status t))
      (format *query-io* "~&Created ~a~%" (namestring (truename path)))
      (uiop:run-program (list "musescore" (namestring (truename path)))
                        :output nil :ignore-error-status t))))

(define-simple-echo-command (com-new-sprite-sheet :menu nil :name t) ()
  (format *query-io* "~&New Sprite Sheet: not yet available.~%"))

(define-simple-echo-command (com-new-tileset :menu nil :name t) ()
  (format *query-io* "~&New Tileset: not yet available.~%"))

(define-simple-echo-command (com-import-resource-json :menu nil :name t) ()
  (format *query-io* "~&Import JSON: not yet implemented.~%"))

;; --- Helper: default save directory ---

(defvar *last-export-directory* nil
  "Most recently used export directory (pathname or NIL).")

(defun default-save-directory ()
  "Return the preferred directory for saving exports:
   most-recently-used, ~/work, ~/Work, or ~/Documents."
  (or *last-export-directory*
      (and (probe-file (merge-pathnames #p"work/" (user-homedir-pathname)))
           (merge-pathnames #p"work/" (user-homedir-pathname)))
      (and (probe-file (merge-pathnames #p"Work/" (user-homedir-pathname)))
           (merge-pathnames #p"Work/" (user-homedir-pathname)))
      (merge-pathnames #p"Documents/" (user-homedir-pathname))))

(defun window-title->filename (frame extension)
  "Convert a frame's pretty name to a filesystem-safe filename with EXTENSION.
   e.g. \"About Skyline-Tool\" + \"pdf\" → \"About Skyline-Tool.pdf\"."
  (let* ((title (or (ignore-errors (frame-pretty-name frame)) "Output"))
         (safe (remove-if (lambda (c) (find c "\\/:*?\"<>|" :test #'char=)) title)))
    (format nil "~a.~a" safe extension)))

;; --- Commands for simple-echo ---

(define-simple-echo-command (com-save-text :menu nil :name t) ()
  (let* ((text (frame-captured-text *application-frame*))
         (frame *application-frame*)
         (save-dir (default-save-directory))
         (default-name (namestring
                        (merge-pathnames
                         (window-title->filename frame "txt")
                         save-dir)))
         (path (%zenity-or-clim "Save As..." default-name)))
    (when (and path (plusp (length path)))
      (let ((p (string-trim '(#\Newline #\Space #\Tab) path)))
        (setf *last-export-directory*
              (make-pathname :defaults p :name nil :type nil))
        (with-open-file (f p :direction :output :if-exists :supersede
                           :external-format :utf-8)
          (princ text f))
        (format *query-io* "~&Saved ~a (~d bytes).~%" p (length text))))))

(define-simple-echo-command (com-save-spreadsheet :menu nil :name t) ()
  "Save the asset list as a CSV spreadsheet."
  (let* ((frame *application-frame*)
         (text (frame-captured-text frame))
         (frame-name (ignore-errors (clim:frame-pretty-name frame)))
         (default-name (format nil "~a.csv" (or frame-name "output")))
         (path (%zenity-or-clim "Save As Spreadsheet..." default-name)))
    (when path
      (with-open-file (f path :direction :output :if-exists :supersede
                           :external-format :utf-8)
        (format f "Kind,Moniker,Asset ID,Hex ID,Builds,Present,Full Path~%")
        (let ((all-assets (collect-all-assets)))
          (dolist (entry all-assets)
            (destructuring-bind (moniker builds kind-name asset-id hex-str present-p full-path)
                entry
              (format f "~a,~a,~a,~a,~a,~a,~a~%"
                      kind-name moniker (or asset-id "") (or hex-str "")
                      (or (format nil "~{~a~^ ~}" builds) "") (if present-p "Yes" "No")
                      (or full-path ""))))))
      (format *query-io* "~&Saved spreadsheet to ~a~%" path))))

(defun %zenity-or-clim (title default-name)
  "Prompt for a filename using zenity (if available) or CLIM's accept."
  (let ((zenity-out (ignore-errors
                     (string-trim '(#\Newline #\Space)
                      (uiop:run-program (list "zenity" "--file-selection" "--save"
                                              (format nil "--filename=~a" default-name)
                                              (format nil "--title=~a" title))
                                        :output :string :ignore-error-status t)))))
    (if (and zenity-out (plusp (length zenity-out)))
        zenity-out
        (let ((path (clim:accept 'pathname :prompt title :default default-name)))
          (when path (namestring path))))))

(define-simple-echo-command (com-print-pdf :menu nil :name t) ()
  (let* ((frame *application-frame*)
         (pdf-fn (frame-pdf-function frame))
         (save-dir (default-save-directory))
         (default-name (namestring
                        (merge-pathnames
                         (window-title->filename frame "pdf")
                         save-dir)))
         (pdf-path (%zenity-or-clim "Save As PDF..." default-name)))
    (when pdf-path
      (let* ((base (pathname-name pdf-path))
             (dir (make-pathname :defaults pdf-path :name nil :type nil))
             (ps-path (merge-pathnames (make-pathname :name base :type "ps") dir))
             (pdf-final (merge-pathnames (make-pathname :name base :type "pdf") dir)))
        (if pdf-fn
            ;; Custom PDF generator (e.g. Assets Index)
            (progn
              (funcall pdf-fn ps-path)
              (uiop:run-program (list "ps2pdf" (namestring ps-path) (namestring pdf-final))
                                :output nil :ignore-error-status t)
              (ignore-errors (delete-file ps-path))
              (format *query-io* "~&Saved ~a~%" (namestring pdf-final))
              (uiop:run-program (list "xdg-open" (namestring pdf-final))
                                :output nil :ignore-error-status t))
            ;; Text-based PDF generation
            (let* ((text (frame-captured-text frame)))
              (unless (and text (stringp text) (plusp (length text)))
                (format *query-io* "~&No content to save (frame text is empty).~%")
                (return-from com-print-pdf))
              (let* ((frame-name (ignore-errors (clim:frame-pretty-name frame)))
                     (game-title (string-capitalize
                                  (or (ignore-errors (symbol-value 'skyline-tool::*game-title*)) "unknown")))
                     (title (or frame-name (format nil "Skyline-Tool: ~a" game-title)))
                     (author (ignore-errors (skyline-tool::user-real-name)))
                     (hostname (machine-instance))
                     (date-str (multiple-value-bind (s m h d mo y) (get-decoded-time)
                                 (declare (ignore s))
                                 (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" y mo d h m)))
                     (lines (with-input-from-string (s text)
                              (loop for l = (read-line s nil nil) while l count l)))
                     (lines-per-page (max 1 (floor (- 680 80) 10)))
                     (total-pages (max 1 (ceiling lines lines-per-page))))
                (with-open-file (ps ps-path :direction :output :if-exists :supersede
                                            :external-format :utf-8)
                  (format ps "%!PS-Adobe-3.0~%")
                  (skyline-tool::write-ps-docinfo ps title "Skyline-Tool"
                                                  (format nil "~a on ~a" author (machine-instance)))
                  (format ps "<< /PageSize [612 792] >> setpagedevice~%")
                  (skyline-tool::write-ps-font-encodings ps)
                  (with-input-from-string (s text)
                    (dotimes (page total-pages)
                      (format ps "%%Page: ~d ~d~%" (1+ page) total-pages)
                      (skyline-tool::write-ps-header-bar ps title date-str author game-title)
                      (format ps "/Times-Roman-ISOLatin1 findfont 9 scalefont setfont 0 0 0 setrgbcolor~%")
                      (let ((y 680) (line-height 10) (bar-w 108) (bar-h 8))
                        (declare (ignore bar-w))
                        (loop for line = (read-line s nil nil)
                              while (and line (>= y 80))
                              do (let ((bracket-pos (position #\[ line))
                                       (pct-pos (position #\% line)))
                                   (cond
                                     ((and bracket-pos pct-pos (find #\] line :start bracket-pos)
                                           (> (length line) (+ bracket-pos 10)))
                                      (let* ((end-bracket (position #\] line :start bracket-pos))
                                             (pct-str (string-trim " " (subseq line (1+ end-bracket))))
                                             (pct (ignore-errors (parse-integer pct-str :junk-allowed t)))
                                             (bar-x 350) (bar-w 108)
                                             (pct (or pct 0)))
                                        (let ((label (string-trim " " (subseq line 0 bracket-pos))))
                                          (format ps "50 ~d moveto (~a) show~%" y
                                                  (skyline-tool::escape-ps-string label)))
                                        (format ps "gsave newpath ~d ~d ~d ~d rectstroke 0.7 0.85 1.0 setrgbcolor fill grestore~%"
                                                bar-x y bar-w bar-h)
                                        (when (> pct 0)
                                          (let ((fill-w (max 1 (round (* bar-w (/ pct 100))))))
                                            (format ps "gsave newpath ~d ~d ~d ~d rectfill 0.0 0.0 0.4 setrgbcolor grestore~%"
                                                    bar-x y fill-w bar-h)
                                            (format ps " ~d ~d moveto (~d%) show~%" (+ bar-x bar-w 5) y pct)))))
                                     ((and bracket-pos pct-pos (> bracket-pos 20))
                                      (let* ((end-bracket (position #\] line :start bracket-pos))
                                             (pct-str (string-trim " " (subseq line (1+ end-bracket))))
                                             (pct (ignore-errors (parse-integer pct-str :junk-allowed t)))
                                             (pct (or pct 0))
                                             (bar-x 50) (bar-w 512) (bar-h 16))
                                        (format ps "gsave newpath ~d ~d ~d ~d rectstroke 0.7 0.85 1.0 setrgbcolor fill grestore~%"
                                                bar-x y bar-w bar-h)
                                        (when (> pct 0)
                                          (let ((fill-w (max 1 (round (* bar-w (/ pct 100))))))
                                            (format ps "gsave newpath ~d ~d ~d ~d rectfill 0.0 0.0 0.4 setrgbcolor grestore~%"
                                                    bar-x y fill-w bar-h)
                                            (format ps " ~d ~d moveto (~d%) show~%" (+ bar-x bar-w 5) y pct)))))
                                     (t
                                      (format ps "50 ~d moveto (~a) show~%" y
                                              (skyline-tool::escape-ps-string line))))
                                   (decf y line-height)))
                        (skyline-tool::write-ps-page-footer ps (1+ page) total-pages game-title date-str author hostname)
                        (format ps "showpage~%")))))
                (uiop:run-program (list "ps2pdf" (namestring ps-path) (namestring pdf-final))
                                  :output nil :ignore-error-status t)
                (ignore-errors (delete-file ps-path))
                (format *query-io* "~&Saved ~a~%" (namestring pdf-final))
                (uiop:run-program (list "xdg-open" (namestring pdf-final))
                                  :output nil :ignore-error-status t))))))))

(defun %framed-text-content ()
  "Return the captured text of the current echo frame, or nil."
  (and (boundp '*application-frame*)
       *application-frame*
       (typep *application-frame* 'simple-echo)
       (frame-captured-text *application-frame*)))

(defun %print-text-to-printer (printer-name)
  "Print the current echo frame to PRINTER-NAME via lp.
   Uses frame-pdf-function if available, otherwise captured text."
  (let* ((frame (and (boundp '*application-frame*) *application-frame*))
         (pdf-fn (and frame (typep frame 'simple-echo) (frame-pdf-function frame)))
         (base (format nil "EchoOutput-~d" (get-universal-time)))
         (ps-path (format nil "~a.ps" base))
         (pdf-path (format nil "~a.pdf" base)))
    (if pdf-fn
        (progn
          (funcall pdf-fn ps-path)
          (uiop:run-program (list "ps2pdf" ps-path pdf-path)
                            :output nil :ignore-error-status t)
          (ignore-errors (delete-file ps-path))
          (uiop:run-program (list "lp" "-d" printer-name pdf-path)
                            :output nil :ignore-error-status t)
          (ignore-errors (delete-file pdf-path))
          (format *query-io* "~&Printed assets index to ~a~%" printer-name))
        (let ((text (%framed-text-content)))
          (when text
            (let* ((lines (count #\Newline text))
                   (total-pages (max 1 (ceiling lines (- 700 50))))
                   (title (format nil "Skyline-Tool for ~a"
                                  (string-capitalize
                                   (or (ignore-errors (symbol-value 'skyline-tool::*game-title*)) "Game"))))
                   (author (ignore-errors (skyline-tool::user-real-name))))
              (with-open-file (ps ps-path :direction :output :if-exists :supersede)
                (format ps "%!PS-Adobe-3.0~%")
                (skyline-tool::write-ps-docinfo ps title "Skyline-Tool"
                                               (format nil "~a on ~a" author (machine-instance)))
                (format ps "<< /PageSize [612 792] >> setpagedevice~%")
                (with-input-from-string (s text)
                  (dotimes (page total-pages)
                    (format ps "%%Page: ~d ~d~%" (1+ page) total-pages)
                    (let ((y 700) (line-height 10))
                      (loop for line = (read-line s nil nil)
                            while (and line (>= y 50))
                            do (format ps "50 ~d moveto (~a) show~%" y (skyline-tool::escape-ps-string line))
                               (decf y line-height)))
                    (format ps "showpage~%"))))
              (uiop:run-program (list "ps2pdf" ps-path pdf-path)
                                :output nil :ignore-error-status t)
              (ignore-errors (delete-file ps-path))
              (uiop:run-program (list "lp" "-d" printer-name pdf-path)
                                :output nil :ignore-error-status t)
              (ignore-errors (delete-file pdf-path))
              (format *query-io* "~&Printed ~a to ~a~%" pdf-path printer-name)))))))

(define-simple-echo-command (com-print-select :menu nil :name t) ()
  (let* ((frame *application-frame*)
         (pdf-fn (frame-pdf-function frame))
         (printers (ignore-errors (skyline-tool::discover-printers))))
    (if (null printers)
        (format *query-io* "~&No printers discovered.~%")
        (progn
          (format *query-io* "~&Select printer (1-~d):~%" (length printers))
          (dotimes (i (length printers))
            (format *query-io* "  ~d. ~a~%" (1+ i) (elt printers i)))
          (force-output *query-io*)
          (let* ((choice (clim:accept 'integer :prompt "Printer :" :default 1))
                 (printer (elt printers (1- choice)))
                 (base (format nil "EchoOutput-~d" (get-universal-time)))
                 (ps-path (format nil "~a.ps" base))
                 (pdf-path (format nil "~a.pdf" base)))
            (if pdf-fn
                (progn
                  (funcall pdf-fn ps-path)
                  (uiop:run-program (list "ps2pdf" ps-path pdf-path)
                                    :output nil :ignore-error-status t)
                  (ignore-errors (delete-file ps-path))
                  (uiop:run-program (list "lp" "-d" printer pdf-path)
                                    :output nil :ignore-error-status t)
                  (ignore-errors (delete-file pdf-path))
                  (format *query-io* "~&Printed assets index to ~a~%" printer))
                (let* ((text (frame-captured-text frame))
                       (lines (count #\Newline text))
                       (lines-per-page (- 700 50))
                       (total-pages (max 1 (ceiling lines lines-per-page)))
                       (title (format nil "Skyline-Tool for ~a"
                                      (string-capitalize
                                       (or (ignore-errors (symbol-value 'skyline-tool::*game-title*)) "Game"))))
                       (author (ignore-errors (skyline-tool::user-real-name))))
                  (with-open-file (ps ps-path :direction :output :if-exists :supersede)
                    (format ps "%!PS-Adobe-3.0~%")
                    (skyline-tool::write-ps-docinfo ps title "Skyline-Tool"
                                                    (format nil "~a on ~a" author (machine-instance)))
                    (format ps "<< /PageSize [612 792] >> setpagedevice~%")
                    (with-input-from-string (s text)
                      (dotimes (page total-pages)
                        (format ps "%%Page: ~d ~d~%" (1+ page) total-pages)
                        (let ((y 700) (line-height 10))
                          (loop for line = (read-line s nil nil)
                                while (and line (>= y 50))
                                do (format ps "50 ~d moveto (~a) show~%" y
                                           (skyline-tool::escape-ps-string line))
                                   (decf y line-height)))
                        (format ps "showpage~%"))))
                  (uiop:run-program (list "ps2pdf" ps-path pdf-path)
                                    :output nil :ignore-error-status t)
                  (ignore-errors (delete-file ps-path))
                  (format *query-io* "~&Sending ~a to ~a~%" pdf-path printer)
                  (uiop:run-program (list "lp" "-d" printer pdf-path)
                                    :output nil :ignore-error-status t))))))))

(define-simple-echo-command (com-about-echo :menu nil :name t) ()
  (let* ((frame (and (boundp '*application-frame*) *application-frame*))
         (title (when frame (ignore-errors (frame-pretty-name frame)))))
    (if (and title (search "About Skyline-Tool" title :test #'char-equal))
        (format *query-io* "~&Already viewing About Skyline-Tool.~%")
        (skyline-tool::com-about-skyline-tool))))

(defun %clipboard-copy (text)
  "Copy TEXT to the system clipboard using wl-copy or xclip, or output it."
  (block nil
    (let ((prog (or (ignore-errors (string-trim '(#\Newline #\Space)
                                      (uiop:run-program '("which" "wl-copy") :output :string)))
                    (ignore-errors (string-trim '(#\Newline #\Space)
                                      (uiop:run-program '("which" "xclip") :output :string))))))
      (if prog
          (with-input-from-string (in text)
            (let ((args (if (search "wl-copy" prog)
                            (list prog)
                            (list prog "-selection" "clipboard"))))
              (let ((exit (uiop:run-program args :input in :output nil
                                            :ignore-error-status t :force-shell nil)))
                (unless exit (return nil))
                (when (and (integerp exit) (zerop exit)) t))))
          ;; No clipboard tool — output to *query-io* instead
          (progn
            (format *query-io* "~&~a" text)
            t)))))

(define-simple-echo-command (com-copy-clipboard :menu nil :name t) ()
  (if (not (boundp '*application-frame*))
      (format *query-io* "~&No active window to copy from.~%")
      (let ((text (frame-captured-text *application-frame*)))
        (if (and (plusp (length text)) (%clipboard-copy text))
            (format *query-io* "~&Copied ~d characters.~%" (length text))
            (format *query-io* "~&Failed to copy.~%")))))

(define-simple-echo-command (com-copy-as-json :menu nil :name t) ()
  (if (not (boundp '*application-frame*))
      (format *query-io* "~&No active window to copy from.~%")
      (let* ((text (frame-captured-text *application-frame*))
             (json (if (and (> (length text) 0) (char= (char text 0) #\{))
                       text
                       (format nil "{ \"window\": ~s, \"content\": ~s, \"timestamp\": ~s }"
                               (ignore-errors (clim:frame-pretty-name *application-frame*))
                               text
                               (multiple-value-bind (s m h d mo y) (get-decoded-time)
                                 (declare (ignore s))
                                 (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" y mo d h m))))))
        (if (%clipboard-copy json)
            (format *query-io* "~&Copied ~d characters as JSON to clipboard.~%" (length json))
            (format *query-io* "~&Copy to clipboard: no clipboard tool found.~%")))))

(define-simple-echo-command (com-save-as-json :menu nil :name t) ()
  (if (not (boundp '*application-frame*))
      (format *query-io* "~&No active window.~%")
      (let* ((text (frame-captured-text *application-frame*))
             (frame-name (ignore-errors (clim:frame-pretty-name *application-frame*)))
             (default-name (format nil "~a.json" (or frame-name "output")))
             (path (%zenity-or-clim "Save As JSON..." default-name)))
        (when path
          (let* ((timestamp (multiple-value-bind (s m h d mo y) (get-decoded-time)
                              (declare (ignore s))
                              (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" y mo d h m)))
                 (json (if (and (> (length text) 0) (char= (char text 0) #\{))
                           text  ; already structured JSON (About dialog)
                           (format nil "{ \"title\": ~s, \"content\": ~s, \"generated\": ~s }"
                                   frame-name text timestamp))))
            (with-open-file (f path :direction :output :if-exists :supersede
                                     :external-format :utf-8)
              (princ json f))
            (format *query-io* "~&Saved ~a (~d bytes).~%" path (length json)))))))

(define-simple-echo-command (com-close-echo :menu nil :name t) ()
  (clim:frame-exit *application-frame*))

(define-simple-echo-command (com-find-in-echo :menu nil :name t) ()
  "Toggle the asset filter in the All Resources window."
  (if (and (boundp 'skyline-tool::*assets-index-filter*)
           (boundp 'skyline-tool::com-find-in-assets))
      (skyline-tool::com-find-in-assets)
      (format *query-io* "~&Find is not available in this window.~%")))

(define-simple-echo-command (com-open-dev-guide :menu nil :name t) ()
  (let ((html-index (namestring
                     (asdf:system-relative-pathname
                      :skyline-tool
                      #p"../Dist/7800/PhantasiaDevGuide-html/index.html"))))
    (if (probe-file html-index)
        (uiop:run-program (list "xdg-open" html-index) :output nil)
        (progn
          (format *query-io* "~&Developers' Guide not built. Run make doc.~%")
          (if (clim:accept 'boolean :prompt "Build now" :default t)
              (clim-sys:make-process
               (lambda ()
                 (uiop:run-program (list "ptyxis" "-s" "--title" "Building Dev Guide"
                                         "--" "make" "doc")
                                   :output nil :ignore-error-status t))
               :name "Building Dev Guide")
              (format *query-io* "~&Run make doc when ready.~%"))))))

(define-simple-echo-command (com-regenerate :menu nil :name t) ()
  "Re-run the current display function to regenerate the report."
  (clim:redisplay-frame-panes *application-frame*))

(macrolet ((def-regen (name build region)
             `(define-simple-echo-command (,name :menu nil :name t) ()
                (skyline-tool::rom-budget ,build ,region))))
  (def-regen com-regenerate-ntsc-demo "Demo" :ntsc)
  (def-regen com-regenerate-ntsc-public "Public" :ntsc)
  (def-regen com-regenerate-ntsc-publisher "Publisher" :ntsc)
  (def-regen com-regenerate-pal-demo "Demo" :pal)
  (def-regen com-regenerate-pal-public "Public" :pal)
  (def-regen com-regenerate-pal-publisher "Publisher" :pal))

;; --- Print To menu population ---

(defun %print-echo-to-printer (printer-queue-name)
  "Print the current echo frame's content to PRINTER-QUEUE-NAME.
   Uses frame-pdf-function if available, otherwise captured text."
  (let* ((frame (and (boundp '*application-frame*) *application-frame*))
         (pdf-fn (and frame (frame-pdf-function frame)))
         (base (format nil "EchoOutput-~d" (get-universal-time)))
         (ps-path (format nil "~a.ps" base))
         (pdf-path (format nil "~a.pdf" base)))
    (if pdf-fn
        (progn
          (funcall pdf-fn ps-path)
          (uiop:run-program (list "ps2pdf" ps-path pdf-path)
                            :output nil :ignore-error-status t)
          (ignore-errors (delete-file ps-path))
          (uiop:run-program (list "lp" "-d" printer-queue-name pdf-path)
                            :output nil :ignore-error-status t)
          (ignore-errors (delete-file pdf-path))
          (format *query-io* "~&Printed assets index to ~a~%" printer-queue-name))
        (let* ((text (and frame (frame-captured-text frame))))
          (unless (and text (stringp text) (plusp (length text)))
            (format *query-io* "~&No content to print.~%")
            (return-from %print-echo-to-printer))
          (let* ((lines (count #\Newline text))
                 (total-pages (max 1 (ceiling lines (- 700 50))))
                 (title (format nil "Skyline-Tool for ~a"
                                (string-capitalize
                                 (or (ignore-errors (symbol-value 'skyline-tool::*game-title*)) "Game"))))
                 (author (ignore-errors (skyline-tool::user-real-name))))
            (with-open-file (ps ps-path :direction :output :if-exists :supersede)
              (format ps "%!PS-Adobe-3.0~%")
              (skyline-tool::write-ps-docinfo ps title "Skyline-Tool"
                                             (format nil "~a on ~a" author (machine-instance)))
              (format ps "<< /PageSize [612 792] >> setpagedevice~%")
              (skyline-tool::write-ps-font-encodings ps)
              (with-input-from-string (s text)
                (dotimes (page total-pages)
                  (format ps "%%Page: ~d ~d~%" (1+ page) total-pages)
                  (let ((y 700) (line-height 10))
                    (loop for line = (read-line s nil nil)
                          while (and line (>= y 50))
                          do (format ps "50 ~d moveto (~a) show~%" y
                                     (skyline-tool::escape-ps-string line))
                             (decf y line-height)))
                  (format ps "showpage~%"))))
            (uiop:run-program (list "ps2pdf" ps-path pdf-path)
                              :output nil :ignore-error-status t)
            (ignore-errors (delete-file ps-path))
            (uiop:run-program (list "lp" "-d" printer-queue-name pdf-path)
                              :output nil :ignore-error-status t)
            (ignore-errors (delete-file pdf-path))
            (format *query-io* "~&Printed ~a to ~a~%" pdf-path printer-queue-name))))))

(defun populate-echo-print-menu (&optional frame (command-table 'echo-print-to-menu))
  "Populate echo-print-to-menu with discovered printers.
   Falls back to captured text printing when frame has no pdf-function.
   When COMMAND-TABLE is specified, populate that table instead."
  (declare (ignore frame))
  (ignore-errors
   (clim:remove-menu-item-from-command-table command-table "No printers found")
   (clim:remove-menu-item-from-command-table command-table "Default Printer (lpr)")
   (dolist (p (ignore-errors (skyline-tool::discover-printers)))
     (ignore-errors
      (clim:remove-menu-item-from-command-table command-table p))))
  (let* ((printers (ignore-errors (skyline-tool::discover-printers-with-names)))
         (queue (gensym "PRINT-QUEUE-"))
         (display (gensym "PRINT-NAME-")))
    (if printers
        (dolist (pair printers)
          (let ((queue-name (car pair))
                (display-name (cdr pair)))
            (clim:add-menu-item-to-command-table
             command-table display-name :command
             `(com-print-to-printer ,queue-name ,display-name)
             :after :end)))
        (clim:add-menu-item-to-command-table
         command-table "Default Printer (lpr)" 
         :command
         `(com-print-to-printer ,queue-name ,display-name)
         :after :end
         :value t)))
  ;; Define the print command dynamically
  (unless (fboundp 'com-print-to-printer)
    (clim:define-command (com-print-to-printer
                          :command-table clim-internals::global-command-table
                          :menu nil :name t)
        ((queue-name 'string) (display-name 'string))
      (declare (ignore display-name))
      (%print-echo-to-printer queue-name))))

;; --- Run function ---

(defun run-in-simple-echo (function &key (width 800)
                                         (height 400)
                                         port
                                         frame-manager
                                         pdf-function
                                         (process-name (format nil "Echo from ~s" function))
                                         (window-title process-name)
                                         menu-bar)
  (let* ((fm (or frame-manager (find-frame-manager :port (or port (clim:find-port)))))
         (pipe function)
          (frame (make-application-frame 'simple-echo
                                         :name "Skyline-Tool"
                                         :pretty-name window-title
                                         :function function
                                         :frame-manager fm
                                         :pipe pipe
                                         :pdf-function pdf-function
                                         :menu-bar (or menu-bar 'echo-menu-bar)
                                         :width width
                                         :height height)))
    (clim-sys:make-process (lambda ()
                             (run-frame-top-level frame))
                           :name process-name)))

;; --- Display function with output capture ---

(defclass capturing-stream (fundamental-character-output-stream)
  ((target :initarg :target :reader capturing-target)
   (capture :initarg :capture :reader capturing-capture))
  (:default-initargs :capture (make-string-output-stream)))

(defmethod stream-write-char ((s capturing-stream) c)
  (write-char c (slot-value s 'target))
  (write-char c (slot-value s 'capture)))

(defmethod stream-write-string ((s capturing-stream) string &optional (start 0) (end (length string)))
  (write-string string (slot-value s 'target) :start start :end end)
  (write-string string (slot-value s 'capture) :start start :end end))

(defmethod stream-force-output ((s capturing-stream))
  (force-output (slot-value s 'target)))

(defmethod stream-fresh-line ((s capturing-stream))
  (fresh-line (slot-value s 'target))
  (fresh-line (slot-value s 'capture)))

(defmethod stream-terpri ((s capturing-stream))
  (terpri (slot-value s 'target))
  (terpri (slot-value s 'capture)))

#+ ()
(defmethod stream-start-line-p ((s capturing-stream))
  (start-line-p (slot-value s 'target)))

;; Delegate CLIM output recording to the target (pane) stream
(defmethod clim:invoke-with-output-to-output-record ((stream capturing-stream) continuation record-type &key parent)
  (clim:invoke-with-output-to-output-record (slot-value stream 'target) continuation record-type :parent parent))

(defmethod clim:stream-add-output-record ((stream capturing-stream) record)
  (clim:stream-add-output-record (slot-value stream 'target) record))

(defmethod clim:stream-output-history ((stream capturing-stream))
  (clim:stream-output-history (slot-value stream 'target)))

(defmethod clim:window-clear ((stream capturing-stream))
  (clim:window-clear (slot-value stream 'target)))

(defmethod clim-internals::invoke-with-sheet-medium (continuation (stream capturing-stream))
  ;; bordered-output and other CLIM infrastructure may need the sheet medium;
  ;; delegate to the target (pane) stream which has a proper medium.
  (clim-internals::invoke-with-sheet-medium continuation (slot-value stream 'target)))

;; Delegate common CLIM stream queries to the target pane
(defmethod clim:stream-cursor-position ((stream capturing-stream))
  (clim:stream-cursor-position (slot-value stream 'target)))

(defmethod clim:stream-set-cursor-position ((stream capturing-stream) x y)
  (clim:stream-set-cursor-position (slot-value stream 'target) x y))

(defmethod stream-line-height ((stream capturing-stream) &key text-style)
  (stream-line-height (slot-value stream 'target) :text-style text-style))

(defmethod clim-internals::stream-write-object ((stream capturing-stream) object)
  (clim-internals::stream-write-object (slot-value stream 'target) object))

(defmethod clim:stream-recording-p ((stream capturing-stream))
  (clim:stream-recording-p (slot-value stream 'target)))

;; Delegate with-drawing-options to the target pane's medium
(defmethod clim:invoke-with-drawing-options ((stream capturing-stream) continuation &rest drawing-options)
  (apply #'clim:invoke-with-drawing-options (slot-value stream 'target) continuation drawing-options))

(defmethod clim-internals::stream-write-object ((stream capturing-stream) object)
  (clim-internals::stream-write-object (slot-value stream 'target) object))

;; Expose the real target pane for code that needs direct CLIM drawing
(defvar *echo-pane* nil
  "Bound to the actual CLIM pane during echo-echo display functions.
   Use this instead of *standard-output* for CLIM operations that
   require a real pane (drawing, surrounding-output-with-border, etc.).")

(defun echo-echo (frame pane)
  ;; Save scroll position before clearing
  (let ((old-x 0) (old-y 0))
    (multiple-value-setq (old-x old-y)
      (ignore-errors (clim:window-viewport-position pane)))
    (clim:window-clear pane)
    (let* ((capture (make-string-output-stream))
           (capturing-stream (make-instance 'capturing-stream
                               :target pane :capture capture)))
      (let ((*standard-output* capturing-stream)
            (*trace-output* *standard-output*)
            (*error-output* *standard-output*)
            (*echo-pane* pane))
        (funcall (frame-pipe frame)))
      (setf (frame-captured-text frame) (get-output-stream-string capture))
      (force-output pane)
      ;; Restore scroll position — on first call old-x/old-y are (0,0), fine.
      (ignore-errors (setf (clim:window-viewport-position pane) (values old-x old-y))))))
