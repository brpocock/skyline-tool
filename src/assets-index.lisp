;;; Phantasia SkylineTool/src/assets-index.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC.

(in-package :skyline-tool)

;;;^L
;; All resources view — replaces "Check for Absent Assets"
;; and "Show Assets Index".

(clim:define-presentation-type unified-asset-entry ())
(clim:define-presentation-type build-checkbox ())

(defun asset-index->filesystem-path (moniker)
  "Convert an Assets.index MONIKER (e.g. 'Blobs/TitleCard') to the
   full filesystem path, or NIL if the file doesn't exist."
  (let* ((parts (split-sequence #\/ moniker))
         (kind (first parts))
         (name (car (last parts)))
         (rest-parts (butlast (rest parts))))
    (flet ((try (dir-rel type)
             (let ((p (make-pathname :directory (list* :relative "Source" dir-rel)
                                      :name name :type type)))
               (when (probe-file p) (namestring (truename p))))))
    (cond ((string-equal kind "Scripts")
           (or (try (append (list "Scripts") rest-parts) "fountain")
               (try (append (list "Scripts") rest-parts) "forth")))
          ((string-equal kind "Songs")
           (try (list "Songs") "mscz"))
          ((string-equal kind "Maps")
           (try (append (list "Maps") rest-parts) "tmx"))
          ((string-equal kind "Blobs")
           (try (list "Blobs" (machine-directory-name)) "xcf"))
          (t nil))))

(defun collect-all-assets ()
  "Return a list of (moniker builds kind-name asset-id hex-str present-p full-path)
   combining all entries from Assets.index with filesystem assets not yet indexed."
  (read-assets-list)
  (let ((results nil)
        (seen (make-hash-table :test 'equal)))
    ;; 1. All Assets.index entries
    (maphash (lambda (key builds)
               (let* ((kind-parts (asset-kind/name key))
                      (kind-name (if kind-parts (first kind-parts) nil))
                      (asset-name (if kind-parts (second kind-parts) key))
                      (kind (kind-by-name kind-name))
                      (asset-id (ignore-errors (get-asset-id kind asset-name)))
                      (hex-str (when asset-id
                                 (format nil "$~(~v,'0x~)"
                                         (if (eql kind :script) 4 2)
                                         asset-id)))
                      (full-path (asset-index->filesystem-path key)))
                 (setf (gethash key seen) t)
                 (push (list key builds kind-name asset-id hex-str (not (null full-path))
                             full-path)
                       results)))
             *assets-list*)
    ;; 2. Filesystem assets not in Assets.index
    (dolist (wild (list (format nil "Source/Blobs/~a/*.xcf" (machine-directory-name))
                        #p"Source/Maps/*/*.tmx"
                        #p"Source/Scripts/**/*.fountain"
                        #p"Source/Scripts/**/*.forth"
                        #p"Source/Songs/*.mscz"))
      (ignore-errors
       (dolist (file (recursive-directory wild))
         (let* ((moniker (asset-file->moniker file)))
           (when (and moniker (not (gethash moniker seen)))
             (setf (gethash moniker seen) t)
             (let* ((kind-parts (asset-kind/name moniker))
                    (kind-name (if kind-parts (first kind-parts) nil))
                    (asset-name (if kind-parts (second kind-parts) moniker))
                    (kind (kind-by-name kind-name))
                    (asset-id (ignore-errors (get-asset-id kind asset-name)))
                    (hex-str (when asset-id
                               (format nil "$~(~v,'0x~)"
                                       (if (eql kind :script) 4 2)
                                       asset-id)))
                    (full-path (namestring (truename file))))
               (push (list moniker nil kind-name asset-id hex-str t full-path)
                     results)))))))
    ;; Sort: Scripts, Songs, Maps, Blobs; alphabetically within each group;
    ;; for Scripts and Maps, sort by locale directory first.
    (let ((order '("Scripts" "Songs" "Maps" "Characters" "Blobs")))
      (sort results (lambda (a b)
                      (let* ((ka (position (third a) order :test #'string-equal))
                             (kb (position (third b) order :test #'string-equal))
                             (ka (or ka most-positive-fixnum))
                             (kb (or kb most-positive-fixnum))
                             (locale-a (if (member (third a) '("Scripts" "Maps") :test #'string-equal)
                                          (let ((p (split-sequence #\/ (first a))))
                                            (if (> (length p) 2) (second p) ""))
                                           ""))
                             (locale-b (if (member (third b) '("Scripts" "Maps") :test #'string-equal)
                                          (let ((p (split-sequence #\/ (first b))))
                                            (if (> (length p) 2) (second p) ""))
                                           "")))
                        (or (< ka kb)
                            (and (= ka kb)
                                 (or (string-lessp locale-a locale-b)
                                     (and (string= locale-a locale-b)
                                          (string-lessp (first a) (first b)))))))))))

(defun color-rgb-for-kind (kind-name)
  "Return PostScript setrgbcolor values for a KIND-NAME background."
  (cond ((string-equal kind-name "Scripts") "0.0 0.0 0.502 setrgbcolor")
        ((string-equal kind-name "Songs") "0.502 0.0 0.0 setrgbcolor")
        ((string-equal kind-name "Maps") "0.302 0.149 0.0 setrgbcolor")
        ((string-equal kind-name "Blobs") "0.0 0.302 0.0 setrgbcolor")
        ((string-equal kind-name "Characters") "0.502 0.0 0.502 setrgbcolor")
        (t "0.3 0.3 0.3 setrgbcolor")))

(defun color-for-asset-kind (kind-name)
  "Return a CLIM color for the KIND-NAME."
  (cond ((string-equal kind-name "Scripts") (clim:make-rgb-color 0 0 0.502))
        ((string-equal kind-name "Songs") (clim:make-rgb-color 0.502 0 0))
        ((string-equal kind-name "Maps") (clim:make-rgb-color 0.302 0.149 0))
        ((string-equal kind-name "Blobs") (clim:make-rgb-color 0 0.302 0))
        ((string-equal kind-name "Characters") (clim:make-rgb-color 0.502 0 0.502))
        (t (clim:make-rgb-color 0.3 0.3 0.3))))

(defun write-assets-index-ps (path)
  "Generate a PostScript document at PATH with the full Assets Index.
   Uses proper kind badges, colored section headings, D/P/A checkboxes,
   typographical quotes, bordered entries, and pagination."
  (let* ((all-assets (collect-all-assets))
         (kind-order '("Scripts" "Songs" "Maps" "Characters" "Blobs"))
         (page-width 612) (page-height 792)
         (margin-left 102) (margin-right 102)
         (page-top 680) (page-bottom 80)
         (line-h 12) (entry-h 14) (heading-h 20)
         (y page-top)
         (page-num 1))
    (flet ((next-page (ps)
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
      (write-ps-header-bar ps (format nil "Assets Index for ~a"
                                      (string-capitalize *game-title*))
                           "" "" (string-capitalize *game-title*) 1 1)
      (format ps "/Times-Roman-ISOLatin1 findfont 9 scalefont setfont 0 0 0 setrgbcolor~%")
      (setf y page-top)
      ;; Group by kind
      (let ((grouped (make-hash-table :test 'equal))
            (locale-groups (make-hash-table :test 'equal)))
        (dolist (entry all-assets)
          (destructuring-bind (moniker builds kind-name asset-id hex-str present-p full-path)
              entry
            (declare (ignore asset-id present-p full-path))
            (push entry (gethash kind-name grouped))))
        ;; Iterate kinds in order
        (dolist (kind kind-order)
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
 " (color-rgb-for-kind kind)
   (+ margin-left 6) (- y 6) (escape-ps-string kind))
            (decf y 24)
            ;; Group by locale for Scripts and Maps
            (clrhash locale-groups)
            (dolist (e entries)
              (destructuring-bind (moniker &rest rest) e
                (declare (ignore rest))
                (let* ((parts (split-sequence #\/ moniker))
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
                    (destructuring-bind (moniker builds kind-name asset-id hex-str present-p full-path)
                        entry
                      (declare (ignore asset-id full-path))
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
                            (decf y entry-h)))))))))
      ;; Page footer and showpage
      (write-ps-page-footer ps page-num page-num
                            (string-capitalize *game-title*) nil
                            (user-real-name) (machine-instance))
      (format ps "showpage~%"))))

;; --- Collapsible section state ---

(defvar *assets-index-collapsed* (make-hash-table :test 'equal)
  "Hash table mapping section key strings to booleans (t = collapsed).")

(defvar *assets-index-state*
  (list :collapsed *assets-index-collapsed*
        :current-kind nil
        :current-locale nil)
  "Plist holding the assets index display state.")

(clim:define-presentation-type assets-section-header () :inherit-from 'string)


;; --- Asset action menu command ---

(clim:define-command (com-asset-action-menu :command-table clim-internals::global-command-table
                                            :menu t :name t)
    ((entry 'unified-asset-entry :gesture :select))
  (destructuring-bind (moniker builds kind-name asset-id hex-str present-p full-path)
      entry
    (%show-asset-context-menu moniker builds kind-name asset-id hex-str present-p full-path)))

(clim:define-command (com-asset-context-menu :command-table clim-internals::global-command-table
                                              :menu t :name t)
    ((entry 'unified-asset-entry :gesture :menu))
  (destructuring-bind (moniker builds kind-name asset-id hex-str present-p full-path)
      entry
    (%show-asset-context-menu moniker builds kind-name asset-id hex-str present-p full-path)))

(clim:define-command (com-toggle-build-flag :command-table clim-internals::global-command-table
                                             :menu t :name t)
    ((entry 'build-checkbox :gesture :select))
  (destructuring-bind (moniker builds kind-name asset-id hex-str present-p full-path)
      entry
    (declare (ignore kind-name asset-id hex-str present-p full-path))
    (%toggle-build-flag moniker builds flag-char)))

;; --- Clipboard PNG copy helper ---

(defun %copy-png-to-clipboard (png-path)
  "Copy a PNG file at PNG-PATH to the system clipboard as image/png."
  (let ((prog (or (ignore-errors (string-trim '(#\Newline #\Space)
                                             (uiop:run-program '("which" "wl-copy") :output :string)))
                  (ignore-errors (string-trim '(#\Newline #\Space)
                                             (uiop:run-program '("which" "xclip") :output :string))))))
    (if prog
        (let ((args (if (search "wl-copy" prog)
                        (list prog "--type" "image/png")
                        (list prog "-selection" "clipboard" "-t" "image/png"))))
          (with-open-file (in png-path :element-type '(unsigned-byte 8))
            (let ((exit (uiop:run-program args :input in :output nil
                                          :ignore-error-status t :force-shell nil)))
              (when (and exit (integerp exit) (zerop exit))
                (format *query-io* "~&Copied PNG to clipboard.~%")
                (return-from %copy-png-to-clipboard t))))
          (format *query-io* "~&Clipboard copy failed.~%"))
        (format *query-io* "~&Clipboard copy requires wl-copy or xclip.~%")))

;; --- Asset action menu (both left and right click) ---

(defun %show-asset-context-menu (moniker builds kind-name asset-id hex-str present-p full-path)
  (flet ((run (cmd &rest a)
            (uiop:run-program (cons cmd a) :output nil :ignore-error-status t))
          (script-src ()
            (format nil "Source/~a.fountain" moniker))
          (item (label fn)
            (list label fn)))
    (let* ((parts (split-sequence #\/ moniker))
           (basename (car (last parts)))
           (type-key (ignore-errors (kind-by-name kind-name)))
           (file-path (or full-path (asset-index->filesystem-path moniker)))
           (dir-path (when file-path
                       (namestring (make-pathname :defaults file-path :name nil :type nil))))
           (moniker-full (if (search "Scripts/" moniker) moniker
                           (format nil "Scripts/~a" moniker)))
           (label (format nil "~a ~a~@[  $~a~]"
                          kind-name
                          (cl-change-case:title-case
                           (cl-ppcre:regex-replace "\\bDont\\b" basename "Don't"))
                          hex-str))
           (items nil))
      (macrolet (($ (label . body) `(push (item ,label (lambda () ,@body)) items))
                 (--- () '(push nil items))))
        ;; Editor entries per type -- dispatch on kind-name string
        (let ((kind-name-str (if kind-name (string kind-name) "")))
          (cond
            ((string-equal kind-name-str "Scripts")
             ($ "Run in playtest emulator" (run-script moniker-full))
             ($ "Read aloud on AtariVox"
                (if (fboundp 'read-script-interactive)
                    (read-script-interactive moniker-full)
                    (format *query-io* "~&AtariVox not loaded.~%")))
             (---)
             ($ "Edit in Climacs"
                (clim-sys:make-process
                 (lambda () (climacs:edit-file (script-src)
                                               :process-name (format nil "Editing ~a" moniker)))
                 :name (format nil "Edit ~a" moniker)))
             ($ "Open in Emacs"
                (clim-sys:make-process
                 (lambda () (run "emacsclient" "-n" (script-src)))
                 :name (format nil "Edit ~a" moniker)))
             ($ "Open in ThiefMD"
                (clim-sys:make-process
                 (lambda () (run "thiefmd" (script-src)))
                 :name (format nil "Edit ~a" moniker)))
             (---)
             ($ "Save as PDF"
                (let* ((def (format nil "~a.pdf" (substitute #\_ #\/ moniker)))
                       (dir (if (find-package :clim-simple-echo)
                                (clim-simple-echo::default-save-directory)
                                (merge-pathnames #p"Documents/" (user-homedir-pathname))))
                       (full (namestring (merge-pathnames def dir)))
                       (path (string-trim '(#\Newline #\Space)
                                          (run "zenity" "--file-selection" "--save"
                                               (format nil "--filename=~a" full)
                                               "--title=Save Script As PDF..."))))
                  (when (and path (> (length path) 0))
                    (handler-case
                        (progn (fountain->pdf moniker path)
                               (format *query-io* "~&Saved ~a~%" path))
                      (error (e) (format *query-io* "~&PDF error: ~a~%" e))))))
             ($ "Print"
                (let* ((pdf (format nil "/tmp/st-~a.pdf" (substitute #\_ #\/ moniker)))
                       (printers (ignore-errors (discover-printers))))
                  (unless printers
                    (return-from %show-asset-context-menu
                      (format *query-io* "~&No printers.~%")))
                  (handler-case (fountain->pdf moniker pdf)
                    (error (e) (return-from %show-asset-context-menu
                                     (format *query-io* "~&PDF error: ~a~%" e))))
                  (let* ((names (mapcar (lambda (p) (if (consp p) (cdr p) p)) printers))
                         (choice (clim:menu-choose
                                  (mapcar (lambda (n) (list n n)) names)
                                  :label "Select printer")))
                    (when choice
                      (let ((q (if (consp (find choice names :test #'equal
                                                         :key (lambda (x)
                                                                (if (consp x) (cdr x) x))))
                                    (car (find choice names :test #'equal
                                                           :key (lambda (x)
                                                                  (if (consp x) (cdr x) x))))
                                    choice)))
                        (format *query-io* "~&Printing to ~a...~%" choice)
                        (run "lp" "-d" q pdf)
                        (ignore-errors (delete-file pdf))
                        (format *query-io* "~&Sent.~%"))))))
            ((string-equal kind-name-str "Maps")
             ($ "Copy as PNG"
                (format *query-io* "~&Map PNG not yet implemented.~%"))
             ($ "Open in Tiled" (run "tiled" (or file-path moniker))))
            ((string-equal kind-name-str "Songs")
             ($ "Open in MuseScore" (run "musescore" (or file-path moniker)))
             ($ "View Score as PDF"
                (let* ((src (or file-path moniker))
                       (pdf (format nil "/tmp/~a.pdf"
                                    (pathname-name (pathname src)))))
                  (run "musescore" src "-o" pdf)
                  (run "xdg-open" pdf)))
             ($ "Play as MIDI"
                (let* ((src (or file-path moniker))
                       (mid (format nil "/tmp/~a.mid"
                                    (pathname-name (pathname src)))))
                  (run "musescore" src "-o" mid)
                  (run "xdg-open" mid)))
             ($ "Play as Ogg Vorbis"
                (let* ((src (or file-path moniker))
                       (ogg (format nil "/tmp/~a.ogg"
                                    (pathname-name (pathname src)))))
                  (run "musescore" src "-o" ogg)
                  (run "xdg-open" ogg)))
             ($ "Play as FLAC"
                (let* ((src (or file-path moniker))
                       (flac (format nil "/tmp/~a.flac"
                                     (pathname-name (pathname src)))))
                  (run "musescore" src "-o" flac)
                  (run "xdg-open" flac))))
            ((string-equal kind-name-str "Blobs")
             (let* ((ext (and file-path (pathname-type (pathname file-path))))
                    (ext (and ext (string-downcase ext))))
               (cond
                 ((or (string-equal ext "png") (string-equal ext "xcf"))
                  ($ "Open in GIMP" (run "gimp" (or file-path moniker))))
                 ((string-equal ext "json")
                  ($ "Open in Emacs" (run "emacsclient" "-n" (or file-path moniker))))
                 ((string-equal ext "txt")
                  ($ "Open in Emacs" (run "emacsclient" "-n" (or file-path moniker))))
                 (t
                  ($ "Open in GIMP" (run "gimp" (or file-path moniker)))))))
            ((string-equal kind-name-str "Characters")
             (let ((char-name (car (last parts)))
                   (char-path (or file-path
                                  (probe-file (format nil "Source/Objects/~a.json"
                                                      (car (last parts)))))))
               ($ "Open (in Skyline-Tool)"
                  (show-class-view char-name))
               ($ "Open in Emacs (JSON)"
                  (when char-path
                    (open-in-emacs char-path)))))
            ((string-equal kind-name-str "Class")
             (let ((class-name (car (last parts))))
               ($ "Open (in Skyline-Tool)"
                  (show-class-view class-name))))
            ((string-equal kind-name-str "Art")
             ;; Sprite sheets and animation sequences
             ($ "Copy as PNG"
                (handler-case
                    (let* ((png-path "/tmp/st-copy-preview.png")
                           (art-name (car (last parts)))
                           (seq (find-if (lambda (s)
                                           (and (simple-animation-sequence-label s)
                                                (search art-name
                                                        (simple-animation-sequence-label s)
                                                        :test #'char-equal)))
                                          (progn (load-all-animation-sequences)
                                                 *animation-sequences*))))
                  (if seq
                      (multiple-value-bind (pixels w h)
                          (render-filmstrip-pixels seq :frame 0 :scale 1)
                        (let ((png (make-instance 'zpng:png :width w :height h
                                                     :color-type :truecolor :bpp 8
                                                     :image-data pixels)))
                          (zpng:write-png png png-path))
                          (%copy-png-to-clipboard png-path)
                          (ignore-errors (delete-file png-path))))
                    (error (e)
                      (format *query-io* "~&Art PNG error: ~a~%" e))))
               ($ "Open in GIMP" (run "gimp" (or file-path moniker))))
            ((string-equal kind-name-str "Tilesets")
             (let ((tile-path (or file-path
                                  (probe-file (format nil "Source/Maps/Tiles/~a.tsx"
                                                      (car (last parts)))))))
               ($ "Copy as PNG"
                  (handler-case
                      (let* ((png-path "/tmp/st-copy-preview.png")
                             (tileset (when tile-path (load-tileset tile-path)))
                             (image (when tileset (tileset-image tileset))))
                    (if image
                        (let* ((w (array-dimension image 0))
                               (h (array-dimension image 1))
                               (rgb (make-array (* w h 3) :element-type '(unsigned-byte 8)
                                                  :initial-element 0)))
                          (dotimes (y h)
                            (dotimes (x w)
                              (let* ((pal-idx (aref image x y))
                                     (color (nth pal-idx +prosystem-ntsc-palette+)))
                                (when color
                                  (setf (aref rgb (* (+ (* y w) x) 3)) (first color)
                                        (aref rgb (+ (+ (* y w) x) 3) 1) (second color)
                                        (aref rgb (+ (+ (* y w) x) 3) 2) (third color))))))
                          (let ((png (make-instance 'zpng:png :width w :height h
                                                         :color-type :truecolor :bpp 8
                                                         :image-data rgb)))
                            (zpng:write-png png png-path)
                            (%copy-png-to-clipboard png-path)
                            (ignore-errors (delete-file png-path))))
                      (error (e)
                        (format *query-io* "~&Tileset PNG error: ~a~%" e))))
                  ($ "Open in GIMP" (run "gimp" (or tile-path moniker)))
                  ($ "Open in Tiled" (run "tiled" (or file-path moniker)))))
            (t
             ($ "Copy as PNG"
                (format *query-io* "~&PNG copy not yet supported for this asset type.~%"))
             ($ "Open in GIMP" (run "gimp" (or file-path moniker))))))
          ;; Universal actions for all resource types
          (---)
          ($ "Copy Moniker"
             (let ((text (or full-path moniker)))
               (if (clim-simple-echo::%clipboard-copy text)
                   (format *query-io* "~&Copied ~a to clipboard.~%" text)
                   (format *query-io* "~&Clipboard copy requires wl-copy or xclip.~%"))))
          ($ "Copy Asset Name"
             (let ((text (cl-change-case:title-case
                          (cl-ppcre:regex-replace "\\bDont\\b" basename "Don't"))))
               (if (clim-simple-echo::%clipboard-copy text)
                   (format *query-io* "~&Copied \"~a\" to clipboard.~%" text)
                   (format *query-io* "~&Clipboard copy requires wl-copy or xclip.~%"))))
          ($ "Copy Hex ID"
             (when hex-str
               (if (clim-simple-echo::%clipboard-copy hex-str)
                   (format *query-io* "~&Copied $~a to clipboard.~%" hex-str)
                   (format *query-io* "~&Clipboard copy requires wl-copy or xclip.~%"))))
          ;; Toggle build flags
          (---)
          ($ "Toggle Demo" (%toggle-build-flag moniker builds #\D))
          ($ "Toggle Public" (%toggle-build-flag moniker builds #\P))
          ($ "Toggle AtariAge" (%toggle-build-flag moniker builds #\A))
          (---)
          ($ "Open Containing Folder" (run "xdg-open" (or dir-path ".")))
          (---)
          ;; Global actions
          ($ "Save List as Text"
             (let* ((text (%script-list-text))
                    (path (prompt-save-pathname "ScriptList.txt" "txt")))
              (when path
                (with-open-file (f path :direction :output :if-exists :supersede
                                        :external-format :utf-8)
                  (princ text f))
                (format *query-io* "~&Saved ~a~%" (namestring path)))))
          ($ "Save List as PDF"
             (let* ((text (%script-list-text))
                    (path (prompt-save-pathname "ScriptList.pdf" "pdf"))
                    (lines (count #\Newline text))
                    (pages (max 1 (ceiling lines (- 700 50))))
                    (game-title (string-capitalize *game-title*))
                    (title (format nil "Skyline-Tool for ~a" game-title))
                    (author (user-real-name))
                    (date-str (multiple-value-bind (s m h d mo y) (get-decoded-time)
                                (declare (ignore s))
                                (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" y mo d h m))))
               (when path
                 (let ((ps (make-pathname :type "ps" :defaults path)))
                   (with-open-file (f ps :direction :output :if-exists :supersede)
                     (format f "%!PS-Adobe-3.0~%")
                     (write-ps-docinfo f title "Skyline-Tool"
                                      (format nil "~a on ~a" author (machine-instance)))
                     (format f "<< /PageSize [612 792] >> setpagedevice~%")
                     (write-ps-font-encodings f)
                     (with-input-from-string (s text)
                       (dotimes (p pages)
                         (format f "%%Page: ~d ~d~%" (1+ p) pages)
                         (let ((y 700))
                           (loop for line = (read-line s nil nil)
                                 while (and line (>= y 50))
                                 do (format f "50 ~d moveto (~a) show~%" y
                                            (escape-ps-string line))
                                    (decf y 10))))
                     (write-ps-page-footer f (1+ p) pages title date-str author)
                     (format f "showpage~%")))
                   (run "ps2pdf" (namestring ps) (namestring path))
                   (ignore-errors (delete-file ps))
                   (format *query-io* "~&Saved ~a~%" (namestring path))))))
          ($ "Copy List"
             (let ((text (%script-list-text)))
               (clim:with-application-frame (frame)
                 (setf (clim-simple-echo::frame-captured-text frame) text))
               (if (clim-simple-echo::%clipboard-copy text)
                   (format *query-io* "~&Copied ~:d char~:p to clipboard.~%" (length text))
                   (format *query-io* "~&Clipboard copy requires wl-copy or xclip.~%")))
             (---)
             ($ "Save Assets Index" (write-sorted-assets-index))
             (---)
             ($ "Redisplay"
                (clim:redisplay-frame-panes *application-frame* :force-p t)))
        ;; Show the menu
        (setf items (nreverse items))
        (let ((choice (clim:menu-choose
                        (mapcar (lambda (item)
                                  (if (null item)
                                      '(nil :divider :line)
                                      (list (first item) (first item))))
                                items)
                        :label label)))
          (when choice
            (let ((fn (second (find choice items :key #'first :test #'equal))))
              (when fn (funcall fn))))))))

(defun %script-list-text ()
  "Return a plain-text listing of all script names grouped by area."
  (with-output-to-string (s)
    (let ((last-area nil))
      (dolist (sn (all-script-names :reloadp t))
        (terpri s)
        (let* ((parts (split-sequence #\/ sn))
               (area (elt parts (- (length parts) 2))))
          (unless (string-equal area last-area)
            (format s "~%~a~%" (cl-change-case:title-case area))
            (setf last-area area)))
        (format s "  ~a~%" (cl-ppcre:regex-replace "\\bDont\\b"
                         (cl-change-case:title-case
                          (subseq sn (1+ (position #\/ sn)))) "Don't"))))))

(defun %toggle-build-flag (moniker builds flag-char)
  "Toggle a build flag (D, P, or A) for MONIKER and rewrite Assets.index."
  (let* ((build-name (case flag-char (#\D "Demo") (#\P "Public") (#\A "AA") (t "")))
         (has-flag (and builds (member build-name builds :test #'string-equal)))
         (new-builds (if has-flag
                         (remove build-name builds :test #'string-equal)
                         (sort (cons build-name (copy-list builds)) #'string-lessp)))
         (lines (with-open-file (f #p"Source/Assets.index")
                  (loop for l = (read-line f nil nil) while l collect l)))
         (old-build-str (when builds
                          (format nil "~{~a~}" (mapcar (lambda (b) (case b ("Demo" "D") ("Public" "P") ("AA" "A"))) builds))))
         (new-build-str (format nil "~{~a~}" (mapcar (lambda (b) (case b ("Demo" "D") ("Public" "P") ("AA" "A"))) new-builds)))
         (old-line (format nil "~a~@[ ~a~]" moniker old-build-str))
         (new-line (format nil "~a~@[ ~a~]" moniker new-build-str)))
    (with-open-file (f #p"Source/Assets.index" :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (dolist (l lines)
        (write-line (if (string= (string-trim " " l) (string-trim " " old-line))
                        new-line l) f)))
    (format *query-io* "~&Toggled ~a for ~a: ~:[(none)~;~a~]~%"
            flag-char moniker (not (emptyp new-builds)) new-build-str)
    (ignore-errors (clim:redisplay-frame-panes *application-frame* :force-p t))))

;; --- Save Assets Index (sorted, blank lines between directories) ---

(defun write-sorted-assets-index ()
  "Rewrite Assets.index in alphabetical order with blank lines between
   directory sections. Preserves header comments."
  (let* ((path #p"Source/Assets.index")
         (lines (with-open-file (f path)
                  (loop for l = (read-line f nil nil) while l collect l)))
          (headers nil)
          (entries nil))
    ;; Split into header comments and sorted entries
    (dolist (l lines)
      (if (or (zerop (length (string-trim " " l)))
              (char= (char (string-trim " " l) 0) #\;))
          (push l headers)
          (push (string-trim " " l) entries)))
    (setf headers (nreverse headers)
          entries (sort (nreverse entries) #'string-lessp))
    ;; Group entries by first directory component (Scripts, Songs, Maps, Blobs)
    (let ((grouped (make-hash-table :test 'equal))
          (order nil))
      (dolist (e entries)
        (let* ((parts (split-sequence #\/ e))
               (dir (first parts)))
          (unless (gethash dir grouped)
            (push dir order))
          (push e (gethash dir grouped))))
      (setf order (nreverse order))
      (with-open-file (f path :direction :output :if-exists :supersede
                           :external-format :utf-8)
        (dolist (h headers) (write-line h f))
        (terpri f)
        (loop for dir in order
              for group = (sort (gethash dir grouped) #'string-lessp)
              do (dolist (e group) (write-line e f))
                 (terpri f))))
    (format *query-io* "~&Assets.index rewritten in sorted order with blank lines between directories.~%")
    ;; Redisplay
    (ignore-errors
      (clim:redisplay-frame-panes *application-frame* :force-p t))))

;; --- Launcher wrappers ---

(defun edit-assets-index ()
  "Open the unified Assets Index in a simple-echo window."
  (clim-simple-echo:run-in-simple-echo #'show-full-assets-index
                                       :process-name "Assets Index"
                                       :width 450 :height 700))

(defun check-for-absent-assets-in-project-folder ()
  "Open the unified Assets Index (includes absent-asset detection)."
  (edit-assets-index))