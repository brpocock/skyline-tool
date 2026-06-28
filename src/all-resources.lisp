;;; All Resources — Asset index browser for the Skyline-Tool Launcher
;;; Migrated from launcher.lisp

(in-package :skyline-tool)

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
            (t nil)))))

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
    (flet ((scan-fs (wild &optional moniker-fn)
             (ignore-errors
              (dolist (file (recursive-directory wild))
                (let* ((moniker (if moniker-fn
                                    (funcall moniker-fn file)
                                    (asset-file->moniker file))))
                  (when (and moniker (not (gethash moniker seen)))
                    (setf (gethash moniker seen) t)
                    (let* ((kind-parts (asset-kind/name moniker))
                           (kind-name (first kind-parts))
                           (asset-name (second kind-parts))
                           (kind (ignore-errors (kind-by-name kind-name)))
                           (asset-id (ignore-errors (get-asset-id kind asset-name)))
                           (hex-str (when asset-id
                                      (format nil "$~(~v,'0x~)"
                                              (if (eql kind :script) 4 2)
                                              asset-id)))
                           (full-path (namestring (truename file))))
                      (push (list moniker nil kind-name asset-id hex-str t full-path)
                            results))))))))
      ;; Scan for filesystem assets
      (scan-fs (format nil "Source/Blobs/~a/*.xcf" (machine-directory-name)))
      (scan-fs #p"Source/Maps/*/*.tmx")
      (scan-fs #p"Source/Scripts/**/*.fountain")
      (scan-fs #p"Source/Scripts/**/*.forth")
      (scan-fs #p"Source/Songs/*.mscz")
      ;; New filesystem-only types: construct moniker as Kind/Name
      (scan-fs #p"Source/Maps/Tiles/*.tsx"
               (lambda (f) (format nil "Tilesets/~a" (pathname-name f))))
      (scan-fs #p"Source/Objects/*.json"
               (lambda (f) (format nil "Object Prototypes/~a" (pathname-name f))))
      (scan-fs #p"Source/Classes/*.cob"
               (lambda (f) (format nil "Classes/~a" (pathname-name f))))
      (scan-fs #p"Source/Maps/RunCommands/*.cob"
               (lambda (f) (format nil "Routines/~a" (pathname-name f))))
      (scan-fs #p"Source/Maps/RunCommands/*.bas"
               (lambda (f) (format nil "Routines/~a" (pathname-name f))))
      (scan-fs #p"Source/Maps/RunCommands/*.pas"
               (lambda (f) (format nil "Routines/~a" (pathname-name f))))
      (scan-fs #p"Source/Art/*.art"
               (lambda (f) (format nil "Sprite Sheets/~a" (pathname-name f))))
      (scan-fs (format nil "Project.~a.json" (machine-directory-name))
               (lambda (f) (format nil "Special Resources/~a" (pathname-name f))))
      ;; Characters from NPCStats.ods
      (ignore-errors
       (load-npc-stats)
       (dolist (row *npc-stats*)
         (let* ((cname (getf row :name))
                (moniker (format nil "Characters/~a" cname)))
           (unless (gethash moniker seen)
             (setf (gethash moniker seen) t)
             (push (list moniker nil "Characters" nil nil t nil) results)))))
      ;; Boats from Boats.ods
      (ignore-errors
       (dolist (boat (load-boats))
         (destructuring-bind (name class notes) boat
           (declare (ignore class notes))
           (let ((moniker (format nil "Boats/~a" name)))
             (unless (gethash moniker seen)
               (setf (gethash moniker seen) t)
               (push (list moniker nil "Boats" nil nil t nil) results)))))))
    ;; Sort: nil (Special Resources) first, then alphabetically by kind-name.
    ;; Within each kind, alphabetically by moniker.
    ;; For Scripts and Maps, sort by locale directory first.
    (sort results (lambda (a b)
                    (let ((ka (third a))
                          (kb (third b)))
                      (cond
                        ((and (null ka) (null kb))
                         (string-lessp (first a) (first b)))
                        ((null ka) t)
                        ((null kb) nil)
                        (t
                         (or (string-lessp ka kb)
                             (and (string= ka kb)
                                  (if (member ka '("Scripts" "Maps") :test #'string-equal)
                                      (let* ((pa (split-sequence #\/ (first a)))
                                             (pb (split-sequence #\/ (first b)))
                                             (la (if (> (length pa) 2) (second pa) ""))
                                             (lb (if (> (length pb) 2) (second pb) "")))
                                        (or (string-lessp la lb)
                                            (and (string= la lb)
                                                 (string-lessp (first a) (first b)))))
                                      (string-lessp (first a) (first b))))))))))))

(defun color-rgb-for-kind (kind-name)
  "Return PostScript setrgbcolor values for a KIND-NAME background.
   NIL or \"Special Resources\" gets white (no badge)."
  (cond ((null kind-name) "1.0 1.0 1.0 setrgbcolor")
        ((string-equal kind-name "Scripts") "0.0 0.0 0.502 setrgbcolor")
        ((string-equal kind-name "Songs") "0.502 0.0 0.0 setrgbcolor")
        ((string-equal kind-name "Maps") "0.302 0.149 0.0 setrgbcolor")
        ((string-equal kind-name "Blobs") "0.0 0.302 0.0 setrgbcolor")
        ((string-equal kind-name "Boats") "0.0 0.2 0.5 setrgbcolor")
        ((string-equal kind-name "Characters") "0.502 0.0 0.502 setrgbcolor")
        ((string-equal kind-name "Tilesets") "0.8 0.4 0.0 setrgbcolor")
        ((string-equal kind-name "Sprite Sheets") "0.0 0.4 0.4 setrgbcolor")
        ((string-equal kind-name "Object Prototypes") "0.4 0.0 0.6 setrgbcolor")
        ((string-equal kind-name "Classes") "0.2 0.3 0.6 setrgbcolor")
        ((string-equal kind-name "Routines") "0.3 0.4 0.1 setrgbcolor")
        (t "0.3 0.3 0.3 setrgbcolor")))

(defun color-for-asset-kind (kind-name)
  "Return a CLIM color for the KIND-NAME.
   NIL or \"Special Resources\" gets white (no badge)."
  (cond ((null kind-name) (clim:make-rgb-color 1 1 1))
        ((string-equal kind-name "Scripts") (clim:make-rgb-color 0 0 0.502))
        ((string-equal kind-name "Songs") (clim:make-rgb-color 0.502 0 0))
        ((string-equal kind-name "Maps") (clim:make-rgb-color 0.302 0.149 0))
        ((string-equal kind-name "Blobs") (clim:make-rgb-color 0 0.302 0))
        ((string-equal kind-name "Boats") (clim:make-rgb-color 0 0.2 0.5))
        ((string-equal kind-name "Characters") (clim:make-rgb-color 0.502 0 0.502))
        ((string-equal kind-name "Tilesets") (clim:make-rgb-color 0.8 0.4 0))
        ((string-equal kind-name "Sprite Sheets") (clim:make-rgb-color 0 0.4 0.4))
        ((string-equal kind-name "Object Prototypes") (clim:make-rgb-color 0.4 0 0.6))
        ((string-equal kind-name "Classes") (clim:make-rgb-color 0.2 0.3 0.6))
        ((string-equal kind-name "Routines") (clim:make-rgb-color 0.3 0.4 0.1))
        (t (clim:make-rgb-color 0.3 0.3 0.3))))

(defun write-all-resources-ps (path)
  "Generate a PostScript document at PATH with the full Assets Index.
   Uses proper kind badges, colored section headings, D/P/A checkboxes,
   typographical quotes, bordered entries, and pagination."
  (let* ((all-assets (collect-all-assets))
         (kind-order '("Scripts" "Songs" "Maps" "Characters" "Boats" "Blobs"
                       "Tilesets" "Sprite Sheets" "Object Prototypes" "Classes" "Routines"
                       "Special Resources"))
         (page-width 612) (page-height 792)
         (margin-left 102) (margin-right 102)
         (page-top 680) (page-bottom 80)
         (line-h 12) (entry-h 14) (heading-h 20)
          (y page-top)
          (page-num 1))
     (declare (ignore line-h heading-h))
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
              (declare (ignore moniker builds asset-id hex-str present-p full-path))
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
                    (push e (gethash (or locale "") locale-groups)))))
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
                          (decf y entry-h)))))))))
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

(clim:define-presentation-type assets-section-header () :inherit-from 'string)

(clim:define-command (com-toggle-section-header :command-table clim-internals::global-command-table
                                                :menu t :name t)
    ((section 'assets-section-header :gesture :select))
  (setf (gethash section *all-resources-collapsed*)
        (not (gethash section *all-resources-collapsed*)))
  (set-pref :all-resources-collapsed
            (loop for k being the hash-keys of *all-resources-collapsed*
                  collect (cons k (gethash k *all-resources-collapsed*))))
  (when (boundp '*application-frame*)
    (clim:redisplay-frame-panes *application-frame* :force-p t)))

(defun show-all-resources-internal ()
  "Display all assets with colored type squares, title-cased names,
    hex IDs, D/P/A checkboxes. Click name for action menu.
    Assets absent from disk appear in red.
    Click kind/locale headings to collapse/expand sections."
  (let ((*trace-output* (make-string-output-stream)))
    ;; Initialize state plist if needed
    (unless (and *all-resources-state*
                 (getf *all-resources-state* :collapsed))
      ;; Restore collapsed state from prefs, or default all to collapsed
      (let ((saved (get-pref :all-resources-collapsed)))
        (if saved
            (dolist (pair saved)
              (setf (gethash (car pair) *all-resources-collapsed*) (cdr pair)))
            (dolist (k '("Scripts" "Songs" "Maps" "Characters" "Boats" "Blobs"
                         "Tilesets" "Sprite Sheets" "Object Prototypes" "Classes" "Routines"))
              (setf (gethash k *all-resources-collapsed*) t))))
      (setf *all-resources-state*
            (list :collapsed *all-resources-collapsed*
                  :current-kind nil
                  :current-locale nil)))
    (let* ((all-assets (collect-all-assets))
           (collapsed (getf *all-resources-state* :collapsed))
           (last-kind nil)
           (last-locale nil)
           skip-kind
           skip-locale
           locale
           (frame (when (boundp '*application-frame*)
                    *application-frame*)))
      (flet ((section-kind (kn) (or kn "Special Resources"))
             (section-key (kn) (or kn "")))
        (terpri)
        (dolist (entry all-assets)
          (destructuring-bind (moniker builds kind-name asset-id hex-str present-p full-path)
              entry
            (let ((sk (section-kind kind-name)))
              ;; --- Kind heading ---
              (unless (string-equal sk last-kind)
                (setf last-kind sk
                      last-locale nil
                      skip-kind (gethash sk collapsed)
                      skip-locale nil)
                (terpri)
                (let* ((pane clim-simple-echo::*echo-pane*)
                       (pane-width (clim:bounding-rectangle-width
                                    (clim:sheet-region pane))))
                  (clim:surrounding-output-with-border (pane
                                                        :background (color-for-asset-kind kind-name))
                    (clim:with-drawing-options (*standard-output* :ink (clim:make-rgb-color 1 1 1))
                      (clim:with-text-face (*standard-output* :bold)
                        (clim:with-text-size (*standard-output* :larger)
                          (clim:with-output-as-presentation
                              (*standard-output* sk 'assets-section-header)
                            (format *standard-output* "~a ~a"
                                    (if skip-kind "▶ " "▼ ") sk)
                            ;; Stretch to full pane width so border fills margin-to-margin
                            (clim:stream-set-cursor-position
                             *standard-output*
                             (- pane-width 5)
                             (nth-value 1 (clim:stream-cursor-position
                                           *standard-output*)))))))))
                (terpri)
                (terpri))
              ;; --- Skip if kind collapsed ---
              (unless skip-kind
                ;; --- Locale group header for Scripts and Maps ---
                (let* ((parts (split-sequence #\/ moniker))
                       (this-locale (when (member kind-name '("Scripts" "Maps") :test #'string-equal)
                                      (and (> (length parts) 2)
                                           (cl-change-case:title-case (second parts))))))
                  (when (and this-locale (not (string-equal this-locale last-locale)))
                    (setf last-locale this-locale)
                    (let ((locale-key (format nil "~a/~a" (section-key kind-name) this-locale)))
                      (setf skip-locale (gethash locale-key collapsed))
                      (let* ((pane clim-simple-echo::*echo-pane*)
                             (pane-width (clim:bounding-rectangle-width
                                          (clim:sheet-region pane))))
                        (clim:surrounding-output-with-border (pane
                                                              :background (color-for-asset-kind kind-name))
                          (clim:with-drawing-options (*standard-output* :ink (clim:make-rgb-color 1 1 1))
                            (clim:with-output-as-presentation
                                (*standard-output* locale-key 'assets-section-header)
                              (clim:with-text-size (*standard-output* :smaller)
                                (format *standard-output* "~a     ~a"
                                        (if skip-locale "▶ " "▼ ") this-locale)
                                ;; Stretch to full pane width
                                (clim:stream-set-cursor-position
                                 *standard-output*
                                 (- pane-width 5)
                                 (nth-value 1 (clim:stream-cursor-position
                                               *standard-output*))))))))
                      (terpri))))
                ;; --- Entry display (skip if locale collapsed) ---
                (unless skip-locale
                  (let* ((parts (split-sequence #\/ moniker))
                         (basename (car (last parts)))
                         (kind-key (kind-by-name kind-name))
                         (display-name
                           (case kind-key
                             ((:script :song :blob)
                              (let ((name (cl-change-case:title-case
                                           (cl-ppcre:regex-replace "\\bDont\\b" basename "Don't"))))
                                (format nil "~c~a~c" (code-char #x201C) name (code-char #x201D))))
                             (:map
                              (cl-change-case:title-case
                               (cl-ppcre:regex-replace "\\bDont\\b" basename "Don't")))
                             (t basename)))
                         (locale-parts (butlast (rest parts))))
                    (setf locale (when locale-parts
                                   (cl-change-case:title-case (first locale-parts))))
                    (clim:with-output-as-presentation
                        (*standard-output* (list moniker builds kind-name asset-id hex-str present-p
                                                 full-path)
                                           'unified-asset-entry)
                      (clim:surrounding-output-with-border (clim-simple-echo::*echo-pane*)
                         (let* ((pane clim-simple-echo::*echo-pane*)
                               (pane-width (clim:bounding-rectangle-width (clim:sheet-region pane)))
                               (start-y (nth-value 1 (clim:stream-cursor-position *standard-output*)))
                               (badge-h 40)
                               (badge-w (floor (* badge-h 1.618)))
                               (red (or (not present-p) (null builds))))
                          ;; Kind badge — golden-ratio rectangle spanning full entry height
                          (when kind-name
                            (clim:draw-rectangle* pane 0 start-y badge-w (+ start-y badge-h)
                                                  :ink (color-for-asset-kind kind-name) :filled t)
                            (clim:stream-set-cursor-position *standard-output* 8 (+ start-y 10))
                            (clim:with-drawing-options (*standard-output* :ink (clim:make-rgb-color 1 1 1))
                              (clim:with-text-face (*standard-output* :bold)
                                (princ kind-name *standard-output*))))
                          ;; Title at fixed offset after kind badge
                          (let ((title-x (if kind-name 60 4)))
                            (clim:stream-set-cursor-position *standard-output* title-x start-y)
                            (flet ((present-name (name)
                                     (case kind-key
                                       (:map (clim:with-text-style
                                                 (*standard-output*
                                                  (clim:make-text-style :serif :roman :normal))
                                               (let* ((str (princ-to-string name))
                                                      (len (length str)))
                                                 (if (and (plusp len) (digit-char-p (char str (1- len))))
                                                     (let ((split (position-if-not #'digit-char-p str
                                                                                   :from-end t
                                                                                   :end (1- len))))
                                                       (if split
                                                           (let ((prefix (subseq str 0 (1+ split)))
                                                                 (digits (subseq str (1+ split))))
                                                             (if red
                                                                 (clim:with-drawing-options
                                                                     (*standard-output* :ink (clim:make-rgb-color 0.8 0 0))
                                                                   (princ prefix *standard-output*))
                                                                 (princ prefix *standard-output*))
                                                             (write-string "  " *standard-output*)
                                                             (clim:with-drawing-options
                                                                 (*standard-output* :ink (clim:make-gray-color 0.25))
                                                               (princ digits *standard-output*)))
                                                           (progn
                                                             (write-string "  " *standard-output*)
                                                             (clim:with-drawing-options
                                                                 (*standard-output* :ink (clim:make-gray-color 0.25))
                                                               (princ str *standard-output*)))))
                                                     (if red
                                                         (clim:with-drawing-options
                                                             (*standard-output* :ink (clim:make-rgb-color 0.8 0 0))
                                                           (princ str *standard-output*))
                                                         (princ str *standard-output*))))))
                                       (t (clim:with-text-style
                                              (*standard-output*
                                               (clim:make-text-style :serif :italic :normal))
                                            (if red
                                                (clim:with-drawing-options
                                                    (*standard-output* :ink (clim:make-rgb-color 0.8 0 0))
                                                  (princ name *standard-output*))
                                                (princ name *standard-output*)))))))
                              (present-name display-name))
                            ;; Subtitle (locale/folder) — only when locale is non-empty
                            (when locale
                              (clim:stream-set-cursor-position *standard-output* title-x (+ start-y 18))
                              (clim:with-text-size (*standard-output* :smaller)
                                (clim:with-drawing-options (*standard-output* :ink (clim:make-gray-color 0.75))
                                  (princ locale *standard-output*)))))
                          ;; Right column: hex ID and checkboxes (only for pool assets)
                          (when (or hex-str builds)
                            (let* ((right-offset (- pane-width 80))
                                   (id-y start-y))
                              (clim:stream-set-cursor-position *standard-output* right-offset id-y)
                              (format *standard-output* "~@[~a~]" hex-str)
                              (clim:stream-set-cursor-position *standard-output* right-offset (+ id-y 14))
                              (clim:with-output-as-presentation
                                  (*standard-output* (list moniker builds kind-name asset-id hex-str present-p
                                                           full-path #\D)
                                                     'build-checkbox)
                                (if (and builds (member "Demo" builds :test 'string-equal))
                                    (clim:with-drawing-options (*standard-output* :ink (clim:make-rgb-color 0 0.4 0))
                                      (princ "☑D" *standard-output*))
                                    (princ "☐D" *standard-output*)))
                              (clim:stream-set-cursor-position *standard-output* right-offset (+ id-y 28))
                              (clim:with-output-as-presentation
                                  (*standard-output* (list moniker builds kind-name asset-id hex-str present-p
                                                           full-path #\P)
                                                     'build-checkbox)
                                (if (and builds (member "Public" builds :test 'string-equal))
                                    (clim:with-drawing-options (*standard-output* :ink (clim:make-rgb-color 0 0.4 0))
                                      (princ "☑P" *standard-output*))
                                    (princ "☐P" *standard-output*)))
                              (clim:stream-set-cursor-position *standard-output* right-offset (+ id-y 42))
                              (clim:with-output-as-presentation
                                  (*standard-output* (list moniker builds kind-name asset-id hex-str present-p
                                                           full-path #\A)
                                                     'build-checkbox)
                                (if (and builds (member "AA" builds :test 'string-equal))
                                    (clim:with-drawing-options (*standard-output* :ink (clim:make-rgb-color 0 0.4 0))
                                      (princ "☑A" *standard-output*))
                                    (princ "☐A" *standard-output*)))))
                          ;; Stretch border to full pane width
                          (clim:stream-set-cursor-position
                           *standard-output*
                           (- pane-width 5)
                           (nth-value 1 (clim:stream-cursor-position *standard-output*)))
                          (terpri))
                        (when frame
                          (clim:redisplay-frame-panes frame :force-p t))))))))))))))
    
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
      (destructuring-bind (moniker builds kind-name asset-id hex-str present-p full-path flag-char)
          entry
        (declare (ignore kind-name asset-id hex-str present-p full-path))
        (%toggle-build-flag moniker builds flag-char)))
    
