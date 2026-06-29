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
                (format *query-io* "~&Emacs server failed to start. Please start Emacs manually with 'server-start'.~%")))))))

(defun special-resource-whitelist ()
  "Return the list of moniker names (without kind prefix) allowed in Special Resources.
Computed lazily because MACHINE-DIRECTORY-NAME needs *MACHINE* to be bound."
  (list (format nil "Project.~a" (machine-directory-name))
        "Item Names"
        "Flag Names"
        "Key Names"
        "SpeakJet.dic"))

(defun reload-assets-index ()
  "Force a reload of Source/Assets.index and the maps index table."
  (setf *assets-list* nil
        *asset-ids-seen* nil
        *maps-ids* nil)
  (read-assets-list)
  (format *query-io* "~&Assets index reloaded.~%"))

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
       ;; .forth files moved to Routines/Forth Scripts
       (scan-fs #p"Source/Scripts/**/*.forth"
                (lambda (f) (format nil "Routines/Forth Scripts/~a" (pathname-name f))))
       (scan-fs #p"Source/Songs/*.mscz")
       ;; New filesystem-only types: construct moniker as Kind/Name
       (scan-fs #p"Source/Maps/Tiles/*.tsx"
                (lambda (f) (format nil "Tilesets/~a" (pathname-name f))))
       (scan-fs #p"Source/Objects/*.json"
                (lambda (f) (format nil "Object Prototypes/~a" (pathname-name f))))
       (scan-fs #p"Source/Classes/*.cob"
                (lambda (f) (format nil "Classes/~a" (pathname-name f))))
       (scan-fs #p"Source/Maps/RunCommands/*.cob"
                (lambda (f) (format nil "Routines/Run Commands/~a" (pathname-name f))))
       (scan-fs #p"Source/Maps/RunCommands/*.bas"
                (lambda (f) (format nil "Routines/Run Commands/~a" (pathname-name f))))
       (scan-fs #p"Source/Maps/RunCommands/*.pas"
                (lambda (f) (format nil "Routines/Run Commands/~a" (pathname-name f))))
(scan-fs #p"Source/Art/*.art"
                (lambda (f) (format nil "Sprite Sheets/~a" (pathname-name f))))
       ;; Instruments from Orchestration.ods
       (ignore-errors
        (load-orchestration)
        (dolist (row *orchestration*)
          (let* ((iname (getf row :instrument-name))
                 (moniker (format nil "Instruments/~a" iname)))
            (unless (gethash moniker seen)
              (setf (gethash moniker seen) t)
              (push (list moniker nil "Instruments" nil nil t nil) results)))))
       ;; Special/computed resources (Items, Flags, Keys) — null kind so they
       ;; sort to the top, have no collapse heading, and render with white icon.
       (ignore-errors
        (load-equipment-index)
        (dolist (row *equipment-index*)
          (let* ((iname (getf row :name))
                 (moniker (format nil "Items/~a" iname)))
            (unless (gethash moniker seen)
              (setf (gethash moniker seen) t)
              (push (list moniker nil nil nil nil t nil) results)))))
       (ignore-errors
        (load-flags)
        (dolist (flag *flags*)
          (let* ((fname (getf flag :name))
                 (moniker (format nil "Flags/~a" fname)))
            (unless (gethash moniker seen)
              (setf (gethash moniker seen) t)
              (push (list moniker nil nil nil nil t nil) results)))))
       (ignore-errors
        (load-keys)
        (dolist (key *keys*)
          (let* ((kname (getf key :name))
                 (moniker (format nil "Keys/~a" kname)))
            (unless (gethash moniker seen)
              (setf (gethash moniker seen) t)
              (push (list moniker nil nil nil nil t nil) results)))))
       ;; AtariVox Dictionary from SpeakJet.dic
       (ignore-errors
        (let* ((f (merge-pathnames "Source/SpeakJet.dic" (uiop:getcwd))))
          (when (probe-file f)
            (let ((moniker "AtariVox Dictionary/SpeakJet.dic"))
              (unless (gethash moniker seen)
                (setf (gethash moniker seen) t)
                (push (list moniker nil "AtariVox Dictionary" nil nil t (namestring (truename f))) results))))))
       ;; Special Resources: only whitelisted monikers with nil kind-name
      ;; (no heading, no badge, sorts first)
      (dolist (spec (special-resource-whitelist))
        (let* ((f (merge-pathnames (format nil "~a.json" spec)
                                   (merge-pathnames #p"Source/" (uiop:getcwd))))
               (moniker (format nil "Special Resources/~a" (pathname-name f))))
          (when (and (probe-file f)
                     (not (gethash moniker seen)))
            (setf (gethash moniker seen) t)
            (push (list moniker nil nil nil nil t (namestring (truename f)))
                  results))))
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
        ((string-equal kind-name "Instruments") "0.6 0.2 0.8 setrgbcolor")
        ((string-equal kind-name "Items") "0.8 0.6 0.0 setrgbcolor")
        ((string-equal kind-name "Flags") "0.9 0.3 0.3 setrgbcolor")
        ((string-equal kind-name "Keys") "0.3 0.7 0.7 setrgbcolor")
        ((string-equal kind-name "AtariVox Dictionary") "0.5 0.5 0.9 setrgbcolor")
        (t "0.3 0.3 0.3 setrgbcolor")))

(defun hsl->rgb (h s l)
  "Convert HSL (H in [0,360], S/L in [0,1]) to CLIM RGB color."
  (let* ((c (* (- 1 (abs (- (* 2 l) 1))) s))
         (x (* c (- 1 (abs (- (mod (/ h 60) 2) 1)))))
         (m (- l (/ c 2)))
         (r1 0) (g1 0) (b1 0))
    (cond ((< h 60)   (setf r1 c g1 x b1 0))
          ((< h 120)  (setf r1 x g1 c b1 0))
          ((< h 180)  (setf r1 0 g1 c b1 x))
          ((< h 240)  (setf r1 0 g1 x b1 c))
          ((< h 300)  (setf r1 x g1 0 b1 c))
          (t          (setf r1 c g1 0 b1 x)))
    (clim:make-rgb-color (+ r1 m) (+ g1 m) (+ b1 m))))

(defun color-for-asset-kind (kind-name)
  "Return a CLIM color for the KIND-NAME.
   NIL gets white (no badge).
   Otherwise, maps alphabetically across a full H=0-360 rainbow
   with S=0.73, L=0.57 (from Royal Blue HSL)."
  (if (null kind-name)
      (clim:make-rgb-color 1 1 1)
      (let* ((kinds '("Blobs" "Boats" "Characters" "Classes" "Flags" "Instruments" "Items" "Keys" "Maps"
                      "Object Prototypes" "Routines" "Scripts" "Songs"
                      "Sprite Sheets" "Tilesets"))
             (pos (position kind-name kinds :test #'string-equal))
             (n (length kinds)))
        (if pos
            (hsl->rgb (* pos (/ 360 (1- n))) 0.73 0.57)
            (clim:make-rgb-color 0.3 0.3 0.3)))))

(defun write-all-resources-ps (path)
  "Generate a PostScript document at PATH with the full Assets Index.
   Uses proper kind badges, colored section headings, D/P/A checkboxes,
   typographical quotes, bordered entries, and pagination."
(let* ((all-assets (let ((*standard-output* (make-string-output-stream))
                           (*trace-output* *standard-output*))
                      (collect-all-assets)))
          (kind-order '("Scripts" "Songs" "Maps" "Characters" "Boats" "Blobs"
                        "Tilesets" "Sprite Sheets" "Object Prototypes" "Classes" "Routines"
                        "Instruments" "Items" "Flags" "Keys" "AtariVox Dictionary"
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

(defvar *assets-index-filter* nil
  "When non-NIL, a string used to filter All Resources entries by moniker or display name.")

(clim:define-presentation-type assets-section-header () :inherit-from 'string)

(clim:define-command (com-toggle-section-header :command-table clim-internals::global-command-table
                                                :menu t :name t)
    ((section 'assets-section-header :gesture :select))
  (setf (gethash section *all-resources-collapsed*)
        (not (gethash section *all-resources-collapsed*)))
  (set-pref :all-resources-collapsed
            (loop for k being the hash-keys of *all-resources-collapsed*
                  collect (cons k (if (gethash k *all-resources-collapsed*) "true" "false"))))
  (when (boundp '*application-frame*)
    (clim:redisplay-frame-panes *application-frame* :force-p t)))

(clim:define-command (com-section-header-menu :command-table clim-internals::global-command-table
                                              :menu t :name t)
    ((section 'assets-section-header :gesture :menu))
  (let* ((parts (split-sequence #\/ section))
         (kind-name (first parts))
         (kind-key (ignore-errors (kind-by-name kind-name)))
         (locale (when (> (length parts) 1) (second parts)))
         (items nil))
    (macrolet (($ (label . body) `(push (list ,label (lambda () ,@body)) items))
               (--- () '(push nil items)))
      ($ "Open in File Manager..."
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
                               :output nil ))))
      (when (and (eql kind-key :map) locale)
        (---)
        ($ (format nil "New Map in ~a..." locale)
           (format t "~&New map creation not yet implemented.~%"))
        ($ (format nil "Edit ~a Atlas..." locale)
           (let ((path (merge-pathnames
                        (format nil "Source/Maps/~a/~a.tex" locale locale)
                        (uiop:getcwd))))
             (when (probe-file path)
               (open-in-emacs (truename path))))))
      (unless locale
        (---)
        ($ "New..."
           (format t "~&New ~a resource creation not yet implemented.~%" kind-name))))
    (clim:menu-choose (reverse items) :label (format nil "~a" section))))

(defun compute-asset-display-name (basename kind-key)
  "Return the display name for an asset entry."
  (case kind-key
    ((:script :song :blob)
     (format nil "~c~a~c" (code-char #x201C)
             (cl-change-case:title-case
              (cl-ppcre:regex-replace "\\bDont\\b" basename "Don't"))
             (code-char #x201D)))
    ((:map :character)
     (cl-change-case:title-case
      (cl-ppcre:regex-replace "\\bDont\\b" basename "Don't")))
    (t (cl-change-case:title-case
        (cl-ppcre:regex-replace "\\bDont\\b" basename "Don't")))))

(defun present-asset-name (stream name kind-key red)
  "Output NAME on STREAM with font and color appropriate for KIND-KEY."
  (case kind-key
    (:map
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
                       (princ str stream)))))
             (if red
                 (clim:with-drawing-options (stream :ink (clim:make-rgb-color 0.8 0 0))
                   (princ str stream))
                 (princ str stream))))))
    (t
     (clim:with-text-style (stream (clim:make-text-style :serif :italic :normal))
       (if red
           (clim:with-drawing-options (stream :ink (clim:make-rgb-color 0.8 0 0))
             (princ name stream))
           (princ name stream))))))

(defun render-kind-badge (stream the-pane start-y end-y badge-w kind-name)
  "Draw the colored kind badge rectangle full height of entry, with white kind-name text."
  (clim:draw-rectangle* the-pane 0 start-y badge-w end-y
                        :ink (color-for-asset-kind kind-name) :filled t)
  (clim:stream-set-cursor-position stream 8 (+ start-y 10))
  (clim:with-drawing-options (stream :ink (clim:make-rgb-color 1 1 1))
    (clim:with-text-face (stream :bold)
      (princ kind-name stream))))

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
              (let ((e-kind (third e))
                    (e-parts (split-sequence #\/ (first e))))
                (and (string-equal e-kind kind-name)
                     (or (null subsection-key)
                         (and (> (length e-parts) 2)
                              (string-equal (second e-parts) subsection-key))))))
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

(defun render-locale-heading (the-pane pane-width kind-name subsection-key this-subsection skip-subsection all-assets)
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
        (princ (if skip-subsection "▶ " "▼ ") *standard-output*)
        (princ this-subsection *standard-output*)))
    ;; Show count in right margin when collapsed
    (when skip-subsection
      (let* ((count (count-kind-assets kind-name this-subsection all-assets))
             (count-str (format nil "(~d)" count))
             (x-pos (- pane-width (clim:text-size the-pane count-str) 16)))
        (clim:stream-set-cursor-position *standard-output* x-pos (+ cursor-y 2))
        (clim:with-drawing-options (*standard-output* :ink (clim:make-rgb-color 1 1 1))
          (princ count-str *standard-output*))))
    (clim:stream-set-cursor-position *standard-output* 0 (+ cursor-y heading-h))))

(defun asset-matches-filter-p (entry filter)
  "Return T if ENTRY (moniker builds kind-name ...) matches FILTER string.
Matches against the full moniker or the asset base name, case-insensitive."
  (destructuring-bind (moniker builds kind-name asset-id hex-str present-p full-path)
      entry
    (declare (ignore builds asset-id hex-str present-p full-path))
    (let ((needle (string-downcase filter))
          (basename (car (last (split-sequence #\/ moniker)))))
      (or (search needle (string-downcase moniker))
          (search needle (string-downcase
                          (compute-asset-display-name basename (kind-by-name kind-name))))))))

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
        (setf (gethash k *all-resources-collapsed*) nil))
      ;; Override with persisted: "true" = collapsed (t), "false" = expanded (nil)
      (when saved
        (dolist (pair saved)
          (setf (gethash (car pair) *all-resources-collapsed*)
                (string-equal (cdr pair) "true")))))
    (setf *all-resources-state*
          (list :collapsed *all-resources-collapsed*
                :current-kind nil
                :current-locale nil)))
  (let* ((all-assets (let ((*standard-output* (make-string-output-stream))
                           (*trace-output* (make-string-output-stream)))
                        (collect-all-assets)))
         (all-assets (if (and (stringp *assets-index-filter*)
                              (plusp (length *assets-index-filter*)))
                         (remove-if-not (lambda (entry)
                                          (asset-matches-filter-p entry *assets-index-filter*))
                                        all-assets)
                         all-assets))
         (collapsed (getf *all-resources-state* :collapsed))
         (last-kind nil)
         (last-locale nil)
         skip-kind
         skip-locale
         locale)
    (flet ((section-kind (kn) (or kn "Special Resources"))
           (section-key (kn) (or kn "")))
      (terpri)
      (dolist (entry all-assets)
        (destructuring-bind (moniker builds kind-name asset-id hex-str present-p full-path)
            entry
          (let ((sk (section-kind kind-name)))
            ;; --- Kind heading (skip for Special Resources: nil kind-name) ---
(unless (string-equal sk last-kind)
               (setf last-kind sk
                     last-locale nil
                     skip-kind (gethash sk collapsed)
                     skip-locale nil)
(unless (null kind-name)
                  (let* ((the-pane clim-simple-echo::*echo-pane*)
                         (pane-width (clim:bounding-rectangle-width
                                      (clim:sheet-region the-pane))))
                    (render-kind-heading the-pane pane-width kind-name sk skip-kind all-assets)))))
            ;; --- Skip if kind collapsed ---
            (unless skip-kind
;; --- Locale group header for Scripts, Maps, and Routines ---
               (let* ((parts (split-sequence #\/ moniker))
                      (this-locale (when (member kind-name '("Scripts" "Maps" "Routines") :test #'string-equal)
                                     (and (> (length parts) 2)
                                          (cl-change-case:title-case (second parts))))))
(when (and this-locale (not (string-equal this-locale last-locale)))
                       (setf last-locale this-locale)
                      (let ((subsection-key (format nil "~a/~a" (section-key kind-name) this-locale)))
                        (setf skip-locale (gethash subsection-key collapsed))
                        (let* ((the-pane clim-simple-echo::*echo-pane*)
                               (pane-width (clim:bounding-rectangle-width
                                            (clim:sheet-region the-pane))))
                          (render-locale-heading the-pane pane-width kind-name subsection-key this-locale skip-locale all-assets)))))
;; --- Entry display (skip if locale collapsed) ---
(unless skip-locale
                  (let* ((parts (split-sequence #\/ moniker))
                         (basename (car (last parts)))
                         (kind-key (kind-by-name kind-name))
                         (display-name (compute-asset-display-name basename kind-key))
                         (locale-parts (butlast (rest parts)))
                         (subtype (when (and (string-equal kind-name "Routines") (> (length parts) 2))
                                    (cl-change-case:title-case (second parts)))))
                    (setf locale (when locale-parts
                                   (cl-change-case:title-case (first locale-parts))))
(clim:with-output-as-presentation
                        (clim-simple-echo::*echo-pane* (list moniker builds kind-name asset-id hex-str present-p
                                                          full-path)
                                                      'unified-asset-entry)
                      (clim:surrounding-output-with-border (clim-simple-echo::*echo-pane*)
                        (let* ((the-pane clim-simple-echo::*echo-pane*)
                               (pane-width (clim:bounding-rectangle-width (clim:sheet-region the-pane)))
                               (start-y (nth-value 1 (clim:stream-cursor-position *standard-output*)))
                               (badge-h 40)
                               (badge-w (floor (* badge-h 1.618)))
                               (red (not present-p))
                               (title-x (if kind-name (+ badge-w 8) 4))
                               (target-y (+ start-y 48))
                               (id-x (- pane-width 120)))
                          ;; Kind badge - full entry height
                          (when kind-name
                            (render-kind-badge *standard-output* the-pane start-y target-y badge-w kind-name))
                          ;; Title
                          (when kind-name (write-string " " *standard-output*))
                          (clim:stream-set-cursor-position *standard-output* title-x start-y)
                          (present-asset-name *standard-output* display-name kind-key red)
                          ;; Subtitle (locale or routine subtype)
                          (when (or locale subtype)
                            (clim:stream-set-cursor-position *standard-output* (+ title-x 16) (+ start-y 18))
                            (clim:with-text-size (*standard-output* :smaller)
                              (clim:with-drawing-options (*standard-output* :ink (clim:make-gray-color 0.75))
                                (princ (or locale subtype) *standard-output*))))
                          ;; Right column: hex ID and D/P/A checkboxes underneath
                          (when (or hex-str builds)
                            (clim:stream-set-cursor-position *standard-output* id-x start-y)
                            (format *standard-output* "~@[~a~]" hex-str)
                            (when builds
                              (clim:stream-set-cursor-position *standard-output* id-x (+ start-y 16))
                              (render-build-checkboxes *standard-output* moniker builds kind-name
                                                       asset-id hex-str present-p full-path)))
                          ;; Minimum 3-line entry height
                          (loop while (< (nth-value 1 (clim:stream-cursor-position *standard-output*))
                                         target-y)
                                do (terpri))
                          ;; Stretch border to full pane width
                          (clim:stream-set-cursor-position
                           *standard-output*
                           (- pane-width 5)
                           (nth-value 1 (clim:stream-cursor-position *standard-output*)))
                          (terpri))))))))))))

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

(clim:define-command (com-reload-assets-index :command-table clim-simple-echo::echo-resource-menu
                                               :menu t :name t)
    ()
  (reload-assets-index)
  (clim:redisplay-frame-panes *application-frame* :force-p t))

(clim:define-command (com-rescan-project-folder :command-table clim-simple-echo::echo-resource-menu
                                                 :menu t :name t)
    ()
  (clim:redisplay-frame-panes *application-frame* :force-p t))

;; --- Asset action menu implementation ---

(defun %show-asset-context-menu (moniker builds kind-name asset-id hex-str present-p full-path)
  "Show context menu for an asset entry with appropriate actions."
  (let* ((items nil)
         (kind-key (kind-by-name kind-name)))
    (macrolet (($ (label . body) `(push (cons ,label (lambda () ,@body)) items))
               (--- () '(push nil items)))
      ;; Universal actions (when full-path exists)
      (when full-path
        ($ "Reveal in File Manager..."
           (uiop:run-program (list "xdg-open" (namestring (truename (make-pathname :defaults full-path
                                                                                   :name nil :type nil))))
                             :output nil))
        (---))
      ;; Type-specific actions
      (case kind-key
        (:song
         ($ "Open in MuseScore..."
            (uiop:run-program (list "musescore" full-path) :output nil ))
         ($ "Save as PDF..."
            (uiop:run-program (list "musescore" "-o" (format nil "~a.pdf" (substitute #\/ #\/ full-path)) full-path)
                              :output nil)))
        (:script
         ($ "Open in Emacs..."
            (open-in-emacs (truename full-path)))
         ($ "Save as PDF..."
            (let ((ps-path (format nil "~a.ps" (substitute #\/ #\/ full-path)))
                  (pdf-path (format nil "~a.pdf" (substitute #\/ #\/ full-path))))
              (uiop:run-program (list "emacs" "--batch" "--eval" 
                                      (format nil "(progn (require 'fountain) (find-file ~s) (fountain-export-to-pdf ~s ~s))"
                                              full-path full-path pdf-path))
                                :output nil ))))
        (:map
         ($ "Inspect..."
            (open-map-inspector full-path))
         ($ "Open in Tiled..."
            (uiop:run-program (list "tiled" full-path) :output nil )))
        (:tileset
         ($ "Inspect..."
            (open-tileset-inspector full-path)))
        (:sprite-sheet
         ($ "Inspect..."
            (open-sprite-sheet-inspector full-path)))
        (:character
         ($ "Inspect..."
            (open-character-inspector moniker)))
        (:boat
         ($ "Inspect..."
            (open-boat-inspector moniker)))
        (:blob
         ($ "Open in GIMP..."
            (when full-path
              (uiop:run-program (list "gimp" full-path) :output nil ))))
        (:class
         ($ "Inspect..."
            (oops-class-inspector)))
        (:object-prototype
         ($ "Open in Emacs..."
            (open-in-emacs (truename full-path))))
        (:routine
         ($ "Open in Emacs..."
            (open-in-emacs (truename full-path))))
        (:instruments
         ($ "Open Instrument Inspector..."
            (open-instrument-inspector moniker)))
        (:items
         ($ "Open Items Index..."
            (open-items-index)))
        (:flags
         ($ "Open Flags Index..."
            (open-flags-index)))
        (:keys
         ($ "Open Keys Index..."
            (open-keys-index)))
        (:atari-vox-dictionary
         ($ "Open AtariVox Dictionary..."
            (open-atari-vox-dictionary-inspector)))
        (t nil))
      (---)
      ;; Toggle build flags (for indexed assets)
      (when builds
        ($ (format nil "~:[Disable~;Enable~] Demo build" (member "Demo" builds :test #'string-equal))
           (%toggle-build-flag moniker builds #\D))
        ($ (format nil "~:[Disable~;Enable~] Public build" (member "Public" builds :test #'string-equal))
           (%toggle-build-flag moniker builds #\P))
        ($ (format nil "~:[Disable~;Enable~] AA build" (member "AA" builds :test #'string-equal))
           (%toggle-build-flag moniker builds #\A))
        (---))
      ;; Copy info
      ($ "Copy moniker..."
         (copy-to-clipboard moniker))
      ($ "Copy full path..."
         (when full-path
           (copy-to-clipboard full-path)))
      ($ "Copy hex ID..."
         (when hex-str
           (copy-to-clipboard hex-str))))
    (let ((chosen-fn (clim:menu-choose (reverse items) :label (format nil "Asset: ~a" moniker))))
      (when chosen-fn (funcall chosen-fn)))))

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
    (format *query-io* "~&Toggled ~c for ~a: ~{~a~^, ~}~%" flag-char moniker new-builds)
    (clim:redisplay-frame-panes *application-frame* :force-p t)))

;; --- Additional commands for Resource menu ---

(clim:define-command (com-find-in-assets :command-table clim-internals::global-command-table
                                         :menu t :name t)
    ()
  "Prompt for a filter string and apply it to the All Resources display."
  (let* ((filter (clim:accept 'string :prompt "Filter assets (substring match):" :default *assets-index-filter*)))
    (setf *assets-index-filter* filter)
    (when (boundp '*application-frame*)
      (clim:redisplay-frame-panes *application-frame* :force-p t))))

;; --- Migrated Launcher commands ---

(clim:define-command (com-edit-project.json :command-table clim-internals::global-command-table
                                             :menu t :name t)
    ()
  "Edit the Project.<machine>.json configuration file in Emacs."
  (if (boundp '*machine*)
      (let* ((machine-dir (machine-directory-name))
             (project-file (format nil "Project.~a.json" machine-dir))
             (project-path project-file))
        (clim-sys:make-process
         (lambda ()
           (handler-case (climacs:edit-file (namestring project-path)
                                            :process-name (format nil "Editing Project (~a)" machine-dir))
             (error (e)
               (format *query-io* "~&Climacs error: ~a~%" e))))
         :name (format nil "Editing Project (~a)" machine-dir)))
      (load-project.json nil #'(lambda () (funcall (symbol-function 'com-edit-project.json))))))

(clim:define-command (com-edit-skyline-config-prefs :command-table clim-internals::global-command-table
                                                     :menu t :name t)
    ()
  "Edit Skyline-Tool preferences file in Emacs."
  (let* ((prefs-file (make-pathname :directory (list :relative ".config" "Skyline-Tool"
                                                      *game-title*
                                                      (machine-directory-name))
                                     :defaults (user-homedir-pathname)
                                     :name "Preferences" :type "lisp")))
    (open-in-emacs prefs-file)))

(clim:define-command (com-quit-skyline-tool :command-table clim-internals::global-command-table
                                              :menu t :name t)
    ()
  "Exit Skyline-Tool."
  (bye))

;; --- Migrated Lisp / Tools / Debug commands ---

(defun %menu-run (name fn)
  "Run FN in a new process named NAME."
  (clim-sys:make-process (lambda () (funcall fn)) :name (string-capitalize name)))

(clim:define-command (com-run-repl :command-table clim-internals::global-command-table
                                    :menu t :name t) ()
  "Open a Lisp REPL."
  (run-repl))

(clim:define-command (com-show-lisp-room :command-table clim-internals::global-command-table
                                          :menu t :name t) ()
  "Show Lisp memory usage."
  (%menu-run "Lisp Room" #'show-lisp-room))

(clim:define-command (com-reload-skyline-tool-from-sources :command-table clim-internals::global-command-table
                                                            :menu t :name t) ()
  "Recompile and reload Skyline-Tool from sources."
  (%menu-run "Recompile" #'reload-skyline-tool-from-sources))

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
                                                  :menu t :name t) ()
  "Show an animation buffer from a core dump."
  (let ((index (clim:accept 'integer :prompt "Animation buffer index:" :default 0)))
    (show-animation-buffer index)))

(clim:define-command (com-show-decal :command-table clim-internals::global-command-table
                                       :menu t :name t) ()
  "Show a decal from a core dump."
  (let ((index (clim:accept 'integer :prompt "Decal index:" :default 0)))
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

