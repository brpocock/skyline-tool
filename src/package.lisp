(cl:defpackage :skyline-tool
  (:use :cl :alexandria :serapeum :split-sequence
   :local-time :cl-change-case :parse-number :bordeaux-threads)
  (:import-from :uiop
                #:run-program
                #:split-string)
  (:shadow #:range)
  (:shadowing-import-from :serapeum #:partition)
  ;; Export ONLY external call points. Unit tests will have to use ::
  (:export #:command
           #:c
           #:about-skyline-tool
           #:run-for-port
           #:run-gui
           #:run-repl
            #:skyline-tool-icon
            #:prefs-pathname
            #:load-prefs
            #:save-prefs
            #:get-pref
            #:set-pref))

(in-package :skyline-tool)

()

(defvar *project.json*)
(defvar *game-title*)
(defvar *part-number*)
(defvar *studio*)
(defvar *publisher*)
(defvar *machine*)
(defvar *sound*)
(defvar *common-palette*)
(defvar *default-skin-color*)
(defvar *default-hair-color*)
(defvar *default-clothes-color*)
(defparameter *region* :ntsc
  "Default TV standard for palette and color conversion (:ntsc, :pal, :secam).")


(defun skyline-tool-icon (&key (resource nil))
  "Return an icon pattern for CLIM frames.
   If RESOURCE is provided (a keyword), tries to load a resource-specific
   icon (e.g. Tools/skyline-tool-icon-ANIMATION-SEQUENCE-EDITOR-128.png),
   falling back to the generic skyline-tool icon."
  (delete nil
          (mapcar (lambda (name)
                    (let ((path (asdf:system-relative-pathname :skyline-tool name)))
                      (when (probe-file path)
                        (clim:make-pattern-from-bitmap-file path))))
                  (append
                   ;; Resource-specific candidates first
                   (when resource
                     (list (format nil "../Tools/skyline-tool-icon-~(~a~)-128.png" resource)
                           (format nil "../Tools/skyline-tool-icon-~(~a~)-64.png" resource)))
                   ;; Fallback generic icons
                   (list "../Tools/skyline-tool-icon-128.png"
                         "../Tools/skyline-tool-icon-64.png")))))

(defvar *icon-hex-cache* nil
  "Cached hex-encoded RGB pixel data for the Skyline-Tool icon.")

(defun icon-hex-data ()
  "Return hex-encoded 48x48 RGB data for the Skyline-Tool icon.
   Caches the result; falls back to a simple grey block if ImageMagick is unavailable."
  (or *icon-hex-cache*
      (setf *icon-hex-cache*
            (let* ((svg-path (namestring
                              (asdf:system-relative-pathname
                               :skyline-tool "../Tools/Icons/Skyline-Tool-Folder.svg")))
                   (logo-path (namestring
                               (asdf:system-relative-pathname
                                :skyline-tool "../Tools/skyline-tool-icon-64.png"))))
              (or (ignore-errors
                    (let* ((pixels (uiop:run-program
                                    (list "magick" "convert"
                                          svg-path
                                          "-alpha" "deactivate"
                                          "-resize" "48x48"
                                          "-depth" "8"
                                          "rgb:-")
                                    :output :vector))
                           (hex (with-output-to-string (s)
                                  (dotimes (i (length pixels))
                                    (format s "~2,'0x" (aref pixels i))))))
                      hex))
                  ;; Fallback: try PNG
                  (ignore-errors
                    (let* ((pixels (uiop:run-program
                                    (list "magick" "convert"
                                          logo-path
                                          "-alpha" "deactivate"
                                          "-resize" "48x48"
                                          "-depth" "8"
                                          "rgb:-")
                                    :output :vector))
                           (hex (with-output-to-string (s)
                                  (dotimes (i (length pixels))
                                    (format s "~2,'0x" (aref pixels i))))))
                      hex))
                  ;; Last resort: 48x48 grey square
                  (make-string (* 48 48 3 2) :initial-element #\9))))))

(defun write-ps-header-icon (ps)
  "Write PostScript code to draw the Skyline-Tool icon as a 48×48 RGB bitmap."
  (let ((hex (icon-hex-data))
        (bpr (* 48 3)))  ; bytes per row (48 pixels × 3 components)
    (format ps "
gsave
  /DeviceRGB setcolorspace
  ~D ~D 8
  [~D 0 0 ~D 0 0]
  { currentfile ~D string readhexstring pop } image
"
           48 48 48 -48 bpr)
    ;; Write hex data, 72 chars per line
    (loop for i from 0 below (length hex) by 72
          do (format ps "~a~%" (subseq hex i (min (+ i 72) (length hex)))))
    (format ps "grestore~%")))

(defun write-ps-docinfo (ps title creator author)
  "Write PDF Document Info (DSC comments + pdfmark) for ps2pdf."
  (format ps "%%%%Title: ~a~%" (escape-ps-string title))
  (when creator
    (format ps "%%%%Creator: ~a~%" (escape-ps-string creator)))
  (when author
    (format ps "%%%%Author: ~a~%" (escape-ps-string author)))
  (format ps "[ /Title (~a) /Creator (~a) /Author (~a) /DOCINFO pdfmark~%"
          (escape-ps-string title) (escape-ps-string (or creator ""))
          (escape-ps-string (or author ""))))

(defun write-ps-header-bar (ps title-text date-str author game-title &optional page-num total-pages)
  "Write PDF header bar: icon at top-left, document title in navy blue.
   Header is positioned 3/4\" (54pt) from page top. No page number."
  (declare (ignore date-str author page-num total-pages game-title))
  (format ps "gsave
 56 738 translate
")
  (write-ps-header-icon ps)
  (format ps "
  /Times-Roman-ISOLatin1 findfont 12 scalefont setfont
  0.0 0.0 0.3 setrgbcolor
  0 36 moveto (~a) show
grestore
" (escape-ps-string title-text)))

(defun write-ps-footer (ps date-str author hostname game-title page-num total-pages)
  "Write PDF footer: icon at lower-left, 'Skyline-Tool for *game-title*',
   date---author (on host), page number right. All 75% black.
   Footer is positioned 3/4\" (54pt) from page bottom."
  (write-ps-header-icon ps)
  (let ((emdash (string (code-char #x2014))))
    (format ps "
gsave
 56 54 translate
 /Times-Roman-ISOLatin1 findfont 7 scalefont setfont
 0.25 0.25 0.25 setrgbcolor
 0 0 moveto (Skyline-Tool for ~a) show
 /Times-Roman-ISOLatin1 findfont 6 scalefont setfont
 0.25 0.25 0.25 setrgbcolor
 0 -10 moveto (~a ~a ~a (on ~a)) show
grestore
/Times-Roman-ISOLatin1 findfont 7 scalefont setfont
0.25 0.25 0.25 setrgbcolor
460 55 moveto (Page ~d of ~d) show
"
    (escape-ps-string game-title)
    (escape-ps-string date-str)
    emdash
    (escape-ps-string author)
    (escape-ps-string hostname)
    page-num total-pages)))

(defun write-ps-font-encodings (ps)
  "Write PostScript font re-encoding prologue for ISOLatin1 support (© ® etc)."
  (format ps "
%% Re-encode standard fonts for ISOLatin1 support
/ISOLatin1Encoding where { pop } {
  /ISOLatin1Encoding [/Gamma /Delta /Theta /Lambda /Xi /Pi /Sigma
    /Gamma /Delta /Theta /Lambda /Xi /Pi /Sigma /Phi /Psi /Omega
    /ff /fi /fl /ffi /ffl /dotlessi /dotlessj /grave /acute /caron
    /breve /macron /ring /cedilla /germandbls /ae /oe /oslash /AE
    /OE /Oslash /suppress /exclamdown /cent /sterling /currency
    /yen /brokenbar /section /dieresis /copyright /ordfeminine
    /guillemotleft /logicalnot /hyphen /registered /macron /degree
    /plusminus /twosuperior /threesuperior /acute /mu /paragraph
    /periodcentered /cedilla /onesuperior /ordmasculine /guillemotright
    /onequarter /onehalf /threequarters /questiondown /Agrave /Aacute
    /Acircumflex /Atilde /Adieresis /Aring /AE /Ccedilla /Egrave
    /Eacute /Ecircumflex /Edieresis /Igrave /Iacute /Icircumflex
    /Idieresis /Eth /Ntilde /Ograve /Oacute /Ocircumflex /Otilde
    /Odieresis /multiply /Oslash /Ugrave /Uacute /Ucircumflex
    /Udieresis /Yacute /Thorn /germandbls /agrave /aacute /acircumflex
    /atilde /adieresis /aring /ae /ccedilla /egrave /eacute /ecircumflex
    /edieresis /igrave /iacute /icircumflex /idieresis /eth /ntilde
    /ograve /oacute /ocircumflex /otilde /odieresis /divide /oslash
    /ugrave /uacute /ucircumflex /udieresis /yacute /thorn /ydieresis]
  def
} ifelse
/ReEncode { findfont dup length dict begin { 1 index /FID ne { def } { pop pop } ifelse } forall /Encoding ISOLatin1Encoding def currentdict end definefont pop } def
(/Helvetica) cvn /Helvetica-ISOLatin1 ReEncode
(/Courier) cvn /Courier-ISOLatin1 ReEncode
(/Helvetica-Bold) cvn /Helvetica-Bold-ISOLatin1 ReEncode
(/Times-Roman) cvn /Times-Roman-ISOLatin1 ReEncode
(/Times-Italic) cvn /Times-Italic-ISOLatin1 ReEncode
"))

(defun escape-ps-string (string)
  "Escape special PostScript characters in STRING for use in show operators.
   Latin-1 characters (© ® etc.) are output as octal escapes for ISOLatin1 font;
   Unicode outside Latin-1 (— '' \"\" … ™) are replaced with ASCII equivalents."
  (with-output-to-string (out)
    (loop for c across string
          for code = (char-code c)
          do (cond
               ((member c '(#\( #\) #\\) :test 'char=)
                (princ "\\" out) (princ c out))
               ((char= c (code-char #x2014)) (princ "—" out))  ; em dash — keep as Unicode char for Ghostscript
               ((char= c (code-char #x2013)) (princ "–" out))  ; en dash
               ((char= c (code-char #x2018)) (princ "'" out))   ; left single quote
               ((char= c (code-char #x2019)) (princ "'" out))   ; right single quote
               ((char= c (code-char #x201C)) (princ "\"" out))  ; left double quote
               ((char= c (code-char #x201D)) (princ "\"" out))  ; right double quote
               ((char= c (code-char #x2022)) (princ "*" out))   ; bullet
               ((char= c (code-char #x2026)) (princ "..." out)) ; ellipsis
               ((char= c (code-char #x203A)) (princ ">" out))   ; single right angle quote
               ((char= c (code-char #x2039)) (princ "<" out))   ; single left angle quote
               ((char= c (code-char #x00A0)) (princ " " out))   ; non-breaking space
               ;; ©, ® are in Latin-1 — let PS font handle via octal escape
               ;; ™ is outside Latin-1 — keep as "TM"
               ((< code 128)
                (princ c out))
               ((< code 256)
                ;; Latin-1 range: output as octal escape (needs ISOLatin1Encoding)
                (format out "\\~3,'0o" code))
               (t
                (princ "\\077" out))))))

(defun write-ps-page-footer (ps page-num total-pages title-text date-str author &optional hostname)
  "Write footer per branding spec:
   - Skyline-Tool icon at bottom left (~48pt)
   - Text indented ~1in (72pt) from left margin
   - 'Skyline-Tool' in Royal Blue, ' for ' in black, GAME in Italic Navy Blue
   - Second line: date — author (on hostname) in 75% dark gray
   - Far bottom right: 'Page N of M' in 75% dark gray
   All face: Times-Roman."
  (format ps "gsave
 0 12 translate
")
    (write-ps-header-icon ps)
    (format ps "
/Times-Roman-ISOLatin1 findfont 10 scalefont setfont
72 38 moveto
0.0 0.2 0.6 setrgbcolor
(Skyline-Tool) show
currentpoint pop 5 add 38 moveto
0.0 0.0 0.0 setrgbcolor
(for ) show
currentpoint pop 3 add 38 moveto
/Times-Italic-ISOLatin1 findfont 10 scalefont setfont
0.0 0.0 0.5 setrgbcolor
(~a) show
/Times-Roman-ISOLatin1 findfont 7 scalefont setfont
0.25 0.25 0.25 setrgbcolor
72 22 moveto
(~a -- ~a~@[ (on ~a)~]) show
522 12 moveto
(Page ~d of ~d) show
grestore
" (escape-ps-string title-text)
      (escape-ps-string date-str) (escape-ps-string author)
      (and hostname (escape-ps-string hostname))
      page-num total-pages))

(defun render-maria-to-rgb (dump mode address width colors)
  "Render Maria tile pixels to a flat RGB byte vector using COLORS (vector of Atari register values).
   Returns PIXELS-WIDE HEIGHT RGB-ARRAY."
  (let* ((ppb (ecase mode (:160a 4) (:160b 2)))
         (tw (* width ppb))
         (th 16)
         (system-palette (ecase *region*
                           (:ntsc +prosystem-ntsc-palette+)
                           (:pal +prosystem-pal-palette+)))
         (fp (extract-maria-pixels dump mode address width))
         (rgb (make-array (* tw th 3 2) :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (y th)
      (dotimes (x tw)
        (let* ((pen (aref fp x y))
               (reg (elt colors pen))
               (col (if (and (integerp reg) (<= 0 reg 255) (nth reg system-palette))
                        (nth reg system-palette)
                        '(0 0 0)))
               (base (* y tw 2)))
          (setf (aref rgb (* (+ base (* 2 x)) 3)) (first col)
                (aref rgb (+ (* (+ base (* 2 x)) 3) 1)) (second col)
                (aref rgb (+ (* (+ base (* 2 x)) 3) 2)) (third col)
                (aref rgb (* (+ base (1+ (* 2 x))) 3)) (first col)
                (aref rgb (+ (* (+ base (1+ (* 2 x))) 3) 1)) (second col)
                (aref rgb (+ (* (+ base (1+ (* 2 x))) 3) 2)) (third col)))))
    (values (* tw 2) th rgb)))

(defun write-ps-image (ps rgb-array img-width img-height max-width max-height)
  "Write PostScript code to display an RGB image, scaled to fit within MAX-WIDTH x MAX-HEIGHT points."
  (let* ((scale (min (/ max-width (max 1 img-width))
                     (/ max-height (max 1 img-height))))
         (bpr (* img-width 3))
         (hex (with-output-to-string (s)
                (dotimes (i (length rgb-array))
                  (format s "~2,'0x" (aref rgb-array i))
                  (when (and (plusp i) (zerop (mod i 72))) (terpri s))))))
    (format ps "/DeviceRGB setcolorspace~%")
    (format ps "gsave~%")
    (format ps "~f ~f scale~%" (* img-width scale) (* img-height scale))
    (format ps "~d ~d 8~%" img-width img-height)
    (format ps "[~d 0 0 ~d 0 0]~%" img-width (- img-height))
    (format ps "{ currentfile ~d string readhexstring pop } image~%" bpr)
    (format ps "~a~%" hex)
    (format ps "grestore~%")))

(defvar *last-save-directory* nil
  "Last directory used by prompt-save-pathname for Save As dialogs.")

(defun find-save-directory ()
  "Return the best default directory for Save As dialogs.
   Checks *last-save-directory*, then saved preferences,
   then ~/work/, ~/Work/, ~/Documents/."
  (or *last-save-directory*
      (let ((saved (get-pref :last-save-directory)))
        (and saved (probe-file (pathname saved)) (pathname saved)))
      (let ((home (user-homedir-pathname)))
        (or (some (lambda (d) (let ((p (merge-pathnames d home)))
                               (when (probe-file p) p)))
                  '("work/" "Work/" "Documents/"))
            (merge-pathnames "Work/" home)))))

(defun prompt-save-pathname (default-name &optional (type "txt") &key prefs-key)
  "Prompt the user for a save pathname, trying zenity first then CLIM dialog.
   DEFAULT-NAME is the suggested filename (e.g. \"Sequence-5.json\").
   PREFS-KEY is a keyword used to persist the chosen directory in preferences.
   Returns the chosen pathname, or NIL if cancelled."
  (let* ((dir (find-save-directory))
         (default (merge-pathnames default-name dir)))
    ;; Try zenity for native Gnome dialog
    (or (ignore-errors
          (let* ((out (string-trim '(#\Newline #\Space)
                        (uiop:run-program
                         (list "zenity" "--file-selection" "--save"
                               (format nil "--filename=~a" (namestring default))
                               "--title=Save As...")
                         :output :string :ignore-error-status t)))
                 (path (when (and out (> (length out) 0)) (pathname out))))
            (when path
              (let ((dir (make-pathname :name nil :type nil :defaults path)))
                (setf *last-save-directory* dir)
                (when prefs-key
                  (set-pref prefs-key (namestring dir))
                  (set-pref :last-save-directory (namestring dir)))
                path))))
        ;; Fallback to CLIM pathname prompter
        (let ((path (clim:accept 'pathname :prompt "Save As" :default default)))
          (when path
            (let ((dir (make-pathname :name nil :type nil :defaults path)))
              (setf *last-save-directory* dir)
              (when prefs-key
                (set-pref prefs-key (namestring dir))
                (set-pref :last-save-directory (namestring dir)))
              path))))))

(defvar *prefs-cache* nil
  "Cached preference plist loaded from the prefs file, or NIL if not yet loaded.")

(defun prefs-pathname ()
  "Return the pathname for the preferences file.
   Constructs ~/.config/Skyline-Tool/<GAME-TITLE>/<PORT>.prefs.json
   using *game-title* (capitalized) and machine-directory-name."
  (let ((game (string-capitalize (if (boundp '*game-title*) *game-title* "Game")))
        (port (ignore-errors (machine-directory-name))))
    (merge-pathnames
     (make-pathname :directory (list :relative ".config" "Skyline-Tool" game)
                    :name port
                    :type "prefs.json")
     (user-homedir-pathname))))

(defun load-prefs ()
  "Read the preferences JSON file and return a plist.
   Returns NIL if the file does not exist."
  (let ((path (prefs-pathname)))
    (when (probe-file path)
      (let ((alist (cl-json:decode-json-from-string (uiop:read-file-string path))))
        (loop for (key . value) in alist
              append (list (intern (string-upcase key) :keyword) value))))))

(defun save-prefs (plist)
  "Write PLIST as JSON to the preferences file.
   Creates the directory if it does not exist."
  (let ((path (prefs-pathname)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (princ (cl-json:encode-json-to-string
              (loop for (key value) on plist by #'cddr
                    collect (cons (string-downcase (symbol-name key)) value)))
             s))))

(defun get-pref (key &optional default)
  "Read a preference value from the cached prefs.
   Returns DEFAULT (default NIL) if KEY is not found."
  (unless *prefs-cache*
    (setf *prefs-cache* (load-prefs)))
  (if *prefs-cache*
      (getf *prefs-cache* key default)
      default))

(defun set-pref (key value)
  "Set a preference value, update the cache, and save the file."
  (unless *prefs-cache*
    (setf *prefs-cache* (load-prefs)))
  (setf (getf *prefs-cache* key) value)
  (save-prefs *prefs-cache*)
  value)

(defvar *printer-cache* nil
  "Cached list of (queue-name . display-name) printer pairs, or NIL if no cache.")
(defvar *printer-cache-time* 0
  "Universal time when *printer-cache* was last refreshed.")

(defun discover-printers (&optional force)
  "Return a list of CUPS printer queue names (strings).
   Calls lpstat -e to enumerate available printers.
   Results are cached for 30 seconds unless FORCE is true."
  (let ((now (get-universal-time)))
    (unless (and *printer-cache* (> (- now *printer-cache-time*) 30))
      (setf *printer-cache* nil
            *printer-cache-time* 0)))
  (when (or force (not *printer-cache*))
    (setf *printer-cache*
          (sort (delete "" (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s))
                                   (ignore-errors
                                     (uiop:run-program '("lpstat" "-e") :output :lines)))
                         :test #'string=)
                #'string-lessp)
          *printer-cache-time* (get-universal-time)))
  *printer-cache*)

(defun discover-printers-with-names ()
  "Return a list of (queue-name . display-name) for CUPS printers.
   Display names come from CUPS HTML or lpstat descriptions."
  (flet ((trim (s) (string-trim '(#\Space #\Tab #\Newline) s)))
    (let ((queues (discover-printers))
          (result nil))
      (dolist (q queues)
        (let ((display q))
          ;; Try CUPS HTTP API for display name
          (ignore-errors
            (let* ((url (format nil "http://localhost:631/printers/~a" q))
                   (html (uiop:run-program
                          (list "curl" "-s" "--connect-timeout" "2" url)
                          :output :string :ignore-error-status t)))
              (when html
                ;; Look for "printer-info" or "printer-make-and-model" in IPP attrs
                (let ((m (ppcre:scan-to-strings
                          "printer-make-and-model[^>]*>([^<]+)"
                          html)))
                  (when (and m (aref m 0) (> (length (aref m 0)) 0))
                    (setf display (trim (aref m 0))))))))
          ;; Fallback: lpstat description
          (when (string= display q)
            (ignore-errors
              (let ((detail (uiop:run-program (list "lpstat" "-l" "-p" q)
                                               :output :string :ignore-error-status t)))
                (when (and detail (search "Description:" detail))
                  (let ((start (+ 12 (search "Description:" detail)))
                        (end (position #\Newline detail :start (search "Description:" detail))))
                    (let ((desc (trim (subseq detail start end))))
                      (when (> (length desc) 0) (setf display desc))))))))
          (push (cons q display) result)))
      (sort result #'string-lessp :key #'cdr))))



(defun window-title (&optional suffix)
  "Return a window title like \"Skyline-Tool for Phantasia 7800\" or with a SUFFIX.
   SUFFIX is appended after a colon and space, e.g. \"Skyline-Tool for Phantasia 7800: Launcher\"."
  (let ((game (ignore-errors (string-capitalize (or (and (boundp '*game-title*) *game-title*) "Game"))))
        (machine (ignore-errors (machine-directory-name))))
    (format nil "Skyline-Tool for ~a~@[ ~a~]~@[: ~a~]" game machine suffix)))

(defun generated-file-path (filename)
  "Return the platform-specific path for a generated file."
  (let ((platform-dir (format nil "~d" *machine*)))
    (merge-pathnames (make-pathname :directory (list :relative "Source" "Generated" platform-dir)
                                    :name (pathname-name filename)
                                    :type (pathname-type filename))
                     (uiop:getcwd))))

