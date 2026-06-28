;;; PostScript and PDF utility functions for Skyline-Tool

(in-package :skyline-tool)

(defun skyline-tool-icon (&key (resource nil))
  "Return an icon pattern for CLIM frames.
   If RESOURCE is provided (a keyword), tries to load a resource-specific
   icon (e.g. Tools/skyline-tool-icon-ANIMATION-SEQUENCE-EDITOR-128.png),
   falling back to the generic skyline-tool icon."
  (let* ((candidates (append
                      (when resource
                        (list (format nil "../Tools/skyline-tool-icon-~(~a~)-128.png" resource)
                              (format nil "../Tools/skyline-tool-icon-~(~a~)-64.png" resource)))
                      (list "../Tools/skyline-tool-icon-128.png"
                            "../Tools/skyline-tool-icon-64.png")))
         (existing (find-if (lambda (n) (probe-file (asdf:system-relative-pathname :skyline-tool n))) candidates)))
    (when existing
      (clim:make-pattern-from-bitmap-file
       (asdf:system-relative-pathname :skyline-tool existing)))))

(defvar *icon-hex-cache* nil
  "Cached hex-encoded RGB pixel data for the Skyline-Tool icon.")

(defun icon-hex-data ()
  "Return hex-encoded 48x48 RGB data for the Skyline-Tool icon.
   Caches the result; falls back to a simple grey block if ImageMagick is unavailable."
  (or *icon-hex-cache*
      (setf *icon-hex-cache*
            (let* ((svg-path (namestring (asdf:system-relative-pathname :skyline-tool "../Tools/Icons/Skyline-Tool-Folder.svg")))
                   (logo-path (namestring (asdf:system-relative-pathname :skyline-tool "../Tools/skyline-tool-icon-64.png")))
                   (try-load (lambda (path)
                               (ignore-errors
                                 (let* ((pixels (uiop:run-program (list "magick" "convert" path "-alpha" "deactivate" "-resize" "48x48" "-depth" "8" "rgb:-") :output :vector))
                                        (hex (with-output-to-string (s)
                                               (dotimes (i (length pixels))
                                                 (format s "~2,'0x" (aref pixels i))))))
                                   hex)))))
              (or (funcall try-load svg-path)
                  (funcall try-load logo-path)
                  (make-string (* 48 48 3 2) :initial-element #\9))))))


;; PostScript/PDF generation functions
(defun write-ps-header-icon (ps)
  "Write PostScript code to draw the Skyline-Tool icon as a 48×48 RGB bitmap.
   Falls back to a dark gray rectangle if icon hex data is empty or invalid."
  (let* ((hex (icon-hex-data))
         (bpr (* 48 3))
         (expected (* 48 48 3 2)))
    (if (and hex (>= (length hex) expected))
        (progn
          (format ps "
gsave
  /DeviceRGB setcolorspace
  ~D ~D 8
  [~D 0 0 ~D 0 0]
  { currentfile ~D string readhexstring pop } image
"
                  48 48 48 -48 bpr)
          (loop for i from 0 below (length hex) by 72
                do (format ps "~a~%" (subseq hex i (min (+ i 72) (length hex)))))
          (format ps "grestore~%"))
        (progn
          (format ps "gsave
  0.3 0.3 0.3 setrgbcolor
  0 0 48 48 rectfill
  grestore~%")))))

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
  "Write PDF header bar: icon at top-left, document title in navy blue centered on page.
   Header is positioned 3/4\" (54pt) from page top. No page number."
  (declare (ignore date-str author page-num total-pages game-title))
  (format ps "gsave
  56 738 translate
")
  (write-ps-header-icon ps)
  (format ps "
  /Times-Roman-ISOLatin1 findfont 18 scalefont setfont
  0.0 0.0 0.3 setrgbcolor
  (~a) dup stringwidth pop 250 exch sub 0 moveto show
  grestore
" (escape-ps-string title-text)))

(defun write-ps-footer (ps date-str author hostname game-title page-num total-pages)
  "Write PDF footer: icon at lower-left, 'Skyline-Tool for *game-title*',
   date — author (on hostname), page number right. All 75% black.
   Footer is positioned 3/4\" (54pt) from page bottom."
(let ((site (ignore-errors (short-site-name))))
      (format ps "gsave 56 54 translate~%")
      (write-ps-header-icon ps)
      (format ps "grestore~%")
      ;; Branding line + date/author on one block
      (format ps "gsave
  56 58 translate
  /Times-Roman-ISOLatin1 findfont 8 scalefont setfont
  0.25 0.25 0.25 setrgbcolor
  0 0 moveto (Skyline-Tool for ~a) show
  0 -12 moveto
  (~a) show
  currentpoint pop 4 add 0 moveto
  gsave currentpoint 2 add moveto 12 0 rlineto stroke grestore
  currentpoint pop 4 add 0 moveto
  (~a) show
  currentpoint pop 3 add 0 moveto
  (\\(on ~a~[ at ~a~]\\)) show
  grestore
/Times-Roman-ISOLatin1 findfont 8 scalefont setfont
  0.25 0.25 0.25 setrgbcolor
  460 55 moveto (Page ~d of ~d) show
"
            (escape-ps-string game-title)
            (escape-ps-string date-str)
            (escape-ps-string author)
            (escape-ps-string hostname)
            (if site (escape-ps-string site) "")
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
    /Acircumflex /Atile /Adieresis /Aring /AE /Ccedilla /Egrave
    /Eacute /Ecircumflex /Edieresis /Igrave /Iacute /Icircumflex
    /Idieresis /Eth /Ntilde /Ograve /Oacute /Ocircumference /Otilde
    /Odieresis /multiply /Oslash /Ugrave /Uacute /Ucircumference /Udieresis
    /Yacute /Thorn /germandbls /agrave /aacute /acircumflex
    /atilde /adieresis /aring /ae /ccedilla /egrave /eacute /ecircumflex
    /edieresis /igrave /iace /icircumflex /idieresis /eth /ntilde
    /ograve /oacute /ocircumference /otilde /odieresis /divide /oslash
    /ugrave /uacute /ucircumference /udieresis /yacute /thorn /ydieresis]
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
               ((char= c (code-char #x2014)) (princ "---" out))  ; em dash → --- (not in Latin-1)
               ((char= c (code-char #x2013)) (princ "–" out))  ; en dash
               ((char= c (code-char #x2018)) (princ "'" out))   ; left single quote
               ((char= c (code-char #x2019)) (princ "'" out))   ; right single quote
               ((char= c (code-char #x201C)) (princ '"'" out))  ; left double quote
               ((char= c (code-char #x201D)) (princ '"'" out))  ; right double quote
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

;; Printer discovery functions
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
                                     (uiop:run-program '("lpstat" "-e") :output :lines))))
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
      (sort result #'string-lessp :key #'cdr)))

(defun write-ps-page-footer (ps page-num total-pages title-text date-str author &optional hostname)
  "Write PDF footer: icon at lower-left, 'Skyline-Tool for GAME',
   date—author (on host), page number right."
  (format ps "gsave 56 54 translate~%")
  (write-ps-header-icon ps)
  (format ps "grestore~%")
  (format ps "gsave
 56 62 translate
 /Times-Roman-ISOLatin1 findfont 8 scalefont setfont
 0 0 moveto
 0.0 0.2 0.6 setrgbcolor
 (Skyline-Tool) show
 currentpoint pop 3 add 0 moveto
 0.0 0.0 0.0 setrgbcolor
 (for ) show
 currentpoint pop 2 add 0 moveto
 /Times-Italic-ISOLatin1 findfont 8 scalefont setfont
 0.0 0.0 0.5 setrgbcolor
 (~a) show
 grestore
" (escape-ps-string title-text))
  (format ps "gsave
 56 44 translate
 /Times-Roman-ISOLatin1 findfont 7 scalefont setfont
 0.25 0.25 0.25 setrgbcolor
 0 0 moveto
 (~a) show
 currentpoint pop 3 add 0 moveto
 gsave currentpoint 2 add moveto 0 2 rlineto stroke grestore
 currentpoint pop 3 add 0 moveto
 (~a~@[ on ~a~]) show
 466 0 moveto
 (Page ~d of ~d) show
 grestore
"
    (escape-ps-string date-str)
    (escape-ps-string author)
    (and hostname (escape-ps-string hostname))
    page-num total-pages)))
