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
                      (list #p"../Tools/skyline-tool-icon-256.png"
                            #p"../Tools/skyline-tool-icon-128.png"
                            #p"../Tools/skyline-tool-icon-64.png")))
         (existing (find-if (lambda (n) (probe-file (asdf:system-relative-pathname :skyline-tool n)))
                            candidates)))
    (when existing
      (clim:make-pattern-from-bitmap-file
       (asdf:system-relative-pathname :skyline-tool existing)))))

(defvar *icon-hex-cache* nil
  "Cached hex-encoded RGB pixel data for the Skyline-Tool icon.")

(defun icon-hex-data ()
  "Return hex-encoded 48x48 RGB data for the Skyline-Tool icon."
  (or *icon-hex-cache*
      (setf *icon-hex-cache*
            (let* ((svg-path (namestring (asdf:system-relative-pathname
                                          :skyline-tool #p"../Tools/Icons/Skyline-Tool-Folder.svg")))
                   (logo-path (namestring (asdf:system-relative-pathname
                                           :skyline-tool #p"../Tools/skyline-tool-icon-64.png")))
                   (try-load (lambda (path)
                               (ignore-errors
                                ;; FIXME: move this internally ... don't call out to `magick`
                                (let* ((pixels (uiop:run-program (list "magick" "convert" path
                                                                       "-alpha" "deactivate" "-resize"
                                                                       "48x48" "-depth" "8" "rgb:-")
                                                                 :output :vector))
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
   Falls back to a dark gray rectangle if icon data is empty or invalid."
  (let ((icon-path (asdf:system-relative-pathname :skyline-tool "../Tools/skyline-tool-icon-64.png")))
    (if (probe-file icon-path)
        (ignore-errors
          (let* ((png (png-read:read-png-file (namestring icon-path)))
                 (w (png-read:width png)) (h (png-read:height png))
                 (data (png-read:image-data png))
                 (dims (array-dimensions data))
                 (rgb (make-array (list w h 3) :element-type '(unsigned-byte 8))))
            (dotimes (y h)
              (dotimes (x w)
                (if (= (length dims) 3)
                    (setf (aref rgb x y 0) (aref data x y 0)
                          (aref rgb x y 1) (aref data x y 1)
                          (aref rgb x y 2) (aref data x y 2))
                    (let ((idx (aref data x y)))
                      (when idx
                        (setf (aref rgb x y 0) idx
                              (aref rgb x y 1) idx
                              (aref rgb x y 2) idx))))))
            (write-ps-image ps rgb w h 48 48)
            t))
        (format ps "gsave
   0.3 0.3 0.3 setrgbcolor
   0 0 48 48 rectfill
   grestore~%"))))

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
  "Write PDF footer with two-row tabular layout:
   Row 1 (top): Icon (spans both rows) | 'Skyline-Tool' (Royal Blue) 'for' (black) 'GameTitle' (Navy Blue, Italic)
   Row 2 (bottom, 75% row1 height): Icon continued | date --- author on host [at site] (75% gray) | Page N of M (75% gray, right-aligned)
   All positioned 3/4\" (54pt) from page bottom. Icon height = both rows combined (48pt)."
  (let ((site (ignore-errors (short-site-name))))
    ;; Draw icon spanning both rows (48pt tall at y=54..102)
    (format ps "gsave 56 54 translate~%")
    (write-ps-header-icon ps)
    (format ps "grestore~%")
    ;; Row 1: Branding text at y=94 baseline (10pt font, row top at y=102)
    (format ps "gsave~%")
    (format ps "/Times-Roman-ISOLatin1 findfont 10 scalefont setfont~%")
    (format ps "0.0 0.2 0.6 setrgbcolor~%")           ; Royal Blue
    (format ps "112 94 moveto~%")
    (format ps "(Skyline-Tool) show~%")
    (format ps "currentpoint pop 2 add 0 moveto~%")
    (format ps "0.0 0.0 0.0 setrgbcolor~%")           ; Black
    (format ps "(for ) show~%")
    (format ps "currentpoint pop 2 add 0 moveto~%")
    (format ps "0.0 0.0 0.5 setrgbcolor~%")           ; Navy Blue
    (if (string= game-title "Phantasia")
        (format ps "/Dublin-ISOLatin1 findfont 10 scalefont setfont~%")
        (format ps "/Times-Italic-ISOLatin1 findfont 10 scalefont setfont~%"))
    (format ps "(~a) show~%" (escape-ps-string game-title))
    (format ps "grestore~%")
    ;; Row 2: Date/author/host/site at y=67.57 baseline (7.5pt font, row top at y=74.57)
    (format ps "gsave~%")
    (format ps "/Times-Roman-ISOLatin1 findfont 7.5 scalefont setfont~%")
    (format ps "0.25 0.25 0.25 setrgbcolor~%")        ; 75% gray
    (format ps "112 67.57 moveto~%")
    (let ((site-str (if site (format nil " at ~a" (escape-ps-string site)) "")))
      (format ps "(~a --- ~a on ~a~a) show~%"
              (escape-ps-string date-str)
              (escape-ps-string author)
              (escape-ps-string hostname)
              site-str))
    ;; Page number right-aligned at x=556 (56pt from right edge of 612pt page)
    (format ps "556 67.57 moveto~%")
    (format ps "(Page ~d of ~d) dup stringwidth pop neg 0 rmoveto show~%"
            page-num total-pages)
    (format ps "grestore~%")))

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
  "Escape PostScript string with proper Unicode to octal conversion.
   Latin-1 chars use \\ooo octal escapes, Unicode chars beyond Latin-1 are replaced."
  (when string
    (with-output-to-string (out)
      (loop for c across string
            for code = (char-code c)
            do (cond
                 ((member c '(#\( #\) #\\) :test 'char=)
                  (princ "\\" out) (princ c out))
                 ((char= c #\—) (format out "\\~8,3,'0r" #x2014)) ;; em dash → ---
                 ((char= c #\–) (format out "\\~8,3,'0r" #x2013)) ;; en dash → --
                 ((char= c #\apostrophe) (format out "\\~8,3,'0r" #x2019)) ;; right quote
                 ((char= c #\left_double_quotation_mark) (format out "\\~8,3,'0r" #x201C)) ;; left double
                 ((char= c #\right_double_quotation_mark) (format out "\\~8,3,'0r" #x201D)) ;; right double
                 ((char= c #\•) (format out "\\042")) ;; bullet
                 ((char= c #\…) (format out "\\263")) ;; ellipsis
                 ((char= c #\>) (format out "\\047")) ;; right angle
                 ((char= c #\<) (format out "\\046")) ;; left angle
                 ((char= c #\non-breaking_space) (princ " " out)) ;; NBSP → space ERROR FIXME
                 ((< code 128) (princ c out))
                 ((< code 256) (format out "\\~8,3,'0r" code)) ;; Latin-1 octal
                 (t (princ "\\?" out))))))) ;; Unicode fallback FIXME this is not acceptable

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
  "Write PostScript code to display an RGB image, scaled to fit within MAX-WIDTH x MAX-HEIGHT points.
   Does NOT emit gsave/grestore; caller must manage graphics state."
  (let* ((scale (min (/ max-width (max 1 img-width))
                      (/ max-height (max 1 img-height))))
         (bpr (* img-width 3))
         (hex (with-output-to-string (s)
                (dotimes (i (length rgb-array))
                  (format s "~2,'0x" (aref rgb-array i))
                  (when (and (plusp i) (zerop (mod i 72))) (terpri s))))))
    (format ps "/DeviceRGB setcolorspace~%")
    (format ps "~f ~f scale~%" (* img-width scale) (* img-height scale))
    (format ps "~d ~d 8~%" img-width img-height)
    (format ps "[~d 0 0 ~d 0 0]~%" img-width (- img-height))
    (format ps "{ currentfile ~d string readhexstring pop } image~%" bpr)
    (format ps "~a~%" hex)))

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
            *printer-cache-time* 0))
    (when (or force (not *printer-cache*))
      (setf *printer-cache*
            (sort (delete "" (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s))
                                     (ignore-errors
                                       (uiop:run-program '("lpstat" "-e") :output :lines))))
                  #'string-lessp)
            *printer-cache-time* (get-universal-time)))
    *printer-cache*))

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

(defun write-ps-page-footer (ps page-num total-pages title-text date-str author &optional hostname)
  "Write PDF page footer with proper formatting.
   Layout:
     | < Icon >  | Skyline-Tool for _Phantasia_ 7800                                                              |              |
     | < ^^^^ >  | 2026-06-30 13:41 --- Bruce-Robert Pocock on Hermes at Star-Hope                                | Page 1 of 19 |
   Icon = two lines tall graphics, _Phantasia_ = italics navy blue, 7800 = machine-directory-name
   Skyline-Tool in Royal Blue, 'for' '7800' in black
   Date - time - user on host at short-site name and page n of m --- all in 75% black (dark gray) and 8pt"
(let* ((site (ignore-errors (short-site-name)))
          (machine (or (ignore-errors (machine-directory-name)) ""))
          (date-part (escape-ps-string date-str))
          (author-part (escape-ps-string author))
          (host-part (when hostname (escape-ps-string hostname)))
          (site-part (escape-ps-string (or site "")))
          (game-title (escape-ps-string title-text))
          (machine-dir (escape-ps-string machine))
          (icon-path (asdf:system-relative-pathname :skyline-tool "../Tools/skyline-tool-icon-64.png"))
          (icon-pattern (and (probe-file icon-path)
                             (ignore-errors (clim:make-pattern-from-bitmap-file icon-path)))))
     ;; Draw icon spanning both rows (icon is 64x64, drawn at 64x64 points)
     (when icon-pattern
       (format ps "gsave~%")
       (format ps "/PatternType 1 /PaintType 2 /TilingType 1~%")
       (format ps "[64 0 0 64 0 0] /PatternType 1 /PaintType 2 /TilingType 1~%")
       (format ps "~a setpattern~%" icon-pattern)
       (format ps "0 42 64 64 rectfill~%")
       (format ps "grestore~%"))
     ;; Row 1: Icon + Branding (at y=106 baseline)
     (format ps "gsave~%")
    (format ps "/Times-Roman-ISOLatin1 findfont 8 scalefont setfont~%")
    (format ps "0.0 0.2 0.6 setrgbcolor~%")
    (format ps "112 106 moveto~%")
    (format ps "(Skyline-Tool) show~%")
    (format ps "currentpoint pop 2 add 0 moveto~%")
    (format ps "0.0 0.0 0.0 setrgbcolor~%")
    (format ps "(for ) show~%")
    (format ps "currentpoint pop 2 add 0 moveto~%")
    (format ps "0.0 0.0 0.5 setrgbcolor~%")
    (format ps "/Times-Italic-ISOLatin1 findfont 8 scalefont setfont~%")
    (format ps "(~a) show~%" game-title)
    (format ps "currentpoint pop 2 add 0 moveto~%")
    (format ps "/Times-Roman-ISOLatin1 findfont 8 scalefont setfont~%")
    (format ps "0.25 0.25 0.25 setrgbcolor~%")
    (format ps "(~a) show~%" machine-dir)
    (format ps "grestore~%")
    ;; Row 2: Date/author on host at site + page number (dark gray, 8pt)
    (format ps "gsave~%")
    (format ps "/Times-Roman-ISOLatin1 findfont 8 scalefont setfont~%")
    (format ps "0.25 0.25 0.25 setrgbcolor~%")
    (format ps "112 79.57 moveto~%")
    ;; Format: date --- author [on host] [at site]
    (format ps "(~a~@[ --- ~a~]~@[ on ~a~]~@[ at ~a~]) show~%"
            date-part
            author-part
            host-part
            site-part)
    ;; Page number right-aligned
    (format ps "556 79.57 moveto~%")
    (format ps "(Page ~d of ~d) dup stringwidth pop neg 0 rmoveto show~%"
            page-num total-pages)
    (format ps "grestore~%")))
