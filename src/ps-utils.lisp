;;; PostScript and PDF utility functions for Skyline-Tool

(in-package :skyline-tool)

(defun skyline-tool-icon (&key (resource nil))
  "Return an icon pattern for CLIM frames.
   If RESOURCE is provided (a keyword), tries to load a resource-specific
   icon, falling back to the generic skyline-tool icon."
  (labels ((try (prefix)
             (let ((path (asdf:system-relative-pathname
                          :skyline-tool
                          (make-pathname
                           :name (format nil "~a-icon"
                                         (string-downcase (string prefix)))
                           :type "png"
                           :directory (list :relative "src" "icons")))))
               (when (probe-file path)
                 (return-from skyline-tool-icon
                   (clim:make-pattern-from-bitmap-file path))))))
    (when resource (try resource))
    (try "skyline-tool")))

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
(defun write-ps-header-icon (ps icon)
  "Write PostScript code to draw the Skyline-Tool icon as a 48×48 RGB bitmap.
   Falls back to a dark gray rectangle if icon data is empty or invalid."
  (let* ((png (png-read:read-png-file icon))
         (width (png-read:width png)) (height (png-read:height png))
         (data (png-read:image-data png)))
    (write-ps-image ps data width height 77 48)
    t))

(defun write-ps-docinfo (ps resource)
  "Write PDF Document Info (DSC comments + pdfmark) for ps2pdf."
  (format ps "%%%%Title: ~a~%" (escape-ps-string (game-resource-title resource)))
  (format ps "%%%%Author: ~a~%" (escape-ps-string (user-real-name)))
  (format ps "[ /Title (~a) /Creator (Skyline-Tool v~a) /Author (~a) /DOCINFO pdfmark~%"
          (escape-ps-string (game-resource-title resource))
          (escape-ps-string (asdf:component-version (asdf:find-system :skyline-tool)))
          (escape-ps-string (user-real-name))))

(defun write-ps-header-bar (ps title &key icon)
  "Write PDF header bar: icon at top-left, document title in navy blue centered on page.
   
Header is positioned 3/4\" (54pt) from page top. (Page numbers in footer)"
  (format ps "gsave
   56 738 translate
 ")
  (when icon (write-ps-header-icon ps icon))
  (format ps "
   /Times-Roman-ISOLatin1 findfont 18 scalefont setfont
   0.0 0.0 0.3 setrgbcolor
   (~a) dup stringwidth pop 250 exch sub 0 moveto show
   grestore
 " (escape-ps-string title)))

(defun write-ps-footer (ps date-str author hostname game-title page-num total-pages)
  "Write PDF footer with two-row tabular layout:
   Row 1 (top): Icon (spans both rows) | 'Skyline-Tool' (12pt Bold Times-Roman, Royal Blue)
   'for' (10.5pt Times-Roman, black) 'GameTitle' (12pt Italic, Navy Blue) machine-dir (10.5pt, Navy Blue)
   Row 2 (bottom): Icon continued | date --- author on host [at site] (75% gray, 8pt) | Page N of M (75% gray, right-aligned)
   All positioned 3/4\" (54pt) from page bottom. Icon height = both rows combined (48pt)."
  (let* ((site (ignore-errors (short-site-name)))
         (machine (or (ignore-errors (machine-directory-name)) ""))
         (date-part (escape-ps-string date-str))
         (author-part (escape-ps-string author))
         (host-part (escape-ps-string hostname))
         (site-part (escape-ps-string (or site "")))
         (gt (escape-ps-string game-title))
         (machine-dir (escape-ps-string machine)))
    ;; Draw icon spanning both rows (48pt tall at y=54..102)
    (format ps "gsave 56 54 translate~%")
    (write-ps-header-icon ps (skyline-tool-icon))
    (format ps "grestore~%")
    ;; Row 1: Branding at y=96 baseline
    (format ps "gsave~%")
    ;; "Skyline-Tool" in 12pt Bold Times-Roman, Royal Blue
    (format ps "/Times-Bold-ISOLatin1 findfont 12 scalefont setfont~%")
    (format ps "0.0 0.2 0.6 setrgbcolor~%")
    (format ps "112 96 moveto (Skyline-Tool) show~%")
    ;; " for " in 10.5pt Times-Roman, Black
    (format ps "currentpoint pop 2 add 0 moveto~%")
    (format ps "/Times-Roman-ISOLatin1 findfont 10.5 scalefont setfont~%")
    (format ps "0.0 0.0 0.0 setrgbcolor~%")
    (format ps "(for ) show~%")
    ;; Game-Title in 12pt Italic Times-Roman, Navy Blue
    (format ps "currentpoint pop 2 add 0 moveto~%")
    (format ps "/Times-Italic-ISOLatin1 findfont 12 scalefont setfont~%")
    (format ps "0.0 0.0 0.5 setrgbcolor~%")
    (format ps "(~a) show~%" gt)
    ;; machine-dir in 10.5pt Times-Roman, Navy Blue
    (format ps "currentpoint pop 2 add 0 moveto~%")
    (format ps "/Times-Roman-ISOLatin1 findfont 10.5 scalefont setfont~%")
    (format ps "0.0 0.0 0.5 setrgbcolor~%")
    (format ps "(~a) show~%" machine-dir)
    (format ps "grestore~%")
    ;; Row 2: Date/author/host/site at y=67.57 baseline (8pt)
    (format ps "gsave~%")
    (format ps "/Times-Roman-ISOLatin1 findfont 8 scalefont setfont~%")
    (format ps "0.25 0.25 0.25 setrgbcolor~%")
    (format ps "112 67.57 moveto~%")
    (format ps "(~a --- ~a on ~a~@[ at ~a~]) show~%"
            date-part author-part host-part site-part)
    ;; Page number right-aligned at x=556
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
(/Times-Bold) cvn /Times-Bold-ISOLatin1 ReEncode
  "))

(defun escape-ps-string (string)
  "Escape PostScript string with proper Unicode to octal conversion.
   Latin-1 chars use \\ooo octal escapes, Unicode chars beyond Latin-1 are replaced."
  (when string
    (with-output-to-string (out)
      (loop for c across string
            for code = (char-code c)
            do (cond
                 ((find c "()\\" :test 'char=)
                  (princ "\\" out) (princ c out))
                 ((char= c #\—) (format out "\\~8,3,'0r" #x2014)) ;; em dash → ---
                 ((char= c #\–) (format out "\\~8,3,'0r" #x2013)) ;; en dash → --
                 ((char= c #\apostrophe) (format out "\\~8,3,'0r" #x2019)) ;; right quote
                 ((char= c #\left_double_quotation_mark)
                  (format out "\\~8,3,'0r" #x201C)) ;; left double
                 ((char= c #\right_double_quotation_mark)
                  (format out "\\~8,3,'0r" #x201D)) ;; right double
                 ((char= c #\•) (format out "\\042")) ;; bullet
                 ((char= c #\…) (format out "\\263")) ;; ellipsis
                 ((char= c #\>) (format out "\\047")) ;; right angle
                 ((char= c #\<) (format out "\\046")) ;; left angle
                 ((char= c #\non-breaking_space)
                  (princ " " out)) ;; NBSP → space ERROR FIXME
                 ((< code 128) (princ c out))
                 ((< code 256) (format out "\\~8,3,'0r" code)) ;; Latin-1 octal
                 (t (error "PostScript Univode failure")))))))

(defun render-maria-to-rgb (dump mode address width colors)
  "Render Maria tile pixels to a flat RGB byte vector using COLORS (vector of Atari register values).
   Returns PIXELS-WIDE HEIGHT RGB-ARRAY."
  (let* ((pixels-per-byte (ecase mode (:160a 4) (:160b 2)))
         (total-width (* width pixels-per-byte))
         (total-height 16)
         (system-palette
           (ecase *region*
             (:ntsc +prosystem-ntsc-palette+)
             (:pal +prosystem-pal-palette+)))
         (fp (extract-maria-pixels dump mode address width))
         (rgb (make-array (* total-width total-height 3 2)
                          :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (y total-height)
      (dotimes (x total-width)
        (let* ((pen (aref fp x y))
               (reg (elt colors pen))
               (col (if (and (integerp reg) (<= 0 reg 15)
                             (nth reg system-palette))
                        (nth reg system-palette)
                        '(0 0 0)))
               (base (* y total-width 2)))
          (setf (aref rgb (* (+ base (* 2 x)) 3)) (first col)
                (aref rgb (+ (* (+ base (* 2 x)) 3) 1)) (second col)
                (aref rgb (+ (* (+ base (* 2 x)) 3) 2)) (third col)
                (aref rgb (* (+ base (1+ (* 2 x))) 3)) (first col)
                (aref rgb (+ (* (+ base (1+ (* 2 x))) 3) 1)) (second col)
                (aref rgb (+ (* (+ base (1+ (* 2 x))) 3) 2)) (third col)))))
    (values (* total-width 2) total-height rgb)))

(defun write-ps-image (ps rgb-array
                       img-width img-height max-width max-height)
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

(defun write-ps-page-footer (ps
                             &key page pages last-updated)
  "Write PDF page footer with proper formatting.
   Layout:
     | < Icon >  | Skyline-Tool for _Phantasia_ 7800                                                              |              |
     | < ^^^^ >  | 2026-06-30 13:41 --- Bruce-Robert Pocock on Hermes at Star-Hope                                | Page 1 of 19 |
   Icon = two lines tall graphics, _Phantasia_ = italics navy blue, 7800 = machine-directory-name
   Skyline-Tool in 12pt Bold Times-Roman Royal Blue, 'for' 10.5pt Times-Roman black,
   Game-Title in 12pt Italic Times-Roman Navy Blue, machine-dir in 10.5pt Times-Roman Navy Blue
   Date/author/host/site in 75% gray 8pt, page number right-aligned."
  (let* ((site (first (remove-if #'null (list (long-site-name) (short-site-name) ""))))
         (date-part (escape-ps-string (local-time:format-timestring
                                       '(:year "-" :month "-" :day
                                         " "
                                         :hour ":" :minute)
                                       last-updated)))
         (site-part (escape-ps-string (if (emptyp site)
                                          ""
                                          (format nil " at ~a" site)))))
    ;; Icon spanning both rows (48x48 at y=54, translates to 54-102 range)
    (format ps "gsave 56 54 translate~%")
    (write-ps-header-icon ps (skyline-tool-icon))
    (format ps "grestore~%")
    ;; Row 1: Icon + Branding at y=96 baseline
    (format ps "gsave~%")
    ;; "Skyline-Tool" in 12pt Bold Times-Roman, Royal Blue
    (format ps "/Times-Bold-ISOLatin1 findfont 12 scalefont setfont~%")
    (format ps "0.0 0.2 0.6 setrgbcolor~%")
    (format ps "112 96 moveto (Skyline-Tool) show~%")
    ;; " for " in 10.5pt Times-Roman, Black
    (format ps "currentpoint pop 2 add 0 moveto~%")
    (format ps "/Times-Roman-ISOLatin1 findfont 10.5 scalefont setfont~%")
    (format ps "0.0 0.0 0.0 setrgbcolor~%")
    (format ps "(for ) show~%")
    ;; Game-Title in 12pt Italic Times-Roman, Navy Blue
    (format ps "currentpoint pop 2 add 0 moveto~%")
    (format ps "/Times-Italic-ISOLatin1 findfont 12 scalefont setfont~%")
    (format ps "0.0 0.0 0.5 setrgbcolor~%")
    (format ps "(~a) show~%" (escape-ps-string *game-title*))
    ;; machine-dir in 10.5pt Times-Roman, Navy Blue
    (format ps "currentpoint pop 2 add 0 moveto~%")
    (format ps "/Times-Roman-ISOLatin1 findfont 10.5 scalefont setfont~%")
    (format ps "0.0 0.0 0.5 setrgbcolor~%")
    (format ps "(~a) show~%" (escape-ps-string (machine-directory-name)))
    (format ps "grestore~%")
    ;; Row 2: Date/author on host at site + page number (dark gray, 8pt)
    (format ps "gsave~%")
    (format ps "/Times-Roman-ISOLatin1 findfont 8 scalefont setfont~%")
    (format ps "0.25 0.25 0.25 setrgbcolor~%")
    (format ps "112 67.57 moveto~%")
    (format ps "(~a~@[ --- ~a~] on ~a ~a) show~%"
            (escape-ps-string date-part)
            (escape-ps-string (user-real-name))
            (escape-ps-string (title-case (machine-instance)))
            site-part)
    (format ps "556 67.57 moveto~%")
    (format ps "(Page ~d of ~d) dup stringwidth pop neg 0 rmoveto show~%"
            (escape-ps-string (format nil "~:d" page))
            (escape-ps-string (format nil "~:d" pages)))
    (format ps "grestore~%")))
