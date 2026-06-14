(in-package :skyline-tool)

(defun vcs-ntsc-color-names ()
  (loop for hue below #x10
        appending (loop for value below #x10 by 2
                        collecting (format nil "~a $~x"
                                           (subseq (string (elt +atari-ntsc-color-names+ hue)) 3)
                                           value))))

(defun vcs-pal-color-names ()
  (loop for hue below #x10
        appending (loop for value below #x10 by 2
                        collecting (format nil "~a $~x"
                                           (subseq (string (elt +atari-pal-color-names+ hue)) 3)
                                           value))))

(defun prosystem-ntsc-color-names ()
  (loop for hue below #x10
        appending (loop for value below #x10
                        collecting (format nil "~a $~x"
                                           (subseq (string (elt +atari-ntsc-color-names+ hue)) 3)
                                           value))))

(defun prosystem-pal-color-names ()
  (loop for hue below #x10
        appending (loop for value below #x10
                        collecting (format nil "~a $~x"
                                           ;; XXX these are NTSC color names
                                           (subseq (string (elt +atari-pal-color-names+ hue)) 3)
                                           value))))

(defun tty-xterm-p (&optional (stream *query-io*))
  "Returns a generalized true value if the terminal seems to be xterm-compatible"
  (and (not (equal "CLIM-CLX" (symbol-package (class-name (class-of stream)))))
       (search "xterm" (uiop:getenv "TERM"))))

(defun write-gimp-palette (name colors &optional color-names)
  (with-output-to-file (pal (make-pathname :name name
                                           :type "gpl"
                                           :directory '(:relative "Tools"))
                            :if-exists :supersede)
    (format pal "GIMP Palette
Name: ~a
Columns: ~d
#~{~%~3d ~3d ~3d # ~a~}~%"
            (substitute #\Space #\- name)
            (ecase (length colors)
              ((1 2 4 8) (length colors))
              ((16 32 64) 16)
              (128 8)
              (256 16)
              (512 32)
              (4096 16))
            (if color-names
                (mapcan (lambda (rgb n) (append rgb (list n)))
                        colors color-names)
                (mapcan (lambda (rgb) (append rgb (list (format nil "#~{~2,'0x~2,'0x~2,'0x~}" rgb))))
                        colors))))
  (format *trace-output* "~&Wrote ~:d color~:p palette “~a”~%"
          (length colors)
          (substitute #\Space #\- name))
  (when (<= (length colors) 256)
    (let ((i 0))
      (dolist (color colors)
        (print-wide-pixel color *trace-output*)
        (cond
          ((< (length colors) 20)
           (format *trace-output* " ~a~%"(elt color-names i)))
          (t
           (when (= 15 (mod i 16))
             (terpri *trace-output*))))
        (incf i))))
  (format *trace-output* "~C[0m" #\Escape))

(defun write-gimp-palettes ()
  "Write out Gimp palettes for those I know"
  (write-gimp-palette "Atari-2600-NTSC" +vcs-ntsc-palette+ (vcs-ntsc-color-names))
  (write-gimp-palette "Atari-2600-PAL" +vcs-pal-palette+ (vcs-pal-color-names))
  (write-gimp-palette "Atari-2600-SECAM" +vcs-secam-palette+
                      (mapcar (lambda (s) (cl-change-case:title-case (subseq (string s) 3)))
                              +vcs-secam-color-names+))
  (write-gimp-palette "Atari-7800-NTSC" +prosystem-ntsc-palette+ (prosystem-ntsc-color-names))
  (write-gimp-palette "Atari-7800-PAL" +prosystem-pal-palette+ (prosystem-pal-color-names))
  (write-gimp-palette "Commodore-64" +c64-palette+
                      (mapcar (compose #'cl-change-case:title-case
                                       #'string)
                              +c64-names+))
  (write-gimp-palette "NES-NTSC" +nes-palette-ntsc+)
  (write-gimp-palette "NES-PAL" +nes-palette-pal+)
  (write-gimp-palette "TurboGrafx-16" +tg16-palette+)
  (write-gimp-palette "Lynx" +lynx-palette+)
  (write-gimp-palette "Intellivision" +intv-palette+
                      (mapcar (compose #'cl-change-case:title-case
                                       #'string)
                              +intv-color-names+)))
(defun palette-reference (rgb palette &key allow-imperfect-p)
  (or (position rgb palette :test 'equalp)
      (if allow-imperfect-p
          (let ((nearest (find-nearest-in-palette (copy-list palette)
                                                  (first rgb)
                                                  (second rgb)
                                                  (third rgb))))
            (or (position nearest palette :test 'equalp)
                (error "Could not map ~s to anything close to palette ~s (wanted ~s)"
                       rgb palette nearest)))
          (error "Palette value ~s is not in palette ~s" rgb palette))))

