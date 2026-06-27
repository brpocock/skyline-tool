(in-package :skyline-tool)

(defvar *show-decal-frame* nil)

(defmacro define-show-decal-frame-command (command args &body body)
  `(clim:define-command ,command ,args
     ,@body
     :command-table show-decal-frame))

(clim:define-command-table decal-save-as-menu
  :menu (("As JSON..." :command com-save-decal)
         ("As Text..." :command com-save-decal)
         ("As PDF..." :command com-save-decal)
          ("As PNG..." :command com-save-decal)))

(clim:define-command-table print-decal-menu
  :menu ())

(clim:define-command-table decal-menu
  :menu (("Save As" :menu decal-save-as-menu)
         (nil :divider :line)
         ("Print Decal" :menu print-decal-menu)
         (nil :divider :line)
         ("Switch Decal..." :command com-switch-decal)
         ("Next Palette" :command com-next-palette)
         ("Find Animation Buffer..." :command com-find-animation-buffer)
         ("Open Ext File..." :command com-open-ext-file)
         (nil :divider :line)
         ("Close Decal" :command com-close-frame)))

(clim:define-application-frame show-decal-frame ()
  ((%decal-index :initform 0 :accessor decal-index :initarg :index)
   (%dump :initform (load-dump-into-mem) :accessor decal-from-dump :initarg :dump))
  (:panes (display-pane :application :height 500 :width 800
                                     :display-function 'display-decal-contents)
          (palette-pane :application :height 400 :width 800
                                     :display-function 'display-decal-palette)
          (interactor :interactor :height 50 :width 800))
  (:menu-bar decal-menu-bar)
  (:icon (skyline-tool-icon :resource :decal))
  (:layouts (default (clim:vertically () display-pane palette-pane interactor))))

(clim:define-command-table decal-help-menu
  :menu (("How to Inspect Decals" :command com-help-for-window)
         ("Skyline-Tool Developers' Guide" :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide" :command com-open-scripting-guide)
         (nil :divider :line)
         ("About Skyline-Tool" :command com-about-skyline-tool)))

(clim:define-command-table decal-menu-bar
  :menu (("Decal" :menu decal-menu) ("Edit" :menu edit-menu) ("Help" :menu decal-help-menu)))

(define-show-decal-frame-command (com-new-decal :menu nil :name t) ()
  (format *query-io* "~&New Decal is not yet implemented.~%"))
(define-show-decal-frame-command (com-import-decal :menu nil :name t) ()
  (format *query-io* "~&Import Decal is not yet implemented.~%"))
(define-show-decal-frame-command (com-save-decal :menu nil :name t) ()
  (let* ((frame *show-decal-frame*)
         (dump (decal-from-dump frame))
         (index (decal-index frame))
         (mode (if (plusp (logand #x80 (aref dump (+ index (find-label-from-files "DecalFlags")))))
                   :160b :160a))
         (address (+ (* #x100 (aref dump (+ index (find-label-from-files "DecalArtH"))))
                     (aref dump (+ index (find-label-from-files "DecalArtL")))))
         (width (1+ (logxor #x1f (logand #x1f (aref dump (+ index (find-label-from-files "DecalPalWidth")))))))
         (colors (coerce
                  (loop for i from 0 below #x10
                        collect (cond ((zerop i) (aref dump (find-label-from-files "MapBackground")))
                                     ((<= 1 i 3) (aref dump (+ (find-label-from-files "MapPalettes")
                                                               (* 3 (ash (logand #xe0 (aref dump (+ index (find-label-from-files "DecalPalWidth")))) -5)) (- i 1))))
                                     ((= 4 i) (aref dump (find-label-from-files "VarColor1")))
                                     ((<= 5 i 7) (aref dump (+ (find-label-from-files "MapPalettes") 3
                                                               (* 3 (ash (logand #xe0 (aref dump (+ index (find-label-from-files "DecalPalWidth")))) -5)) (- i 5))))
                                     ((= 8 i) (aref dump (find-label-from-files "VarColor2")))
                                     ((<= 9 i 11) (aref dump (+ (find-label-from-files "MapPalettes") 6
                                                                (* 3 (ash (logand #xe0 (aref dump (+ index (find-label-from-files "DecalPalWidth")))) -5)) (- i 9))))
                                     ((= 12 i) (aref dump (find-label-from-files "VarColor3")))
                                     ((<= 13 i 15) (aref dump (+ (find-label-from-files "MapPalettes") 9
                                                                 (* 3 (ash (logand #xe0 (aref dump (+ index (find-label-from-files "DecalPalWidth")))) -5)) (- i 13))))
                                     (t nil)))
                  'vector)))
    (multiple-value-bind (iw ih rgb) (render-maria-to-rgb dump mode address width colors)
      (let ((path (prompt-save-pathname (format nil "Decal-$~x.png" index) "png")))
        (when path
          (let ((png (make-instance 'zpng:png :width iw :height ih
                                             :color-type :truecolor :bpp 8
                                             :image-data rgb)))
            (zpng:write-png png path))
          (format *query-io* "~&Saved ~a (~dx~d)~%" (namestring path) iw ih)
          (uiop:run-program (list "xdg-open" (namestring path)) :output nil :ignore-error-status t)))))
(define-show-decal-frame-command (com-discover-printers-decal :menu nil :name t) ()
  (populate-decal-print-menu))

(defun %print-decal-to-printer (printer-queue-name)
  (let* ((frame *show-decal-frame*)
         (dump (decal-from-dump frame))
         (index (decal-index frame))
         (decal-mode (if (plusp (logand #x80 (aref dump (+ index (find-label-from-files "DecalFlags")))))
                         :160b :160a))
         (address (+ (* #x100 (aref dump (+ index (find-label-from-files "DecalArtH"))))
                     (aref dump (+ index (find-label-from-files "DecalArtL")))))
         (palette-index (ash (logand #xe0 (aref dump (+ index (find-label-from-files "DecalPalWidth")))) -5))
         (width (1+ (logxor #x1f (logand #x1f (aref dump (+ index (find-label-from-files "DecalPalWidth")))))))
         (has-zone2 (plusp (logand #x40 (aref dump (+ index (find-label-from-files "DecalFlags"))))))
         (palette (loop for i from 0 below #x10
                        collect (cond ((zerop i) (aref dump (find-label-from-files "MapBackground")))
                                      ((<= 1 i 3) (aref dump (+ (find-label-from-files "MapPalettes")
                                                                (* 3 palette-index) (- i 1))))
                                      ((= 4 i) (aref dump (find-label-from-files "VarColor1")))
                                      ((<= 5 i 7) (aref dump (+ (find-label-from-files "MapPalettes") 3
                                                                (* 3 palette-index) (- i 5))))
                                      ((= 8 i) (aref dump (find-label-from-files "VarColor2")))
                                      ((<= 9 i 11) (aref dump (+ (find-label-from-files "MapPalettes") 6
                                                                 (* 3 palette-index) (- i 9))))
                                      ((= 12 i) (aref dump (find-label-from-files "VarColor3")))
                                      ((<= 13 i 15) (aref dump (+ (find-label-from-files "MapPalettes") 9
                                                                  (* 3 palette-index) (- i 13))))
                                      (t nil))))
         (colors (coerce palette 'vector))
         (title (format nil "~a: Decal $~x" (title-case *game-title*) index))
         (author (user-real-name))
         (date-str (multiple-value-bind (s m h d mo y) (get-decoded-time)
                     (declare (ignore s))
                     (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" y mo d h m))))
    (multiple-value-bind (iw ih rgb) (render-maria-to-rgb dump decal-mode address width colors)
      (let* ((base (format nil "Decal-$~x" index))
             (ps-path (format nil "~a.ps" base))
             (pdf-path (format nil "~a.pdf" base)))
        (with-open-file (ps ps-path :direction :output :if-exists :supersede)
          (format ps "%!PS-Adobe-3.0~%")
          (format ps "<< /PageSize [792 612] >> setpagedevice~%")
          (format ps "%%Page: 1 1~%")
          (skyline-tool::write-ps-font-encodings ps)
          (skyline-tool::write-ps-header-bar ps title date-str author (title-case *game-title*))
          (skyline-tool::write-ps-footer ps date-str author (machine-instance) (title-case *game-title*) 1 1)
          (format ps "/Helvetica findfont 9 scalefont setfont~%")
          (format ps "50 500 moveto (Decal: $~x  Mode: ~a  Width: ~d  Palette: ~d) show~%"
                  index decal-mode width palette-index)
          ;; Palette color swatches
          (format ps "gsave~%")
          (dotimes (i (min (length colors) 16))
            (let* ((reg (elt colors i))
                   (col (when (integerp reg)
                          (elt (ecase *region*
                                 (:ntsc +prosystem-ntsc-palette+)
                                 (:pal +prosystem-pal-palette+)) reg))))
              (when col
                (destructuring-bind (r g b) col
                  (format ps "~f ~f ~f setrgbcolor~%" (/ r 255.0) (/ g 255.0) (/ b 255.0))
                  (format ps "~d 470 ~d 10 rectfill~%"
                          (+ 50 (* (mod i 4) 120))
                          (- 60 (* (floor i 4) 14)))
                  (format ps "0 0 0 setrgbcolor~%")
                  (format ps "~d 470 ~d 10 rectstroke~%"
                          (+ 50 (* (mod i 4) 120))
                          (- 60 (* (floor i 4) 14)))))))
          (format ps "grestore~%")
          ;; Sprite image (zone 1)
          (skyline-tool::write-ps-image ps rgb iw ih 490 260)
          (when has-zone2
            (format ps "50 40 moveto (* Zone 2 present) show~%"))
          (format ps "showpage~%"))
        (uiop:run-program (list "ps2pdf" ps-path pdf-path)
                          :output nil :ignore-error-status t)
        (ignore-errors (delete-file ps-path))
        (uiop:run-program (list "lp" "-d" printer-queue-name pdf-path)
                          :output nil :ignore-error-status t)
        (format *query-io* "~&Sent ~a to ~a~%" pdf-path printer-queue-name)))))

(defun populate-decal-print-menu ()
  (ignore-errors
    (clim:remove-menu-item-from-command-table 'print-decal-menu "No printers found")
    (dolist (p (discover-printers))
      (ignore-errors
        (clim:remove-menu-item-from-command-table 'print-decal-menu p))))
  (let* ((printers (discover-printers-with-names)))
    (if (null printers)
        (clim:add-menu-item-to-command-table
         'print-decal-menu "No printers found" :function
         (lambda (g n)
           (declare (ignore g n))
           (format *query-io* "~&No printers discovered.~%")))
        (dolist (pair printers)
          (let ((queue-name (car pair))
                (display-name (cdr pair)))
            (clim:add-menu-item-to-command-table
             'print-decal-menu display-name :function
             (lambda (gesture numeric-arg)
               (declare (ignore gesture numeric-arg))
               (%print-decal-to-printer queue-name))
             :after :end))))))

(clim:define-presentation-type decal-index-value () :inherit-from 'integer)
(clim:define-presentation-type decal-write-mode () :inherit-from 'symbol)

(define-show-decal-frame-command (com-switch-decal :menu t :name t) ((new-index 'integer))
  (setf (decal-index *show-decal-frame*) new-index)
  (clim:redisplay-frame-panes *show-decal-frame* ))

(clim:define-presentation-to-command-translator click-to-switch-to-decal
    (decal-index-value com-switch-decal show-decal-frame
                       :gesture :select :menu nil
                       :documentation "Switch to viewing this decal")
  (object)
  (list object))

(define-show-decal-frame-command (com-next-palette :name t :menu t) ()
  (let* ((address (+ (decal-index *show-decal-frame*)
                     (find-label-from-files "DecalPalWidth")))
         (old-palette (ash (logand #xe0 (aref (decal-from-dump *show-decal-frame*) address))
                           -5)))
    (setf (aref (decal-from-dump *show-decal-frame*) address)
          (logior (logand (logxor #xe0 #xff)
                          (aref (decal-from-dump *show-decal-frame*) address))
                  (ash (if (plusp (logand #x80
                                          (aref (decal-from-dump *show-decal-frame*)
                                                (+ (decal-index *show-decal-frame*)
                                                   (find-label-from-files "DecalFlags")))))
                           #| 160b |# (if (zerop old-palette) 4 0)
                                      #| 160a |# (mod (1+ old-palette) 8))
                       5)))
    (clim:redisplay-frame-panes *show-decal-frame*)))

(clim:define-presentation-to-command-translator click-for-next-palette
    (palette-selector com-next-palette show-decal-frame
     :gesture :edit :menu nil
     :documentation "Switch to next palette")
    (palette-selector)
  ())

(defun read-labels-for-decal-info (&optional (pathname #p"./Object/7800/Bank00.Public.NTSC.o.LABELS.txt"))
  (with-input-from-file (label-file pathname :if-does-not-exist :error)
    (loop for line = (read-line label-file nil nil)
          while line
          for (label$ addr$) = (split-sequence #\= line)
          for label = (string-trim #(#\Space) label$)
          for addr = (cond
                       ((and (> 2 (length (string-trim #(#\Space) addr$)))
                             (char= (char (string-trim #(#\Space) addr$) 0) #\$)
                             (every (rcurry #'member
                                            '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f)
                                            :test #'char-equal)
                                    (subseq (string-trim #(#\Space) addr$) 1)))
                        (parse-integer (subseq (string-trim #(#\Space) addr$) 1)
                                       :radix 16))
                       ((every (rcurry #'member
                                       '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                                       :test #'char=)
                               (string-trim #(#\Space) addr$))
                        (parse-integer (string-trim #(#\Space) addr$))))
          when (or (and label (< 5 (length label)) (search "Decal" label))
                   (and label (< 4 (length label)) (search "Map" label)))
            collect (cons label addr))))

(defun dl-for-decal-status (dump address art-address expected-x)
  (if-let (dll-start (dll-can-reach-dl-entry-p dump address))
    (multiple-value-bind (bytes stringp x art-pointer)
        (decode-header (coerce (subseq dump address (+ 5 address)) 'list)
                       :silentp t)
      (concatenate
       'string
       (if (= (floor expected-x) x)
           "X correct"
           (format nil "X position incorrect, wanted ~d (~d), got ~d"
                   expected-x (floor expected-x) x))
       "; "
       (if (= dll-start (detect-active-dll dump))
           "in current (active) DLL; "
           "in back buffer DLL; ")
       (cond (stringp "header is for a string, not a stamp")
             ((not bytes) "no header decoded at address")
             ((not art-pointer) "no art pointer in header?")
             ((and (= (logand #xff art-pointer)
                      (logand #xff art-address))
                   (> #x1000 (abs (- (logand #xff00 art-pointer)
                                     (logand #xff00 art-address)))))
              (format nil "art address matches; $~4,'0x ≅ $~4,'0x" art-address art-pointer))
             (t (format nil "expected art at $~4,'0x but display list points to $~4,'0x"
                        art-address art-pointer)))))
    "unreachable"))

(defun decode-decal (dump index)
  (let ((addresses (read-labels-for-decal-info)))
    (labels ((fetch (field)
               (elt dump (+ (cdr (assoc field addresses :test #'string=)) index))))
      (let* ((absolute-x (+ (/ (fetch "DecalXFraction") #x100)
                            (fetch "DecalXL") (* #x08 (fetch "DecalXH"))))
             (absolute-y (+ (/ (fetch "DecalYFraction") #x100)
                            (fetch "DecalYL") (* #x10 (fetch "DecalYH"))))
             (expected-x (- absolute-x
                            (+ (* 8 (elt dump (cdr (assoc "MapLeftColumn" addresses :test #'string=))))
                               (elt dump (cdr (assoc "MapLeftPixel" addresses :test #'string=))))
                            7)))
        (format t "~2&#<Decal # $~2,'0x info:" index)
        (format t "~%~10tLocation: ($~2,'0x.~1x.~2,'0x, $~2,'0x.~1x.~2,'0x) = (~6f, ~6f)px = (~a, ~a)tiles"
                (fetch "DecalXH") (fetch "DecalXL") (fetch "DecalXFraction")
                (fetch "DecalYH") (fetch "DecalYL") (fetch "DecalYFraction")
                absolute-x absolute-y
                (floor (+ (fetch "DecalXH")
                          (/ (fetch "DecalXL") #x08)
                          (/ (fetch "DecalXFraction") #x800)))
                (floor (+ (fetch "DecalYH")
                          (/ (fetch "DecalYL") #x10)
                          (/ (fetch "DecalYFraction") #x1000))))
        (cond ((plusp (fetch "DecalObjectH"))
               (format t "~%~10tObject: @ $~2,'0x~2,'0x"
                       (fetch "DecalObjectH") (fetch "DecalObjectL")))
              ((plusp (fetch "DecalObjectL"))
               (format t "~%~10tScenery Object $~2,'0x" (fetch "DecalObjectL")))
              (t (format t "~%~10tNo Object associated")))
        (format t "~%~10tArt: @ $~2,'0x~2,'0x"
                (fetch "DecalArtH") (fetch "DecalArtL"))
        (when (and (= #x50 (fetch "DecalArtH"))
                   (<= 0 (fetch "DecalArtL") #x3f)
                   (zerop (mod (fetch "DecalArtL") 4)))
          (let ((anim-buffer (floor (fetch "DecalArtL") 4)))
            (clim:with-output-as-presentation (*standard-output* anim-buffer 'anim-buffer-index-value)
              (format t "~40tanimation buffer $~x" anim-buffer))))
        (format t "~%~10tPalette: ~d~@[ (flash for ~*~d frame~:p to ~d)~]"
                (ash (logand #xe0 (fetch "DecalPalWidth")) -5)
                (plusp (fetch "DecalFlashTime"))
                (fetch "DecalFlashTime")
                (if (plusp (logand #x80 (fetch "DecalFlags")))
                    (logand 4 (ash (logxor #xe0 (logand #xe0 (fetch "DecalPalWidth"))) -5))
                    (ash (logxor #xe0 (logand #xe0 (fetch "DecalPalWidth"))) -5)))
        (format t "~%~10tWidth: ~d byte~:p"
                (1+ (logxor #x1f (logand #x1f (fetch "DecalPalWidth")))))
        (format t "~%~10tDisplay List Position: ~:[none~;@ $~2,'0x~2,'0x (~a)~]"
                (or (plusp (fetch "DecalDLH"))
                    (plusp (fetch "DecalDLL")))
                (fetch "DecalDLH") (fetch "DecalDLL")
                (dl-for-decal-status dump 
                                     (+ (* #x100 (fetch "DecalDLH")) 
                                        (fetch "DecalDLL"))
                                     (+ (* #x100 (fetch "DecalArtH")) 
                                        (fetch "DecalArtL"))
                                     expected-x))
        (format t "~@[~*~%~10tSecond Display List Position: @ $~2,'0x~2,'0x (~a)~]"
                (or (plusp (fetch "DecalDL2H"))
                    (plusp (fetch "DecalDL2L")))
                (fetch "DecalDL2H") (fetch "DecalDL2L")
                (if (plusp (logand #x40 (fetch "DecalFlags")))
                    (dl-for-decal-status dump 
                                         (+ (* #x100 (fetch "DecalDL2H")) 
                                            (fetch "DecalDL2L"))
                                         (+ (* #x100 (fetch "DecalArtH")) 
                                            (fetch "DecalArtL")
                                            #x10)
                                         expected-x)
                    (dl-for-decal-status dump 
                                         (+ (* #x100 (fetch "DecalDL2H")) 
                                            (fetch "DecalDL2L"))
                                         (+ (* #x100 (fetch "DecalArtH")) 
                                            (fetch "DecalArtL"))
                                         expected-x)))
        (format t "~%~10tDrawing mode: ~a"
                (if (plusp (logand #x80 (fetch "DecalFlags")))
                    "160B" "160A"))
        (format t "~%~10tZone 2 format: ~a"
                (if (plusp (logand #x40 (fetch "DecalFlags")))
                    "aligned" "floating"))
        (format t "~%~10tDisposal mode: ~a"
                (if (plusp (logand #x20 (fetch "DecalFlags")))
                    "particle" "sprite"))
        (format t "~%~10tZ-order: $~2,'0x (~a normal), index: ~d"
                (fetch "DecalZ")
                (cond
                  ((< #x80 (fetch "DecalZ")) "under")
                  ((> #x80 (fetch "DecalZ")) "above")
                  (t "just"))
                (or (loop for i from 0 below (dump-peek "NumDecals" dump)
                          when (= index (dump-peek (+ i (find-label-from-files "DecalsInZOrder")) dump))
                            do (return i))
                    "N/A"))
        (format t "~%~10tFlash time: ~d" (fetch "DecalFlashTime"))
        (format t "~%~10tAnimation State: ~a, ~a"
                (if (plusp (logand #x40 (fetch "DecalAnimationState")))
                    "on tick"
                    "on demand")
                (if (plusp (logand #x80 (fetch "DecalAnimationState")))
                    "redraw demanded"
                    "ready")
                )
        (format t " >~%")
        (finish-output)))))

(defun decode-all-decals (&optional (dump (load-dump-into-mem)))
  (let* ((addresses (read-labels-for-decal-info))
         (num-decals (elt dump (cdr (assoc "NumDecals" addresses :test #'string=)))))
    (format t "~2&Reporting on ~:d decal~:p in dump…" num-decals)
    (dotimes (index num-decals)
      (decode-decal dump index))))

(defun decode-stamp (end-address width &optional (dump (load-dump-into-mem)))
  (loop for row = end-address then (- row #x100)
        while (> row (- end-address #x1000))
        do (terpri)
        do (princ
            (substitute
             #\# #\1
             (substitute
              #\Space #\0
              (format nil "~& ~{~8,'0b~}" (coerce (subseq dump row (+ row width)) 'list)))))))

(defmethod display-decal-contents ((frame show-decal-frame) (display-pane clim:pane))
  (let* ((stream display-pane)
         (decal-mode (if (plusp (logand #x80
                                        (aref (decal-from-dump frame)
                                              (+ (decal-index frame)
                                                 (find-label-from-files "DecalFlags")))))
                         :160b
                         :160a))
         (address (+ (* #x100 (aref (decal-from-dump frame)
                                    (+ (decal-index frame)
                                       (find-label-from-files "DecalArtH")))) 
                     (aref (decal-from-dump frame)
                           (+ (decal-index frame)
                              (find-label-from-files "DecalArtL")))))
         (palette-index (ash
                         (logand #xe0 (aref (decal-from-dump frame)
                                            (+ (decal-index frame)
                                               (find-label-from-files "DecalPalWidth"))))
                         -5))
         (width (1+ (logxor #x1f
                            (logand #x1f (aref (decal-from-dump frame)
                                               (+ (decal-index frame)
                                                  (find-label-from-files "DecalPalWidth")))))))
         (palette (loop for i from 0 below #x10
                        collecting
                        (cond
                          ((zerop i) (aref (decal-from-dump frame)
                                           (find-label-from-files "MapBackground")))
                          ((<= 1 i 3) (aref (decal-from-dump frame)
                                            (+ (find-label-from-files "MapPalettes")
                                               (* 3 palette-index)
                                               (- i 1))))
                          ((= 4 i) (aref (decal-from-dump frame)
                                         (find-label-from-files "VarColor1")))
                          ((<= 5 i 7) (aref (decal-from-dump frame)
                                            (+ (find-label-from-files "MapPalettes")
                                               3
                                               (* 3 palette-index)
                                               (- i 5))))
                          ((= 8 i) (aref (decal-from-dump frame)
                                         (find-label-from-files "VarColor2")))
                          ((<= 9 i 11) (aref (decal-from-dump frame)
                                             (+ (find-label-from-files "MapPalettes")
                                                6
                                                (* 3 palette-index)
                                                (- i 9))))
                          ((= 12 i) (aref (decal-from-dump frame)
                                          (find-label-from-files "VarColor3")))
                          ((<= 13 i 15) (aref (decal-from-dump frame)
                                              (+ (find-label-from-files "MapPalettes")
                                                 9
                                                 (* 3 palette-index)
                                                 (- i 13))))
                          (t nil)))))
    (clim:window-clear stream)
    (format stream "~&Showing contents of decal $~x, address $~4,'0x~%"
            (decal-index frame) address)
    (if (plusp (logand #x40 (aref (decal-from-dump frame)
                                  (+ (decal-index frame)
                                     (find-label-from-files "DecalFlags")))))
        (clim:formatting-table (stream :x-spacing 0 :y-spacing 0)
                               (clim:formatting-row (stream)
                                                    (clim:formatting-cell (stream)
                                                                          (display-maria-art stream
                                                                                             :dump (decal-from-dump frame)
                                                                                             :mode decal-mode
                                                                                             :address address
                                                                                             :colors palette
                                                                                             :width width
                                                                                             :unit 8)))
                               (clim:formatting-row (stream)
                                                    (clim:formatting-cell (stream)
                                                                          (display-maria-art stream
                                                                                             :dump (decal-from-dump frame)
                                                                                             :mode decal-mode
                                                                                             :address (+ #x10 address)
                                                                                             :colors palette
                                                                                             :width width
                                                                                             :unit 8))))
        (display-maria-art stream
                           :dump (decal-from-dump frame)
                           :mode decal-mode
                           :address address
                           :colors palette
                           :width width))
    (terpri stream)
    (clim:with-output-as-presentation (stream decal-mode 'decal-write-mode)
      (format stream "~&Write Mode: ~a" decal-mode))
    (terpri stream)
    (let ((num-decals (aref (decal-from-dump frame)
                            (find-label-from-files "NumDecals"))))
      (format stream "~&All decals: (~d)" num-decals)
      (dotimes (i #x40)
        (when (zerop (mod i 8))
          (terpri stream)
          (unless (zerop (mod i 16))
            (format stream "~5t")))
        (clim:with-output-as-presentation (stream i 'decal-index-value)
          (clim:with-text-face (stream (if (= i (decal-index frame))
                                           :bold
                                           (if (< i num-decals)
                                               :roman
                                               :italic)))
            (format stream " [ $~x ] " i)))))
    (force-output stream)))

(defmethod display-decal-palette ((frame show-decal-frame) (display-pane clim:pane))
  (let* ((stream display-pane)
         (decal-mode (if (plusp (logand #x80
                                        (aref (decal-from-dump frame)
                                              (+ (decal-index frame)
                                                 (find-label-from-files "DecalFlags")))))
                         :160b
                         :160a))
         (palette-index (ash
                         (logand #xe0 (aref (decal-from-dump frame)
                                            (+ (decal-index frame)
                                               (find-label-from-files "DecalPalWidth"))))
                         -5))
         (palette (loop for i from 0 below #x10
                        collecting
                        (cond
                          ((zerop i) (aref (decal-from-dump frame)
                                           (find-label-from-files "MapBackground")))
                          ((<= 1 i 3) (aref (decal-from-dump frame)
                                            (+ (find-label-from-files "MapPalettes")
                                               (* 3 palette-index)
                                               (- i 1))))
                          ((= 4 i) (aref (decal-from-dump frame)
                                         (find-label-from-files "VarColor1")))
                          ((<= 5 i 7) (aref (decal-from-dump frame)
                                            (+ (find-label-from-files "MapPalettes")
                                               3
                                               (* 3 palette-index)
                                               (- i 5))))
                          ((= 8 i) (aref (decal-from-dump frame)
                                         (find-label-from-files "VarColor2")))
                          ((<= 9 i 11) (aref (decal-from-dump frame)
                                             (+ (find-label-from-files "MapPalettes")
                                                6
                                                (* 3 palette-index)
                                                (- i 9))))
                          ((= 12 i) (aref (decal-from-dump frame)
                                          (find-label-from-files "VarColor3")))
                          ((<= 13 i 15) (aref (decal-from-dump frame)
                                              (+ (find-label-from-files "MapPalettes")
                                                 9
                                                 (* 3 palette-index)
                                                 (- i 13))))
                          (t nil)))))
    (clim:window-clear stream)
    (format stream "~&Palette for decal $~x — " (decal-index frame))
    (clim:with-output-as-presentation (stream palette-index 'palette-selector)
      (format stream "using palette ~d" palette-index))
    (ecase decal-mode
      (:160a (dotimes (i 4)
               (let ((entry-name (palette-register-name i palette-index))
                     (color-index (elt palette i)))
                 (terpri stream)
                 (clim:with-output-as-presentation (stream i 'palette-register)
                   (princ entry-name stream))
                 (princ " is set to " stream)
                 (print-clim-color color-index stream))))
      (:160b (dotimes (i 16)
               (let ((entry-name (palette-register-name i palette-index))
                     (color-index (elt palette i)))
                 (terpri stream)
                 (if (and (plusp i) (zerop (mod i 4)))
                     (progn
                       (clim:with-output-as-presentation
                           (stream i 'palette-indirect-register)
                         (princ entry-name stream))
                       (princ " is a reference to " stream)
                       (clim:with-output-as-presentation
                           (stream color-index 'palette-reference)
                         (princ (palette-register-name color-index palette-index)
                                stream))
                       (princ ", which is set to " stream)
                       (print-clim-color (elt palette color-index) stream))
                     (progn
                       (clim:with-output-as-presentation
                           (stream i 'palette-register)
                         (princ entry-name stream))
                       (princ " is set to " stream)
                       (print-clim-color color-index stream)))))))
    (terpri stream)
    (terpri stream)
    (let ((*standard-output* stream))
      (decode-decal (decal-from-dump frame) (decal-index frame))
      (let ((object-h (aref (decal-from-dump frame)
                            (+ (decal-index frame)
                               (find-label-from-files "DecalObjectH")))))
        (when (plusp object-h)
          (let ((object-address (+ (ash object-h 8) 
                                   (aref (decal-from-dump frame)
                                         (+ (decal-index frame)
                                            (find-label-from-files "DecalObjectL"))))))
            (decode-object-at (decal-from-dump frame) object-address)))))
    #+ () (print-machine-palette stream)
    (force-output stream)))

(define-show-decal-frame-command (com-find-animation-buffer :menu t :name t)
    ((buffer 'anim-buffer-index-value))
  (clim-sys:make-process (lambda () (show-animation-buffer buffer 0))
                         :name "Show animation buffer"))

(clim:define-presentation-to-command-translator click-for-animation-buffer
    (anim-buffer-index-value com-find-animation-buffer show-decal-frame
     :gesture :select :menu nil
     :documentation "Show the animation buffer")
    (id)
  (list id))

(define-show-decal-frame-command (com-open-ext-file :name t :menu t)
    ((pathname 'ext-file-link))
  (clim-sys:make-process
   (lambda ()
     (uiop:run-program (list "xdg-open" (enough-namestring pathname))))
   :name (format nil "Edit spreadsheet ~a" (enough-namestring pathname))))

(clim:define-presentation-to-command-translator click-for-ext-file
    (ext-file-link com-open-ext-file show-decal-frame
                   :gesture :edit :menu nil
                   :documentation "Open spreadsheet file for editing")
  (pathname)
  (list pathname))

(defun show-decal (index &key (dump (load-dump-into-mem)))
  "Display (from a core dump) the details of a decal's state"
  (clim-sys:make-process
   (lambda ()
     (let ((frame (clim:make-application-frame 'show-decal-frame
                                               :index index
                                               :dump dump)))
       (let ((*show-decal-frame* frame))
          (setf (clim:frame-pretty-name frame)
                (window-title "Decal"))
         (clim:run-frame-top-level frame))))
   :name "Show Decal"))

(eval-when (:load-toplevel)
  (populate-decal-print-menu))
)
