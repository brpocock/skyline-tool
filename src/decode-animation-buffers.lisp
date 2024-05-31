(in-package :skyline-tool)

;; duplicated declaration
(clim:define-presentation-type decal-index-value () :inherit-from 'integer)

(clim:define-application-frame anim-buffer-frame ()
  ((%anim-buffer-index :initform 0 :accessor anim-buffer-index :initarg :index)
   (%mode :initform :160b :accessor anim-buffer-mode :initarg :mode)
   (%palette :initform 4 :accessor anim-buffer-palette :initarg :palette)
   (%colors :accessor anim-buffer-colors)
   (%dump :initform (load-dump-into-mem) :accessor anim-buffer-from-dump :initarg :dump)
   (%offset :initform 0 :accessor anim-buffer-offset :initarg :offset))
  (:panes (display-pane :application :height 400 :width 800
                                     :display-function 'display-anim-buffer-contents)
          (palette-pane :application :height 300 :width 800
                                     :display-function 'display-anim-buffer-palette)
          (interactor :interactor :height 200 :width 800))
  (:layouts (default (clim:vertically () display-pane palette-pane interactor))))

(defun set-animation-buffer-colors (frame)
  (setf (anim-buffer-colors frame)
        (loop for i from 0 below #x10
              collecting
              (cond
                ((zerop i) (dump-peek "MapBackground"))
                ((<= 1 i 3) (dump-peek (+ (find-label-from-files "MapPalettes")
                                          (* 3 (anim-buffer-palette frame))
                                          (- i 1))))
                ((= 4 i) (dump-peek (+ (find-label-from-files "MapPalettes")
                                       (logand #xf (dump-peek "VarColor1")))))
                ((<= 5 i 7) (dump-peek (+ (find-label-from-files "MapPalettes")
                                          3
                                          (* 3 (anim-buffer-palette frame))
                                          (- i 5))))
                ((= 8 i) (dump-peek (+ (find-label-from-files "MapPalettes")
                                       (logand #xf (dump-peek "VarColor2")))))
                ((<= 9 i 11) (dump-peek (+ (find-label-from-files "MapPalettes")
                                           6
                                           (* 3 (anim-buffer-palette frame))
                                           (- i 9))))
                ((= 12 i) (dump-peek (+ (find-label-from-files "MapPalettes")
                                        (logand #xf (dump-peek "VarColor3")))))
                ((<= 13 i 15) (dump-peek (+ (find-label-from-files "MapPalettes")
                                            9
                                            (* 3 (anim-buffer-palette frame))
                                            (- i 13))))
                (t nil)))))

(defmethod initialize-instance :after ((self anim-buffer-frame) &rest _)
  (declare (ignore _))
  (set-animation-buffer-colors self))

(defvar *anim-buffer-frame* nil)

(define-anim-buffer-frame-command (com-switch-buffer :menu t :name t) ((new-buffer 'integer))
  (setf (anim-buffer-index *anim-buffer-frame*) new-buffer)
  (clim:redisplay-frame-panes *anim-buffer-frame*))

(define-anim-buffer-frame-command (com-find-decal :menu t :name t)
    ((decal 'decal-index-value))
  (clim-sys:make-process (lambda () (show-decal decal))
               :name "Show decal"))

(clim:define-presentation-type palette-color () :inherit-from 'integer)
(clim:define-presentation-type palette-reference () :inherit-from 'integer)
(clim:define-presentation-type palette-register () :inherit-from 'integer)
(clim:define-presentation-type palette-indirect-register () :inherit-from 'integer)
(clim:define-presentation-type palette-selector () :inherit-from 'integer)
(clim:define-presentation-type offset-selector () :inherit-from 'integer)
(clim:define-presentation-type anim-buffer-mode () :inherit-from 'keyword)
(clim:define-presentation-type anim-buffer-index-value () :inherit-from 'integer)

(clim:define-presentation-to-command-translator click-to-edit-color
    (palette-register com-change-color anim-buffer-frame
     :gesture :edit :menu nil
     :documentation "Change the color in this palette register")
    (frame)
  ())

(clim:define-drag-and-drop-translator change-palette-color-value
    (palette-color palette-register palette-register
                   anim-buffer-frame
                   :gesture :select
                   :documentation "Set the palette register to a certain color")
    (frame color reg)
  (setf (elt (anim-buffer-palette frame) reg) color))

(clim:define-presentation-to-command-translator click-to-toggle-mode
    (anim-buffer-mode com-toggle-mode anim-buffer-frame
     :gesture :edit :menu nil
     :documentation "Toggle interpretation in mode 160A/B")
    (frame)
  ())

(clim:define-presentation-to-command-translator click-for-next-palette
    (palette-selector com-next-palette anim-buffer-frame
     :gesture :edit :menu nil
     :documentation "Switch to next palette")
    (frame)
  ())

(clim:define-presentation-to-command-translator click-for-next-color
    (palette-indirect-register com-switch-indirect-color anim-buffer-frame
     :gesture :edit :menu nil
     :documentation "Switch to next color in palette")
    (frame)
  ())

(clim:define-presentation-to-command-translator click-for-next-offset
    (offset-selector com-next-offset anim-buffer-frame
     :gesture :edit :menu nil
     :documentation "Switch to next offset")
    (frame)
  ())

(clim:define-presentation-to-command-translator click-to-switch-to-buffer
    (anim-buffer-index-value com-switch-buffer anim-buffer-frame
     :gesture :select :menu nil
     :documentation "Switch to viewing this buffer")
    (object)
  (list object))

(defmethod display-anim-buffer-palette ((frame anim-buffer-frame) (display-pane clim:pane))
  (let ((stream display-pane))
    (clim:window-clear stream)
    (format stream "~&Palette for animation buffer $~x — " (anim-buffer-index frame))
    (clim:with-output-as-presentation (stream (anim-buffer-palette frame) 'palette-selector)
      (format stream "using palette ~d" (anim-buffer-palette frame)))
    (ecase (anim-buffer-mode frame)
      (:160a (dotimes (i 4)
               (let ((entry-name (palette-register-name i
                                                        (anim-buffer-palette frame)))
                     (color-index (elt (anim-buffer-colors frame) i)))
                 (terpri stream)
                 (clim:with-output-as-presentation (stream i 'palette-register)
                   (princ entry-name stream))
                 (princ " is set to " stream)
                 (print-clim-color color-index stream))))
      (:160b (dotimes (i 16)
               (let ((entry-name (palette-register-name i (anim-buffer-palette frame)))
                     (color-index (elt (anim-buffer-colors frame) i)))
                 (terpri stream)
                 (if (and (plusp i) (zerop (mod i 4)))
                     (progn
                       (clim:with-output-as-presentation (stream i 'palette-indirect-register)
                         (princ entry-name stream))
                       (princ " is a reference to " stream)
                       (clim:with-output-as-presentation (stream (logand #xf color-index)
                                                                 'palette-reference)
                         (princ (palette-register-name (logand #xf color-index)
                                                       (anim-buffer-palette frame))
                                stream))
                       (princ ", which is set to " stream)
                       (print-clim-color (elt (anim-buffer-colors frame)
                                              (logand #xf color-index))
                                         stream))
                     (progn
                       (clim:with-output-as-presentation (stream i 'palette-register)
                         (princ entry-name stream))
                       (princ " is set to " stream)
                       (print-clim-color color-index stream)))))))
    (print-machine-palette stream)
    (force-output stream)))

(defmethod display-anim-buffer-contents ((frame anim-buffer-frame) (display-pane clim:pane))
  (let ((stream display-pane)
        (address (+ #x5000
                    (anim-buffer-offset frame)
                    (* 4 (anim-buffer-index frame)))))
    (setf (anim-buffer-offset frame) (or (anim-buffer-offset frame) 0))
    (clim:window-clear stream)
    (format stream "~&Showing contents of animation buffer $~x, " (anim-buffer-index frame))
    (clim:with-output-as-presentation (stream (anim-buffer-offset frame) 'offset-selector)
      (format stream "relative offset ~:[+~;-~]$~4,'0x"
              (minusp (anim-buffer-offset frame))
              (abs (anim-buffer-offset frame))))
    (format stream ", address $~4,'0x" address)
    (terpri stream)
    (display-maria-art stream
                       :dump (anim-buffer-from-dump frame)
                       :mode (anim-buffer-mode frame)
                       :colors (anim-buffer-colors *anim-buffer-frame*)
                       :address address
                       :width 4
                       :unit 8
                       :var-colors (list (dump-peek "VarColor1")
                                         (dump-peek "VarColor2")
                                         (dump-peek "VarColor3")))
    (terpri stream)
    (clim:with-output-as-presentation (stream (anim-buffer-mode frame) 'anim-buffer-mode)
      (format stream "~&Write Mode: ~a" (anim-buffer-mode frame)))
    (terpri stream)
    (format stream "~&All buffers: ")
    (dotimes (buf #x10)
      (clim:with-output-as-presentation (stream buf 'anim-buffer-index-value)
        (clim:with-text-face
            (stream (if (some (lambda (decal)
                                (and (= #x50
                                        (dump-peek (+ decal (find-label-from-files "DecalArtH"))
                                                   (anim-buffer-from-dump frame)))
                                     (= (* 4 buf)
                                        (dump-peek (+ decal (find-label-from-files "DecalArtL"))
                                                   (anim-buffer-from-dump frame)))))
                              (loop for i below #x40 collect i))
                        (if (= buf (anim-buffer-index frame))
                            :bold
                            :roman)
                        (if (= buf (anim-buffer-index frame))
                            (if (= (* 4 buf)
                                   (dump-peek "AnimationBackBuffer"))
                                :bold
                                :italic)
                            (if (= (* 4 buf)
                                   (dump-peek "AnimationBackBuffer"))
                                :roman
                                :italic))))
          (format stream " [ $~x ] " buf))))
    (terpri stream)
    (terpri stream)
    (loop for decal below #x40
          when (and (= #x50
                       (dump-peek (+ decal (find-label-from-files "DecalArtH"))
                                  (anim-buffer-from-dump frame)))
                    (= (* 4 (anim-buffer-index *anim-buffer-frame*))
                       (dump-peek (+ decal (find-label-from-files "DecalArtL"))
                                  (anim-buffer-from-dump frame))))
            do (clim:with-output-as-presentation (stream decal 'decal-index-value)
                 (format stream "~%In use by decal $~x" decal)))
    (when (= (dump-peek "AnimationBackBuffer")
             (* 4 (anim-buffer-index frame)))
      (format stream "~%This is the Animation Back Buffer."))
    (force-output stream)))

(define-anim-buffer-frame-command (com-change-color :name t :menu t)
    ((palette-color 'palette-register) (color-index '(or integer null)))
  (assert (<= 0 palette-color 15) (palette-color)
          "Palette register number must be 0-15, got ~d" palette-color)
  (when (null color-index)
    (setf color-index (clim:prompt-for-accept *standard-input*
                                              'palette-color
                                              nil
                                              :prompt "New Color")))
  (when (null color-index)
    (format *standard-input* "~2&NULL color chosen, not acceptable. Try again.")
    (return-from com-change-color))
  (assert (<= 0 color-index #xff) (color-index)
          "Color index number must be 0-255 ($00-$ff), got ~d ($~x)" color-index color-index)
  (format *standard-input* "~2&Palette color register ~a will be interpreted as $~2,'0x (~a)"
          (palette-register-name palette-color (anim-buffer-palette *anim-buffer-frame*))
          color-index (atari-colu-string color-index))
  (force-output *standard-input*)
  (setf (elt (anim-buffer-colors *anim-buffer-frame*) palette-color) color-index)
  (clim:redisplay-frame-panes *anim-buffer-frame*))

(define-anim-buffer-frame-command (com-switch-indirect-color :name t :menu t)
    ((palette-indirect-color 'palette-indirect-register)
     (register-with-direct-color 'palette-register))
  (assert (member palette-indirect-color '(4 8 12))
          (palette-indirect-color)
          "Palette register number must be 4, 8, or 12 ($c), got ~d" palette-indirect-color)
  (assert (and (<= 0 register-with-direct-color)
               (plusp (mod register-with-direct-color 4)))
          (register-with-direct-color)
          "Palette variable reference must point to a non-variable palette register 1-3, e.g. P2C2, not ~d"
          register-with-direct-color)
  (format *standard-input* "~2&Palette color register ~a will be a reference to ~a"
          (palette-register-name palette-indirect-color
                                 (anim-buffer-palette *anim-buffer-frame*))
          (palette-register-name register-with-direct-color
                                 (anim-buffer-palette *anim-buffer-frame*)))
  (force-output *standard-input*)
  (setf (elt (anim-buffer-colors *anim-buffer-frame*) palette-indirect-color) register-with-direct-color)
  (clim:redisplay-frame-panes *anim-buffer-frame*))

(define-anim-buffer-frame-command (com-toggle-mode :name t :menu t) ()
  (setf (anim-buffer-mode *anim-buffer-frame*)
        (ecase (anim-buffer-mode *anim-buffer-frame*)
          (:160a
           (setf (anim-buffer-palette *anim-buffer-frame*)
                 (if (zerop (anim-buffer-palette *anim-buffer-frame*))
                     0 4))
           (loop until (zerop (mod (anim-buffer-offset *anim-buffer-frame*) #x1000))
                 do (com-next-offset))
           :160b)
          (:160b :160a)))
  (set-animation-buffer-colors *anim-buffer-frame*)
  (clim:redisplay-frame-panes *anim-buffer-frame*))

(define-anim-buffer-frame-command (com-next-palette :name t :menu t) ()
  (setf (anim-buffer-palette *anim-buffer-frame*)
        (ecase (anim-buffer-mode *anim-buffer-frame*)
          (:160a (mod (1+ (anim-buffer-palette *anim-buffer-frame*)) 8))
          (:160b (if (zerop (anim-buffer-palette *anim-buffer-frame*))
                     4 0))))
  (set-animation-buffer-colors *anim-buffer-frame*)
  (clim:redisplay-frame-panes *anim-buffer-frame*))

(define-constant +anim-buffer-offsets+
    (list 0 (- #x1000) #x1000)
  :test 'equalp)

(define-anim-buffer-frame-command (com-next-offset :name t :menu t) ()
  (let* ((current-offset-index (or (position (anim-buffer-offset *anim-buffer-frame*)
                                             +anim-buffer-offsets+)
                                   0))
         (new-offset (elt +anim-buffer-offsets+
                          (mod (1+ current-offset-index)
                               (ecase (anim-buffer-mode *anim-buffer-frame*)
                                 (:160a 6)
                                 (:160b 3))))))
    (setf (anim-buffer-offset *anim-buffer-frame*) new-offset))
  (clim:redisplay-frame-panes *anim-buffer-frame*))

(defun show-animation-buffer (&optional (index 0) &key (mode :160b) (palette 4)
                                                       (dump (load-dump-into-mem)))
  "Show (from a core dump) the state of the animation buffers"
  (flet ((run ()
           (let ((frame (clim:make-application-frame 'anim-buffer-frame
                                                     :index index
                                                     :mode mode
                                                     :palette palette
                                                     :dump dump)))
             (let ((*anim-buffer-frame* frame))
               (setf (clim:frame-pretty-name frame)
                     (format nil "Show Animation Buffer"))
               (clim:run-frame-top-level frame)))))
    (run)))

(clim:define-presentation-to-command-translator click-for-decal
    (decal-index-value com-find-decal anim-buffer-frame
     :gesture :select :menu nil
     :documentation "Show the decal")
    (id)
  (list id))

(defun decode-animation-buffers (&optional (dump (load-dump-into-mem)))
  (format t "~&Decoding animation buffers' status, to view contents (SHOW-ANIMATION-BUFFER n)")
  (let ((busy-start (find-label-from-files "AnimationBufferBusy"))
        (buffer-info (make-array (list #x10 2) :initial-element nil))
        (back-buffer/4 (/ (dump-peek "AnimationBackBuffer") 4)))
    (unless (integerp back-buffer/4) ()
            (cerror "Continue, ignore"
                    "AnimationBackBuffer points to $~4,'0x, which is not (and integer (mod 4))"
                    (* 4 back-buffer/4))
            (setf back-buffer/4 (floor back-buffer/4)))
    (setf (aref buffer-info back-buffer/4 1)
          "AnimationBackBuffer")
    (dotimes (i #x10)
      (when (plusp (dump-peek (+ i busy-start) dump))
        (setf (aref buffer-info i 0) t)))
    (dotimes (i (dump-peek "NumDecals" dump))
      (let ((art-pointer-l (dump-peek (+ i (find-label-from-files "DecalArtL")) dump))
            (art-pointer-h (dump-peek (+ i (find-label-from-files "DecalArtH")) dump)))
        (when (and (= #x50 art-pointer-h)
                   (<= 0 art-pointer-l #x0f))
          (unless (zerop (mod art-pointer-l 4))
            (cerror "Continue, ignore"
                    "Decal $~2,'0x points to $50~2,'0x, which is not (and integer (mod 4))"
                    i art-pointer-l)
            (setf art-pointer-l (* 4 (floor art-pointer-l 4))))
          (if (aref buffer-info (/ art-pointer-l 4) 1)
              (setf (aref buffer-info (/ art-pointer-l 4) 1)
                    (format nil "~a, AND ALSO decal $~2,'0x"
                            (aref buffer-info (/ art-pointer-l 4) 1)
                            i))
              (setf (aref buffer-info (/ art-pointer-l 4) 1) i)))))
    (dotimes (i #x10)
      (cond
        ((and (aref buffer-info i 0)
              (aref buffer-info i 1))
         (format t "~&Buffer $~x is in use by decal $~2,'0x."
                 i (aref buffer-info i 1)))
        ((aref buffer-info i 0)
         (format t "~&Buffer $~x is marked busy but is not in use by any decal.~40t ← BAD" i))
        ((aref buffer-info i 1)
         (format t "~&Buffer $~x is marked available but is in use by decal $~2,'0x.~40t ← BAD"
                 i (aref buffer-info i 1)))
        (t (format t "~&Buffer $~x is free." i))))))
