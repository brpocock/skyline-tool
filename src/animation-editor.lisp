(in-package :skyline-tool)
(defvar *anim-seq-editor-frame* nil)
(defvar *show-tileset-frame* nil)
(defvar *anim-seq-assign-frame* nil)
(defvar *anim-seq-assigns-frame* nil)
(defvar *animation-sequences* nil)

(defun all-tilesets ()
  (loop for file in (directory (make-pathname :directory (list :relative "Source" "Maps" "Tiles")
                                              :name :wild :type "tsx"))
        when (search "Tiles" (pathname-name file))
          collect (pathname-name file)))

(defun all-scenery-decals ()
  (loop for file in (directory (make-pathname :directory (list :relative "Source" "Maps" "Tiles")
                                              :name :wild :type "tsx"))
        when (and (not (string= "CommonDecals" (pathname-name file)))
                  (not (search "Decals2" (pathname-name file)))
                  (search "Decals" (pathname-name file)))
          collect (pathname-name file)))

(defun all-npc-art-sheets-for-kind (decal-kind)
  (ecase decal-kind
    (:human '("NPCs"))
    (:vizier '("Vizier"))
    (:nefertem '("PlayerTiles"))
    ((:earl :captain :princess :elder) '("MainNPCs"))
    (:phantom '("Phantoms"))
    (:sailor '("Sailors"))
    (:player '("PlayerTiles"))
    (:enemy
     (loop for file in (directory (make-pathname :directory (list :relative "Source" "Art")
                                                 :name :wild :type "png"))
           when (search "Enemies" (pathname-name file))
             collect (pathname-name file)))
    (:block1 '("NPCBlock1"))
    (:block2 '("NPCBlock2"))
    (:block3 '("NPCBlock3"))
    (:block4 '("NPCBlock4"))))

(defun decal-kind-name (decal-kind)
  (ecase decal-kind
    (:human "Generic Human NPCs")
    (:vizier "The Grand Vizier")
    (:nefertem "Nefertem")
    (:earl "Earl Ulluk")
    (:captain "Captain Caspar")
    (:princess "Princess Aisling")
    (:elder "Elder Tranh")
    (:phantom "Phantom Soldiers")
    (:sailor "Sailors")
    (:player "The Player")
    (:enemy "Enemies")
    (:block1 "NPC Block 1")
    (:block2 "NPC Block 2")
    (:block3 "NPC Block 3")
    (:block4 "NPC Block 4")))

(define-constant +decal-kinds+
    '( :player :human :earl :captain :princess :elder :nefertem
      :vizier :phantom :sailor :enemy :block1 :block2 :block3 :block4)
  :test 'equalp)

(clim:define-presentation-type simple-animation-sequence-index () :inherit-from 'integer)
(clim:define-presentation-type simple-animation-sequence-major-kind () :inherit-from 'symbol)
(clim:define-presentation-type simple-animation-sequence-decal-kind () :inherit-from 'symbol)
(clim:define-presentation-type simple-animation-sequence-decal-body () :inherit-from 'integer)
(clim:define-presentation-type simple-animation-sequence-label () :inherit-from 'string)
(clim:define-presentation-type simple-animation-sequence-tile-sheet-name () :inherit-from 'string)
(clim:define-presentation-type simple-animation-sequence-frame-rate-scalar () :inherit-from 'rational)
(clim:define-presentation-type simple-animation-sequence-frame-count () :inherit-from 'integer)
(clim:define-presentation-type simple-animation-sequence-frame-reference () :inherit-from 'integer)
(clim:define-presentation-type animation-assignment-slot () :inherit-from 'integer)
(clim:define-presentation-type anim-seq-action () :inherit-from 'symbol)
(clim:define-presentation-type anim-seq-facing () :inherit-from 'symbol)
(clim:define-presentation-type anim-seq-assign-sequence () :inherit-from 'integer)
(clim:define-presentation-type magic-four-ways-button () :inherit-from 'symbol)
(clim:define-presentation-type new-animation-sequence () :inherit-from 'symbol)

(defclass simple-animation-sequence ()
  ((index :accessor simple-animation-sequence-index :initarg :index)
   (label :accessor simple-animation-sequence-label :initarg :label :initform nil)
   (major-kind :type (member :background :scenery :npc)
               :accessor simple-animation-sequence-major-kind :initarg :major-kind)
   (decal-kind :type #.(cons 'member (copy-list +decal-kinds+))
               :initform :human
               :accessor simple-animation-sequence-decal-kind :initarg :decal-kind)
   (decal-body :type integer
               :initform 0
               :accessor simple-animation-sequence-decal-body :initarg :decal-body)
   (frame-rate-scalar :type rational :initform 0
                      :accessor simple-animation-sequence-frame-rate-scalar
                      :initarg :frame-rate-scalar)
   (frame-count :initform 0
                :accessor simple-animation-sequence-frame-count
                :initarg :frame-count)
   (tile-sheet-name :type string
                    :accessor simple-animation-sequence-tile-sheet
                    :initarg :tile-sheet)
   (write-mode :type (member :160a :160b) :initform :160a
               :accessor simple-animation-sequence-write-mode
               :initarg :write-mode)
   (bytes-width :type (integer 2 32) :initform 2
                :accessor simple-animation-sequence-bytes-width
                :initarg :bytes-width)
   (frames :type (vector integer 8)
           :accessor simple-animation-sequence-frames
           :initarg :frames)))

(defmethod initialize-instance :after ((seq simple-animation-sequence) &key &allow-other-keys)
  (setf (simple-animation-sequence-write-mode seq)
        (ecase (simple-animation-sequence-major-kind seq)
          ((:background :scenery) :160a)
          (:npc (case (simple-animation-sequence-decal-kind seq)
                  ((:nefertem :phantom :enemy) :160a)
                  (otherwise :160b))))))

(clim:define-application-frame anim-seq-editor-frame ()
  ((%seq-index :initform 0 :accessor anim-seq-editor-index :initarg :sequence)
   (%sequence :accessor anim-seq-editor-sequence)
   (%palette :type integer :initform 0 :accessor anim-seq-editor-palette
             :initarg :palette))
  (:panes (anim-seq-filmstrip-pane :application :height 550 :width 1800
                                                :display-function 'display-anim-seq-filmstrip)
          (interactor :interactor :height 250 :width 400)
          (anim-seq-detail-pane :application :height 250 :width 700
                                             :display-function 'display-anim-seq-properties)
          (anim-seq-preview-pane :application :height 250 :width 700
                                              :display-function 'display-anim-preview))
  (:layouts (default (clim:vertically ()
                       anim-seq-filmstrip-pane
                       (clim:horizontally ()
                         interactor
                         anim-seq-detail-pane
                         anim-seq-preview-pane)))))

(defun find-animation-sequence (id)
  (when id
    (elt *animation-sequences* id)))

(defmethod initialize-instance :after ((frame anim-seq-editor-frame)
                                       &key sequence palette &allow-other-keys)
  (setf (anim-seq-editor-sequence frame) (find-animation-sequence sequence))
  (update-params frame)
  (unless palette
    (setf (anim-seq-editor-palette frame) (ecase (simple-animation-sequence-write-mode
                                                  (anim-seq-editor-sequence frame))
                                            (:160b 4)
                                            (:160a (case (simple-animation-sequence-decal-kind
                                                          (anim-seq-editor-sequence frame))
                                                     (:nefertem 6)
                                                     (otherwise 0)))))
    (clim:redisplay-frame-panes frame)))

(defmethod update-params ((frame anim-seq-editor-frame))
  (unless (slot-boundp frame '%sequence)
    (setf (anim-seq-editor-sequence frame)
          (find-animation-sequence (anim-seq-editor-index frame))))
  (let* ((major-kind (simple-animation-sequence-major-kind
                      (anim-seq-editor-sequence frame)))
         (name (simple-animation-sequence-tile-sheet
                (anim-seq-editor-sequence frame)))
         (valid (ecase major-kind
                  (:background (all-tilesets))
                  (:scenery (all-scenery-decals) )
                  (:npc (all-npc-art-sheets-for-kind
                         (simple-animation-sequence-decal-kind
                          (anim-seq-editor-sequence frame)))))))
    (unless (member name valid :test #'string=)
      (setf (simple-animation-sequence-tile-sheet
             (anim-seq-editor-sequence frame))
            (first (sort valid
                         (lambda (a b)
                           (> (prefix-match-length a name)
                              (prefix-match-length b name)))))))
    (setf (simple-animation-sequence-write-mode (anim-seq-editor-sequence frame))
          (ecase major-kind
            ((:background :scenery) :160a)
            (:npc (case (simple-animation-sequence-decal-kind
                         (anim-seq-editor-sequence frame))
                    ((:nefertem :phantom :enemy) :160a)
                    (otherwise :160b)))))
    (setf (simple-animation-sequence-bytes-width (anim-seq-editor-sequence frame))
          (ecase (simple-animation-sequence-write-mode (anim-seq-editor-sequence frame))
            (:160a 2) (:160b 4)))
    (when (eql :160b (simple-animation-sequence-write-mode (anim-seq-editor-sequence frame)))
      (setf (anim-seq-editor-palette frame) (if (zerop (anim-seq-editor-palette frame))
                                                0 4))))
  (clim:redisplay-frame-panes frame))

(defun load-tile-sheet-object-by-name (name artp)
  (let ((mem (make-array 4096 :element-type '(unsigned-byte 8))))
    (with-input-from-file
        (bin (if artp
                 (cond
                   ((string= "Phantoms" name)
                    (make-pathname
                     :directory (list :relative "Object" "Assets")
                     :name "Art.SandyIslandEnemies" :type "o"))
                   ((string= "Sailors" name)
                    (make-pathname
                     :directory (list :relative "Object" "Assets")
                     :name "Art.Sailors" :type "o"))
                   ((string= "Nefertem" name)
                    (make-pathname
                     :directory (list :relative "Object" "Assets")
                     :name "Art.CharacterEffectsTiles" :type "o"))
                   (t (make-pathname
                       :directory (list :relative "Object" "Assets")
                       :name (format nil "Art.~a" name)
                       :type "o")))
                 (make-pathname
                  :directory (list :relative "Object" "Assets")
                  :name (format nil "Tileset.~a" name)
                  :type "o"))
             :element-type '(unsigned-byte 8))
      (loop for byte = (read-byte bin nil nil)
            for i from 0 below #x10000
            while byte
            do (setf (aref mem i) byte)))
    mem))

(defun read-palette-for-tile-sheet (tile-sheet-name palette-index &key write-mode)
  (let ((tileset-pathname (make-pathname :directory (list :relative "Source" "Maps" "Tiles")
                                         :name tile-sheet-name
                                         :type "tsx")))
    (unless (probe-file tileset-pathname)
      (setf tileset-pathname (make-pathname :directory (list :relative "Source" "Maps" "Tiles")
                                            :name "SandyIslandTiles"
                                            :type "tsx")))
    (let* ((tileset (load-tileset tileset-pathname))
           (palettes (extract-palettes (tileset-image tileset)))
           (colors-count (ecase write-mode
                           (:160a 4) (:160b 16)))
           (colors (make-array colors-count :element-type 'integer)))
      (ecase write-mode
        (:160a (dotimes (i 4)
                 (setf (aref colors i)
                       (aref palettes palette-index i))))
        (:160b (dotimes (j 4)
                 (dotimes (i 4)
                   (setf (aref colors (+ (* 4 j) i))
                         (aref palettes (+ j palette-index) i))))
         (setf (aref colors 4) (aref palettes palette-index 0)
               (aref colors 8) (aref palettes palette-index 0)
               (aref colors 12) (aref palettes palette-index 0))))
      colors)))

(define-anim-seq-editor-frame-command (com-set-label :name t) ((label 'string))
  (setf (simple-animation-sequence-label
         (anim-seq-editor-sequence *anim-seq-editor-frame*))
        label)
  (clim:redisplay-frame-panes *anim-seq-editor-frame*))

(clim:define-presentation-to-command-translator click-to-set-label
    (simple-animation-sequence-label com-set-label anim-seq-editor-frame
     :gesture :select :menu nil
     :documentation "Change label")
    (object)
  (list))

(define-anim-seq-editor-frame-command (com-change-frame :name t) ((sequence-frame 'integer)
                                                                  (sprite-frame '(or null integer)))
  (if sprite-frame
      sprite-frame
      (let ((frame *anim-seq-editor-frame*)
            (seq (anim-seq-editor-sequence *anim-seq-editor-frame*)))
        (choose-tile-from-set :tileset  (simple-animation-sequence-tile-sheet seq)
                              :write-mode (ecase (simple-animation-sequence-major-kind seq)
                                            ((:background :scenery) :160a)
                                            (:npc (case (simple-animation-sequence-decal-kind
                                                         (anim-seq-editor-sequence
                                                          *anim-seq-editor-frame*))
                                                    ((:nefertem :phantom :enemy) :160a)
                                                    (otherwise :160b))))
                              :artp (ecase (simple-animation-sequence-major-kind seq)
                                      ((:background :scenery) nil)
                                      (:npc t))
                              :callback
                              (lambda (value)
                                (setf (aref (simple-animation-sequence-frames seq)
                                            sequence-frame)
                                      value)
                                (clim:redisplay-frame-panes frame))
                              :palette (anim-seq-editor-palette *anim-seq-editor-frame*))))
  (clim:redisplay-frame-panes *anim-seq-editor-frame*))

(clim:define-presentation-to-command-translator click-to-change-frame
    (simple-animation-sequence-frame-reference com-change-frame anim-seq-editor-frame
     :gesture :select :menu nil
     :documentation "Change the frame selected")
    (sequence-frame sprite-frame)
  (list sequence-frame sprite-frame))

(defun create-new-animation-sequence (&key major-kind decal-kind body)
  (let ((new-seq (make-instance 'simple-animation-sequence
                                :frames #(0 0 0 0 0 0 0 0)
                                :major-kind (or major-kind :npc)
                                :decal-kind (or decal-kind :human)
                                :decal-body (or body 0)
                                :bytes-width (if (and major-kind (not (eql major-kind :npc)))
                                                 2 4)
                                :write-mode (if (and major-kind (not (eql major-kind :npc)))
                                                :160a :160b)
                                :frame-count 1
                                :frame-rate-scalar 1
                                :tile-sheet (if (and major-kind (not (eql major-kind :npc)))
                                                "SandyIslandTiles"
                                                "NPCs")
                                :index (length *animation-sequences*))))
    (appendf *animation-sequences* (list new-seq))
    (save-all-animation-sequences)
    new-seq))

(define-anim-seq-editor-frame-command (com-create-new-sequence :name t :menu t) ()
  (save-all-animation-sequences)
  (let ((new-seq (create-new-animation-sequence)))
    (setf (anim-seq-editor-index *anim-seq-editor-frame*) (length *animation-sequences*)
          (anim-seq-editor-sequence *anim-seq-editor-frame*) new-seq))
  (update-params *anim-seq-editor-frame*)
  (clim:redisplay-frame-panes *anim-seq-editor-frame*))

(define-anim-seq-editor-frame-command (com-switch-to-sequence :name t :menu t) ((id 'integer))
  (save-all-animation-sequences)
  (setf (anim-seq-editor-index *anim-seq-editor-frame*) id
        
        (anim-seq-editor-sequence *anim-seq-editor-frame*)
        (find-animation-sequence id))
  (update-params *anim-seq-editor-frame*)
  (clim:redisplay-frame-panes *anim-seq-editor-frame*))

(clim:define-presentation-to-command-translator click-to-switch-sequence
    (simple-animation-sequence-index com-switch-sequence anim-seq-editor-frame
     :gesture :select :menu nil
     :documentation "Switch to another sequence")
    (object)
  (list))

(define-anim-seq-editor-frame-command (com-switch-frame-rate-scalar :name t) ()
  (setf (simple-animation-sequence-frame-rate-scalar
         (anim-seq-editor-sequence *anim-seq-editor-frame*))
        (ecase (simple-animation-sequence-frame-rate-scalar
                (anim-seq-editor-sequence *anim-seq-editor-frame*))
          (1 1/2)
          (1/2 1/4)
          (1/4 1/8)
          (1/8 1)))
  (update-params *anim-seq-editor-frame*))

(clim:define-presentation-to-command-translator click-to-rotate-speed-scalar
    (simple-animation-sequence-frame-rate-scalar com-switch-frame-rate-scalar
     anim-seq-editor-frame
     :gesture :select :menu nil
     :documentation "Change frame rate scalar")
    (object)
  (list))

(define-anim-seq-editor-frame-command (com-switch-frame-count :name t) ()
  (setf (simple-animation-sequence-frame-count
         (anim-seq-editor-sequence *anim-seq-editor-frame*))
        (ecase (simple-animation-sequence-frame-count
                (anim-seq-editor-sequence *anim-seq-editor-frame*))
          (1 2)
          (2 4)
          (4 8)
          (8 1)))
  (update-params *anim-seq-editor-frame*))

(clim:define-presentation-to-command-translator click-to-rotate-frame-count
    (simple-animation-sequence-frame-count com-switch-frame-count
     anim-seq-editor-frame
     :gesture :select :menu nil
     :documentation "Change number of frames in the sequence")
    (object)
  (list))

(define-anim-seq-editor-frame-command (com-set-decal-body :name t) ((body 'integer))
  (setf (simple-animation-sequence-decal-body
         (anim-seq-editor-sequence *anim-seq-editor-frame*))
        (mod body
             (ecase (simple-animation-sequence-decal-kind
                     (anim-seq-editor-sequence *anim-seq-editor-frame*))
               (:human 2)
               (:sailor 8)
               (:enemy #x100)
               (:block1 8)
               (:block2 8)
               (:block3 8)
               (:block4 8))))
  (clim:redisplay-frame-panes *anim-seq-editor-frame*))

(clim:define-presentation-to-command-translator click-to-set-body
    (simple-animation-sequence-decal-body com-set-decal-body anim-seq-editor-frame
     :gesture :select :menu nil
     :documentation "Change body number")
    (object)
  (list))

(define-anim-seq-editor-frame-command (com-next-palette :name t) ()
  (setf (anim-seq-editor-palette *anim-seq-editor-frame*)
        (ecase (simple-animation-sequence-write-mode 
                (anim-seq-editor-sequence *anim-seq-editor-frame*))
          (:160a (mod (1+ (anim-seq-editor-palette *anim-seq-editor-frame*)) 8))
          (:160b (if (zerop (anim-seq-editor-palette *anim-seq-editor-frame*))
                     4 0))))
  (clim:redisplay-frame-panes *anim-seq-editor-frame*))

(clim:define-presentation-to-command-translator click-for-next-palette
    (palette-selector com-next-palette anim-seq-editor-frame
     :gesture :select :menu nil
     :documentation "Switch to next palette")
    (frame)
  ())

(define-anim-seq-editor-frame-command (com-switch-palette :name t) ((palette 'integer))
  (setf (anim-seq-editor-palette *anim-seq-editor-frame*)
        (ecase (simple-animation-sequence-write-mode 
                (anim-seq-editor-sequence *anim-seq-editor-frame*))
          (:160a (mod palette 8))
          (:160b (if (zerop palette) 0 4))))
  (clim:redisplay-frame-panes *anim-seq-editor-frame*))

(define-anim-seq-editor-frame-command (com-switch-tileset :name t)
    ((name 'simple-animation-sequence-tile-sheet-name))
  (cond
    ((anim-seq-editing-background-p)
     (assert (member name (all-tilesets) :test #'string=) (name)
             "Name must be a valid background tileset; got ~s but wanted one of:~{~& • ~s~}"
             name (all-tilesets)))
    ((anim-seq-editing-scenery-p)
     (assert (member name (all-scenery-decals) :test #'string=) (name)
             "Name must be a valid decals tileset; got ~s but wanted one of:~{~& • ~s~}"
             name (all-scenery-decals)))
    (t (let ((all-npc-sheets (all-npc-art-sheets-for-kind
                              (simple-animation-sequence-decal-kind
                               (anim-seq-editor-sequence *anim-seq-editor-frame*)))))
         (when (and (= 1 (length all-npc-sheets))
                    (not (string-equal name (first all-npc-sheets))))
           (setf name (first all-npc-sheets)))
         (assert (member name all-npc-sheets :test #'string=)
                 (name)
                 "Name must be a valid tileset for ~a NPCs; got ~s but wanted one of:~{~& • ~s~}"
                 (simple-animation-sequence-decal-kind
                  (anim-seq-editor-sequence *anim-seq-editor-frame*))
                 name (all-npc-art-sheets-for-kind
                       (simple-animation-sequence-decal-kind
                        (anim-seq-editor-sequence *anim-seq-editor-frame*)))))))
  
  (setf (simple-animation-sequence-tile-sheet
         (anim-seq-editor-sequence *anim-seq-editor-frame*))
        name)
  (clim:redisplay-frame-panes *anim-seq-editor-frame*))

(defun anim-seq-editing-background-p (&rest _)
  (declare (ignore _))
  (eql :background (simple-animation-sequence-tile-sheet
                    (anim-seq-editor-sequence *anim-seq-editor-frame*))))

(defun anim-seq-editing-scenery-p (&rest _)
  (declare (ignore _))
  (eql :scenery (simple-animation-sequence-tile-sheet
                 (anim-seq-editor-sequence *anim-seq-editor-frame*))))

(dolist (tileset (all-tilesets))
  (eval `(clim:define-presentation-to-command-translator
             ,(format-symbol :skyline-tool "switch-to-tileset-~a"
                             (string-upcase (param-case tileset)))
             (simple-animation-sequence-tile-sheet-name com-switch-tileset anim-seq-editor-frame
              :menu t :tester anim-seq-editing-background-p
              :documentation ,(format nil "Switch to tile sheet ~a" tileset))
             (frame)
           (list ,tileset))))

(dolist (tileset (all-scenery-decals))
  (eval `(clim:define-presentation-to-command-translator
             ,(format-symbol :skyline-tool "switch-to-tileset-~a"
                             (string-upcase (param-case tileset)))
             (simple-animation-sequence-tile-sheet-name com-switch-tileset anim-seq-editor-frame
              :menu t :tester anim-seq-editing-scenery-p
              :documentation ,(format nil "Switch to tile sheet ~a" tileset))
             (frame)
           (list ,tileset))))

(define-anim-seq-editor-frame-command (com-select-decal-kind :name t)
    ((name 'simple-animation-sequence-decal-kind))
  (assert (member name +decal-kinds+) (name)
          "Name must be a valid decal-kind; got ~s but wanted one of:~{~& • ~s~}"
          name +decal-kinds+)
  (setf (simple-animation-sequence-decal-kind
         (anim-seq-editor-sequence *anim-seq-editor-frame*))
        name)
  (update-params *anim-seq-editor-frame*))

(dolist (kind +decal-kinds+)
  (eval `(clim:define-presentation-to-command-translator
             ,(format-symbol :skyline-tool "switch-to-decal-kind-~a" kind)
             (simple-animation-sequence-decal-kind  com-select-decal-kind anim-seq-editor-frame
              :menu t
              :documentation ,(format nil "Switch to ~a decal kind" kind))
             (frame)
           (list ,kind))))

(clim:define-presentation-to-command-translator select-specific-palette
    (palette-selector com-switch-palette anim-seq-editor-frame
     :gesture :edit :menu nil
     :documentation "Switch to a specific")
    (frame)
  ())

(defun prefix-match-length (a b)
  (loop for i below (min (length a) (length b))
        with count = 0
        if (char= (char a i) (char b i))
          do (incf count)
        else
          do (return count)
        finally (return count)))

(define-anim-seq-editor-frame-command (com-switch-major-kind :name t) ()
  (let ((new-kind (ecase (simple-animation-sequence-major-kind
                          (anim-seq-editor-sequence *anim-seq-editor-frame*))
                    (:background :scenery)
                    (:scenery :npc)
                    (:npc :background))))
    (setf (simple-animation-sequence-major-kind
           (anim-seq-editor-sequence *anim-seq-editor-frame*))
          new-kind)
    (update-params *anim-seq-editor-frame*)))

(define-anim-seq-editor-frame-command (com-select-major-kind :name t) ((kind 'symbol))
  (setf (simple-animation-sequence-major-kind
         (anim-seq-editor-sequence *anim-seq-editor-frame*))
        kind)
  (update-params *anim-seq-editor-frame*))

(dolist (major-kind (list :background :scenery :npc))
  (eval `(clim:define-presentation-to-command-translator
             ,(format-symbol :skyline-tool "switch-to-major-kind-~a" major-kind)
             (simple-animation-sequence-major-kind com-select-major-kind anim-seq-editor-frame
              :menu t
              :documentation ,(format nil "Switch to ~a major kind" major-kind))
             (object)
           (list ,major-kind))))

(define-anim-seq-editor-frame-command (com-switch-decal-kind :name t) ()
  (when (eql :npc (simple-animation-sequence-major-kind
                   (anim-seq-editor-sequence *anim-seq-editor-frame*)))
    (setf (simple-animation-sequence-decal-kind
           (anim-seq-editor-sequence *anim-seq-editor-frame*))
          (elt +decal-kinds+
               (mod (1+ (or (position (simple-animation-sequence-decal-kind
                                       (anim-seq-editor-sequence *anim-seq-editor-frame*))
                                      +decal-kinds+)
                            -1))
                    (length +decal-kinds+)))))
  (update-params *anim-seq-editor-frame*))

(defmethod display-anim-seq-filmstrip ((window anim-seq-editor-frame) pane)
  (let ((seq (anim-seq-editor-sequence window)))
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (dotimes (n (simple-animation-sequence-frame-count seq))
          (let ((frame (or (elt (simple-animation-sequence-frames seq) n) 0)))
            (clim:formatting-cell (pane)
              (clim:with-output-as-presentation (pane n 'simple-animation-sequence-frame-reference)
                (display-maria-art pane
                                   :dump (load-tile-sheet-object-by-name
                                          (simple-animation-sequence-tile-sheet seq)
                                          (ecase
                                              (simple-animation-sequence-major-kind seq)
                                            ((:background :scenery) nil)
                                            (:npc t)))
                                   :mode (simple-animation-sequence-write-mode seq)
                                   :address (* (simple-animation-sequence-bytes-width seq)
                                               frame)
                                   :colors (read-palette-for-tile-sheet
                                            (simple-animation-sequence-tile-sheet seq)
                                            (anim-seq-editor-palette window)
                                            :write-mode
                                            (simple-animation-sequence-write-mode seq))
                                   :var-colors #(#x48 #x88 #xc8)
                                   :width (simple-animation-sequence-bytes-width seq))
                (terpri pane)
                (format pane "~10tFrame ~d: $~2,'0x" (1+ n) frame)))))))
    (let ((pal (anim-seq-editor-palette window)))
      (clim:with-output-as-presentation (pane pal 'palette-selector)
        (format pane "~2%Palette: ~d" pal)))))

(defmethod display-anim-seq-properties ((frame anim-seq-editor-frame) pane)
  (block nil
    (let ((seq (anim-seq-editor-sequence frame)))
      (if seq
          (progn (format pane "~10t")
                 (clim:formatting-table (pane)
                   (clim:formatting-row (pane)
                     (clim:formatting-cell (pane)
                       (clim:with-text-face (pane :bold)
                         (clim:with-text-size (pane :large)
                           (clim:with-output-as-presentation (pane (anim-seq-editor-index frame)
                                                                   'simple-animation-sequence-index)
                             (format pane "Sequence:")))))
                     (clim:formatting-cell (pane)
                       (clim:with-output-as-presentation (pane (anim-seq-editor-index frame)
                                                               'simple-animation-sequence-index)
                         (clim:with-text-size (pane :large)
                           (format pane "~d" (anim-seq-editor-index frame))))))
                   (terpri pane)
                   (clim:formatting-row (pane)
                     (clim:formatting-cell (pane)
                       (clim:with-text-face (pane :bold)
                         (format pane "Label:")))
                     (clim:formatting-cell (pane)
                       (clim:with-output-as-presentation (pane (simple-animation-sequence-label seq)
                                                               'simple-animation-sequence-label)
                         (let ((label (simple-animation-sequence-label seq)))
                           (format pane "~a" (if (emptyp label)
                                                 " — "
                                                 label))))))
                   (terpri pane)
                   (clim:formatting-row (pane)
                     (clim:formatting-cell (pane)
                       (clim:with-text-face (pane :bold)
                         (format pane "Major Kind:")))
                     (clim:formatting-cell (pane)
                       (clim:with-output-as-presentation
                           (pane (simple-animation-sequence-major-kind seq)
                                 'simple-animation-sequence-major-kind)
                         (let ((kind (simple-animation-sequence-major-kind seq)))
                           (format pane (ecase kind
                                          (:npc "Non-Player Character")
                                          (:background "Background (map tiles)")
                                          (:scenery "Scenery decals")))))))
                   (when (eql :npc (simple-animation-sequence-major-kind seq))
                     (terpri pane)
                     (clim:formatting-row (pane)
                       (clim:formatting-cell (pane)
                         (clim:with-text-face (pane :bold)
                           (format pane "Decal Kind:")))
                       (clim:formatting-cell (pane)
                         (clim:with-output-as-presentation
                             (pane (simple-animation-sequence-decal-kind seq)
                                   'simple-animation-sequence-decal-kind)
                           (format pane "~a"
                                   (decal-kind-name
                                    (simple-animation-sequence-decal-kind seq)))))))
                   (when (and (eql :npc (simple-animation-sequence-major-kind seq))
                              (member (simple-animation-sequence-decal-kind seq)
                                      '(:human :enemy :sailor :block1 :block2 :block3 :block4)))
                     (terpri pane)
                     (clim:formatting-row (pane)
                       (clim:formatting-cell (pane)
                         (clim:with-text-face (pane :bold)
                           (format pane "Body Number:")))
                       (clim:formatting-cell (pane)
                         (clim:with-output-as-presentation (pane (simple-animation-sequence-decal-body seq)
                                                                 'simple-animation-sequence-decal-body)
                           (format pane "~d"
                                   (simple-animation-sequence-decal-body seq))))))
                   (terpri pane)
                   (clim:formatting-row (pane)
                     (clim:formatting-cell (pane)
                       (clim:with-text-face (pane :bold)
                         (format pane "Tile Sheet:")))
                     (clim:formatting-cell (pane)
                       (clim:with-output-as-presentation (pane (simple-animation-sequence-tile-sheet seq)
                                                               'simple-animation-sequence-tile-sheet-name)
                         (format pane "~a"
                                 (simple-animation-sequence-tile-sheet seq)))))
                   (terpri pane)
                   (clim:formatting-row (pane)
                     (clim:formatting-cell (pane)
                       (clim:with-text-face (pane :bold)
                         (format pane "Frames:")))
                     (clim:formatting-cell (pane)
                       (clim:with-output-as-presentation (pane (simple-animation-sequence-frame-count seq)
                                                               'simple-animation-sequence-frame-count)
                         (format pane "~r frame~:p"
                                 (simple-animation-sequence-frame-count seq)))))
                   (terpri pane)
                   (clim:formatting-row (pane)
                     (clim:formatting-cell (pane)
                       (clim:with-text-face (pane :bold)
                         (format pane "Speed Scalar:")))
                     (clim:formatting-cell (pane)
                       (clim:with-output-as-presentation (pane (simple-animation-sequence-frame-rate-scalar seq)
                                                               'simple-animation-sequence-frame-rate-scalar)
                         (format pane "~a"
                                 (rationalize (simple-animation-sequence-frame-rate-scalar seq))))))
                   (terpri pane)
                   (clim:formatting-row (pane)
                     (clim:formatting-cell (pane)
                       (clim:with-text-face (pane :bold)
                         (format pane "Frame Rate:")))
                     (clim:formatting-cell (pane)
                       (format pane "~{~d ~a~} FPS (frames per second)"
                               (substitute
                                #\Space 0
                                (multiple-value-list
                                 (floor (* 10 (simple-animation-sequence-frame-rate-scalar seq))))))))
                   (terpri pane)
                   (clim:formatting-row (pane)
                     (clim:formatting-cell (pane)
                       (clim:with-text-face (pane :bold)
                         (format pane "Duration:")))
                     (clim:formatting-cell (pane)
                       (format pane "~{~d ~a~} sec (~0,2fs)"
                               (substitute
                                #\Space 0
                                (multiple-value-list
                                 (floor (/ (simple-animation-sequence-frame-count seq)
                                           (* 10
                                              (simple-animation-sequence-frame-rate-scalar seq))))))
                               (/ (simple-animation-sequence-frame-count seq)
                                  (* 10.0
                                     (simple-animation-sequence-frame-rate-scalar seq))))))
                   (terpri pane)
                   (clim:formatting-row (pane)
                     (clim:formatting-cell (pane)
                       (clim:with-text-face (pane :bold)
                         (format pane "Write Mode:")))
                     (clim:formatting-cell (pane)
                       (format pane "~a"
                               (simple-animation-sequence-write-mode seq))))
                   (terpri pane)
                   (clim:formatting-row (pane)
                     (clim:formatting-cell (pane)
                       (clim:with-text-face (pane :bold)
                         (format pane "Bytes Width:")))
                     (clim:formatting-cell (pane)
                       (format pane "~d byte~:p"
                               (simple-animation-sequence-bytes-width seq))))
                   (force-output pane)))
          
          (clim:with-output-as-presentation (pane nil 'simple-animation-sequence-index)
            (format pane "~&No sequence selected"))))))

(defun parse-animation-sequence-row-from-ss (row)
  (flet ((elt* (row n)
           (if (< n (length row))
               (elt row n)
               nil)))
    (unless (emptyp (elt* row 0))
      (make-instance 'simple-animation-sequence
                     :index (parse-integer (elt* row 0))
                     :major-kind (find-symbol (elt* row 1) :keyword)
                     :decal-kind (find-symbol (elt* row 2) :keyword)
                     :decal-body (if (emptyp (elt* row 3))
                                     0
                                     (parse-integer (elt* row 3)))
                     :tile-sheet (elt* row 4)
                     :frame-count (expt 2 (if (emptyp (elt* row 5))
                                              0
                                              (parse-number (elt* row 5))))
                     :frame-rate-scalar
                     (/ 1 (expt 2 (if (emptyp (elt* row 6))
                                      0
                                      (parse-number (elt* row 6)))))
                     :frames (coerce (loop for i from 7 below 15
                                           collect
                                           (If (emptyp (elt* row i))
                                               0
                                               (parse-integer (elt* row i) :radix 16)))
                                     'vector)
                     :label (elt* row 15)))))

(defun load-all-animation-sequences ()
  (let ((spreadsheet
          (read-ods-into-lists
           (make-pathname :directory (list :relative "Source" "Tables")
                          :name "Animation" :type "ods"))))
    (flet ((elt* (row n)
             (if (< n (length row))
                 (elt row n)
                 nil)))
      (setf *animation-sequences*
            (remove-if
             #'null
             (mapcar
              (lambda (row)
                (parse-animation-sequence-row-from-ss row))
              (second spreadsheet)))
            
            *animation-assignments*
            (let ((hash (make-hash-table :test 'equalp)))
              (mapcar
               (lambda (row)
                 (let ((decal-kind (find-symbol (elt* row 0) :keyword))
                       (body (unless (emptyp (elt* row 1))
                               (parse-integer (elt* row 1))))
                       (action (find-symbol (elt* row 2) :keyword))
                       (facing (find-symbol (elt* row 3) :keyword))
                       (index (unless (emptyp (elt* row 4))
                                (parse-integer (elt* row 4)))))
                   (if (and decal-kind body action facing index)
                       (setf (gethash (list decal-kind body action facing) hash)
                             (elt* *animation-sequences* index)))))
               (third spreadsheet))
              hash)))))

(defun save-all-animation-sequences ()
  (let ((text-props-normal '(:color "#000000" :font-name "Cantarell" :font-size "10.5pt"))
        (text-props-title '(:color "#000000" :font-name "Cantarell"
                            :font-size "14pt" :font-weight :bold))
        (text-props-link '(:color "#0563c1" :font-name "Cantarell" :font-size "10.5pt"
		       :text-underline-style :solid :text-underline-type :single)))
    (clods:with-spreadsheet ((make-pathname :name "Animation" :type "ods"
                                            :directory (list :relative "Source" "Tables"))
                             :generator "Skyline-Tool" :creator (user-real-name))
      (clods:using-fonts ()
        ;; fonts
        (clods:font "Cantarell" :family "Cantarell" :family-generic :swiss))
      (clods:using-styles (:locale (clods:make-locale "US" #\space 3 #\.))
        ;; number formats
        (clods:number-text-style "n-text")
        ;; cell styles
        (clods:cell-style "ce-normal" nil text-props-normal :data-style "n-text")
        (clods:cell-style "ce-title" "ce-normal" text-props-title :horizontal-align :center)
        (clods:cell-style "ce-header" "ce-normal" text-props-title
                          :border-bottom '(:thin :solid "#000000"))
        (clods:cell-style "ce-link" "ce-normal" text-props-link)
        
        ;; column styles
        (clods:column-style "co-column" nil :width "5.0cm")
        
        ;; row styles
        (clods:row-style "ro-normal" nil :height "10.5pt" :use-optimal-height t)
        (clods:row-style "ro-title" nil :height "14pt" :use-optimal-height t))
      
      (clods:with-body ()
        (clods:with-table ("Caution")
          (clods:with-header-columns ()
            (clods:column :style "co-column"))
	;; then, add the data row-by-row, starting with headers
	(clods:with-header-rows ()
	  (clods:with-row (:style "ro-title")
              (clods:cell "Caution" :style "ce-header")))
          (clods:with-row (:style "ro-normal")
            (clods:cell "This file is read, and potentially overwritten, by Skyline Tool's Animation Editor.")))
        (clods:with-table ("Animation Sequences")
          (clods:with-header-columns ()
            (loop repeat 20 do (clods:column :style "co-column")))
	;; then, add the data row-by-row, starting with headers
	(clods:with-header-rows ()
	  (clods:with-row (:style "ro-title")
              (clods:cell "Sequence ID" :style "ce-header")
	    (clods:cell "Major Kind" :style "ce-header")
              (clods:cell "Decal Kind" :style "ce-header")
              (clods:cell "Decal Body" :style "ce-header")
              (clods:cell "Tile Sheet" :style "ce-header")
              (clods:cell "Frames Exp" :style "ce-header")
              (clods:cell "Delay Exp" :style "ce-header")
              (dotimes (i 8)
                (clods:cell (format nil "Frame ~d" i) :style "ce-title"))
              (clods:cell "Label" :style "ce-title")))
          (dolist (seq *animation-sequences*)
	  (clods:with-row (:style "ro-normal")
              (clods:cell (princ-to-string
                           (simple-animation-sequence-index seq)))
              (clods:cell (princ-to-string
                           (simple-animation-sequence-major-kind seq)))
              (clods:cell (princ-to-string
                           (simple-animation-sequence-decal-kind seq)))
              (clods:cell (princ-to-string
                           (simple-animation-sequence-decal-body seq)))
              (clods:cell (princ-to-string
                           (simple-animation-sequence-tile-sheet seq)))
              (clods:cell (princ-to-string
                           (floor (log (simple-animation-sequence-frame-count seq) 2))))
              (clods:cell (princ-to-string
                           (floor (log (/ 1
                                          (simple-animation-sequence-frame-rate-scalar seq))
                                       2))))
              (dotimes (frame 8)
                (clods:cell (format nil "~2,'0x" 
                                    (aref
                                     (simple-animation-sequence-frames seq)
                                     frame))))
              (clods:cell (simple-animation-sequence-label seq)))))
        (clods:with-table ("NPC Animation Assignments")
	;; then, add the data row-by-row, starting with headers
	(clods:with-header-columns ()
            (loop repeat 19 do (clods:column :style "co-column")))
	;; then, add the data row-by-row, starting with headers
	(clods:with-header-rows ()
	  (clods:with-row (:style "ro-title")
              (clods:cell "Decal Kind" :style "ce-title")
              (clods:cell "Body" :style "ce-title")
              (clods:cell "Action" :style "ce-title")
              (clods:cell "Facing" :style "ce-title")
              (clods:cell "Sequence ID" :style "ce-title")))
          (dolist (key (hash-table-keys *animation-assignments*))
            (destructuring-bind (kind body action facing) key
              (clods:with-row (:style "ro-normal")
                (clods:cell (princ-to-string kind))
                (clods:cell (princ-to-string body))
                (clods:cell (princ-to-string action))
                (clods:cell (princ-to-string facing))
                (clods:cell
                  (when-let ((seq (gethash key *animation-assignments*)))
                    (princ-to-string
                     (simple-animation-sequence-index seq)))))))))))
  (load-all-animation-sequences))

(defun save-animation-sequence (sequence)
  (let ((index (simple-animation-sequence-index sequence)))
    (if (< (length *animation-sequences*) index)
        (if (= (length *animation-sequences*) index)
            (appendf *animation-sequences* (list sequence))
            (error "Got confused how to save index ~d when there are only ~d sequence~:p"
                   index (length *animation-sequences*)))
        (setf (elt *animation-sequences* index) sequence)))
  (save-all-animation-sequences))

(define-anim-seq-editor-frame-command (com-save-sequence :name t :menu t) ()
  (save-animation-sequence (anim-seq-editor-sequence *anim-seq-editor-frame*)))

(defun edit-animation-sequence (&optional (sequence 0))
  "Select which frames go together to form an animation sequence"
  (clim-sys:make-process (lambda ()
                 (load-all-animation-sequences)
                 (let ((*anim-seq-editor-frame*
                         (clim:make-application-frame 'anim-seq-editor-frame
                                                      :sequence sequence)))
                   (setf (clim:frame-pretty-name *anim-seq-editor-frame*)
                         "Edit Animation Sequence")
                   (clim:run-frame-top-level *anim-seq-editor-frame*)))
               :name "Edit Animation Sequence"))

(define-constant +all-actions+
    '(:idle :climbing :dead :flying
      :knocked-back :swimming :use-equipment :wading
      :walking :wave-arms :gesture :sleep :non-interactive
      :dance :panic :special-walk-with-shield :special-idle-with-shield)
  :test 'equalp)

(clim:define-application-frame anim-seq-assign-frame ()
  ((%seq-index :initform 0 :accessor anim-seq-assign-index :initarg :sequence)
   (%sequence :accessor anim-seq-assign-sequence)
   (%decal-kind :accessor anim-seq-assign-decal-kind :initarg :decal-kind)
   (%body :accessor anim-seq-assign-body :initarg :body)
   (%parent :accessor anim-seq-assign-parent :initarg :parent)
   (%action-id :type #.(cons 'member +all-actions+)
               :initform :idle :accessor anim-seq-assign-action :initarg :action)
   (%facing-id :type (member :all :north :south :east :west)
               :initform :south :accessor anim-seq-assign-facing :initarg :facing))
  (:panes (anim-seq-detail-pane :application :height 400 :width 400
                                             :display-function 'display-anim-seq-assignment)
          (interactor :interactor :height 150 :width 400))
  (:layouts (default (clim:vertically ()
                         anim-seq-detail-pane
                       interactor))))

(defmethod display-anim-seq-assignment ((frame anim-seq-assign-frame) pane)
  (block nil
    (clim:with-text-size (pane :large)
      (format pane "Decal Kind: "))
    (clim:with-output-as-presentation (pane (anim-seq-assign-decal-kind frame)
                                            'simple-animation-sequence-decal-kind)
      (clim:with-text-size (pane :large)
        (format pane "~a" (decal-kind-name
                           (anim-seq-assign-decal-kind frame)))))
    (terpri pane)
    (clim:with-text-size (pane :large)
      (format pane "Body: "))
    (clim:with-output-as-presentation (pane (anim-seq-assign-body frame)
                                            'simple-animation-sequence-decal-body)
      (clim:with-text-size (pane :large)
        (format pane "~d" (anim-seq-assign-body frame))))
    (terpri pane)
    (clim:with-text-size (pane :large)
      (format pane "Action: "))
    (clim:with-output-as-presentation (pane (anim-seq-assign-action frame)
                                            'anim-seq-action)
      (clim:with-text-size (pane :large)
        (format pane "~a" (title-case
                           (string (anim-seq-assign-action frame))))))
    (terpri pane)
    (clim:with-text-size (pane :large)
      (format pane "Facing: "))
    (clim:with-output-as-presentation (pane (anim-seq-assign-facing frame)
                                            'anim-seq-facing)
      (clim:with-text-size (pane :large)
        (format pane "~a" (title-case
                           (string (anim-seq-assign-facing frame))))))
    (when (reduce #'eql (list (find-assigned-animation-sequence
                               (anim-seq-assign-decal-kind frame)
                               (anim-seq-assign-body frame)
                               (anim-seq-assign-action frame)
                               :north)
                              (find-assigned-animation-sequence
                               (anim-seq-assign-decal-kind frame)
                               (anim-seq-assign-body frame)
                               (anim-seq-assign-action frame)
                               :south)
                              (find-assigned-animation-sequence
                               (anim-seq-assign-decal-kind frame)
                               (anim-seq-assign-body frame)
                               (anim-seq-assign-action frame)
                               :east)
                              (find-assigned-animation-sequence
                               (anim-seq-assign-decal-kind frame)
                               (anim-seq-assign-body frame)
                               (anim-seq-assign-action frame)
                               :west)))
      (format pane "~&~20tAll four facing directions match."))
    (clim:with-output-as-presentation (pane (anim-seq-assign-index frame) 'magic-four-ways-button)
      (clim:with-text-size (pane :small)
        (format pane "~&~20t(assign to all four directions)")))
    (terpri pane)
    (terpri pane)
    (let ((seq (anim-seq-assign-sequence frame)))
      (unless seq
        (clim:with-output-as-presentation (pane nil 'simple-animation-sequence-index)
          (clim:with-text-size (pane :large)
            (format pane "~&No sequence selected")))
        (return))
      (format pane "~10t")
      (clim:formatting-table (pane)
        (clim:formatting-row (pane)
          (clim:formatting-cell (pane)
            (clim:with-text-face (pane :bold)
              (clim:with-text-size (pane :large)
                (clim:with-output-as-presentation (pane (anim-seq-assign-index frame)
                                                        'simple-animation-sequence-index)
                  (format pane "Sequence:")))))
          (clim:formatting-cell (pane)
            (clim:with-output-as-presentation (pane (anim-seq-assign-index frame)
                                                    'simple-animation-sequence-index)
              (clim:with-text-size (pane :large)
                (format pane "~d" (anim-seq-assign-index frame))))))
        (terpri pane)
        (clim:formatting-row (pane)
          (clim:formatting-cell (pane)
            (clim:with-text-face (pane :bold)
              (format pane "Label:")))
          (clim:formatting-cell (pane)
            (clim:with-output-as-presentation (pane (simple-animation-sequence-label seq)
                                                    'simple-animation-sequence-label)
              (if-let ((label (simple-animation-sequence-label seq)))
                (format pane "~a" label)
                (format pane "(no label)")))))
        (terpri pane)
        (clim:formatting-row (pane)
          (clim:formatting-cell (pane)
            (clim:with-text-face (pane :bold)
              (format pane "Major Kind:")))
          (clim:formatting-cell (pane)
            (clim:with-output-as-presentation (pane (simple-animation-sequence-major-kind seq)
                                                    'simple-animation-sequence-major-kind)
              (let ((kind (simple-animation-sequence-major-kind seq)))
                (format pane (ecase kind
                               (:npc "Non-Player Character")
                               (:background "Background (map tiles)")
                               (:scenery "Scenery decals")))))))
        (when (eql :npc (simple-animation-sequence-major-kind seq))
          (terpri pane)
          (clim:formatting-row (pane)
            (clim:formatting-cell (pane)
              (clim:with-text-face (pane :bold)
                (format pane "Decal Kind:")))
            (clim:formatting-cell (pane)
              (clim:with-output-as-presentation (pane (simple-animation-sequence-decal-kind seq)
                                                      'simple-animation-sequence-decal-kind)
                (format pane "~a"
                        (decal-kind-name (simple-animation-sequence-decal-kind seq)))))))
        (when (and (eql :npc (simple-animation-sequence-major-kind seq))
                   (member (simple-animation-sequence-decal-kind seq)
                           '(:human :enemy :sailor :block1 :block2 :block3 :block4)))
          (terpri pane)
          (clim:formatting-row (pane)
            (clim:formatting-cell (pane)
              (clim:with-text-face (pane :bold)
                (format pane "Body Number:")))
            (clim:formatting-cell (pane)
              (clim:with-output-as-presentation (pane (simple-animation-sequence-decal-body seq)
                                                      'simple-animation-sequence-decal-body)
                (format pane "~d"
                        (simple-animation-sequence-decal-body seq))))))
        (terpri pane)
        (clim:formatting-row (pane)
          (clim:formatting-cell (pane)
            (clim:with-text-face (pane :bold)
              (format pane "Tile Sheet:")))
          (clim:formatting-cell (pane)
            (clim:with-output-as-presentation (pane (simple-animation-sequence-tile-sheet seq)
                                                    'simple-animation-sequence-tile-sheet-name)
              (format pane "~a"
                      (simple-animation-sequence-tile-sheet seq)))))
        (terpri pane)
        (clim:formatting-row (pane)
          (clim:formatting-cell (pane)
            (clim:with-text-face (pane :bold)
              (format pane "Frames:")))
          (clim:formatting-cell (pane)
            (clim:with-output-as-presentation (pane (simple-animation-sequence-frame-count seq)
                                                    'simple-animation-sequence-frame-count)
              (format pane "~d frame~:p"
                      (simple-animation-sequence-frame-count seq)))))
        (terpri pane)
        (clim:formatting-row (pane)
          (clim:formatting-cell (pane)
            (clim:with-text-face (pane :bold)
              (format pane "Speed Scalar:")))
          (clim:formatting-cell (pane)
            (clim:with-output-as-presentation (pane (simple-animation-sequence-frame-rate-scalar seq)
                                                    'simple-animation-sequence-frame-rate-scalar)
              (format pane "~a"
                      (rationalize (simple-animation-sequence-frame-rate-scalar seq))))))
        (terpri pane)
        (clim:formatting-row (pane)
          (clim:formatting-cell (pane)
            (clim:with-text-face (pane :bold)
              (format pane "Frame Rate:")))
          (clim:formatting-cell (pane)
            (format pane "~{~d ~a~} FPS (frames per second)"
                    (substitute
                     #\Space 0
                     (multiple-value-list
                      (floor (* 10 (simple-animation-sequence-frame-rate-scalar seq))))))))
        (terpri pane)
        (clim:formatting-row (pane)
          (clim:formatting-cell (pane)
            (clim:with-text-face (pane :bold)
              (format pane "Duration:")))
          (clim:formatting-cell (pane)
            (format pane "~{~d ~a~} sec (~0,2fs)"
                    (substitute
                     #\Space 0
                     (multiple-value-list
                      (floor (/ (simple-animation-sequence-frame-count seq)
                                (* 10
                                   (simple-animation-sequence-frame-rate-scalar seq))))))
                    (/ (simple-animation-sequence-frame-count seq)
                       (* 10.0
                          (simple-animation-sequence-frame-rate-scalar seq))))))
        (terpri pane)
        (clim:with-output-as-presentation (pane (simple-animation-sequence-index seq)
                                                'anim-seq-assign-sequence)
          (format pane "~%~20t(Click here to edit sequence)"))
        (terpri pane)
        (force-output pane)))))

(defmethod initialize-instance :after ((frame anim-seq-assign-frame) &key sequence &allow-other-keys)
  (setf (anim-seq-assign-sequence frame) (find-animation-sequence sequence)))

(defun assign-animation-sequence (decal-kind body action facing)
  "Select which animation sequence applies to which action(s)"
  (let ((parent *anim-seq-assigns-frame*))
    (clim-sys:make-process (lambda ()
                   (let ((*anim-seq-assign-frame*
                           (clim:make-application-frame
                            'anim-seq-assign-frame
                            :sequence (when-let (seq (gethash (list decal-kind body action facing)
                                                              *animation-assignments*))
                                        (simple-animation-sequence-index seq))
                            :decal-kind decal-kind
                            :body body
                            :action action
                            :facing facing
                            :parent parent)))
                     (setf (clim:frame-pretty-name *anim-seq-assign-frame*)
                           "Assign Animation Sequence")
                     (clim:run-frame-top-level *anim-seq-assign-frame*)))
                 :name "Assign Animation Sequence")))

(clim:define-application-frame anim-seq-assigns-frame ()
  ()
  (:panes (anim-seq-assignments-pane :application :height 600 :width 1600
                                                  :display-function 'display-anim-seq-assignments)
          (interactor :interactor :height 200 :width 400))
  (:layouts (default (clim:vertically ()
                       anim-seq-assignments-pane
                       interactor))))

(defun body-count-for-decal-kind (kind)
  (case kind
    (:human 2)
    (:sailor 8)
    (:enemy 8); TODO count the number from somewhere?
    (:block1 8)
    (:block2 8)
    (:block3 8)
    (:block4 8)
    (otherwise 1)))

(defvar *animation-assignments* (make-hash-table :test 'equalp))

(defun find-assigned-animation-sequence (decal-kind body action facing)
  (gethash (list decal-kind body action facing) *animation-assignments* nil))

(defun set-animation-sequence-assignment (decal-kind body action facing object)
  (setf (gethash (list decal-kind body action facing) *animation-assignments* nil)
        object)
  (save-all-animation-sequences))

(defun animation-sequence-compatible-p (sequence decal-kind body)
  (and (eql decal-kind (simple-animation-sequence-decal-kind sequence))
       (eql body (simple-animation-sequence-decal-body sequence))))

(defun find-next-compatible-sequence (decal-kind body start-from)
  (or (loop for i from start-from below (length *animation-sequences*)
            when (animation-sequence-compatible-p (elt *animation-sequences* i)
                                                  decal-kind body)
              do (return (elt *animation-sequences* i)))
      (loop for i from 0 below start-from
            when (animation-sequence-compatible-p (elt *animation-sequences* i)
                                                  decal-kind body)
              do (return (elt *animation-sequences* i)))))

(defmethod display-anim-seq-assignments ((frame anim-seq-assigns-frame) pane)
  (block nil
    (clim:with-text-size (pane :large)
      (format pane "Assign Animation Sequences to NPCs"))
    (terpri pane)
    (terpri pane)
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane)
          (clim:with-text-face (pane :bold)
            (clim:with-text-size (pane :large)
              (format pane "  Decal Kind"))))
        (clim:formatting-cell (pane)
          (clim:with-text-face (pane :bold)
            (clim:with-text-size (pane :large)
              (format pane "  Body"))))
        (clim:formatting-cell (pane)
          (clim:with-text-face (pane :bold)
            (clim:with-text-size (pane :large)
              (format pane "  Facing"))))
        (dolist (action +all-actions+)
          (clim:formatting-cell (pane)
            (clim:with-text-face (pane :bold)
              (clim:with-text-size (pane :large)
                (format pane "  ~a" (title-case (string action))))))))
      (terpri pane)
      (let (printed-decal-kind)
        (dolist (decal-kind +decal-kinds+)
          (let (printed-body)
            (dotimes (body (body-count-for-decal-kind decal-kind))
              (clim:formatting-row (pane)
                (clim:formatting-cell (pane :min-height 15)
                  (format pane "~3%")))
              (dolist (facing '(:north :south :east :west))
                (clim:formatting-row (pane)
                  (clim:formatting-cell (pane)
                    (unless (eql printed-decal-kind decal-kind)
                      (format pane "  ~a" (title-case (string decal-kind)))
                      (setf printed-decal-kind decal-kind)))
                  (clim:formatting-cell (pane)
                    (unless (eql printed-body body)
                      (format pane "~d" body)
                      (setf printed-body body)))
                  (clim:formatting-cell (pane)
                    (format pane "~a" (title-case (string facing))))
                  (dolist (action +all-actions+)
                    (clim:formatting-cell (pane)
                      (clim:with-output-as-presentation (pane (list decal-kind body action facing)
                                                              'animation-assignment-slot)
                        (if-let ((seq (find-assigned-animation-sequence
                                       decal-kind body action facing)))
                          (format pane "    ~a~@[ (~a)~]"
                                  (title-case
                                   (simple-animation-sequence-index seq))
                                  (simple-animation-sequence-label seq))
                          (format pane "   —   ")))))))))))
      (force-output pane))))

(defun assign-animation-sequences ()
  "Select which animation sequence applies to which action(s)"
  (clim-sys:make-process (lambda ()
                 (load-all-animation-sequences)
                 (let ((*anim-seq-assigns-frame*
                         (clim:make-application-frame 'anim-seq-assigns-frame)))
                   (setf (clim:frame-pretty-name *anim-seq-assigns-frame*)
                         "Assign Animation Sequences to Actions")
                   (clim:run-frame-top-level *anim-seq-assigns-frame*)))
               :name "Assign Animation Sequences"))

(define-anim-seq-assigns-frame-command (com-edit-assignment :name t)
    ((decal-kind 'simple-animation-sequence-decal-kind)
     (body 'simple-animation-sequence-decal-body)
     (action 'anim-seq-action)
     (facing 'anim-seq-facing))
  (assign-animation-sequence decal-kind body action facing))

(clim:define-presentation-to-command-translator click-to-edit-assignment
    (animation-assignment-slot com-edit-assignment
     anim-seq-assigns-frame
     :gesture :select :menu nil
     :documentation "Change the sequence assigned to this action")
    (parts)
  parts)

(defun accept-chosen-sequence (assign-frame sequence-id all-facings-p)
  (setf (anim-seq-assign-index assign-frame) sequence-id
        (anim-seq-assign-sequence assign-frame) (find-animation-sequence sequence-id))
  (let ((facings (if (or (eql :all (anim-seq-assign-facing assign-frame)) all-facings-p)
                     (list :north :east :west :south)
                     (list (anim-seq-assign-facing assign-frame))))
        (seq (find-animation-sequence sequence-id)))
    (when (emptyp (simple-animation-sequence-label seq))
      (setf (simple-animation-sequence-label seq)
            (format nil "★ ~a~@[ (body ~d)~] ~a ~a"
                    (decal-kind-name (anim-seq-assign-decal-kind assign-frame))
                    (unless (zerop (anim-seq-assign-body assign-frame))
                      (anim-seq-assign-body assign-frame))
                    (sentence-case
                     (string (anim-seq-assign-action assign-frame)))
                    (if (= 1 (length facings))
                        (string-downcase (first facings))
                        "(all directions)"))))
    (dolist (facing facings)
      (set-animation-sequence-assignment (anim-seq-assign-decal-kind assign-frame)
                                         (anim-seq-assign-body assign-frame)
                                         (anim-seq-assign-action assign-frame)
                                         facing
                                         seq)))
  (clim:redisplay-frame-panes assign-frame)
  (when-let (parent (anim-seq-assign-parent assign-frame))
    (clim:redisplay-frame-panes parent)))

(define-anim-seq-assign-frame-command (com-choose-sequence :name t :menu t) ()
  (choose-animation-sequence
   :major-kind :npc
   :decal-kind (anim-seq-assign-decal-kind *anim-seq-assign-frame*)
   :body (anim-seq-assign-body *anim-seq-assign-frame*)
   :selection (anim-seq-assign-index *anim-seq-assign-frame*)
   :callback (curry #'accept-chosen-sequence *anim-seq-assign-frame*)))

(clim:define-presentation-to-command-translator click-to-choose-sequence
    (simple-animation-sequence-index com-choose-sequence
     anim-seq-assign-frame
     :gesture :select :menu nil
     :documentation "Choose a sequence to assign to this action")
    (object)
  (list))

(clim:define-application-frame show-tileset-frame ()
  ((%tileset :type string :initarg :tileset :accessor show-which-tileset)
   (%artp :type boolean :initarg :artp :accessor show-tileset-artp)
   (%write-mode :type keyword :initarg :write-mode :accessor show-tileset-write-mode)
   (%callback :type function :initarg :callback :accessor show-tileset-callback)
   (%palette :type integer :initarg :palette :initform 0 :accessor show-tileset-palette))
  (:panes (tileset-pane :application :height 850 :width 1600
                                     :display-function 'display-tileset)
          (interactor :interactor :height 100 :width 1600))
  (:layouts (default (clim:vertically ()
                       tileset-pane
                       interactor))))

(defmethod display-tileset ((frame show-tileset-frame) pane)
  (let ((columns (ecase (show-tileset-write-mode frame)
                   (:160a 16) (:160b 8)))
        (dump (load-tile-sheet-object-by-name
               (show-which-tileset frame)
               (show-tileset-artp frame)))
        (colors (read-palette-for-tile-sheet
                 (show-which-tileset frame)
                 (show-tileset-palette frame)
                 :write-mode
                 (show-tileset-write-mode frame)))
        (width (ecase (show-tileset-write-mode frame)
                 (:160a 2) (:160b 4))))
    (clim:formatting-table (pane :x-spacing 5 :y-spacing 5)
      (dotimes (row 8)
        (clim:formatting-row (pane)
          (dotimes (column columns)
            (let ((n (+ column (* row columns))))
              (clim:formatting-cell (pane)
                (clim:with-output-as-presentation (pane n 'simple-animation-sequence-frame-reference)
                  (display-maria-art pane
                                     :dump dump
                                     :mode (show-tileset-write-mode frame)
                                     :address (* width n)
                                     :colors colors
                                     :var-colors #(#x48 #x88 #xc8)
                                     :width width 
                                     :unit 5/2)))))))))
  (terpri pane)
  (terpri pane)
  (let ((pal (show-tileset-palette frame)))
    (clim:with-output-as-presentation (pane pal 'palette-selector)
      (format pane "~2%Palette: ~d" pal))))

(defun choose-tile-from-set (&key tileset callback artp write-mode palette)
  "Choose a tile from a set"
  (clim-sys:make-process (lambda ()
                 (load-all-animation-sequences)
                 (let ((*show-tileset-frame*
                         (clim:make-application-frame 'show-tileset-frame
                                                      :tileset tileset
                                                      :callback callback
                                                      :artp artp
                                                      :write-mode write-mode
                                                      :palette palette)))
                   (setf (clim:frame-pretty-name *show-tileset-frame*)
                         (format nil "Show Tileset ~a"
                                 (title-case tileset)))
                   (clim:run-frame-top-level *show-tileset-frame*)))
               :name "Show Tileset"))

(define-show-tileset-frame-command (com-choose-tile :name t)
    ((index 'integer))
  (funcall (show-tileset-callback *show-tileset-frame*) index)
  (clim:frame-exit *show-tileset-frame*))

(clim:define-presentation-to-command-translator click-to-choose-tile
    (simple-animation-sequence-frame-reference com-choose-tile
     show-tileset-frame
     :gesture :select :menu nil
     :documentation "Change the tile assigned to this frame")
    (object)
  (list object))

(clim:define-application-frame choose-sequence-frame ()
  ((%major-kind :accessor choose-sequence-major-kind :initarg :major-kind)
   (%decal-kind :accessor choose-sequence-decal-kind :initarg :decal-kind)
   (%body :accessor choose-sequence-body :initarg :body)
   (%selection :accessor choose-sequence-selection :initarg :selection)
   (%callback :accessor choose-sequence-callback :initarg :callback))
  (:panes (list-pane :application :height 500 :width 400
                                  :display-function 'display-sequences-list)
          (interactor :interactor :height 100 :width 400))
  (:layouts (default (clim:vertically ()
                       list-pane
                       interactor))))

(defun find-sequences-matching (&key major-kind decal-kind body)
  (ecase major-kind
    ((:backgroud :scenery) (error "unimplemented TODO"))
    (:npc (remove-if-not (lambda (seq)
                           (animation-sequence-compatible-p seq decal-kind body))
                         (copy-list *animation-sequences*)))))

(defun animation-sequences-natural-order (a b)
  (cond
    ((string< (simple-animation-sequence-major-kind a)
              (simple-animation-sequence-major-kind b))
     t)
    ((string> (simple-animation-sequence-major-kind a)
              (simple-animation-sequence-major-kind b))
     nil)
    (t (cond ((string< (simple-animation-sequence-decal-kind a)
                       (simple-animation-sequence-decal-kind b))
              t)
             ((string> (simple-animation-sequence-decal-kind a)
                       (simple-animation-sequence-decal-kind b))
              nil)
             (t (cond ((< (simple-animation-sequence-decal-body a)
                          (simple-animation-sequence-decal-body b))
                       t)
                      ((> (simple-animation-sequence-decal-body a)
                          (simple-animation-sequence-decal-body b))
                       nil)
                      (t (cond
                           ((string< (simple-animation-sequence-label a)
                                     (simple-animation-sequence-label b))
                            t)
                           ((string> (simple-animation-sequence-label a)
                                     (simple-animation-sequence-label b))
                            nil)
                           (t (< (simple-animation-sequence-index a)
                                 (simple-animation-sequence-index b)))))))))))

(defmethod display-sequences-list ((frame choose-sequence-frame) pane)
  (let ((matches (sort (find-sequences-matching
                        :major-kind (choose-sequence-major-kind frame)
                        :decal-kind (choose-sequence-decal-kind frame)
                        :body (choose-sequence-body frame))
                       #'animation-sequences-natural-order)))
    (clim:with-text-size (pane :large)
      (format pane "~& ~:(~r~) ~a sequence~0@*~p~*~@[~*~% match decal kind ~a and body ~a~]"
              (length matches)
              (ecase (choose-sequence-major-kind frame)
                (:npc "Non-Player Character")
                (:background "Background (map tiles)")
                (:scenery "Scenery decals"))
              (eql :npc (choose-sequence-major-kind frame))
              (title-case (string (choose-sequence-decal-kind frame)))
              (choose-sequence-body frame)))
    (terpri pane)
    (dolist (match matches)
      (clim:with-output-as-presentation (pane (simple-animation-sequence-index match)
                                              'simple-animation-sequence-index)
        (clim:with-text-face (pane (if (eql (simple-animation-sequence-index match)
                                            (choose-sequence-selection frame))
                                       :bold
                                       :roman))
          (format pane "~%Sequence #~d ~a"
                  (simple-animation-sequence-index match)
                  (let ((label (simple-animation-sequence-label match)))
                    (if (emptyp label)
                        "(unlabeled)"
                        (format nil "“~a”" label)))))))
    (clim:with-output-as-presentation (pane nil 'simple-animation-sequence-index)
      (clim:with-text-face (pane (if (null (choose-sequence-selection frame))
                                     (list :italic :bold)
                                     :italic))
        (format pane "~2%(No explicit assignment)")))
    (clim:with-output-as-presentation (pane nil 'new-animation-sequence)
      (clim:with-text-face (pane :italic)
        (format pane "~2%Create a new sequence")))))

(defvar *choose-sequence-frame* nil)

(define-choose-sequence-frame-command (com-choose-this-sequence :name t)
    ((index 'integer))
  (funcall (choose-sequence-callback *choose-sequence-frame*) index nil)
  (clim:frame-exit *choose-sequence-frame*))

(clim:define-presentation-to-command-translator click-to-choose-sequence
    (simple-animation-sequence-index com-choose-this-sequence
     choose-sequence-frame
     :gesture :select :menu nil
     :documentation "Choose the animation sequence")
    (object)
  (list object))

(define-anim-seq-assign-frame-command (com-edit-this-sequence :name t :menu t)
    ((index 'integer))
  (edit-animation-sequence index))

(clim:define-presentation-to-command-translator click-to-edit-this-sequence
    (anim-seq-assign-sequence com-edit-this-sequence anim-seq-assign-frame
     :gesture :select
     :documentation "Edit the animation sequence")
    (object)
  (list object))

(define-anim-seq-assign-frame-command (com-choose-this-sequence-four-ways :name t)
    ((index 'integer))
  (accept-chosen-sequence *anim-seq-assign-frame* index t)
  (when-let (parent (anim-seq-assign-parent *anim-seq-assign-frame*))
    (clim:redisplay-frame-panes parent))
  (clim:redisplay-frame-panes *anim-seq-assign-frame*))

(clim:define-presentation-to-command-translator click-to-choose-sequence-four-ways
    (magic-four-ways-button com-choose-this-sequence-four-ways anim-seq-assign-frame
     :gesture :select :menu t
     :documentation "Choose the animation sequence to go in all four facing directions")
    (object)
  (list object))

(defun choose-animation-sequence (&key major-kind decal-kind body callback
                                       selection)
  (let ((frame
          (clim:make-application-frame 'choose-sequence-frame
                                       :callback callback
                                       :selection selection
                                       :major-kind major-kind
                                       :decal-kind decal-kind
                                       :body body))
        (name (format nil "Choose Animation Sequence (~a, ~a, ~d)"
                      (title-case (string major-kind))
                      (title-case (string decal-kind))
                      body)))
    (setf *choose-sequence-frame* frame
          (clim:frame-pretty-name frame) name)
    (clim-sys:make-process (lambda () (clim:run-frame-top-level frame))
                 :name name)))

(define-anim-seq-assigns-frame-command (com-save-all-animations :name t :menu t) ()
  (save-all-animation-sequences))

(define-anim-seq-assigns-frame-command (com-load-all-animations :name t :menu t) ()
  (load-all-animation-sequences))

(define-choose-sequence-frame-command (com-create-sequence :name t :menu t) ()
  (let ((seq
          (create-new-animation-sequence :major-kind (choose-sequence-major-kind
                                                      *choose-sequence-frame*)
                                         :decal-kind (choose-sequence-decal-kind
                                                      *choose-sequence-frame*)
                                         :body (choose-sequence-body
                                                *choose-sequence-frame*))))
    (edit-animation-sequence (simple-animation-sequence-index seq))))

(clim:define-presentation-to-command-translator click-to-create-sequence
    (new-animation-sequence com-create-sequence choose-sequence-frame
     :gesture :select
     :documentation "Create a new animation sequence matching these criteria")
    (object)
  (list))

(defun display-anim-preview (window pane)
  (format pane "TODO: Display a preview of the animation here"))

(defun sort-matching-lists-by-decal-kind (a b)
  (let ((d-a (position (elt a 0) +decal-kinds+))
        (d-b (position (elt b 0) +decal-kinds+)))
    (cond
      ((< d-a d-b) t)
      ((> d-a d-b) nil)
      (t (cond
           ((< (elt a 1) (elt b 1)) t)
           ((> (elt a 1) (elt b 1)) nil)
           (t (< (sxhash a)
                 (sxhash b))))))))

(defun sort-animation-sequences-by-decal-kind (a b)
  (let ((d-a (position (simple-animation-sequence-decal-kind a) +decal-kinds+))
        (d-b (position (simple-animation-sequence-decal-kind b) +decal-kinds+)))
    (cond
      ((< d-a d-b) t)
      ((> d-a d-b) nil)
      (t (cond
           ((< (simple-animation-sequence-decal-body a)
               (simple-animation-sequence-decal-body b))
            t)
           ((> (simple-animation-sequence-decal-body a)
               (simple-animation-sequence-decal-body b))
            nil)
           (t (< (simple-animation-sequence-index a)
                 (simple-animation-sequence-index b))))))))

(defun compile-animation-sequences ()
  (format *trace-output* "~&Compiling animation sequence data …")
  (let ((source-name (make-pathname :directory (list :relative "Source" "Generated")
                                    :name "AnimationTable" :type "s")))
    (ensure-directories-exist source-name)
    (load-all-animation-sequences)
    (let ((sorted (sort (copy-list *animation-sequences*)
                        #'sort-animation-sequences-by-decal-kind))
          (id-remap (vector (make-hash-table) (make-hash-table) (make-hash-table)))
          (keys (sort (hash-table-keys *animation-assignments*)
                      #'sort-matching-lists-by-decal-kind)))
      (loop for seq in sorted
            for log2-1 = (1- (floor (log (simple-animation-sequence-frame-count seq)
                                         2)))
            with max = (vector -1 -1 -1 -1)
            unless (minusp log2-1)
              do (setf (gethash (simple-animation-sequence-index seq)
                                (aref id-remap log2-1))
                       (incf (aref max log2-1))))
      (with-output-to-file (s source-name :if-exists :supersede)
        (format s ";;; ~a Source/Generated/AnimationTable.s
;;;; This is a generated file (from Animation.ods), editing it is futile~2%"
                (title-case *game-title*))
        (format s "~2%~10t.section BankData~%AnimationTable: .block")
        (format s "~2%Sequences:")
        (dolist (frames '(2 4 8))
          (format s "~2%~:(~r~)Frames:" frames)
          (dolist (seq sorted)
            (when (= frames (simple-animation-sequence-frame-count seq))
              (format s "~%~10t.byte ~{$~2,'0x~^, ~}~40t; ~d. ~a ~a"
                      (mapcar (curry #'* (ecase (simple-animation-sequence-write-mode seq)
                                           (:160a 2)
                                           (:160b 4)))
                              (subseq (coerce (simple-animation-sequence-frames seq) 'list)
                                      0 (simple-animation-sequence-frame-count seq)))
                      (rationalize (simple-animation-sequence-index seq))
                      (simple-animation-sequence-label seq)
                      (simple-animation-sequence-write-mode seq)))))
        (format s "~2%Assignments:")
        (format s "~2%MatchDecalAlternatives:")
        (dolist (kind +decal-kinds+)
          (format s "~%~10t.byte ~d~31t; ~a"
                  (count-if (lambda (key)
                              (eql kind (first key)))
                            (hash-table-keys *animation-assignments*))
                  (title-case (string kind))))
        (format s "~2%~10tDecalKindActions=(~{Match~aAction~^, ~})"
                (mapcar (compose #'pascal-case #'string) +decal-kinds+))
        (format s "

MatchDecalKindActionL:
~10t.byte <DecalKindActions
MatchDecalKindActionH:
~10t.byte >DecalKindActions")
        (format s "~2%~10tDecalKindDetails=(~{Match~aDetails~^, ~})"
                (mapcar (compose #'pascal-case #'string) +decal-kinds+))
        (format s "

MatchDecalKindDetailsL:
~10t.byte <DecalKindDetails
MatchDecalKindDetailsH:
~10t.byte >DecalKindDetails")
        (format s "~2%~10tDecalKindReferences=(~{Match~aReference~^, ~})"
                (mapcar (compose #'pascal-case #'string) +decal-kinds+))
        (format s "

MatchDecalKindReferenceL:
~10t.byte <DecalKindReferences
MatchDecalKindReferenceH:
~10t.byte >DecalKindReferences")
        (format s "~2%MatchDecalBodies:")
        (dolist (kind +decal-kinds+)
          (let ((set (make-hash-table)))
            (dolist (seq *animation-sequences*)
              (when (eql kind (simple-animation-sequence-decal-kind seq))
                (incf (gethash (simple-animation-sequence-decal-body seq) set 0))))
            (format s "~%~10t.byte ~d~32t; Body count for ~a"
                    (hash-table-count set)
                    (title-case (string kind)))))
        (format s "~2%MatchAction:")
        (let (last-kind last-body)
          (dolist (key keys)
            (destructuring-bind (kind body action facing) key
              (unless (eql kind last-kind)
                (format s "~%Match~aAction:" (pascal-case (string kind)))
                (setf last-kind kind))
              (unless (or (zerop body) (eql body last-body))
                (format s "~%;;; switch to body ~s~%~10t.byte 0" body)
                (setf last-body body))
              (let* ((seq (gethash key *animation-assignments*))
                     (i (simple-animation-sequence-index seq)))
                (format s "~%~10t.byte Action~20,,1a | DecalFacingMatch~:(~6,,1a~) << 6~%~10t;; ~d. ~a~%"
                        (pascal-case (string action))
                        facing
                        i
                        (simple-animation-sequence-label seq))))))
        (format s "~2%Details:")
        (let (last-kind)
          (dolist (key keys)
            (destructuring-bind (kind body action facing) key
              (declare (ignore body action facing))
              (unless (eql kind last-kind)
                (format s "~%Match~aDetails:" (pascal-case (string kind)))
                (setf last-kind kind))
              (let* ((seq (gethash key *animation-assignments*))
                     (i (simple-animation-sequence-index seq)))
                (format s "
~10t.byte ~2d << 4 | ~2d ~32t; ~d. ~a
~10t;; (~r frame~:p at ~{~a ~a~} fps)~%"
                        (floor (log (simple-animation-sequence-frame-count seq) 2))
                        (floor
                         (log (/ 1
                                 (simple-animation-sequence-frame-rate-scalar seq))
                              2))
                        i
                        (simple-animation-sequence-label seq)
                        (simple-animation-sequence-frame-count seq)
                        (substitute
                         #\Space 0
                         (multiple-value-list
                          (floor (* 10 (simple-animation-sequence-frame-rate-scalar seq))))))))))
        (format s "~2%Reference:")
        (let (last-kind)
          (dolist (key keys)
            (let* ((seq (gethash key *animation-assignments*))
                   (i (simple-animation-sequence-index seq)))
              (let ((kind (simple-animation-sequence-decal-kind seq)))
                (unless (eql kind last-kind)
                  (format s "~%Match~aReference:" (pascal-case (string kind)))
                  (setf last-kind kind)))
              (ecase (simple-animation-sequence-frame-count seq)
                (1 (format s "~%~10t.byte $~2,'0x~20t; single frame index; ~d. ~a (~a)"
                           (* (ecase (simple-animation-sequence-write-mode seq)
                                (:160a 2)
                                (:160b 4))
                              (aref (simple-animation-sequence-frames seq) 0))
                           i
                           (simple-animation-sequence-label seq)
                           (simple-animation-sequence-write-mode seq)))
                ((2 4 8)
                 (format s "~%~10t.byte $~2,'0x~20t; ~r-frame animation; ~d. ~a"
                         (gethash i
                                  (aref id-remap
                                        (1- (floor (log
                                                    (simple-animation-sequence-frame-count seq)
                                                    2)))))
                         (simple-animation-sequence-frame-count seq)
                         i
                         (simple-animation-sequence-label seq)))))))
        (format s "~2%~10t;; any unavailable kinds will appear in this section to make tables consistent")
        (dolist (kind +decal-kinds+)
          (unless (some (lambda (seq) (eql kind (simple-animation-sequence-decal-kind seq)))
                        *animation-sequences*)
            (format s "
Match~aAction:~0@*
Match~aDetails:~0@*
Match~aReference:
"
                    (pascal-case (string kind)))))
        (format s "~2%~10t.byte $ff")
        (format s "~2%~10t.bend~%~10t.send~2%"))))
  (format *trace-output* " done, AnimationTable.s ready.~%"))
