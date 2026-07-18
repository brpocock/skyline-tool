;;; Phantasia SkylineTool/src/sprite-sheet-inspector.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC.

(in-package :skyline-tool)

;; --- Data structures ---

(defstruct sprite-entry
  (filename "" :type string)
  (name "" :type string)
  (mode "160B" :type string)
  (width 8 :type integer)
  (height 16 :type integer))

(defun sprite-bytes (entry)
  "Calculate the number of bytes for a SPRITE-ENTRY based on dimensions and mode."
  (* (sprite-entry-width entry) (sprite-entry-height entry)
     (if (string-equal (sprite-entry-mode entry) "160A") 4 2)))

(defun sprite-budget (entry)
  "Return the byte budget for a SPRITE-ENTRY (8192 for 160A, 4096 for 160B)."
  (if (string-equal (sprite-entry-mode entry) "160A") 8192 4096))

;; --- Art file I/O ---

(defun parse-art-line (line)
  "Parse a single .art file LINE into a sprite-entry struct, or NIL."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline) line))
         (parts (split-sequence #\Space trimmed :remove-empty-subseqs t)))
    (when (< (length parts) 3)
      (return-from parse-art-line nil))
    (let* ((filename (first parts))
           (mode (second parts))
           (dims (third parts)))
      (multiple-value-bind (w h) (parse-dimensions dims)
        (when (and w h)
          (make-sprite-entry
           :filename filename
           :name (pathname-name filename)
           :mode mode
           :width w
           :height h))))))

(defun parse-dimensions (str)
  "Parse a dimension string like \"8×16\" into (values W H)."
  (let ((pos (or (position (code-char #x00D7) str)
                 (position-if (lambda (c) (char-equal c #\x)) str))))
    (when pos
      (let ((w (parse-integer (subseq str 0 pos) :junk-allowed t))
            (h (parse-integer (subseq str (1+ pos)) :junk-allowed t)))
        (values w h)))))

(defun load-art-file (path)
  "Load all sprite entries from an .art file at PATH."
  (with-open-file (f path :external-format :utf-8)
    (loop for line = (read-line f nil nil)
          while line
          for entry = (parse-art-line line)
          when entry
            collect entry)))

(defun save-art-file (path sprites)
  "Save SPRITES list to PATH in .art file format."
  (with-open-file (f path :direction :output :if-exists :supersede
                      :external-format :utf-8)
    (dolist (s sprites)
      (format f "~a ~a ~d~c~d~%" (sprite-entry-filename s) (sprite-entry-mode s)
              (sprite-entry-width s) (code-char #x00D7) (sprite-entry-height s)))))

;; --- Presentation types ---

(clim:define-presentation-type sprite-entry-presentation ()
  :inherit-from 'sprite-entry)

(clim:define-presentation-type sprite-thumbnail-presentation ()
  :inherit-from 'string)

;; --- Frame ---

(clim:define-application-frame sprite-sheet-inspector-frame (resource-inspector-mixin uniform-inspector-frame)
  ((path :initarg :path :accessor frame-path)
   (sprites :initform nil :accessor frame-sprites)
   (dirty :initform nil :accessor frame-dirty))
  (:menu-bar sprite-sheet-inspector-menu-bar)
  (:panes
   (display-pane :application :display-function 'display-sprite-sheet
                 :height 600 :width 750
                 :scroll-bars :vertical)
   (interactor :interactor :height 80 :width 750))
  (:layouts
   (default (clim:vertically () display-pane interactor))))

;; --- Command tables ---

(clim:define-command-table sprite-sheet-inspector-file-menu
  :menu (("Save" :command com-save-sprite-sheet)
         (nil :divider :line)
         ("Close" :command com-close-sprite-sheet-inspector)))

(clim:define-command-table sprite-sheet-inspector-edit-menu
  :menu (("Add Sprite..." :command com-add-sprite)
         ("Delete Sprite" :command com-delete-sprite)))

(clim:define-command-table sprite-sheet-inspector-help-menu
  :menu (("How to Edit Sprite Sheets" :command com-help-for-window)
         ("Skyline-Tool Developers' Guide" :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide" :command com-open-scripting-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table sprite-sheet-inspector-menu-bar
  :menu (("File" :menu sprite-sheet-inspector-file-menu)
         ("Edit" :menu sprite-sheet-inspector-edit-menu)
         ("Help" :menu sprite-sheet-inspector-help-menu)))

;; --- Display helpers ---

(defun art-directory-from-path (art-path)
  "Return the directory portion of an ART-PATH for finding sibling PNG/XCF files."
  (make-pathname :defaults art-path :name nil :type nil))

(defun xcf-path-from-sprite (art-path sprite)
  "Return the expected XCF path for a SPRITE-ENTRY in ART-PATH."
  (let ((dir (art-directory-from-path art-path)))
    (merge-pathnames (make-pathname :name (sprite-entry-name sprite) :type "xcf") dir)))

(defun png-path-from-sprite (art-path sprite)
  "Return the expected PNG path for a SPRITE-ENTRY in ART-PATH."
  (let ((dir (art-directory-from-path art-path)))
    (merge-pathnames (make-pathname :name (sprite-entry-name sprite) :type "png") dir)))

;; --- Thumbnail loading ---

(defvar *sprite-thumbnail-cache* (make-hash-table :test 'equal)
  "Cache of loaded PNG images: path → (width height rgb-array)")

(defun load-sprite-png (art-path sprite)
  "Load PNG for SPRITE, return (width height rgb-array) or nil."
  (let* ((png-rel (sprite-entry-filename sprite))
         (dir (make-pathname :defaults art-path :name nil :type nil))
         (png-path (merge-pathnames (make-pathname :name (pathname-name png-rel)
                                                    :type "png") dir))
         (key (namestring png-path)))
    (or (gethash key *sprite-thumbnail-cache*)
        (when (probe-file png-path)
          (handler-case
              (let* ((png (png-read:read-png-file key))
                     (w (png-read:width png))
                     (h (png-read:height png))
                     (data (png-read:image-data png))
                     (dims (array-dimensions data))
                     (bpp (if (= (length dims) 3) (third dims) 3))
                     (rgb (make-array (list w h 3) :element-type '(unsigned-byte 8))))
                (dotimes (y h)
                  (dotimes (x w)
                    (if (= (length dims) 3)
                        (progn
                          (setf (aref rgb x y 0) (aref data x y 0)
                                (aref rgb x y 1) (aref data x y 1)
                                (aref rgb x y 2) (aref data x y 2)))
                        ;; Paletted image — use pixel value as gray
                        (let ((idx (aref data x y)))
                          (when idx
                            (setf (aref rgb x y 0) idx
                                  (aref rgb x y 1) idx
                                  (aref rgb x y 2) idx))))))
                (let ((result (list w h rgb)))
                  (setf (gethash key *sprite-thumbnail-cache*) result)
                  result))
            (error () nil)))
        (progn (setf (gethash key *sprite-thumbnail-cache*) nil) nil))))

(defun draw-sprite-thumbnail (pane sprite art-path x y scale)
  "Draw a scaled thumbnail of SPRITE at (x, y) in PANE."
  (multiple-value-bind (png-w png-h rgb)
      (values-list (load-sprite-png art-path sprite))
    (when rgb
      (let* ((tw (min (sprite-entry-width sprite) png-w))
             (th (min (sprite-entry-height sprite) png-h))
             (sw (* tw scale))
             (sh (* th scale)))
        (dotimes (ty th)
          (dotimes (tx tw)
            (let ((r (aref rgb tx ty 0))
                  (g (aref rgb tx ty 1))
                  (b (aref rgb tx ty 2)))
              (clim:draw-rectangle* pane (+ x (* tx scale)) (+ y (* ty scale))
                                     (+ x (* (1+ tx) scale)) (+ y (* (1+ ty) scale))
                                     :ink (clim:make-rgb-color (/ r 255.0) (/ g 255.0) (/ b 255.0)))))))
      (clim:draw-rectangle* pane x y (+ x (* (min (sprite-entry-width sprite) png-w) scale))
                             (+ y (* (min (sprite-entry-height sprite) png-h) scale))
                             :ink clim:+foreground-ink+ :filled nil))))

;; --- Display ---

(defmethod display-inspector-content ((frame sprite-sheet-inspector-frame) pane)
  (display-sprite-sheet frame pane))

(defun display-sprite-sheet (frame pane)
  "Display all sprites with thumbnails, mode badges, dimensions, byte progress, and budget summary."
  (clim:window-clear pane)
  (let* ((sprites (frame-sprites frame))
         (path (frame-path frame)))
    (unless sprites
      (format pane "~&~%(empty sprite sheet)")
      (return-from display-sprite-sheet))
    (format pane "~&")
    (clim:with-text-face (pane :bold)
      (clim:with-text-size (pane :smaller)
        (format pane "  ~24a  ~a   ~a    ~a  ~a~%"
                "Sprite" "Mode" "Dims" "Bytes" "Used/Budget")))
    (format pane "  ~24a  ~a   ~a    ~a  ~a~%"
            "------------------------" "----" "----" "-----" "---------------")
    (let ((total-bytes 0)
          (total-budget 0)
          (row 0))
      (dolist (sprite sprites)
        (let* ((bytes (sprite-bytes sprite))
               (budget (sprite-budget sprite))
               (pct (/ (min bytes budget) (max budget 1))))
          (incf total-bytes bytes)
          (incf total-budget budget)
          ;; Thumbnail column
          (clim:with-output-as-presentation (pane sprite 'sprite-entry-presentation)
            (let ((thumb-size 2)
                  (cx (clim:stream-cursor-position pane)))
              (draw-sprite-thumbnail pane sprite path
                                     (nth-value 0 cx) (nth-value 1 cx) thumb-size))
            (format pane " ")
            ;; Sprite name as clickable thumbnail link
            (clim:with-output-as-presentation
                (pane (sprite-entry-name sprite) 'sprite-thumbnail-presentation)
              (clim:with-text-face (pane :bold)
                (let ((xcf (xcf-path-from-sprite path sprite)))
                  (if (probe-file xcf)
                      (clim:with-drawing-options (pane :ink (clim:make-rgb-color 0 0.4 0.7))
                        (format pane "~24a" (sprite-entry-name sprite)))
                      (format pane "~24a" (sprite-entry-name sprite))))))
            ;; Mode badge
            (if (string-equal (sprite-entry-mode sprite) "160A")
                (clim:with-drawing-options (pane :ink (clim:make-rgb-color 0.8 0.2 0.2))
                  (format pane "  ~4a" (sprite-entry-mode sprite)))
                (clim:with-drawing-options (pane :ink (clim:make-rgb-color 0.2 0.2 0.8))
                  (format pane "  ~4a" (sprite-entry-mode sprite))))
            ;; Dimensions
            (format pane "  ~d×~d" (sprite-entry-width sprite) (sprite-entry-height sprite))
            ;; Bytes
            (format pane "  ~5d" bytes)
            ;; Progress bar for this sprite
            (emit-clim-progress-bar pane pct :width 60 :height 8)
            (terpri pane)
            (incf row)))
      ;; Budget summary
      (format pane "~%")
      (multiple-value-bind (cx cy) (clim:stream-cursor-position pane)
        (declare (ignore cx))
        (clim:draw-rectangle* pane 2 cy
                               (- (clim:bounding-rectangle-width (clim:sheet-region pane)) 2)
                               (+ cy 2)
                               :filled t :ink (clim:make-gray-color 0.8)))
      (format pane "~&")
      (clim:with-text-face (pane :bold)
        (format pane "  Total: ~6d bytes  Budget: ~6d  " total-bytes total-budget))
      (emit-clim-progress-bar pane (/ (min total-bytes total-budget) (max total-budget 1))
                              :width 200 :height 16)
      (format pane "  (~d%)"
              (round (* 100 (min 1 (/ total-bytes (max total-budget 1))))))
      (format pane "~&  Click a sprite name to open in GIMP; click a row to edit.")))

;; --- Commands ---

(clim:define-command (com-save-sprite-sheet :menu t :name t) ()
  (let* ((frame (and (boundp 'clim:*application-frame*) clim:*application-frame*))
         (path (and frame (frame-path frame)))
         (sprites (and frame (frame-sprites frame))))
    (when (and path sprites)
      (save-art-file path sprites)
      (setf (frame-dirty frame) nil)
      (format *query-io* "~&Saved ~a~%" (namestring path)))))

(clim:define-command (com-close-sprite-sheet-inspector :menu t :name t) ()
  (let ((frame (and (boundp 'clim:*application-frame*) clim:*application-frame*)))
    (when frame
      (when (frame-dirty frame)
        (unless (run-confirm-dialog "Unsaved changes. Close anyway?" :title "Confirm Close")
          (return-from com-close-sprite-sheet-inspector)))
      (clim:frame-exit frame))))

(clim:define-command (com-add-sprite :menu t :name t) ()
  (let* ((frame (and (boundp 'clim:*application-frame*) clim:*application-frame*))
         (sprites (and frame (frame-sprites frame))))
    (when frame
      (let* ((fields `((:name :filename :label "Filename: " :value "NewSprite.png" :type string)
                      (:name :mode :label "Mode: " :value "160B" :type string)
                      (:name :width :label "Width: " :value 8 :type integer)
                      (:name :height :label "Height: " :value 16 :type integer)))
             (result (run-multi-field-input-dialog "Add new sprite:" fields :title "Add Sprite")))
        (when result
          (let ((filename (cdr (assoc :filename result)))
                (mode (cdr (assoc :mode result)))
                (width (cdr (assoc :width result)))
                (height (cdr (assoc :height result))))
            (push (make-sprite-entry
                   :filename filename
                   :name (pathname-name filename)
                   :mode mode
                   :width width
                   :height height)
                  sprites)
            (setf (frame-sprites frame) (sort sprites #'string-lessp :key #'sprite-entry-name)
                  (frame-dirty frame) t)
            (clim:redisplay-frame-panes frame :force-p t)))))))

(clim:define-command (com-delete-sprite :menu t :name t)
    ((sprite 'sprite-entry-presentation :gesture :select))
  (let* ((frame (and (boundp 'clim:*application-frame*) clim:*application-frame*))
         (sprites (and frame (frame-sprites frame))))
    (when (and frame sprites)
      (setf (frame-sprites frame) (remove sprite sprites :test #'equalp)
            (frame-dirty frame) t)
      (clim:redisplay-frame-panes frame :force-p t))))

(clim:define-command (com-edit-sprite :command-table clim-internals::global-command-table
                                      :menu nil :name t)
    ((sprite 'sprite-entry-presentation :gesture :select))
  (let* ((frame (and (boundp 'clim:*application-frame*) clim:*application-frame*))
         (sprites (and frame (frame-sprites frame)))
         (pos (and sprites (position sprite sprites :test #'equalp))))
    (when pos
      (let* ((old (elt sprites pos))
             (fields `((:name :filename :label "Filename: " :value ,(sprite-entry-filename old) :type string)
                       (:name :mode :label "Mode: " :value ,(sprite-entry-mode old) :type string)
                       (:name :width :label "Width: " :value ,(sprite-entry-width old) :type integer)
                       (:name :height :label "Height: " :value ,(sprite-entry-height old) :type integer)))
             (result (run-multi-field-input-dialog "Edit sprite:" fields :title "Edit Sprite")))
        (when result
          (let ((filename (cdr (assoc :filename result)))
                (mode (cdr (assoc :mode result)))
                (width (cdr (assoc :width result)))
                (height (cdr (assoc :height result))))
            (setf (elt (frame-sprites frame) pos)
                  (make-sprite-entry
                   :filename filename
                   :name (pathname-name filename)
                   :mode mode
                   :width width
                   :height height))
            (setf (frame-dirty frame) t)
            (clim:redisplay-frame-panes frame :force-p t)))))))

;; --- Open sprite in GIMP ---

(clim:define-command (com-open-sprite-in-gimp :command-table clim-internals::global-command-table
                                               :menu nil :name t)
    ((sprite-name 'sprite-thumbnail-presentation :gesture :select))
  (let* ((frame (and (boundp 'clim:*application-frame*) clim:*application-frame*))
         (path (and frame (frame-path frame)))
         (xcf (and path (xcf-path-from-sprite path
                                              (make-sprite-entry :name sprite-name))))
         (png (and path (png-path-from-sprite path
                                              (make-sprite-entry :name sprite-name))))
         (target (cond ((and xcf (probe-file xcf)) xcf)
                       ((and png (probe-file png)) png)
                       (t nil))))
    (if target
        (let ((choices (clim:menu-choose
                        (list (list "Open in GIMP" 'gimp)
                              (list "Open Containing Folder" 'folder))
                        :label (format nil "~a" sprite-name))))
          (case choices
            (gimp (uiop:run-program (list "gimp" (namestring (truename target)))
                                     :output nil :ignore-error-status t))
            (folder (uiop:run-program (list "xdg-open"
                                            (namestring (art-directory-from-path path)))
                                       :output nil :ignore-error-status t))))
        (format *query-io* "~&Source file not found for ~a" sprite-name))))))

;; --- Opening the inspector ---

(defun open-sprite-sheet-inspector (path)
  (let* ((sprites (load-art-file path))
         (resource (make-instance 'game-resource-sprite-sheet
                                  :moniker (pathname-name path)
                                  :kind "Sprite Sheet"
                                  :full-path (truename path)))
         (fm (clim:find-frame-manager :port (or (clim:find-port) (clim:find-port :server-path :x))))
         (frame (clim:make-application-frame
                  'sprite-sheet-inspector-frame
                  :resource resource
                  :path path
                  :sprites sprites
                  :frame-manager fm
                  :pretty-name (format nil "Sprite Sheet: ~a" (pathname-name path))
                  :width 800 :height 700)))
    (clim-sys:make-process
     (lambda () (clim:run-frame-top-level frame))
     :name (format nil "Sprite Sheet Inspector: ~a" (pathname-name path)))))
