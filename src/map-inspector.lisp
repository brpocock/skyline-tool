;;; Phantasia SkylineTool/src/map-editor.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC.

(in-package :skyline-tool)

;; Macro for defining frame-specific commands for map-inspector-frame
(defmacro define-map-inspector-frame-command ((name &rest options) args &body body)
  "Define a CLIM command for the map-inspector-frame command table."
  `(clim:define-command (,name :command-table map-inspector-frame ,@options)
     ,args
     ,@body))

;; --- TMX Parsing ---

(defstruct map-editor-data
  tmx-path
  xml
  display-name
  numeric-suffix
  width
  height
  tilesets
  properties
  spawns
  run-commands
  entrances
  exits
  triggers)

(defun split-map-name (filename)
  (let* ((stem (pathname-name filename))
         (len (length stem)))
    (if (and (plusp len) (digit-char-p (char stem (1- len))))
        (let ((split (position-if-not #'digit-char-p stem :from-end t :end (1- len))))
          (if split
              (values (subseq stem 0 (1+ split)) (subseq stem (1+ split)))
              (values stem "")))
        (values stem ""))))

;; ─── Tile rendering for PDF export ─────────────────────

(defun %parse-layer-gids (tmx-xml)
  "Parse TMX layer data, returning list of (width height gid-array)."
  (dolist (child (subseq (second tmx-xml) 2))
    (when (equal "layer" (car child))
      (let* ((width (parse-integer (xml-attr "width" (second child))))
             (height (parse-integer (xml-attr "height" (second child))))
             (data-el (xml-elt "data" child))
             (encoding (xml-attr "encoding" (second data-el))))
        (when (equal "base64" encoding)
          (let* ((data-xml (third data-el))
                 (text (if (stringp data-xml) data-xml ""))
                 (bytes (cl-base64:base64-string-to-usb8-array
                         (string-trim '(#\Space #\Tab #\Newline) text)))
                 (gids (make-array (list width height) :element-type 'integer)))
            (dotimes (y height)
              (dotimes (x width)
                (let ((offset (* (+ x (* y width)) 4)))
                  (when (< (+ offset 3) (length bytes))
                    (setf (aref gids x y)
                          (logior (aref bytes offset)
                                  (ash (aref bytes (1+ offset)) 8)
                                  (ash (aref bytes (+ offset 2)) 16)
                                  (ash (aref bytes (+ offset 3)) 24)))))))
            (return-from %parse-layer-gids (list width height gids))))))
    (when (equal "layer" (car child))
      (let* ((width (parse-integer (xml-attr "width" (second child))))
             (height (parse-integer (xml-attr "height" (second child))))
             (data-el (xml-elt "data" child))
             (encoding (xml-attr "encoding" (second data-el))))
        (when (equal "csv" encoding)
          (let* ((csv-text (string-trim '(#\Space #\Tab #\Newline) (third data-el)))
                 (values (mapcar #'parse-integer
                                 (cl-ppcre:split "[,\\s]+" csv-text :omit-unmatched t)))
                 (gids (make-array (list width height) :element-type 'integer)))
            (dotimes (y height)
              (dotimes (x width)
                (let ((idx (+ x (* y width))))
                  (when (< idx (length values))
                    (setf (aref gids x y) (elt values idx))))))
            (return-from %parse-layer-gids (list width height gids))))))
  nil))

(defun %resolve-tsx-png (tsx-path)
  "Given a TSX pathname, resolve its referenced PNG image path."
  (ignore-errors
    (let* ((xml (xmls:parse-to-list (read-file-into-string tsx-path)))
           (img-el (xml-elt "image" (second xml)))
           (src (xml-attr "source" (second img-el))))
      (when src
        (merge-pathnames (make-pathname :name src :type nil)
                         (make-pathname :directory (pathname-directory tsx-path)))))))

(defun %render-map-tiles (data)
  "Render map tile grid to (width-pixels height-pixels rgb-array) using tileset PNGs.
   Each tile is 8x16 pixels, doubled horizontally for aspect ratio."
  (let* ((map-w (map-inspector-data-width data))
         (map-h (map-inspector-data-height data))
         (tile-w 8) (tile-h 16)
         (out-w (* map-w tile-w 2)) (out-h (* map-h tile-h))
         (rgb (make-array (list out-w out-h 3) :element-type '(unsigned-byte 8) :initial-element 0))
         (layer-data (%parse-layer-gids (map-inspector-data-xml data)))
         (tsx-cache (make-hash-table :test 'equal)))
    (when layer-data
      (destructuring-bind (grid-w grid-h gids) layer-data
        (dotimes (y (min grid-h map-h))
          (dotimes (x (min grid-w map-w))
            (let* ((gid (aref gids x y)))
              (when (plusp gid)
                ;; Find which tileset this GID belongs to
                (let ((ts-info (find gid (map-inspector-data-tilesets data)
                                    :test (lambda (g ts) (>= g (car ts)))
                                    :key #'car))
                      (tile-bytes nil))
                  (when ts-info
                    (let* ((firstgid (car ts-info))
                           (tsx-name (cdr ts-info))
                           (local-id (- gid firstgid))
                           (tsx-full (merge-pathnames
                                       (make-pathname :name tsx-name :type "tsx")
                                       (make-pathname :directory '(:relative "Source" "Maps" "Tiles"))))
                           (png-path (or (gethash tsx-name tsx-cache)
                                         (let ((p (%resolve-tsx-png tsx-full)))
                                           (setf (gethash tsx-name tsx-cache) p) p))))
                      (when png-path
                        (ignore-errors
                          (let* ((png (png-read:read-png-file (namestring png-path)))
                                 (pw (png-read:width png))
                                 (ph (png-read:height png))
                                 (data (png-read:image-data png))
                                 (cols (floor pw tile-w))
                                 (tx (mod local-id cols))
                                 (ty (floor local-id cols))
                                 (px (* tx tile-w))
                                 (py (* ty tile-h)))
                            ;; Copy tile pixels, doubled horizontally
                            (dotimes (dy tile-h)
                              (when (< (+ py dy) ph)
                                (dotimes (dx tile-w)
                                  (when (< (+ px dx) pw)
                                    (let ((r (if (= (array-total-size data) (* pw ph 4))
                                                 (aref data (+ px dx) (+ py dy) 0)
                                                 (aref data (+ px dx) (+ py dy))))
                                          (g (if (= (array-total-size data) (* pw ph 4))
                                                 (aref data (+ px dx) (+ py dy) 1)
                                                 (aref data (+ px dx) (+ py dy))))
                                          (b (if (= (array-total-size data) (* pw ph 4))
                                                 (aref data (+ px dx) (+ py dy) 2)
                                                 (aref data (+ px dx) (+ py dy)))))
                                      (let ((out-x (* (+ (* x tile-w) dx) 2))
                                            (out-y (+ (* y tile-h) dy)))
                                        ;; Double pixel horizontally
                                        (setf (aref rgb out-x out-y 0) r (aref rgb out-x out-y 1) g (aref rgb out-x out-y 2) b)
                                        (setf (aref rgb (1+ out-x) out-y 0) r (aref rgb (1+ out-x) out-y 1) g (aref rgb (1+ out-x) out-y 2) b)))))))))))))))))
        (values out-w out-h rgb)))))

;; ─── TMX Parsing ──────────────────────────────────────

(defun parse-tmx-file (tmx-path)
  (let* ((xml (xmls:parse-to-list (read-file-into-string tmx-path)))
         (map-el xml)
         (width (parse-integer (xml-attr "width" (second map-el))))
         (height (parse-integer (xml-attr "height" (second map-el))))
         (tilesets '())
         (properties '())
         (spawns '())
         (run-commands '())
         (entrances '())
         (exits '())
         (triggers '()))
    (dolist (child (subseq map-el 2))
      (cond
        ((equal "tileset" (car child))
         (let ((firstgid (parse-integer (xml-attr "firstgid" (second child))))
               (source (xml-attr "source" (second child))))
           (push (cons firstgid source) tilesets)))
        ((equal "properties" (car child))
         (setf properties (properties->plist child)))
        ((equal "objectgroup" (car child))
         (dolist (obj (subseq child 2))
           (when (equal "object" (car obj))
             (let* ((attrs (second obj))
                    (name (or (assocdr "name" attrs) ""))
                    (type (or (assocdr "type" attrs) ""))
                    (x (parse-number (or (assocdr "x" attrs) "0")))
                    (y (parse-number (or (assocdr "y" attrs) "0")))
                    (obj-props (and (xml-match "properties" obj nil)
                                    (properties->plist (xml-match "properties" obj)))))
               (cond
                 ((getf obj-props :|Exit|)
                  (push (list (getf obj-props :|Exit|) x y) exits))
                 ((getf obj-props :|Entrance|)
                  (push (list (getf obj-props :|Entrance|) x y) entrances))
                 ((getf obj-props :|Character|)
                  (push (list (getf obj-props :|Character|) :character x y) spawns))
                 ((getf obj-props :|Object|)
                  (push (list (getf obj-props :|Object|) :object x y) spawns))
                 ((getf obj-props :|Script|)
                  (push (list (getf obj-props :|Script|) x y) triggers))
                 ((and (plusp (length type))
                       (not (member type '("" "Wall") :test #'equal)))
                  (push (list type :type x y) spawns))))))))
    (let ((rc (getf properties :|RC|)))
      (when rc
        (push rc run-commands)))
    (multiple-value-bind (display-name numeric-suffix)
        (split-map-name tmx-path)
      (make-map-editor-data
       :tmx-path (truename tmx-path)
       :xml xml
       :display-name display-name
       :numeric-suffix numeric-suffix
       :width width
       :height height
       :tilesets (nreverse tilesets)
       :properties properties
       :spawns (nreverse spawns)
       :run-commands (nreverse run-commands)
       :entrances (nreverse entrances)
       :exits (nreverse exits)
       :triggers (nreverse triggers))))))

;; --- Frame Definition ---

(clim:define-application-frame map-inspector-frame (resource-inspector-mixin clim:standard-application-frame)
  ((%data :initform nil :accessor map-inspector-frame-data)
   (%original-tmx-path :initarg :tmx-path :accessor map-editor-original-tmx-path)
   (%new-suffix :initform nil :accessor map-inspector-new-suffix))
  (:panes
   (content :application :scroll-bars t :height 700 :width 600
            :display-function 'display-resource-inspector)
   (interactor :interactor :height 100 :width 600 :max-height 100))
  (:menu-bar map-inspector-menu-bar)
  (:icon (skyline-tool-icon))
  (:layouts (default (clim:vertically () content interactor))))

;; --- Command Tables ---

(clim:define-command-table map-inspector-save-as-menu
  :menu (("Text..." :command com-map-inspector-save-text)
         ("JSON..." :command com-map-inspector-save-json)
         ("PDF..." :command com-map-inspector-save-pdf)
         ("PNG..." :command com-map-inspector-save-png)))

(clim:define-command-table map-inspector-print-to-menu
  :menu ())

(clim:define-command-table map-inspector-copy-as-menu
  :menu (("Text" :command com-map-inspector-copy-text)
         ("JSON" :command com-map-inspector-copy-json)
         ("PNG" :command com-map-inspector-copy-png)))

(clim:define-command-table map-inspector-file-menu
  :menu (("New Map..." :command com-map-inspector-new)
         ("Import TMX..." :command com-map-inspector-import-tmx)
         ("Import World..." :command com-map-inspector-import-world)
         (nil :divider :line)
         ("Go To Map..." :command com-map-inspector-go-to)
         (nil :divider :line)
         ("Save As" :menu map-inspector-save-as-menu)
         ("Print To" :menu map-inspector-print-to-menu)
         (nil :divider :line)
         ("Close" :command com-map-inspector-close)))

(clim:define-command-table map-inspector-edit-menu
  :menu (("Copy As" :menu map-inspector-copy-as-menu)))

(clim:define-command-table map-inspector-help-menu
  :menu (("How to Edit Maps..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-scripting-guide)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table map-inspector-menu-bar
  :menu (("Map" :menu map-inspector-file-menu)
         ("Edit" :menu map-inspector-edit-menu)
         ("Help" :menu map-inspector-help-menu)))

;; --- Display ---

(clim:define-presentation-type map-inspector-tileset-ref () :inherit-from 'string)
(clim:define-presentation-type map-inspector-spawn-ref () :inherit-from 'string)
(clim:define-presentation-type map-inspector-script-ref () :inherit-from 'string)
(clim:define-presentation-type map-inspector-song-ref () :inherit-from 'string)

(clim:define-command (com-map-inspector-open-song :command-table clim-internals::global-command-table
                                                :menu nil :name t)
    ((song 'map-inspector-song-ref :gesture :select))
  (let ((path (format nil "../Source/Songs/~a.mscz" song)))
    (let ((choice (clim:menu-choose
                   `(("Open in MuseScore" open)
                     ("Export as MIDI" midi)
                     ("Export as Ogg" ogg))
                   :label song)))
      (case choice
        (open (uiop:run-program (list "musescore" path) :output nil :ignore-error-status t))
        (midi (uiop:run-program (list "musescore" path "-o" (format nil "/tmp/~a.mid" song))
                                :output nil :ignore-error-status t)
              (uiop:run-program (list "xdg-open" (format nil "/tmp/~a.mid" song))
                                :output nil :ignore-error-status t))
        (ogg (uiop:run-program (list "musescore" path "-o" (format nil "/tmp/~a.ogg" song))
                                :output nil :ignore-error-status t)
             (uiop:run-program (list "xdg-open" (format nil "/tmp/~a.ogg" song))
                               :output nil :ignore-error-status t))))))

(clim:define-command (com-map-inspector-open-tileset :command-table clim-internals::global-command-table
                                                   :menu nil :name t)
    ((ts 'map-inspector-tileset-ref :gesture :select))
  (let ((choice (clim:menu-choose
                 `(("Open in GIMP" gimp)
                   ("Open in Tiled" tiled))
                 :label (enough-namestring (pathname ts)))))
    (case choice
      (gimp (uiop:run-program (list "gimp" ts) :output nil :ignore-error-status t))
      (tiled (uiop:run-program (list "tiled" ts) :output nil :ignore-error-status t)))))

(clim:define-command (com-map-inspector-open-spawn :command-table clim-internals::global-command-table
                                                 :menu nil :name t)
    ((spawn 'map-inspector-spawn-ref :gesture :select))
  (let ((choice (clim:menu-choose
                 `(("Edit in Skyline-Tool Editor" skyline)
                   ("Open JSON in Emacs" emacs))
                 :label spawn)))
    (case choice
      (skyline (format *query-io* "~&Open ~a in Character Editor: not yet implemented.~%" spawn))
      (emacs (uiop:run-program (list "emacsclient" "-n"
                                     (format nil "../Source/Objects/~a.json" spawn))
                               :output nil :ignore-error-status t)))))

(clim:define-command (com-map-inspector-open-script :command-table clim-internals::global-command-table
                                                  :menu nil :name t)
    ((script 'map-inspector-script-ref :gesture :select))
  (let ((path (format nil "../Source/Scripts/~a.fountain" script)))
    (let ((choice (clim:menu-choose
                   `(("Open in Emacs" emacs)
                     ("Open in ThiefMD" thiefmd)
                     ("Export as PDF" pdf))
                   :label script)))
      (case choice
        (emacs (uiop:run-program (list "emacsclient" "-n" path)
                                 :output nil :ignore-error-status t))
        (thiefmd (uiop:run-program (list "thiefmd" path)
                                   :output nil :ignore-error-status t))
        (pdf (format *query-io* "~&Export Script as PDF: not yet implemented.~%"))))))

(defmethod display-inspector-content ((frame map-inspector-frame) pane)
  (display-map-inspector frame pane))

(defun display-map-inspector (frame pane)
  (let* ((data (map-inspector-frame-data frame))
         (stream pane))
    (unless data
      (format pane "~&No map data loaded.")
      (return-from display-map-inspector))
    (let* ((display-name (map-inspector-data-display-name data))
           (suffix (or (map-inspector-new-suffix frame)
                       (map-inspector-data-numeric-suffix data))))
      ;; Header: map name
      (clim:with-text-style (stream (clim:make-text-style :sans-serif :bold :larger))
        (format stream "~&~a" display-name)
        (clim:with-drawing-options (stream :ink (clim:make-gray-color 0.25))
          (format stream "~a" suffix)))
      (terpri stream) (terpri stream)
      ;; Dimensions
      (clim:with-text-style (stream (clim:make-text-style :sans-serif :bold :normal))
        (format stream "~&Dimensions:  "))
      (format stream "~d × ~d tiles" (map-inspector-data-width data) (map-inspector-data-height data))
      (terpri stream) (terpri stream)
      ;; Tilesets
      (clim:with-text-style (stream (clim:make-text-style :sans-serif :bold :normal))
        (format stream "~&Tilesets:"))
      (dolist (ts (map-inspector-data-tilesets data))
        (destructuring-bind (firstgid . source) ts
          (format stream "~&  ")
          (clim:with-output-as-presentation (stream source 'map-inspector-tileset-ref)
            (format stream "GID ~d: ~a" firstgid source))))
      (terpri stream)
      ;; Properties — BGM is a resource presentation
      (let ((props (map-inspector-data-properties data)))
        (when props
          (terpri stream)
          (clim:with-text-style (stream (clim:make-text-style :sans-serif :bold :normal))
            (format stream "~&Properties:"))
          (loop for (key value) on props by #'cddr
                do (if (eq key :bgm)
                       (progn
                         (format stream "~&  BGM = ")
                         (clim:with-output-as-presentation (stream value 'map-inspector-song-ref)
                           (princ value stream))
                         (terpri stream))
                       (format stream "~&  ~a = ~a"
                               (string-downcase (symbol-name key)) value)))))
      (terpri stream)
      ;; Spawns
      (let ((spawns (map-inspector-data-spawns data)))
        (when spawns
          (clim:with-text-style (stream (clim:make-text-style :sans-serif :bold :normal))
            (format stream "~&Spawns:"))
          (dolist (spawn spawns)
            (destructuring-bind (name kind x y) spawn
              (format stream "~&  ")
              (clim:with-output-as-presentation (stream name 'map-inspector-spawn-ref)
                (format stream "~a (~a) @ (~d, ~d)" name kind (floor x) (floor y)))))))
      (terpri stream)
      ;; Run Commands
      (let ((rcs (map-inspector-data-run-commands data)))
        (when rcs
          (clim:with-text-style (stream (clim:make-text-style :sans-serif :bold :normal))
            (format stream "~&Run Commands:"))
          (dolist (rc rcs)
            (format stream "~&  ")
            (clim:with-output-as-presentation (stream rc 'map-inspector-script-ref)
              (princ rc stream)))))
      ;; Entrances
      (let ((entrances (map-inspector-data-entrances data)))
        (when entrances
          (terpri stream)
          (clim:with-text-style (stream (clim:make-text-style :sans-serif :bold :normal))
            (format stream "~&Entrances:"))
          (dolist (ent entrances)
            (destructuring-bind (name x y) ent
              (format stream "~&  “~a” @ (~d, ~d)" name (floor x) (floor y))))))
      ;; Exits
      (let ((exits (map-inspector-data-exits data)))
        (when exits
          (terpri stream)
          (clim:with-text-style (stream (clim:make-text-style :sans-serif :bold :normal))
            (format stream "~&Exits:"))
          (dolist (exit exits)
            (destructuring-bind (target x y) exit
              (format stream "~&  → ~a @ (~d, ~d)" target (floor x) (floor y))))))
      ;; Triggers
      (let ((triggers (map-inspector-data-triggers data)))
        (when triggers
          (terpri stream)
          (clim:with-text-style (stream (clim:make-text-style :sans-serif :bold :normal))
            (format stream "~&Triggers:"))
          (dolist (trig triggers)
            (destructuring-bind (script x y) trig
              (format stream "~&  ")
              (clim:with-output-as-presentation (stream script 'map-inspector-script-ref)
                (format stream "~a @ (~d, ~d)" script (floor x) (floor y))))))))))

;; --- Commands ---

(define-map-inspector-frame-command (com-map-inspector-save :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (data (map-inspector-frame-data frame))
         (old-tmx-path (map-editor-original-tmx-path frame))
         (new-suffix (map-inspector-new-suffix frame)))
    (unless data
      (format *query-io* "~&No map data loaded." )
      (return-from com-map-inspector-save))
    (unless (and new-suffix (plusp (length new-suffix)))
      (format *query-io* "~&No new suffix provided. Use Edit > Change Suffix first." )
      (return-from com-map-inspector-save))
    (let* ((old-name (pathname-name old-tmx-path))
           (old-stem (map-inspector-data-display-name data))
           (new-stem (concatenate 'string old-stem new-suffix))
           (old-dir (make-pathname :defaults old-tmx-path :name nil :type nil))
           (new-tmx-path (merge-pathnames (make-pathname :name new-stem :type "tmx") old-dir)))
      ;; Rename the TMX file
      (rename-file old-tmx-path new-tmx-path)
      (format *query-io* "~&Renamed “~a” → “~a”~%" (file-namestring old-tmx-path)
              (file-namestring new-tmx-path))
      ;; Scan fountain files for references
      (format *query-io* "~&Scanning scripts for references to “~a”…" old-stem)
      (force-output *query-io*)
      (clim-sys:make-process
       (lambda ()
         (let ((old-stem (map-inspector-data-display-name data))
               (references '()))
           (dolist (f (directory
                       (make-pathname :directory '(:relative "Source" "Scripts")
                                      :name :wild :type "fountain")))
             (with-open-file (stream f :external-format :utf-8)
               (let ((line-num 0)
                     (found '()))
                 (loop for line = (read-line stream nil nil)
                       while line
                       do (incf line-num)
                       when (search old-stem line :test #'char-equal)
                         do (push line-num found))
                 (when found
                   (push (list f (nreverse found)) references)))))
           (clim-simple-echo:run-in-simple-echo
            (lambda ()
              (format t "~&Rename Report for “~a” → “~a”~2%"
                      (file-namestring old-tmx-path) (file-namestring new-tmx-path))
              (if (null references)
                  (format t "~&No scripts reference “~a”.~%" old-stem)
                  (progn
                    (format t "~&The following scripts may need updating:~2%")
                    (dolist (ref references)
                      (destructuring-bind (file lines) ref
                        (format t "  ~a~%     Lines: ~{~d~^, ~}~%"
                                (enough-namestring file) lines)))))
              (terpri)
              (format t "~&--- End of Report ---~%"))
            :window-title (format nil "Map Rename: ~a → ~a"
                                  (file-namestring old-tmx-path)
                                  (file-namestring new-tmx-path))
            :width 600 :height 400))
       :name (format nil "Scan scripts for ~a" old-stem)))))

(define-map-inspector-frame-command (com-map-inspector-close :menu t :name t) ()
  (clim:frame-exit clim:*application-frame*))

(define-map-inspector-frame-command (com-map-inspector-copy-json :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (data (map-inspector-frame-data frame)))
    (unless data
      (format *query-io* "~&No map data.")
      (return-from com-map-inspector-copy-json))
    (let* ((plist (map-inspector-data-properties data))
           (prop-alist (loop for (k v) on plist by #'cddr
                             collect (cons (string-downcase (symbol-name k)) v)))
           (json-data `(("name" . ,(map-inspector-data-display-name data))
                        ("suffix" . ,(or (map-inspector-new-suffix frame)
                                         (map-inspector-data-numeric-suffix data)))
                        ("width" . ,(map-inspector-data-width data))
                        ("height" . ,(map-inspector-data-height data))
                        ("tilesets" .
                         ,(loop for (gid . src) in (map-inspector-data-tilesets data)
                                collect (list (cons "firstgid" gid)
                                              (cons "source" src))))
                        ("properties" . ,prop-alist)
                        ("spawns" .
                         ,(loop for (name kind x y) in (map-inspector-data-spawns data)
                                collect (list (cons "name" name)
                                              (cons "kind" (string-downcase kind))
                                              (cons "x" (floor x))
                                              (cons "y" (floor y)))))
                        ("runCommands" . ,(map-inspector-data-run-commands data))
                        ("entrances" .
                         ,(loop for (name x y) in (map-inspector-data-entrances data)
                                collect (list (cons "name" name)
                                              (cons "x" (floor x))
                                              (cons "y" (floor y)))))
                        ("exits" .
                         ,(loop for (target x y) in (map-inspector-data-exits data)
                                collect (list (cons "target" target)
                                              (cons "x" (floor x))
                                              (cons "y" (floor y)))))
                        ("triggers" .
                         ,(loop for (script x y) in (map-inspector-data-triggers data)
                                collect (list (cons "script" script)
                                              (cons "x" (floor x))
                                              (cons "y" (floor y)))))))
           (json-str (json:encode-json-to-string json-data)))
      (clim-simple-echo:run-in-simple-echo
       (lambda () (princ json-str))
       :window-title (format nil "JSON — ~a" (map-inspector-data-display-name data))
       :width 600 :height 400))))

(define-map-inspector-frame-command (com-map-inspector-copy-text :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (data (map-inspector-frame-data frame)))
    (unless data
      (format *query-io* "~&No map data.")
      (return-from com-map-inspector-copy-text))
    (let ((text (with-output-to-string (s)
                  (let* ((display-name (map-inspector-data-display-name data))
                         (suffix (or (map-inspector-new-suffix frame)
                                     (map-inspector-data-numeric-suffix data))))
                    (format s "Map: ~a~a~%" display-name suffix)
                    (format s "Dimensions: ~d × ~d~%"
                            (map-inspector-data-width data)
                            (map-inspector-data-height data))
                    (format s "~%Tilesets:~%")
                    (dolist (ts (map-inspector-data-tilesets data))
                      (destructuring-bind (gid . src) ts
                        (format s "  GID ~d: ~a~%" gid src)))
                    (let ((props (map-inspector-data-properties data)))
                      (when props
                        (format s "~%Properties:~%")
                        (loop for (k v) on props by #'cddr
                              do (format s "  ~a = ~a~%"
                                         (string-downcase (symbol-name k)) v))))
                    (let ((spawns (map-inspector-data-spawns data)))
                      (when spawns
                        (format s "~%Spawns:~%")
                        (dolist (sp spawns)
                          (destructuring-bind (name kind x y) sp
                            (format s "  ~a (~a) @ (~d, ~d)~%" name kind
                                    (floor x) (floor y))))))
                    (let ((rcs (map-inspector-data-run-commands data)))
                      (when rcs
                        (format s "~%Run Commands:~%")
                        (dolist (rc rcs)
                          (format s "  ~a~%" rc))))
                    (let ((entrances (map-inspector-data-entrances data)))
                      (when entrances
                        (format s "~%Entrances:~%")
                        (dolist (ent entrances)
                          (destructuring-bind (name x y) ent
                            (format s "  “~a” @ (~d, ~d)~%" name (floor x) (floor y))))))
                    (let ((exits (map-inspector-data-exits data)))
                      (when exits
                        (format s "~%Exits:~%")
                        (dolist (exit exits)
                          (destructuring-bind (target x y) exit
                            (format s "  → ~a @ (~d, ~d)~%" target (floor x) (floor y))))))
                    (let ((triggers (map-inspector-data-triggers data)))
                      (when triggers
                        (format s "~%Triggers:~%")
                        (dolist (trig triggers)
                          (destructuring-bind (script x y) trig
                            (format s "  ~a @ (~d, ~d)~%" script (floor x) (floor y))))))))))
      (clim-simple-echo:run-in-simple-echo
       (lambda () (princ text))
       :window-title (format nil "Text — ~a" (map-inspector-data-display-name data))
       :width 600 :height 400)))))

;; --- Change suffix command (interactive) ---

(define-map-inspector-frame-command (com-map-inspector-change-suffix :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (data (map-inspector-frame-data frame)))
    (let* ((current (or (map-inspector-new-suffix frame)
                        (map-inspector-data-numeric-suffix data) ""))
           (new (clim:accept 'string :prompt "New suffix" :default current
                             :history 'map-editor-suffix)))
      (when (and new (not (string= new current)))
        (setf (map-inspector-new-suffix frame) new)
        (clim:redisplay-frame-panes frame :force-p t)))))

;; --- Opening function ---

;; --- Stub commands for menu items ---

(define-map-inspector-frame-command (com-map-inspector-new :menu t :name t) ()
  (let* (;; Choose subdirectory from existing map folders (exclude Tiles, RunCommands)
         (map-dirs (sort (remove-if (lambda (d)
                                      (member d '("Tiles" "RunCommands") :test #'string-equal))
                                    (mapcar #'pathname-name
                                            (ignore-errors
                                              (directory #p"../Source/Maps/*/" :resolve-symlinks nil))))
                          #'string-lessp))
         (subdir (clim:accept `(member ,@map-dirs) :prompt "Map subdirectory" :default (first map-dirs)))
         ;; Display name in Title Case
         (display-name (clim:accept 'string :prompt "Map display name (Title Case)" :default "New Map"))
         (pascal-name (cl-change-case:title-case display-name))
         (clean-name (remove-if (lambda (c) (find c " .,;:!?/-_'")) pascal-name))
         ;; Find lowest unused map ID
         (read-assets-list)
         (all-maps (loop for k being the hash-keys of *assets-list*
                         when (search "Maps/" k :test #'char-equal)
                         collect k))
         (used-ids (sort (loop for m in all-maps
                               for parts = (split-sequence #\/ m)
                               for last = (car (last parts))
                               for num = (parse-integer last :junk-allowed t)
                               when num collect num)
                          #'<))
         (new-id (loop for i from 1
                       unless (member i used-ids)
                       return i))
         (moniker (format nil "Maps/~a/~a~d" subdir clean-name new-id))
         ;; Read tileset names from Tilesets.txt or scan tiles directory
         (port-name (or (ignore-errors (machine-directory-name)) "7800"))
         (tsx-names (or (ignore-errors
                         (with-open-file (f (merge-pathnames
                                             (format nil "Source/Tables/Tilesets.~a.txt" port-name)
                                             (uiop:getcwd)))
                           (loop for line = (read-line f nil nil)
                                 while line
                                 for trimmed = (string-trim '(#\Space #\Tab) line)
                                 for parts = (split-sequence #\Tab trimmed)
                                 when (>= (length parts) 2)
                                 collect (string-trim " " (second parts)))))
                        (ignore-errors
                         (mapcar #'pathname-name
                                 (directory #p"../Source/Maps/Tiles/*.tsx")))))
         (tileset (when tsx-names
                    (clim:accept `(member ,@tsx-names) :prompt "Tileset" :default (first tsx-names))))
         (tmx-name (format nil "~a~d.tmx" clean-name new-id))
         (tmx-dir (merge-pathnames
                   (make-pathname :directory (list :relative "Source" "Maps" subdir))
                   (uiop:getcwd))))
    (ensure-directories-exist tmx-dir)
    (let ((tmx-path (merge-pathnames tmx-name tmx-dir)))
      (unless (probe-file tmx-path)
        ;; Create one-screen TMX (standard 7800 screen: 40×24 tiles at 8×8 = 20×12 at 16×16)
        (with-open-file (f tmx-path :direction :output :if-exists :error)
          (format f "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
          (format f "<map version=\"1.11\" orientation=\"orthogonal\" renderorder=\"right-down\"~%")
          (format f "  width=\"20\" height=\"12\" tilewidth=\"16\" tileheight=\"16\" infinite=\"0\">~%")
          (format f "  <tileset firstgid=\"1\" source=\"../Tiles/~a.tsx\"/>~%" tileset)
          (format f "  <layer id=\"1\" name=\"Ground\" width=\"20\" height=\"12\">~%")
          (format f "    <data encoding=\"base64\">~%")
          (format f "    </data>~%")
          (format f "  </layer>~%")
          (format f "</map>~%"))
        ;; Add to Assets.index
        (setf (gethash moniker *assets-list*) (list "D" "P" "AA"))
        (write-sorted-assets-index)
        (format *query-io* "~&Created ~a (ID ~d). Opening in Tiled...~%" (namestring tmx-path) new-id)
        ;; Open in Tiled
        (uiop:run-program (list "tiled" (namestring (truename tmx-path)))
                          :output nil :ignore-error-status t)))))

(define-map-inspector-frame-command (com-map-inspector-import-tmx :menu t :name t) ()
  (let ((path (clim:accept 'pathname :prompt "Select TMX file" :default #p"*.tmx")))
    (when (and path (probe-file path))
      (open-map-inspector (namestring (truename path))))))

(define-map-inspector-frame-command (com-map-inspector-import-world :menu t :name t) ()
  (error "Not Yet Implemented"))

(define-map-inspector-frame-command (com-map-inspector-go-to :menu t :name t) ()
  (let* ((all-assets (collect-all-assets))
         (maps (remove-if-not (lambda (e) (string-equal (third e) "Maps")) all-assets))
         (map-names (mapcar #'first maps))
         (choice (clim:accept `(member ,@map-names) :prompt "Go to map")))
    (let* ((entry (find choice maps :key #'first :test #'equal))
           (path (and entry (seventh entry))))
      (if (and path (probe-file path))
          (open-map-inspector path)
          (format *query-io* "~&Map ~a not found on disk.~%" choice)))))

(define-map-inspector-frame-command (com-map-inspector-save-text :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (data (map-inspector-frame-data frame)))
    (unless data
      (format *query-io* "~&No map data loaded.")
      (return-from com-map-inspector-save-text))
    (let ((text (with-output-to-string (s)
                  (let* ((display-name (map-inspector-data-display-name data))
                         (suffix (or (map-inspector-new-suffix frame)
                                     (map-inspector-data-numeric-suffix data))))
                    (format s "Map: ~a~a~%" display-name suffix)
                    (format s "Dimensions: ~d × ~d~%"
                            (map-inspector-data-width data)
                            (map-inspector-data-height data))
                    (format s "~%Tilesets:~%")
                    (dolist (ts (map-inspector-data-tilesets data))
                      (destructuring-bind (gid . src) ts
                        (format s "  GID ~d: ~a~%" gid src)))
                    (let ((props (map-inspector-data-properties data)))
                      (when props
                        (format s "~%Properties:~%")
                        (loop for (k v) on props by #'cddr
                              do (format s "  ~a = ~a~%"
                                         (string-downcase (symbol-name k)) v))))
                    (let ((spawns (map-inspector-data-spawns data)))
                      (when spawns
                        (format s "~%Spawns:~%")
                        (dolist (sp spawns)
                          (destructuring-bind (name kind x y) sp
                            (format s "  ~a (~a) @ (~d, ~d)~%" name kind
                                    (floor x) (floor y))))))
                    (let ((rcs (map-inspector-data-run-commands data)))
                      (when rcs
                        (format s "~%Run Commands:~%")
                        (dolist (rc rcs)
                          (format s "  ~a~%" rc))))
                    (let ((entrances (map-inspector-data-entrances data)))
                      (when entrances
                        (format s "~%Entrances:~%")
                        (dolist (ent entrances)
                          (destructuring-bind (name x y) ent
                            (format s "  \"~a\" @ (~d, ~d)~%" name (floor x) (floor y))))))
                    (let ((exits (map-inspector-data-exits data)))
                      (when exits
                        (format s "~%Exits:~%")
                        (dolist (exit exits)
                          (destructuring-bind (target x y) exit
                            (format s "  → ~a @ (~d, ~d)~%" target (floor x) (floor y))))))
                    (let ((triggers (map-inspector-data-triggers data)))
                      (when triggers
                        (format s "~%Triggers:~%")
                        (dolist (trig triggers)
                          (destructuring-bind (script x y) trig
                            (format s "  ~a @ (~d, ~d)~%" script (floor x) (floor y)))))))))
          (path (prompt-save-pathname
                 (format nil "~a.txt" (map-inspector-data-display-name data))
                 :type "txt")))
      (when path
        (with-open-file (f path :direction :output :if-exists :supersede
                            :external-format :utf-8)
          (princ text f))
        (format *query-io* "~&Saved ~a (~d bytes).~%" (namestring path) (length text))))))

(define-map-inspector-frame-command (com-map-inspector-save-json :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (data (map-inspector-frame-data frame)))
    (unless data
      (format *query-io* "~&No map data loaded.")
      (return-from com-map-inspector-save-json))
    (let* ((plist (map-inspector-data-properties data))
           (prop-alist (loop for (k v) on plist by #'cddr
                              collect (cons (string-downcase (symbol-name k)) v)))
           (json-data `(("name" . ,(map-inspector-data-display-name data))
                         ("suffix" . ,(or (map-inspector-new-suffix frame)
                                          (map-inspector-data-numeric-suffix data)))
                         ("width" . ,(map-inspector-data-width data))
                         ("height" . ,(map-inspector-data-height data))
                         ("tilesets" .
                          ,(loop for (gid . src) in (map-inspector-data-tilesets data)
                                 collect (list (cons "firstgid" gid)
                                               (cons "source" src))))
                         ("properties" . ,prop-alist)
                         ("spawns" .
                          ,(loop for (name kind x y) in (map-inspector-data-spawns data)
                                 collect (list (cons "name" name)
                                               (cons "kind" (string-downcase kind))
                                               (cons "x" (floor x))
                                               (cons "y" (floor y)))))
                         ("runCommands" . ,(map-inspector-data-run-commands data))
                         ("entrances" .
                          ,(loop for (name x y) in (map-inspector-data-entrances data)
                                 collect (list (cons "name" name)
                                               (cons "x" (floor x))
                                               (cons "y" (floor y)))))
                         ("exits" .
                          ,(loop for (target x y) in (map-inspector-data-exits data)
                                 collect (list (cons "target" target)
                                               (cons "x" (floor x))
                                               (cons "y" (floor y)))))
                         ("triggers" .
                          ,(loop for (script x y) in (map-inspector-data-triggers data)
                                 collect (list (cons "script" script)
                                               (cons "x" (floor x))
                                               (cons "y" (floor y)))))))
           (json-str (json:encode-json-to-string json-data))
           (path (prompt-save-pathname
                  (format nil "~a.json" (map-inspector-data-display-name data))
                  :type "json")))
      (when path
        (with-open-file (f path :direction :output :if-exists :supersede
                            :external-format :utf-8)
          (princ json-str f))
        (format *query-io* "~&Saved ~a (~d bytes).~%" (namestring path) (length json-str))))))

(define-map-inspector-frame-command (com-map-inspector-save-pdf :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (data (map-inspector-frame-data frame)))
    (unless data
      (format *query-io* "~&No map data loaded.")
      (return-from com-map-inspector-save-pdf))
    (let* ((text (with-output-to-string (s)
                   (let* ((display-name (map-inspector-data-display-name data))
                          (suffix (or (map-inspector-new-suffix frame)
                                      (map-inspector-data-numeric-suffix data))))
                     (format s "Map: ~a~a~%" display-name suffix)
                     (format s "Dimensions: ~d × ~d~%"
                             (map-inspector-data-width data)
                             (map-inspector-data-height data))
                     (format s "~%Tilesets:~%")
                     (dolist (ts (map-inspector-data-tilesets data))
                       (destructuring-bind (gid . src) ts
                         (format s "  GID ~d: ~a~%" gid src)))
                     (let ((props (map-inspector-data-properties data)))
                       (when props
                         (format s "~%Properties:~%")
                         (loop for (k v) on props by #'cddr
                               do (format s "  ~a = ~a~%"
                                          (string-downcase (symbol-name k)) v))))
                     (let ((spawns (map-inspector-data-spawns data)))
                       (when spawns
                         (format s "~%Spawns:~%")
                         (dolist (sp spawns)
                           (destructuring-bind (name kind x y) sp
                             (format s "  ~a (~a) @ (~d, ~d)~%" name kind
                                     (floor x) (floor y))))))
                     (let ((rcs (map-inspector-data-run-commands data)))
                       (when rcs
                         (format s "~%Run Commands:~%")
                         (dolist (rc rcs)
                           (format s "  ~a~%" rc))))
                     (let ((entrances (map-inspector-data-entrances data)))
                       (when entrances
                         (format s "~%Entrances:~%")
                         (dolist (ent entrances)
                           (destructuring-bind (name x y) ent
                             (format s "  \"~a\" @ (~d, ~d)~%" name (floor x) (floor y))))))
                     (let ((exits (map-inspector-data-exits data)))
                       (when exits
                         (format s "~%Exits:~%")
                         (dolist (exit exits)
                           (destructuring-bind (target x y) exit
                             (format s "  → ~a @ (~d, ~d)~%" target (floor x) (floor y))))))
                     (let ((triggers (map-inspector-data-triggers data)))
                       (when triggers
                         (format s "~%Triggers:~%")
                         (dolist (trig triggers)
                           (destructuring-bind (script x y) trig
                             (format s "  ~a @ (~d, ~d)~%" script (floor x) (floor y)))))))))
           (path (prompt-save-pathname
                  (format nil "~a.pdf" (map-inspector-data-display-name data))
                  :type "pdf")))
      (when path
        (let* ((base (pathname-name path))
               (dir (make-pathname :defaults path :name nil :type nil))
               (ps-path (merge-pathnames (make-pathname :name base :type "ps") dir))
               (pdf-final (merge-pathnames (make-pathname :name base :type "pdf") dir))
               (lines (with-input-from-string (s text)
                        (loop for l = (read-line s nil nil) while l count l)))
               (lines-per-page (max 1 (floor (- 680 80) 10)))
               (total-pages (max 1 (+ (ceiling lines lines-per-page) (if data 1 0))))
               (title (format nil "Map Report: ~a" (map-inspector-data-display-name data)))
               (author (ignore-errors (user-real-name)))
               (hostname (machine-instance))
               (date-str (multiple-value-bind (s m h d mo y) (get-decoded-time)
                           (declare (ignore s))
                           (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" y mo d h m)))
               (game-title (string-capitalize
                            (or (ignore-errors (symbol-value '*game-title*)) "Unknown"))))
          (with-open-file (ps ps-path :direction :output :if-exists :supersede
                                       :external-format :utf-8)
            (format ps "%!PS-Adobe-3.0~%")
            (write-ps-docinfo ps title "Skyline-Tool"
                              (format nil "~a on ~a" author hostname))
            (format ps "<< /PageSize [612 792] >> setpagedevice~%")
            (write-ps-font-encodings ps)
            ;; Page 1: Map tile rendering
            (format ps "%%Page: 1 ~d~%" total-pages)
            (write-ps-header-bar ps (format nil "~a Map" game-title) date-str author game-title)
            (multiple-value-bind (mw mh mrgb) (%render-map-tiles data)
              (when mrgb
                (let* ((max-w 460) (max-h 600)
                       (scale (min (/ max-w mw) (/ max-h mh) 1.0))
                       (dw (floor (* mw scale))) (dh (floor (* mh scale))))
                  (write-ps-image ps mrgb mw mh dw dh))))
            ;; Overlay spawn/entrance/exit labels
            (format ps "/Times-Roman-ISOLatin1 findfont 6 scalefont setfont 1 0 0 setrgbcolor~%")
            (dolist (sp (map-inspector-data-spawns data))
              (destructuring-bind (name kind x y) sp
                (declare (ignore kind))
                (format ps "~d ~d moveto (~a) show~%"
                        (+ 56 (floor (* x 16))) (- 680 (floor (* y 16)))
                        (escape-ps-string name))))
            (format ps "0 0 1 setrgbcolor~%")
            (dolist (ent (map-inspector-data-entrances data))
              (destructuring-bind (name x y) ent
                (format ps "~d ~d moveto (E:~a) show~%"
                        (+ 56 (floor (* x 16))) (- 680 (floor (* y 16)))
                        (escape-ps-string name))))
            (format ps "0.5 0 0.5 setrgbcolor~%")
            (dolist (exit (map-inspector-data-exits data))
              (destructuring-bind (target x y) exit
                (format ps "~d ~d moveto (X:~a) show~%"
                        (+ 56 (floor (* x 16))) (- 680 (floor (* y 16)))
                        (escape-ps-string target))))
            (write-ps-page-footer ps 1 total-pages game-title date-str author hostname)
            (format ps "showpage~%")
            ;; Remaining pages: text report
            (with-input-from-string (s text)
              (dotimes (page (1- total-pages))
                (let ((page-num (+ 2 page)))
                  (format ps "%%Page: ~d ~d~%" page-num total-pages)
                  (write-ps-header-bar ps title date-str author game-title)
                  (format ps "/Times-Roman-ISOLatin1 findfont 9 scalefont setfont 0 0 0 setrgbcolor~%")
                  (let ((y 680) (line-height 10))
                    (loop for line = (read-line s nil nil)
                          while (and line (>= y 80))
                          do (format ps "50 ~d moveto (~a) show~%" y
                                     (escape-ps-string line))
                             (decf y line-height)))
                  (write-ps-page-footer ps page-num total-pages game-title date-str author hostname)
                  (format ps "showpage~%")))))
          (uiop:run-program (list "ps2pdf" (namestring ps-path) (namestring pdf-final))
                            :output nil :ignore-error-status t)
          (ignore-errors (delete-file ps-path))
          (format *query-io* "~&Saved ~a~%" (namestring pdf-final))
          (uiop:run-program (list "xdg-open" (namestring pdf-final))
                            :output nil :ignore-error-status t))))))

(define-map-inspector-frame-command (com-map-inspector-save-png :menu t :name t) ()
  (error "Not Yet Implemented"))

(define-map-inspector-frame-command (com-map-inspector-copy-png :menu t :name t) ()
  (error "Not Yet Implemented"))

(defun open-map-inspector (tmx-path &key locale)
  (let* ((fm (clim:find-frame-manager :port (or (clim:find-port)
                                                  (clim:find-port :server-path :x))))
         (resource (make-instance 'game-resource-map
                                  :moniker (pathname-name tmx-path)
                                 
                                  :locale (or locale :en)
                                  :full-path (truename tmx-path)))
         (frame (clim:make-application-frame 'map-inspector-frame
                                              :resource resource
                                              :tmx-path tmx-path
                                              :frame-manager fm
                                              :width 620 :height 820)))
    (clim-sys:make-process
     (lambda ()
       (let ((clim:*application-frame* frame))
         (handler-case
             (let ((data (parse-tmx-file tmx-path)))
               (setf (map-inspector-frame-data frame) data)
               (setf (map-editor-original-tmx-path frame) (truename tmx-path))
               (clim:run-frame-top-level frame))
           (error (e)
             (format *query-io* "~&Map Editor error: ~a~%" e)))))
     :name (format nil "Map Editor: ~a" (pathname-name tmx-path)))))
