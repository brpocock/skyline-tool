(in-package :skyline-tool)

(defun read-lynx-art-index (index-in)
  "Read Lynx art index file and return list of (png-name mode width-px height-px)"
  (let ((png-list (list)))
    (format *trace-output* "~&~A: reading Lynx art index …" (enough-namestring index-in))
    (with-input-from-file (index index-in)
      (loop for line = (read-line index nil)
            while (and line (plusp (length line)) (not (char= #\; (char line 0))))
            do (let ((line (string-trim #(#\Space #\Tab #\Newline #\Return #\Page) line)))
                 (cond
                   ((emptyp line) nil)
                   ((char= #\# (char line 0)) nil)
                   (t (destructuring-bind (png-name mode cell-size)
                          (split-sequence #\Space line :remove-empty-subseqs t :test #'char=)
                        (destructuring-bind (width-px height-px)
                            (split-sequence #\× cell-size :test #'char=)
                          (push (list (make-keyword mode)
                                      (make-pathname :defaults index-in
                                                     :name (subseq png-name 0
                                                                   (position #\. png-name :from-end t))
                                                     :type "png")
                                      (parse-integer width-px)
                                      (parse-integer height-px))
                                png-list))))))))
    (format *trace-output* " done. Got ~:D PNG files to read." (length png-list))
    (reverse png-list)))

(defun parse-into-lynx-chr-data (art-index)
  "Parse PNG files into Lynx CHR data (sprites with 16-color palette)"
  (let ((chr-data (list)))
    (dolist (art-item art-index)
      (destructuring-bind (mode png-name width-px height-px) art-item
        (format *trace-output* "~&~A: parsing Lynx CHR data … "
                png-name)
        (let* ((png (png-read:read-png-file png-name))
               (height (png-read:height png))
               (width (png-read:width png))
               (palette-pixels (png->palette (png-read:image-data png)
                                             (png-read:transparency png))))
          ;; Lynx CHR format: sprites with 16 colors per sprite
          (let ((sprite-data (parse-lynx-sprite-data palette-pixels width-px height-px mode)))
            (setf chr-data (append chr-data sprite-data))))
        (format *trace-output* " done.")))
    chr-data))

(defun parse-lynx-sprite-data (palette-pixels width-px height-px mode)
  "Parse palette pixels into Lynx sprite data format"
  (declare (ignore mode)) ;; For now, ignore mode - could be SPRITE, TILE, etc.
  ;; Lynx sprites use 16 colors (4-bit indices)
  ;; Convert the palette pixels to 4-bit indices
  (let ((sprite-data (list)))
    (dotimes (y height-px)
      (dotimes (x width-px)
        (let ((color-index (aref palette-pixels y x)))
          ;; Lynx uses 4-bit color indices (0-15)
          (push (logand color-index #x0F) sprite-data))))
    ;; Return as byte array (reversed since we pushed)
    (reverse sprite-data)))

(defun write-lynx-chr-rom (index-out chr-data)
  "Write Lynx CHR ROM data to binary file"
  (with-output-to-file (out index-out :element-type '(unsigned-byte 8)
                                      :if-exists :supersede)
    (dolist (sprite chr-data)
      (dotimes (i (length sprite))
        (write-byte (aref sprite i) out))))
  (format *trace-output* "~&Wrote ~:D bytes to ~A"
          (reduce #'+ (mapcar #'length chr-data)) index-out))

(defun compile-art-lynx (index-out index-in)
  "Compile art assets for Atari Lynx platform"
  (let ((*machine* 200))
    (write-lynx-chr-rom index-out
                        (parse-into-lynx-chr-data
                         (read-lynx-art-index index-in)))))

;; Font compilation for Lynx

(defmethod compile-font-generic ((machine-type (eql 200)) format source-file-base-name font-input)
  "Compile font for Atari Lynx platform"
  (let ((*machine* 200))
    (declare (ignore format)) ;; Lynx uses a standard font format
    (compile-font-8×8 source-file-base-name "Lynx/Fonts" 8 8
                      (png->palette font-input nil))))

;; Blob ripping for Lynx

(defun blob-rip-lynx-sprite (png-file)
  "Dispatch Lynx sprite PNG through dispatch-png for auto-detection."
  (let ((*machine* 200)
        (out-dir (merge-pathnames (make-pathname :directory '(:relative "Source" "Generated" "Lynx" "Assets"))
                                  (or (uiop:getcwd) (uiop:getcwd)))))
    (dispatch-png png-file out-dir)))

(defun blob-rip-lynx-tile (png-file)
  "Dispatch Lynx tile PNG through dispatch-png for auto-detection."
  (blob-rip-lynx-sprite png-file))

(defun blob-rip-lynx-font (png-file)
  "Dispatch Lynx font PNG through dispatch-png for auto-detection."
  (blob-rip-lynx-sprite png-file))
