(in-package :skyline-tool)

(defun compile-art-sms (index-out index-in)
  "Compile art assets for Sega Master System platform"
  (let ((*machine* 3010))
    (write-sms-chr-rom index-out
                       (parse-into-sms-chr-data
                        (read-sms-art-index index-in)))))

(defun read-sms-art-index (index-in)
  "Read SMS art index file and return list of (png-name mode width-px height-px)"
  (let ((png-list (list)))
    (format *trace-output* "~&~A: reading SMS art index …" (enough-namestring index-in))
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

(defun parse-into-sms-chr-data (art-index)
  "Parse PNG files into SMS CHR data (8x8 tiles)"
  (let ((chr-data (list)))
    (dolist (art-item art-index)
      (destructuring-bind (mode png-name width-px height-px) art-item
        (format *trace-output* "~&~A: parsing SMS CHR data … "
                png-name)
        (let* ((png (png-read:read-png-file png-name))
               (height (png-read:height png))
               (width (png-read:width png))
               (palette-pixels (png->palette (png-read:image-data png)
                                             (png-read:transparency png))))
          ;; SMS CHR format: 8x8 tiles, 4 colors (2 bits per pixel)
          (let ((tile-data (parse-sms-chr-tiles palette-pixels width-px height-px)))
            (setf chr-data (append chr-data tile-data))))
        (format *trace-output* " done.")))
    chr-data))

(defun parse-sms-chr-tiles (palette-pixels width-px height-px)
  "Convert palette pixels to SMS CHR ROM format (8x8 tiles, 2 bits per pixel)"
  (let ((tiles (list))
        (tile-width (/ width-px 8))
        (tile-height (/ height-px 8)))
    ;; Process each 8x8 tile
    (dotimes (ty tile-height)
      (dotimes (tx tile-width)
        (let ((tile-bytes (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
          ;; SMS uses 4 bitplanes (2 bits per pixel)
          (dotimes (y 8)
            (dotimes (x 8)
              (let* ((px (+ (* tx 8) x))
                     (py (+ (* ty 8) y))
                     (color-index (if (and (< px width-px) (< py height-px))
                                      (mod (aref palette-pixels py px) 4)
                                      0))
                     (bit0 (if (logbitp 0 color-index) 1 0))
                     (bit1 (if (logbitp 1 color-index) 1 0)))
                ;; Set bits in the bitplanes
                (when (= bit0 1)
                  (setf (aref tile-bytes y)
                        (logior (aref tile-bytes y) (ash 1 (- 7 x)))))
                (when (= bit1 1)
                  (setf (aref tile-bytes (+ y 16))
                        (logior (aref tile-bytes (+ y 16)) (ash 1 (- 7 x))))))))
          (push tile-bytes tiles))))
    (nreverse tiles)))

(defun write-sms-chr-rom (index-out chr-data)
  "Write SMS CHR ROM data to binary file"
  (with-output-to-file (out index-out :element-type '(unsigned-byte 8)
                                      :if-exists :supersede)
    (dolist (tile chr-data)
      (dotimes (i 32)
        (write-byte (aref tile i) out))))
  (format *trace-output* "~&Wrote ~:D bytes to ~A" (* (length chr-data) 32) index-out))

;; Apple //gs Art Compilation Interface

(defmethod compile-art-generic ((machine-type (eql 222)) format source-file-base-name art-input)
  "Compile art for Apple //gs platform"
  (declare (ignore format art-input))
  (let ((*machine* 222))
    (compile-atari-8×8 source-file-base-name #p"2gs/Fonts/" 8 8)))

