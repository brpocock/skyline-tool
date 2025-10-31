(in-package :skyline-tool/test)

(def-suite chaos-character-compilation :in skyline-tool)
(in-suite chaos-character-compilation)

(defun create-test-palette-array (width height &key (pattern 1))
  "Create a test palette pixel array for testing."
  (let ((array (make-array (list width height) :initial-element 0)))
    (loop for y from 0 below height
          do (loop for x from 0 below width
                   do (setf (aref array x y) (if (evenp (+ x y)) pattern 0))))
    array))

(defun write-palette-array-to-png (palette-array png-file)
  "Write a palette array to a PNG file for testing.
   
   palette-array: (array width height) where 0 = transparent/black, non-zero = white
   Converts to RGB format: 0 -> (0,0,0), non-zero -> (255,255,255)"
  (let* ((width (array-dimension palette-array 0))
         (height (array-dimension palette-array 1))
         (rgb-array (make-array (list width height 3) 
                                :element-type '(unsigned-byte 8)
                                :initial-element 0)))
    ;; Convert palette array to RGB (0 -> black, non-zero -> white)
    (loop for x from 0 below width
          do (loop for y from 0 below height
                   do (let ((palette-value (aref palette-array x y)))
                        (if (zerop palette-value)
                            (progn
                              (setf (aref rgb-array x y 0) 0
                                    (aref rgb-array x y 1) 0
                                    (aref rgb-array x y 2) 0))
                            (progn
                              (setf (aref rgb-array x y 0) 255
                                    (aref rgb-array x y 1) 255
                                    (aref rgb-array x y 2) 255))))))
    ;; Write PNG using zpng if available, otherwise use png-read's write capability
    #+quicklisp
    (handler-case
        (progn
          (ql:quickload :zpng :silent t)
          (let ((png (make-instance 'zpng:png 
                                    :width width 
                                    :height height 
                                    :color-type :truecolor)))
            (loop for y from 0 below height
                  do (loop for x from 0 below width
                           do (zpng:set-pixel png x y 
                                               (aref rgb-array x y 0)
                                               (aref rgb-array x y 1)
                                               (aref rgb-array x y 2))))
            (zpng:write-png png png-file)))
      (error (e)
        (declare (ignore e))
        ;; Fallback: create minimal PNG manually (PNG signature + minimal IHDR)
        (with-open-file (out png-file 
                            :direction :output 
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
          ;; PNG signature
          (write-sequence #(#x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a) out)
          ;; Minimal IHDR chunk (simplified for testing)
          ;; For a proper fix, would need full PNG encoding, but this stub allows compilation
          ;; In practice, tests should use actual PNG files or proper PNG library
          (warn "PNG writing not fully implemented - test may need actual PNG files"))))
    #-quicklisp
    (warn "Cannot write PNG without Quicklisp/zpng - test needs actual PNG files")))

(test compile-chaos-character-no-byte-directive
  "Test that output doesn't contain .byte directives"
  (let* ((test-png (merge-pathnames #p"test-character.png" (uiop:temporary-directory)))
         (test-bas (merge-pathnames #p"test-character.bas" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           ;; Create minimal valid 64×256 test image
           (let ((test-pixels (create-test-palette-array 64 256 :pattern 1)))
             ;; Create PNG file from test pixels
             (write-palette-array-to-png test-pixels test-png)
             ;; Now compile the character sprite
             (skyline-tool::compile-chaos-character test-bas test-png)
             (let ((output (uiop:read-file-string test-bas)))
               (is (not (search ".byte" output))
                   "Output should not contain .byte directives")
               (is (search "%" output)
                   "Output should contain binary format (%...)"))))
      (when (probe-file test-bas)
        (delete-file test-bas))
      (when (probe-file test-png)
        (delete-file test-png)))))

(test compile-chaos-character-no-remarks-in-data
  "Test that remarks are not inside data blocks"
  (let* ((test-png (merge-pathnames #p"test-character.png" (uiop:temporary-directory)))
         (test-bas (merge-pathnames #p"test-character.bas" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           ;; Test that "rem" doesn't appear after "data" and before "end"
           (let ((test-pixels (create-test-palette-array 64 256 :pattern 1)))
             ;; Create PNG file from test pixels
             (write-palette-array-to-png test-pixels test-png)
             ;; Now compile the character sprite
             (skyline-tool::compile-chaos-character test-bas test-png)
             (let ((output (uiop:read-file-string test-bas))
                   (data-pos (search "data" output))
                   (end-pos (search "end" output :start2 (or data-pos 0))))
               (when (and data-pos end-pos)
                 (let ((data-block (subseq output data-pos end-pos)))
                   (is (not (search "rem" data-block))
                       "Data block should not contain remarks"))))))
      (when (probe-file test-bas)
        (delete-file test-bas))
      (when (probe-file test-png)
        (delete-file test-png)))))

(test compile-chaos-character-end-in-column1
  "Test that 'end' keyword is in column 1"
  (let* ((test-png (merge-pathnames #p"test-character.png" (uiop:temporary-directory)))
         (test-bas (merge-pathnames #p"test-character.bas" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (let ((test-pixels (create-test-palette-array 64 256 :pattern 1)))
             ;; Create PNG file from test pixels
             (write-palette-array-to-png test-pixels test-png)
             ;; Now compile the character sprite
             (skyline-tool::compile-chaos-character test-bas test-png)
             (let ((output (uiop:read-file-string test-bas))
                   (lines (uiop:split-string output :separator '(#\Newline))))
               (loop for line in lines
                     when (string= (string-trim " " line) "end")
                     do (is (char= (char line 0) #\e)
                            "end keyword should start at column 0")))))
      (when (probe-file test-bas)
        (delete-file test-bas))
      (when (probe-file test-png)
        (delete-file test-png)))))

(test compile-chaos-character-frame-mapping-format
  "Test that frame mapping is 8 per line, 16 lines"
  (let* ((test-png (merge-pathnames #p"test-character.png" (uiop:temporary-directory)))
         (test-bas (merge-pathnames #p"test-character.bas" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (let ((test-pixels (create-test-palette-array 64 256 :pattern 1)))
             ;; Create PNG file from test pixels
             (write-palette-array-to-png test-pixels test-png)
             ;; Now compile the character sprite
             (skyline-tool::compile-chaos-character test-bas test-png)
             (let ((output (uiop:read-file-string test-bas))
                   (framemap-start (search "data" output :from-end t))
                   (framemap-end (search "end" output :start2 framemap-start)))
               (when (and framemap-start framemap-end)
                 (let ((framemap-block (subseq output framemap-start framemap-end))
                       (lines (remove-if (lambda (s) (or (zerop (length s))
                                                          (string= (string-trim " " s) "data")
                                                          (string-prefix-p "rem" (string-trim " " s))))
                                         (uiop:split-string framemap-block :separator '(#\Newline)))))
                   ;; Should have 16 lines of frame mappings
                   (is (<= 16 (length lines))
                       "Should have at least 16 lines of frame mappings")
                   ;; Each line should have 8 comma-separated values
                   (loop for line in (subseq lines 0 (min 16 (length lines)))
                         do (let ((values (uiop:split-string line :separator '(#\,))))
                              (is (= (length values) 8)
                                  "Each line should have 8 comma-delimited values"))))))))
      (when (probe-file test-bas)
        (delete-file test-bas))
      (when (probe-file test-png)
        (delete-file test-png)))))

(test compile-chaos-character-data-block-names
  "Test that data blocks are named according to character name"
  (let* ((test-png (merge-pathnames #p"Bernie.png" (uiop:temporary-directory)))
         (test-bas (merge-pathnames #p"Bernie.bas" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (let ((test-pixels (create-test-palette-array 64 256 :pattern 1)))
             ;; Create PNG file from test pixels
             (write-palette-array-to-png test-pixels test-png)
             ;; Now compile the character sprite
             (skyline-tool::compile-chaos-character test-bas test-png)
             (let ((output (uiop:read-file-string test-bas)))
               (is (search "data BernieFrames" output)
                   "Should contain data block named BernieFrames")
               (is (search "data BernieFrameMap" output)
                   "Should contain data block named BernieFrameMap"))))
      (when (probe-file test-bas)
        (delete-file test-bas))
      (when (probe-file test-png)
        (delete-file test-png)))))

