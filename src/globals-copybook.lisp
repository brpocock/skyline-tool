(in-package :skyline-tool)

;;; 64tass assembly -> COBOL copybook generator
;;; Parses ZeroPage.s, SysRAM.s, CartRAM.s, Enums.s, Constants.s
;;; to produce Source/Generated/Classes/Phantasia-Globals.cpy.
;;;
;;; Variable annotation syntax (add to assembly source as needed):
;;;   Label: .byte ?  ; @ClassName          -> OBJECT REFERENCE ClassName
;;;   Label: .fill n, ? ; = PIC X(n)        -> explicit PIC clause
;;;   Label: .fill n, ? ; = VARCHAR(n) DEPENDING ON LenField -> varchar

(defparameter *sysram-section-labels*
  '(("SysRAMLow"  . "SYS-RAM-LOW")
    ("SysRAMMid"  . "SYS-RAM-MID")
    ("SysRAMHigh" . "SYS-RAM-HIGH"))
  "Assembly section labels and their COBOL 01-level names.")

(defun parse-asm-annotation (comment-text)
  "Parse an annotation from assembly trailing comment text (after the ;).
Returns NIL, (:object-ref cls), (:pic string), or (:varchar n f)."
  (let ((s (string-trim " " comment-text)))
    (cond
      ((and (> (length s) 1) (char= #\@ (char s 0)))
       (list :object-ref (string-trim " " (subseq s 1))))
      ((and (> (length s) 2)
            (char= #\= (char s 0))
            (member (char s 1) (list #\Space #\Tab)))
       (let ((spec (string-trim " " (subseq s 1))))
         (or (cl-ppcre:register-groups-bind (n field)
                 ("(?i)VARCHAR\\((\\d+)\\)\\s+DEPENDING\\s+ON\\s+(\\S+)" spec)
               (list :varchar (parse-integer n) field))
             (list :pic spec))))
      (t nil))))

(defun parse-asm-line (line)
  "Parse one 64tass source line.
Returns plist :name :kind :size :value :annotation :raw-size-sym, or NIL."
  (let* ((trimmed (string-right-trim " " line))
         (len     (length trimmed)))
    (when (or (zerop len) (char= #\; (char trimmed 0)))
      (return-from parse-asm-line nil))
    (let* ((semi-pos  (position #\; trimmed))
           (code-part (if semi-pos (subseq trimmed 0 semi-pos) trimmed))
           (comment   (if semi-pos
                          (string-trim " " (subseq trimmed (1+ semi-pos)))
                          ""))
           (annotation (parse-asm-annotation comment))
           (code       (string-trim " " code-part)))
      ;; Skip assembler directive lines (.if .fi .block etc.)
      (when (and (> (length code) 0) (char= #\. (char code 0)))
        (return-from parse-asm-line nil))
      ;; Label: .DIRECTIVE args
      (let ((m (nth-value 1 (cl-ppcre:scan-to-strings
                             "^([\\w.]+):\\s+\\.([A-Za-z]+)\\s*(.*?)\\s*$"
                             code))))
        (when m
          (let ((name  (aref m 0))
                (kw    (string-downcase (aref m 1)))
                (args  (aref m 2)))
            (cond
              ((string= kw "byte")
               (return-from parse-asm-line
                 (list :name name :kind :byte
                       :size (length (cl-ppcre:split "," args))
                       :annotation annotation)))
              ((string= kw "word")
               (return-from parse-asm-line
                 (list :name name :kind :word
                       :size (* 2 (length (cl-ppcre:split "," args)))
                       :annotation annotation)))
              ((string= kw "fill")
               (let* ((parts (mapcar (lambda (a) (string-trim " " a))
                                     (cl-ppcre:split "," args)))
                      (count-str (find-if (lambda (a) (string/= a "?")) parts))
                      (count-int (when count-str
                                   (ignore-errors
                                    (if (and (> (length count-str) 1)
                                             (char= #\$ (char count-str 0)))
                                        (parse-integer (subseq count-str 1) :radix 16)
                                        (parse-integer count-str))))))
                 (return-from parse-asm-line
                   (list :name name :kind :fill
                         :size (or count-int 1)
                         :raw-size-sym (unless count-int count-str)
                         :annotation annotation))))
              (t nil)))))
      ;; Name = integer  (constant/enum)
      (let ((m2 (nth-value 1 (cl-ppcre:scan-to-strings
                               "^(\\w+)\\s*=\\s*(.+)$" code))))
        (when m2
          (let* ((name    (aref m2 0))
                 (val-str (string-trim " " (aref m2 1)))
                 (intval  (ignore-errors
                            (cond
                              ((and (> (length val-str) 1)
                                    (char= #\$ (char val-str 0)))
                               (parse-integer (subseq val-str 1) :radix 16))
                              ((every #'digit-char-p val-str)
                               (parse-integer val-str))
                              (t nil)))))
            (when intval
              (return-from parse-asm-line
                (list :name name :kind :const
                      :value intval :annotation annotation))))))
      nil)))

(defun var-to-cobol-pic (kind size annotation raw-size-sym)
  "Return COBOL PIC clause string for a variable, or NIL to skip."
  (cond
    ((eq kind :const) nil)
    ((null annotation)
     (cond
       ((and (eq kind :byte) (= size 1)) "PIC 99 USAGE BINARY")
       ((and (eq kind :word) (= size 2)) "PIC 9999 USAGE BINARY")
       ((eq kind :fill)
        (if (and (= size 1) (not raw-size-sym)) "PIC 99 USAGE BINARY"
            (format nil "OCCURS ~a TIMES PIC 99 USAGE BINARY"
                    (or raw-size-sym size))))
       ((and (eq kind :byte) (> size 1))
        (format nil "OCCURS ~a TIMES PIC 99 USAGE BINARY"
                (or raw-size-sym size)))
       ((and (eq kind :word) (> size 2))
        (format nil "OCCURS ~a TIMES PIC 9999 USAGE BINARY"
                (or raw-size-sym (/ size 2))))
       (t nil)))
    ((eq (first annotation) :object-ref)
     (format nil "OBJECT REFERENCE ~a" (second annotation)))
    ((eq (first annotation) :varchar)
     (format nil "PIC X(~a) DEPENDING ON ~a"
             (second annotation) (third annotation)))
    ((eq (first annotation) :pic)
     (second annotation))
    (t "PIC 99 USAGE BINARY")))

(defun parse-assembly-globals (path &key skip-conditional-blocks)
  "Return variable/constant plists parsed from 64tass source file PATH."
  (let (results (depth 0))
    (handler-case
        (with-input-from-file (stream path)
          (loop for line = (read-line stream nil nil) while line do
                (cond
                  (skip-conditional-blocks
                   (cond
                     ((cl-ppcre:scan "^\\s+\\.if\\b" line)  (incf depth))
                     ((cl-ppcre:scan "^\\s+\\.fi\\b" line)
                      (when (> depth 0) (decf depth)))
                     ((> depth 0) nil)
                     (t (let ((item (parse-asm-line line)))
                          (when item (push item results))))))
                  (t (let ((item (parse-asm-line line)))
                       (when item (push item results)))))))
      (file-error (e) (warn "~&Could not read ~a: ~a" path e)))
    (nreverse results)))

(defun emit-cobol-var (level name pic stream)
  (format stream "~5d ~a ~a.~%" level name pic))

(defun emit-cobol-const (level name value stream)
  (let ((pic (cond ((< value 256)   "PIC 99 USAGE BINARY")
                   ((< value 65536) "PIC 9999 USAGE BINARY")
                   (t               "PIC 9(8) USAGE BINARY"))))
    (format stream "~5d ~a ~a VALUE ~d.~%" level name pic value)))

(defun make-globals-copybook
    (&key (root-dir #p"./")
          output-path)
  "Generate Phantasia-Globals.cpy from 64tass source files.
Default output: Source/Generated/Classes/Phantasia-Globals.cpy"
  (let* ((common   (merge-pathnames #p"Source/Code/7800/Common/" root-dir))
         (out-dir  (merge-pathnames #p"Source/Generated/Classes/" root-dir))
         (out-path (or output-path
                       (merge-pathnames #p"Phantasia-Globals.cpy" out-dir))))
    (ensure-directories-exist out-path)
    (with-output-to-file (stream out-path :if-exists :supersede)
      (format stream "* Phantasia-Globals.cpy~%")
      (format stream "* Generated by make-classes-for-oops -- DO NOT EDIT~%")
      (format stream
              "* Source: Source/Code/7800/Common/{ZeroPage,SysRAM,CartRAM,Enums,Constants}.s~2%")

      ;; ZeroPage -> 01 ZERO-PAGE EXTERNAL
      (let ((items (remove-if (lambda (i) (eq :const (getf i :kind)))
                              (parse-assembly-globals
                               (merge-pathnames "ZeroPage.s" common)
                               :skip-conditional-blocks t))))
        (when items
          (format stream " 01 ZERO-PAGE EXTERNAL.~%")
          (dolist (item items)
            (let ((pic (var-to-cobol-pic (getf item :kind)
                                         (getf item :size)
                                         (getf item :annotation)
                                         (getf item :raw-size-sym))))
              (when pic (emit-cobol-var 5 (getf item :name) pic stream))))))

      ;; SysRAM -> three 01 sections
      (let ((sysram-path (merge-pathnames "SysRAM.s" common))
            (current-section nil)
            (depth 0))
        (handler-case
            (with-input-from-file (sysram sysram-path)
              (loop for line = (read-line sysram nil nil) while line do
                    (cond
                      ((cl-ppcre:scan "^\\s+\\.if\\b" line)  (incf depth))
                      ((cl-ppcre:scan "^\\s+\\.fi\\b" line)
                       (when (> depth 0) (decf depth)))
                      ((> depth 0) nil)
                      (t
                       (let* ((t2      (string-trim " " line))
                              (lbl     (cl-ppcre:regex-replace ":$" t2 ""))
                              (new-s   (cdr (assoc lbl *sysram-section-labels*
                                                   :test #'string=))))
                         (if new-s
                             (progn (setf current-section new-s)
                                    (format stream "~% 01 ~a EXTERNAL.~%" new-s))
                             (when current-section
                               (let ((item (parse-asm-line line)))
                                 (when (and item (not (eq :const (getf item :kind))))
                                   (let ((pic (var-to-cobol-pic
                                               (getf item :kind)
                                               (getf item :size)
                                               (getf item :annotation)
                                               (getf item :raw-size-sym))))
                                     (when pic
                                       (emit-cobol-var 5 (getf item :name)
                                                       pic stream))))))))))))
          (file-error (e) (warn "~&Could not read SysRAM.s: ~a" e))))

      ;; CartRAM -> 01 CART-RAM EXTERNAL / gap structure
      (let ((cart-path (merge-pathnames "CartRAM.s" common))
            (gap-n 0) (depth 0) (in-gap nil))
        (format stream "~% 01 CART-RAM EXTERNAL.~%")
        (handler-case
            (with-input-from-file (cart cart-path)
              (loop for line = (read-line cart nil nil) while line do
                    (cond
                      ((cl-ppcre:scan "^\\s+\\.if\\b" line)  (incf depth))
                      ((cl-ppcre:scan "^\\s+\\.fi\\b" line)
                       (when (> depth 0) (decf depth)))
                      ((> depth 0) nil)
                      ((cl-ppcre:scan "^\\s+\\.Gap\\b" line)
                       (incf gap-n) (setf in-gap t)
                       (format stream "~5d CART-RAM-GAP-~d.~%" 5 gap-n))
                      (in-gap
                       (let ((item (parse-asm-line line)))
                         (when (and item (not (eq :const (getf item :kind))))
                           (let ((pic (var-to-cobol-pic
                                       (getf item :kind)
                                       (getf item :size)
                                       (getf item :annotation)
                                       (getf item :raw-size-sym))))
                             (when pic
                               (emit-cobol-var 10 (getf item :name)
                                               pic stream)))))))))
          (file-error (e) (warn "~&Could not read CartRAM.s: ~a" e))))

      ;; Enums -> 78 level
      (let ((items (remove-if-not (lambda (i) (eq :const (getf i :kind)))
                                  (parse-assembly-globals
                                   (merge-pathnames "Enums.s" common)))))
        (when items
          (format stream "~%* Enumeration constants~%")
          (dolist (item items)
            (when (getf item :value)
              (emit-cobol-const 78 (getf item :name)
                                (getf item :value) stream)))))

      ;; Constants -> 77 level
      (let ((items (remove-if-not (lambda (i) (eq :const (getf i :kind)))
                                  (parse-assembly-globals
                                   (merge-pathnames "Constants.s" common)))))
        (when items
          (format stream "~%* Non-enumeration constants~%")
          (dolist (item items)
            (when (getf item :value)
              (emit-cobol-const 77 (getf item :name)
                                (getf item :value) stream))))))
    (format t "~&Generated ~a~%" (enough-namestring out-path))
    out-path))
