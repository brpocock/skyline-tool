(in-package :skyline-tool)

;;; 64tass assembly -> EIGHTBOL copybook generator
;;; Parses ZeroPage.s, SysRAM.s, CartRAM.s, Enums.s, Constants.s
;;; Outputs a platform-specific Source/Generated/Classes/{Machine}-Globals.cpy.
;;;
;;; Variable annotation syntax (add to assembly source as needed):
;;;   Label: .byte ?  ; @ClassName          -> OBJECT REFERENCE ClassName
;;;   Label: .fill n, ? ; = PIC X(n)        -> explicit PIC clause
;;;   Label: .fill n, ? ; = VARCHAR(n) DEPENDING ON LenField -> varchar
;;;
;;; Generated files follow EIGHTBOL fixed-format record layout:
;;;   Cols  1-6  : 6-digit sequence number (increments by 10)
;;;   Col   7    : indicator (' ' = normal, '*' = comment)
;;;   Col   8+   : program text
;;;     Level 01, 77, 78 → column  8  (Area A)
;;;     Level 05         → column 12  (Area B, 4-space indent)
;;;     Level 10         → column 16  (8-space indent)

;;; ---------------------------------------------------------------
;;; EIGHTBOL fixed-format output helpers (shared with oops.lisp)
;;; ---------------------------------------------------------------

(defvar *eightbol-seq* 0
  "Current EIGHTBOL sequence number; bound fresh by WITH-EIGHTBOL-SEQUENCE.")

(defmacro with-eightbol-sequence (&body body)
  "Execute BODY with *EIGHTBOL-SEQ* reset to 0 so each file starts at 000010."
  `(let ((*eightbol-seq* 0)) ,@body))

(defun next-eightbol-seq ()
  "Return the next EIGHTBOL sequence number (increments *EIGHTBOL-SEQ* by 10)."
  (incf *eightbol-seq* 10))

(defun eightbol-level-indent (level)
  "Return the content-area indentation string for LEVEL.
Column 8 is the start of the content area (Area A).
  01, 77, 78 → col  8  (Area A — no indent)
  05         → col 12  (Area B — 4 spaces)
  10         → col 16  (8 spaces)"
  (case level
    ((1 77 78) "")
    ((5)       "    ")
    ((10)      "        ")
    (t         "    ")))

(defun emit-eightbol-comment (stream text)
  "Emit a EIGHTBOL comment line (* in column 7)."
  (format stream "~6,'0d*~a~%" (next-eightbol-seq) text))

(defun emit-eightbol-blank (stream)
  "Emit a blank EIGHTBOL comment line for visual separation."
  (format stream "~6,'0d*~%" (next-eightbol-seq)))

(defun emit-eightbol-section (stream level name)
  "Emit a EIGHTBOL group header (e.g. 01 ZERO-PAGE EXTERNAL.)."
  (format stream "~6,'0d ~a~2,'0d ~a.~%"
          (next-eightbol-seq) (eightbol-level-indent level) level name))

(defun emit-eightbol-var (level name pic stream)
  "Emit a EIGHTBOL data description entry."
  (format stream "~6,'0d ~a~2,'0d ~a ~a.~%"
          (next-eightbol-seq) (eightbol-level-indent level)
          level (pascal-to-eightbol-name name) pic))

(defun emit-eightbol-const (level name value stream)
  "Emit a EIGHTBOL constant (77 or 78 level) with VALUE."
  (let ((pic (cond ((< value 256)   "PIC 99 USAGE BINARY")
                   ((< value 65536) "PIC 9999 USAGE BINARY")
                   (t               "PIC 9(8) USAGE BINARY"))))
    (format stream "~6,'0d ~a~2,'0d ~a ~a VALUE ~d.~%"
            (next-eightbol-seq) (eightbol-level-indent level)
            level (pascal-to-eightbol-name name) pic value)))

(defparameter *sysram-section-labels*
  '(("SysRAMLow"  . "SYS-RAM-LOW")
    ("SysRAMMid"  . "SYS-RAM-MID")
    ("SysRAMHigh" . "SYS-RAM-HIGH"))
  "Assembly section labels and their EIGHTBOL 01-level names.")

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

(defun var-to-eightbol-pic (kind size annotation raw-size-sym)
  "Return EIGHTBOL PIC clause string for a variable, or NIL to skip.
Symbol names in OCCURS and DEPENDING ON clauses are converted to EIGHTBOL form."
  (flet ((eb (sym) (if sym (pascal-to-eightbol-name (string sym)) nil)))
    (cond
      ((eq kind :const) nil)
      ((null annotation)
       (cond
         ((and (eq kind :byte) (= size 1)) "PIC 99 USAGE BINARY")
         ((and (eq kind :word) (= size 2)) "PIC 9999 USAGE BINARY")
         ((eq kind :fill)
          (if (and (= size 1) (not raw-size-sym)) "PIC 99 USAGE BINARY"
              (format nil "OCCURS ~a TIMES PIC 99 USAGE BINARY"
                      (or (eb raw-size-sym) size))))
         ((and (eq kind :byte) (> size 1))
          (format nil "OCCURS ~a TIMES PIC 99 USAGE BINARY"
                  (or (eb raw-size-sym) size)))
         ((and (eq kind :word) (> size 2))
          (format nil "OCCURS ~a TIMES PIC 9999 USAGE BINARY"
                  (or (eb raw-size-sym) (/ size 2))))
         (t nil)))
      ((eq (first annotation) :object-ref)
       (format nil "OBJECT REFERENCE ~a" (pascal-to-eightbol-name (second annotation))))
      ((eq (first annotation) :varchar)
       ;; Correct COBOL: PIC X OCCURS 0 TO n TIMES DEPENDING ON size-field.
       ;; Size-field does not get a picture.
       (format nil "PIC X OCCURS 0 TO ~a TIMES DEPENDING ON ~a"
               (second annotation)
               (pascal-to-eightbol-name (third annotation))))
      ((eq (first annotation) :pic)
       (second annotation))
      (t "PIC 99 USAGE BINARY"))))

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

(defun make-globals-copybook
    (&key (root-dir    #p"./")
          output-path
          (machine     (machine-directory-name)))
  "Generate a platform-specific globals copybook from 64tass source files.
MACHINE selects the platform subdirectory (defaults to (machine-directory-name) from *MACHINE*).
Input:  Source/Code/{machine}/Common/{ZeroPage,SysRAM,CartRAM,Enums,Constants}.s
Output: Source/Generated/{machine}/Classes/{MACHINE}-Globals.cpy  (or OUTPUT-PATH if given)"
  (let* ((common   (merge-pathnames
                    (make-pathname :directory `(:relative "Source" "Code" ,machine "Common"))
                    root-dir))
         (out-dir  (merge-pathnames
                    (make-pathname :directory `(:relative "Source" "Generated" ,machine "Classes"))
                    root-dir))
         (out-name (format nil "~a-Globals" (string-upcase machine)))
         (out-path (or output-path
                       (merge-pathnames
                        (make-pathname :name out-name :type "cpy")
                        out-dir))))
    (ensure-directories-exist out-path)
    (with-output-to-file (stream out-path :if-exists :supersede)
      (with-eightbol-sequence
        (emit-eightbol-comment stream (format nil " ~a-Globals.cpy" (string-upcase machine)))
        (emit-eightbol-comment stream " Generated by make-classes-for-oops -- DO NOT EDIT")
        (emit-eightbol-comment stream
                            (format nil " Source: Source/Code/~a/Common/{{ZeroPage,SysRAM,CartRAM,Enums,Constants}}.s"
                                    machine))
        (emit-eightbol-blank stream)

        ;; ZeroPage -> 01 ZERO-PAGE EXTERNAL
        (let ((items (remove-if (lambda (i) (eq :const (getf i :kind)))
                                (parse-assembly-globals
                                 (merge-pathnames "ZeroPage.s" common)
                                 :skip-conditional-blocks t))))
          (when items
            (emit-eightbol-blank stream)
            (emit-eightbol-section stream 1 "ZERO-PAGE EXTERNAL")
            (dolist (item items)
              (let ((pic (var-to-eightbol-pic (getf item :kind)
                                           (getf item :size)
                                           (getf item :annotation)
                                           (getf item :raw-size-sym))))
                (when pic (emit-eightbol-var 5 (getf item :name) pic stream))))))

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
                         (let* ((t2    (string-trim " " line))
                                (lbl   (cl-ppcre:regex-replace ":$" t2 ""))
                                (new-s (cdr (assoc lbl *sysram-section-labels*
                                                   :test #'string=))))
                           (if new-s
                               (progn
                                 (setf current-section new-s)
                                 (emit-eightbol-blank stream)
                                 (emit-eightbol-section stream 1
                                                     (format nil "~a EXTERNAL" new-s)))
                               (when current-section
                                 (let ((item (parse-asm-line line)))
                                   (when (and item (not (eq :const (getf item :kind))))
                                     (let ((pic (var-to-eightbol-pic
                                                 (getf item :kind)
                                                 (getf item :size)
                                                 (getf item :annotation)
                                                 (getf item :raw-size-sym))))
                                       (when pic
                                         (emit-eightbol-var 5 (getf item :name)
                                                         pic stream)))))))))))
            (file-error (e) (warn "~&Could not read SysRAM.s: ~a" e))))

        ;; CartRAM -> 01 CART-RAM EXTERNAL / gap structure
        (let ((cart-path (merge-pathnames "CartRAM.s" common))
              (gap-n 0) (depth 0) (in-gap nil))
          (emit-eightbol-blank stream)
          (emit-eightbol-section stream 1 "CART-RAM EXTERNAL")
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
                         (emit-eightbol-section stream 5
                                             (format nil "CART-RAM-GAP-~d" gap-n)))
                        (in-gap
                         (let ((item (parse-asm-line line)))
                           (when (and item (not (eq :const (getf item :kind))))
                             (let ((pic (var-to-eightbol-pic
                                         (getf item :kind)
                                         (getf item :size)
                                         (getf item :annotation)
                                         (getf item :raw-size-sym))))
                               (when pic
                                 (emit-eightbol-var 10 (getf item :name)
                                                 pic stream)))))))))
            (file-error (e) (warn "~&Could not read CartRAM.s: ~a" e))))

        ;; Enums -> 78 level
        (let ((items (remove-if-not (lambda (i) (eq :const (getf i :kind)))
                                    (parse-assembly-globals
                                     (merge-pathnames "Enums.s" common)))))
          (when items
            (emit-eightbol-blank stream)
            (emit-eightbol-comment stream " Enumeration constants")
            (dolist (item items)
              (when (getf item :value)
                (emit-eightbol-const 78 (getf item :name)
                                  (getf item :value) stream)))))

        ;; Constants -> 77 level
        (let ((items (remove-if-not (lambda (i) (eq :const (getf i :kind)))
                                    (parse-assembly-globals
                                     (merge-pathnames "Constants.s" common)))))
          (when items
            (emit-eightbol-blank stream)
            (emit-eightbol-comment stream " Non-enumeration constants")
            (dolist (item items)
              (when (getf item :value)
                (emit-eightbol-const 77 (getf item :name)
                                  (getf item :value) stream)))))))
    (format t "~&Generated ~a~%" (enough-namestring out-path))
    out-path)))
