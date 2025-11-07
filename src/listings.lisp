(in-package :skyline-tool)

(defun labels-to-mame (labels-file mame-file &optional (guardrailsp$ nil))
  "Converts LABELS-FILE into a format MAME can read as “comments” in MAME-FILE.

--guardrails to enable guardrails in MAME. (May cause crashes.)"
  (with-output-to-file (mame mame-file :if-exists :supersede)
    (with-input-from-file (labels labels-file)
      (let ((comments (make-hash-table))
            break
            minor-fault
            (guardrailsp (when guardrailsp$
                           (string-equal "--guardrails" guardrailsp$
                                         :end1 (min 3 (length guardrailsp$))))))
        (loop for line = (read-line labels nil nil)
              for (label address &rest ignorep) = (mapcar (curry #'string-trim " :")
                                                          (split-sequence #\= line))
              while (and label address)
              do (unless (and nil ignorep)
                   (when-let (addr (or (let* ((position (position #\$ address))
                                              (digits (and position
                                                           (subseq address (1+ position)))))
                                         (and digits
                                              (every (rcurry #'digit-char-p 16) digits)
                                              (parse-integer digits :radix 16)))
                                       (when (every #'digit-char-p address)
                                         (parse-integer address))
                                       nil))
                     #+ () (format t "~& “~a” = ~x" label addr)
                     (when (>= addr #x4000)
                       (setf (gethash addr comments) (cl-change-case:param-case label)))
                     (when (string-equal "Break" label)
                       (setf break addr))
                     (when (string-equal "MinorFault" label)
                       (setf minor-fault addr)))))
        (format mame "printf \"Wait a moment . . .\"~%")
        ;; (loop for addr being the hash-keys of comments
        ;;       for label = (gethash addr comments)
        ;;       do (format mame "comadd ~8,'0x, ~a~%" addr (string-trim " " label)))
        (when guardrailsp
          (format mame "wp 0140,2,w,{frame > 10},{printf \"Stagehand stack theatens to overflow into TIA/Maria mirrors\"}
wp 0160,2,w,{frame > 10},{printf \"Script stack threatens to overwrite NMI stack\"}
wp 01c0,2,w,{frame > 10},{printf \"Main stack threatens to overwrite Script stack\"}
wp 0100,40,rw,{frame > 10 && b@pc != 2c},{printf \"Garbage TIA/Maria mirrors access attempted\"}
wp 0300,100,rw,{frame > 10 && b@pc != 2c},{printf \"Garbage memory access attempted\"}
wp 0400,50,rw,{frame > 10 && b@pc != 2c},{printf \"Unmapped device memory access attempted\"}
wp 0460,20,rw,{frame > 10 && b@pc != 2c},{printf \"Unmapped device memory access attempted\"}
wp 0480,80,rw,{frame > 10},{printf \"RIOT memory access attempted\"}
wp 0500,80,rw,{frame > 10 && b@pc != 2c},{printf \"Unmapped device memory access attempted\"}
wp 0580,80,rw,{frame > 10},{printf \"RIOT memory access attempted\"}
wp 0600,1200,rw,{frame > 10 && b@pc != 2c},{printf \"Unmapped device memory access attempted\"}
wp 2040,c0,rw,{frame > 10 && b@pc != 2c},{printf \"Garbage memory access attempted\"}
wp 2140,c0,rw,{frame > 10 && b@pc != 2c},{printf \"Garbage memory access attempted\"}
wp 2800,800,rw,{frame > 10 && b@pc != 2c},{printf \"BIOS memory access attempted\"}
wp 3000,1000,rw,{frame > 10 && b@pc != 2c},{printf \"Unmapped device memory access attempted\"}
wp 8002,7ffe,w,1,{printf \"Write to ROM detected\"}
wp 2c,1,w,1,{printf \"DPPH write ($%02x)\", wpdata; go}
wp 30,1,w,1,{printf \"DPPL write ($%02x)\", wpdata; go}
"))
        (format mame "
wp 8000,2,w,{(wpdata & 3f) != 3f},{printf \"Bank switch: now in $%02x (pc $%04x, beamy %d)\", wpdata, pc, beamy; go}
wp 8000,2,w,{(wpdata & 3f) == 3f},{printf \"Tried to bank in LastBank at $8000\"}
wp 5048,1,w,{wpdata == 0},{printf \"Switching context to main thread (tid 0) (pc $%04x, beamy %d)\", pc, beamy; go}
wp 5048,1,w,{wpdata == 1},{printf \"Switching context to NMI thread (tid 1) (pc $%04x, beamy %d)\", pc, beamy; go}
wp 5048,1,w,{wpdata == 2},{printf \"Switching context to Stagehand thread (tid 2) (pc $%04x, beamy %d)\", pc, beamy; go}
wp 5048,1,w,{wpdata == 3},{printf \"Switching context to Script thread (tid 3) (pc $%04x, beamy %d)\", pc, beamy; go}
wp 5048,1,w,{wpdata > 3},{printf \"Switching context to non-existing thread (tid %d) (pc $%04x, beamy %d)\", wpdata, pc, beamy}
bp ~4,'0x,1,{snap \"brk.snap.png\"; save \"brk.core\",0,10000; printf \"BRK handler invoked at $%02x:%04x\", b@(4661),  -2+w@(2+sp)}
bp ~4,'0x,1,{snap \"fault.snap.png\"; save \"fault.core\",0,10000; printf \"Minor Fault %x.%x.%x.%x invoked at $%02x:%04x\", b@(1+w@(1+sp)), b@(2+w@(1+sp)), b@(3+w@(1+sp)), b@(4+w@(1+sp)), b@(4661),  -2+w@(1+sp)}
rp {pc<8000},{printf \"Program counter underflow\"}
bp c024,1,{printf \"NMI selector: $%04x (scanline %d)\", w@97, beamy;go}
printf \"\\n\\n\\n\\n\\n\\nReady.\\n(Press <F12> to start game)\"
"
                (or break 0)
                (or minor-fault 0))))))

(defun clean-redefs (redefs-file)
  "Clean and validate batariBASIC variable redefinitions file.
  
Parses the format structurally based on how batariBASIC generates it:
- Format: variable_name = value
- Valid values: temp1-temp6, var0-var47, a-z, or assembly expressions (.skipL...)
- Detects concatenation when value contains invalid patterns and truncates at boundary.
Outputs cleaned file to *standard-output*."
  (let ((*trace-output* *error-output*)
        (redefs-path (pathname redefs-file))
        (lines-fixed 0)
        (lines-kept 0))
    (with-input-from-file (input redefs-path)
      (loop for line = (read-line input nil nil)
            while line
            do (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline) line)))
                 (cond
                   ;; Skip empty lines
                   ((zerop (length trimmed))
                    (values))
                   
                   ;; Keep comment lines
                   ((char= (char trimmed 0) #\;)
                    (write-line trimmed *standard-output*)
                    (incf lines-kept))
                   
                   ;; Parse variable = value format
                   (t (let ((equals-pos (position #\= trimmed)))
                        (if (not equals-pos)
                            ;; No equals sign - skip malformed line
                            (incf lines-fixed)
                            (let ((var-name (string-trim '(#\Space #\Tab) (subseq trimmed 0 equals-pos)))
                                  (value-raw (string-trim '(#\Space #\Tab) (subseq trimmed (1+ equals-pos)))))
                              (if (valid-redef-value-p value-raw)
                                  ;; Valid - keep it
                                  (progn
                                    (write-line trimmed *standard-output*)
                                    (incf lines-kept))
                                  ;; Invalid - try to fix by truncating at concatenation boundary
                                  (let ((fixed-value (fix-concatenated-value value-raw)))
                                    (if fixed-value
                                        (progn
                                          (format *standard-output* "~a = ~a~%" var-name fixed-value)
                                          (incf lines-fixed))
                                        ;; Can't fix - skip it
                                        (incf lines-fixed))))))))))))
    (when (plusp lines-fixed)
      (format *trace-output* "~&Cleaned redefs file: fixed ~d malformed entries, kept ~d valid entries~%"
              lines-fixed lines-kept)
      (finish-output *trace-output*))))

(defun valid-redef-value-p (value)
  "Check if VALUE is a valid batariBASIC redefinition value.
Valid formats: temp1-temp6, var0-var47, a-z, r000-w127, or assembly expressions starting with '.'"
  (cond
    ((zerop (length value)) nil)
    ;; Assembly expression: .skipL... or similar - check for concatenation
    ((char= (char value 0) #\.)
     ;; Check if it's a valid assembly expression (no concatenation)
     (let ((underscore-pos (position #\_ value)))
       (if (and underscore-pos
                (< (1+ underscore-pos) (length value))
                (upper-case-p (char value (1+ underscore-pos))))
           nil  ; Concatenation detected
           t)))  ; Valid assembly expression
    ;; Simple identifier: temp1-temp6
    ((and (>= (length value) 5)
          (string= value "temp" :end1 4)
          (digit-char-p (char value 4))
          (or (= (length value) 5)
              (and (= (length value) 6) (digit-char-p (char value 5)))))
     t)
    ;; var0-var47
    ((and (>= (length value) 4)
          (string= value "var" :end1 3)
          (every #'digit-char-p (subseq value 3)))
     (let ((num (parse-integer value :start 3 :junk-allowed t)))
       (and num (<= 0 num 47))))
    ;; Single letter a-z
    ((and (= (length value) 1)
          (lower-case-p (char value 0)))
     t)
    ;; Register: r000-w127 format
    ((and (>= (length value) 4)
          (member (char value 0) '(#\r #\w))
          (every #'digit-char-p (subseq value 1)))
     t)
    ;; Decimal literal (optional leading sign)
    ((numeric-literal-p value)
     t)
    ;; Hex ($xx) or binary (%xx) literal
    ((or (hex-literal-p value)
         (binary-literal-p value))
     t)
    ;; Invalid - contains concatenation patterns
    (t nil)))

(defun fix-concatenated-value (value)
  "Fix concatenated value by truncating at the concatenation boundary.
Returns the valid prefix, or nil if no valid prefix can be determined."
  (cond
    ((zerop (length value)) nil)
    ;; If starts with '.', it's an assembly expression - check for concatenation
    ((char= (char value 0) #\.)
     (let ((underscore-pos (position #\_ value)))
       (if (and underscore-pos
                (< (1+ underscore-pos) (length value))
                (upper-case-p (char value (1+ underscore-pos))))
           ;; Concatenation detected - truncate before underscore
           (if (> underscore-pos 1)
               (subseq value 0 underscore-pos)
               nil)
           value)))
    ;; Numeric literal
    ((numeric-literal-p value)
     (truncate-numeric value))
    ;; Hex literal
    ((hex-literal-p value)
     (truncate-prefixed-literal value))
    ;; Binary literal
    ((binary-literal-p value)
     (truncate-prefixed-literal value))
    ;; If starts with 'temp', extract just tempN
    ((and (>= (length value) 4)
          (string= value "temp" :end1 4))
     (if (and (>= (length value) 5) (digit-char-p (char value 4)))
         (let ((digit-end (position-if-not #'digit-char-p value :start 4)))
           (if digit-end
               (subseq value 0 digit-end)
               value))
         nil))
    ;; If starts with 'var', extract just varN
    ((and (>= (length value) 3)
          (string= value "var" :end1 3))
     (if (and (>= (length value) 4) (digit-char-p (char value 3)))
         (let ((digit-end (position-if-not #'digit-char-p value :start 3)))
           (if digit-end
               (subseq value 0 digit-end)
               value))
         nil))
    ;; Single letter
    ((and (= (length value) 1)
          (lower-case-p (char value 0)))
     value)
    ;; Register format r000 or w000
    ((and (>= (length value) 4)
          (member (char value 0) '(#\r #\w))
          (every #'digit-char-p (subseq value 1)))
     (let ((digit-end (position-if-not #'digit-char-p value :start 1)))
       (if digit-end
           (subseq value 0 digit-end)
           value)))
    ;; Try to extract valid prefix before concatenation
    (t (let ((concat-pos (detect-concatenation-boundary value)))
         (if concat-pos
             (let ((prefix (subseq value 0 concat-pos)))
               ;; Only return if the prefix is a valid value
               (if (valid-redef-value-p prefix)
                   prefix
                   nil))
             nil)))))

(defun numeric-literal-p (value)
  "Return true if VALUE is a decimal literal with optional sign."
  (and (> (length value) 0)
       (let ((start (if (member (char value 0) '(#\+ #\-)) 1 0)))
         (and (< start (length value))
              (every #'digit-char-p (subseq value start))))))

(defun hex-literal-p (value)
  "Return true if VALUE is a $-prefixed hexadecimal literal."
  (and (> (length value) 1)
       (char= (char value 0) #\$)
       (every #'hex-digit-p (subseq value 1))))

(defun binary-literal-p (value)
  "Return true if VALUE is a %-prefixed binary literal."
  (and (> (length value) 1)
       (char= (char value 0) #\%)
       (every (lambda (c) (or (char= c #\0) (char= c #\1)))
              (subseq value 1))))

(defun truncate-numeric (value)
  "Return numeric prefix of VALUE (handles optional sign)."
  (let* ((start (if (member (char value 0) '(#\+ #\-)) 1 0))
         (end (position-if-not #'digit-char-p value :start start)))
    (cond
      ((null end) value)
      ((and (= end start) (> start 0))
       nil) ; sign without digits
      (t (subseq value 0 end)))))

(defun truncate-prefixed-literal (value)
  "Return prefix of VALUE for literals starting with $ or %."
  (let ((end (position-if-not #'alphanumericp value :start 1)))
    (if end
        (subseq value 0 end)
        value)))

(defun hex-digit-p (char)
  (not (null (digit-char-p char 16))))

(defun detect-concatenation-boundary (value)
  "Detect where concatenation starts in VALUE.
Returns position where concatenation begins, or nil if no concatenation detected."
  (let ((len (length value)))
    (when (plusp len)
      (cond
        ;; Lowercase followed by uppercase (like "tempSong")
        ((lower-case-p (char value 0))
         (let ((upper-pos (position-if #'upper-case-p value :start 1)))
           (when (and upper-pos
                      (> upper-pos 1)
                      (every (lambda (c) (or (lower-case-p c) (digit-char-p c)))
                             (subseq value 1 upper-pos)))
             upper-pos)))
        ;; Underscore followed by uppercase (like "temp_Song")
        (t (let ((underscore-pos (position #\_ value)))
             (when (and underscore-pos
                        (< (1+ underscore-pos) len)
                        (upper-case-p (char value (1+ underscore-pos))))
               (1+ underscore-pos))))))))
