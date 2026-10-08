;;; Phantasia SkylineTool/src/cbm-tooling.lisp
;;;; CBM distribution helpers: PETSCII documentation SEQ emitters and GEOS
;;;; VLIR stub sector packing.  Prefer VICE petcat for ASCII→PETSCII SEQ and
;;;; BASIC tokenization when installed; see cbm-petscii-docs when petcat is absent.
;;;; Copyright © 2026 Interworldly Adventuring, LLC.

(in-package :skyline-tool)

(defun %petscii-doc-upper-byte (char)
  "Return PETSCII byte for CHAR if it is ASCII A–Z, else NIL.

@table @asis
@item CHAR
Designator for one character (must be @code{standard-char})
@item Returns
Unsigned byte @code{#xc1}–@code{#xda} or NIL
@end table"
  (when (and (standard-char-p char)
             (char<= #\A (char-upcase char) #\Z))
    (+ #xc1 (- (char-code (char-upcase char)) (char-code #\A)))))

(defun %ascii-doc-line-to-petscii-seq-bytes (line)
  "Convert one LINE of printable ASCII into a PETSCII SEQ line (CR-terminated).

Maps A–Z to unshifted PETSCII letters (@code{#xc1}–@code{#xda}); keeps space,
digits, and a small punctuation set as ASCII codes; maps other characters to
@code{#x3f}.  Embedded CR becomes @code{#x0d}; LF is ignored (line breaks come
from caller splitting).

@table @asis
@item LINE
String without trailing newline
@item Returns
@code{(unsigned-byte 8)} vector including trailing @code{#x0d}
@end table"
  (let ((pieces (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t
                               :fill-pointer 0)))
    (flet ((emit (b)
             (vector-push-extend b pieces)))
      (dotimes (i (length line))
        (let ((ch (char line i)))
          (cond
            ((char= ch #\Newline))
            ((char= ch #\Return)
             (emit #x0d))
            (t (let ((u (char-upcase ch)))
                 (cond
                   ((%petscii-doc-upper-byte u)
                    (emit (the (unsigned-byte 8)
                               (%petscii-doc-upper-byte u))))
                   ((char= ch #\Space)
                    (emit #x20))
                   ((char<= #\0 ch #\9)
                    (emit (char-code ch)))
                   ((find ch ".:,;!?-'\"()" :test #'char=)
                    (emit (char-code ch)))
                   (t (emit #x3f))))))))
      (emit #x0d)
      pieces)))

(defun %utf8-file-to-petscii-doc-seq-bytes (path)
  "Read UTF-8 text PATH and return concatenated PETSCII SEQ bytes.

@table @asis
@item PATH
Pathname designator
@item Returns
Fresh @code{(unsigned-byte 8)} vector
@end table"
  (let ((chunks ()))
    (with-open-file (stream path :external-format :utf-8)
      (loop for line = (read-line stream nil nil)
            while line
            do (push (%ascii-doc-line-to-petscii-seq-bytes line) chunks)))
    (let* ((ordered (nreverse chunks))
           (total (reduce #'+ ordered :key #'length :initial-value 0))
           (out (make-array total :element-type '(unsigned-byte 8)))
           (pos 0))
      (dolist (c ordered)
        (replace out c :start1 pos)
        (incf pos (length c)))
      out)))

(defun %parse-flag-args (args keys-alist)
  "Parse ARGS as alternating @code{--flag value}; KEYS-ALIST maps flag string to symbol.

Unknown flags signal an error.  Returns plist of SYMBOL keyword pairs plus
list of leftover tokens (should be empty for strict commands).

@table @asis
@item ARGS
List of strings
@item KEYS-ALIST
@code{((\"--out-dir\" . :out-dir) …)}
@item Returns
Two values: plist suitable for @code{getf}, and remaining strings
@end table"
  (let ((plist ())
        (rest args))
    (loop while rest
          do (let ((token (pop rest)))
               (unless (and (stringp token) (> (length token) 0) (char= #\- (char token 0)))
                 (error "Expected a flag starting with --, got ~s" token))
               (let ((sym (cdr (assoc token keys-alist :test #'string=))))
                 (unless sym
                   (error "Unknown flag ~a" token))
                 (unless rest
                   (error "Missing value after ~a" token))
                 (setf (getf plist sym) (pop rest)))))
    (values plist rest)))

(defun cbm-petscii-docs (&rest args)
  "Fallback: emit phantasia.doc.*.seq under OUT-DIR without VICE petcat.

When VICE @command{petcat} is available, the build should use it instead
(e.g. @samp{petcat -text -w2 -o OUT.seq -- MASTER.txt} per the VICE manual)
for ASCII→PETSCII @code{SEQ} and @samp{-w@var{version}} for tokenized BASIC.
This command duplicates the fixed retail/demo @file{SEQ} layout from UTF-8
masters under @file{Source/Code/CBM/Reference/petscii/} if @command{petcat} is
absent or unsuitable.

@table @asis
@item ARGS
Command-line tokens: @code{--out-dir} then a directory pathname string
@item Side effects
Creates DIRECTORY, writes two @code{SEQ} binaries, prints byte counts
@end table

@xref{fun:geos-vlir-stub-pack}."
  (multiple-value-bind (plist rest)
      (%parse-flag-args args
                        '(("--out-dir" . :out-dir)))
    (when rest
      (error "Extra arguments: ~s" rest))
    (let ((out-dir (getf plist :out-dir)))
      (unless out-dir
        (error "Missing required --out-dir"))
      (ensure-directories-exist (pathname out-dir))
      (let ((root (merge-pathnames
                   #p"Source/Code/CBM/Reference/petscii/"
                   (uiop:getcwd)))
            (pairs '(("MANUAL-RETAIL-MASTER.txt" . "phantasia.doc.manual.seq")
                     ("DEMO-ZIP-MASTER.txt" . "phantasia.doc.demo.seq"))))
        (dolist (pair pairs)
          (destructuring-bind (src-name . dst-name) pair
            (let ((src (merge-pathnames src-name root)))
              (unless (probe-file src)
                (error "Missing master documentation source ~a" src))
              (let ((data (%utf8-file-to-petscii-doc-seq-bytes src))
                    (dst (merge-pathnames dst-name (pathname out-dir))))
                (with-open-file (out dst :direction :output
                                        :if-exists :supersede
                                        :element-type '(unsigned-byte 8))
                  (write-sequence data out))
                (format t "~&Wrote ~a (~d bytes)~%" dst (length data))))))))))

(defun %read-cbm-prg (path)
  "Read a CBM @code{.prg} file; return LOAD-ADDRESS and payload BYTES.

@table @asis
@item PATH
Pathname designator to a file whose first two bytes are little-endian load address
@item Values
@code{LOAD-ADDRESS} (integer), @code{BYTES} (@code{(unsigned-byte 8)} vector)
@end table"
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((len (file-length stream)))
      (when (< len 3)
        (error "~a: PRG too short" path))
      (let ((buf (make-array len :element-type '(unsigned-byte 8))))
        (read-sequence buf stream)
        (values (+ (aref buf 0) (ash (aref buf 1) 8))
                (subseq buf 2))))))

(defun %paste-geos-info-c-string (buf start end-exclusive string)
  "Write ASCII STRING as NUL-terminated bytes into BUF between START and END-EXCLUSIVE.

@table @asis
@item BUF
@code{(unsigned-byte 8)} vector at least @code{END-EXCLUSIVE} elements
@item STRING
Simple string containing only code points below 128
@end table"
  (let ((room (- end-exclusive start 1)))
    (when (plusp room)
      (let ((n (min room (length string))))
        (loop for i below n
              do (setf (aref buf (+ start i))
                       (char-code (char string i))))
        (setf (aref buf (+ start n)) 0)))))

(defun %build-geos-info-sector (load-addr end-addr klass author desc)
  "Build a 256-byte GEOS INFO sector stub matching the Python packer layout.

@table @asis
@item LOAD-ADDR END-ADDR
16-bit addresses (application load range)
@item KLASS AUTHOR DESC
Short ASCII metadata strings
@end table"
  (let ((buf (make-array 256 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref buf 0) #x00
          (aref buf 1) #xff
          (aref buf 2) #x03
          (aref buf 3) #x15
          (aref buf 4) #xbf
          (aref buf #x44) #x82
          (aref buf #x45) #x06
          (aref buf #x46) #x01
          (aref buf #x47) (ldb (byte 8 0) load-addr)
          (aref buf #x48) (ldb (byte 8 8) load-addr)
          (aref buf #x49) (ldb (byte 8 0) end-addr)
          (aref buf #x4a) (ldb (byte 8 8) end-addr)
          (aref buf #x4b) (ldb (byte 8 0) load-addr)
          (aref buf #x4c) (ldb (byte 8 8) load-addr))
    (%paste-geos-info-c-string buf #x4d #x61 klass)
    (%paste-geos-info-c-string buf #x61 #x75 author)
    (%paste-geos-info-c-string buf #xa0 #x100 desc)
    buf))

(defun %build-geos-record-index-sector (first-track first-sector)
  "Return a 256-byte RECORD index sector with placeholder T/S for record 0.

@table @asis
@item FIRST-TRACK FIRST-SECTOR
Integers 0–255 used as the chain head placeholder
@end table"
  (let ((buf (make-array 256 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref buf 0) #x00
          (aref buf 1) #xff
          (aref buf 2) (logand first-track #xff)
          (aref buf 3) (logand first-sector #xff))
    buf))

(defun geos-vlir-stub-pack (&rest args)
  "Emit GEOS VLIR stub binaries (INFO, RECORD index, record0 PRG) under OUT-DIR.

Reads @code{--record0-prg} (CBM load address plus payload), writes
@file{PhantasiaBoot-info.bin}, @file{PhantasiaBoot-record.bin}, and
@file{PhantasiaBoot-record0.prg}.  Optional @code{--record0-track} and
@code{--record0-sector} set placeholder T/S (default 1 and 16).

@table @asis
@item ARGS
@code{--record0-prg}, @code{--out-dir}, and optional track/sector flags
@item Side effects
Writes three files; prints a short confirmation
@end table

@xref{fun:cbm-petscii-docs}."
  (multiple-value-bind (plist rest)
      (%parse-flag-args args
                        '(("--record0-prg" . :record0-prg)
                          ("--out-dir" . :out-dir)
                          ("--record0-track" . :record0-track)
                          ("--record0-sector" . :record0-sector)))
    (when rest
      (error "Extra arguments: ~s" rest))
    (let ((prg-path (getf plist :record0-prg))
          (out-dir (getf plist :out-dir))
          (track (or (getf plist :record0-track) "1"))
          (sector (or (getf plist :record0-sector) "16")))
      (unless prg-path
        (error "Missing required --record0-prg"))
      (unless out-dir
        (error "Missing required --out-dir"))
      (let ((track-n (parse-integer track))
            (sector-n (parse-integer sector)))
        (multiple-value-bind (load-addr body)
            (%read-cbm-prg prg-path)
          (let* ((end-addr (+ load-addr (length body)))
                 (info (%build-geos-info-sector
                        load-addr end-addr
                        "Phantasia GEOS stub"
                        "Interworldly Adventuring"
                        (concatenate 'string
                                     "Bootstrap record0 placeholder - replace T/S "
                                     "and chain with geoProgrammer or disk tools.")))
                 (rec (%build-geos-record-index-sector track-n sector-n))
                 (dir (uiop:ensure-directory-pathname (pathname out-dir))))
            (ensure-directories-exist dir)
            (let ((info-file (merge-pathnames "PhantasiaBoot-info.bin" dir))
                  (rec-file (merge-pathnames "PhantasiaBoot-record.bin" dir))
                  (r0-file (merge-pathnames "PhantasiaBoot-record0.prg" dir))
                  (r0-bytes (make-array (+ 2 (length body))
                                        :element-type '(unsigned-byte 8))))
              (setf (aref r0-bytes 0) (ldb (byte 8 0) load-addr)
                    (aref r0-bytes 1) (ldb (byte 8 8) load-addr))
              (replace r0-bytes body :start1 2)
              (with-open-file (out info-file :direction :output
                                             :if-exists :supersede
                                             :element-type '(unsigned-byte 8))
                (write-sequence info out))
              (with-open-file (out rec-file :direction :output
                                            :if-exists :supersede
                                            :element-type '(unsigned-byte 8))
                (write-sequence rec out))
              (with-open-file (out r0-file :direction :output
                                           :if-exists :supersede
                                           :element-type '(unsigned-byte 8))
                (write-sequence r0-bytes out))
              (format t "~&Wrote ~a (+ record index + record0 copy)~%" info-file))))))))
