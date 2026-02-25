;;; service-banks-copybook.lisp — generate service→bank mapping from 64tass bank files
;;;
;;; Scans Source/Code/{machine}/Banks/**/*.s for BANK = $NN and .Dispatch ServiceXxx, Label.
;;; Reads Enums.s for BankXxx = $NN to resolve bank numbers to symbols.
;;; Outputs Source/Generated/{machine}/Classes/{MACHINE}-Service-Banks.cpy
;;; for use by eightbol when compiling CALL SERVICE target without explicit bank.
(in-package :skyline-tool)

;;; ---------------------------------------------------------------
;;; Parse Enums.s for bank number → symbol mapping
;;; ---------------------------------------------------------------

(defun parse-bank-enums (enums-path)
  "Parse Enums.s for lines 'BankXxx = $YY' and return alist (bank-num . symbol-name).
BANK-NUM is the integer value (e.g. 3 for $03)."
  (let ((alist ()))
    (handler-case
        (with-open-file (stream enums-path :direction :input)
          (loop for line = (read-line stream nil nil) while line do
                (cl-ppcre:register-groups-bind (sym hex)
                    ("^\\s*\\b(Bank[A-Za-z0-9]+)\\s*=\\s*\\$([0-9a-fA-F]+)\\b" line)
                  (when (and sym hex)
                    (let ((num (parse-integer hex :radix 16)))
                      (push (cons num sym) alist))))))
      (cl:file-error () (warn "~&Could not read ~a" enums-path)))
    (nreverse alist)))

;;; ---------------------------------------------------------------
;;; Parse a single bank .s file for BANK = and .Dispatch
;;; ---------------------------------------------------------------

(defun parse-bank-file (path bank-num->symbol)
  "Parse one bank .s file. Return list of (service-name . bank-symbol) for each .Dispatch.
PATH is the bank file path. BANK-NUM->SYMBOL is an alist (integer . symbol-string)."
  (let ((bank-num nil)
        (bank-sym nil)
        (results ()))
    (handler-case
        (with-open-file (stream path :direction :input)
          (loop for line = (read-line stream nil nil) while line do
                ;; BANK = $NN
                (cl-ppcre:register-groups-bind (hex)
                    ("^\\s*BANK\\s*=\\s*\\$([0-9a-fA-F]+)\\b" line)
                  (when hex
                    (setf bank-num (parse-integer hex :radix 16))
                    (setf bank-sym (cdr (assoc bank-num bank-num->symbol))))
                ;; .Dispatch ServiceXxx, Label
                (cl-ppcre:register-groups-bind (service)
                    ("^\\s*\\.Dispatch\\s+(Service[A-Za-z0-9]+)\\s*," line)
                  (when (and service bank-sym)
                    (push (cons service bank-sym) results))))))
      (cl:file-error () (warn "~&Could not read ~a" path)))
    (nreverse results)))

;;; ---------------------------------------------------------------
;;; Main generator
;;; ---------------------------------------------------------------

(defun make-service-banks-copybook
    (&key (root-dir #p"./")
          output-path
          (machine (machine-directory-name)))
  "Generate a platform-specific service→bank copybook from 64tass bank files.
MACHINE selects the platform subdirectory (defaults to (machine-directory-name) from *MACHINE*).
Input:  Source/Code/{machine}/Banks/**/*.s, Source/Code/{machine}/Common/Enums.s
Output: Source/Generated/{machine}/Classes/{MACHINE}-Service-Banks.cpy  (or OUTPUT-PATH if given)

The copybook format is EIGHTBOL-compatible:
  * Service bank ServiceComposeCharacter BankPlayer
  * Service bank ServiceEnterCharacter BankPrototypes
..."
  (let* ((common (merge-pathnames
                  (make-pathname
                   :directory `(:relative "Source" "Code" ,machine "Common"))
                  root-dir))
         (enums-path (merge-pathnames "Enums.s" common))
         (banks-dir (merge-pathnames
                     (make-pathname :directory `(:relative "Source" "Code" ,machine "Banks"))
                     root-dir))
         (out-dir (merge-pathnames
                   (make-pathname :directory `(:relative "Source" "Generated" ,machine "Classes"))
                   root-dir))
         (out-name (format nil "~a-Service-Banks" (string-upcase machine)))
         (out-path (or output-path
                      (merge-pathnames
                       (make-pathname :name out-name :type "cpy")
                       out-dir))))
    (let ((bank-num->symbol (parse-bank-enums enums-path))
          (service->bank (make-hash-table :test 'equal)))
      ;; Collect all bank .s files (Banks/BankNN/BankNN.s)
      (let ((bank-files (when (probe-file banks-dir)
                          (loop for n from 0 to 31
                                for subdir = (merge-pathnames
                                              (make-pathname :directory
                                                            `(:relative ,(format nil "Bank~a" (let ((s (format nil "~x" n)))
                                     (concatenate 'string
                                       (make-string (max 0 (- 2 (length s))) :initial-element #\0)
                                       s)))))
                                              banks-dir)
                                when (probe-file subdir)
                                append (directory (merge-pathnames
                                                  (make-pathname :name :wild :type "s")
                                                  subdir))))))
        (dolist (path (or bank-files ()))
          (dolist (pair (parse-bank-file path bank-num->symbol))
            (setf (gethash (car pair) service->bank) (cdr pair)))))
      (ensure-directories-exist out-path)
      (with-output-to-file (stream out-path :if-exists :supersede)
        (with-eightbol-sequence
          (emit-eightbol-comment stream (format nil " ~a-Service-Banks.cpy" (string-upcase machine)))
          (emit-eightbol-comment stream " Generated by make-service-banks-copybook -- DO NOT EDIT")
          (emit-eightbol-comment stream
                                (format nil " Source: Source/Code/~a/Banks/**/*.s, Common/Enums.s"
                                        machine))
          (emit-eightbol-blank stream)
          (maphash (lambda (service bank)
                     (emit-eightbol-comment stream (format nil " Service bank ~a ~a" service bank)))
                   service->bank))))))
