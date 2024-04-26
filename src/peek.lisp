(in-package :skyline-tool)

(defun find-label-from-files (goal
                              &optional (low-bank-pathname #p"Object/Bank02.Public.NTSC.o.LABELS.txt")
                                        (last-bank-pathname #p"Object/Bank3f.Public.NTSC.o.LABELS.txt"))
  (dolist (pathname (list low-bank-pathname last-bank-pathname))
    (with-input-from-file (labeled pathname :if-does-not-exist :error)
      (loop for line = (read-line labeled nil nil)
            while line
            do (destructuring-bind (label$ value$$) (split-sequence #\= line)
                 (let* ((label (string-trim #(#\Space) label$))
                        (value$ (string-trim #(#\Space) value$$)))
                   (when (string= label goal)
                     (return-from find-label-from-files
                       (if (char= #\$ (char value$ 0))
                           (+ 0 (parse-integer (subseq value$ 1) :radix 16))
                           (+ 0 (parse-integer value$))))))))))
  nil)

(defun dump-peek (label &optional (dump (load-dump-into-mem)))
  (when-let ((address (etypecase label
                        (string (find-label-from-files label))
                        (number label))))
    (values (elt dump address) address)))

(defun ppeek (label &optional (dump (load-dump-into-mem)))
  (multiple-value-bind (value address) (dump-peek label dump)
    (format t "~a @ $~2,'0x = [byte][ %~8,'0b = $~2,'0x = &~o = ~d = ~@d ] [word]( $~4,'0x )"
            label address
            value value value value
            (if (plusp (logand #x80 value))
                (1- (- (logxor #xff value)))
                value)
            (+ value (* #x100 (elt dump (1+ address)))))))

(defun error-code ()
  (let ((ec (find-label-from-files "BreakSignal")))
    (format t "~&Error BreakSignal — ~{~a~}"
            (mapcar #'minifont->char
                    (list (dump-peek ec)
                          (dump-peek (+ 1 ec))
                          (dump-peek (+ 2 ec))
                          (dump-peek (+ 3 ec)))))))
