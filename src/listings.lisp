(in-package :skyline-tool)

(defun labels-to-mame (labels-file mame-file)
  "Converts LABELS-FILE into a format MAME can read as “comments” in MAME-FILE."
  (with-output-to-file (mame mame-file :if-exists :supersede)
    (with-input-from-file (labels labels-file)
      (let ((comments (make-hash-table))
            break
            minor-fault)
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
        (format mame "
wp 0140,2,w,{frame > 10},{printf \"Stagehand stack theatens to overflow into TIA/Maria mirrors\"}
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
wp 8000,2,w,{(wpdata & 3f) != 3f},{printf \"Bank switch: now in $%02x (pc $%04x, beamy %d)\", wpdata, pc, beamy; go}
wp 8000,2,w,{(wpdata & 3f) == 3f},{printf \"Tried to bank in LastBank at $8000\"}
wp 5048,1,w,{wpdata == 0},{printf \"Switching context to main thread (tid 0) (pc $%04x, beamy %d)\", pc, beamy; go}
wp 5048,1,w,{wpdata == 1},{printf \"Switching context to NMI thread (tid 1) (pc $%04x, beamy %d)\", pc, beamy; go}
wp 5048,1,w,{wpdata == 2},{printf \"Switching context to Stagehand thread (tid 2) (pc $%04x, beamy %d)\", pc, beamy; go}
wp 5048,1,w,{wpdata == 3},{printf \"Switching context to Script thread (tid 3) (pc $%04x, beamy %d)\", pc, beamy; go}
wp 5048,1,w,{wpdata > 3},{printf \"Switching context to non-existing thread (tid %d) (pc $%04x, beamy %d)\", wpdata, pc, beamy}
wp 2c,1,w,1,{printf \"DPPH write ($%02x)\", wpdata; go}
wp 30,1,w,1,{printf \"DPPL write ($%02x)\", wpdata; go}
bp ~4,'0x,1,{snap \"brk.snap.png\"; save \"brk.core\",0,10000; printf \"BRK handler invoked at $%02x:%04x\", b@(4661),  -2+w@(2+sp)}
bp ~4,'0x,1,{snap \"fault.snap.png\"; save \"fault.core\",0,10000; printf \"Minor Fault invoked at $%02x:%04x\", b@(4661),  -2+w@(1+sp)}
rp {pc<8000},{printf \"Program counter underflow\"}
printf \"\\n\\n\\n\\n\\n\\nReady.\\n(Press <F12> to start game)\"
"
                (or break 0)
                (or minor-fault 0))))))
