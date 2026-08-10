(in-package :skyline-tool)

(defun header->string (HEADER MEM)
  "Extract string data from HEADER in MEM for indirect-mode display list entries.

@table @asis
@item HEADER
Five-byte display list header (list or vector)
@item MEM
Full memory dump as a byte vector
@item Returns
Substring from MEM at the address specified by HEADER, or NIL
@end table"
  (cond 
    ((zerop (elt HEADER 1)) nil)
    ((= #x60 (logand #x6f (elt HEADER 1)))
     (let ((string (+ (* #x100 (elt HEADER 2)) (elt HEADER 0)))
            (width (1+ (logxor #x1f (logand #x1f (elt HEADER 3))))))
        (subseq MEM string (+ string width))))
    (t nil)))

(defun decode-header (BYTES &key SILENTP)
  "Decode a 4- or 5-byte 7800 display list header BYTES.

@table @asis
@item BYTES
List of 4 or 5 byte values representing a display list header
@item SILENTP
If true, suppress diagnostic output
@item Returns
Multiple values: (header-length indirectp xpos address) or NIL for end-of-list
@end table"
  (check-type BYTES cons)
  (assert (member (length BYTES) '(2 4 5)))
  (cond 
    ((and (zerop (elt BYTES 0)) (zerop (elt BYTES 1)))
     (multiple-value-prog1
         nil
       (unless SILENTP
         (format t "~&  end of display list."))))
    ((= #x60 (logand #x6f (elt BYTES 1)))
     (multiple-value-prog1 
         (values 5 t (elt BYTES 4) (+ (* #x100 (elt BYTES 2)) (elt BYTES 0)))
       (unless SILENTP
         (format t "~&  indirect stamp header, write mode ~d, string @ $~4,'0x, x = ~d, palette ~d, width ~d" 
                 (ash (logand #x80 (elt BYTES 1)) -7)
                 (+ (* #x100 (elt BYTES 2)) (elt BYTES 0))
                 (if (< (elt BYTES 4) 200)
                     (elt BYTES 4)
                     (- (elt BYTES 4) #x100))
                 (ash (logand (elt BYTES 3) #xe0) -5)
                 (1+ (logxor #x1f (logand #x1f (elt BYTES 3))))))))
    (t (progn 
         (unless SILENTP
           (format t "~&  direct stamp header, "))
         (if (= #x40 (logand #x6f (elt BYTES 1)))
             (multiple-value-prog1 
                 (values 5 nil (elt BYTES 4) (+ (* #x100 (elt BYTES 2)) (elt BYTES 0)))
               (unless SILENTP
                 (format t "extended, write mode ~d, stamp @ $~4,'0x, x = ~d, palette ~d, width ~d"
                         (ash (logand #x80 (elt BYTES 1)) -7)
                         (+ (* #x100 (elt BYTES 2)) (elt BYTES 0))
                         (if (< (elt BYTES 4) 168) 
                             (elt BYTES 4)
                             (- (elt BYTES 4) #x100))
                         (ash (logand (elt BYTES 3) #xe0) -5)
                         (1+ (logxor #x1f (logand #x1f (elt BYTES 3)))))))
             (multiple-value-prog1
                 (values 4 nil (elt BYTES 3) (+ (* #x100 (elt BYTES 2)) (elt BYTES 0)))
               (unless SILENTP
                 (format t "stamp @ $~4,'0x, x = ~d, palette ~d, width ~d"
                         (+ (* #x100 (elt BYTES 2)) (elt BYTES 0))
                         (if (< (elt BYTES 3) 168)
                             (elt BYTES 3) 
                             (- (elt BYTES 3) #x100))
                         (ash (logand (elt BYTES 1) #xe0) -5)
                         (1+ (logxor #x1f (logand #x1f (elt BYTES 1))))))))))))

(defun string->hex (string)
  "Parse STRING as a sequence of hex values and return a list of byte values"
  (loop for i from 0 by (if (find #\space string) 3 2)
        while (< i (length string))
        collecting (parse-integer (subseq string i (+ i 2)) :radix 16)))

(defun decode-hex-header (string)
  (decode-header (string->hex string)))

(defun decode-dll-entry (BYTE1 BYTE2 BYTE3 &key SILENTP)
  "Decode a 3-byte Display List List (DLL) entry into address and offset.

@table @asis
@item BYTE1
DLL control byte (bit 7 = DLI, bits 6-5 = holey DMA, bits 3-0 = offset)
@item BYTE2
High byte of display list address
@item BYTE3
Low byte of display list address
@item SILENTP
If true, suppress diagnostic output
@item Returns
List of (dl-address offset), where dl-address may be NIL for a null entry
@end table"
  (let ((dl-address (logior (ash BYTE2 8) BYTE3))
        (offset (1+ (logand #x0f BYTE1))))
    (unless SILENTP
      (format t "~&Display list @ $~4,'0x~@[~*, with DLI~]~
~@[,~* 16 high holey DMA~]~@[~*, 8 high holey DMA~], offset ~d~@[~*, INVALID (bit $10 set)~]"
              dl-address
              (plusp (logand #x80 BYTE1))
              (plusp (logand #x40 BYTE1))
              (plusp (logand #x20 BYTE1))
              offset
              (plusp (logand #x10 BYTE1))))
    (list (unless (zerop dl-address) dl-address)
          offset)))

(defun decode-dll-hex (STRING)
  "Decode display list entries from a STRING of space-separated hex bytes.

@table @asis
@item STRING
Space-separated hex byte values (e.g. @code{\"80 18 00 A0 18 80\"})
@end table"
  (let ((bytes (string->hex string)))
    (loop for i from 0 below (length bytes) by 3
          do (apply #'decode-dll-entry (subseq bytes i (+ 3 i))))))

(defun hex-dump (bytes)
  (when bytes
    (format t "~{~&    > ~2,'0x~^ ~2,'0x~^ ~2,'0x~^ ~2,'0x~^~
~19t~2,'0x~^ ~2,'0x~^ ~2,'0x~^ ~2,'0x~^~
~32t~2,'0x~^ ~2,'0x~^ ~2,'0x~^ ~2,'0x~^~
~46t~2,'0x~^ ~2,'0x~^ ~2,'0x~^ ~2,'0x~}" (coerce bytes 'list))))

(defun decode-display-list (MEM &key (OFFSET 0))
  "Decode a 7800 display list from MEM starting at OFFSET.

@table @asis
@item MEM
Memory dump as byte vector
@item OFFSET
Starting address within MEM (default 0)
@item Returns
NIL; outputs display list contents to @code{*standard-output*}
@end table"
  (loop with dl-entry = OFFSET
        for header = (coerce (subseq MEM dl-entry (+ 5 dl-entry)) 'list)
        for dl-increment = (decode-header header)
        while (and dl-increment (< (+ dl-entry dl-increment) (length MEM)))
        do (let ((string (header->string header MEM)))
             (when (> dl-entry (+ OFFSET 512))
               (format t "~&  — list truncated~%")
               (return-from decode-display-list nil))
             (hex-dump string)
             (when (and (not (emptyp string))
                        (every (lambda (byte) (< byte #x80)) string)) 
               (format t "~&    =“~a”" (minifont->unicode string)))
             (incf dl-entry dl-increment))))

(defun decode-dll-deeply (MEM &optional (START-ADDRESS 0))
  "Recursively decode a Display List List (DLL) from MEM starting at START-ADDRESS.

@table @asis
@item MEM
Memory dump as byte vector
@item START-ADDRESS
Starting DLL address within MEM (default 0)
@item Returns
NIL; outputs decoded DLL and display list contents to @code{*standard-output*}
@end table"
  (format t "DLL starting at $~4,'0x" START-ADDRESS)
  (loop with y = 20
        for dll-address from START-ADDRESS by 3
        for (dll-pointer offset) = (apply #'decode-dll-entry
                                          (coerce (subseq MEM dll-address
                                                          (+ 3 dll-address))
                                                  'list))
        do (incf y offset)
        while (and (< dll-address (+ START-ADDRESS 511))
                   (<= (- y offset) 262))
        do (progn (format t " [DLL $~4,'0x] (Y = ~d…~d)"
                          dll-address
                          (1+ (- y offset)) y)
                  (when dll-pointer
                    (decode-display-list MEM :offset dll-pointer)))))

(defun detect-active-dll (&optional (DUMP-FILE #p"/tmp/dump"))
  "Determine which DLL (primary or alternate) is active in DUMP-FILE.

@table @asis
@item DUMP-FILE
Path to core dump file (default @file{/tmp/dump})
@item Returns
Address of the active DLL (#x1800 or #x1880)
@end table"
  (ecase (elt (etypecase DUMP-FILE
                (string (load-dump-into-mem DUMP-FILE))
                (vector DUMP-FILE)
                (t (load-dump-into-mem DUMP-FILE)))
              (find-label-from-files "ActiveDLL"))
    (#x80 (find-label-from-files "AltDLL"))
    (0 (find-label-from-files "DLL"))))

(defun decode-dll-from-dump (&optional (dump-file #p"/tmp/dump")
                                       (start-address (detect-active-dll dump-file)))
  (decode-dll-deeply (load-dump-into-mem dump-file) start-address))

(defun show-dll-from-dump (&optional (dump-file #p"/tmp/dump")
                                     (start-address (detect-active-dll dump-file)))
  (clim-sys:make-process (lambda ()
                           (clim-simple-echo:run-in-simple-echo
                            (lambda () (decode-dll-from-dump dump-file start-address))
                            :process-name "Decode DLL from Dump"
                            :window-title "Decode DLL from Dump"
                            :width 650 :height 800))
                         :name "Decode DLL from Dump"))

(defun pathname-string (pathname)
  (format nil "~a" pathname))

(defun dump-to-text (dump start-address)
  (let ((dump.txt (make-pathname :defaults dump
                                 :directory "/tmp/"
                                 :type "txt")))
    (with-output-to-file (*standard-output* dump.txt :if-exists :supersede)
      (decode-dll-from-dump dump start-address))
    (pathname-string dump.txt)))

(defun copy-dump-as-dump2 ()
  "Make a copy of /tmp/dump as /tmp/dump2 for later comparison"
  (copy-file "/tmp/dump" "/tmp/dump2" :if-to-exists :supersede)
  (clim-simple-echo:run-in-simple-echo
   (lambda ()
     (format t "/tmp/dump copied to /tmp/dump2. You can use it for compares later."))
   :process-name "Core dump copied"
   :height 80))

(defun compare-dlls-from-dumps (&optional (dump1 "/tmp/dump") (dump2 "/tmp/dump2")
                                          (start-address-1 (detect-active-dll dump1))
                                          (start-address-2 (detect-active-dll dump2)))
  "Compare (in Meld) two Display List Lists from core dumps (defaults /tmp/dump & /tmp/dump2)"
  (sb-ext:run-program "/usr/bin/meld" (list (dump-to-text dump1 start-address-1)
                                            (dump-to-text dump2 start-address-2))))

(defun dl-contains-entry-p (MEM DL-ENTRY-GOAL &key OFFSET)
  "Return true if the display list in MEM at OFFSET contains DL-ENTRY-GOAL.

@table @asis
@item MEM
Memory dump as byte vector
@item DL-ENTRY-GOAL
Target display list entry address to search for
@item OFFSET
Starting offset within MEM for display list (default 0)
@item Returns
T if the entry is found, NIL otherwise
@end table"
  (loop with dl-entry = OFFSET
        for header = (coerce (subseq MEM dl-entry (+ 5 dl-entry)) 'list)
        for dl-increment = (decode-header header :silentp t)
        while (and dl-increment (< (+ dl-entry dl-increment) (length MEM)))
        when (= dl-entry DL-ENTRY-GOAL)
          do (return t)
        when (or (> dl-entry (+ OFFSET 512)) (> dl-entry (- (length MEM) #x10)))
          do (return nil)
        do (incf dl-entry dl-increment)
        finally (return nil)))

(defun dll-can-reach-dl-entry-p (MEM DL-ENTRY-POINTER)
  "Return the DLL start address if DL-ENTRY-POINTER is reachable from either DLL in MEM.

@table @asis
@item MEM
Memory dump as byte vector
@item DL-ENTRY-POINTER
Target display list entry address to check
@item Returns
Starting DLL address (#x1800 or #x1880) if reachable, NIL otherwise
@end table"
  (dolist (start-address '(#x1800 #x1880))
    (loop with y = 0
          for dll-address from start-address by 3
          for (dll-pointer offset) = (apply #'decode-dll-entry
                                            (append (coerce (subseq MEM dll-address
                                                                    (+ 3 dll-address))
                                                            'list)
                                                    (list :silentp t)))
          do (incf y offset)
          while (and (< dll-address (+ start-address 511))
                     (<= (- y offset) 262)
                     (< dll-address #xfff0))
          do (when (and dll-pointer
                        (dl-contains-entry-p MEM DL-ENTRY-POINTER :offset dll-pointer))
               (return-from dll-can-reach-dl-entry-p start-address))))
  nil)

