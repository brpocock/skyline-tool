(in-package :skyline-tool)

(defun header->string (header mem)
  (cond 
    ((zerop (elt header 1)) nil)
    ((= #x60 (logand #x6f (elt header 1)))
     (let ((string (+ (* #x100 (elt header 2)) (elt header 0)))
           (width (1+ (logxor #x1f (logand #x1f (elt header 3))))))
       (subseq mem string (+ string width))))
    (t nil)))

(defun decode-header (bytes &key silentp)
  (check-type bytes cons)
  (assert (member (length bytes) '(4 5)))
  (cond 
    ((and (zerop (elt bytes 0)) (zerop (elt bytes 1)))
     (multiple-value-prog1
         nil
       (unless silentp
         (format t "~&  end of display list."))))
    ((= #x60 (logand #x6f (elt bytes 1)))
     (multiple-value-prog1 
         (values 5 t (elt bytes 4) (+ (* #x100 (elt bytes 2)) (elt bytes 0)))
       (unless silentp
         (format t "~&  indirect stamp header, write mode ~d, string @ $~4,'0x, x = ~d, palette ~d, width ~d" 
                 (ash (logand #x80 (elt bytes 1)) -7)
                 (+ (* #x100 (elt bytes 2)) (elt bytes 0))
                 (if (< (elt bytes 4) 200)
                     (elt bytes 4)
                     (- (elt bytes 4) #x100))
                 (ash (logand (elt bytes 3) #xe0) -5)
                 (1+ (logxor #x1f (logand #x1f (elt bytes 3))))))))
    (t (progn 
         (unless silentp
           (format t "~&  direct stamp header, "))
         (if (= #x40 (logand #x6f (elt bytes 1)))
             (multiple-value-prog1 
                 (values 5 nil (elt bytes 4) (+ (* #x100 (elt bytes 2)) (elt bytes 0)))
               (unless silentp
                 (format t "extended, write mode ~d, stamp @ $~4,'0x, x = ~d, palette ~d, width ~d"
                         (ash (logand #x80 (elt bytes 1)) -7)
                         (+ (* #x100 (elt bytes 2)) (elt bytes 0))
                         (if (< (elt bytes 4) 168) 
                             (elt bytes 4)
                             (- (elt bytes 4) #x100))
                         (ash (logand (elt bytes 3) #xe0) -5)
                         (1+ (logxor #x1f (logand #x1f (elt bytes 3)))))))
             (multiple-value-prog1
                 (values 4 nil (elt bytes 3) (+ (* #x100 (elt bytes 2)) (elt bytes 0)))
               (unless silentp
                 (format t "stamp @ $~4,'0x, x = ~d, palette ~d, width ~d"
                         (+ (* #x100 (elt bytes 2)) (elt bytes 0))
                         (if (< (elt bytes 3) 168)
                             (elt bytes 3) 
                             (- (elt bytes 3) #x100))
                         (ash (logand (elt bytes 1) #xe0) -5)
                         (1+ (logxor #x1f (logand #x1f (elt bytes 1))))))))))))

(defun string->hex (string)
  "Parse STRING as a sequence of hex values and return a list of byte values"
  (loop for i from 0 by (if (find #\space string) 3 2)
        while (< i (length string))
        collecting (parse-integer (subseq string i (+ i 2)) :radix 16)))

(defun decode-hex-header (string)
  (decode-header (string->hex string)))

(defun decode-dll-entry (byte1 byte2 byte3 &key silentp)
  (let ((dl-address (logior (ash byte2 8) byte3))
        (offset (1+ (logand #x0f byte1))))
    (unless silentp
      (format t "~&Display list @ $~4,'0x~@[~*, with DLI~]~
~@[,~* 16 high holey DMA~]~@[~*, 8 high holey DMA~], offset ~d~@[~*, INVALID (bit $10 set)~]"
              dl-address
              (plusp (logand #x80 byte1))
              (plusp (logand #x40 byte1))
              (plusp (logand #x20 byte1))
              offset
              (plusp (logand #x10 byte1))))
    (list (unless (zerop dl-address) dl-address)
          offset)))

(defun decode-dll-hex (string)
  (let ((bytes (string->hex string)))
    (loop for i from 0 below (length bytes) by 3
          do (apply #'decode-dll-entry (subseq bytes i (+ 3 i))))))

(defun hex-dump (bytes)
  (when bytes
    (format t "~{~&    > ~2,'0x~^ ~2,'0x~^ ~2,'0x~^ ~2,'0x~^~
~19t~2,'0x~^ ~2,'0x~^ ~2,'0x~^ ~2,'0x~^~
~32t~2,'0x~^ ~2,'0x~^ ~2,'0x~^ ~2,'0x~^~
~46t~2,'0x~^ ~2,'0x~^ ~2,'0x~^ ~2,'0x~}" (coerce bytes 'list))))

(defun decode-display-list (mem &key (offset 0))
  (loop with dl-entry = offset
        for header = (coerce (subseq mem dl-entry (+ 5 dl-entry)) 'list)
        for dl-increment = (decode-header header)
        while (and dl-increment (< (+ dl-entry dl-increment) (length mem)))
        do (let ((string (header->string header mem)))
             (when (> dl-entry (+ offset 512))
               (format t "~&  — list truncated~%")
               (return-from decode-display-list nil))
             (hex-dump string)
             (when (and (not (emptyp string))
                        (every (lambda (byte) (< byte #x40)) string)) 
               (format t "~&    =“~a”" (minifont->unicode string)))
             (incf dl-entry dl-increment))))

(defun decode-dll-deeply (mem &optional (start-address 0))
  (format t "DLL starting at $~4,'0x" start-address)
  (loop with y = 20
        for dll-address from start-address by 3
        for (dll-pointer offset) = (apply #'decode-dll-entry
                                          (coerce (subseq mem dll-address
                                                          (+ 3 dll-address))
                                                  'list))
        do (incf y offset)
        while (and (< dll-address (+ start-address 511))
                   (<= (- y offset) 262))
        do (progn (format t " (Y = ~d…~d)" (1+ (- y offset)) y)
                  (when dll-pointer
                    (decode-display-list mem :offset dll-pointer)))))

(defun detect-active-dll (&optional (dump-file #p"/tmp/dump"))
  (ecase (elt (etypecase dump-file
                (string (load-dump-into-mem dump-file))
                (vector dump-file)
                (t (load-dump-into-mem dump-file)))
              (find-label-from-files "ActiveDLL"))
    (#x80 (find-label-from-files "AltDLL"))
    (0 (find-label-from-files "DLL"))))

(defun decode-dll-from-dump (&optional (dump-file #p"/tmp/dump")
                                       (start-address (detect-active-dll dump-file)))
  (decode-dll-deeply (load-dump-into-mem dump-file) start-address))

#+mcclim
(defun show-dll-from-dump (&optional (dump-file #p"/tmp/dump")
                                     (start-address (detect-active-dll dump-file)))
  (clim-sys:make-process (lambda ()
                           (clim-simple-interactor:run-in-simple-interactor
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

(defun compare-dlls-from-dumps (&optional (dump1 "/tmp/dump") (dump2 "/tmp/dump2")
                                          (start-address-1 (detect-active-dll dump1))
                                          (start-address-2 (detect-active-dll dump2)))
  "Compare (in Meld) two Display List Lists from core dumps (defaults /tmp/dump & /tmp/dump2)"
  (sb-ext:run-program "/usr/bin/meld" (list (dump-to-text dump1 start-address-1)
                                            (dump-to-text dump2 start-address-2))))

(defun dl-contains-entry-p (mem dl-entry-goal &key offset)
  (loop with dl-entry = offset
        for header = (coerce (subseq mem dl-entry (+ 5 dl-entry)) 'list)
        for dl-increment = (decode-header header :silentp t)
        while (and dl-increment (< (+ dl-entry dl-increment) (length mem)))
        when (= dl-entry dl-entry-goal)
          do (return t)
        when (or (> dl-entry (+ offset 512)) (> dl-entry (- (length mem) #x10)))
          do (return nil)
        do (incf dl-entry dl-increment)
        finally (return nil)))

(defun dll-can-reach-dl-entry-p (mem dl-entry-pointer)
  (dolist (start-address '(#x1800 #x1880))
    (loop with y = 0
          for dll-address from start-address by 3
          for (dll-pointer offset) = (apply #'decode-dll-entry
                                            (append (coerce (subseq mem dll-address
                                                                    (+ 3 dll-address))
                                                            'list)
                                                    (list :silentp t)))
          do (incf y offset)
          while (and (< dll-address (+ start-address 511))
                     (<= (- y offset) 262)
                     (< dll-address #xfff0))
          do (when (and dll-pointer
                        (dl-contains-entry-p mem dl-entry-pointer :offset dll-pointer))
               (return-from dll-can-reach-dl-entry-p start-address))))
  nil)

