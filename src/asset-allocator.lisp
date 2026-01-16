(in-package :skyline-tool)

(defvar *bank*)
(defvar *last-bank*)

(defun parse-assets-line (line)
  "Parse one LINE from Assets.index"
  (if (or (emptyp (string-trim " " line))
          (char= #\; (char (string-trim " " line) 0)))
      (list nil nil)
      (destructuring-bind (asset &optional builds-string)
          (split-sequence #\space line :remove-empty-subseqs t)
        (list (string-trim " " asset)
              (if (null builds-string)
                  (list "AA" "Public" "Demo")
                  (remove-if #'null
                             (list
                              (when (find #\A builds-string :test #'char-equal)
                                "AA")
                              (when (find #\P builds-string :test #'char-equal)
                                "Public")
                              (when (find #\D builds-string :test #'char-equal)
                                "Demo"))))))))

(defun kind-by-name (kind$)
  (cond
    ((or (equal kind$ "Songs")
         (equal kind$ "Song"))
     :song)
    ((equal kind$ "Art") :art)
    ((or (equal kind$ "Blobs")
         (equal kind$ "Blob"))
     :blob)
    ((or (equal kind$ "Maps")
         (equal kind$ "Map"))
     :map)
    ((or (equal kind$ "Scripts")
         (equal kind$ "Script"))
     :script)
    ((or (null kind$)
	 (emptyp kind$))
     nil)
    (t (error "Unrecognized asset kind: ~a" kind$))))

(defun asset-kind/name (asset)
  (when asset
    (list (subseq asset 0 (position #\/ asset))
          (subseq asset (1+ (position #\/ asset))))))

(defun kind-of-asset (indicator)
  "Return the keyword for the kind of asset indicated by INDICATOR"
  (kind-by-name (first (asset-kind/name indicator))))

(defvar *assets-list* nil)
(defvar *asset-ids-seen* nil)

(defun make-seen-ids-table ()
  (let ((seen-ids (make-hash-table)))
    (dolist (kind '(:song :map :script :blob))
      (setf (gethash kind seen-ids) (make-hash-table)))
    seen-ids))

(defun interpret-line-from-assets-list (line &key seen-ids index-hash)
  (tagbody top
     (destructuring-bind (asset builds) (parse-assets-line line)
       (when asset
         (destructuring-bind (kind$ name) (asset-kind/name asset)
           (let* ((kind (kind-by-name kind$))
                  (id (get-asset-id kind name)))
             (assert id (id)
                     "Could not find the asset ID for ~(~a~) “~a”"
                     kind name)
             (if-let (existing (gethash id (gethash kind seen-ids)))
               (restart-case
                   (error "Two ~(~a~)s (at least) have the same ID: “~a” and “~a”~:[ (both nil)~;~:* (both $~x)~]"
                          kind existing name id)
                 (reload-assets ()
                   :report "Reload the assets to check for changed IDs"
                   (setf *maps-ids* nil
                         *assets-list* nil
                         *asset-ids-seen* nil)
                   (go top)))
               (setf (gethash id (gethash kind seen-ids)) name)))))
       (setf (gethash asset index-hash) builds))))

(defun read-assets-list (&optional (index-file #p"Source/Assets.index"))
  "Read Assets.index from INDEX-FILE (using *ASSETS-LIST* cache)"
  (when (and *assets-list* *asset-ids-seen*)
    (return-from read-assets-list
      (values *assets-list* *asset-ids-seen*)))
  (format *trace-output* "~&Reading assets index from ~a…"
          (enough-namestring index-file))
  (let ((index-hash (make-hash-table :test 'equal))
        (seen-ids (make-seen-ids-table)))
    (with-input-from-file (index index-file)
      (loop for line = (read-line index nil nil)
            while line
            do (interpret-line-from-assets-list line
                                                :seen-ids seen-ids :index-hash index-hash)))
    (setf *assets-list* index-hash
          *asset-ids-seen* seen-ids)
    (values index-hash seen-ids)))

(defun filter-assets-for-build (index-hash build)
  "Select only the assets from INDEX-HASH which are for the selected BUILD"
  (loop for asset being the hash-keys of index-hash
        when (member build (gethash asset index-hash) :test #'equal)
          collect asset))

(defun existing-object-file (file-name)
  "Asset that file FILE-NAME exists"
  (assert (probe-file file-name) (file-name)
          "Object file not found: “~a”" (enough-namestring file-name))
  file-name)

(defun asset-file (asset &key video)
  "The filename of the object file indicated by ASSET, in format for VIDEO."
  (existing-object-file (asset->object-name asset :video video)))

(defun song-asset-p (asset)
  "Is ASSET a song?"
  (eql :song (kind-of-asset asset)))

(defun script-asset-p (asset)
  "Is ASSET a script?"
  (eql :script (kind-of-asset asset)))

(defun map-asset-p (asset)
  "Is ASSET a map?"
  (eql :map (kind-of-asset asset)))

(defun blob-asset-p (asset)
  "Is ASSET a BLOB?"
  (eql :blob (kind-of-asset asset)))

(defgeneric asset-loader-size (kind record-count)
  (:method ((kind (eql :overhead)) record-count)
    (ecase *machine*
      (7800 12)
      ((35902 20953 9918 1000 3010 837 3 6 7 264 8 9 10) ; New platforms - placeholder sizes
       (error "~A asset loader overhead size not yet implemented" *machine*))))
  (:method ((kind (eql :song)) record-count)
    (ecase *machine*
      (7800 256) ; FIXME #124
      ((35902 20953 9918 1000 3010 837 3 6 7 264 8 9 10) ; New platforms - placeholder sizes
       (error "~A song asset loader size not yet implemented" *machine*))))
  (:method ((kind (eql :script)) record-count)
    (ecase *machine*
      (7800 (+ 128 (* (1+ record-count) 4)))
      ((35902 20953 9918 1000 3010 837 3 6 7 264 8 9 10) ; New platforms - placeholder sizes
       (error "~A script asset loader size not yet implemented" *machine*))))
  (:method ((kind (eql :blob)) record-count)
    (ecase *machine*
      (7800 (+ 284 1 (* record-count 3)))
      ((35902 20953 9918 1000 3010 837 3 6 7 264 8 9 10) ; New platforms - placeholder sizes
       (error "~A blob asset loader size not yet implemented" *machine*))))
  (:method ((kind (eql :map)) record-count)
    (ecase *machine*
      (7800 (+
             #|LoadMap|# 1024 #| approx XXX |#
             #| end of table |# 1
             #|per record|# (* record-count 3)))
      ((35902 20953 9918 1000 3010 837 3 6 7 264 8 9 10) ; New platforms - placeholder sizes
       (error "~A map asset loader size not yet implemented" *machine*)))))

(defun bank-size (asset-size-hash)
  "The size of the ROM bank indicated by ASSET-SIZE-HASH plus overhead."
  (let ((assets (hash-table-keys asset-size-hash)))
    (+ (asset-loader-size :overhead (length assets))
       (loop for kind in (remove-duplicates
                          (mapcar #'kind-of-asset assets))
             sum (asset-loader-size kind
                                    (count-if (lambda (x)
                                                (eql kind (kind-of-asset x)))
                                              assets)))
       (loop for asset in (hash-table-keys asset-size-hash)
             sum (gethash asset asset-size-hash)))))

(defun best-permutation (permutations)
  "Find the best of PERMUTATIONS to fit into the smallest number of ROM banks."
  (loop with optimal-count = most-positive-fixnum
        with optimal-assets = nil
        for sequence being the hash-keys of permutations
        for banks = (gethash sequence permutations)
        for bank-count = (length (hash-table-keys banks))
        when (< bank-count optimal-count)
          do (setf optimal-count bank-count
                   optimal-assets banks)
        finally (return optimal-assets)))

(defun size-of-banks ()
  "Size of each ROM bank in bytes."
  (ecase *machine*
    (2600 #x1000)
    (7800 #x4000)))

(defun try-allocation-sequence (sequence file-sizes &key video)
  (tagbody top
     (loop with banks = (make-hash-table :test 'equal)
           with bank = 0
           with bank-assets = (make-hash-table :test 'equal)
           for asset in sequence
           for asset-file = (asset-file asset :video video)
           for asset-size = (gethash asset-file file-sizes)
           for tentative-bank = (let ((tentative-bank (copy-hash-table bank-assets)))
                                  (setf (gethash asset tentative-bank) asset-size)
                                  tentative-bank)
           when (null asset-size)
             do (progn
                  (cerror "Pretend it's 8kiB"
                          "Did not get size of asset file~%~8t“~a” (for ~a)" asset-file asset)
                  8192)
           when (zerop asset-size)
             do (restart-case
                    (error "Asset file is empty~%~8t“~a” is empty (for asset ~a)" asset-file asset)
                  (continue () :report "Pretend it's 8kiB"
                    8192)
                  (make-file () :report "Re-run “make” for file"
                    (uiop:run-program (list "make" asset-file "AUTOCONTINUE=t")
                                      :ignore-error-status t)
                    (go top)))
           if (< (bank-size tentative-bank) (size-of-banks))
             do (setf bank-assets tentative-bank)
           else
             do (setf (gethash bank banks) bank-assets
                      bank-assets (make-hash-table :test 'equal)
                      (gethash asset bank-assets) asset-size
                      bank (1+ bank))
           finally (progn
                     (setf (gethash bank banks)
                           (when (plusp (hash-table-count bank-assets))
                             bank-assets))
                     (return-from try-allocation-sequence banks)))))

(defun compute-asset-size (asset-file &key file-sizes)
  (let ((n (cond ((equal "o" (pathname-type asset-file))
                  (ql-util:file-size asset-file))
                 ((equal "s" (pathname-type asset-file))
                  (assemble-file-for-size asset-file))
                 (t (cerror "Pretend asset size is 8kiB"
                            "Don't know how to estimate size of “~a”"
                            (enough-namestring asset-file))
                    8192))))
    (when file-sizes
      (if (< (* 12 1024) n)
          (warn "Asset file “~a” is over 12kiB (~~~:dkiB) and will not be included"
                (enough-namestring asset-file)
                (round n 1024))
          (setf (gethash asset-file file-sizes) n)))
    n))

(defun find-best-allocation (assets &key build video)
  (format *trace-output*
          "~&Finding best allocation for ~:d asset~:p (build ~s, video ~s)"
          (length assets) build video)
  (let ((file-sizes (make-hash-table :test 'equal)))
    (dolist (asset assets)
      (let ((asset-file (asset-file asset :video video)))
        (compute-asset-size asset-file :file-sizes file-sizes)
        (unless (gethash asset-file file-sizes)
          (warn "Removing asset “~a” from consideration" asset)
          (removef assets asset))))
    (let ((available-banks (- (number-of-banks build video)
                              (first-assets-bank build)
                              (if (= #x40 (number-of-banks build video))
                                  2
                                  1)))
          (tries 0))
      (format *trace-output*
              "~&Will try every possible permutation to find one that fits into ~:d ROM bank~:p … "
              available-banks)
      (map-permutations
       (lambda (sequence)
         (incf tries)
         (let ((try (try-allocation-sequence sequence file-sizes
                                             :video video)))
           (when (<= (hash-table-count try) available-banks)
             (format *trace-output* " got a fit in ~:d tr~:@p" tries)
             (return-from find-best-allocation (values try file-sizes)))))
       assets)
      (error "Unable to fit ~:d asset~:p into ~:d bank~:p of ROM, tried ~:d permutation~:p"
             (length assets) available-banks tries))))

(define-constant +all-builds+ '("AA" "Public" "Demo")
  :test #'equalp)

(define-constant +all-video+ '("NTSC" "PAL")
  :test #'equalp)

(defun supported-video-types (machine)
  "Return the list of video types supported by MACHINE.
   Filters out unsupported video types for specific machines."
  (if machine
      (case machine
        ;; Atari 5200: NTSC only, no PAL or SECAM
        (5200 '("NTSC"))
        ;; Atari 7800: NTSC and PAL, no SECAM
        (7800 '("NTSC" "PAL"))
        ;; ZX81/T/S-1000: NTSC only
        (81 '("NTSC"))
        ;; ZX Spectrum/T/S-2068: NTSC only
        (2068 '("NTSC"))
        ;; Most other machines support NTSC and PAL
        (otherwise +all-video+))
      +all-video+))

(defvar *first-assets-bank* nil)

(defun first-assets-bank (build)
  (or *first-assets-bank*
      (setf *first-assets-bank*
            (loop for bank from 0
                  for bank-name = (format nil "Bank~(~2,'0x~)" bank)
                  unless (probe-file (make-pathname :directory (list :relative
                                                                     "Source"
                                                                     "Banks"
                                                                     bank-name)
                                                    :name bank-name
                                                    :type "s"))
                    return bank))))

(defun allocation-list-name (bank build video)
  (make-pathname :directory '(:relative "Source" "Generated")
                 :name (format nil "Bank~(~2,'0x~).~a.~a"
                               bank
                               build video)
                 :type "list"))

(defun allocation-size-name (bank build video)
  (make-pathname :directory '(:relative "Source" "Generated")
                 :name (format nil "Bank~(~2,'0x~).~a.~a"
                               bank
                               build video)
                 :type "size"))

(defun allocate-assets (build &optional (*machine* 7800))
  "allocate the banks for assets for BUILD (AA, Public, &c)"
  (assert (member build +all-builds+ :test 'equal) (build)
          "BUILD must be one of ~{~a~^ or ~} not “~a”" +all-builds+ build)
  (let ((assets-list (all-assets-for-build build)))
    (dolist (video (supported-video-types *machine*))
      (format *trace-output* "~&Writing asset list files for ~a ~a: Bank "
              build video)
      (loop with allocation = (find-best-allocation assets-list
                                                    :build build :video video)
            for bank-offset being the hash-keys of allocation
            for bank = (+ (first-assets-bank build) bank-offset)
            for assets = (gethash bank-offset allocation)
            for allocation-list-name = (allocation-list-name bank build video)
            for allocation-size-name = (allocation-size-name bank build video)
            unless (and assets (plusp (hash-table-count assets)))
              do (error "No assets assigned to bank ~(~2,'0x~)" bank)
            do (ensure-directories-exist allocation-list-name)
            do (with-output-to-file (allocation-file allocation-list-name
                                                     :if-exists :supersede)
                 (format *trace-output* " $~(~2,'0x~) (#~:*~d; ~:d asset~:p) "
                         bank (length (hash-table-keys assets)))
                 (format allocation-file "~{~a~%~}" (hash-table-keys assets)))
            do (ensure-directories-exist allocation-size-name)
            do (with-output-to-file (allocation-file allocation-size-name
                                                     :if-exists :supersede)
                 (format allocation-file "~{~&~a	~d~}~2%@	~d~%"
                         (hash-table-plist assets)
                         (reduce #'+ (hash-table-values assets))))
            finally (when (< (+ (length (hash-table-keys allocation)) (first-assets-bank build))
                             (1- (number-of-banks build video)))
                      (format *trace-output* "~&… and blank asset lists for: Bank ")
                      (let ((empty-banks (list)))
                        (loop for bank from (+ (first-assets-bank build)
                                               (length (hash-table-keys allocation)))
                                below (1- (number-of-banks build video))
                              for allocation-list-name = (allocation-list-name bank build video)
                              for allocation-size-name = (allocation-size-name bank build video)
                              do (ensure-directories-exist allocation-list-name)
                              do (with-output-to-file (allocation-file allocation-list-name
                                                                       :if-exists :supersede)
                                   (push bank empty-banks)
                                   (fresh-line allocation-file))
                              do (ensure-directories-exist allocation-size-name)
                              do (with-output-to-file (allocation-file allocation-size-name
                                                                       :if-exists :supersede)
                                   (format allocation-file "@	0~%")))
                        (format *trace-output* "~{~a~^, ~}"
                                (apply #'compress-sequential-numbers
                                       (sort empty-banks #'<)))))))))

(defun number-of-banks (build video)
  (declare (ignore video))
  (ecase *machine*
    (7800 (cond
            ((equal build "Demo") 64)
            ((equal build "Test") 64)
            (t 64)))))

(defun included-file (line)
  (let ((match (nth-value 1 (cl-ppcre:scan-to-strings "\\.include \"(.*)\\.s\"" line))))
    (when (and match (plusp (array-dimension match 0)))
      (aref match 0))))

(defun included-binary-file (line)
  (let ((match (nth-value 1 (cl-ppcre:scan-to-strings "\\.binary \"(.*)\\.o\"" line))))
    (when (and match (plusp (array-dimension match 0)))
      (aref match 0))))

(defun machine-directory-name ()
  "Return the directory name for the current machine platform"
    (ecase *machine*
      (1 "Oric-1")      ; Oric-1
      (2 "A2")    ; Apple ][
      (3 "NES")   ; NES
      (6 "SNES")  ; SNES
      (7 "BBC")   ; BBC
      (8 "A2")    ; Apple II
      (9 "A3")    ; Apple ///
      (10 "2gs")  ; Apple //gs
      (16 "TG16") ; TG-16
      (20 "C16")  ; VIC-20
      (23 "A2e")  ; Apple //e
      (64 "CBM")  ; C=64
      (88 "SNES") ; SNES (duplicate, but ok)
      (128 "CBM") ; C=128 (same as C=64)
      (200 "Lynx")    ; Lynx
      (222 "2gs") ; Apple //gs (duplicate)
      (223 "BBC") ; BBC (duplicate)
      (264 "C16") ; C=16 & Plus/4 (duplicate)
      (837 "GG")   ; Game Gear
      (1000 "SG1000") ; SG-1000
      (1601 "SMD") ; Genesis/MegaDrive
      (2600 "2600") ; VCS
      (2609 "Intv") ; Intellivision
      (3010 "SMS") ; Master System
      (81 "ZX81") ; ZX81/T/S-1000
      (2068 "Spectrum") ; Spectrum/T/S-2068
      (5200 "5200") ; SuperSystem
      (7800 "7800") ; ProSystem
      (9918 "Clcv") ; ColecoVision
      (20953 "DMG") ; Game Boy
      (35902 "GBC") ; Game Boy Color
      ))

(defun include-paths-for-current-bank (&key cwd testp)
  (let* ((bank (if (= *bank* *last-bank*)
                   "LastBank"
                   (format nil "Bank~(~2,'0x~)" *bank*)))
         (machine-dir (machine-directory-name))
         (includes (list (list :relative "Source")
                         (list :relative "Source" "Common")
                         (list :relative "Source" "Routines")
                         (list :relative "Source" "Classes")
                         (list :relative "Source" "Stagehand")
                         (list :relative "Object")
                         (list :relative "Object" "Assets")
                         (list :relative "Source" "Generated")
                         (list :relative "Source" "Generated" "Assets"))))
    ;; Add platform-specific directories
    (when machine-dir
      (appendf includes (list (list :relative "Source" machine-dir)))
      ;; also include subdirectories
      (appendf includes (list (list :relative "Source" machine-dir "Common")
                               (list :relative "Source" machine-dir "Platform")
                               (list :relative "Source" machine-dir "Routines"))))
    (when cwd (appendf includes (list (pathname-directory cwd))))
    (when testp (appendf includes (list (list :relative "Source" "Tests"))))
    ;; also include platform tests when testing
      (appendf includes (list (list :relative "Source" machine-dir "Tests")))
    (when (probe-file (make-pathname :directory (list :relative "Source" "Banks" bank)
                                     :name bank :type "s"))
      (appendf includes (list (list :relative "Source" "Banks" bank))))
    includes))

(defun generated-path (path)
  (let ((platform-dir (machine-directory-name)))
    (cond
      ((equalp path '(:relative "Source" "Common"))
       (list :relative "Source" "Generated" platform-dir "Common"))
      ((equalp (subseq path 0 3) '(:relative "Source" "Banks"))
       (append (list :relative "Source" "Generated" platform-dir) (subseq path 3)))
      (t (error "Don't know how to find a generated path from ~a" path)))))

(defun write-blob-generation (pathname)
  (let ((blob-name (pathname-name pathname))
        (blob-path (enough-namestring pathname))
        (machine-dir (machine-directory-name)))
    (ecase *machine*
      (200 ; Lynx
       (format t "~%
Source/Generated/~a/Assets/Blob.Lynx/~a.s: ~a \\
~10tbin/skyline-tool
	mkdir -p Source/Generated/~a/Assets
	bin/skyline-tool --port Lynx dispatch-png $< Source/Generated/~a/Assets"
               machine-dir blob-name blob-path machine-dir machine-dir))
      (7800 ; Atari 7800
       (format t "~%
Source/Generated/~a/Assets/Blob.~a.s: ~a\\~%~10tbin/skyline-tool
	mkdir -p Source/Generated/~a/Assets
	bin/skyline-tool blob-rip-7800 $<"
               machine-dir blob-name blob-path machine-dir))
      ((1 2 8 16 20 64 88 128 222 223 264 2609 1601 2600 3010 5200) ; Other supported machines without blob support
       (error "Blob generation not supported for machine ~A (~A)" *machine* (skyline-tool::machine-long-name))))))

(defun write-art-generation (pathname)
  (let ((art-name (pathname-name pathname))
        (art-path (enough-namestring pathname))
        (machine-dir (machine-directory-name)))
    (ecase *machine*
      (200 ; Lynx
       (format t "~%
Object/~a/Assets/Art.~a.o: ~a~%	bin/skyline-tool
	mkdir -p Object/~a/Assets
	bin/skyline-tool compile-art-lynx $@ $<"
               machine-dir art-name art-path machine-dir))
      (7800 ; Atari 7800
       (format t "~%
Object/~a/Assets/Art.~a.o: ~a \\~{~%	~a \\~}~%	bin/skyline-tool
	mkdir -p Object/~a/Assets
	bin/skyline-tool compile-art-7800 $@ $<"
               machine-dir art-name art-path
               (mapcar (compose #'enough-namestring #'second)
                       (read-7800-art-index pathname))
               machine-dir))
      ((1 2 8 16 20 64 88 128 222 223 264 2609 1601 2600 3010 5200) ; Other supported machines without art support
       (error "Art generation not supported for machine ~A (~A)" *machine* (skyline-tool::machine-long-name))))))

(defun write-tsx-generation (pathname)
  (let ((machine-dir (machine-directory-name)))
    (if (and (search "Decals" (pathname-name pathname))
             (not (search "Decals2" (pathname-name pathname))))
        (format t "~%
Object/~a/Assets/Tileset.~a.o: Source/Maps/Tiles/~:*~a.tsx Source/Maps/Tiles/CommonDecals.tsx \\
~10tSource/Maps/Tiles/~:*~a.png Source/Maps/Tiles/CommonDecals.png \\
~10tbin/skyline-tool
	mkdir -p Object/~a/Assets
	bin/skyline-tool compile-tileset $< Source/Maps/Tiles/CommonDecals.tsx"
                machine-dir (pathname-name pathname) machine-dir)
        (format t "~%
Object/~a/Assets/Tileset.~a.o: Source/Maps/Tiles/~:*~a.tsx \\
~10tSource/Maps/Tiles/~:*~a.png \\
~10tbin/skyline-tool
	mkdir -p Object/~a/Assets
	bin/skyline-tool compile-tileset $<"
                machine-dir (pathname-name pathname) machine-dir))))

(defun makefile-contains-target-p (target)
  (let ((target-prefix (concatenate 'string (typecase target
                                              (pathname (enough-namestring target))
                                              (string target)
                                              (t (princ-to-string target)))
                                    ":")))
    (with-input-from-file (makefile #p"Makefile")
      (loop for line = (read-line makefile nil nil)
            while line
            when (eql 0 (search target-prefix line))
              do (return t)))))

(defun find-included-file (name &key cwd testp)
  (let ((generated-asset-pathname
          (make-pathname :directory '(:relative "Source" "Generated" "Assets")
                         :name name :type "s")))
    (when (some (lambda (frag)
                  (eql 0 (search frag name)))
                (list "Song." "Art." "Blob." "Script."))
      (return-from find-included-file generated-asset-pathname)))
  (dolist (path (include-paths-for-current-bank :cwd cwd :testp testp))
    (let ((possible-file (make-pathname :directory path :name name :type "s")))
      (when (probe-file possible-file)
        (return-from find-included-file possible-file))))
  (let ((generated-pathname
          (make-pathname :directory '(:relative "Source" "Generated")
                         :name name :type "s")))
    (when (skyline-tool-writes-p generated-pathname)
      (return-from find-included-file generated-pathname))
    (if (makefile-contains-target-p generated-pathname)
        (return-from find-included-file generated-pathname)))
  (error "Cannot find a possible source for included ~:[source~;test~] ~
file ~a.s in bank $~(~2,'0x~)~
~@[~&Current working directory: ~a~]~
~@[~&TestP: ~a~]"
         testp name *bank* cwd testp))

(defun find-included-binary-file (name)
  (when (search "StagehandHigh" name)
    (return-from find-included-binary-file
      (make-pathname :directory '(:relative "Object")
                     :name "StagehandHigh" :type "o")))
  (when (search "StagehandLow" name)
    (return-from find-included-binary-file
      (make-pathname :directory '(:relative "Object")
                     :name "StagehandLow" :type "o")))
  (when (eql 0 (search "Art." name))
    (let ((possible-file (make-pathname :directory '(:relative "Source" "Art")
                                        :name (subseq name 4) :type "art")))
      (when (probe-file possible-file)
        (return-from find-included-binary-file
          (make-pathname :directory '(:relative "Object" "Assets")
                         :name name :type "o")))))
  (when (eql 0 (search "Tileset." name))
    (let ((possible-file (make-pathname
                          :directory '(:relative "Source" "Maps" "Tiles")
                          :name (subseq name 8) :type "tsx")))
      (when (probe-file possible-file)
        (return-from find-included-binary-file
          (make-pathname :directory '(:relative "Object" "Assets")
                         :name name :type "o")))))
  (when (eql 0 (search "Blob." name))
    (let ((possible-file (make-pathname :directory '(:relative "Source" "Blobs")
                                        :name (subseq name 5) :type "xcf")))
      (when (probe-file possible-file)
        (return-from find-included-binary-file
          (make-pathname :directory '(:relative "Source" "Generated" "Assets")
                         :name name :type "s")))))
  (when (eql 0 (search "Song." name))
    (let ((possible-file (make-pathname :directory '(:relative "Source" "Songs")
                                        :name (subseq name 5) :type "mscz")))
      (when (probe-file possible-file)
        (return-from find-included-binary-file
          (make-pathname :directory '(:relative "Object" "Assets")
                         :name name :type "o")))))
  (error "Cannot find a possible source for included binary file ~a.o in bank ~(~2,'0x~)"
         name *bank*))

(defun recursive-read-deps (source-file &key testp)
  (unless (equal (pathname-type source-file) "o")
    (unless (probe-file source-file)
      (if (skyline-tool-writes-p source-file)
          (write-source-file source-file)
          (error "Can't find “~a” and don't know how to make it~2%(~s)"
                 (enough-namestring source-file) source-file)))
    (with-input-from-file (source source-file)
      (let* ((testp (or testp
                        (when (search "Tests" (namestring source-file)) t)))
             (includes (loop for line = (read-line source nil nil)
                             while line
                             for included = (included-file line)
                             for binary = (included-binary-file line)
                             for file = (cond
                                          (included (find-included-file included :testp testp))
                                          (binary (find-included-binary-file binary))
                                          (t nil))
                             when file collect file)))
        (remove-duplicates
         (flatten (append (list source-file) includes
                          (mapcar (lambda (file) (recursive-read-deps file :testp testp))
                                  includes)))
         :test #'equal)))))

(defun extract-palette (palette-file)
  (let* ((base-name (subseq (pathname-name palette-file)
                            0
                            (- (length (pathname-name palette-file)) 7)))
         (tsx-file (make-pathname :name (format nil "~aTiles" base-name)
                                  :type "tsx"
                                  :directory (list :relative "Source" "Maps" "Tiles"))))
    (extract-tileset-palette tsx-file palette-file)))

(defun compile-enemy-stats ()
  (compile-enemies #p"Source/Tables/EnemyStats.ods"
                   #p"Source/Generated/EnemyStatsTable.s"))

(define-constant +skyline-writes-files+
    (list "ActorPrototypes" 'write-actor-prototypes
          "AnimationTable" 'compile-animation-sequences
          "AssetIDs" 'write-asset-ids
          "EnemyStatsTable" 'compile-enemy-stats
          "EquipmentIndex" 'write-equipment-index
          "ItemDropTable" 'compile-item-drops
          "DocksIndex" 'write-docks-index
          "CharacterIDs" 'write-character-ids
          "ClassConstants" 'make-classes-for-oops
          "ClassSizes" 'make-classes-for-oops
          "ClassMethods" 'make-classes-for-oops
          "ClassInheritance" 'make-classes-for-oops
          "InventoryLabels" 'write-inventory-tables
          "KeyLabels" 'write-keys-tables
          "Orchestration" 'write-orchestration)
  :test 'equalp)

(defun skyline-tool-writes-p (pathname)
  (and (member (pathname-type pathname) '("s" "forth") :test #'string=)
       (member "Generated" (pathname-directory pathname) :test #'string=)
       (or (when-let (found (member (pathname-name pathname) +skyline-writes-files+
                                    :test #'string=))
                     (second found))
           (when (search "Palette" (pathname-name pathname))
             (lambda () (extract-palette pathname))))))

(defun write-source-file (pathname)
  (when-let (f (skyline-tool-writes-p pathname))
    (format *trace-output* "~&~a: File not created yet, creating now to facilitate writing generated Makefile"
            (enough-namestring pathname))
    (finish-output *trace-output*)
    (funcall f)
    (unless (probe-file pathname)
      (error "Failed to create ~s, tried to write a source file and don't see it now." pathname))
    (format *trace-output* "~&~a has been created now, proceeding with Makefile generation…"
            (enough-namestring pathname))))

(defun recursive-directory (wild-pathname)
  (remove-if
   #'null
   (flatten
    (concatenate
     'list
     (directory wild-pathname)
     (loop for subdir
             in (directory
                 (make-pathname :name :wild
                                :type nil
                                :directory (pathname-directory
                                            wild-pathname)))
           when (cl-fad:directory-pathname-p subdir)
             collect (recursive-directory
                      (make-pathname :name :wild
                                     :type (pathname-type
                                            wild-pathname)
                                     :directory
                                     (pathname-directory subdir))))))))

(defun all-bare-assets ()
  (let ((source-prefix-length
          (length (pathname-directory (merge-pathnames #p"Source/")))))
    (loop for (dir . type) in '(("Maps" . "tmx") ("Songs" . "mscz")
                                ("Scripts" . "fountain") ("Blobs" . "xcf"))
          append
          (mapcar
           (lambda (pathname)
             (subseq
              (enough-namestring
               (make-pathname :directory
                              (append (list :relative "Source")
                                      (subseq (pathname-directory
                                               (merge-pathnames pathname))
                                              source-prefix-length))
                              :name (pathname-name pathname)
                              :version nil
                              :type nil))
              (length "Source/")))
           (recursive-directory
            (make-pathname :directory (list :relative "Source" dir)
                           :name :wild
                           :type type))))))

(defun asset->object-name (asset-indicator &key (video *region*))
  (let ((machine-dir (machine-directory-name)))
    (if *machine*
        (ecase *machine*
          (7800 (destructuring-bind (kind name) (asset-kind/name asset-indicator)
                  (cond ((equal kind "Songs")
                         (assert (not (null video)))
                         (format nil "Object/~a/Assets/Song.~a.~a.o" machine-dir name video))
                        ((equal kind "Maps")
                         (assert (not (null video)))
                         (format nil "Object/~a/Assets/Map.~a.~a.o" machine-dir (substitute #\. #\/ name) video))
                        ((equal kind "Scripts")
                         (format nil "Source/Generated/~a/Assets/Script.~a.s" machine-dir (substitute #\. #\/ name)))
                        ((equal kind "Blobs")
                         (format nil "Source/Generated/~a/Assets/Blob.~a.s" machine-dir name))
                        (t
                         (format nil "Object/~a/Assets/~a.~a.o" machine-dir kind name)))))
          (64 (destructuring-bind (kind name) (asset-kind/name asset-indicator)
                (cond ((equal kind "Songs")
                       (format nil "Object/~a/Assets/Song.~a.~a.CBM.o" machine-dir name video))
                      ((equal kind "Maps")
                       (format nil "Object/~a/Assets/Map.~a.~a.CBM.o" machine-dir (substitute #\. #\/ name) video))
                      ((equal kind "Scripts")
                       (format nil "Source/Generated/~a/Assets/Script.~a.CBM.s" machine-dir (substitute #\. #\/ name)))
                      ((equal kind "Blobs")
                       (format nil "Source/Generated/~a/Assets/Blob.~a.CBM.s" machine-dir name))
                      (t
                       (format nil "Object/~a/Assets/~a.~a.o" machine-dir kind name)))))
          (128 (destructuring-bind (kind name) (asset-kind/name asset-indicator)
                 (cond ((equal kind "Songs")
                        (format nil "Object/~a/Assets/Song.~a.~a.CBM.o" machine-dir name video))
                       ((equal kind "Maps")
                        (format nil "Object/~a/Assets/Map.~a.~a.CBM.o" machine-dir (substitute #\. #\/ name) video))
                       ((equal kind "Scripts")
                        (format nil "Source/Generated/~a/Assets/Script.~a.CBM.s" machine-dir (substitute #\. #\/ name)))
                       ((equal kind "Blobs")
                        (format nil "Source/Generated/~a/Assets/Blob.~a.CBM.s" machine-dir name))
                       (t
                        (format nil "Object/~a/Assets/~a.~a.o" machine-dir kind name))))))
        ;; Fallback when *machine* is NIL
        (destructuring-bind (kind name) (asset-kind/name asset-indicator)
          (format nil "Object/Unknown/Assets/~a.~a.o" kind name)))))

(defun asset->deps-list (asset-indicator build)
  (declare (ignore build))
  (let ((machine-dir (machine-directory-name)))
    (destructuring-bind (kind name) (asset-kind/name asset-indicator)
      (cond ((equal kind "Songs")
             (format nil "~20tSource/Generated/~a/Orchestration.s\\
~{~20tObject/~a/Assets/Song.~{~a.~a~}.o~^ \\~%~}"
                     machine-dir machine-dir
                     (loop for video in (supported-video-types *machine*)
                           collecting (list name video))))
          ((equal kind "Maps")
           (format nil "~{~25t~a~^ \\~%~}"
                   (loop for video in (supported-video-types *machine*)
                         collect (asset->object-name asset-indicator :video video))))
          ((equal kind "Blob")
           (format nil "Source/Generated/Assets/Blob.~a.s" name))
          ((equal kind "Art")
           (format nil "Source/Generated/Assets/Art.~a.s" name))
          (t (asset->object-name asset-indicator)))))

(defun asset->symbol-name (asset-indicator)
  (destructuring-bind (kind &rest name) (split-sequence #\/ asset-indicator)
    (format nil "~a_~{~a~^_~}"
            (subseq kind 0 (1- (length kind)))
            name)))

(defun asset->source-name (asset-indicator)
  (destructuring-bind (kind &rest name) (split-sequence #\/ asset-indicator)
    (format nil "Source/~a~{/~a~}.~a" kind name
            (cond
              ((equal kind "Maps") "tmx")
              ((equal kind "Songs") "midi")
              ((equal kind "Scripts") "fountain")
              ((equal kind "Blobs") "png")
              (t (error "Asset kind ~a not known" kind))))))

(defun asset-compilation-line (asset-indicator &key video)
  (let ((machine-dir (machine-directory-name)))
    (destructuring-bind (kind &rest name) (split-sequence #\/ asset-indicator)
      (cond
        ((equal kind "Maps")
         (format nil "bin/skyline-tool compile-map $<"))
        ((equal kind "Songs")
         (format nil "bin/skyline-tool compile-midi $< HOKEY ~a $@" video))
        ((equal kind "Scripts")
         (format nil "bin/skyline-tool compile-script $< Source/Generated/~a/Assets/Script.~{~a~^.~}.forth
	bin/skyline-tool compile-forth ~:*Source/Generated/~a/Assets/Script.~{~a~^.~}.forth $@"
                 machine-dir name machine-dir name))
        ((equal kind "Blobs")
         (if (eql *machine* 200)
             (format nil "bin/skyline-tool --port Lynx dispatch-png $< Object/~a/Assets" machine-dir)
             (format nil "bin/skyline-tool blob-rip-7800 $<")))
        (t (error "Asset kind ~a not known" kind))))))

(defun write-asset-compilation/music (asset-indicator)
  (let* ((machine-dir (machine-directory-name))
         (basename (last-segment asset-indicator #\/))
         (source-pathname (make-pathname :directory (list :relative "Source" "Generated" machine-dir "Assets")
                                         :name (format nil "Song.~a" basename)
                                         :type "s")))
    (ensure-directories-exist source-pathname)
    (with-output-to-file (source source-pathname :if-exists :supersede)
      (format source ";; This is a generated file~2%")
      (dolist (video (supported-video-types *machine*))
        (format source "~%~10t.if TV == ~a
~10t  .binary \"Song.~a.~a.o\"
~10t.fi~%"
                video basename video)))
    (dolist (video (supported-video-types *machine*))
      (format t "~%
~a: ~a \\
~10tSource/Assets.index bin/skyline-tool Source/Generated/~a/Orchestration.s Source/Tables/Orchestration.ods
	mkdir -p Object/~a/Assets
	~a"
              (asset->object-name asset-indicator :video video)
              (asset->source-name asset-indicator)
              machine-dir machine-dir
              (asset-compilation-line asset-indicator :video video)))))

(defun write-asset-compilation/map (asset-indicator)
  (let ((machine-dir (machine-directory-name)))
    (dolist (video (supported-video-types *machine*))
      (format t "~%
~a: ~a \\
~10tSource/Assets.index bin/skyline-tool
	mkdir -p Object/~a/Assets
	~a"
              (asset->object-name asset-indicator :video video)
              (asset->source-name asset-indicator)
              machine-dir
              (asset-compilation-line asset-indicator :video video)))))

(defun write-asset-compilation/blob (asset-indicator)
  (let ((machine-dir (machine-directory-name)))
    (dolist (video (supported-video-types *machine*))
      (format t "~%
~a: ~a \\
~10tSource/Assets.index bin/skyline-tool
	mkdir -p Object/~a/Assets
	~a"
              (asset->object-name asset-indicator :video video)
              (asset->source-name asset-indicator)
              machine-dir
              (asset-compilation-line asset-indicator :video video)))))

(defun write-asset-compilation (asset-indicator)
  (let ((machine-dir (machine-directory-name)))
    (cond ((song-asset-p asset-indicator)
           (write-asset-compilation/music asset-indicator))
          ((map-asset-p asset-indicator)
           (write-asset-compilation/map asset-indicator))
          ((blob-asset-p asset-indicator)
           (ecase *machine*
             (200 ; Lynx platform
              (format *trace-output* "~&(Write-Asset-Compilation processing LYNX BLOB ~a)" asset-indicator)
              (write-asset-compilation/blob-lynx asset-indicator))
             ((1 2 8 16 20 64 88 128 222 223 264 2609 1601 2600 3010 5200 7800) ; Other machines - ignore blobs for now
              (format *trace-output* "~&(Write-Asset-Compilation is ignoring BLOB ~a for machine ~A)" asset-indicator *machine*))))
          ((script-asset-p asset-indicator)
           (format t "~%
~a: ~a~@[ \\~%~10t~a~] \\
~10tSource/Generated/~a/Labels.Public.NTSC.forth Source/Generated/~a/Classes.forth \\
~10tSource/Assets.index bin/skyline-tool
	# FIXME: #1237 NTSC is not actually right for everyone
	mkdir -p Object/~a/Assets
	~a"
                   (asset->object-name asset-indicator)
                   (asset->source-name asset-indicator)
                   (when (speech-supported-p) "Source/Tables/SpeakJet.dic")
                   machine-dir machine-dir machine-dir
                   (asset-compilation-line asset-indicator)))
          (t
           (cerror "Continue with generic code" "Unexpected asset kind in indicator: ~a" asset-indicator)
           (format t "~%
~a: ~a~@[\\~%	~a~]\\
~10tSource/Assets.index bin/skyline-tool
	mkdir -p Object/~a/Assets
	~a"
                   (asset->object-name asset-indicator)
                   (asset->source-name asset-indicator)
                   (when (and (script-asset-p asset-indicator) (speech-supported-p))
                     "Source/Tables/SpeakJet.dic")
                   machine-dir
                   (asset-compilation-line asset-indicator))))))

(defun speech-supported-p ()
  "Return true if the current platform supports speech synthesis."
  (ecase *machine*
    ((2600 7800 2609) t) ; VCS (AtariVox), 7800 (AtariVox), Intellivision (IntelliVoice)
    (t nil))) ; All others: false

(defun asset-loaders (asset-objects)
  "Enumerates the asset loaders that might be needed for the ASSET-OBJECTS given.

Currently just enumerates all four asset loaders."
  (declare (ignore asset-objects))
  (list "Source/Routines/LoadMap.s"
        "Source/Routines/LoadBlob.s"
        "Source/Routines/LoadSong.s"
        "Source/Routines/LoadScript.s"))

(defun write-asset-ids (&optional (outfile-pathname #p"Source/Generated/AssetIDs.s")
                                  (infile-pathname #p"Source/Assets.index"))
  "Computes the hashes of assets from INFILE-PATHNAME and writes OUTFILE-PATHNAME.

Defaults are Source/Assets.index → Source/Generated/AssetIDs.s and .forth"
  (ensure-directories-exist outfile-pathname)
  (with-output-to-file (outfile outfile-pathname :if-exists :supersede)
    (format outfile ";;; Asset IDs are auto-generated")
    (multiple-value-bind (asset-builds asset-ids) (read-assets-list infile-pathname)
      (declare (ignore asset-builds))
      (format *trace-output* "~&Writing AssetIDs.s for ~:d asset~:p" (hash-table-count asset-ids))
      (loop for kind being the hash-keys in asset-ids using (hash-value ids-by-kind)
            do (terpri outfile)
            do (loop for asset-hash being the hash-keys in ids-by-kind using (hash-value asset-name)
                     do (format outfile "~%~10t~:(~a~)_~{~a~^_~}_ID = $~2,'0x"
                                kind (split-sequence #\/ asset-name) asset-hash)))))

  (with-output-to-file (outfile (merge-pathnames (make-pathname :type "forth")
                                                 outfile-pathname)
                                :if-exists :supersede)
    (format outfile " ( -*- forth -*- Asset IDs are auto-generated )")
    (multiple-value-bind (asset-builds asset-ids) (read-assets-list infile-pathname)
      (declare (ignore asset-builds))
      (format *trace-output* "~&Writing AssetIDs.forth for ~:d asset~:p" (hash-table-count asset-ids))
      (loop for kind being the hash-keys in asset-ids using (hash-value ids-by-kind)
            do (terpri outfile)
            do (loop for asset-hash being the hash-keys in ids-by-kind using (hash-value asset-name)
                     do (format outfile "~%: ~:(~a~)_~{~a~^_~}_ID  ~d ( ~:*$~2,'0x ) ;"
                                kind (split-sequence #\/ asset-name) asset-hash))))))

(defun write-asset-bank-makefile (bank &key build video)
  "Writes the Makefile for an asset ROM bank"
  (let* ((all-assets (all-assets-for-build build))
         (asset-objects (mapcar (rcurry #'asset->deps-list build) all-assets)))
    (format t "~%
Source/Generated/Bank~(~2,'0x~).~a.~a.list: Source/Assets.index \\
~10tbin/skyline-tool \\~{~%~10t~a~^ \\~}
	bin/skyline-tool allocate-assets ~a

Source/Generated/Bank~(~2,'0x~).~a.~a.s: Source/Assets.index Source/Generated/Bank~(~2,'0x~).~a.~a.list \\
~10tbin/skyline-tool \\~{~%~10t~a~^ \\~}
	bin/skyline-tool write-asset-bank ~x ~a ~a

Object/Bank~(~2,'0x~).~a.~a.o \\
  ~3:*Object/Bank~(~2,'0x~).~a.~a.o.list.txt \\
  ~3:*Object/Bank~(~2,'0x~).~a.~a.o.LABELS.txt: \\
		Source/Generated/Bank~(~2,'0x~).~a.~a.s \\
~10tSource/Assets.index bin/skyline-tool \\~{~%~10t~a~^ \\~}
	mkdir -p Object
	${AS7800} -DTV=~a ~a \\~{~%		-I ~a \\~}
		~0@*-l Object/Bank~(~2,'0x~).~a.~a.o.LABELS.txt \\
                    ~0@*-L Object/Bank~(~2,'0x~).~a.~a.o.list.txt \\
		~0@*$< -o Object/Bank~(~2,'0x~).~a.~a.o
	bin/skyline-tool prepend-fundamental-mode ~0@*Object/Bank~(~2,'0x~).~a.~a.o.list.txt"
            bank build video
            asset-objects
            build
            bank build video
            bank build video
            asset-objects
            bank build video
            bank build video
            bank build video
            (append asset-objects (asset-loaders asset-objects))
            video (cond ((equal build "AA") "-DATARIAGE=true -DPUBLISHER=true")
                        ((equal build "Demo") "-DDEMO=true")
                        (t ""))
            (mapcar (lambda (path) (format nil "~{~a~^/~}" (rest path)))
                    (include-paths-for-current-bank)))))

(defun write-bank-makefile (bank-source &key build video)
  "Writes the Makefile entry for a ROM bank"
  (when (= *bank* *last-bank*)
    (format t "~%
~*Source/Generated/LastBankDefs.~a.~a.s: ~0@*Object/Bank~(~2,'0x~).~a.~a.o \\
~10t~0@*Object/Bank~(~2,'0x~).~a.~a.o.LABELS.txt
	bin/skyline-tool labels-to-include ~0@*Object/Bank~(~2,'0x~).~a.~a.o.LABELS.txt \\
		c000 ffff ~1@*LastBankDefs.~a.~a"
            *bank* build video))
  (format t "~%
Object/Bank~(~2,'0x~).~a.~a.o ~
~3:*Object/Bank~(~2,'0x~).~a.~a.o.list.txt ~
~3:*Object/Bank~(~2,'0x~).~a.~a.o.LABELS.txt: ~
~{ \\~%~20t~a~}~@[ \\~%~20t~a~]
	mkdir -p Object
	-rm -f $@
	${AS7800} -DTV=~a \\
		~@[-DLASTBANK=true -DBANK=~d ~] -DFIRSTASSETSBANK=~d \\
		~a \\~{~%		-I ~a \\~}
		~0@*-l Object/Bank~(~2,'0x~).~a.~a.o.LABELS.txt \\
		~0@*-L Object/Bank~(~2,'0x~).~a.~a.o.list.txt $< \\
		~0@*-o Object/Bank~(~2,'0x~).~a.~a.o 2>&1 | \\
		tee ~0@*Object/Bank~(~2,'0x~).~a.~a.out
	echo \"@	$$(grep 'warning: Bank .~0@*~(~2,'0x~) ends at ' ~
 ~0@*Object/Bank~(~2,'0x~).~a.~a.out | ~
 cut -d',' -f2)\" > ~
 ~0@*Source/Generated/Bank~(~2,'0x~).~a.~a.size
	bin/skyline-tool prepend-fundamental-mode \\
                      ~0@* Object/Bank~(~2,'0x~).~a.~a.o.list.txt
	[ -f $@ ]"
          *bank* build video (recursive-read-deps bank-source)
          (if (= *bank* *last-bank*)
              "Source/Generated/Orchestration.s"
              (format nil "Source/Generated/LastBankDefs.~a.~a.s" build video))
          video
          (when (= *bank* *last-bank*) *bank*)
          (first-assets-bank build)
          (cond ((equal build "AA") "-DATARIAGE=true -DPUBLISHER=true")
                ((equal build "Demo") "-DDEMO=true")
                (t ""))
          (mapcar (lambda (path) (format nil "~{~a~^/~}" (rest path)))
                  (include-paths-for-current-bank))))

(defun write-ram-bank-makefile (&key build video)
  "Writes the Makefile entry for the RAM bank used by 7800GD"
  (format t "~%
Object/Bank3e.~a.~a.o.LABELS.txt:~0@*
	mkdir -p Object/
	echo \";;; nop\" > $@

Object/Bank3e.~a.~a.o:
	mkdir -p Object
	dd if=/dev/zero bs=1024 count=16 of=$@
"
          build video))

(defun write-makefile-test-target ()
  "Writes the test ROM target for the Makefile"
  (format t "~%
Dist/~a.Test.a78: Dist/~:*~a.Test.bin
	cp $^ $@
	bin/7800header -f Source/Generated/header.Test.script $@

Dist/~:*~a.Test.bin: \\~
~{~%~10tObject/Bank~(~2,'0x~).Test.o~^ \\~}
	mkdir -p Dist
	cat $^ > $@
	bin/7800sign -w $@

~0@*Dist/~a.Test.a78: .EXTRA_PREREQS = bin/7800header

~0@*Dist/~a.Test.bin: .EXTRA_PREREQS = bin/7800sign
"
          *game-title*
          (loop for bank below (number-of-banks :public :ntsc)
                collect bank)
          *game-title*))

(defun write-makefile-top-line (&key video build)
  "Writes the top lines for the Makefile"
  (ecase *machine*
    (7800 (format t "~%
Dist/~a.~a.~a.a78: ~0@* Dist/~a.~a.~a.bin
	cp $^ $@
	bin/7800header -f Source/Generated/header.~1@*~a.~a.script $@

~0@*
Dist/~a.~a.~a.bin: \\~
~{~%~10tObject/Bank~(~2,'0x~).~a.~a.o~^ \\~}
	mkdir -p Dist
	cat $^ > $@
	bin/7800sign -w $@

~0@*Dist/~a.~a.~a.a78: .EXTRA_PREREQS = bin/7800header

~0@*Dist/~a.~a.~a.bin: .EXTRA_PREREQS = bin/7800sign
"
                  *game-title*
                  build video
                  (loop for bank below (number-of-banks build video)
                        appending (list bank build video))
                  build video
                  *game-title*))
    (64 (format t "~%
Dist/Phantasia.CBM.zip: ~0@* Object/Phantasia.CBM.zip
	cp $^ $@

Object/Phantasia.CBM.zip: \\~
~{~%~10tObject/Phantasia.CBM/~a ~^ \\~}
	mkdir -p Dist
	zip $@ $^

"
                (all-encoded-asset-names)
                *game-title*))
    (128 (format t "~%
Dist/Phantasia.CBM.zip: ~0@* Object/Phantasia.CBM.zip
	cp $^ $@

Object/Phantasia.CBM.zip: \\~
~{~%~10tObject/Phantasia.CBM/~a ~^ \\~}
	mkdir -p Dist
	zip $@ $^

"
                 (all-encoded-asset-names)
                 *game-title*))))


(defvar *assets-for-builds* (make-hash-table :test 'equalp)
  "A cache of assets and in which builds they are used.")

(defun all-assets-for-build (build)
  "Collect all assets for the build BUILD from Source/Assets.index

Uses *ASSETS-FOR-BUILDS* as a cache"
  (or (gethash build *assets-for-builds*)
      (format *trace-output* "~&Assets for build ~s: " build)
      (let ((assets (filter-assets-for-build (read-assets-list #p"Source/Assets.index")
                                             build)))
        (format *trace-output* " …~:d asset~:p selected" (length assets))
        (setf (gethash build *assets-for-builds*) assets)
        assets)))

(defun write-assets-makefile (&key build video)
  "Write the makefile for assets for BUILD and VIDEO"
  (assert build) (assert video)
  (format t "
Source/Generated/Bank~(~2,'0x~).~a.~a.s: \\~{~%~10t~a~^ \\~}
	bin/skyline-tool allocate-assets ~a"
          *bank*
          build video
          (all-assets-for-build build)
          build))

(defun current-julian-date ()
  ;; Julian day of year
  (1+ (- (local-time:modified-julian-date (local-time:now))
         (local-time:modified-julian-date (local-time:adjust-timestamp (local-time:now)
                                            (set :month 1) (set :day-of-month 1))))))

(defun current-year ()
  (nth-value 5 (decode-universal-time (get-universal-time))))

(defun write-header-script (&key build video)
  "Write the header script for the given BUILD, VIDEO ROM"
  (let ((script-pathname (make-pathname :directory '(:relative "Source" "Generated")
                                        :name (format nil "header.~a.~a"
                                                      build video)
                                        :type "script")))
    (ensure-directories-exist script-pathname)
    (with-output-to-file (script script-pathname
                                 :if-exists :supersede)
      (format script "name ~15a ~6a/~1a~1a~2d.~3,'0d
set tv~(~a~)
set supergame
set ram@4000
set snes1
unset 7800joy2
set savekey
set hsc
set composite
set pokey@450
save
exit
"
              *game-title*
              *part-number*
              (string (char build 0)) (string (char video 0))
              (mod (current-year) 100) (current-julian-date)
              video))))

(defun write-test-header-script ()
  "Write the header file for the test ROM"
  (let ((script-pathname (make-pathname :directory '(:relative "Source" "Generated")
                                        :name "header.Test"
                                        :type "script")))
    (ensure-directories-exist script-pathname)
    (with-output-to-file (script script-pathname :if-exists :supersede)
      (format script "name ~16a ~4a ~2d.~3,'0d
set tvntsc
set supergame
set ram@4000
set snes1
unset 7800joy2
set savekey
set composite
set pokey@450
save
exit
"
              *game-title*
              "Test"
              (current-year) (current-julian-date)))))

(defun write-makefile-test-banks ()
  "Write the sources for test memory banks"
  (let ((*last-bank* (1- (number-of-banks :public :ntsc))))
    (dotimes (*bank* (1+ *last-bank*))
      (let* ((bank (if (= *bank* *last-bank*)
                       "LastBank"
                       (format nil "Bank~(~2,'0x~)" *bank*)))
             (bank-source (make-pathname
                           :directory (list :relative "Source" "Banks" bank)
                           :name bank
                           :type "s")))
        (when (= *bank* *last-bank*)
          (format t "~%
Object/Bank~(~2,'0x~).Test.o.LABELS.txt: Object/Bank~(~:*~2,'0x~).Test.o
	$(MAKE) -f Source/Generated/Makefile $<

Source/Generated/LastBankDefs.Test.NTSC.s: Object/Bank~(~2,'0x~).Test.o Object/Bank~(~:*~2,'0x~).Test.o.LABELS.txt
	bin/skyline-tool labels-to-include Object/Bank~(~:*~2,'0x~).Test.o.LABELS.txt \\
		c000 ffff LastBankDefs.Test.NTSC"
                  *bank* *last-bank*))
        (if (and (= #x3f *last-bank*)
                 (= #x3e *bank*))
            (format t "~%
Object/Bank~(~2,'0x~).Test.o.LABELS.txt:~:*
	mkdir -p Object/
	echo \";;; nop\" > $@

Object/Bank~(~2,'0x~).Test.o:
	mkdir -p Object/
	dd if=/dev/zero bs=1024 count=16 of=$@
"
                    *bank*)
            (format t "~%
Object/Bank~(~2,'0x~).Test.o:~{ \\~%~20t~a~}~@[~* \\~%~20tSource/Generated/LastBankDefs.Test.NTSC.s~]
	mkdir -p Object
	${AS7800} ~@[~a~] -DTV=NTSC -DUNITTEST=true \\
	-DFIRSTASSETSBANK=~d ~{ \\~%		-I ~a ~} \\
		-l $@.LABELS.txt -L $@.list.txt $< -o $@
	bin/skyline-tool prepend-fundamental-mode $@.list.txt
"
                    *bank*
                    (if (probe-file bank-source)
                        (recursive-read-deps bank-source)
                        (list (make-pathname :directory (list :relative "Source" "Generated")
                                             :name (format nil "Bank~(~2,'0x~).Public.NTSC" *bank*)
                                             :type "s")))
                    (< *bank* *last-bank*)
                    (when (= *bank* *last-bank*)
                      (format nil "-DBANK=~d -DLASTBANK=true" *bank*))
                    (first-assets-bank "Test")
                    (mapcar (lambda (path) (format nil "~{~a~^/~}" (rest path)))
                            (include-paths-for-current-bank))))))))

(defun write-makefile-for-blobs ()
  (dolist (blob (remove-duplicates
                 (recursive-directory (make-pathname :directory (list :relative "Source" "Blobs")
                                                     :name :wild
                                                     :type "xcf"))))
    (when (or (not (eql *machine* 200)) ; Not Lynx, process all
              (search "Lynx/" (namestring blob))) ; Lynx, only process Lynx/ blobs
      (write-blob-generation blob))))

(defun write-makefile-for-art ()
  (dolist (art (recursive-directory (make-pathname :directory (list :relative "Source" "Art")
                                                    :name :wild
                                                    :type "art")))
    (write-art-generation art)))

(defun write-makefile-for-tilesets ()
  (dolist (tileset (recursive-directory (make-pathname :directory (list :relative "Source" "Maps")
                                                       :name :wild
                                                       :type "tsx")))
    (write-tsx-generation tileset)))

(defun write-makefile-for-bare-assets ()
  (dolist (asset (all-bare-assets))
    (write-asset-compilation asset)))

(defun write-makefile-header ()
  (format t "# Makefile (generated)~%# -*- makefile -*-~%"))

(defun bank-source-pathname ()
  (make-pathname
   :directory (list :relative "Source" "Banks"
                    (format nil "Bank~(~2,'0x~)" *bank*))
   :name (format nil "Bank~(~2,'0x~)" *bank*)
   :type "s"))

(defun last-bank-source-pathname ()
  (make-pathname
   :directory (list :relative "Source" "Banks" "LastBank")
   :name "LastBank" :type "s"))

(defun write-master-makefile-for-machine (machine)
  "Write machine-specific makefile content for MACHINE"
  (cond
    ((member machine '(7800 200 5200))
     ;; Machines that support multiple builds and videos
     (dolist (build +all-builds+)
       (dolist (video (supported-video-types machine))
         (let ((*last-bank* (1- (number-of-banks build video))))
           (write-makefile-top-line :build build :video video)
           (write-header-script :build build :video video)
           (dotimes (*bank* (1+ *last-bank*))
             (let ((bank-source (bank-source-pathname)))
               (cond
                 ((= *bank* *last-bank*)
                  (write-bank-makefile (last-bank-source-pathname)
                                       :build build :video video))
                 ((and (= *last-bank* #x3f) (= *bank* #x3e))
                  (write-ram-bank-makefile :build build :video video))
                 ((probe-file bank-source)
                  (write-bank-makefile bank-source
                                       :build build :video video))
                 (t (write-asset-bank-makefile *bank*
                                               :build build :video video))))))))))
    (t
     ;; Simple machines that just need makefile top line
     (write-makefile-top-line))))
