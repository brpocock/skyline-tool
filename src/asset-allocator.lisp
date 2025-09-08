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
                   (setf *maps-ids* nil)
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
      (7800 12)))
  (:method ((kind (eql :song)) record-count)
    (ecase *machine*
      (7800 256)) ; FIXME #124
    )
  (:method ((kind (eql :script)) record-count)
    (ecase *machine*
      (7800 (+ 128 (* (1+ record-count) 4)))))
  (:method ((kind (eql :blob)) record-count)
    (ecase *machine*
      (7800 (+ 284 1 (* record-count 3)))))
  (:method ((kind (eql :map)) record-count)
    (ecase *machine*
      (7800 (+
             #|LoadMap|# 1024 #| approx XXX |#
             #| end of table |# 1
             #|per record|# (* record-count 3))))))

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
    (dolist (video +all-video+)
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

(defun include-paths-for-current-bank (&key cwd testp)
  (let* ((bank (if (= *bank* *last-bank*)
                   "LastBank"
                   (format nil "Bank~(~2,'0x~)" *bank*)))
         (includes (list (list :relative "Source")
                         (list :relative "Source" "Common")
                         (list :relative "Source" "Routines")
                         (list :relative "Source" "Classes")
                         (list :relative "Source" "Stagehand")
                         (list :relative "Object")
                         (list :relative "Object" "Assets")
                         (list :relative "Source" "Generated")
                         (list :relative "Source" "Generated" "Assets"))))
    (when cwd (appendf includes (list (pathname-directory cwd))))
    (when testp (appendf includes (list (list :relative "Source" "Tests"))))
    (when (probe-file (make-pathname :directory (list :relative "Source" "Banks" bank)
                                     :name bank :type "s"))
      (appendf includes (list (list :relative "Source" "Banks" bank))))
    includes))

(defun generated-path (path)
  (cond
    ((equalp path '(:relative "Source" "Common"))
     (list :relative "Source" "Generated" "Common"))
    ((equalp (subseq path 0 3) '(:relative "Source" "Banks"))
     (append (list :relative "Source" "Generated") (subseq path 3)))
    (t (error "Don't know how to find a generated path from ~a" path))))

(defun write-blob-generation (pathname)
  (format t "~%
Source/Generated/Assets/Blob.~a.s: Source/Blobs/~a.png\\~%~10tbin/skyline-tool
	mkdir -p Source/Generated/Assets
	bin/skyline-tool blob-rip-7800 $<"
          (pathname-name pathname)
          (pathname-name pathname)))

(defun write-art-generation (pathname)
  (format t "~%
Object/Assets/Art.~a.o: Source/Art/~a.art \\~{~%~10t~a \\~}~%~10tbin/skyline-tool
	mkdir -p Object/Assets
	bin/skyline-tool compile-art-7800 $@ $<"
          (pathname-name pathname)
          (pathname-name pathname)
          (mapcar (compose #'enough-namestring #'second)
                  (read-7800-art-index pathname))))

(defun write-tsx-generation (pathname)
  (if (and (search "Decals" (pathname-name pathname))
           (not (search "Decals2" (pathname-name pathname))))
      (format t "~%
Object/Assets/Tileset.~a.o: Source/Maps/Tiles/~:*~a.tsx Source/Maps/Tiles/CommonDecals.tsx \\
~10tSource/Maps/Tiles/~:*~a.png Source/Maps/Tiles/CommonDecals.png \\
~10tbin/skyline-tool
	mkdir -p Object/Assets
	bin/skyline-tool compile-tileset $< Source/Maps/Tiles/CommonDecals.tsx"
              (pathname-name pathname))
      (format t "~%
Object/Assets/Tileset.~a.o: Source/Maps/Tiles/~:*~a.tsx \\
~10tSource/Maps/Tiles/~:*~a.png \\
~10tbin/skyline-tool
	mkdir -p Object/Assets
	bin/skyline-tool compile-tileset $<"
              (pathname-name pathname))))

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

(defun asset->object-name (asset-indicator &key video)
  (destructuring-bind (kind name) (asset-kind/name asset-indicator)
    (cond ((equal kind "Songs")
           (assert (not (null video)))
           (format nil "Object/Assets/Song.~a.~a.o" name video))
          ((equal kind "Maps")
           (assert (not (null video)))
           (format nil "Object/Assets/Map.~a.~a.o" (substitute #\. #\/ name) video))
          ((equal kind "Scripts")
           (format nil "Source/Generated/Assets/Script.~a.s" (substitute #\. #\/ name)))
          ((equal kind "Blobs")
           (format nil "Source/Generated/Assets/Blob.~a.s" name))
          (t
           (format nil "Object/Assets/~a.~a.o" kind name)))))

(defun asset->deps-list (asset-indicator build)
  (declare (ignore build))
  (destructuring-bind (kind name) (asset-kind/name asset-indicator)
    (cond ((equal kind "Songs")
           (format nil "~20tSource/Generated/Orchestration.s\\
~{~20tObject/Assets/Song.~{~a.~a~}.o~^ \\~%~}"
                   (loop for video in +all-video+
                         collecting (list name video))))
          ((equal kind "Maps")
           (format nil "~{~25t~a~^ \\~%~}"
                   (loop for video in +all-video+
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
  (destructuring-bind (kind &rest name) (split-sequence #\/ asset-indicator)
    (cond
      ((equal kind "Maps")
       (format nil "bin/skyline-tool compile-map $<"))
      ((equal kind "Songs")
       (format nil "bin/skyline-tool compile-midi $< HOKEY ~a $@" video))
      ((equal kind "Scripts")
       (format nil "bin/skyline-tool compile-script $< Source/Generated/Assets/Script.~{~a~^.~}.forth
	bin/skyline-tool compile-forth ~:*Source/Generated/Assets/Script.~{~a~^.~}.forth $@"
               name))
      ((equal kind "Blobs")
       (format nil "bin/skyline-tool blob-rip-7800 $<"))
      (t (error "Asset kind ~a not known" kind)))))

(defun write-asset-compilation/music (asset-indicator)
  (let* ((basename (last-segment asset-indicator #\/))
         (source-pathname (make-pathname :directory '(:relative "Source" "Generated" "Assets")
                                         :name (format nil "Song.~a" basename)
                                         :type "s")))
    (ensure-directories-exist source-pathname)
    (with-output-to-file (source source-pathname :if-exists :supersede)
      (format source ";; This is a generated file~2%")
      (dolist (video +all-video+)
        (format source "~%~10t.if TV == ~a
~10t  .binary \"Song.~a.~a.o\"
~10t.fi~%"
                video basename video)))
    (dolist (video +all-video+)
      (format t "~%
~a: ~a \\
~10tSource/Assets.index bin/skyline-tool Source/Generated/Orchestration.s Source/Tables/Orchestration.ods
	mkdir -p Object/Assets
	~a"
              (asset->object-name asset-indicator :video video)
              (asset->source-name asset-indicator)
              (asset-compilation-line asset-indicator :video video)))))

(defun write-asset-compilation/map (asset-indicator)
  (dolist (video +all-video+)
    (format t "~%
~a: ~a \\
~10tSource/Assets.index bin/skyline-tool
	mkdir -p Object/Assets
	~a"
            (asset->object-name asset-indicator :video video)
            (asset->source-name asset-indicator)
            (asset-compilation-line asset-indicator :video video))))

(defun write-asset-compilation/blob (asset-indicator)
  (dolist (video +all-video+)
    (format t "~%
~a: ~a \\
~10tSource/Assets.index bin/skyline-tool
	mkdir -p Object/Assets
	~a"
            (asset->object-name asset-indicator :video video)
            (asset->source-name asset-indicator)
            (asset-compilation-line asset-indicator :video video))))

(defun write-asset-compilation (asset-indicator)
  (cond ((song-asset-p asset-indicator)
         (write-asset-compilation/music asset-indicator))
        ((map-asset-p asset-indicator)
         (write-asset-compilation/map asset-indicator))
        ((blob-asset-p asset-indicator)
         (format *trace-output* "~&(Write-Asset-Compilation is ignoring BLOB ~a)" asset-indicator))
        ((script-asset-p asset-indicator)
         (format t "~%
~a: ~a \\
~10tSource/Tables/SpeakJet.dic Source/Generated/Labels.Public.NTSC.forth Source/Generated/Classes.forth \\
~10tSource/Assets.index bin/skyline-tool
	# FIXME: #1237 NTSC is not actually right for everyone
	mkdir -p Object/Assets
	~a"
                 (asset->object-name asset-indicator)
                 (asset->source-name asset-indicator)
                 (asset-compilation-line asset-indicator)))
        (t
         (cerror "Continue with generic code" "Unexpected asset kind in indicator: ~a" asset-indicator)
         (format t "~%
~a: ~a~@[\\~%	~a~]\\
~10tSource/Assets.index bin/skyline-tool
	mkdir -p Object/Assets
	~a"
                 (asset->object-name asset-indicator)
                 (asset->source-name asset-indicator)
                 (when (script-asset-p asset-indicator)
                   "Source/Tables/SpeakJet.dic")
                 (asset-compilation-line asset-indicator)))))

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
  (format t "~%
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
                 (directory (make-pathname :directory (list :relative "Source" "Blobs")
                                           :name :wild
                                           :type "xcf"))))
    (write-blob-generation blob)))

(defun write-makefile-for-art ()
  (dolist (art (directory (make-pathname :directory (list :relative "Source" "Art")
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

(defun write-master-makefile ()
  "Write  out   Source/Generated/Makefile  for  building   everything  not
mentioned in the top-level Makefile."
  (let ((*machine* 7800))
    (ensure-directories-exist #p"Source/Generated/")
    (format *trace-output* "~&Writing master Makefile content …")
    (with-output-to-file (*standard-output* #p"Source/Generated/Makefile" :if-exists :supersede)
      (write-makefile-header)
      (write-makefile-for-bare-assets)
      (write-makefile-for-tilesets)
      (write-makefile-for-art)
      (write-makefile-for-blobs)
      (write-makefile-test-target)
      (write-test-header-script)
      (write-makefile-test-banks)
      (dolist (build +all-builds+)
        (dolist (video +all-video+)
          (let ((*last-bank* (1- (number-of-banks build video))))
            (write-makefile-top-line :build build :video video)
            (write-header-script :build build :video video)
            (dotimes (*bank* (1+ *last-bank*))
              (let ((bank-source (bank-source-pathname)))
                (cond
                  ((= *bank* *last-bank*)
                   (write-bank-makefile (last-bank-source-pathname)
                                        :build build :video video))
                  ((and (= *last-bank* #x3f)
                        (= *bank* #x3e))
                   (write-ram-bank-makefile :build build :video video))
                  ((probe-file bank-source)
                   (write-bank-makefile bank-source
                                        :build build :video video))
                  (t (write-asset-bank-makefile *bank*
                                                :build build :video video))))))))
      (format *trace-output* " … done writing master Makefile.~%"))))

(defmethod get-asset-id ((kind (eql :map)) asset)
  "Find the asset ID for ASSET (a map), ultimately via `FIND-LOCALE-ID-FROM-XML'"
  (let ((*current-scene* asset))
    (let ((id (find-locale-id-from-xml (xmls:parse-to-list
                                        (alexandria:read-file-into-string
                                         (locale-pathname asset))))))
      (assert id (id)
              "Could not find asset ID for map “~a”" asset)
      (format *trace-output* "~&//* Map “~a” has ID $~2,'0x" asset id)
      id)))

(defmethod get-asset-id ((kind (eql :script)) asset)
  "Calls `FIND-SCRIPT-ID' for ASSET"
  (or (find-script-id asset)
      (error "Counld not get asset ID for ~s" asset)))

(defmethod get-asset-id ((kind (eql :art)) asset-name)
  "Find the asset-id of ASSET-NAME from its name"
  (let ((id (logand #xff (sxhash asset-name))))
    (format *trace-output* "~&//* Art “~a” has ID $~2,'0x" asset-name id)
    id))

(defmethod get-asset-id ((kind (eql :blob)) asset-name)
  "Find the asset-id of ASSET-NAME from its name"
  (let ((id (logand #xff (sxhash asset-name))))
    (format *trace-output* "~&//* Blob “~a” has ID $~2,'0x" asset-name id)
    id))

(defmethod get-asset-id ((kind (eql :song)) asset-name)
  "Find the asset ID for a song (based on its workNumber or name ASSET-NAME)"
  (let ((pathname (make-pathname :directory '(:relative "Source" "Songs")
                                 :name asset-name
                                 :type "mscz")))
    (when (probe-file pathname)
      (zip:with-zipfile (zip pathname)
        (if-let (entry (gethash (format nil "~a.mscx" asset-name)
                                (zip:zipfile-entries zip)))
          (when-let (work-number$
                     (third (find-if
                             (lambda (el)
                               (and (equal (first el) "metaTag")
                                    (equalp (second el) '(("name" "workNumber")))))
                             (cddr
                              (lastcar (xmls:parse-to-list
                                        (babel:octets-to-string
                                         (zip:zipfile-entry-contents entry))))))))
            (format *trace-output* "~&//* Song “~a” has ID $~2,'0x (from workNumber)"
                    (pathname-name pathname) (parse-integer work-number$))
            (return-from get-asset-id (parse-integer work-number$))))))
    (let ((id (ash (logand #xff00 (sxhash asset-name)) -8)))
      (format *trace-output* "~&//* Song “~a” has ID $~2,'0x"
              (pathname-name pathname) id)
      id)))

(defun write-asset-source (kind$ predicate assets source)
  "Write a generic asset stanza for any KIND$ ASSETS (meeting PREDICATE) into bank SOURCE"
  (let ((kind (kind-by-name kind$)))
    (when (some predicate assets)
      (when (equal :map kind)
        (format source "~&~10t.include \"ZX7Decompressor.s\""))
      (format source "~&~10t.include \"Load~:(~a~).s\"~2%~:(~a~)s:" kind kind)
      (dolist (asset (remove-if-not predicate assets))
        (format source "~&~10t.byte ~a_ID" (asset->symbol-name asset))
        (format source "~&~10t.word ~a" (asset->symbol-name asset)))
      (format source "~&~10t.byte $ff~2%"))))

(defun write-asset-source/blob (assets source)
  "Write a stanza into an asset's bank SOURCE file for any BLOBs in ASSETS"
  (when (some #'blob-asset-p assets)
    (format source "~%~10t.include \"LoadBlob.s\"~2%Blobs:")
    (dolist (asset (remove-if-not #'blob-asset-p assets))
      (format source "~%~10t.byte ~a_ID" (asset->symbol-name asset))
      (format source "~%~10t.word ~a" (asset->symbol-name asset)))
    (format source "~%~10t.byte $ff~2%")))

(defun write-asset-source/script (assets source)
  "Write a stanza into an asset bank's SOURCE file for any scripts in ASSETS"
  (when (some #'script-asset-p assets)
    (let ((scripts (remove-if-not #'script-asset-p assets)))
      (format source "~&~10t.include \"ScriptIncludes.s\"")
      (format source "~&~10t.include \"LoadScript.s\"~2%ScriptsH:")
      (dolist (asset scripts)
        (format source "~&~10t.byte >~a" (asset->symbol-name asset)))
      (format source "~2%ScriptsL:")
      (dolist (asset scripts)
        (format source "~&~10t.byte <~a" (asset->symbol-name asset)))
      (format source "~2%ScriptIDH:")
      (dolist (asset scripts)
        (format source "~&~10t.byte >~a_ID" (asset->symbol-name asset)))
      (format source "~&~10t.byte $ff~2%ScriptIDL:")
      (dolist (asset scripts)
        (format source "~&~10t.byte <~a_ID" (asset->symbol-name asset))))))

(defun last-segment (string char)
  "The segment of STRING, following the last instance of CHAR.

If CHAR does not occur in STRING, returns STRING."
  (if-let (position (position char string :from-end t))
    (subseq string (1+ position))
    string))

(defun write-asset-bank (bank-hex build video)
  "Write out the skeletal bank file for BANK-HEX for BUILD with VIDEO formats specified.

This will  include the assets  and asset  loaders needed for  that bank,
based on the asset listing files."
  (let* ((*bank* (parse-integer bank-hex :radix 16))
         (basename (format nil "Bank~(~2,'0x~).~a.~a" *bank* build video))
         (outfile (make-pathname :directory (list :relative "Source" "Generated")
                                 :name basename
                                 :type "s"))
         (assets (with-input-from-file (list (allocation-list-name *bank* build video))
                   (sort (loop for asset = (read-line list nil nil)
                               while asset
                               collect asset)
                         #'string<))))
    (format *trace-output* "~& Bank ~(~2,'0x~) assets: ~s" *bank* assets)
    (ensure-directories-exist outfile)
    (with-output-to-file (source outfile :if-exists :supersede)
      (format source ";;; Bank ~(~2,'0x~) file (generated by Skyline Tool)

~10tBANK = $~(~2,'0x~)

~10t.include \"StartBank.s\"

VLoadMap:~10t~:[sec
~10trts
~10tnop~;jmp LoadMap~]
VLoadSong:~10t~:[sec
~10trts
~10tnop~;jmp LoadSong~]
VLoadScript:~10t~:[sec
~10trts
~10tnop~;jmp LoadScript~]
VLoadBlob:~10t~:[sec
~10trts
~10tnop~;jmp LoadBlob~]
~2%"
              *bank* *bank*
              (some #'map-asset-p assets)
              (some #'song-asset-p assets)
              (some #'script-asset-p assets)
              (some #'blob-asset-p assets))
      (write-asset-source "Map" #'map-asset-p assets source)
      (write-asset-source "Song" #'song-asset-p assets source)
      (write-asset-source/script assets source)
      (write-asset-source/blob assets source)
      (terpri source)
      (dolist (asset assets)
        (cond ((song-asset-p asset)
               (format source "
~10t.section BankData
~a:
~10t.if TV == NTSC
~10t.binary \"Song.~a.NTSC.o\"
~10t.else
~10t.binary \"Song.~1@*~a.PAL.o\"
~10t.fi
~10t.send"
                       (asset->symbol-name asset)
                       (subseq asset (1+ (position #\/ asset)))))
              ((map-asset-p asset)
               (destructuring-bind (dir map)
                   (split-sequence #\/ (subseq asset (1+ (position #\/ asset))))
                 (format source "~&
~10t.section BankData
~a: .proc
~10t.if TV == NTSC
Binary: .binary \"Map.~a.~a.NTSC.o\"
~10t.else
Binary: .binary \"Map.~a.~a.PAL.o\"
~10t.fi
EndOfBinary = *
~10t* = Binary + 2~32t; mark up relative pointers to be actual ROM positions
~10t.word ( (Binary[2] + Binary[3] * $100) + Binary )
~10t.word ( (Binary[4] + Binary[5] * $100) + Binary )
~10t.word ( (Binary[6] + Binary[7] * $100) + Binary )
~10t.word ( (Binary[8] + Binary[9] * $100) + Binary )
~10t.word ( (Binary[10] + Binary[11] * $100) + Binary )
~10t.word ( (Binary[12] + Binary[13] * $100) + Binary )
~10t.word ( (Binary[14] + Binary[15] * $100) + Binary )
~10t* = EndOfBinary
~10t.pend
~10t.send
"
                         (asset->symbol-name asset)
                         dir map dir map)))
              ((blob-asset-p asset)
               (format source "~2%~10t.section BankData~%~10t.include \"Blob.~a.s\"~%~10t.send"
                       (subseq asset (1+ (position #\/ asset)))))
              ((script-asset-p asset)
               (format source "~2%~10t.include \"Script.~{~a.~a~}.s\""
                       (split-sequence #\/ (subseq asset (1+ (position #\/ asset))))))
              (t (error "Unknown kind of asset (“~a”)" asset))))
      (format source "~3&~10t.dsection BankData~%~10t.include \"EndBank.s\"~%"))))

(defun labels-to-include (labels-file lower upper include-file-name)
  "Extract labels between LOWER and UPPER (hex) from LABELS-FILE into Source/Generated/INCLUDE-FILE-NAME"
  (let ((low (parse-integer lower :radix 16))
        (high (parse-integer upper :radix 16))
        (include-file (make-pathname :name include-file-name
                                     :type "s"
                                     :directory '(:relative "Source" "Generated"))))
    (with-input-from-file (labs labels-file)
      (ensure-directories-exist include-file)
      (with-output-to-file (incs include-file :if-exists :supersede)
        (format *trace-output* "~&Converting ~a to include file Source/Generated/~a.s… "
                labels-file include-file-name)
        (finish-output *trace-output*)
        (format incs ";;; Generated file~2%Lib:~10t.block")
        (let ((table (make-hash-table)))
          (loop for line = (read-line labs nil nil)
                while line
                do (destructuring-bind (label value)
                       (mapcar (lambda (each)
                                 (string-trim #(#\Space #\Newline) each))
                               (split-sequence #\= line))
                     (let ((number (cond
                                     ((char= #\$ (char value 0))
                                      (parse-integer (subseq value 1) :radix 16))
                                     ((every #'digit-char-p value)
                                      (parse-integer value))
                                     (t 0))))
                       (when (and (<= low number high) (not (ends-with-subseq "_ID" label)))
                         (setf (gethash number table) label)))))
          (loop for number in (sort (copy-list (hash-table-keys table)) #'<)
                for label = (gethash number table)
                do (format incs "~&~10t~a = $~x" label number)))
        (format incs "~2%~10t.bend~%")
        (format *trace-output* "Done.")))))

(defun labels-to-forth (labels-file include-file-name)
  "Extract labels between LOWER and UPPER (hex) from LABELS-FILE into Source/Generated/INCLUDE-FILE-NAME"
  (let ((include-file include-file-name))
    (with-input-from-file (labs labels-file)
      (ensure-directories-exist include-file)
      (with-output-to-file (incs include-file :if-exists :supersede)
        (format *trace-output* "~&Converting ~a to include file Source/Generated/~a… "
                labels-file include-file-name)
        (finish-output *trace-output*)
        (format incs " ( -*- forth -*- Generated file ) ~2%")
        (let ((table (make-hash-table)))
          (loop for line = (read-line labs nil nil)
                while line
                do (destructuring-bind (label value)
                       (mapcar (lambda (each)
                                 (string-trim #(#\Space #\Newline) each))
                               (split-sequence #\= line))
                     (when-let (number (cond
                     			 ((char= #\~ (char value 0))
                     			  (logxor #xffff
                     			    (if (char= #\$ (char value 1))
                                              (parse-integer (subseq value 2) :radix 16)
                     			      (parse-integer (subseq value 1)))))
                                         ((char= #\$ (char value 0))
                                          (parse-integer (subseq value 1) :radix 16))
                                         ((every #'digit-char-p value)
                                          (parse-integer value))
                                         ((char= #\" (char value 0))
                                          (char->minifont (char value 1)))
                                         (t nil)))
                       (setf (gethash label table) number))))
          (loop for label in (sort (copy-list (hash-table-keys table)) #'string-lessp)
                for number = (gethash label table)
                do (format incs "~% : ~a ~d ; " label number)))
        (terpri incs)))
    (format *trace-output* " Done.")))

(defun check-for-absent-assets ()
  "Looks into Assets.index and searches Source directories for “forgotten” files."
  (read-assets-list)
  (let ((absent nil))
    (dolist (asset-file (loop for wild in '(#p"Source/Blobs/*.xcf"
                                            #p"Source/Maps/*.tmx"
                                            #p"Source/Scripts/*.fountain"
                                            #p"Source/Songs/*.mscz")
                              append (recursive-directory wild)))
      (let* ((dir (pathname-directory asset-file))
             (moniker (format nil "~{~a~^/~}"
                              (append (subseq dir
                                              (1+ (position "Source" dir
                                                            :test #'string=)))
                                      (cons (pathname-name asset-file) nil)))))
        (unless (gethash moniker *assets-list*)
          (push (enough-namestring asset-file) absent))))
    (when absent
      (finish-output *error-output*)
      (finish-output *standard-output*)
      (format *error-output*
              "~3&The following assets are not found in any build in Source/Assets.index:
~{~% ~a~}~2%"
              absent)
      (finish-output *error-output*))))

(defun assemble-with-64tass (source-name object-name error-stream)
  (let ((cmd (list "64tass" "--nostart" "--long-branch"
                   "--case-sensitive" "--ascii" "-Wall"
                   "-Werror=shadow" "-Werror=wrap-pc"
                   "-Wno-leading-zeros" "--m6502" "-m" "--tab-size=1"
                   "--verbose-list" "-DTV=NTSC"
                   "-I"
                   (namestring
                    (merge-pathnames #p"Source/Common/"))
                   "-I"
                   (namestring
                    (merge-pathnames #p"Source/Generated/"))
                   (enough-namestring source-name)
                   "-o"
                   (enough-namestring object-name))))
    (format *trace-output* "~&~{~a~^ ~}" cmd)
    (run-program cmd
                 :error-output error-stream
                 :ignore-error-status t)))

(defun write-assembly-skeleton-for-size (tmp.s pathname)
  (format tmp.s ";;; Temporary rig to get size of “~a”" (enough-namestring pathname))
  (format tmp.s "
~10t.include \"StartBank.s\"
~10t.include \"SpeakJet.s\"
~10tBANK=0
Start:
~10t.include ~s

~10t.dsection BankData
~10t.error format(\"$SIZE$%04x\", (* - Start))
"
          (namestring (merge-pathnames pathname)))
  (finish-output tmp.s))

(defun compress-sequential-numbers (first &optional next &rest rest)
  (cond
    (rest (let ((begin (compress-sequential-numbers first next)))
            (apply #'compress-sequential-numbers begin rest)))
    ((stringp first)
     (destructuring-bind (start$ end$) (split-sequence #\… first :count 2)
       (let ((start (parse-number start$))
             (end (parse-number end$)))
         (if (= (1+ end) next)
             (list (format nil "~d…~d" start next))
             (list first next)))))
    ((stringp next)
     (compress-sequential-numbers next first))
    ((consp first)
     (flatten (list (butlast first) (compress-sequential-numbers (lastcar first) next))))
    ((= (1+ first) next)
     (format nil "~d…~d" first next))
    (t (list first next))))

(defun assemble-file-for-size (pathname)
  (uiop/stream:with-temporary-file (:pathname object-name
                                    :prefix (concatenate 'string
                                                         (pathname-name pathname)
                                                         "-")
                                    :suffix "-GetSoloSize"
                                    :type "o"
                                    :keep nil
                                    :element-type '(unsigned-byte 8))
    (uiop/stream:with-temporary-file (:stream tmp.s
                                      :pathname temp-name
                                      :prefix (concatenate 'string
                                                           (pathname-name pathname)
                                                           "-")
                                      :suffix "-GetSoloSize"
                                      :type "s"
                                      :keep nil
                                      :direction :output
                                      :external-format :utf-8)
      (write-assembly-skeleton-for-size tmp.s pathname)
      (format *trace-output* "~&Using Turbo Assembler to get size of “~a”"
              (enough-namestring pathname))
      (let ((err (with-output-to-string (e)
                   (assemble-with-64tass temp-name object-name e))))
        (let ((size (nth-value 1 (cl-ppcre:scan-to-strings
                                  "\\$SIZE\\$([0-9a-f]{4})" err))))
          (unless size
            (cerror "Pretend it's 8kiB"
                    "Tried to assemble “~a” to get size of asset “~a”
Did not get expected $SIZE$xxxx token in:~%~a~%(~:d byte~:p)"
                    (enough-namestring temp-name)
                    (enough-namestring pathname)
                    err (length err))
            (return-from assemble-file-for-size 8192))
          (parse-integer (aref size 0) :radix 16))))))


