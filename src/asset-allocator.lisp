(in-package :skyline-tool)

(defvar *bank* nil
  "The current ROM bank number being processed during asset allocation.

@table @asis
@item Purpose
This dynamic variable tracks which ROM bank is currently being allocated.
@item Modified by
Asset allocation process.
@item Used by
@ref{fun:include-paths-for-current-bank}, @ref{fun:find-included-file}, bank-specific file generation.
@item Impact
Controls which bank-specific source directories are searched for includes.
@end table

Do not modify this variable directly; it is managed by the asset allocation system.")

(defvar *last-bank* nil
  "The final ROM bank number in the system.

@table @asis
@item Purpose
This dynamic variable identifies the highest-numbered ROM bank in the system.
@item Modified by
System initialization from project configuration.
@item Used by
@ref{fun:include-paths-for-current-bank} for bank naming logic.
@item Impact
Affects whether bank directories are named \"LastBank\" vs \"Bank$XX\".
@end table

Do not modify this variable after system initialization.")

(defun parse-assets-line (line)
  "Parse one LINE from Assets.index.

@table @asis
@item LINE
A line from the assets index file.
@end table

Returns @code{(ASSET BUILDS)} where ASSET is the asset identifier and BUILDS is a list of build targets."
  (if (or (emptyp (string-trim " " line))
          (char= #\; (char (string-trim " " line) 0)))
      (list nil nil)
      (destructuring-bind (asset &optional builds-string &rest machines)
          (split-sequence #\space line :remove-empty-subseqs t)
        (declare (ignore machines)) ;; TODO
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
  "Convert a string KIND$ representing an asset type to the corresponding keyword.

@table @asis
@item Input
@table @asis
@item KIND$
A string like @samp{Art}, @samp{Blob}, @samp{Song}, etc. Can be nil or empty.
@end table
@end table

@table @asis
@item Output
Returns the keyword @code{:ART}, @code{:BLOB}, @code{:SONG}, etc., or @code{NIL} for empty/null input.
Signals an error for unknown asset types.
@end table"
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
  "Split an ASSET identifier into its kind and name components.

@table @asis
@item Input
@table @asis
@item ASSET
A string like @samp{Art/PlayerSprite} or @samp{Song/BackgroundMusic}.
@end table
@end table

@table @asis
@item Output
Returns a list of @code{(KIND NAME)} where @var{KIND} is the asset type and @var{NAME} is the asset name.
Returns @code{NIL} if @var{ASSET} is @code{NIL}.
@end table"
  (when asset
    (let ((parts (split-sequence #\/ asset)))
      (if (string= (machine-directory-name) (first parts))
          (list (second parts) (format nil "~{~a~^/~}" (cddr parts)))
          (list (first parts) (format nil "~{~a~^/~}" (rest parts)))))))

(defun kind-of-asset (indicator)
  "Return the keyword for the kind of asset indicated by INDICATOR.

@table @asis
@item INDICATOR
Asset identifier string like @samp{Art/PlayerSprite}.
@end table

Returns the asset type keyword (@code{:ART}, @code{:BLOB}, etc.)."
  (kind-by-name (first (asset-kind/name indicator))))

(defvar *assets-list* nil)
(defvar *asset-ids-seen* nil)

(defun make-seen-ids-table ()
  "Create a hash table for tracking seen asset IDs.

Returns a new hash table with test @code{EQL} for tracking asset IDs that have been processed."
  (let ((seen-ids (make-hash-table)))
    (dolist (kind '(:song :map :script :blob))
      (setf (gethash kind seen-ids) (make-hash-table)))
    seen-ids))

(defun interpret-line-from-assets-list (line &key seen-ids index-hash)
  "Parse one LINE from Assets.index and register the asset.

@table @asis
@item LINE
A line from the assets index file.
@item SEEN-IDS
Hash table tracking already seen asset IDs (optional).
@item INDEX-HASH
Hash table for asset index data (optional).
@end table

Returns the parsed asset information or NIL if line is empty/invalid."
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
  "Read Assets.index from INDEX-FILE (using *ASSETS-LIST* cache).

@table @asis
@item INDEX-FILE
Pathname to the assets index file (default @file{Source/Assets.index}).
@end table

Returns the cached or freshly parsed assets list."
  (read-map-ids-table #p"Source/Tables/MapsIndex.ods")
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
  "Select only the assets from INDEX-HASH which are for the selected BUILD.

@table @asis
@item INDEX-HASH
Hash table mapping asset identifiers to build lists.
@item BUILD
Build target string (e.g., @samp{AA}, @samp{Public}).
@end table

Returns a list of asset identifiers that are included in the specified BUILD."
  (loop for asset being the hash-keys of index-hash
        when (member build (gethash asset index-hash) :test #'equal)
          collect asset))

(defun existing-object-file (file-name)
  "Assert that file FILE-NAME exists.

@table @asis
@item FILE-NAME
Name of the file to check.
@end table

Signals an error if the file does not exist."
  (assert (probe-file file-name) (file-name)
          "Object file not found: “~a”" (enough-namestring file-name))
  file-name)

(defun asset-file (asset &key video)
  "The filename of the object file indicated by ASSET, in format for VIDEO.

@table @asis
@item ASSET
Asset identifier string.
@item VIDEO
Video standard keyword (optional).
@end table

Returns the object filename for the asset, asserting it exists."
  (existing-object-file (asset->object-name asset :video video)))

(defun song-asset-p (asset)
  "Is ASSET a song?

@table @asis
@item ASSET
Asset identifier string.
@end table

Returns @code{T} if the asset is a song, @code{NIL} otherwise."
  (eql :song (kind-of-asset asset)))

(defun script-asset-p (asset)
  "Is ASSET a script?

@table @asis
@item ASSET
Asset identifier string.
@end table

Returns @code{T} if the asset is a script, @code{NIL} otherwise."
  (eql :script (kind-of-asset asset)))

(defun map-asset-p (asset)
  "Is ASSET a map?

@table @asis
@item ASSET
Asset identifier string.
@end table

Returns @code{T} if the asset is a map, @code{NIL} otherwise."
  (eql :map (kind-of-asset asset)))

(defun blob-asset-p (asset)
  "Is ASSET a BLOB?

@table @asis
@item ASSET
Asset identifier string.
@end table

Returns @code{T} if the asset is a BLOB, @code{NIL} otherwise."
  (eql :blob (kind-of-asset asset)))

(defgeneric asset-loader-size (kind record-count machine)
  (:method ((kind (eql :overhead)) record-count (machine (eql 7800)))
    12)
  (:method ((kind (eql :song)) record-count (machine (eql 7800)))
    256)
  (:method ((kind (eql :script)) record-count (machine (eql 7800)))
    (+ 128 (* (1+ record-count) 4)))
  (:method ((kind (eql :blob)) record-count (machine (eql 7800)))
    (+ 284 1 (* record-count 3)))
  (:method ((kind (eql :map)) record-count (machine (eql 7800)))
    (+ 384
       #|LoadMap|# 1024 #| approx XXX |#
       #| end of table |# 1
       #|per record|# (* record-count 3)))
  ;; Atari 5200 (same loader tables as 7800 on this port)
  (:method ((kind (eql :overhead)) record-count (machine (eql 5200)))
    12)
  (:method ((kind (eql :song)) record-count (machine (eql 5200)))
    256)
  (:method ((kind (eql :script)) record-count (machine (eql 5200)))
    (+ 128 (* (1+ record-count) 4)))
  (:method ((kind (eql :blob)) record-count (machine (eql 5200)))
    (+ 284 1 (* record-count 3)))
  (:method ((kind (eql :map)) record-count (machine (eql 5200)))
    (+
     #|LoadMap|# 1024 #| approx XXX |#
     #| end of table |# 1
     #|per record|# (* record-count 3)))
  ;; Atari 400/800 delegate to 5200
  (:method ((kind (eql :song)) record-count (machine (eql 400)))
    (asset-loader-size kind record-count 5200))
  (:method ((kind (eql :script)) record-count (machine (eql 400)))
    (asset-loader-size kind record-count 5200))
  (:method ((kind (eql :blob)) record-count (machine (eql 400)))
    (asset-loader-size kind record-count 5200))
  (:method ((kind (eql :map)) record-count (machine (eql 400)))
    (asset-loader-size kind record-count 5200))
  (:method ((kind (eql :song)) record-count (machine (eql 800)))
    (asset-loader-size kind record-count 5200))
  (:method ((kind (eql :script)) record-count (machine (eql 800)))
    (asset-loader-size kind record-count 5200))
  (:method ((kind (eql :blob)) record-count (machine (eql 800)))
    (asset-loader-size kind record-count 5200))
  (:method ((kind (eql :map)) record-count (machine (eql 800)))
    (asset-loader-size kind record-count 5200))
  (:method ((kind (eql :script)) record-count (machine (eql 20953)))
    (+ 96 (* (1+ record-count) 3))))

(defun bank-size (asset-size-hash)
  "The size of the ROM bank indicated by ASSET-SIZE-HASH plus overhead."
  (let ((assets (hash-table-keys asset-size-hash)))
    (+ (asset-loader-size :overhead (length assets) *machine*)
       (loop for kind in (remove-duplicates
                          (mapcar #'kind-of-asset assets))
             sum (asset-loader-size kind
                                    (count-if (lambda (x)
                                                (eql kind (kind-of-asset x)))
                                              assets)
                                    *machine*))
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
    (7800 #x4000)
    ;; Atari Lynx (Phantasia #1321 / Phase 1 #1322): 16 KiB logical bank,
    ;; 32 banks × 16 KiB = 512 KiB total cart, packaged via LNX header
    ;; produced by @code{write-cart-header} (machine 200).
    (200 #x4000)
    ;; 32 KiB per bank × 32 banks = 1 MiB SuperCart-style image (5200, 400, 800).
    ((5200 400 800) #x8000)
    ;; Z80 targets: placeholder 16 KiB per bank until cartridge map is fixed per title.
    ((3010 837 2110 9918 1000) #x4000)))

(defun assembler-invocation-macro ()
  "Makefile token for 64tass: @samp{${AS7800}} or @samp{${AS5200}} depending on @code{*MACHINE*}.
Z80 machines use @samp{${ASZ80}} (z80asm) when wired in the port Makefile."
  (case *machine*
    ((5200 400 800) "${AS5200}")
    ((3010 9918 1000) "${ASZ80}")
    (t "${AS7800}")))

(defun try-allocation-sequence (sequence file-sizes &key video)
  "Attempt to allocate assets in SEQUENCE into ROM banks.

@table @asis
@item SEQUENCE
List of assets to allocate.
@item FILE-SIZES
Hash table mapping assets to their sizes.
@item VIDEO
Video standard for allocation (optional).
@end table

Returns allocation result if successful, NIL if allocation fails."
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
  "Compute the size of ASSET-FILE for ROM allocation.

@table @asis
@item ASSET-FILE
Pathname or asset identifier.
@item FILE-SIZES
Optional hash table of pre-computed file sizes.
@end table

Returns the size in bytes required for the asset in ROM."
  (let ((n (cond ((equal "o" (pathname-type asset-file))
                  (ql-util:file-size asset-file))
                 ((equal "s" (pathname-type asset-file))
                  (assemble-file-for-size asset-file))
                 (t (cerror "Pretend asset size is 8kiB"
                            "Don't know how to estimate size of “~a”"
                            (enough-namestring asset-file))
                    8192))))
    (when (zerop n)
      (setf n (max 8192 (or (ignore-errors (ql-util:file-size asset-file)) 8192))))
    (when file-sizes
      (when (< (* 12 1024) n)
        (warn "Asset file “~a” is over 12kiB (~~~:dkiB); ROM packing may be tight"
              (enough-namestring asset-file)
              (round n 1024)))
      (setf (gethash asset-file file-sizes) n))
    n))

(defconstant +permutation-allocation-limit+ 12
  "Above this many assets, @code{map-permutations} overflows the stack.")

(defun find-best-allocation (assets &key build video)
  "Finds the optimal allocation of ASSETS into ROM banks for BUILD and VIDEO mode.

@table @asis
@item ASSETS
List of asset identifiers to allocate
@item BUILD
Build target (e.g., \"Public\", \"Demo\", \"AA\")
@item VIDEO
Video standard (e.g., :ntsc, :pal)
@end table

@table @asis
@item Returns
Two values: hash table mapping assets to bank numbers, and file sizes hash table
@end table

@table @asis
@item Behavior
For at most @code{+PERMUTATION-ALLOCATION-LIMIT+} assets, tries all
permutations of asset ordering to find one that fits within available ROM
banks.  Larger builds use a small set of greedy orderings instead.
@end table"
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
      (flet ((try-fit (sequence)
               (incf tries)
               (let ((try (try-allocation-sequence sequence file-sizes
                                                   :video video)))
                 (when (<= (hash-table-count try) available-banks)
                   (format *trace-output* " got a fit in ~:d tr~:@p" tries)
                   (return-from find-best-allocation (values try file-sizes))))))
        (if (> (length assets) +permutation-allocation-limit+)
            (progn
              (format *trace-output*
                      "~&~:d assets: skipping brute-force permutations; ~
trying greedy orderings for ~:d ROM bank~:p … "
                      (length assets) available-banks)
              (try-fit assets)
              (try-fit (sort (copy-list assets) #'>
                             :key (lambda (asset)
                                    (gethash (asset-file asset :video video)
                                             file-sizes))))
              (try-fit (sort (copy-list assets) #'<
                             :key (lambda (asset)
                                    (gethash (asset-file asset :video video)
                                             file-sizes)))))
            (progn
              (format *trace-output*
                      "~&Will try every possible permutation to find one that fits into ~:d ROM bank~:p … "
                      available-banks)
              (map-permutations #'try-fit assets))))
      (error "Unable to fit ~:d asset~:p into ~:d bank~:p of ROM, tried ~:d permutation~:p"
             (length assets) available-banks tries))))

(define-constant +all-builds+ '("AA" "Public" "Demo")
  :test #'equalp)

(define-constant +all-video+ '(:ntsc :pal :secam)
  :test #'equalp)

(defun supported-video-types (&optional (machine *machine*))
  "Return the list of video types supported by MACHINE.

Portable and single-region handheld devices return a one-element list
@code{(:ntsc)} so that Makefile generation iterates only once and
@code{asset->object-name} emits a single video-suffix-free target.
TV-connected machines with both NTSC and PAL releases return
@code{(:ntsc :pal)}; the catch-all additionally includes SECAM."
  (case machine
    ;; Portable/single-region devices: Lynx, Game Boy family, Game Gear,
    ;; WonderSwan family, Virtual Boy.  All emit video-independent objects.
    ((200    ; Lynx
      810    ; VB
      837    ; GG
      3296   ; GBA
      4800   ; WS
      6800   ; WSC
      20953  ; CGB
      35902) ; DMG
     '(:ntsc))
    (5200 '(:ntsc))
    ((400 800 20 64 128 7800 7850) '(:ntsc :pal))
    (t '(:ntsc :pal :secam))))

(defvar *first-assets-bank* nil)

(defvar *makefile-bank-rules-emitted* nil
  "During @code{write-master-makefile}, EQ hash table keyed by
@code{(bank build video kind)} to skip duplicate GNU Make rules for the same
@file{Object/$(PORT)/Bank…} targets (avoids “overriding recipe” warnings).")

(defvar *asset-bank-list-batch* nil
  "Integer bank numbers for which @file{BankNN.*.list} rules are deferred.

@code{write-asset-bank-makefile} records banks here; @code{emit-grouped-asset-bank-list-rules}
emits one GNU Make rule for all pending @file{*.list} targets so
@command{allocate-assets} runs once per build/video.  That avoids parallel
@command{make -j} invoking @command{allocate-assets} concurrently (each run
rewrites every bank list file, which previously truncated or corrupted outputs).")

(defun reset-asset-bank-list-batch ()
  "Clear @code{*ASSET-BANK-LIST-BATCH*} before scanning banks for one build/video."
  (setf *asset-bank-list-batch* nil))

(defun note-asset-bank-list-target (bank)
  "Record BANK for a later grouped @file{BankNN.*.list} Makefile rule."
  (push bank *asset-bank-list-batch*))

(defun emit-grouped-asset-bank-list-rules (build video)
  "Emit one Makefile rule for every pending @file{Source/Generated/$(PORT)/Bank*.list}.

Prerequisites match the former per-bank rules; the recipe runs
@command{allocate-assets} exactly once for BUILD and VIDEO, then clears the batch."
  (when *asset-bank-list-batch*
    (let* ((banks (sort (remove-duplicates *asset-bank-list-batch*) #'<))
           (all-assets (all-assets-for-build build))
           (asset-objects (apply #'nconc (mapcar (rcurry #'asset->deps-list build) all-assets)))
           (target-lines
             (loop for b in banks
                   collect (format nil "Source/Generated/${PORT}/Bank~a.~a.~a.list"
                                   (string-upcase (format nil "~2,'0x" b))
                                   build video))))
      (format t "~%# Batched Bank*.list targets: allocate-assets writes every bank list;~%")
      (format t "# one recipe per build/video avoids parallel make clobbering those files.~%")
      ;; One physical target line (all BankNN…list files, space-separated), then
      ;; @samp{: Source/Assets.index \\}.  GNU Make 4.4 rejects some multi-line
      ;; multi-target groupings (``multiple target patterns'') when the first line
      ;; ends with @samp{\\} before any @samp{:} appears on the continued logical line.
      (format t "~{~a~^ ~}: Source/Assets.index \\~%" target-lines)
      (format t "          bin/skyline-tool \\~%")
      (loop for dep on asset-objects
            do (if (cdr dep)
                   (format t "          ~a \\~%" (car dep))
                   (format t "          ~a~%" (car dep))))
      (format t "	bin/skyline-tool --port ${PORT} allocate-assets ~a~%"
              build))
    (setf *asset-bank-list-batch* nil)))

(defun %makefile-game-title ()
  "Returns the game title used in generated Makefile target names.

This keeps parser tests usable when the command-line launcher has not bound
@code{*GAME-TITLE*} yet."
  (if (and (boundp '*game-title*)
           (symbol-value '*game-title*))
      (symbol-value '*game-title*)
      "Phantasia"))

(defun %makefile-video-key (video)
  "Normalize VIDEO for generated Makefile hash keys.

@table @asis
@item VIDEO
Keyword, string, or @code{NIL} for video-independent targets.
@end table"
  (etypecase video
    (null "NONE")
    (keyword (symbol-name video))
    (string video)))

(defun first-assets-bank (build)
  "First bank number that has no hand-written @file{BankNN/BankNN.s} (asset-only from there on).

Uses the same path layout as @code{bank-source-pathname}
  (e.g. @file{Source/Code/5200/Banks/Bank00/Bank00.s})."
  (declare (ignore build))
  (or *first-assets-bank*
      (setf *first-assets-bank*
            (loop for bank from 0
                  unless (probe-file (bank-source-pathname bank))
                    return bank))))

(defun allocation-list-name (bank build video)
  (make-pathname :directory `(:relative "Source" "Generated" ,(machine-directory-name))
                 :name (format nil "Bank~2,'0x.~a.~a"
                               bank
                               build video)
                 :type "list"))

(defun allocation-size-name (bank build video)
  (make-pathname :directory `(:relative "Source" "Generated" ,(machine-directory-name))
                 :name (format nil "Bank~2,'0x.~a.~a"
                               bank
                               build video)
                 :type "size"))

(defun allocate-assets (build &optional supplied-machine)
  "Allocate ROM bank lists for BUILD (Demo, AA, Public, Test).

Uses special @code{*machine*} from the loaded @code{--port} unless
SUPPLIED-MACHINE overrides it for tests; defaults to machine 7800 when no port
binding exists."
  (let ((*machine* (or supplied-machine
                       (when (boundp '*machine*)
                         *machine*)
                       7800)))
    (assert (member build +all-builds+ :test 'equal) (build)
            "BUILD must be one of ~{~a~^ or ~} not “~a”" +all-builds+ build)
    (let ((assets-list (all-assets-for-build build)))
      (dolist (video (supported-video-types))
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
                do (error "No assets assigned to bank ~2,'0x" bank)
              do (ensure-directories-exist allocation-list-name)
              do (with-output-to-file (allocation-file allocation-list-name
                                                       :if-exists :supersede)
                   (format *trace-output* " $~2,'0x (#~d; ~:*~:d asset~:p) "
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
                                         (sort empty-banks #'<))))))))))

(defun number-of-banks (build video)
  (declare (ignore video))
  (ecase *machine*
    (7800 (cond
            ((equal build "Demo") 64)
            ((equal build "Test") 64)
            (t 64)))
    ((5200 400 800) 32)
    ;; Atari Lynx (Phantasia #1321 / Phase 1 #1322): 32 logical 16 KiB banks
    ;; produce a 512 KiB cart image, matching @file{Project.Lynx.json}
    ;; @samp{CartBankCount}.  All builds (Demo/Test/Public) use the same
    ;; 32-bank layout for now.
    (200 32)
    ;; Intellivision: placeholder bank count for asset Makefile layout (see
    ;; @code{write-master-makefile-for-machine} for 2609).
    (2609 32)
    ;; Z80 (SMS, Game Gear, ColecoVision, SG-1000): placeholder bank count until banking layout is finalized.
    ((3010 837 2110 9918 1000) 32)
    ;; Atari vcs800 (native / bundle host): placeholder bank count for tooling symmetry.
    (7850 32)))

(defun included-file (line)
  "Extract the filename from an assembler .include directive or COBOL COPY statement in LINE.

@table @asis
@item Input
@table @asis
@item LINE
A string containing an assembler directive like @samp{.include \"Filename.s\"} or COBOL @samp{COPY Filename.cob.}.
@end table
@end table

@table @asis
@item Output
Returns the filename (e.g., @samp{Filename}) if found, otherwise @code{NIL}.
@end table"
  (or
   ;; Check for assembler .include directive
   (let ((match (nth-value 1 (cl-ppcre:scan-to-strings "\\.include \"(.*)\\.s\"" line))))
     (when (and match (plusp (array-dimension match 0)))
       (aref match 0)))
   ;; Check for COBOL COPY statement
   (let ((match (nth-value 1 (cl-ppcre:scan-to-strings "\\s+COPY ([^A-Za-z-]+)\\." line))))
     (when (and (> (length line) 8)
	      (not (char= #\* (char line 6)))
	      match
	      (plusp (array-dimension match 0)))
       (concatenate 'string (aref match 0) ".cpy")))))

(defun included-binary-file (line)
  (let ((match (nth-value 1 (cl-ppcre:scan-to-strings "\\.binary \"(.*)\\.o\"" line))))
    (when (and match (plusp (array-dimension match 0)))
      (aref match 0))))

(defun cpu-directory-name (&optional (machine *machine*))
  (ecase machine
    ((1 2 3 8 16 20 23 64 128 200 223 264 400 800 1200 2600 5200 7800 7850)
     "6502")
    ((9 1080 1601 8011) "m68k")
    ((15) "F8")
    ((81 1000 2068 2110 3010 837 9918) "Z80")
    ((88 222 2416) "65816")
    ((821) "V810")
    ((1624) "sh2")
    ((2609) "cp1610")
    ((3296) "ARM7")))

(defun pointer-size-for-machine (&optional (machine *machine*))
  "Return the pointer size in bytes for the given MACHINE (default *machine*).
Used when @ClassName object references in Classes.Defs need architecture-appropriate
pointer width: 2 bytes for 16-bit (6502, Z80, etc.), 3 for 24-bit (65816), 4 for 32-bit (m68k, ARM7, SH2, V810)."
  (let ((cpu-name (string-downcase (cpu-directory-name machine))))
    (cond
      ((member cpu-name '("m68k" "sh2" "arm7" "v810") :test #'string=) 4)
      ((string= cpu-name "65816") 3)
      ((member cpu-name '("i286") :test #'string=) 2)
      (t 2))))

(defun machine-directory-name (&optional (machine *machine*))
  "Return the directory name for the current machine platform"
  (ecase machine
    (1 "Oric")
    (2 "A2")
    (3 "A3")
    (8 "NES")
    (9 "NG")
    (15 "F")
    (16 "TG16")
    (20 "VIC20")
    (23 "A2e")
    ((64 128) "CBM")
    (81 "ZX81")
    (88 "SNES")
    (200 "Lynx")
    (222 "2gs")
    (223 "BBC")
    (264 "C16")
    (400 "400")
    (800 "800")
    (810 "VB")
    (920 "NNG")
    (1000 "SG1000")
    (1080 "ST")
    (1200 "1200")
    (1601 "SMD")
    (1624 "32X")
    (2068 "Spc")
    (837 "GG")
    (2600 "2600")
    (2609 "Intv")
    (3000 "Vx")
    (3010 "SMS")
    (3296 "GBA")
    (4386 "HS")
    (4800 "WS")
    (5200 "5200")
    (6122 "Vs")
    (6800 "WSC")
    (7600 "O2")
    (7800 "7800")
    (7850 "vcs800")
    (7801 "SC")
    (8011 "Jag")
    (9001 "PSX")
    (9918 "ClcV")
    (2416 "CDR")
    (20953 "CGB")
    (35902 "DMG")))

(defun machine-number-by-tag (tag)
  (ecase (make-keyword (string-upcase tag))
    (:Oric 1)
    (:A2 2)
    (:A3 3)
    (:NES 8)
    (:NG 9)
    (:F 15)
    (:TG16 16)
    (:VIC 20)
    (:VIC20 20)
    (:A2e 23)
    (:C64 64)
    (:ZX81 81)
    (:SNES 88)
    (:C128 128)
    (:Lynx 200)
    (:2gs 222)
    (:BBC 223)
    (:C16 264)
    (:400 400)
    (:800 800)
    (:VB 810)
    (:GG 2110)
    (:NNG 920)
    (:1000 1000)
    (:ST 1080)
    (:1200 1200)
    (:SMD 1601)
    (:32X 1624)
    (:Spc 2068)
    (:2600 2600)
    (:Intv 2609)
    (:Vx 3000)
    (:SMS 3010)
    (:GBA 3296)
    (:HS 4386)
    (:WS 4800)
    (:5200 5200)
    (:Vs 6122)
    (:WSC 6800)
    (:O2 7600)
    (:7800 7800)
    (:vcs800 7850)
    (:SC 7801)
    (:Jag 8011)
    (:PSX 9001)
    (:ClcV 9918)
    (:CDR 2416)
    (:DMG 35902)
    (:CGB 20953)))

(defun include-paths-for-current-bank (&key cwd testp)
  "Return a list of directories to search for included files in the current bank.

@table @asis
@item CWD
Current working directory (optional).
@item TESTP
If true, include test-specific directories.
@end table

Returns a list of pathnames as directory lists for @code{CL:MAKE-PATHNAME}."
  (let* ((bank (if (= *bank* *last-bank*)
                   "LastBank"
                   (format nil "Bank~2,'0x" *bank*)))
         (machine-dir (machine-directory-name))
         (cpu-dir (cpu-directory-name))
         (base-includes (append (list (list :relative "Source" "Code" machine-dir)
			        (list :relative "Source" "Code" machine-dir "Common")
			        ;; Shared Atari 8-bit video (GTIA/ANTIC) used by 400/800/5200 via Atari8.s
			        (list :relative "Source" "Code" "Atari8" "Common")
			        (list :relative "Source" "Code" machine-dir "Routines")
			        (list :relative "Source" "Generated" "Classes" cpu-dir) ; EightBol .s output
			        (list :relative "Source" "Code" machine-dir "Classes")
			        (list :relative "Source" "Generated" machine-dir "Classes") ; Copybooks (Globals, *-Slots.cpy)
			        (list :relative "Source" "Code" machine-dir "Stagehand")
			        (list :relative "Object" machine-dir)
			        (list :relative "Object" machine-dir "Assets")
			        (list :relative "Source" "Generated" machine-dir)
			        (list :relative "Source" "Generated" machine-dir "Assets"))
                                (when (= *machine* 128)
                                  (list (list :relative "Source" "Generated" "Classes" "z80")))))
         (includes (if cwd
                       (append base-includes (list (pathname-directory cwd)))
                       base-includes)))
    (when testp
      (appendf includes (list (list :relative "Source" "Code" machine-dir "Tests"))))
    (when (probe-file (make-pathname :directory (list :relative "Source" "Code" machine-dir "Banks" bank)
                                     :name bank :type "s"))
      (appendf includes (list (list :relative "Source" "Code" machine-dir "Banks" bank))))
    includes))

(defun generated-path (path)
  (let ((platform-dir (machine-directory-name)))
    (cond
      ((equalp path '(:relative "Source" "Code" platform-dir "Common"))
       (list :relative "Source" "Generated" platform-dir "Common"))
      ((and (>= (length path) 5) (equalp (subseq path 0 5) (list :relative "Source" "Code" platform-dir "Banks")))
       (append (list :relative "Source" "Generated" platform-dir) (subseq path 4)))
      (t (error "Don't know how to find a generated path from ~a" path)))))

(defun write-blob-generation (pathname)
  (let ((blob-name (pathname-name pathname))
        (blob-path (enough-namestring pathname))
        (machine-dir (machine-directory-name))
        ;; blob-rip-7800 expects PNG; source is .xcf, so depend on .png (built from .xcf by %.png: %.xcf)
        (blob-png-path (enough-namestring (make-pathname :defaults pathname :type "png"))))
    (ecase *machine*
      (200 ; Lynx
       (format t "~%
Source/Generated/Lynx/Assets/Blob/~a.s: ~a \\
          bin/skyline-tool
	mkdir -p Source/Generated/Lynx/Assets
	bin/skyline-tool --port Lynx dispatch-png $< Source/Generated/Lynx/Assets"
               blob-name blob-path))
      (7800 ; Atari 7800
       (format t "~%
Source/Generated/~a/Assets/Blob.~a.s: ~a\\~%          bin/skyline-tool
	mkdir -p Source/Generated/~a/Assets
	bin/skyline-tool --port 7800 blob-rip-7800 $<"
               machine-dir blob-name blob-png-path machine-dir))
      ((5200 400 800) ; Maria-class cart ports — ANTIC Mode E BLOBs, not 7800 DLHeader stamps
       (format t "~%
Source/Generated/~a/Assets/Blob.~a.s: ~a\\~%          bin/skyline-tool
	mkdir -p Source/Generated/~a/Assets
	bin/skyline-tool --port ${PORT} blob-rip-5200 $<"
               machine-dir blob-name blob-png-path machine-dir))
      ((35902 20953) ; Game Boy (DMG) and Game Boy Color
       ;; Check if this is an SGB frame
       (if (search "SGB" (string-upcase blob-name))
           (format t "~%
Source/Generated/~a/Assets/Blob.SGB.~a.s: ~a~%	bin/skyline-tool
	mkdir -p Source/Generated/~a/Assets
	bin/skyline-tool --port ~a compile-sgb-frame $@ $<"
                   machine-dir blob-name blob-path machine-dir
                   (if (= *machine* 20953) "CGB" "DMG"))
           ;; Regular blob processing
           (format t "~%
Source/Generated/~a/Assets/Blob.~a.s: ~a~%	bin/skyline-tool
	mkdir -p Source/Generated/~a/Assets
	bin/skyline-tool --port ~a dispatch-png $< Source/Generated/~a/Assets"
                   machine-dir blob-name blob-path machine-dir
                   (if (= *machine* 20953) "CGB" "DMG") machine-dir)))
      (2609 ; Intellivision — tile-mapped blob screen + GRAM cards
       (format t "~%
Source/Generated/~a/Assets/Blob.~a.s: ~a\\~%          bin/skyline-tool
	mkdir -p Source/Generated/~a/Assets
	bin/skyline-tool --port ${PORT} compile-blob-intv $< $@"
               machine-dir blob-name blob-png-path machine-dir))
      ((3010 9918 1000 837 2110) ; Z80 + TMS9918 family (SMS, ClcV, SG-1000, GG, VS)
       ;; ColecoVision uses Blob.<stem>.ClcV.s (see asset->object-name); others Blob.<stem>.s.
       (if (= *machine* 9918)
           (format t "~%
Source/Generated/~a/Assets/Blob.~a.ClcV.s: ~a\\~%          bin/skyline-tool
	mkdir -p Source/Generated/~a/Assets
	bin/skyline-tool --port ${PORT} blob-rip-tms9918 $<"
                   machine-dir blob-name blob-png-path machine-dir)
           (format t "~%
Source/Generated/~a/Assets/Blob.~a.s: ~a\\~%          bin/skyline-tool
	mkdir -p Source/Generated/~a/Assets
	bin/skyline-tool --port ${PORT} blob-rip-tms9918 $<"
                   machine-dir blob-name blob-png-path machine-dir)))
      ((64 128) ; C64/C128: VIC-II character-cell blobs (via dispatch-png)
       (format t "~%
Source/Generated/~a/Assets/Blob.~a.s: ~a\\~%          bin/skyline-tool
	mkdir -p Source/Generated/~a/Assets
	bin/skyline-tool --port ${PORT} dispatch-png $< Source/Generated/~a/Assets"
               machine-dir blob-name blob-png-path machine-dir machine-dir)))))

(defun write-art-generation (pathname)
  "Generates Makefile rules for compiling art assets for the current platform.

@table @asis
@item PATHNAME
Path to the art asset file (.xcf, .png, etc.)
@end table

@table @asis
@item Output
@table @asis
@item Makefile Rules
Printed to standard output for inclusion in Makefiles
@end table

@item Behavior
Examines the target platform and generates appropriate compilation commands
for art assets. Different platforms may require different preprocessing
or compilation steps for graphics conversion."
  (let ((art-name (pathname-name pathname))
        (art-path (enough-namestring pathname))
        (machine-dir (machine-directory-name)))
    (ecase *machine*
      (200 ; Lynx
       (format t "~%
Object/~a/Assets/Art.~a.o: ~a~%	bin/skyline-tool
	mkdir -p Object/~a/Assets
	bin/skyline-tool --port Lynx compile-art-lynx $@ $<"
               machine-dir art-name art-path machine-dir))
      (7800 ; Atari 7800
       (format t "~%
Object/~a/Assets/Art.~a.o: ~a \\~{~%	~a \\~}~%	bin/skyline-tool
	mkdir -p Object/~a/Assets
	bin/skyline-tool --port 7800 compile-art-7800 $@ $<"
               machine-dir art-name art-path
               (mapcar (compose #'enough-namestring #'second)
                       (read-7800-art-index pathname))
               machine-dir))
      (2609 ; Intellivision (as1600 includes assembly output, not 64tass .o)
       (format t "~%
Object/~a/Assets/Art.~a.s: ~a \\~{~%	~a \\~}~%	bin/skyline-tool
	mkdir -p Object/~a/Assets
	bin/skyline-tool --port ${PORT} compile-art-intv $@ $<"
               machine-dir art-name art-path
               (mapcar (compose #'enough-namestring #'first)
                       (read-intv-art-index pathname))
               machine-dir))
      (20953 ; Game Boy Color
       (format t "~%
Object/~a/Assets/Art.~a.o: ~a~%	bin/skyline-tool
	mkdir -p Object/~a/Assets
	bin/skyline-tool --port CGB compile-art-cgb $@ $<"
               machine-dir art-name art-path machine-dir))
      (35902 ; Game Boy (DMG)
       (format t "~%
Object/~a/Assets/Art.~a.o: ~a~%	bin/skyline-tool
	mkdir -p Object/~a/Assets
	bin/skyline-tool --port DMG compile-art-dmg $@ $<"
               machine-dir art-name art-path machine-dir))
      (264 ; Commodore 16/Plus4 (TED)
       (format t "~%
Object/~a/Assets/Art.~a.o: ~a~%	bin/skyline-tool
	mkdir -p Object/~a/Assets
	bin/skyline-tool --port 264 compile-art-264 $@ $<"
               machine-dir art-name art-path machine-dir))
      ((1 2 8 16 20 64 88 128 223 1601 2600 3010 5200 400 800) ; Other supported machines without art support
       (error "Art generation not supported for machine ~A (~A)" *machine* (skyline-tool::machine-long-name))))))

(defun write-tsx-generation (pathname)
  (let ((machine-dir (machine-directory-name)))
    (if (and (search "Decals" (pathname-name pathname))
             (not (search "Decals2" (pathname-name pathname))))
        (format t "~%
Object/~a/Assets/Tileset.~a.o: Source/Maps/Tiles/~:*~a.tsx Source/Maps/Tiles/CommonDecals.tsx \\
          Source/Maps/Tiles/~:*~a.png Source/Maps/Tiles/CommonDecals.png \\
          bin/skyline-tool
	mkdir -p Object/~a/Assets
	bin/skyline-tool --port ${PORT} compile-tileset $< Source/Maps/Tiles/CommonDecals.tsx"
                machine-dir (pathname-name pathname) machine-dir)
        (format t "~%
Object/~a/Assets/Tileset.~a.o: Source/Maps/Tiles/~:*~a.tsx \\
          Source/Maps/Tiles/~:*~a.png \\
          bin/skyline-tool
	mkdir -p Object/~a/Assets
	bin/skyline-tool --port ${PORT} compile-tileset $<"
                machine-dir (pathname-name pathname) machine-dir))))

(defun makefile-contains-target-p (target)
  (let* ((target-str (typecase target
                       (pathname (enough-namestring target))
                       (string target)
                       (t (princ-to-string target))))
         (target-prefix (concatenate 'string target-str ":"))
         (makefiles (list #p"Source/Generated/7800/Makefile" 
                          #p"common.mak" 
                          (make-pathname :directory (list :relative "Source" "Build")
                                         :name (machine-directory-name) :type "mak")))
         (cpu-dir (cpu-directory-name)))
    (flet ((matches-p (line)
             (or (eql 0 (search target-prefix line))
                 ;; Match Source/Generated/${CPUDIR}/%Class.s pattern rule for any eightbol class
                 (and (eql 0 (search "Source/Generated/" line))
                      (search "Class.s:" line)
                      (or (search (format nil "Generated/Classes/~a/" cpu-dir) line)
                          (search "Generated/Classes/${CPUDIR}/" line))))))
      (dolist (makefile makefiles)
        (when (probe-file makefile)
          (with-open-file (stream makefile :external-format :utf-8)
            (loop for line = (read-line stream nil nil)
                  while line
                  when (matches-p line)
                    do (return-from makefile-contains-target-p t))))))
    nil))

(defun find-copybook (name)
  (make-pathname :directory (list :relative "Source" "Generated"
                                  (machine-directory-name) "Classes")
                 :name name :type "cpy"))

(defun find-included-file (name &key cwd testp)
  "Find the pathname of an included source file NAME.

@table @asis
@item NAME
The base name of the file to find (without extension).
@item CWD
Current working directory (optional).
@item TESTP
If true, include test directories in search.
@end table

Searches for .s, .cob, or .cpy sources.

Returns the pathname of the found file, or signals an error if not found."
  ;; Check    for   COBOL    copybooks   in    Generated/Classes   (e.g.
  ;; Phantasia-Globals.cpy, Basic-Object-Slots.cpy)
  (let ((cobol-path (make-pathname :directory (list :relative "Source" "Generated"
                                                    (machine-directory-name) "Classes")
                                   :name name :type "cpy")))
    (when (probe-file cobol-path)
      (return-from find-included-file cobol-path)))
  (let ((cobol-path (make-pathname :directory (list :relative "Source" "Classes")
                                   :name name :type "cob")))
    (when (probe-file cobol-path)
      (return-from find-included-file cobol-path)))

  (let ((generated-asset-pathname
          (make-pathname :directory (list :relative "Source" "Generated"
				  (machine-directory-name) "Assets")
                         :name name :type "s")))
    (when (some (lambda (frag)
                  (eql 0 (search frag name)))
                (list "Song." "Art." "Blob." "Script."))
      (return-from find-included-file generated-asset-pathname)))
  ;; EightBol-generated          class           assembly          (e.g.
  ;; Source/Generated/Classes/6502/MummyCourseClass.s               from
  ;; MummyCourse.cob)   When  NAME   ends   with   "Class",  check   for
  ;; corresponding  .cob;   if  present,   use  eightbol   output  path.
  ;; Return  path  relative  to  project  root  so  Makefile  deps  work
  ;; regardless of project-root resolution.
  (when (and (>= (length name) 5)
             (string-equal (subseq name (- (length name) 5)) "Class"))
    (let* ((cob-name (header-case (subseq name 0 (- (length name) 5))))
           (cob-path (make-pathname :directory (list :relative "Source" "Classes")
                                    :name cob-name :type "cob")))
      (when (probe-file cob-path)
        (return-from find-included-file
          (make-pathname :directory (list :relative "Source" "Generated" "Classes"
                                          (cpu-directory-name))
                         :name name :type "s")))))
  (dolist (path (include-paths-for-current-bank :cwd cwd :testp testp))
    (let ((possible-file (make-pathname :directory path :name name :type "s")))
      (when (probe-file possible-file)
        (return-from find-included-file possible-file))))
  (let ((generated-pathname
          (make-pathname :directory (list :relative "Source" "Generated" (machine-directory-name))
                         :name name :type "s")))
    (when (skyline-tool-writes-p generated-pathname)
      (return-from find-included-file generated-pathname))
    (when (makefile-contains-target-p generated-pathname)
      (return-from find-included-file generated-pathname)))
  (error "Cannot find a possible source for included ~:[source~;test~] ~
file ~a.s in bank $~2,'0x~
~@[~&Current working directory: ~a~]~
~@[~&TestP: ~a~]"
         testp name *bank* cwd testp))

(defun find-included-binary-file (name)
  (when (search "StagehandHigh" name)
    (return-from find-included-binary-file
      (make-pathname :directory (list :relative "Object" (machine-directory-name))
                     :name "StagehandHigh" :type "o")))
  (when (search "StagehandLow" name)
    (return-from find-included-binary-file
      (make-pathname :directory (list :relative "Object" (machine-directory-name))
                     :name "StagehandLow" :type "o")))
  (when (eql 0 (search "Art." name))
    (let ((possible-file (make-pathname :directory (list :relative "Source" "Art" (machine-directory-name))
                                        :name (subseq name 4) :type "art")))
      (when (probe-file possible-file)
        (return-from find-included-binary-file
          (make-pathname :directory (list :relative "Object" (machine-directory-name) "Assets")
                         :name name :type "o")))))
  (when (eql 0 (search "Tileset." name))
    (let ((possible-file (make-pathname
                          :directory (list :relative "Source" "Maps" "Tiles")
                          :name (subseq name 8) :type "tsx")))
      (when (probe-file possible-file)
        (return-from find-included-binary-file
          (make-pathname :directory (list :relative "Object" (machine-directory-name) "Assets")
                         :name name :type "o")))))
  (when (eql 0 (search "Blob." name))
    (let ((possible-file (make-pathname :directory (list :relative "Source" "Blobs" (machine-directory-name))
                                        :name (subseq name 5) :type "xcf")))
      (when (probe-file possible-file)
        (return-from find-included-binary-file
          (make-pathname :directory (list :relative "Source" "Generated" (machine-directory-name) "Assets")
                         :name name :type "s")))))
  (when (eql 0 (search "Song." name))
    (let ((possible-file (make-pathname :directory '(:relative "Source" "Songs")
                                        :name (subseq name 5) :type "mscz")))
      (when (probe-file possible-file)
        (return-from find-included-binary-file
          (make-pathname :directory (list :relative "Object"
				  (machine-directory-name) "Assets")
                         :name name :type "o")))))
  (error "Cannot find a possible source for included binary file ~a.o in bank ~2,'0x"
         name *bank*))

(defun eightbol-compile (source
		     &optional
		       (target (make-pathname :directory (list :relative "Source"
						       "Classes"
						       (cpu-directory-name))
					:name (concatenate 'string
						         (pascal-case (pathname-name source))
						         "Class")
					:type "s")))
  (eightbol::compile-eightbol
   (list source)
   :cpus (list (make-keyword (string-upcase
			(cpu-directory-name))))
   :copybook-paths (list "Source/Classes"
		     (format nil "Source/Generated/~a/Classes"
			   (machine-directory-name)))
   :output-file target))

(defun recursive-read-deps (source-file &key testp)
  (unless (equal (pathname-type source-file) "o")
    (unless (probe-file source-file)
      (cond
        ((skyline-tool-writes-p source-file)
         (write-source-file source-file))
        ((eightbol-class-file-p source-file)
         (let* ((eightbol-file (make-pathname
			  :directory '(:relative "Source" "Classes")
			  :name (header-case
			         (subseq (pathname-name source-file)
				       0 (- (length (pathname-name source-file)) 5)))
			  :type "cob"))
	      (copybooks (with-input-from-file (eightbol eightbol-file)
		         (loop for line = (read-line eightbol nil nil)
			     while line
			     for included = (included-file line)
			     for file = (when included (find-copybook included))
			     when file collect file))))
	 (return-from recursive-read-deps
	   (remove-duplicates (list* source-file eightbol-file copybooks)
			  :test #'equal))))
        (t
         (error "Can't find “~a” and don't know how to make it~2%(~s)"
                (enough-namestring source-file) source-file))))
    (with-input-from-file (source source-file)
      (let* ((testp (or testp
		    (when (search "Tests" (namestring source-file)) t)))
             (includes (loop for line = (read-line source nil nil)
                             while line
                             for included = (included-file line)
                             for binary = (included-binary-file line)
                             for file = (cond
				  (included (find-included-file included
							  :testp testp))
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

(define-constant +skyline-writes-files+
    (list "ActorPrototypes" 'write-actor-prototypes
          "ObjectPrototypes" 'write-object-prototypes
          "AnimationTable" 'compile-animation-sequences
          "AssetIDs" 'write-asset-ids
          "Asset-IDs" 'write-asset-ids
          "EquipmentIndex" 'write-equipment-index
          "FlagLabels" 'write-flags-tables
          "ItemDropTable" 'compile-item-drops
          "DocksIndex" 'write-docks-index
          "CharacterIDs" 'write-character-ids
          "ClassConstants" 'make-classes-for-oops
          "Classes" 'make-classes-for-oops
          "ClassSizes" 'make-classes-for-oops
          "ClassMethods" 'make-classes-for-oops
          "ClassInheritance" 'make-classes-for-oops
          "InventoryLabels" 'write-inventory-tables
          "KeyLabels" 'write-keys-tables
          "Orchestration" 'write-orchestration)
  :test 'equalp)

(defun eightbol-class-file-p (pathname)
  "True if PATHNAME is an EightBol-generated class assembly

(Source/Generated/Classes/${CPU}/${ClassName}Class.s)."
  (let ((name (pathname-name pathname))
        (dir (ensure-list (pathname-directory pathname))))
    (and (string= (pathname-type pathname) "s")
         (>= (length name) 5)
         (string-equal (subseq name (- (length name) 5)) "Class")
         (member "Generated" dir :test #'string=)
         (member "Classes" dir :test #'string=)
         (probe-file (make-pathname :directory (list :relative "Source" "Classes"
					   (cpu-directory-name))
                                    :name (header-case (subseq name 0 (- (length name) 5)))
                                    :type "cob")))))

(defun skyline-tool-writes-p (pathname)
  "Check if PATHNAME is a file that Skyline-Tool can generate.

PATHNAME: A pathname object.
Returns a function to generate the file if Skyline-Tool handles it, otherwise NIL.
Checks for files in Generated directories with specific names or containing 'Palette'."
  (and (member (pathname-type pathname) '("s" "forth" "cpy") :test #'string=)
       (member "Generated" (pathname-directory pathname) :test #'string=)
       (or (when-let (found (member (pathname-name pathname) +skyline-writes-files+
			      :test #'equal))
             (second found))
           (when (and (boundp '*game-title*)
                      (symbol-value '*game-title*)
                      (string-equal (pathname-name pathname)
                                    (concatenate 'string (symbol-value '*game-title*) "-Globals"))
                      (string= (pathname-type pathname) "cpy"))
             'write-globals-copybook)
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

(defun all-portable-assets ()
  (let ((source-prefix-length
          (length (pathname-directory (merge-pathnames #p"Source/")))))
    (loop for (dir . type) in '(("Maps" . "tmx") ("Songs" . "mscz")
                                ("Scripts" . "fountain"))
          append
          (mapcar
           (lambda (pathname)
	   (subseq
	    (enough-namestring
	     (make-pathname :directory
			(append (list :relative "Source")
			        (subseq (pathname-directory (merge-pathnames pathname))
				      source-prefix-length))
			:name (pathname-name pathname)
			:version nil
			:type nil))
	    (length "Source/")))
           (recursive-directory
	  (make-pathname :directory (list :relative "Source" dir)
                           :name :wild
                           :type type))))))

(defun all-bare-assets ()
  (append
   (mapcar
    (lambda (pathname)
      (let ((path (enough-namestring pathname)))
        (subseq path (length "Source/") (position #\. path :from-end t))))
    (recursive-directory
     (make-pathname :directory (list :relative "Source" "Blobs" (machine-directory-name))
		:name :wild
		:type "xcf")))
   (all-portable-assets)))

(defun %asset-leaf-name (name)
  "Returns the final path component of asset NAME.

@table @asis
@item NAME
Slash-separated asset name component.
@end table

@table @asis
@item Return
The final component of @var{NAME}.
@end table"
  (car (last (split-sequence #\/ name))))

(defun asset->object-name (asset-indicator &key (video (when (boundp *region*) *region*)))
  "Return the generated Makefile target path for ASSET-INDICATOR.

@table @asis
@item ASSET-INDICATOR
Asset identifier such as @code{Blobs/TitleCard}, @code{Scripts/Title},
@code{Songs/Title}, or @code{Maps/Solace/AncientBurialSite2}.
@item VIDEO
Video standard keyword (e.g.@: @code{:ntsc}, @code{:pal}) for assets
whose object files vary by video mode.  Required for Songs and Maps on
all machines except portable/single-region devices and Intellivision.
@end table

@table @asis
@item Return
String naming the generated Makefile target for @code{*machine*}.
Machines are divided into three naming conventions:

@itemize
@item
@strong{Standard video-dependent} — Atari 7800/vcs800/5200/400/800 and
all TV-connected ports (NES, SNES, SMS, TG16, etc.): object paths include
the video-standard suffix, e.g.@:
@file{Object/NES/Assets/Song.Title.ntsc.o}.

@item
@strong{Portable/single-region} — Atari Lynx and all handheld/portable
ports (DMG, CGB, GBA, GG, WS, WSC, VB): object paths omit the video
suffix, e.g.@: @file{Object/DMG/Assets/Song.Title.o}.  Blobs and Scripts
are placed under @file{Source/Generated/@var{PORT}/Assets/}.

@item
@strong{Special-cased platforms} — Intellivision (Songs emitted as
@file{.s} source), CBM (C64/C128, @file{.CBM} suffix), and ClcV
(ColecoVision, @file{.ClcV} suffix).
@end itemize

@item Faults
Signals @code{simple-error} for any @code{*machine*} value not
registered in @code{machine-directory-name}.
@end table"
  (let ((machine-dir (machine-directory-name)))
    (ecase *machine*
      ;; Standard video-dependent machines: Atari family plus all TV-connected
      ;; ports.  Object-file names include the video-standard suffix so that
      ;; NTSC and PAL builds produce distinct targets.
      ((7800 7850 5200 400 800
             ;; Additional TV-connected ports:
             1      ; Oric
             2      ; A2
             3      ; A3
             8      ; NES
             9      ; NG
             15     ; F
             16     ; TG16
             20     ; VIC20
             23     ; A2e
             81     ; ZX81
             88     ; SNES
             222    ; 2gs
             223    ; BBC
             264    ; C16
             920    ; NNG
             1000   ; SG1000
             1080   ; ST
             1200   ; 1200
             1601   ; SMD
             1624   ; 32X
             2068   ; Spc
             2416   ; CDR
             2600   ; 2600
             3000   ; Vx
             3010   ; SMS
             4386   ; HS
             6122   ; Vs
             7600   ; O2
             7801   ; SC
             8011   ; Jag
             9001)  ; PSX
       (destructuring-bind (kind name) (asset-kind/name asset-indicator)
(cond ((equal kind "Songs")
      (format nil "Object/~a/Assets/Song.~a.~a.o"
              machine-dir name video))
((equal kind "Maps")
      (format nil "Object/~a/Assets/Map.~a.~a.o"
              machine-dir (substitute #\. #\/ name) video))
	     ((equal kind "Scripts")
	      (format nil "Source/Generated/~a/Assets/Script.~a.s"
                        machine-dir (substitute #\. #\/ name)))
	     ((equal kind "Blobs")
	      (format nil "Source/Generated/~a/Assets/Blob.~a.s"
                        machine-dir (%asset-leaf-name name)))
	     (t
	      (format nil "Object/~a/Assets/~a.~a.o" machine-dir kind name)))))
      (2609
       (destructuring-bind (kind name) (asset-kind/name asset-indicator)
         (cond ((equal kind "Songs")
                (format nil "Source/Generated/~a/Assets/Song.~a.s"
                        machine-dir name))
               ((equal kind "Maps")
                (assert (not (null video)))
                (format nil "Object/~a/Assets/Map.~a.~a.o"
                        machine-dir (substitute #\. #\/ name) video))
               ((equal kind "Scripts")
                (format nil "Source/Generated/~a/Assets/Script.~a.s"
                        machine-dir (substitute #\. #\/ name)))
               ((equal kind "Blobs")
                (format nil "Source/Generated/~a/Assets/Blob.~a.s"
                        machine-dir (%asset-leaf-name name)))
               (t
                (format nil "Object/~a/Assets/~a.~a.o" machine-dir kind name)))))
      ;; Portable/single-region devices: Atari Lynx plus all handheld ports
      ;; (DMG, CGB, GBA, GG, WS, WSC, VB).  These have no NTSC/PAL variant —
      ;; the hardware has its own display — so Maps and Songs targets are
      ;; emitted without a video suffix.  Blobs and Scripts go under
      ;; Source/Generated/<PORT>/Assets/.
      ((200    ; Lynx
        810    ; VB (Virtual Boy)
        837    ; GG (Game Gear)
        3296   ; GBA (Game Boy Advance)
        4800   ; WS (WonderSwan)
        6800   ; WSC (WonderSwan Color)
        20953  ; CGB (Game Boy Color)
        35902) ; DMG (Game Boy)
       (destructuring-bind (kind name) (asset-kind/name asset-indicator)
         (cond ((equal kind "Songs")
                (format nil "Object/~a/Assets/Song.~a.o"
                        machine-dir name))
               ((equal kind "Maps")
                (format nil "Object/~a/Assets/Map.~a.o"
                        machine-dir (substitute #\. #\/ name)))
               ((equal kind "Scripts")
                (format nil "Source/Generated/~a/Assets/Script.~a.s"
                        machine-dir (substitute #\. #\/ name)))
               ((equal kind "Blobs")
                (format nil "Source/Generated/~a/Assets/Blob.~a.s"
                        machine-dir (substitute #\. #\/ name)))
               (t
                (format nil "Object/~a/Assets/~a.~a.o"
                        machine-dir kind name)))))
      ((64 128) (destructuring-bind (kind name) (asset-kind/name asset-indicator)
	        (cond ((equal kind "Songs")
		     (format nil "Object/~a/Assets/Song.~a.~a.CBM.o"
                                 machine-dir name video))
		    ((equal kind "Maps")
		     (format nil "Object/~a/Assets/Map.~a.~a.CBM.o"
                                 machine-dir (substitute #\. #\/ name) video))
		    ((equal kind "Scripts")
		     (format nil "Source/Generated/~a/Assets/Script.~a.CBM.s"
                                 machine-dir (substitute #\. #\/ name)))
		    ((equal kind "Blobs")
		     (format nil "Source/Generated/~a/Assets/Blob.~a.CBM.s"
                                 machine-dir name))
		    (t
		     (format nil "Object/~a/Assets/~a.~a.o"
                                 machine-dir kind name)))))
      (9918 (destructuring-bind (kind name) (asset-kind/name asset-indicator)
	    (cond ((equal kind "Songs")
		 (assert (not (null video)))
		 (format nil "Object/~a/Assets/Song.~a.~a.ClcV.o"
                             machine-dir name video))
		((equal kind "Maps")
		 (assert (not (null video)))
		 (format nil "Object/~a/Assets/Map.~a.~a.ClcV.o"
                             machine-dir (substitute #\. #\/ name) video))
		((equal kind "Scripts")
		 (format nil "Source/Generated/~a/Assets/Script.~a.ClcV.s"
                             machine-dir (substitute #\. #\/ name)))
		((equal kind "Blobs")
		 (format nil "Source/Generated/~a/Assets/Blob.~a.ClcV.s"
                             machine-dir (%asset-leaf-name name)))
		(t
		 (format nil "Object/~a/Assets/~a.~a.o" machine-dir kind name))))))))

(defun asset->deps-list (asset-indicator build)
  "Return a list of one-line dependency path strings for ASSET-INDICATOR in BUILD.

Each element is a single path suitable for Makefile continuation lines (one path per line)."
  (declare (ignore build))
  (let ((machine-dir (machine-directory-name)))
    (destructuring-bind (kind name) (asset-kind/name asset-indicator)
      (cond ((equal kind "Songs")
	   (list* (format nil "Source/Generated/~a/Orchestration.s" machine-dir)
		(loop for video in (supported-video-types)
                          collect (format nil "Object/~a/Assets/Song.~a.~a.o"
                                          machine-dir name video))))
	  ((equal kind "Maps")
	   (loop for video in (supported-video-types)
                   collect (asset->object-name asset-indicator :video video)))
	  ((equal kind "Blob")
	   (list (format nil "Source/Generated/Assets/Blob.~a.s" name)))
	  ((equal kind "Art")
	   (list (format nil "Source/Generated/Assets/Art.~a.s" name)))
	  (t (list (asset->object-name asset-indicator)))))))

(defun asset->symbol-name (asset-indicator)
  (destructuring-bind (kind &rest name) (split-sequence #\/ asset-indicator)
    (format nil "~a_~{~a~^_~}"
	  (subseq kind 0 (1- (length kind)))
	  name)))

(defun asset->source-name (asset-indicator)
  (destructuring-bind (kind &rest name) (split-sequence #\/ asset-indicator)
    (if (equal kind "Blobs")
        (format nil "Source/Blobs/~a/~a.png"
                (machine-directory-name)
                (%asset-leaf-name (format nil "~{~a~^/~}" name)))
        (format nil "Source/~a~{/~a~}.~a" kind name
                (cond
                  ((equal kind "Maps") "tmx")
                  ((equal kind "Songs") "midi")
                  ((equal kind "Scripts") "fountain")
                  (t (error "Asset kind ~a not known" kind)))))))

(defun asset-compilation-line (asset-indicator &key video)
  "Generates the shell command line for compiling an ASSET-INDICATOR for VIDEO mode.

@table @asis
@item ASSET-INDICATOR
Asset identifier in format 'Kind/Name' (e.g., 'Songs/Title', 'Maps/Level1')
@item VIDEO
Video standard (:ntsc, :pal, etc.) for video-specific assets
@end table

@table @asis
@item Returns
String containing the compilation command for make
@end table

@item Behavior
Determines the appropriate compilation tool and parameters based on asset type
and target platform. Handles special cases for different machines and video modes."
  (let ((machine-dir (machine-directory-name)))
    (destructuring-bind (kind &rest name) (split-sequence #\/ asset-indicator)
      (cond
        ((equal kind "Maps")
         ;; Intv (2609): compile-map still emits 7800 .map; future compile-map-intv
         ;; should emit per-quadrant TL/TR/BL/BR GROM ($0000-$00FF) or GRAM
         ;; ($0100+) card refs with color in tileset data (GROM-first dedup).
         (format nil "bin/skyline-tool --port ${PORT} compile-map $<"))
        ((equal kind "Songs")
         (ecase *machine*
           (16 ; TG16
	  (format nil "bin/skyline-tool --port ${PORT} compile-midi $< HUC6280 ~a $@" video))
           (222 ; Apple IIGS
	  (format nil "bin/skyline-tool --port ${PORT} compile-midi $< DOC ~a $@" video))
           ;; FIXME: Dedicated Mikey / Lynx  song backend; HOKEY path is
           ;; a stub so master Makefiles can be emitted and parsed.
           (200
            (format nil "bin/skyline-tool --port ${PORT} compile-midi $< HOKEY ~a $@" video))
           ((5200 7800 400 800 7850)
	  (format nil "bin/skyline-tool --port ${PORT} compile-midi $< HOKEY ~a $@" video))
           (2609 ; Intellivision — AY-3-8910 PSG (STIC is display only; not used for music)
	  (format nil "bin/skyline-tool --port ${PORT} compile-music $@ $< 2609 AY-3-8910 ~a" video))
           ((3010 9918 1000) ; SMS, ColecoVision, SG-1000 — SN76489 PSG
	  (format nil "bin/skyline-tool --port ${PORT} compile-midi $< SN76489 ~a $@" video))
           (35902 ; DMG
	  (format nil "bin/skyline-tool --port ${PORT} compile-midi $< DMG ~a $@" video))))
        ((equal kind "Scripts")
         ;; One shell line for compile-script (FROM and FORTH); next line compile-forth.
         ;; Do not split compile-script across lines without a shell @samp{\\} — a folded
         ;; Lisp string drops the backslash and Make then invokes compile-script with only
         ;; @samp{$<}.
         (format nil "bin/skyline-tool --port ${PORT} compile-script $< Source/Generated/~a/Assets/Script.~{~a~^.~}.forth~%	bin/skyline-tool --port ${PORT} compile-forth Source/Generated/~a/Assets/Script.~{~a~^.~}.forth $@"
                 machine-dir name machine-dir name))
        ((equal kind "Blobs")
         (cond
           ((= *machine* 2609)
            (format nil "bin/skyline-tool --port ${PORT} compile-blob-intv $< $@"))
           ((member *machine* '(3010 9918 1000 837 2110))
            (format nil "bin/skyline-tool --port ${PORT} blob-rip-tms9918 $<"))
           (t
            (format nil "bin/skyline-tool --port ${PORT} dispatch-png $< Object/~a/Assets" machine-dir))))
        (t (error "Asset kind ~a not known" kind))))))

(defun write-asset-compilation/music (asset-indicator)
  (let* ((machine-dir (machine-directory-name))
         (basename (last-segment asset-indicator #\/)))
    (unless (= *machine* 2609)
      (let ((source-pathname (make-pathname
                              :directory (list :relative "Source" "Generated" machine-dir "Assets")
                              :name (format nil "Song.~a" basename)
                              :type "s")))
        (ensure-directories-exist source-pathname)
        (with-output-to-file (source source-pathname :if-exists :supersede)
          (format source ";; This is a generated file~2%")
          (dolist (video (supported-video-types))
            (format source "~%~10t.if TV == ~a
~10t  .binary \"Song.~a.~a.o\"
~10t.fi~%"
                    video basename video)))))
    (dolist (video (supported-video-types))
      (format t "
~a: ~a \\
          Source/Assets.index bin/skyline-tool Source/Generated/~a/Orchestration.s Source/Tables/Orchestration.ods
	mkdir -p ~a
	~a"
	    (asset->object-name asset-indicator :video video)
	    (asset->source-name asset-indicator)
	    machine-dir
	    (if (= *machine* 2609)
                  (format nil "Source/Generated/~a/Assets" machine-dir)
                  (format nil "Object/~a/Assets" machine-dir))
	    (asset-compilation-line asset-indicator :video video)))))

(defun write-asset-compilation/map (asset-indicator)
  (let* ((machine-dir (machine-directory-name))
         (source (asset->source-name asset-indicator))
         (source-pathname (merge-pathnames source (uiop:getcwd)))
         (deps ()))
    (when (probe-file source-pathname)
      (handler-case
          (let* ((xml (xmls:parse-to-list
                       (alexandria:read-file-into-string source-pathname)))
                 (source-dir (make-pathname
                              :directory (pathname-directory source-pathname)
                              :name nil :type nil)))
            (dolist (ts (xml-matches "tileset" xml))
              (let ((ts-source (xml-attr "source" (second ts))))
                (when ts-source
                  (let* ((ts-path (merge-pathnames
                                   (parse-namestring ts-source) source-dir))
                         (ts-name (pathname-name ts-path)))
                    (push (format nil "Object/~a/Assets/Tileset.~a.o"
                                  machine-dir ts-name)
                          deps)))))
            (let ((map-props (xml-match "properties" xml nil)))
              (when map-props
                (dolist (prop (xml-matches "property" map-props))
                  (let* ((attrs (second prop))
                         (name (xml-attr "name" attrs))
                         (value (xml-attr "value" attrs)))
                    (when (and name value (string-equal name "rc"))
                      (push (format nil "Source/Generated/~a/RunCommands/~a.s"
                                    (machine-directory-name)
                                    (pascal-case (remove #\' value)))
                            deps))))))
            (dolist (og (xml-matches "objectgroup" xml))
              (dolist (obj (xml-matches "object" og))
                (let ((obj-props (xml-match "properties" obj nil)))
                  (when obj-props
                    (dolist (prop (xml-matches "property" obj-props))
                      (let* ((attrs (second prop))
                             (name (xml-attr "name" attrs))
                             (value (xml-attr "value" attrs)))
                         (when (and name value (string-equal name "Script"))
                           (let* ((stripped (remove #\' value))
                                  (path (mapcar #'pascal-case
                                                (flatten
                                                 (mapcar (lambda (s)
                                                           (split-sequence #\/ s))
                                                         (split-sequence #\- stripped)))))
                                  (indicator (format nil "Scripts/~{~a~^/~}" path)))
                             (push (asset->object-name indicator)
                                   deps))))))))))
        (error (c)
          (warn "write-asset-compilation/map: failed to parse ~a for dependencies: ~a"
                source c))))
    (format t "~%~{~a~^ ~}: ~a \\~%          Source/Assets.index bin/skyline-tool"
            (mapcar (lambda (v) (asset->object-name asset-indicator :video v))
                    (supported-video-types))
            source)
    (dolist (dep (reverse deps))
      (format t " \\~%          ~a" dep))
    (format t "~%	mkdir -p Object/~a/Assets~%	bin/skyline-tool --port ${PORT} compile-map $<"
            machine-dir)))

(defun makefile-blob-videos ()
  "Video keywords to emit for blob compile rules.

Blobs whose paths omit a video suffix (TMS9918-family Z80 ports) must not
emit duplicate GNU Make targets for :ntsc / :pal / :secam."
  (if (member *machine* '(3010 9918 1000 837 2110))
      '(:ntsc)
      (supported-video-types)))

(defun write-asset-compilation/blob (asset-indicator)
  (let ((machine-dir (machine-directory-name)))
    (if (= *machine* 2609)
        (format t "~%
~a: ~a \\
          Source/Assets.index bin/skyline-tool
	mkdir -p Source/Generated/~a/Assets
	~a"
                (asset->object-name asset-indicator)
                (asset->source-name asset-indicator)
                machine-dir
                (asset-compilation-line asset-indicator :video :ntsc))
        (dolist (video (makefile-blob-videos))
          (format t "~%
~a: ~a \\
          Source/Assets.index bin/skyline-tool
	mkdir -p ~a
	~a"
                  (asset->object-name asset-indicator :video video)
                  (asset->source-name asset-indicator)
                  (if (member *machine* '(3010 9918 1000 837 2110))
                      (format nil "Source/Generated/~a/Assets" machine-dir)
                      (format nil "Object/~a/Assets" machine-dir))
                  (asset-compilation-line asset-indicator :video video))))))

(defun write-asset-compilation (asset-indicator)
  (let ((machine-dir (machine-directory-name)))
    (cond ((song-asset-p asset-indicator)
           (write-asset-compilation/music asset-indicator))
          ((map-asset-p asset-indicator)
           (write-asset-compilation/map asset-indicator))
          ((blob-asset-p asset-indicator)
           (case *machine*
	   (200 ; Lynx platform
	    (format *trace-output* "~&(Write-Asset-Compilation processing LYNX BLOB ~a)" asset-indicator)
	    (write-asset-compilation/blob-lynx asset-indicator))
	   (2609 ; Intellivision — tile map + GRAM (see @code{compile-blob-intv})
	    (format *trace-output* "~&(Write-Asset-Compilation processing INTV BLOB ~a)" asset-indicator)
	    (write-asset-compilation/blob asset-indicator))
	   (5200 ; Atari 5200 — Mode E bitmap blobs (@code{dispatch-png})
	    (format *trace-output* "~&(Write-Asset-Compilation processing 5200 BLOB ~a)" asset-indicator)
	    (write-asset-compilation/blob asset-indicator))
	   ((3010 9918 1000 837 2110) ; SMS, ColecoVision, SG-1000, Game Gear, VS — TMS9918 family
	    (format *trace-output* "~&(Write-Asset-Compilation processing TMS9918-family BLOB ~a)" asset-indicator)
	    (write-asset-compilation/blob asset-indicator))
	   (otherwise ; Machines without per-port blob makefile integration yet
	    (format *trace-output* "~&(Write-Asset-Compilation is ignoring BLOB ~a for machine ~A)"
                      asset-indicator *machine*))))
          ((script-asset-p asset-indicator)
           (format t "~%
~a: ~a~@[ \\~%          ~a~] \\
          Source/Generated/~a/Labels.Public.NTSC.forth Source/Generated/~a/Classes.forth \\
          Source/Assets.index bin/skyline-tool
	# FIXME: #1237 NTSC is not actually right for everyone
	mkdir -p Object/~a/Assets
	~a"
                   (asset->object-name asset-indicator)
                   (asset->source-name asset-indicator)
                   ;; XXX this needs fixing for IntelliVoice:
                   (when (speech-supported-p) "Source/Tables/SpeakJet.dic")
                   machine-dir machine-dir machine-dir
                   (asset-compilation-line asset-indicator)))
          (t
           (cerror "Continue with generic code" "Unexpected asset kind in indicator: ~a" asset-indicator)
           (format t "~%
~a: ~a~@[\\~%          ~a~]\\
          Source/Assets.index bin/skyline-tool
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
  (member *machine* '(2600 7800 2609))) ; VCS (AtariVox), 7800 (AtariVox), Intellivision (IntelliVoice)

(defun asset-loaders (asset-objects)
  "Enumerates the asset loaders that might be needed for the ASSET-OBJECTS given.

Currently just enumerates all four asset loaders."
  (declare (ignore asset-objects))
  (let ((dir (machine-directory-name)))
    (list (make-pathname :directory `(:relative "Source" "Code" ,dir "Routines")
                         :name "LoadMap" :type "s")
          (make-pathname :directory `(:relative "Source" "Code" ,dir "Routines")
                         :name "LoadBlob" :type "s")
          (make-pathname :directory `(:relative "Source" "Code" ,dir "Routines")
                         :name "LoadSong" :type "s")
          (make-pathname :directory `(:relative "Source" "Code" ,dir "Routines")
                         :name "LoadScript" :type "s"))))

(defun %asset-ids-find-song-hash (asset-ids song-name)
  "Return hash key for SONG-NAME in @code{:song} table of ASSET-IDs, or NIL."
  (let ((songs (gethash :song asset-ids)))
    (when songs
      (loop for asset-hash being the hash-keys in songs using (hash-value asset-name)
	  when (string-equal asset-name song-name)
	    do (return asset-hash)))))

(defun write-asset-ids (&optional
                          (outfile-pathname (make-pathname :directory (list :relative "Source" "Generated"
                                                                            (machine-directory-name))
                                                           :name "AssetIDs"
                                                           :type "s"))
                          (infile-pathname #p"Source/Assets.index"))
  "Write AssetIDs.s, AssetIDs.forth, and Asset-IDs.cpy from INFILE-PATHNAME (optional paths).

@table @asis
@item OUTFILE-PATHNAME
Base output path (typeless); @file{.s}, @file{.forth}, and @file{.cpy} siblings are written.
@item INFILE-PATHNAME
@file{Assets.index} to read (default @file{Source/Assets.index}).
@end table

@subsection Outputs
Three generated files next to @code{OUTFILE-PATHNAME}: assembly equates, Forth constants,
and an EIGHTBOL @code{COPY} book of @code{77} level IDs.

@subsection Side effects
Reads the assets index (cached) and overwrites the three output files.
"
  (ensure-directories-exist outfile-pathname)
  (multiple-value-bind (asset-builds asset-ids) (read-assets-list infile-pathname)
    (declare (ignore asset-builds))
    (let ((asset-count (loop for kind being the hash-keys in asset-ids
		         sum (hash-table-count (gethash kind asset-ids)))))
      (format *trace-output* "~&Writing AssetIDs.s for ~:d asset~:p" asset-count)
      (with-output-to-file (outfile outfile-pathname :if-exists :supersede)
        (format outfile ";;; Asset IDs are auto-generated")
        (loop for kind being the hash-keys in asset-ids using (hash-value ids-by-kind)
              do (terpri outfile)
              do (loop for asset-hash being the hash-keys in ids-by-kind using (hash-value asset-name)
                       do (let ((sym (asset->symbol-name
                                      (format nil "~:(~a~)s/~a" kind asset-name))))
                            (if (= *machine* 2609)
                                (format outfile "~%~a_ID           EQU     $~2,'0x"
                                        sym asset-hash)
                                (format outfile "~%~10t~a_ID = $~2,'0x"
                                        sym asset-hash))))))))

  (with-output-to-file (outfile (make-pathname :defaults outfile-pathname :type "forth")
                                :if-exists :supersede)
    (format outfile " ( -*- forth -*- Asset IDs are auto-generated )")
    (multiple-value-bind (asset-builds asset-ids) (read-assets-list infile-pathname)
      (declare (ignore asset-builds))
      (format *trace-output* "~&Writing AssetIDs.forth for ~:d asset~:p"
              (loop for kind being the hash-keys in asset-ids
		sum (hash-table-count (gethash kind asset-ids))))
      (loop for kind being the hash-keys in asset-ids using (hash-value ids-by-kind)
	  do (terpri outfile)
	  do (loop for asset-hash being the hash-keys in ids-by-kind using (hash-value asset-name)
		 do (format outfile "~%: ~:(~a~)_~{~a~^_~}_ID  ~d ( ~:*$~2,'0x ) ;"
                                kind (split-sequence #\/ asset-name) asset-hash)))))
  (let ((copybook (make-pathname
                   :directory (list :relative "Source" "Generated" (machine-directory-name) "Classes")
                   :name "Asset-IDs" :type "cpy")))
    (ensure-directories-exist copybook)
    (with-output-to-file (outfile copybook :if-exists :supersede)
      (format outfile "~
000000* Asset IDs are auto-generated
000001* This must be COPY:ed into the WORKING-STORAGE SECTION
000002
000010  DATA DIVISION.
000020   WORKING-STORAGE SECTION.
000100      01 ASSET-IDS EXTERNAL.")
      (multiple-value-bind (asset-builds asset-ids) (read-assets-list infile-pathname)
        (declare (ignore asset-builds))
        (format *trace-output* "~&Writing Asset-IDs.cpy for ~:d asset~:p"
                (loop for kind being the hash-keys in asset-ids
		  sum (hash-table-count (gethash kind asset-ids))))
        (loop for kind being the hash-keys in asset-ids using (hash-value ids-by-kind)
	    do (terpri outfile)
	    do (loop for asset-hash being the hash-keys in ids-by-kind using (hash-value asset-name)
		   do (format
                           outfile
                           (if (eql kind :script)
		           "~%~10t78 ~:(~a~)--~{~a~^--~}--ID PIC 9999
~6T-~20TUSAGE IS BINARY VALUE IS x'~4,'0x'."
		           "~%~10t78 ~:(~a~)--~{~a~^--~}--ID PIC 99
~6T-~20TUSAGE IS BINARY VALUE IS x'~2,'0x'.")
                           kind (mapcar #'header-case
                                        (split-sequence #\/ asset-name))
                           asset-hash)))))))

(defun write-asset-bank-makefile (bank &key build video)
  "Writes the Makefile for an asset ROM bank"
  (unless *makefile-bank-rules-emitted*
    (setf *makefile-bank-rules-emitted* (make-hash-table :test 'equal)))
  (let ((dedup-key (list bank build (%makefile-video-key video) :asset)))
    (when (gethash dedup-key *makefile-bank-rules-emitted*)
      (return-from write-asset-bank-makefile))
    (setf (gethash dedup-key *makefile-bank-rules-emitted*) t))
  (let* ((all-assets (all-assets-for-build build))
         (asset-objects (apply #'nconc (mapcar (rcurry #'asset->deps-list build) all-assets)))
         (bank-hex (string-upcase (format nil "~2,'0x" bank)))
         (object-deps (append asset-objects (asset-loaders asset-objects)))
         (include-paths (mapcar (lambda (path) (format nil "~{~a~^/~}" (rest path)))
			  (include-paths-for-current-bank)))
         (asm-flags (cond ((equal build "AA") "-DATARIAGE=true -DPUBLISHER=true")
                          ((equal build "Demo") "-DDEMO=true")
                          (t ""))))
    (note-asset-bank-list-target bank)
    ;; Keep argument order explicit in this format string.
    ;; Relative argument jumps (~n:*) have proven brittle and can emit
    ;; malformed prerequisite tokens in generated Makefiles.
    (format t "
Source/Generated/${PORT}/Bank~a.~a.~a.s: Source/Assets.index Source/Generated/${PORT}/Bank~a.~a.~a.list \\
          bin/skyline-tool \\~{~%          ~a~^ \\~}
	bin/skyline-tool --port ${PORT} write-asset-bank ~x ~a ~a
~%"
	  bank-hex build video
	  bank-hex build video
	  asset-objects
	  bank build video)
    (format t "
Object/${PORT}/Bank~a.~a.~a.o \\
  ~3:*Object/${PORT}/Bank~a.~a.~a.o.list.txt \\
  ~3:*Object/${PORT}/Bank~a.~a.~a.o.LABELS.txt: \\
~0@*                    Source/Generated/${PORT}/Bank~a.~a.~a.s \\
                    Source/Assets.index bin/skyline-tool \\~{~%                    ~a~^ \\~}
	mkdir -p Object/${PORT}
	~a -DTV=~a ~a \\~{~%		-I ~a \\~}
		~0@*-l Object/${PORT}/Bank~a.~a.~a.o.LABELS.txt \\
                    ~0@*-L Object/${PORT}/Bank~a.~a.~a.o.list.txt \\
		~0@*$< -o Object/${PORT}/Bank~a.~a.~a.o
	bin/skyline-tool --port ${PORT} prepend-fundamental-mode ~0@*Object/${PORT}/Bank~a.~a.~a.o.list.txt"
	  bank-hex build video
	  object-deps
	  (assembler-invocation-macro)
	  video asm-flags
	  include-paths)))

(defun write-bank-makefile (bank-source &key build video)
  "Writes the Makefile entry for a ROM bank.

Prerequisites include an order-only dependency on @code{$(EIGHTBOL_CLASS_OUTPUTS)} so
that every @file{Source/Generated/Classes/$(CPUDIR)/*Class.s} exists before 64tass
assembles any bank that @code{.include}s @file{Classes.s}.  That allows hand-written
@file{Source/Code/.../Classes/*Class.s} files to be deleted once each class is ported
to EIGHTBOL (@file{Source/Classes/*.cob})."
  (let ((dedup-key (list *bank* build (%makefile-video-key video) :hand-written)))
    (when (gethash dedup-key *makefile-bank-rules-emitted*)
      (return-from write-bank-makefile))
    (setf (gethash dedup-key *makefile-bank-rules-emitted*) t))
  (when (= *bank* *last-bank*)
    (format t "~%
~*Source/Generated/${PORT}/LastBankDefs.~a.~a.s: ~0@*Object/${PORT}/Bank~2,'0x.~a.~a.o \\
          ~0@*Object/${PORT}/Bank~2,'0x.~a.~a.o.LABELS.txt
	bin/skyline-tool --port ${PORT} labels-to-include ~0@*Object/${PORT}/Bank~2,'0x.~a.~a.o.LABELS.txt \\
		c000 ffff ~1@*LastBankDefs.~a.~a"
	  *bank* build video))
  (let ((bank-hex (format nil "~2,'0x" *bank*)))
    (format t "~%
Object/${PORT}/Bank~a.~a.~a.o ~
~3:*Object/${PORT}/Bank~a.~a.~a.o.list.txt ~
~3:*Object/${PORT}/Bank~a.~a.~a.o.LABELS.txt: ~
~{ \\~%                    ~a~}~@[ \\~%                    ~a~] \\
                    bin/skyline-tool
	mkdir -p Object/${PORT}
	-rm -f $@
	~a -DTV=~a \\
		~a-DFIRSTASSETSBANK=~d \\
		~a \\~{~%		-I ~a \\~}
		~0@*-l Object/${PORT}/Bank~a.~a.~a.o.LABELS.txt \\
                    ~0@*-L Object/${PORT}/Bank~a.~a.~a.o.list.txt $< \\
		~0@*-o Object/${PORT}/Bank~a.~a.~a.o 2>&1 | \\
		tee ~0@*Object/${PORT}/Bank~a.~a.~a.out
	echo \"@	$$(grep 'warning: Bank .~0@*~a ends at ' ~
 ~0@*Object/${PORT}/Bank~a.~a.~a.out | ~
 cut -d',' -f2)\" > ~
 ~0@*Source/Generated/Bank~a.~a.~a.size
	bin/skyline-tool --port ${PORT} prepend-fundamental-mode ~0@*Object/${PORT}/Bank~a.~a.~a.o.list.txt
	[ -f $@ ]
"
	  bank-hex build video
	  (recursive-read-deps bank-source
		:testp (string-equal build "Test"))
	  (if (= *bank* *last-bank*)
	      "Source/Generated/${PORT}/Orchestration.s"
	      (format nil "Source/Generated/${PORT}/LastBankDefs.~a.~a.s" build video))
	  (assembler-invocation-macro)
	  video
	  (if (= *bank* *last-bank*) "-DLASTBANK=true " "")
	  (first-assets-bank build)
	  (cond ((equal build "AA") "-DATARIAGE=true -DPUBLISHER=true")
                  ((equal build "Demo") "-DDEMO=true")
                  (t ""))
	  (mapcar (lambda (path) (format nil "~{~a~^/~}" (rest path)))
		(include-paths-for-current-bank
		  :testp (string-equal build "Test"))))))

(defun write-ram-bank-makefile (&key build video)
  "Writes the Makefile entry for the RAM bank used by 7800GD"
  (let ((dedup-key (list #x3e build (%makefile-video-key video) :ram-bank)))
    (when (gethash dedup-key *makefile-bank-rules-emitted*)
      (return-from write-ram-bank-makefile))
    (setf (gethash dedup-key *makefile-bank-rules-emitted*) t))
  (let ((ram-bank-hex (format nil "~2,'0x" #x3e)))
    (format t "~%
Object/${PORT}/Bank~a.~a.~a.o.LABELS.txt:~0@*
	mkdir -p Object/${PORT}
	echo \";;; nop\" > $@

Object/${PORT}/Bank~a.~a.~a.o:
	mkdir -p Object/${PORT}
	dd if=/dev/zero bs=1024 count=16 of=$@
"
	  ram-bank-hex build video ram-bank-hex build video)))

(defun write-makefile-test-target ()
  "Writes the test ROM target for the Makefile"
  ;; Raw @file{.bin} is bank objects concatenated only (unsigned).  @file{.a78}
  ;; adds the header script then @command{7800sign -w} (mandatory Atari signature
  ;; on the header+ROM image; never sign @file{.bin} or the ROM tail is wrong).
  (format t "~%
Dist/$(PORT)/~a.Test.a78: Dist/$(PORT)/~:*~a.Test.bin
	cp $^ $@
	bin/7800header -f Source/Generated/$(PORT)/header.Test.script $@
	bin/7800sign -w $@

Dist/$(PORT)/~:*~a.Test.bin: \\~
~{~%          Object/${PORT}/Bank~a.Test.o~^ \\~}
	mkdir -p Dist/${PORT}
	cat $^ > $@

~0@*Dist/$(PORT)/~a.Test.a78: .EXTRA_PREREQS = bin/7800header bin/7800sign

"
          (%makefile-game-title)
          (loop for bank below (number-of-banks :public :ntsc)
                collect (format nil "~2,'0x" bank))
          (%makefile-game-title)))

(defun write-makefile-top-line (&key video build)
  "Writes the top lines for the Makefile"
  (ecase *machine*
    (7800 (format t "~%
Dist/$(PORT)/~a.~a.~a.a78: ~0@* Dist/$(PORT)/~a.~a.~a.bin
	cp $^ $@
	bin/7800header -f Source/Generated/$(PORT)/header.~1@*~a.~a.script $@
	bin/7800sign -w $@

~0@*
Dist/$(PORT)/~a.~a.~a.bin: \\~
~{~%          Object/${PORT}/Bank~a.~a.~a.o~^ \\~}
	mkdir -p Dist/${PORT}
	cat $^ > $@

~0@*Dist/$(PORT)/~a.~a.~a.a78: .EXTRA_PREREQS = bin/7800header bin/7800sign

"
                  (%makefile-game-title)
                  build video
                  (loop for bank below (number-of-banks build video)
                        appending (list (format nil "~2,'0x" bank) build video))
                  build video
                  (%makefile-game-title)))
    (64 (format t "~%
Dist/$(PORT)/Phantasia.CBM.zip: ~0@* Object/Phantasia.CBM.zip
	cp $^ $@

Object/Phantasia.CBM.zip: \\~
~{~%          Object/Phantasia.CBM/~a ~^ \\~}
	mkdir -p Dist/${PORT}
	zip $@ $^

"
                (all-encoded-asset-names)
                (%makefile-game-title)))
    (128 (format t "~%
Dist/$(PORT)/Phantasia.CBM.zip: ~0@* Object/Phantasia.CBM.zip
	cp $^ $@

Object/Phantasia.CBM.zip: \\~
~{~%          Object/Phantasia.CBM/~a ~^ \\~}
	mkdir -p Dist/${PORT}
	zip $@ $^

"
                 (all-encoded-asset-names)
                 (%makefile-game-title)))
    ((5200 400 800) (format t "~%
Dist/$(PORT)/~a.~a.~a.bin: \\~
~{~%          Object/${PORT}/Bank~a.~a.~a.o~^ \\~}
	mkdir -p Dist/${PORT}
	rm -f $@.wip
	for bank_o in $^; do tail -c 32768 \"$$bank_o\" >> $@.wip; done
	dd if=$@.wip of=$@ bs=1048576 conv=sync
	rm -f $@.wip
"
		        (%makefile-game-title)
		        build video
		        (loop for bank below (number-of-banks build video)
			    appending (list (format nil "~2,'0x" bank) build video))))
    (2609 (format t "~%
# Intellivision (CP1610): cartridge image @file{Dist/$(PORT)/Intv/$(GAME).Public.int} is
# produced by @code{make -f Source/Build/Intv.mak game}, not by catting bank .o
# files. Rules below still emit @file{Object/Intv/…} asset prerequisites.
"))
    ;; Atari Lynx (Phantasia issue #1321 / Phase 1 #1322 / SkylineTool #1323).
    ;; The runnable @file{.lnx} image is built by @file{Source/Build/Lynx.mak}
    ;; (LNX header + concatenated bank objects), not by emitting an a78-style
    ;; rule here.  We only need a banner comment so the generated Makefile
    ;; remains self-documenting.
    (200 (format t "~%~
# --- Atari Lynx (200): runnable @file{.lnx} images are assembled via @file{Source/Build/Lynx.mak}.~%~
# Generated rules below only list per-bank asset prerequisites for this port.~%"))))

(defvar *assets-for-builds* (make-hash-table :test 'equalp)
  "A cache of assets and in which builds they are used.")

(defun all-assets-for-build (build)
  "Collect all assets for the build BUILD from Source/Assets.index

Uses *ASSETS-FOR-BUILDS* as a cache"
  (or (gethash build *assets-for-builds*)
      (let ((assets (concatenate 'list
                                 (filter-assets-for-build (read-assets-list #p"Source/Assets.index")
			                            build)
                                 (all-portable-assets))))
        (format *trace-output* "~&Assets for build ~s: …~:d asset~:p selected" build
                (length assets))
        (setf (gethash build *assets-for-builds*) assets)
        assets)))

(defun write-assets-makefile (&key build video)
  "Write the makefile for assets for BUILD and VIDEO"
  (assert build) (assert video)
  (format t "
Source/Generated/${PORT}/Bank~a.~a.~a.s: \\~{~%          ~a~^ \\~}
	bin/skyline-tool --port ${PORT} allocate-assets ~a"
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

(defparameter *7800-a78-header-shared-script-lines*
  "set supergame
set hsc
set savekey
set pokey@450
set ram@4000
unset composite
set 7800joy1
unset 7800joy2
fix
save
exit
"
  "Lines after @code{name} and @code{set tv…} for every 7800 @file{.a78} script.

AA, Public, Demo, and Test builds all use this same block so cart hardware,
controllers, and peripherals stay identical; only the embedded title line and TV
format differ per script.")

(defun write-header-script (&key build video)
  "Write the @command{7800header} parameter script for BUILD and VIDEO.

Only the Atari 7800 port uses A78 headers; other machines skip this step.

Embedded name is @code{<game> <build>.<NTSC|PAL>}; TV is @code{tvntsc} or
@code{tvpal}.  All other options match @code{*7800-a78-header-shared-script-lines*}."
  (unless (eql *machine* 7800)
    (return-from write-header-script nil))
  (assert (keywordp video) (video)
          "7800 header script requires VIDEO (:ntsc or :pal), not ~s" video)
  (let ((script-pathname (make-pathname
                          :directory `(:relative "Source" "Generated" ,(machine-directory-name))
                          :name (format nil "header.~a.~a"
                                        build video)
                          :type "script")))
    (ensure-directories-exist script-pathname)
    (with-output-to-file (script script-pathname
                                 :if-exists :supersede)
      (format script "name ~a ~a.~a~%set tv~(~a~)~%~a"
	    (%makefile-game-title)
	    build
	    (string-upcase (symbol-name video))
	    video
	    *7800-a78-header-shared-script-lines*))))

(defun write-test-header-script ()
  "Write the header file for the test ROM (7800 only; same flags as other builds)."
  (unless (eql *machine* 7800)
    (return-from write-test-header-script nil))
  (let ((script-pathname (make-pathname
                          :directory `(:relative "Source" "Generated" ,(machine-directory-name))
                          :name "header.Test"
                          :type "script")))
    (ensure-directories-exist script-pathname)
    (with-output-to-file (script script-pathname :if-exists :supersede)
      (format script "name ~a Test~%set tvntsc~%~a"
	    (%makefile-game-title)
	    *7800-a78-header-shared-script-lines*))))

(defun write-makefile-test-banks ()
  "Write Makefile rules for test ROM banks

The last-bank prerequisite line uses @code{~:[~;…~]} (CLHS 22.3.7.2) so the
branch flag consumes one @code{format} argument.  @code{~@[…~]} would not
consume a true argument, mis-binding later @code{~a} directives (@code{~{ …
~}} would then see a fixnum instead of a list)."
  (let ((*last-bank* (1- (number-of-banks :public :ntsc))))
    (dotimes (*bank* (1+ *last-bank*))
      (let* ((bank (if (= *bank* *last-bank*)
		   "LastBank"
		   (format nil "Bank~2,'0x" *bank*)))
	   (bank-source (make-pathname
                           :directory (list :relative "Source" "Code" (machine-directory-name) "Banks" bank)
                           :name bank
                           :type "s")))
        (when (= *bank* *last-bank*)
          (format t "~%
Source/Generated/${PORT}/LastBankDefs.Test.NTSC.s: Object/${PORT}/Bank~2,'0x.Test.o Object/${PORT}/Bank~:*~2,'0x.Test.o.LABELS.txt
	bin/skyline-tool --port ${PORT} labels-to-include Object/${PORT}/Bank~:*~2,'0x.Test.o.LABELS.txt \\
		c000 ffff LastBankDefs.Test.NTSC"
                  *bank* *bank*))
        ;; LABELS are a side effect of assembling Bank*.Test.o; without an explicit
        ;; prerequisite chain, QuitOnVictory.mame can stay stale while .LABELS.txt exists.
        (unless (and (= #x3f *last-bank*) (= #x3e *bank*))
          (format t "~%
Object/${PORT}/Bank~2,'0x.Test.o.LABELS.txt: Object/${PORT}/Bank~:*~2,'0x.Test.o
	$(MAKE) -f Source/Generated/${PORT}/Makefile $<
" *bank*))
        (if (and (= #x3f *last-bank*)
                 (= #x3e *bank*))
	  (format t "~%
Object/${PORT}/Bank~2,'0x.Test.o.LABELS.txt:~:*
	mkdir -p Object/${PORT}
	echo \";;; nop\" > $@

Object/${PORT}/Bank~2,'0x.Test.o:
	mkdir -p Object/${PORT}
	dd if=/dev/zero bs=1024 count=16 of=$@
"
		*bank*)
	  ;; Prerequisites: mirrors WRITE-BANK-MAKEFILE (see same ~:[ branch).
	  ;; Non-last banks INCLUDE AssemblerSetup, which INCLUDEs LastBankDefs.
	  ;; LASTBANK omit that include; a LastBankDefs prereq on the last bank is
	  ;; a make cycle ( defs are emitted from LAST bank labels ).  ~:[ uses
	  ;; one explicit FORMAT argument per CLHS 22.3.7.2.
	  (format t "~%
Object/${PORT}/Bank~2,'0x.Test.o:~{ \\~%                    ~a~}~:[~; \\~%                    Source/Generated/${PORT}/LastBankDefs.Test.NTSC.s~] \\
                    bin/skyline-tool | $(EIGHTBOL_CLASS_OUTPUTS)
	mkdir -p Object/${PORT}
	~a ~@[~a~] -DTV=NTSC -DUNITTEST=true \\
	-DFIRSTASSETSBANK=~d ~{ \\~%		-I ~a ~} \\
		-l $@.LABELS.txt -L $@.list.txt $< -o $@
	bin/skyline-tool --port ${PORT} prepend-fundamental-mode $@.list.txt
"
		*bank*
		(if (probe-file bank-source)
                        (recursive-read-deps bank-source
                          :testp t)
                        ;; Keep generated bank asm path platform-qualified.
                        ;; Asset-bank rules are emitted under Source/Generated/${PORT}/.
                        (list (make-pathname
			 :directory (list :relative "Source" "Generated" (machine-directory-name))
			 :name (format nil "Bank~2,'0x.Public.NTSC" *bank*)
			 :type "s")))
		(/= *bank* *last-bank*)
		(assembler-invocation-macro)
		(when (= *bank* *last-bank*)
		  "-DLASTBANK=true")
		(first-assets-bank "Test")
		(mapcar (lambda (path) (format nil "~{~a~^/~}" (rest path)))
		        (include-paths-for-current-bank
		          :testp t))))))))

(defun write-makefile-for-blobs ()
  "Emit Makefile rules for @file{Source/Blobs/<platform>/*.xcf}.

Intellivision uses @code{compile-blob-intv} (tile map + GRAM cards)."
  (dolist (blob (remove-duplicates
                 (directory (make-pathname :directory (list :relative "Source" "Blobs" (machine-directory-name))
                                           :name :wild
                                           :type "xcf"))))
    (write-blob-generation blob)))

(defun write-makefile-for-art ()
  (dolist (art (directory (make-pathname :directory (list :relative "Source" "Art" (machine-directory-name))
                                         :name :wild
                                         :type "art")))
    (write-art-generation art)))

(defun write-makefile-for-tilesets ()
  (dolist (tileset (recursive-directory (make-pathname :directory (list :relative "Source" "Maps")
					     :name :wild
					     :type "tsx")))
    (write-tsx-generation tileset)))

(defun write-makefile-for-bare-assets ()
  (maphash (lambda (asset builds)
             (declare (ignore builds))
             (when asset
               (write-asset-compilation asset)))
           (read-assets-list)))

(defun write-makefile-header ()
  (format t "# Makefile (generated)~%# -*- makefile -*-~%"))

(define-constant +eightbol-cpus+
    '("6502" "65c02" "65c816" "cp1610" "HuC6280" "RP2A03" "Z80" "SM83" "m68k" "i286" "ARM7" "F8")
  :test #'equalp
  :documentation "CPU names for EIGHTBOL .cob -> .s pattern rules (must match eightbol +cpu-display-names+).")

(defun eightbol-sources ()
  "Return sorted unique class stems under @code{Source/Classes/} from @code{*.cob} and @code{*.bas}."
  (let ((stems ()))
    (dolist (p (directory #p"Source/Classes/*.cob"))
      (pushnew (pathname-name p) stems :test #'equalp))
    (dolist (p (directory #p"Source/Classes/*.bas"))
      (pushnew (pathname-name p) stems :test #'equalp))
    (sort stems #'string<)))

(defun write-makefile-for-eightbol-classes ()
  "Emit           pattern           rules           for           EIGHTBOL:
@code{Source/Generated/Classes/$(EIGHTBOL_CPUDIR)/%Class.s}         from
@code{%.bas}  when   present,  else   @code{%.cob}.  When   both  exist,
@code{.bas} wins."
  (format t "~%
EIGHTBOL_CPUDIR ?= $(CPUDIR)
")
  (dolist (class-id (eightbol-sources))
    (let* ((bas (make-pathname :directory (pathname-directory #p"Source/Classes/")
                               :name class-id :type "bas"))
           (cob (make-pathname :directory (pathname-directory #p"Source/Classes/")
                               :name class-id :type "cob"))
           (pascal (pascal-case class-id)))
      (cond
        ((probe-file bas)
         (format t "
Source/Generated/Classes/$(EIGHTBOL_CPUDIR)/~aClass.s: Source/Classes/~a.bas \\
		Source/Generated/$(PORT)/Classes/~a-Slots.cpy \\
                    Source/Generated/$(PORT)/Classes/Asset-IDs.cpy \\
		Source/Generated/$(PORT)/Classes/$(GAME)-Globals.cpy \\
		bin/eightbol
	mkdir -p Source/Generated/Classes/$(EIGHTBOL_CPUDIR)
	bin/eightbol --basic $< -m $(EIGHTBOL_CPUDIR) -o $@ \\
         -I Source/Generated/$(PORT)/Classes \\
         -I Source/Classes
"
                 pascal class-id class-id))
        ((probe-file cob)
         (format t "
Source/Generated/Classes/$(EIGHTBOL_CPUDIR)/~aClass.s: Source/Classes/~a.cob \\
		Source/Generated/$(PORT)/Classes/~a-Slots.cpy \\
                    Source/Generated/$(PORT)/Classes/Asset-IDs.cpy \\
		Source/Generated/$(PORT)/Classes/$(GAME)-Globals.cpy \\
		bin/eightbol
	mkdir -p Source/Generated/Classes/$(EIGHTBOL_CPUDIR)
	bin/eightbol $< -m $(EIGHTBOL_CPUDIR) -o $@ \\
		-I Source/Generated/$(PORT)/Classes \\
		-I Source/Classes"
                 pascal class-id class-id))
        (t nil))))
  (let ((outputs
          (loop for class-id in (eightbol-sources)
                for bas = (make-pathname :directory (pathname-directory #p"Source/Classes/")
                                         :name class-id :type "bas")
                for cob = (make-pathname :directory (pathname-directory #p"Source/Classes/")
                                         :name class-id :type "cob")
                when (or (probe-file bas) (probe-file cob))
                  collect (format nil "Source/Generated/Classes/$(EIGHTBOL_CPUDIR)/~aClass.s"
                                  (pascal-case class-id)))))
    (when outputs
      (format t "~%# Concrete class outputs for order-only bank prerequisites (see write-bank-makefile).~%")
      (format t "EIGHTBOL_CLASS_OUTPUTS :=~{ ~a~}~%" outputs))))

(defun bank-source-pathname (&optional (bank *bank*))
  (if (and *last-bank* bank (= bank *last-bank*))
      (last-bank-source-pathname)
      (make-pathname :directory (list :relative "Source" "Code"
			        (machine-directory-name) "Banks"
			        (format nil "Bank~2,'0x" bank))
		 :name (format nil "Bank~2,'0x" bank)
		 :type "s")))

(defun last-bank-source-pathname ()
  (make-pathname :directory (list :relative "Source" "Code"
			    (machine-directory-name) "Banks" "LastBank")
                 :name "LastBank" :type "s"))

(defgeneric write-master-makefile-for-machine (machine)
  (:documentation "Write machine-specific makefile content for MACHINE")
  (:method ((machine t))
    (error "Can't write the master Makefile for machine: ~s" machine)))

(defmethod write-master-makefile-for-machine ((machine (eql 7800)))
  "Write makefile content for Atari 7800"
  (dolist (build +all-builds+)
    (dolist (video (supported-video-types machine))
      (format t "
Object/$(PORT)/Bank01.~a.~a.o: Source/Generated/$(PORT)/Classes/Classes.cpy ~
~{\\~%     Source/Generated/Classes/$(CPUDIR)/~aClass.s~}
"
              build video
              (mapcar #'pascal-case (eightbol-sources)))))
  (let ((machine-dir (machine-directory-name)))
    (dolist (ext '("cob" "bas"))
      (dolist (f (directory (make-pathname :directory '(:relative "Source" "Maps" "RunCommands")
                                           :name :wild
                                           :type ext)))
        (let* ((stem (pathname-name f))
               (pascal (pascal-case stem))
               (out (format nil "Source/Generated/~a/RunCommands/~a.s" machine-dir pascal))
               (flag (if (string-equal ext "bas") "--basic " "")))
          (format t "~%~a: ~a \\~%          bin/eightbol~%	mkdir -p Source/Generated/~a/RunCommands~%	bin/eightbol ~a$< -m $(CPUDIR) -o $@"
                       out (enough-namestring f) machine-dir flag)))))
  (dolist (build +all-builds+)
    (dolist (video (supported-video-types machine))
      (let ((*last-bank* (1- (number-of-banks build video))))
        (write-makefile-top-line :build build :video video)
        (write-header-script :build build :video video)
        (reset-asset-bank-list-batch)
        (dotimes (*bank* (1+ *last-bank*))
          (cond
            ((and (= *last-bank* #x3f)
                  (= *bank* #x3e))
             (write-ram-bank-makefile :build build :video video))
            ((probe-file (bank-source-pathname))
             (write-bank-makefile (bank-source-pathname)
                                  :build build :video video))
            (t (write-asset-bank-makefile *bank*
                                          :build build :video video))))
        (emit-grouped-asset-bank-list-rules build video)))))

(defmethod write-master-makefile-for-machine ((machine (eql 200)))
  "Write makefile content for Atari Lynx"
  (dolist (build +all-builds+)
    (let ((*last-bank* (1- (number-of-banks build nil))))
      (write-makefile-top-line :build build)
      (write-header-script :build build)
      (reset-asset-bank-list-batch)
      (dotimes (*bank* (1+ *last-bank*))
        (let ((bank-source (bank-source-pathname)))
          (cond
	  ((= *bank* *last-bank*)
	   (write-bank-makefile (last-bank-source-pathname)
                                  :build build))
	  ((and (= *last-bank* #x3f)
                  (= *bank* #x3e))
	   (write-ram-bank-makefile :build build))
	  ((probe-file bank-source)
	   (write-bank-makefile bank-source
                                  :build build))
	  (t (write-asset-bank-makefile *bank*
                                          :build build))))))
    (emit-grouped-asset-bank-list-rules build nil)))

(defmethod write-master-makefile-for-machine ((machine (eql 64)))
  "Write makefile content for Commodore 64"
  (write-makefile-top-line)
  (reset-asset-bank-list-batch)
  (dotimes (*bank* (1+ *last-bank*))
    (let ((bank-source (bank-source-pathname)))
      (cond
        ((= *bank* *last-bank*)
         (write-bank-makefile (last-bank-source-pathname)))
        ((probe-file bank-source)
         (write-bank-makefile bank-source))
        (t (write-asset-bank-makefile *bank*)))))
  (emit-grouped-asset-bank-list-rules nil nil))

(defmethod write-master-makefile-for-machine ((machine (eql 3010)))
  "Append SMS note: Z80 build uses Source/Build/SMS.mak and ASZ80 (z80asm), not 64tass per-bank assembly."
  (format t "~%
# --- Sega Master System (3010): Z80 ---
# See Source/Build/SMS.mak and Source/Build/z80-common.mak.
"))

(defmethod write-master-makefile-for-machine ((machine (eql 837)))
  "Append Game Gear note: Z80 build uses Source/Build/GG.mak and ASZ80 (z80asm)."
  (format t "~%
# --- Sega Game Gear (837): Z80 ---
# See Source/Build/GG.mak and Source/Build/z80-common.mak.
"))

(defmethod write-master-makefile-for-machine ((machine (eql 2110)))
  "Append Game Gear note (alternate machine number): same as 837."
  (format t "~%
# --- Sega Game Gear (2110): Z80 ---
# See Source/Build/GG.mak and Source/Build/z80-common.mak.
"))

(defmethod write-master-makefile-for-machine ((machine (eql 9918)))
  "Append ColecoVision note (Z80 + TMS9918)."
  (format t "~%
# --- ColecoVision (9918): Z80 ---
# See Source/Build/ClcV.mak and Source/Build/z80-common.mak.
"))

(defmethod write-master-makefile-for-machine ((machine (eql 1000)))
  "Append SG-1000 note (Z80 + SN76489)."
  (format t "~%
# --- SG-1000 (1000): Z80 (TMS9918 VDP family) ---
# See Source/Build/z80-common.mak.
"))

(defun write-atari8-cartridge-bank-rules ()
  "Emit Dist and per-bank recipes for @code{*machine*} 5200, 400, or 800 (32×32 KiB / 1 MiB)."
  (let ((machine *machine*))
    (dolist (build +all-builds+)
      (dolist (video (supported-video-types machine))
        (let ((*region* video)
	    (*last-bank* (1- (number-of-banks build video))))
          (write-makefile-top-line :build build :video video)
          (write-header-script :build build :video video)
          (reset-asset-bank-list-batch)
          (dotimes (*bank* (1+ *last-bank*))
	  (let ((bank-source (bank-source-pathname)))
	    (cond
                ((= *bank* *last-bank*)
                 (write-bank-makefile (last-bank-source-pathname)
			        :build build :video video))
                ((probe-file bank-source)
                 (write-bank-makefile bank-source
			        :build build :video video))
                (t (write-asset-bank-makefile *bank*
				      :build build :video video)))))
          (emit-grouped-asset-bank-list-rules build video)
          (format t "~%")
          (format t "~%"))))))

(defmethod write-master-makefile-for-machine ((machine (eql 5200)))
  "Write makefile content for Atari 5200 (32 × 32 KiB banks, 1 MiB concatenated image)."
  (declare (ignore machine))
  (write-atari8-cartridge-bank-rules))

(defmethod write-master-makefile-for-machine ((machine (eql 400)))
  "Write makefile content for Atari 400 (same cartridge layout as 5200)."
  (declare (ignore machine))
  (write-atari8-cartridge-bank-rules))

(defmethod write-master-makefile-for-machine ((machine (eql 800)))
  "Write makefile content for Atari 800 (same cartridge layout as 5200)."
  (declare (ignore machine))
  (write-atari8-cartridge-bank-rules))

(defmethod write-master-makefile-for-machine ((machine (eql 81)))
  "Write makefile content for ZX81 platform"
  (write-makefile-top-line)
  (reset-asset-bank-list-batch)
  (dotimes (*bank* (1+ *last-bank*))
    (let ((bank-source (bank-source-pathname)))
      (cond
        ((= *bank* *last-bank*)
         (write-ram-bank-makefile :build "Public" :video :NTSC))
        ((probe-file bank-source)
         (write-bank-makefile bank-source
			:build "Public" :video "NTSC"))
        (t (write-asset-bank-makefile *bank*
			        :build "Public" :video "NTSC")))))
  (emit-grouped-asset-bank-list-rules "Public" "NTSC"))

(defmethod write-master-makefile-for-machine ((machine (eql 2068)))
  "Write makefile content for ZX Spectrum platform"
  (write-makefile-top-line)
  (reset-asset-bank-list-batch)
  (dotimes (*bank* (1+ *last-bank*))
    (let ((bank-source (bank-source-pathname)))
      (cond
        ((= *bank* *last-bank*)
         (write-ram-bank-makefile :build "Public" :video "NTSC"))
        ((probe-file bank-source)
         (write-bank-makefile bank-source
			:build "Public" :video "NTSC"))
        (t (write-asset-bank-makefile *bank*
			        :build "Public" :video "NTSC")))))
  (emit-grouped-asset-bank-list-rules "Public" "NTSC"))

(defmethod write-master-makefile-for-machine ((machine (eql 128)))
  "Write makefile content for Commodore 128"
  (write-makefile-top-line)
  (reset-asset-bank-list-batch)
  (dotimes (*bank* (1+ *last-bank*))
    (let ((bank-source (bank-source-pathname)))
      (cond
        ((= *bank* *last-bank*)
         (write-bank-makefile (last-bank-source-pathname)))
        ((probe-file bank-source)
         (write-bank-makefile bank-source))
        (t (write-asset-bank-makefile *bank*)))))
  (emit-grouped-asset-bank-list-rules nil nil))

(defmethod write-master-makefile-for-machine ((machine (eql 20953)))
  "Write makefile content for Game Boy Color"
  (dolist (build +all-builds+)
    (dolist (video (supported-video-types machine))
      (let ((*last-bank* (1- (number-of-banks build video))))
        (write-makefile-top-line :build build :video video)
        (write-header-script :build build :video video)
        (reset-asset-bank-list-batch)
        (dotimes (*bank* (1+ *last-bank*))
          (let ((bank-source (bank-source-pathname)))
	  (cond
	    ((= *bank* *last-bank*)
	     (write-bank-makefile (last-bank-source-pathname)
			      :build build :video video))
	    ((probe-file bank-source)
	     (write-bank-makefile bank-source
			      :build build :video video))
	    (t (write-asset-bank-makefile *bank*
				    :build build :video video)))))
        (emit-grouped-asset-bank-list-rules build video)))))

(defmethod write-master-makefile-for-machine ((machine (eql 2609)))
  "Write makefile tail for Intellivision (Mattel CP1610).

Asset rules (@code{write-makefile-for-art}, tilesets, etc.) appear earlier in
@code{write-master-makefile}.  Per-bank @file{Object/Intv/Bank*.o} recipes are
not emitted here: @code{write-bank-makefile} targets 64tass, while Intv uses
as1600 (@file{Source/Build/Intv.mak}).  The cartridge binary is
@file{Dist/$(PORT)/Intv/$(GAME).Public.int}."
  (format t "~%
# --- Intellivision (2609): CP1610 cartridge build ---
# Runnable ROM:  make -f Source/Build/Intv.mak game
# Emulator:      bin/jzintv -J1 Dist/$(PORT)/Intv/$(GAME).Public.int
# (Intellicart-style .int/.bin+.cfg is a different container; as1600 outputs .rom.)
"))

(defun %scrub-makefile-nul-bytes (pathname)
  "Remove embedded @code{#\\Nul} bytes from PATHNAME if present.

Generated master Makefiles must be plain UTF-8 text; a stray NUL (e.g. from a
bad @code{FORMAT} argument) makes GNU Make warn and abort parsing."
  (let ((bytes (with-open-file (in pathname :direction :input
				    :element-type '(unsigned-byte 8))
	       (let ((buf (make-array (file-length in) :element-type '(unsigned-byte 8))))
	         (read-sequence buf in)
	         buf))))
    (when (position 0 bytes)
      (let ((clean (remove 0 bytes)))
        (with-open-file (out pathname :direction :output
			        :if-exists :supersede
			        :element-type '(unsigned-byte 8))
	(write-sequence clean out))
        (format *trace-output*
	      "~&Warning: removed ~:d NUL byte~:p from ~a~%"
	      (- (length bytes) (length clean)) pathname)))))

(defun write-master-makefile (&optional (*machine* *machine*))
  "Generates the master Makefile for the current platform

in Source/Generated/{platform}/Makefile.

@table @asis
@item Input
@table @asis
@item *MACHINE*
Global variable specifying the target platform/machine
@end table

@item Output
@table @asis
@item File
Source/Generated/{platform}/Makefile with all build rules
@end table

@item Behavior
Creates a comprehensive Makefile containing rules for building all assets,
generating bank allocations, and compiling platform-specific code.
This Makefile handles everything not covered by the top-level Makefile.

Do not shell-redirect standard output of @command{bin/skyline-tool … write-master-makefile}
into the generated Makefile path: the process may also have file descriptor 1 attached
to that path, which then receives @code{*trace-output*} progress lines and corrupts the
file interleaved with @code{*standard-output*} content.  Progress is sent to
@code{*trace-output*}, rebound to @code{*error-output*} for the duration of the write so
accidental @samp{> Makefile} only duplicates Makefile bytes on fd 1 while trace stays on
stderr."
  (let* ((gen-dir (make-pathname :directory (list :relative "Source" "Generated"
                                                  (machine-directory-name))))
         (gen-mf (merge-pathnames (make-pathname :name "Makefile" :type nil) gen-dir)))
    (ensure-directories-exist gen-dir)
    (format *trace-output* "~&Writing master Makefile content to ~a …" gen-mf)
    (with-output-to-file (*standard-output* gen-mf
				    :if-exists :supersede
				    :external-format :utf-8)
      (let ((*trace-output* *error-output*))
        (setf *makefile-bank-rules-emitted* (make-hash-table :test 'equal))
        (let ((*region* nil))
          (write-makefile-header)
          (write-makefile-for-bare-assets)
          (write-makefile-for-eightbol-classes)
          (write-makefile-for-tilesets)
          (write-makefile-for-art)
          (write-makefile-for-blobs)
          (unless (member *machine* '(5200 400 800 2609 200))
	  (write-makefile-test-target)
	  (write-test-header-script)
	  (write-makefile-test-banks))
          (write-master-makefile-for-machine *machine*))))
    (%scrub-makefile-nul-bytes gen-mf)
    (format *trace-output* " … done writing master Makefile.~%")))

(defun %intv-blob-stem (sym)
  "Return PNG stem (e.g. @code{ZPH}) from catalog symbol @code{Blob_ZPH}."
  (subseq sym 5))

(defun %intv-read-blob-equ (blob-source sym suffix)
  "Read integer @code{SYM_SUFFIX} from generated @file{Blob.*.s}, else 0."
  (when (probe-file blob-source)
    (with-open-file (s blob-source :direction :input)
      (let ((needle (format nil "~a_~a EQU " sym suffix)))
        (loop for line = (read-line s nil nil)
              while line
              when (search needle line :test #'char-equal)
                do (let ((pos (+ (length needle) (search needle line :test #'char-equal))))
                     (return-from %intv-read-blob-equ
                       (parse-integer (string-trim '(#\space #\tab) (subseq line pos))
                                      :junk-allowed t)))))))
  0)

(defun %intv-read-map-equ (map-source lab suffix)
  "Read integer @code{LAB_SUFFIX} EQU from generated @file{Map.*.s}, else 0.

@table @asis
@item MAP-SOURCE
Pathname of the generated map assembly file.
@item LAB
The assembly label prefix (e.g. @samp{Map_Global_TheOpenSeas}).
@item SUFFIX
The EQU name suffix (e.g. @samp{MAP_WIDTH}).
@end table"
  (when (probe-file map-source)
    (with-open-file (s map-source :direction :input)
      (let ((needle (format nil "~a_~a EQU " lab suffix)))
        (loop for line = (read-line s nil nil)
              while line
              when (search needle line :test #'char-equal)
                do (let ((pos (+ (length needle) (search needle line :test #'char-equal))))
                     (return-from %intv-read-map-equ
                       (parse-integer (string-trim '(#\space #\tab) (subseq line pos))
                                      :junk-allowed t)))))))
  0)

(defun %intv-map-catalog-entries ()
  "Return sorted alists for compiled Intv map @file{Map.*.s} files on disk.

Each entry has @code{:id}, @code{:sym}, @code{:gram} (0), @code{:map}
(@code{LAB_MAP_HEADER} pointer), @code{:cols} (width), @code{:rows} (height),
@code{:ngram} (0).  Uses @code{read-assets-list} IDs for @code{:map} kind only."
  (read-assets-list)
  (let ((gen-dir #p"Source/Generated/Intv/Assets/")
        (id-table (gethash :map *asset-ids-seen*))
        (entries nil))
    (when (and gen-dir id-table)
      (loop for id being the hash-keys of id-table
            for name = (gethash id id-table)
            for lab = (asset->symbol-name (format nil "Maps/~a" name))
            for gen-s = (merge-pathnames
                         (make-pathname :name (format nil "Map.~a" (substitute #\. #\/ name))
                                        :type "s")
                         gen-dir)
            when (probe-file gen-s)
              do (let ((w (%intv-read-map-equ gen-s lab "MAP_WIDTH"))
                       (h (%intv-read-map-equ gen-s lab "MAP_HEIGHT")))
                   (push (list :id id :sym lab
                               :gram 0
                               :map (format nil "~a_MAP_HEADER" lab)
                               :cols w :rows h :ngram 0)
                         entries))))
    (sort entries #'< :key (lambda (e) (getf e :id)))))

(defun %intv-blob-catalog-entries ()
  "Return sorted alists for Intv BLOB PNGs on disk.

Each entry has @code{:id}, @code{:sym}, @code{:gram}, @code{:map}, @code{:cols},
@code{:rows}, @code{:ngram}.  Uses @code{read-assets-list} IDs only."
  (read-assets-list)
  (let ((blob-dir #p"Source/Blobs/Intv/")
        (gen-dir #p"Source/Generated/Intv/Assets/")
        (id-table (gethash :blob *asset-ids-seen*))
        (entries nil))
    (when (and blob-dir (uiop:directory-exists-p blob-dir) id-table)
      (loop for id being the hash-keys of id-table
            for name = (gethash id id-table)
            for png = (merge-pathnames (make-pathname :name name :type "png") blob-dir)
            when (probe-file png)
              do (let* ((sym (asset->symbol-name (format nil "Blobs/~a" name)))
                        (blob-s (merge-pathnames (make-pathname :name (format nil "Blob.~a" name)
                                                                :type "s")
                                                 gen-dir)))
                   (push (list :id id :sym sym
                               :gram (format nil "~a_GRAM_DATA" sym)
                               :map (format nil "~a_TILE_MAP" sym)
                               :cols (%intv-read-blob-equ blob-s sym "TILE_COLS")
                               :rows (%intv-read-blob-equ blob-s sym "TILE_ROWS")
                               :ngram (%intv-read-blob-equ blob-s sym "UNIQUE_GRAM_CARDS"))
                         entries))))
    (sort entries #'< :key (lambda (e) (getf e :id)))))

(defun %intv-song-catalog-entries ()
  "Return sorted alists for Intv AY songs with generated @file{Song.*.s} on disk."
  (read-assets-list)
  (let ((song-dir #p"Source/Songs/" )
        (gen-dir #p"Source/Generated/Intv/Assets/")
        (id-table (gethash :song *asset-ids-seen*))
        (entries nil))
    (when (and song-dir (uiop:directory-exists-p song-dir) id-table)
      (loop for id being the hash-keys of id-table
            for name = (gethash id id-table)
            for mscz = (merge-pathnames (make-pathname :name name :type "mscz") song-dir)
            for gen-s = (merge-pathnames (make-pathname :name (format nil "Song.~a" name)
                                                        :type "s")
                                         gen-dir)
            when (and (probe-file mscz) (%intv-song-asm-ready-p gen-s))
              do (let ((sym (asset->symbol-name (format nil "Songs/~a" name))))
                   (push (list :id id :sym sym
                               :gram 0
                               :map (format nil "~a_DATA" sym)
                               :cols 0 :rows 0 :ngram 0)
                         entries))))
    (sort entries #'< :key (lambda (e) (getf e :id)))))

(defun write-intv-asset-catalog (&optional
                                   (output-path
                                    #p"Source/Generated/Intv/AssetCatalog.s"))
  "Write @file{AssetCatalog.s} for the current @code{*machine*} (Intv / 2609).

Each row is @code{(id, gram, map, cols, rows, nunique, bank)}. Lookup is by
unique ID only. Requires @code{--port Intv} so @code{*machine*} is 2609."
  (unless (= *machine* 2609)
    (error "write-intv-asset-catalog requires Intellivision port (~a); *machine* is ~a"
           (machine-directory-name) *machine*))
  (let* ((entries (sort (append (%intv-blob-catalog-entries)
                                (%intv-song-catalog-entries)
                                (%intv-map-catalog-entries))
                        #'< :key (lambda (e) (getf e :id)))))
    (ensure-directories-exist output-path)
    (with-output-to-file (s output-path :if-exists :supersede :external-format :utf-8)
      (format s ";;; Intellivision asset catalog (generated by write-intv-asset-includes)~%")
      (format s ";;; Rows are sorted by unique asset ID; lookup by ID only.~2%")
      (format s "AssetCatalogStride    EQU     6~%")
      (format s "AssetCatalogCount     EQU     ~D~2%" (length entries))
      (format s "AssetCatalog:~%")
      (dolist (entry entries)
        (format s "    DECLE   $~4,'0X    ; ~a_ID~%" (getf entry :id) (getf entry :sym))
        (format s "    DECLE   ~a~%" (getf entry :gram))
        (format s "    DECLE   ~a~%" (getf entry :map))
        (format s "    DECLE   ~d           ; cols~%" (getf entry :cols))
        (format s "    DECLE   ~d           ; rows~%" (getf entry :rows))
        (format s "    DECLE   ~d           ; unique GRAM cards~%" (getf entry :ngram))
        (format s "    DECLE   0           ; bank (flat ROM until mapper lands)~%")))
    (format *trace-output* "~&Wrote ~a (~:d catalog row~:p)~%" output-path (length entries))
    output-path))

(defun %intv-song-asm-ready-p (gen-s)
  "True when @file{gen-s} is as1600 AY song data (not a 64tass @code{.binary} stub)."
  (when (probe-file gen-s)
    (with-open-file (s gen-s :direction :input)
      (loop for line = (read-line s nil nil)
            while line
            when (search "_DATA:" line)
              do (return t)))))

(defun write-intv-asset-includes (&optional
                                    (output-path #p"Source/Generated/Intv/AssetIncludes.s"))
  "Write @file{OUTPUT-PATH} with Intellivision generated asset includes.

Also writes @file{AssetCatalog.s} beside it. Invoke with @code{--port Intv}
so @code{*machine*} is 2609 from @file{Project.Intv.json}.

Each line includes the matching Skyline output @file{Object/<port>/Assets/Art.<name>.s}
or @file{Source/Generated/<port>/Assets/Blob.<name>.s} so @file{Phantasia.s} can
pull compiled GRAM and fullscreen BLOB data into the cartridge. With no
generated assets, emits a comment-only stub.

@table @asis
@item OUTPUT-PATH
Path relative to project root (default @file{Source/Generated/Intv/AssetIncludes.s})
@end table"
  (unless (= *machine* 2609)
    (error "write-intv-asset-includes requires Intellivision port (~a); *machine* is ~a"
           (machine-directory-name) *machine*))
  (let* ((port-dir (machine-directory-name))
         (art-dir (format nil "Source/Art/~a/" port-dir))
         (blob-dir #p"Source/Blobs/Intv/"))
    (ensure-directories-exist output-path)
    (with-output-to-file (s output-path :if-exists :supersede :external-format :utf-8)
      (format s ";;; Intellivision asset includes (generated by write-intv-asset-includes)~%")
      (let ((arts (when (uiop:directory-exists-p art-dir)
                    (directory (merge-pathnames #p"*.art" art-dir))))
            (blobs (when (uiop:directory-exists-p blob-dir)
                     (directory (merge-pathnames #p"*.png" blob-dir)))))
        (if (or arts blobs)
            (progn
              (dolist (a (sort arts #'string< :key #'namestring))
                (let ((stem (pathname-name a)))
                  (format s "~%        INCLUDE \"Object/~a/Assets/Art.~A.s\"~%"
                          port-dir stem)))
              (dolist (b (sort blobs #'string< :key #'namestring))
                (let ((stem (pathname-name b)))
                  (format s "~%        INCLUDE \"Source/Generated/~a/Assets/Blob.~A.s\"~%"
                          port-dir stem)))
              (let ((song-dir #p"Source/Songs/")
                    (id-table (progn (read-assets-list) (gethash :song *asset-ids-seen*))))
                (when (and (uiop:directory-exists-p song-dir) id-table)
                  (dolist (song (sort (loop for name being the hash-values of id-table
                                            collect name)
                                      #'string<))
                    (let ((mscz (merge-pathnames (make-pathname :name song :type "mscz") song-dir))
                          (gen-s  (make-pathname :directory (list :relative "Source"
                                                                  "Generated"
                                                                  port-dir
                                                                  "Assets")
                                                 :name (format nil "Song.~a" song)
                                                 :type "s")))
                      (when (and (probe-file mscz) (%intv-song-asm-ready-p gen-s))
                        (format s "~%        INCLUDE \"Source/Generated/~a/Assets/Song.~A.s\"~%"
                                port-dir song))))))
              ;; Include compiled Intv map assemblies
              (let* ((map-id-table (gethash :map *asset-ids-seen*))
                     (gen-dir (format nil "Source/Generated/~a/Assets/" port-dir)))
                (when map-id-table
                  (dolist (name (sort (loop for n being the hash-values of map-id-table
                                            collect n)
                                      #'string<))
                    (let* ((dot-name (substitute #\. #\/ name))
                           (gen-s (merge-pathnames (make-pathname :name (format nil "Map.~a" dot-name)
                                                                  :type "s")
                                                   gen-dir)))
                      (when (probe-file gen-s)
                        (format s "~%        INCLUDE \"Source/Generated/~a/Assets/Map.~A.s\"~%"
                                port-dir dot-name)))))))
            (format s ";;; (no Source/Art/~a/*.art or Source/Blobs/Intv/*.png yet)~%" port-dir))))
    (format *trace-output* " … Wrote ~a~%" output-path)
    (write-intv-asset-catalog)))

(defmethod get-asset-id ((kind (eql :map)) asset)
  "Find the asset ID for ASSET (a map)"
  (tagbody top
     (restart-case
         (return-from get-asset-id
           (or (gethash asset *maps-ids*)
               (progn
                 (read-map-ids-table)
                 (gethash asset *maps-ids*))))
       (reload-map-ids-table ()
         :report "Reload Source/Tables/MapsIndex.ods"
         (read-map-ids-table)
         (go top)))))

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
	(when-let (raw (third (find-if
                                 (lambda (el)
			     (and (equal (first el) "metaTag")
                                        (equalp (second el) '(("name" "workNumber")))))
                                 (cddr
			    (lastcar (xmls:parse-to-list
				    (babel:octets-to-string
				     (zip:zipfile-entry-contents entry))))))))
	  (let ((work-number$ (string raw)))
	    (when (every #'digit-char-p work-number$)
                (format *trace-output* "~&//* Song “~a” has ID $~2,'0x (from workNumber)"
                        (pathname-name pathname) (parse-integer work-number$))
                (return-from get-asset-id (parse-integer work-number$))))))))
    (let ((id (ash (logand #xff00 (sxhash asset-name)) -8)))
      (format *trace-output* "~&//* Song “~a” has ID $~2,'0x"
	    (pathname-name pathname) id)
      id)))

(defun write-asset-source (kind$ predicate assets source)
  "Write a generic asset stanza for any KIND$ ASSETS (meeting PREDICATE) into bank SOURCE"
  (let ((kind (kind-by-name kind$)))
    (when (some predicate assets)
      (when (and (equal :map kind)
                 (not (= *machine* 2609))
                 (= *machine* 7800))
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

(defun start-bank-include-name ()
  "Return @file{StartBank.s} or @file{StartAssetBank.s} for the active @code{*machine*}."
  (if (member *machine* '(5200 400 800))
      "StartAssetBank.s"
      "StartBank.s"))

(defun write-asset-bank (bank-hex build video)
  "Write out the skeletal bank file for BANK-HEX for BUILD with VIDEO formats specified.

This will  include the assets  and asset  loaders needed for  that bank,
based on the asset listing files."
  (let* ((*bank* (parse-integer bank-hex :radix 16))
         (basename (format nil "Bank~2,'0x.~a.~a" *bank* build video))
         (outfile (make-pathname :directory (list :relative "Source" "Generated" (machine-directory-name))
                                 :name basename
                                 :type "s"))
         (assets (with-input-from-file (list (allocation-list-name *bank* build video))
                   (sort (loop for asset = (read-line list nil nil)
			 while asset
			 collect asset)
                         #'string<))))
    (format *trace-output* "~& Bank ~2,'0x assets: ~s" *bank* assets)
    (ensure-directories-exist outfile)
    (with-output-to-file (source outfile :if-exists :supersede)
      (format source ";;; Bank ~2,'0x file (generated by Skyline Tool)

~10tBANK = $~2,'0x

~10t.include \"~a\"

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
	    (start-bank-include-name)
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
~10t.word ( (Binary[2] + Binary[3] * $100) + Binary ) ; compressed art/attributes
~10t.word ( (Binary[4] + Binary[5] * $100) + Binary ) ; scenery decals
~10t.word ( (Binary[6] + Binary[7] * $100) + Binary ) ; object prototypes
~10t* = Binary + 13~32t; one last pointer
~10t.word ( (Binary[13] + Binary[14] * $100) + Binary ) ; run-commands
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

(defun %parse-64tass-label-equate-address (value-string)
  "Return an unsigned integer address for VALUE-STRING from 64tass @file{-l} label listing, or NIL.

Accepts @samp{$hex}, plain decimal integers, and @samp{~$hex} (same 16-bit transform as
@code{labels-to-forth}). Rejects floats, quoted strings, and other non-address forms so they
are not folded to address @samp{0} (which previously collided with real zero-page labels)."
  (let ((v (string-trim '(#\Space #\Tab #\Newline) value-string)))
    (when (zerop (length v))
      (return-from %parse-64tass-label-equate-address nil))
    (flet ((parse-hex-from (start)
	   (ignore-errors (parse-integer v :radix 16 :start start))))
      (cond
        ((and (>= (length v) 2)
	    (char= #\~ (char v 0))
	    (char= #\$ (char v 1)))
         (let ((n (parse-hex-from 2)))
           (when n (logxor #xffff n))))
        ((char= #\$ (char v 0))
         (parse-hex-from 1))
        ((every #'digit-char-p v)
         (ignore-errors (parse-integer v)))
        (t nil)))))

(defun labels-to-include (labels-file lower upper include-file-name)
  "Extract labels between LOWER and UPPER (hex) from LABELS-FILE into @file{Source/Generated/{machine}/INCLUDE-FILE-NAME.s}.

@table @asis
@item LABELS-FILE
64tass @code{-l} listing: @samp{Label = value} per line (value may be @samp{$hex}, decimal, or @samp{~$hex}).
@item LOWER, UPPER
Inclusive hex bounds (strings, radix 16).
@item INCLUDE-FILE-NAME
Stem for output @file{.s} under @file{Source/Generated/<machine>/}.
@end table

@subsection Outputs
Writes the include file. Every label whose numeric value lies in @code{[LOWER, UPPER]} and whose
name does not end in @samp{_ID} becomes a @samp{Label = $addr} line. Multiple labels at the same
address are all emitted (previous code kept only one per address). Unparseable values (floats,
strings, @&c.) are skipped — they are not assigned address @samp{0}.

@subsection Side effects
Creates parent directories if needed; overwrites the output file."
  (let ((low (parse-integer lower :radix 16))
        (high (parse-integer upper :radix 16))
        (include-file (make-pathname :name include-file-name
			       :type "s"
			       :directory `(:relative "Source" "Generated" ,(machine-directory-name)))))
    (with-input-from-file (labs labels-file)
      (ensure-directories-exist include-file)
      (with-output-to-file (incs include-file :if-exists :supersede)
        (format *trace-output* "~&Converting ~a to include file Source/Generated/~a/~a.s… "
                labels-file (machine-directory-name) include-file-name)
        (finish-output *trace-output*)
        (format incs ";;; Generated file~2%Lib:~10t.block")
        (let ((table (make-hash-table)))
          (loop for line = (read-line labs nil nil)
                while line
                do (let* ((eq-pos (position #\= line))
                          (label (when eq-pos
                                   (string-trim '(#\Space #\Tab #\Newline)
                                                (subseq line 0 eq-pos))))
                          (value (when eq-pos
                                   (string-trim '(#\Space #\Tab #\Newline)
                                                (subseq line (1+ eq-pos)))))
                          (number (when value (%parse-64tass-label-equate-address value))))
		 (when (and label (plusp (length label)) number
			  (<= low number high)
			  (not (ends-with-subseq "_ID" label)))
		   (pushnew label (gethash number table) :test #'string=))))
          (loop for number in (sort (copy-list (hash-table-keys table)) #'<)
                do (dolist (label (sort (copy-list (gethash number table)) #'string<))
		 (format incs "~&~10t~a = $~x" label number))))
        (format incs "~2%~10t.bend~%")
        (format *trace-output* "Done.")))))

(defun labels-to-forth (labels-file include-file-name)
  "Extract labels between LOWER and UPPER (hex) from LABELS-FILE into Source/Generated/INCLUDE-FILE-NAME"
  (let ((include-file include-file-name))
    (with-input-from-file (labs labels-file)
      (ensure-directories-exist include-file)
      (with-output-to-file (incs include-file :if-exists :supersede)
        (format *trace-output* "~&Converting ~a to include file ~a… "
                labels-file include-file-name)
        (finish-output *trace-output*)
        (format incs " ( -*- forth -*- Generated file ) ~2%")
        (let ((table (make-hash-table)))
          (loop for line = (read-line labs nil nil)
                while line
                do (let ((parts (mapcar (lambda (each)
                                          (string-trim #(#\Space #\Newline) each))
				(split-sequence #\= line))))
		 (when (>= (length parts) 2)
		   (destructuring-bind (label value) parts
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
		       (setf (gethash label table) number))))))
          (loop for label in (sort (copy-list (hash-table-keys table)) #'string-lessp)
                for number = (gethash label table)
                do (format incs "~% : ~a ~d ; " label number)))
        (terpri incs)))
    (format *trace-output* " Done.")))

(defun check-for-absent-assets ()
  "Looks into Assets.index and searches Source directories for “forgotten” files."
  (read-assets-list)
  (let ((absent nil))
    (dolist (asset-file (loop for wild in '(#p"Source/Blobs/*/*.xcf"
				    #p"Source/Maps/*/*.tmx"
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
  (let* ((machine (machine-directory-name))
         (cmd (list* "64tass" "--nostart" "--long-branch"
                     "--case-sensitive" "--ascii" "-Wall"
                     "-Werror=shadow" "-Werror=wrap-pc"
                     "-Wno-leading-zeros" "--m6502" "-m" "--tab-size=1"
                     "--verbose-list" "-D" "TV=NTSC"
                     (append
                      (list "-I"
                            (enough-namestring
                             (merge-pathnames (format nil "Source/Code/~a/" machine)))
                            "-I"
                            (enough-namestring
                             (merge-pathnames (format nil "Source/Code/~a/Common/" machine)))
                            "-I"
                            (enough-namestring
                             (merge-pathnames (format nil "Source/Code/~a/Stagehand/" machine)))
                            "-I"
                            (enough-namestring
                             (merge-pathnames (make-pathname :directory (list :relative
                                                                              "Source" "Generated"
                                                                              machine))))
                            "-I"
                            (enough-namestring
                             (merge-pathnames (make-pathname :directory (list :relative
                                                                              "Source" "Generated"
                                                                              machine
                                                                              "Assets")))))
                      (list (enough-namestring source-name)
                            "-o"
                            (enough-namestring object-name))))))
    (format *trace-output* "~&~{~a~^ ~}" cmd)
    (run-program cmd
                 :error-output error-stream
                 :ignore-error-status t)))

(defun write-assembly-skeleton-for-size (tmp.s pathname)
  (format tmp.s ";;; Temporary rig to get size of “~a”" (enough-namestring pathname))
  (format tmp.s "
~10tBANK = $00
~10t.include \"~a\"
"
          (start-bank-include-name))
  (when (not (member *machine* '(5200 400 800)))
    (format tmp.s "~10t.include \"SpeakJet.s\"~%"))
  (format tmp.s "Start:
~10t.include ~s

~10t.dsection BankData
~10t.error format(\"$SIZE$%04x\", (* - Start))
"
          (file-namestring pathname))
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

(defun write-asset-compilation/blob-lynx (asset-indicator)
  "Generate makefile rules for Lynx BLOB assets (PNG graphics)"
  (let ((source-name (asset->source-name asset-indicator))
        (object-name (asset->object-name asset-indicator))
        (machine-dir (machine-directory-name)))
    (format t "~%
~a: ~a \\
          bin/skyline-tool
	mkdir -p Object/~a/Assets
	bin/skyline-tool --port Lynx dispatch-png $< Object/~a/Assets"
	  object-name source-name machine-dir machine-dir)))


(defun collect-assets (&rest args)
  "Stub function for collect-assets command"
  (format t "collect-assets called with args: ~A~%" args))

(defun all-encoded-asset-names ()
  "Return a list of all encoded asset names for the current machine."
  (let ((assets (read-assets-list)))
    (loop for asset being the hash-keys of assets
          collect (format nil "~a" asset))))

(defun prepend-fundamental-mode (&rest args)
  "Stub function for prepend-fundamental-mode command"
  (format *standard-output* "prepend-fundamental-mode called with args: ~A~%" args))
