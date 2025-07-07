(in-package :skyline-tool)

(defun read-ods-into-lists (pathname)
  (zip:with-zipfile (zip pathname :force-utf-8 t)
    (let ((xml (xmls:parse-to-list
                (babel:octets-to-string
                 (zip:zipfile-entry-contents
                  (zip:get-zipfile-entry "content.xml" zip))
                 :encoding :utf-8))))
      (assert (and (consp (first xml))
                   (equal (car (first xml)) "document-content")
                   (equal (cdr (first xml))
                          "urn:oasis:names:tc:opendocument:xmlns:office:1.0"))
              (xml)
              "ODS file seems to be malformed: document-content tag missing or invalid~%~s"
              (first xml))
      (let ((body (first (remove-if-not (lambda (el)
                                          (equal (caar el) "body"))
                                        (rest xml)))))
        (assert (and (consp (caaddr body))
                     (equal (car (caaddr body)) "spreadsheet")
                     (equal (cdr (caaddr body))
                            "urn:oasis:names:tc:opendocument:xmlns:office:1.0"))
                (xml)
                "ODS is not a spreadsheet?~%~s"
                (first body))
        (let* ((tables (mapcar #'cdr
                               (remove-if-not (lambda (el) (equal (caar el) "table"))
                                              (subseq (caddr body) 2)))))
          (mapcar #'ods-table-rows->list tables))))))

(defun extract-ss-titles (row1)
  (loop for column in row1
        when (not (emptyp column))
          collect (make-keyword (string-upcase (cl-change-case:param-case column)))))

(defun ss->lol (page)
  (let* ((row1 (first page))
         (titles (extract-ss-titles row1)))
    (loop for row in (cdr page)
          collect (loop for title in titles
                        for column in row
                        when (and title (not (emptyp column)))
                          append (list title column)))))

(defun ss->arrays (page)
  (destructuring-bind (titles-row &rest body) page
    (let ((titles (extract-ss-titles titles-row))
          (records (make-hash-table)))
      (dolist (title titles)
        (setf (gethash title records) (make-array (length body))))
      (loop for row in body
            for index from 0
            do (loop for title in titles
                     for column from 0
                     for value = (elt row column)
                     do (unless (emptyp value)
                          (setf (aref (gethash title records) index) value)))
            finally (return (loop for title in titles
                                  append (list title
                                               (coerce (gethash title records)
                                                       'list))))))))

(defun number? (value)
  (etypecase value
    (number value)
    (null nil)
    (string (handler-case (parse-number value)
              (invalid-number ())
              (sb-int:simple-parse-error ())))))

(defun compile-enemies (&optional (pathname "Source/Tables/EnemyStats.ods")
                                  (output-pathname "Source/Generated/EnemyTables.s")
                                  index-pathname)
  "Compile the stats sheets for enemies from PATHNAME into OUTPUT-PATHNAME. List to INDEX-PATHNAME."
  (unless index-pathname
    (setf index-pathname (make-pathname :defaults output-pathname
                                        :name "Enemies" :type "index")))
  (format *trace-output* "~&Reading enemies stats sheets in ~a … "
          (enough-namestring pathname))
  (finish-output *trace-output*)
  (let ((sheet (read-ods-into-lists pathname)))
    (let* ((enemy-stats (first sheet))
           (enemy-art (second sheet))
           (data (ss->lol enemy-stats))
           (art (ss->lol enemy-art)))
      (with-output-to-file (output output-pathname :if-exists :supersede)
        (with-output-to-file (index index-pathname :if-exists :supersede)
          (format *trace-output* "writing ~a and ~a …"
                  (enough-namestring output-pathname)
                  (enough-namestring index-pathname))
          (finish-output *trace-output*)
          (format output ";;; Generated from ~a~2%Enemies: .block~%"
                  (enough-namestring pathname))
          (format output "~%~10t;; Order of enemies:~{~%~10t;; $~2,'0x = ~a~}"
                  (loop for record in data
                        for i from 0
                        append (list i (getf record :enemy))))
          (format index "~{~2,'0x~a~%~}"
                  (loop for record in data
                        for i from 0
                        append (list i (getf record :enemy))))
          (format output "~%Levels:~{~%~10t.byte ~d~20t; ~a~}"
                  (loop for record in data
                        append (list (number? (getf record :level))
                                     (getf record :enemy))))
          (format output "~%HitPoints:~{~%~10t.byte ~d~20t; ~a~}"
                  (loop for record in data
                        append (list (number? (getf record :hit-points))
                                     (getf record :enemy))))
          (format output "~%Flags:~{~%~10t.byte 0 ~@[| UNDEAD~]~40t; ~a~}"
                  (loop for record in data
                        append (list (getf record :undead-p)
                                     (getf record :enemy))))
          (format output "~%TouchDamage:~{~%~10t.byte ~d~20t; ~a~}"
                  (loop for record in data
                        append (list (number? (getf record :touch-damage))
                                     (getf record :enemy))))
          (format output "~%Attack:~{~%~10t.byte ~:[0~;~:*Attack~a~]~20t; ~a~}"
                  (loop for record in data
                        append (list (getf record :attack)
                                     (getf record :enemy))))
          (format output "~%ProjectileRate:~{~%~10t.byte ~:[0~;~:*~d~]~20t; ~a~}"
                  (loop for record in data
                        append (list (getf record :projectile-rate)
                                     (getf record :enemy))))
          (format output "~%Projectile:~{~%~10t.byte ~:[0~;~:*Projectile~a~]~20t; ~a~}"
                  (loop for record in data
                        append (list (getf record :projectile)
                                     (getf record :enemy))))
          (format output "~2%~10t.bend~%")))))
  (format *trace-output* " Done.~%"))

(defun compile-item-drops (&optional (pathname "Source/Tables/ItemDrop.ods")
                                     (output-pathname
                                      "Source/Generated/ItemDropTable.s"))
  "Compile the item drop tables from PATHNAME into OUTPUT-PATHNAME"
  (format *trace-output* "~&Reading item drops sheets in ~a … "
          (enough-namestring pathname))
  (let ((sheet (read-ods-into-lists pathname)))
    (destructuring-bind (enemies-page tiles-page) sheet
      (let* ((enemies-drops (ss->lol enemies-page))
             (tiles-drops (ss->lol tiles-page)))
        (with-output-to-file (output output-pathname :if-exists :supersede)
          (format *trace-output* "writing ~a … " (enough-namestring output-pathname))
          (finish-output *trace-output*)
          (format output ";;; Generated from ~a~2%EnemiesDrops: .block~%"
                  (enough-namestring pathname))
          (format output "~%Level:
~10t.byte ~{~,3d~^, ~,3d~^, ~,3d~^, ~,3d~^~%~10t.byte ~}"
                  (mapcar (lambda (drop) (number? (getf drop :enemy-level)))
                          enemies-drops))
          (format output "~%Combo:
~10t.byte ~{~3d~^, ~3d~^, ~3d~^, ~3d~^~%~10t.byte ~}"
                  (mapcar (lambda (drop) (number? (getf drop :combo)))
                          enemies-drops))
          (format output "~%Chance:
~10t.byte ~{$~2,'0x~^, $~2,'0x~^, $~2,'0x~^, $~2,'0x~^~%~10t.byte ~}"
                  (mapcar (lambda (drop)
                            (when-let (n (number? (getf drop :chance)))
                              (floor (* #x100 n) #x100)))
                          enemies-drops))
          (format output "~%Item:~{~%~10t.byte Item~a ~}"
                  (mapcar (lambda (drop) (getf drop :item))
                          enemies-drops))
          (format output "~%Flags:~{~%~10t.byte 0~@[ | ITEM_HEALING ~]~}"
                  (mapcar (lambda (drop) (number? (getf drop :ihealing-p)))
                          enemies-drops))
          (format output "~%~10t.bend")
          (format output "~2%TilesDrops:~{~%;;; ~{ ~a: ~s~^, ~}~}" tiles-drops)))))
  (format *trace-output* "Done.~%"))

(defun compile-shops (&optional (pathname "Source/Tables/Shops.ods")
                                (output-pathname "Source/Generated/ShoppingTable.s"))
  "Compile the shopping tables from PATHNAME into OUTPUT-PATHNAME"
  (format *trace-output* "~&Reading shopping sheets in ~a … "
          (enough-namestring pathname))
  (let* ((sheet (read-ods-into-lists pathname))
         (page1 (first sheet))
         (data (ss->lol page1)))
    (with-output-to-file (output output-pathname :if-exists :supersede)
      (format *trace-output* "writing ~a … " (enough-namestring output-pathname))
      (finish-output *trace-output*)
      (format output ";;; Generated from ~a~%;;; Shopping Tables:~%"
              (enough-namestring pathname))
      (format output "~%Shopping: .block")
      (format output "~{~%~{~%;;; ~:(~a~): ~^~s~}~}" data)
      (format output "~%~10t.bend")))
  (format *trace-output* "Done.~%"))

(defun write-projection-tables.s ()
  "Writes Source/Generated/ProjectionTables.s database.
This contains pre-computed sine and cosine values of various kinds for the 3D projection subsystem."
  (format *trace-output* "~2&Writing ProjectionTables database…")
  (labels ((beautify-name (symbol)
             (let ((string (cl-change-case:lower-case (symbol-name symbol))))
               (cl-ppcre:regex-replace-all
                "phi"
                (cl-ppcre:regex-replace-all
                 "theta"
                 (cl-ppcre:regex-replace-all
                  "dx"
                  (cl-ppcre:regex-replace-all
                   "dz"
                   (remove #\- string)
                   "d₃")
                  "dₓ")
                 "θ")
                "φ"))))
    (with-output-to-file (projection-tables.csv #p"Source/Generated/ProjectionTables.csv"
                                                :if-exists :supersede)
      (with-output-to-file (projection-tables.s #p"Source/Generated/ProjectionTables.s"
                                                :if-exists :supersede)
        (format projection-tables.s ";;; ProjectionTables.s
;;; Generated by Skyline Tool, editing is futile.~2%
ProjectionTables:~20t.block")
        (let* ((mu #x08) (nu #x10)
               (phi (atan (/ 1 100.0)))
               (long-tables (list :cos-theta-dx (list)
                                  :sin-theta-dz (list)
                                  :cos-theta-sin-phi-dx (list)
                                  :sin-theta-sin-phi-dz (list)))
               (short-tables (list :cos-theta (list)
                                   :sin-theta (list)
                                   :cos-theta-sin-phi (list)
                                   :sin-theta-sin-phi (list))))
          (format projection-tables.s "~&~10tMu = $~2,'0x~%~10tNu = $~2,'0x~%~20t.bend" mu nu)
          (format projection-tables.s "
~10t.if floor(Phi * 1000) != ~d
~12t.error format(\"Phi value, expected about %f, got %f\", ~f, Phi)
~10t.fi"
                  (floor (* 1000 phi)) phi)
          (dotimes (theta-i (/ #x100 mu))
            (let* ((theta-brads (* theta-i mu))
                   (theta-rads (* (/ theta-brads #x100) 2.0d0 pi)))
              (format projection-tables.csv "~&Function,θ brads,θ rads,θ°,,,float,fixed")
              (loop for (table value)
                      on (list :cos-theta (cos theta-rads)
                               :sin-theta (sin theta-rads)
                               :cos-theta-sin-phi (* (cos theta-rads) (sin phi))
                               :sin-theta-sin-phi (* (sin theta-rads) (sin phi)))
                    by #'cddr
                    do (appendf (getf short-tables table) (list value))
                    do (format projection-tables.csv
                               "~&~:(~20@a~),$~2,'0x,~6fπ,~6f°,,,~6f,~{$~2,'0x.~2,'0x~}"
                               (beautify-name table)
                               theta-brads (rationalize (/ theta-rads pi)) (* 180 (/ theta-rads pi))
                               value (fixed-8.8 value :note (list table theta-brads))))
              (format projection-tables.csv "~&Function,θ brads,θ rads,θ°,x|z,x|z,float,fixed")
              (dotimes (xz-i (/ #x80 nu))
                (let ((xz (* xz-i nu)))
                  (loop for (table value)
                          on (list :cos-theta-dx (* (cos theta-rads) xz)
                                   :sin-theta-dz (* (sin theta-rads) xz)
                                   :cos-theta-sin-phi-dx (* (cos theta-rads) (sin phi) xz)
                                   :sin-theta-sin-phi-dz (* (sin theta-rads) (sin phi) xz))
                        by #'cddr
                        do (unless (getf (getf long-tables table) theta-i)
                             (appendf (getf long-tables table) (list theta-i (list))))
                        do (format projection-tables.csv
                                   "~&~:(~20@a~),$~2,'0x,~6fπ,~6f°,$~2,'0x,~3d,~6f,~{$~2,'0x.~2,'0x~}"
                                   (beautify-name table)
                                   theta-brads (rationalize (/ theta-rads pi)) (* 180 (/ theta-rads pi))
                                   xz xz
                                   value (fixed-8.8 value :note (list table theta-brads xz)))
                        do (appendf (getf (getf long-tables table) theta-i)
                                    (list value)))))))
          (loop for (table values) on short-tables by #'cddr
                do (format projection-tables.s "
~aL:
~{~&~10t.byte $~2,'0x, $~2,'0x, $~2,'0x, $~2,'0x,  $~2,'0x, $~2,'0x, $~2,'0x, $~2,'0x~}
~aH:
~{~&~10t.byte $~2,'0x, $~2,'0x, $~2,'0x, $~2,'0x,  $~2,'0x, $~2,'0x, $~2,'0x, $~2,'0x~}"
                           (cl-change-case:pascal-case (symbol-name table))
                           (mapcar (lambda (n) (second (fixed-8.8 n)))
                                   values)
                           (cl-change-case:pascal-case (symbol-name table))
                           (mapcar (lambda (n) (first (fixed-8.8 n)))
                                   values)))
          (loop for (table theta-values) on long-tables by #'cddr
                do (format projection-tables.s "
~aLThetaL:
~{~&~10t.byte <(~aL_Theta_eql_~2,'0x)~}~0@*
~aLThetaH:
~{~&~10t.byte >(~aL_Theta_eql_~2,'0x)~}~0@*
~aHThetaL:
~{~&~10t.byte <(~aH_Theta_eql_~2,'0x)~}~0@*
~aHThetaH:
~{~&~10t.byte >(~aH_Theta_eql_~2,'0x)~}
"
                           (cl-change-case:pascal-case (symbol-name table))
                           (loop for (theta-i values) on theta-values by #'cddr
                                 append (list (cl-change-case:pascal-case (symbol-name table))
                                              theta-i)))
                do (loop for (theta-i values) on theta-values by #'cddr
                         do (format projection-tables.s "
~aL_Theta_eql_~2,'0x:
~{~&~10t.byte $~2,'0x, $~2,'0x, $~2,'0x, $~2,'0x,  $~2,'0x, $~2,'0x, $~2,'0x, $~2,'0x~}
~aH_Theta_eql_~2,'0x:
~{~&~10t.byte $~2,'0x, $~2,'0x, $~2,'0x, $~2,'0x,  $~2,'0x, $~2,'0x, $~2,'0x, $~2,'0x~}"
                                    (cl-change-case:pascal-case (symbol-name table))
                                    theta-i
                                    (mapcar (lambda (n) (second (fixed-8.8 n)))
                                            values)
                                    (cl-change-case:pascal-case (symbol-name table))
                                    theta-i
                                    (mapcar (lambda (n) (first (fixed-8.8 n)))
                                            values))))))))
  (format *trace-output* " Done.~%"))

(defun debug-projection (dx dz cx cz theta-brads)
  (let ((theta (* 2.0d0 pi (/ theta-brads #x100)))
        (phi (atan (/ 1 100.0))))
    (list (+ (+ (* (n-8.8 (cos theta)) dx))
             (- (* (n-8.8 (sin theta)) dz))
             (+ (* (n-8.8 (sin theta)) cz))
             (- (* (n-8.8 (cos theta)) cx)))
          (+ (+ (* (n-8.8 (* (cos theta) (sin phi))) dx))
             (+ (* (n-8.8 (* (sin theta) (sin phi))) dz))
             (- (* (n-8.8 (* (cos theta) (sin phi))) cx))
             (- (* (n-8.8 (* (sin theta) (sin phi))) cz))
             (+ (cos phi))))))

(defun n-8.8 (number)
  (apply #'fixed-rational (fixed-8.8 number)))

(defun fixed-rational (low high)
  (+ high (/ low #x100)))

(defun fixed-display (fixed)
  (format nil "~{~2,'0x.~2,'0x~}" fixed))

(defun write-inventory-tables (&optional (source-text "Source/Tables/Inventory.txt")
                                         (source-code "Source/Generated/InventoryLabels.s")
                                         (label "Item"))
  "Collect the names of all inventory items and write them out"
  (format *trace-output* "~&Reading inventory ~(~a~) names from ~a…"
          label (enough-namestring source-text))
  (finish-output *trace-output*)
  (with-output-to-file (code source-code :if-exists :supersede)
    (format code ";;; Generated from ~a

~aNames: .block~2%"
            (enough-namestring source-text)
            label)
    (with-input-from-file (text source-text)
      (loop for counter from 0 below (* 16 8)
            for line = (read-line text nil nil)
            while (and line (not (emptyp line)))
            do (format code "~&~a~x:~10t.text ~s~60t; ~d (~{~d.~d~})"
                       label
                       counter
                       (subseq line 0 (position #\; line))
                       counter
                       (multiple-value-list (floor counter 8)))
            finally
               (progn
                 (format *trace-output* " …read ~:d ~(~a~) name~:p (of ~:d max), done.~%"
                         (1+ counter) label (* 16 8))
                 (format code (format nil "
~~10tEndOf~0@*~aNames := *
~~10tAll~0@*~aNames=(~~{~0@*~a~~x~~^,~~})
Low:~~10t.byte <(All~0@*~aNames), <EndOf~0@*~aNames
High:~~10t.byte >(All~0@*~aNames), >EndOf~0@*~aNames
~~10t.bend
;;; end of file~~%" label)
                         (loop for i from 0 below counter
                               collecting i)))))))

(defun write-keys-tables ()
  "Write the file out with the enumerated key names"
  (write-inventory-tables #p"Source/Tables/Keys.txt" #p"Source/Generated/KeyLabels.s" "Key"))

(defun write-flags-tables (&optional (source-text #p"Source/Tables/Flags.txt")
                                     (source-code #p"Source/Generated/FlagLabels.s")
                                     (forth-code #p"Source/Generated/FlagLabels.forth"))
  "Write the file out with the enumerated flag names"
  (format *trace-output* "~&Reading game flag names from ~a…" (enough-namestring source-text))
  (finish-output *trace-output*)
  (with-output-to-file (code source-code :if-exists :supersede)
    (format code ";;; Generated from ~a

GameFlag: .block~2%"
            (enough-namestring source-text))
    (with-output-to-file (forth forth-code :if-exists :supersede)
      (format forth " ( Generated from ~a )~2%" (enough-namestring source-text))
      (with-input-from-file (text source-text)
        (loop for counter from 0 below (* 16 8)
              for line = (read-line text nil nil)
              while (and line (not (emptyp line)))
              do (progn
                   (format code "~&~10t~a = $~2,'0x" (pascal-case line) counter)
                   (format forth "~% : GameFlag_~a ~d ; " (pascal-case line) counter))
              finally
                 (progn
                   (format *trace-output* " …read ~:d game flag name~:p (of ~:d max), done.~%"
                           (1+ counter) (* 16 8))
                   (format code "~%~10t.bend~% ;;; end of file~%")
                   (format forth "~%( end of file )~%")))))))

(defun write-characters-tables (&optional (spreadsheet-pathname "Source/Tables/NPCStats.ods")
                                          (source-pathname "Source/Generated/CharacterTables.s"))
  (format *trace-output* "~&Reading NPC stats from ~a … "
          (enough-namestring spreadsheet-pathname))
  (finish-output *trace-output*)
  (let ((*npc-stats* (list)))
    (push (list :name "Player" :kind "Player") *npc-stats*)
    (push (list :name "Narrator" :kind "Narrator") *npc-stats*)
    (load-npc-stats spreadsheet-pathname)
    (with-output-to-file (source source-pathname :if-exists :supersede)
      (format *trace-output* "writing ~a … " (enough-namestring source-pathname))
      (finish-output *trace-output*)
      (format source ";;; Generated from ~a
;;; Character tables~2%"
              (enough-namestring spreadsheet-pathname))
      (format source "~%CharNames:
~10t.text \"terrificguy\"
~10t.text \"narrator\", 0, 0, 0, 0~
~{~%~10t.text \"~a\"~@[,~30t~{~a~^, ~a~^, ~a~^, ~a~^,   ~}~]~}
"
              (loop for char in *npc-stats*
                    collect (getf char :name)
                    collect (loop repeat (- 12 (length (getf char :name)))
                                  collect 0)))
      (loop for name in '(:character-id :hp :ac :pitch :speed
                          :kind :hair-color :skin-color :clothes-color
                          :head :body)
            for asm-name = (cl-change-case:pascal-case (string name))
            do (progn
                 (format source "~%~a:~%~10t.byte $00, $00~32t; Player, Narrator" asm-name)
                 (dolist (char *npc-stats*)
                   (format *trace-output* "~%~4t~s~20t~a" (npc-interpret-field (getf char name) name)
                           (getf char :name)))
                 (loop for char in *npc-stats*
                       do (format source "~%~10t.byte $~2,'0x~32t; (~:(~a~))"
                                  (or (npc-interpret-field (getf char name) name
                                                           :name (getf char :name))
                                      0)
                                  (getf char :name)))))
      (format *trace-output* " done."))))

(defun write-docks-index (&optional (pathname #p"Source/Generated/DocksIndex.s"))
  "Write the dock IDs from the maps index to PATHNAME"
  (read-map-ids-table)
  (let ((max-dock-id (loop for dock-id being the hash-keys of *dock-ids-maps*
                           maximize dock-id into max-dock-id
                           finally (return max-dock-id))))
    (with-output-to-file (code pathname :if-exists :supersede)
      (loop for i from 1 upto max-dock-id
            for name = (gethash i *dock-ids-maps* "untitled")
            do (format code "~%DockName~d: .ptext \"~a\""
                       i
                       (cl-change-case:lower-case (gethash name *maps-display-names* "untitled"))))
      (format code "~2%~10tDockNames = (~{DockName~d~^, ~})"
              (loop for i from 1 upto max-dock-id collecting i))
      (format code "~2%DockNameL: <(DockNames)~%DockNameH: >(DockNames)"))))

(defun read-orchestration (&optional (pathname #p"Source/Tables/Orchestration.ods"))
  "Read the orchestration table from Source/Tables/Orchestration.ods"
  (format *trace-output* "~&Reading orchestration from ~a" (enough-namestring pathname))
  (finish-output *trace-output*)
  (let ((table (ss->lol (first (read-ods-into-lists pathname)))))
    (loop for row in table
          when (and row (not (emptyp (string-trim #(#\Space) (getf row :instrument)))))
            collecting (list :instrument (getf row :instrument)
                             :distortion (make-keyword (string-upcase (getf row :distortion)))
                             :attack-addend (parse-number (getf row :attack-addend))
                             :decay-subtrahend (parse-number (getf row :decay-subtrahend))
                             :decay-duration (parse-number (getf row :decay-duration))
                             :release-subtrahend (parse-number (getf row :release-subtrahend))
                             :tia-distortion (parse-number (getf row :tia-distortion))))))

(defun write-orchestration (&optional (input #p"Source/Tables/Orchestration.ods")
                                      (output #p"Source/Generated/Orchestration.s"))
  "Write the orchestration tables to a source code file. 

INPUT & OUTPUT pathnames can be given."
  (with-simple-restart (do-over "Re-read the Orchestration.ods file")
    (with-output-to-file (out output :if-exists :supersede)
      (format *trace-output* "~&Going to write orchestration tables from ~a to source code file ~a…"
              (enough-namestring input) (enough-namestring output))
      (format out ";;;; Phantasia ~a~%;;; This file is generated from ~a~2%"
              (enough-namestring output) (enough-namestring input))
      (let ((table (read-orchestration input)))
        (format out "~2%~10tNumInstruments = ~d" (length table))
        (format out "~2%;;; FIXME PAL support 
;;; (multiple-value-bind (int fract) (floor (/ (* 1.0 60) 50)) (list int (floor (* fract #x100))))
;;; FIXME PAL support")
        (format out "~2%InstrumentHokeyDistortion:")
        (dolist (row table)
          (format out "~%~10t.byte $~2,'0x~40t; ~a"
                  (ash (or (parse-integer (string (getf row :distortion)) :junk-allowed t) 10) 4)
                  (title-case (getf row :instrument))))
        (format out "~2%InstrumentTIADistortion:")
        (dolist (row table)
          (format out "~%~10t.byte $~2,'0x~40t; ~a"
                  (getf row :tia-distortion)
                  (title-case (getf row :instrument))))
        (format out "~2%InstrumentAttackAddend:")
        (dolist (row table)
          (format out "~%~10t.byte $~2,'0x~40t; ~a"
                  (floor (getf row :attack-addend))
                  (title-case (getf row :instrument))))
        (format out "~2%InstrumentAttackFraction:")
        (dolist (row table)
          (format out "~%~10t.byte $~2,'0x~40t; ~a"
                  (floor (* #x100 (nth-value 1 (floor (getf row :attack-addend)))))
                  (title-case (getf row :instrument))))
        (format out "~2%InstrumentDecaySubtrahend:")
        (dolist (row table)
          (format out "~%~10t.byte $~2,'0x~40t; ~a"
                  (floor (getf row :decay-subtrahend))
                  (title-case (getf row :instrument))))
        (format out "~2%InstrumentDecayFraction:")
        (dolist (row table)
          (format out "~%~10t.byte $~2,'0x~40t; ~a"
                  (floor (* #x100 (nth-value 1 (floor (getf row :decay-subtrahend)))))
                  (title-case (getf row :instrument))))
        (format out "~2%InstrumentDecayDuration:")
        (dolist (row table)
          (format out "~%~10t.byte $~2,'0x~40t; ~a"
                  (floor (getf row :decay-duration))
                  (title-case (getf row :instrument))))
        (format out "~2%InstrumentReleaseSubtrahend:")
        (dolist (row table)
          (format out "~%~10t.byte $~2,'0x~40t; ~a"
                  (floor (getf row :release-subtrahend))
                  (title-case (getf row :instrument))))
        (format out "~2%InstrumentReleaseFraction:")
        (dolist (row table)
          (format out "~%~10t.byte $~2,'0x~40t; ~a"
                  (floor (* #x100 (nth-value 1 (floor (getf row :release-subtrahend)))))
                  (title-case (getf row :instrument))))
        (format out "~2%;;; End of Orchestration~2%")))))

(defun write-equipment-index (&optional (pathname #p"Source/Generated/EquipmentIndex.s"))
  "Write EquipmentIndex.s from Source/Tables/EquipmentIndex.ods"
  (format *trace-output* "~&Reading equipment attributes from ~a…" #p"Source/Tables/EquipmentIndex.ods")
  (finish-output *trace-output*)
  (let ((sheet (read-ods-into-lists #p"Source/Tables/EquipmentIndex.ods")))
    (let* ((equipment-stats (remove-if-not (lambda (record)
                                             (loop for (key value) on record
                                                   by #'cddr
                                                   unless (str:blankp value)
                                                     return t
                                                   finally (return nil)))
                                           (ss->lol (first sheet)))))
      (ensure-directories-exist pathname)
      (with-output-to-file (output pathname :if-exists :supersede)
        (format *trace-output* "writing ~a …" (enough-namestring pathname))
        (finish-output *trace-output*)
        (format output ";;; Generated from Source/Tables/EquipmentIndex.ods~2%EquipmentIndex: .block~%")
        (flet ((always (format value)
                 (declare (ignore value))
                 format)
               (here? (format s)
                 (if (str:blankp s) ".byte 0" format))
               (dec (format s)
                 (if (str:blankp s)
                     ".byte $ff"
                     format))
               (hex (format s)
                 (if (str:blankp s)
                     ".byte $ff"
                     format))
               (drawing-mode-filter (format s)
                 (declare (ignore format))
                 (if (string-equal "160B" (string-trim #(#\Space) s))
                     ".byte Decal160B"
                     ".byte 0")))
          (loop for (format validator field-info)
                  on
                  (list ".byte $~2,'0x" #'hex :index
                        ".byte $~2,'0x" #'hex :decal-bank
                        ".byte ~aClass" #'here? '(:entity-class :entity)
                        ".byte <~aPrototype" #'here? '(:entity-prototype :entity-prototype-l)
                        ".byte >~aPrototype" #'here? '(:entity-prototype :entity-prototype-h)
                        "" #'drawing-mode-filter :drawing-mode
                        ".byte ~aClass" #'here? '(:course-class :course)
                        ".byte <~aPrototype" #'here? '(:course-prototype :course-prototype-l)
                        ".byte >~aPrototype" #'here? '(:course-prototype :course-prototype-h)
                        ".byte Song_~a_ID" #'here? :sound
                        ".byte $~2,'0x" #'hex :up
                        ".byte $~2,'0x" #'hex :down
                        ".byte $~2,'0x" #'hex :left
                        ".byte $~2,'0x" #'hex :right
                        ".byte ~d << PaletteShift" #'dec :palette
                        ".byte ~d" #'dec :displace-up
                        ".byte ~d" #'dec :displace-down
                        ".byte ~d" #'dec :displace-left
                        ".byte ~d" #'dec :displace-right
                        ".byte >~a" #'here? :decal-sheet)
                by #'cdddr

                for field-name = (if (listp field-info)
                                     (first field-info)
                                     field-info)
                for field-asm-name = (if (listp field-info)
                                         (second field-info)
                                         field-info)
                do (format output "~2%~a:" (pascal-case (string field-asm-name)))
                do (dolist (item equipment-stats)
                     (let ((value (getf item field-name)))
                       (format output "~%~10t~?~40t; ~a"
                               (funcall validator format value)
                               (cons value nil)
                               (title-case (getf item :item-name)))))))
        (format output "~2%~10t.bend~%")))))
