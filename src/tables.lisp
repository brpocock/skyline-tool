(in-package :skyline-tool)

(defun read-ods-into-lists (pathname)
  "Read an OpenDocument Spreadsheet (.ods) file into Lisp lists.

Parses an ODS file and extracts spreadsheet data as a list of lists,
where each sublist represents a worksheet/page in the spreadsheet.

@table @asis
@item PATHNAME
Path to the .ods file
@item Returns
List of worksheet data, where each worksheet is a list of rows
@end table

@xref{fun:ss->lol}, @xref{fun:extract-ss-titles}."
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
  "Extract column titles from spreadsheet header row.

Converts spreadsheet column headers to Lisp keywords for use as property names.

@table @asis
@item ROW1
First row of spreadsheet data containing column headers
@item Returns
List of keywords representing column names
@end table

@xref{fun:ss->lol}."
  (loop for column in row1
        when (not (emptyp column))
          collect (make-keyword (string-upcase (cl-change-case:param-case column)))))

(defun ss->lol (page)
  "Convert spreadsheet page data to list-of-lists format.

Transforms raw spreadsheet data into a list of property lists, where each
row becomes a plist with column titles as keys.

@table @asis
@item PAGE
Spreadsheet page data (list of rows)
@item Returns
List of property lists, one per data row
@end table

@xref{fun:extract-ss-titles}, @xref{fun:ss->arrays}."
  (let* ((row1 (first page))
         (titles (extract-ss-titles row1)))
    (loop for row in (cdr page)
          collect (loop for title in titles
                        for column in row
                        when (and title (not (emptyp column)))
                          append (list title column)))))

(defun ss->arrays (page)
  "Convert spreadsheet page to column-major arrays.

Transforms spreadsheet data into a plist where each property is a column
of data stored as a Lisp array, useful for bulk processing of table columns.

@table @asis
@item PAGE
Spreadsheet page data (list of rows)
@item Returns
Property list with column names as keys and arrays as values
@end table

@xref{fun:ss->lol}, @xref{fun:extract-ss-titles}."
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
  "Parse a VALUE as a number if possible.

Attempts to convert various input types to numbers, returning NIL for
non-numeric inputs.

@table @asis
@item VALUE
Input value (number, string, or nil)
@item Returns
Number if parseable, otherwise NIL
@end table"
  (etypecase value
    (number value)
    (null nil)
    (string (handler-case (parse-number value)
              (invalid-number ())
              (sb-int:simple-parse-error ())))))

(defun write-projection-tables.s ()
  "Writes Source/Generated/$(PORT)/ProjectionTables.s database.
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
    (let ((machine-dir (format nil "Source/Generated/~a/" (machine-directory-name))))
      (ensure-directories-exist (merge-pathnames machine-dir (uiop:getcwd)))
      (with-output-to-file (projection-tables.csv (merge-pathnames (concatenate 'string machine-dir "ProjectionTables.csv") (uiop:getcwd))
        :if-exists :supersede)
        (with-output-to-file (projection-tables.s (merge-pathnames (concatenate 'string machine-dir "ProjectionTables.s") (uiop:getcwd))
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
    (format *trace-output* " Done.~%")))

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

(defun write-inventory-tables (&optional (source-text (merge-pathnames "Source/Tables/Inventory.txt" (uiop:getcwd)))
                                         (source-code (merge-pathnames (format nil "Source/Generated/~a/InventoryLabels.s" (skyline-tool::machine-directory-name))
                                                                       (uiop:getcwd)))
                                         (label "Item"))
  "Collect the names of all inventory items and write them out"
  (format *trace-output* "~&Reading inventory ~(~a~) names from ~a…"
          label (enough-namestring source-text))
  (finish-output *trace-output*)
  (let ((counter 0))
    (with-output-to-file (code source-code :if-exists :supersede)
      (format code ";;; Generated from ~a

~aNames: .block~2%"
              (enough-namestring source-text)
              label)
      (with-input-from-file (text source-text)
        (loop for line = (read-line text nil nil)
              while (and line (not (emptyp line)) (< counter (* 16 8)))
              do (format code "~&~a~x:~10t.text ~s~60t; ~d (~{~d.~d~})"
                         label
                         counter
                         (subseq line 0 (position #\; line))
                         counter
                         (multiple-value-list (floor counter 8)))
                 (incf counter)))
      (format *trace-output* " …read ~:d ~(~a~) name~:p (of ~:d max), done.~%"
              counter label (* 16 8))
      (format code "~2&~10tEndOf~aNames := *
~10tAll~aNames=(~{~a~^,~})
Low:~10t.byte <(All~aNames), <EndOf~aNames
High:~10t.byte >(All~aNames), >EndOf~aNames
~10t.bend
;;; end of file~%"
              label label
              (loop for i from 0 below counter collecting (format nil "~a~x" label i))
              label label label label))))

(defun write-keys-tables ()
  "Write the file out with the enumerated key names"
  (write-inventory-tables #p"Source/Tables/Keys.txt"
                          (format nil "Source/Generated/~a/KeyLabels.s" (machine-directory-name))
                          "Key"))

(defun write-flags-tables
    (&optional (source-text #p"Source/Tables/Flags.txt")
               (source-code (format nil "Source/Generated/~a/FlagLabels.s" (machine-directory-name)))
               (forth-code (format nil "Source/Generated/~a/FlagLabels.forth" (machine-directory-name))))
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

(defun write-characters-tables
    (&optional (spreadsheet-pathname #p"Source/Tables/NPCStats.ods")
               (source-pathname (format nil "Source/Generated/~a/CharacterTables.s"
                                        (skyline-tool::machine-directory-name)))) 
  "Write character tables from SPREADSHEET-PATHNAME to SOURCE-PATHNAME.

SPREADSHEET-PATHNAME: Path to the NPC stats spreadsheet (default: Source/Tables/NPCStats.ods)
SOURCE-PATHNAME: Output path for the generated source file"
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
                          :head :body :equipment :shield)
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

(defun find-dock-in-tmx (locale-name &optional dock-name)
  "Find a dock object in the TMX file for LOCALE-NAME.
Searches all object groups for an object with type=\"Dock\".
When DOCK-NAME is provided, also filters on the object's name.
Returns (values tether-x tether-y) in tile units
(pixel coordinates divided by 16)."
  (declare (ignore dock-name))
  (let ((xml (load-other-map locale-name)))
    (dolist (object-group (xml-matches "objectgroup" xml))
      (dolist (object (xml-matches "object" object-group))
        (let ((type (xml-attr "type" (second object)))
              (x (xml-attr "x" (second object)))
              (y (xml-attr "y" (second object))))
          (when (and type (string-equal type "Dock"))
            (return-from find-dock-in-tmx
              (values (floor (parse-number:parse-number x) 16)
                      (floor (parse-number:parse-number y) 16)))))))
    ;; No Dock object found — default to 4 tiles from the right edge, midline
    (let* ((map-attrs (second xml))
           (map-width (parse-number (xml-attr "width" map-attrs)))
           (map-height (parse-number (xml-attr "height" map-attrs)))
           (default-x (- map-width 4))
           (default-y (floor map-height 2)))
      (values default-x default-y))))

(defun read-flag-index (flag-name)
  "Read Flags.txt and return the 0-based line index of FLAG-NAME.
Comparison is case-insensitive.  Returns 0 when FLAG-NAME is nil or
blank (always-visible flag).  Signals an error when the flag name is
not found in the file."
  (if (or (null flag-name)
          (emptyp (string-trim '(#\Space #\Tab) (string flag-name))))
      0
      (let ((pathname (merge-pathnames #p"Source/Tables/Flags.txt"
                                       (uiop:getcwd))))
        (with-open-file (stream pathname :direction :input)
          (loop for line = (read-line stream nil nil)
                for i from 0
                while line
                for trimmed = (string-trim '(#\Space #\Tab #\Return #\Newline) line)
                do (when (string-equal (header-case trimmed) (header-case flag-name))
                     (return i))
                finally (error "Flag ~s not found in ~a"
                               flag-name pathname))))))

(defun write-sea-chart-docks-index
    (&optional (pathname (merge-pathnames
                          (format nil "Source/Generated/~a/Docks.s"
                                  (machine-directory-name))
                          (uiop:getcwd)))
               (spreadsheet (merge-pathnames #p"Source/Tables/SeaChartDocks.ods"
                                             (uiop:getcwd))))
  "Read dock locations from SPREADSHEET and write 7800 assembly to PATHNAME.
Reads the SeaChartDocks.ods spreadsheet (first sheet), looks up each
dock's map ID from the maps index, finds tether coordinates from the
locale TMX file, resolves flag indices from Flags.txt, and generates
a 64tass assembly file describing dock cursor positions and warps."
  (format *trace-output* "~&Reading sea chart docks from ~a …"
          (enough-namestring spreadsheet))
  (finish-output *trace-output*)
  (read-map-ids-table)
  (let* ((raw (first (read-ods-into-lists spreadsheet)))
         (table (and raw (ss->lol raw)))
         (docks (and table
                     (loop for row in table
                           for locale = (getf row :island)
                           for map-name = (getf row :map)
                           for sea-chart-x = (getf row :chart-x)
                           for sea-chart-y = (getf row :chart-y)
                           for island-name = (getf row :island)
                           for flag-name = (getf row :flag)
                           when (and locale map-name
                                     (not (emptyp (string locale)))
                                     (not (emptyp (string map-name))))
                             collect (list :locale locale
                                           :map-name map-name
                                           :sea-chart-x sea-chart-x
                                           :sea-chart-y sea-chart-y
                                           :island-name island-name
                                           :flag-name flag-name)))))
    (assert docks)
    (format *trace-output* " … read ~:d dock~:p …" (length docks))
    (finish-output *trace-output*)
    (with-output-to-file (code pathname :if-exists :supersede)
      (format code ";;; Generated from ~a~2%;;; Sea Chart Dock Index~2%"
              (enough-namestring spreadsheet))
      (format code "~2%Docks: .block")
      (format code "~2%MapID:")
      (dolist (dock docks)
        (format code "~%~10t.byte $~2,'0x~32t ~a/~a"
                (gethash (format nil "~a/~a"
                                 (pascal-case (getf dock :locale-name))
                                 (pascal-case (getf dock :map-name)))
                         *maps-index*)
                (getf dock :locale) (getf dock :map-name)))
      (format code "~2%Name:")
      (dolist (dock docks)
        (format code "~%~10t.ftext 20, \"~a\""
                (string-trim #(#\space #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                             (getf dock :map-name))))
      (format code "~2%Flag:")
      (dolist (dock docks)
        (if-let ((flag-id (read-flag-index (getf dock :flag-name))))
          (format code "~%~10t.byte $~2,'0x~32t; ~a … ~a"
                  flag-id
                  (getf dock :flag-name)
                  (getf dock :locale-name))
          (format code "~%~10t.byte 0~32t; — (~a)"
                  (getf dock :locale-name))))
      (format code "~2%TetherX:")
      (dolist (dock docks)
        (multiple-value-bind (tether-x tether-y)
            (find-dock-in-tmx (format nil "~a/~a"
                                      (pascal-case (getf dock :locale-name))
                                      (pascal-case (getf dock :map-name))))
          (declare (ignore tether-y))
          (format code "~%~10t.byte ~d~32t; ~a"
                  (parse-integer (string tether-x))
                  (getf dock :locale-name))))
      (format code "~2%TetherY:")
      (dolist (dock docks)
        (multiple-value-bind (tether-x tether-y)
            (find-dock-in-tmx (format nil "~a/~a"
                                      (pascal-case (getf dock :locale-name))
                                      (pascal-case (getf dock :map-name))))
          (declare (ignore tether-x))
          (format code "~%~10t.byte ~d~32t; ~a"
                  (parse-integer (string tether-y))
                  (getf dock :locale-name)))        )
      (format code "~2%~10t.bend~%")
      (format code "~2%NumDocks = ~d" (length docks)))
    (format *trace-output* " wrote ~a." (enough-namestring pathname))))

(defun parse-number-or-fraction (value)
  "Parse VALUE as a number, supporting integers, decimals, and fractions (e.g. 20/3).
Returns a real number. Handles values that may be strings, numbers, or nil.
Uses parse-number:parse-number for numeric parsing (supports decimals, hex, etc.)."
  (cond
    ((numberp value) value)
    ((null value) 0)
    (t (let ((s (string-trim #(#\Space #\Tab) (string value))))
         ;; Normalize Unicode fraction slash (U+2044) to ASCII slash
         (setf s (substitute #\/ (code-char #x2044) s))
         (let ((pos (position #\/ s)))
           (if pos
               (/ (parse-number:parse-number (subseq s 0 pos))
                  (parse-number:parse-number (subseq s (1+ pos))))
               (parse-number:parse-number s)))))))

(defun read-orchestration (&optional (pathname #p"Source/Tables/Orchestration.ods"))
  "Read the orchestration table from Source/Tables/Orchestration.ods"
  (format *trace-output* "~&Reading orchestration from ~a" (enough-namestring pathname))
  (finish-output *trace-output*)
  (let ((table (ss->lol (first (read-ods-into-lists pathname)))))
    (loop for row in table
          when (and row (not (emptyp (string-trim #(#\Space) (getf row :instrument)))))
            collecting
            (list :instrument (getf row :instrument)
                  :distortion (make-keyword (string-upcase (getf row :distortion)))
                  :attack-addend (parse-number-or-fraction (getf row :attack-addend))
                  :decay-subtrahend (parse-number-or-fraction (getf row :decay-subtrahend))
                  :decay-duration (parse-number-or-fraction (getf row :decay-duration))
                  :release-subtrahend (parse-number-or-fraction (getf row :release-subtrahend))
                  :tia-distortion (parse-number-or-fraction (getf row :tia-distortion))
                  :vibrato (parse-number-or-fraction (getf row :vibrato))
                  :tremolo (parse-number-or-fraction (getf row :tremolo))
                  :psg-tone (getf row :psg-tone)))))

(defun orchestration-psg-tone-byte (row)
  "Return 0 for tonal PSG voices, 1 for the white-noise generator path.

Uses @code{PSG Tone} from @var{ROW} when present (@code{0}/tone = tonal,
@code{1}/noise = noise).  When blank, percussion instrument names and
@code{Snare Drum} / @code{Wood Blocks} default to noise; all others default
to tonal (Hokey @code{Distortion} is not used on Intellivision)."
  (let ((psg (getf row :psg-tone))
        (name (string-downcase (string (getf row :instrument "")))))
    (cond
      ((or (search "snare" name)
           (search "wood block" name)
           (search "kick" name)
           (search "hi-hat" name)
           (search "cymbal" name))
       1)
      ((and psg (stringp psg) (not (str:blankp psg))
            (or (search "noise" (string-downcase psg))
                (string-equal psg "1")))
       1)
      ((and psg (stringp psg) (not (str:blankp psg))
            (or (search "tone" (string-downcase psg))
                (string-equal psg "0")))
       0)
      (psg
       (let ((n (ignore-errors (parse-number-or-fraction psg))))
         (cond ((null n) 0)
               ((<= n 0) 0)
               ((= n 1) 1)
               (t 0))))
      (t 0))))

(defun write-intv-orchestration (&optional
                                   (input #p"Source/Tables/Orchestration.ods")
                                   (output (format nil "Source/Generated/~a/Orchestration.s"
                                                   (machine-directory-name))))
  "Write cp1610 orchestration tables for Intellivision (PSG ADSR + voice map)."
  (with-output-to-file (out output :if-exists :supersede)
    (format *trace-output* "~&Going to write Intellivision orchestration from ~a to ~a…"
            (enough-namestring input) (enough-namestring output))
    (format out ";;;; ~:(~a~) ~a~%;;; This file is generated from ~a~2%"
            *game-title* (enough-namestring output) (enough-namestring input))
    (let ((table (read-orchestration input)))
      (format out "NumInstruments~32tEQU     ~d~2%" (length table))
      (format out "InstrumentAttackAddend:")
      (dolist (row table)
        (format out "~%~12tBYTE $~2,'0x~40t; ~a"
                (floor (getf row :attack-addend))
                (title-case (getf row :instrument))))
      (format out "~2%InstrumentAttackFraction:")
      (dolist (row table)
        (format out "~%~12tBYTE $~2,'0x~40t; ~a"
                (floor (* #x100 (nth-value 1 (floor (getf row :attack-addend)))))
                (title-case (getf row :instrument))))
      (format out "~2%InstrumentDecaySubtrahend:")
      (dolist (row table)
        (format out "~%~12tBYTE $~2,'0x~40t; ~a"
                (floor (getf row :decay-subtrahend))
                (title-case (getf row :instrument))))
      (format out "~2%InstrumentDecayFraction:")
      (dolist (row table)
        (format out "~%~12tBYTE $~2,'0x~40t; ~a"
                (floor (* #x100 (nth-value 1 (floor (getf row :decay-subtrahend)))))
                (title-case (getf row :instrument))))
      (format out "~2%InstrumentDecayDuration:")
      (dolist (row table)
        (format out "~%~12tBYTE $~2,'0x~40t; ~a"
                (floor (getf row :decay-duration))
                (title-case (getf row :instrument))))
      (format out "~2%InstrumentReleaseSubtrahend:")
      (dolist (row table)
        (format out "~%~12tBYTE $~2,'0x~40t; ~a"
                (floor (getf row :release-subtrahend))
                (title-case (getf row :instrument))))
      (format out "~2%InstrumentReleaseFraction:")
      (dolist (row table)
        (format out "~%~12tBYTE $~2,'0x~40t; ~a"
                (floor (* #x100 (nth-value 1 (floor (getf row :release-subtrahend)))))
                (title-case (getf row :instrument))))
      (format out "~2%InstrumentVibratoTremelo:~%~12t;; vibrato in high nybble, tremolo in low")
      (dolist (row table)
        (format out "~%~12tBYTE $~x~x~40t; ~a"
                (floor (min 15 (max 0 (getf row :vibrato 0))))
                (floor (min 15 (max 0 (getf row :tremolo 0))))
                (title-case (getf row :instrument))))
      (format out "~2%InstrumentPSGTone:~%~12t;; 0 = tonal voice, 1 = white noise")
      (dolist (row table)
        (format out "~%~12tBYTE ~d~40t; ~a"
                (orchestration-psg-tone-byte row)
                (title-case (getf row :instrument))))
      (format out "~2%;;; End of Orchestration~%"))))

(defun write-orchestration (&optional
                              (input #p"Source/Tables/Orchestration.ods")
                              (output (format nil "Source/Generated/~a/Orchestration.s"
                                              (machine-directory-name))))
  "Write the orchestration tables to a source code file.

INPUT & OUTPUT pathnames can be given."
  (when (string-equal (machine-directory-name) "Intv")
    (return-from write-orchestration (write-intv-orchestration input output)))
  (with-simple-restart (do-over "Re-read the Orchestration.ods file")
    (with-output-to-file (out output :if-exists :supersede)
      (format *trace-output* "~&Going to write orchestration tables from ~a to source code file ~a…"
              (enough-namestring input) (enough-namestring output))
      (format out ";;;; ~:(~a~) ~a~%;;; This file is generated from ~a~2%"
              *game-title* (enough-namestring output) (enough-namestring input))
      (let ((table (read-orchestration input)))
        (format out "~2%~10tNumInstruments = ~d" (length table))
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
        (format out "~%~10t.if TV == NTSC")
        (dolist (row table)
          (format out "~%~10t.byte $~2,'0x~40t; ~a"
                  (floor (getf row :attack-addend))
                  (title-case (getf row :instrument))))
        (format out "~%~10t.else")
        (dolist (row table)
          (format out "~%~10t.byte $~2,'0x~40t; ~a"
                  (multiple-value-bind (int fract)
                      (floor (/ 60.0 50.0))
                    (declare (ignore fract))
                    (getf row :attack-addend)
                    int)
                  (title-case (getf row :instrument))))
        (format out "~%~10t.fi")
        (format out "~2%InstrumentAttackFraction:")
        (format out "~%~10t.if TV == NTSC")
        (dolist (row table)
          (format out "~%~10t.byte $~2,'0x~40t; ~a"
                  (floor (* #x100 (nth-value 1 (floor (getf row :attack-addend)))))
                  (title-case (getf row :instrument))))
        (format out "~%~10t.else")
        (dolist (row table)
          (format out "~%~10t.byte $~2,'0x~40t; ~a"
                  (multiple-value-bind (int fract)
                      (floor (/ 60.0 50.0))
                    (declare (ignore int))
                    (getf row :attack-addend)
                    (floor (* #x100 fract)))
                  (title-case (getf row :instrument))))
        (format out "~%~10t.fi")
        (format out "~2%InstrumentDecaySubtrahend:")
        (format out "~%~10t.if TV == NTSC")
        (dolist (row table)
          (format out "~%~10t.byte $~2,'0x~40t; ~a"
                  (floor (getf row :decay-subtrahend))
                  (title-case (getf row :instrument))))
        (format out "~%~10t.else")
        (dolist (row table)          
          (format out "~%~10t.byte $~2,'0x~40t; ~a"
                  (multiple-value-bind (int fract)
                      (floor (/ 60.0 50.0))
                    (declare (ignore fract))
                    (getf row :decay-subtrahend)
                    int)
                  (title-case (getf row :instrument))))
        (format out "~%~10t.fi")
        (format out "~2%InstrumentDecayFraction:")
        (format out "~%~10t.if TV == NTSC")
        (dolist (row table)
          (format out "~%~10t.byte $~2,'0x~40t; ~a"
                  (floor (* #x100 (nth-value 1 (floor (getf row :decay-subtrahend)))))
                  (title-case (getf row :instrument))))
        (format out "~%~10t.else")
        (dolist (row table)
          (format out "~%~10t.byte $~2,'0x~40t; ~a"
                  (multiple-value-bind (int fract)
                      (floor (/ 60.0 50.0))
                    (declare (ignore int))
                    (getf row :decay-subtrahend)
                    (floor (* #x100 fract)))
                  (title-case (getf row :instrument))))
        (format out "~%~10t.fi")
        (format out "~2%InstrumentDecayDuration:")
        (dolist (row table)
          (format out "~%~10t.byte $~2,'0x~40t; ~a"
                  (floor (getf row :decay-duration))
                  (title-case (getf row :instrument))))
        (format out "~2%InstrumentReleaseSubtrahend:")
        (format out "~%~10t.if TV == NTSC")
        (dolist (row table)
          (format out "~%~10t.byte $~2,'0x~40t; ~a"
                  (floor (getf row :release-subtrahend))
                  (title-case (getf row :instrument))))
        (format out "~%~10t.else")
        (dolist (row table)
          (format out "~%~10t.byte $~2,'0x~40t; ~a"
                  (multiple-value-bind (int fract)
                      (floor (/ 60.0 50.0))
                    (declare (ignore fract))
                    (getf row :release-subtrahend)
                    int)
                  (title-case (getf row :instrument))))
        (format out "~%~10t.fi")
        (format out "~2%InstrumentReleaseFraction:")
        (format out "~%~10t.if TV == NTSC")
        (dolist (row table)
          (format out "~%~10t.byte $~2,'0x~40t; ~a"
                  (floor (* #x100 (nth-value 1 (floor (getf row :release-subtrahend)))))
                  (title-case (getf row :instrument))))
        (format out "~%~10t.else")
        (dolist (row table)
          (format out "~%~10t.byte $~2,'0x~40t; ~a"
                  (multiple-value-bind (int fract)
                      (floor (/ 60.0 50.0))
                    (declare (ignore int))
                    (getf row :release-subtrahend)
                    (floor (* #x100 fract)))
                  (title-case (getf row :instrument))))
        (format out "~%~10t.fi")
        (format out "~2%InstrumentVibratoTremelo:~%~10t;; vibrato in high nybble, tremolo in low")
        (dolist (row table)
          (format out "~%~10t.byte $~x~x~40t; ~a"
                  (floor (min 15 (max 0 (getf row :vibrato 0))))
                  (floor (min 15 (max 0 (getf row :tremolo 0))))
                  (title-case (getf row :instrument))))
        (format out "~2%InstrumentPSGTone:~%~10t;; 0 = tonal voice, 1 = white noise")
        (dolist (row table)
          (format out "~%~10t.byte ~d~40t; ~a"
                  (orchestration-psg-tone-byte row)
                  (title-case (getf row :instrument))))
        (format out "~2%;;; End of Orchestration~2%")))))

(defun read-equipment-stats ()
  (remove-if-not (lambda (record)
                   (loop for (key value) on record
                         by #'cddr
                         unless (str:blankp value)
                           return t
                         finally (return nil)))
                 (ss->lol (first (read-ods-into-lists #p"Source/Tables/EquipmentIndex.ods")))))

(defun write-equipment-index (&optional
                                (pathname (format nil "Source/Generated/~a/EquipmentIndex.s"
                                                  (machine-directory-name))))
  "Write EquipmentIndex.s from Source/Tables/EquipmentIndex.ods"
  (format *trace-output* "~&Reading equipment attributes from Source/Tables/EquipmentIndex.ods…")
  (finish-output *trace-output*)
  (let* ((equipment-stats (read-equipment-stats)))
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
      (format output "~2%~10t.bend~%"))))
