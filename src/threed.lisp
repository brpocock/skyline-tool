(in-package :skyline-tool)

(defvar *max-3d-vertices* nil)
(defvar *max-3d-triangles* nil)
(defvar *max-3d-edges* nil)

(defun read-3d-limits ()
  (with-input-from-file (enums #p"Source/Common/Enums.s")
    (loop for line = (read-line enums nil nil)
          while line
          do (cl-ppcre:register-groups-bind (var value)
                 ("^[ ]+Max(Vertices|Triangles|Edges)[ ]*=[ ]*([0-9]+)" line)
               (when var
                 (ecase (make-keyword (string-upcase var))
                   (:vertices (setf *max-3d-vertices* (parse-integer value)))
                   (:triangles (setf *max-3d-triangles* (parse-integer value)))
                   (:edges (setf *max-3d-edges* (parse-integer value)))))))
    (unless (and *max-3d-vertices* *max-3d-triangles* *max-3d-edges*)
      (error "Define MaxVertices, MaxTriangles, and MaxEdges in Source/Common/Enums.s
(MaxVertices = ~s, MaxTriangles = ~s, MaxEdges = ~s)"
             *max-3d-vertices* *max-3d-triangles* *max-3d-edges*))))

(defun fixed-8.8 (float &key note)
  (check-type float real)
  (assert (> #x80 float (- #x80)) (float)
          "The value ~f is out of range for a signed fixed-point 8.8 bit value~@[ (~a)~]"
          float note)
  (let ((bytes
          (multiple-value-bind (whole fraction) (ftruncate (/ (round float 1/256) 256))
            (cond
              ((zerop float) (list 0 0))
              ((and (minusp float) (zerop whole))
               (list #xff (min (1+ (logxor #xff (round (* #x100 (abs fraction))))) #xff)))
              ((minusp float)
               (list (1+ (logxor #xff (round (abs whole))))
                     (min (1+ (logxor #xff (round (* #x100 (abs fraction))))) #xff)))
              ((plusp float)
               (list (round whole)
                     (round (* #x100 fraction))))
              (t (error "What kind of number is ~s anyway?" float))))))
    (assert (every (lambda (byte) (<= 0 byte #xff)) bytes)
            (float)
            "Got (~{$~2,'0x $~2,'0x~}) for ~f, that's not right" bytes float)
    bytes))

(assert (equalp '(1 0) (fixed-8.8 1)))
(assert (equalp '(1 #x80) (fixed-8.8 1.5)))
(assert (equalp '(1 #x40) (fixed-8.8 1.25)))
(assert (equalp '(0 0) (fixed-8.8 0)))
(assert (equalp '(0 #x55) (fixed-8.8 1/3)))
(assert (equalp '(#xff #xff) (fixed-8.8 -1)))
(assert (equalp '(24 0) (fixed-8.8 24.0)))
(assert (equalp '(#xff #xab) (fixed-8.8 -1.333)))

(defun signed-area-triangle (ax ay bx by cx cy)
  (- (/ (+ (* ax (- by cy))
           (* bx (- cy ay))
           (* cx (- ay by)))
        2)))

(defun compile-obj (source-out-pathname object-pathname)
  "Compile into SOURCE-OUT-PATHNAME from OBJECT-PATHNAME"
  (let (*max-3d-vertices*
        *max-3d-triangles*
        *max-3d-edges*
        (moniker (cl-change-case:pascal-case (pathname-name source-out-pathname)))
        (id (parse-integer source-out-pathname
                           :start (position-if #'digit-char-p source-out-pathname)
                           :junk-allowed t)))
    (read-3d-limits)
    (format *trace-output* "~&Writing ~a from 3D object ~a for track “~a” (~d)…"
            source-out-pathname object-pathname moniker id)
    (with-input-from-file (object-in object-pathname)
      (with-output-to-file (source-out source-out-pathname :if-exists :supersede)
        (format source-out ";;; Generated file (do not bother editing), source: ~s"
                (enough-namestring object-pathname))
        (let ((vertices (make-array (list 0) :adjustable t :fill-pointer 0))
              (vertex-id-mapping (make-hash-table))
              (triangles (list)))
          (labels ((add-vertex (coords index)
                     (assert (< (elt coords 1) 0.0001) (coords)
                             "Y mantissa must be zero, not ~:d" (elt coords 1))
                     (unless (< (1+ (length vertices)) *max-3d-vertices*)
                       (error "3D object contains more than the ~:d vertices allowed" *max-3d-vertices*))
                     (let ((x-z (list (elt coords 0) (elt coords 2))))
                       (if-let (i (position x-z vertices :test #'equalp))
                         (setf (gethash index vertex-id-mapping) i)
                         (prog1
                             (setf (gethash index vertex-id-mapping) (fill-pointer vertices))
                           (vector-push-extend x-z vertices)))))
                   (ensure-triangle-ccw (vertex-ids)
                     (let ((a (elt vertices (gethash (1- (elt vertex-ids 0)) vertex-id-mapping)))
                           (b (elt vertices (gethash (1- (elt vertex-ids 1)) vertex-id-mapping)))
                           (c (elt vertices (gethash (1- (elt vertex-ids 2)) vertex-id-mapping))))
                       (if (plusp (signed-area-triangle (elt a 0) (elt a 1)
                                                        (elt b 0) (elt b 1)
                                                        (elt c 0) (elt c 1)))
                           vertex-ids
                           (reverse vertex-ids))))
                   (add-triangle (tri-vertices)
                     (assert (= 3 (length tri-vertices)))
                     (unless (< (1+ (length triangles)) *max-3d-triangles*)
                       (error "3D object contains more than the ~:d triangles allowed" *max-3d-triangles*))
                     (if (= 3 (length (remove-duplicates tri-vertices)))
                         (appendf triangles
                                  (cons (mapcar (lambda (index)
                                                  (gethash index vertex-id-mapping))
                                                (mapcar #'1-
                                                        (ensure-triangle-ccw tri-vertices)))
                                        nil))
                         (warn "Ignoring defective triangle between vertices: (~{~d, ~d, ~d~})"
                               tri-vertices))))
            (loop for line = (read-line object-in nil nil)
                  while line
                  do (cond ((zerop (length line)) nil)
                           ((char= (char line 0) #\#)
                            (format source-out "~&;;; ~a" line))
                           ((and (char= (char line 0) #\o)
                                 (char= (char line 1) #\Space))
                            (format source-out "~&;;; Object: ~a" (subseq line 2)))
                           ((and (char= (char line 0) #\v)
                                 (char= (char line 1) #\Space))
                            (add-vertex (mapcar #'parse-number
                                                (subseq (split-sequence #\Space line
                                                                        :remove-empty-subseqs t)
                                                        1 4))
                                        (hash-table-count vertex-id-mapping)))
                           ((and (char= (char line 0) #\f)
                                 (char= (char line 1) #\Space))
                            (add-triangle (mapcar (lambda (vert)
                                                    (parse-integer vert :end (position #\/ vert)))
                                                  (rest (split-sequence #\Space line
                                                                        :remove-empty-subseqs t)))))
                           #+ () (t (warn "Ignored OBJ line: ~s" line)))
                  finally (progn
                            (setf vertices vertices
                                  triangles triangles)
                            (format source-out
                                    "
;;; 3D data follows
~a:
 .block
ID:~10t.byte ~d
VertexCookie: .byte $3, $d
VertexCount: .byte ~d ; from ~d original vertices
Vertices:~{~{~%~10t.byte ~{$~2,'0x, $~2,'0x~^,   ~}~20t; (~{~7f~^, ~})~}~}

TriangleCookie: .byte $3, $d
TriangleCount: .byte ~d
Triangles:~{~%~10t.byte ~{~d, ~d, ~d~}~}

EndCookie: .byte $2, $d
 .bend
"
                                    (cl-change-case:pascal-case moniker)
                                    id
                                    (length vertices) (hash-table-count vertex-id-mapping)
                                    (mapcar (lambda (vertex)
                                              (destructuring-bind (x z) vertex
                                                (list (reduce
                                                       (curry #'concatenate 'list)
                                                       (list (fixed-8.8 x :note (list :x vertex))
                                                             (fixed-8.8 z :note (list :z vertex))))
                                                      (list x z))))
                                            (coerce vertices 'list))
                                    (length triangles)
                                    triangles)
                            (format *trace-output* " … done.~2%")))))))))
