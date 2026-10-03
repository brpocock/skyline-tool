(in-package :skyline-tool)

;; Base91 encoding implementation
;; Based on base91 algorithm by Joachim Henke
;; Provides ~23% overhead vs base64's 33%

(alexandria:define-constant +base91-chars+
    (coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()~-_=+[{|]};:,<.>/? "
            'string)
  :test #'string-equal
  :documentation "91 printable ASCII characters for base91 encoding")

(alexandria:define-constant +base91-decode-table+
  (let ((table (make-array 256 :element-type 'integer :initial-element -1)))
    (loop for i from 0 below 91
          do (setf (aref table (char-code (char +base91-chars+ i))) i))
    table)
  :test #'equalp
  :documentation "Decode table for base91 characters")

(defun encode-base91 (bytes)
  "Encode BYTES (vector of (unsigned-byte 8)) as base91 string with length prefix.
Returns encoded string. Uses 4-byte big-endian length prefix for perfect round-trips."
  (declare (type (vector (unsigned-byte 8)) bytes))
  (let* ((len (length bytes))
         (total-len (+ 4 len))
         (prefixed-bytes (make-array (+ 4 len) :element-type '(unsigned-byte 8)))
         (result (make-array (+ 4 (* 2 (ceiling (/ (+ 4 len) 13))))
                             :element-type 'character :fill-pointer 0))
         (v 0) (n 0))
    (declare (type integer v n))
    ;; Write 4-byte big-endian length prefix
    (setf (aref prefixed-bytes 0) (ldb (byte 8 24) len)
          (aref prefixed-bytes 1) (ldb (byte 8 16) len)
          (aref prefixed-bytes 2) (ldb (byte 8 8) len)
          (aref prefixed-bytes 3) (ldb (byte 8 0) len))
    (loop for i from 4 below (+ 4 len)
          do (setf (aref prefixed-bytes i) (aref bytes (- i 4))))
    (let ((v 0) (n 0))
      (declare (type integer v n))
      (loop for i from 0 below (+ 4 len)
            for b = (aref prefixed-bytes i)
            do (setf v (logior v (ash b n))
                     n (+ n 8))
           (when (> n 13)
             (let ((ev (logand v 8191)))
               (if (> ev 88)
                   (let ((ev14 (logand v 16383)))
                     ;; 14-bit value must be < 91*91 = 8281 to fit in two base91 digits
                     (if (< ev14 8281)
                         (progn
                           (setf ev ev14)
                           (setf v (ash v -14))
                           (decf n 14))
                         (progn
                           (setf v (ash v -13))
                           (decf n 13))))
                     (progn
                       (setf v (ash v -13))
                       (decf n 13)))
               (vector-push-extend (char +base91-chars+ (mod ev 91)) result)
               (vector-push-extend (char +base91-chars+ (floor ev 91)) result))))
     (when (> n 0)
       (vector-push-extend (char +base91-chars+ (mod v 91)) result)
       (when (or (> n 7) (> v 90))
         (vector-push-extend (char +base91-chars+ (floor v 91)) result)))
result)))

(defun decode-base91 (string)
  "Decode base91 STRING to vector of (unsigned-byte 8).
Returns decoded byte vector (without length prefix)."
  (declare (type string string))
  (let* ((len (length string))
         (result (make-array len :element-type '(unsigned-byte 8) :fill-pointer 0))
         (v -1) (n 0) (b 0))
    (declare (type integer v n b))
    (loop for i from 0 below len
          for ch = (char string i)
          for val = (aref +base91-decode-table+ (char-code ch))
          when (>= val 0)
            do (if (= v -1)
                   (setf v val)
                   (progn
                     (setf v (+ v (* val 91))
                           b (logior b (ash v n))
                           n (+ n (if (> (logand v 8191) 88) 13 14)))
                     (loop while (>= n 8)
                           do (vector-push-extend (logand b 255) result)
                              (setf b (ash b -8)
                                    n (- n 8)))
                     (setf v -1))))
    (when (and (/= v -1) (> n 0))
      (vector-push-extend (logand (logior b (ash v n)) 255) result))
    ;; Remove 4-byte length prefix and return only the data
    (when (>= (fill-pointer result) 4)
      (let ((data-len (+ (ash (aref result 0) 24)
                         (ash (aref result 1) 16)
                         (ash (aref result 2) 8)
                         (aref result 3))))
        (when (<= data-len (- (fill-pointer result) 4))
          (setf (fill-pointer result) (+ 4 data-len))
          (adjust-array result (list (+ 4 data-len)) :fill-pointer (+ 4 data-len))
          (replace result result :start1 0 :start2 4 :end2 (+ 4 data-len))
          (setf (fill-pointer result) data-len)
          (adjust-array result (list data-len) :fill-pointer data-len))))
    result))

(defun encode-file-to-base91 (path)
  "Read file at PATH and return base91-encoded string, or NIL if file not found.
Base91 provides ~23% overhead vs base64's 33%."
  (when (and path (probe-file path))
    (with-open-file (stream path :element-type '(unsigned-byte 8))
      (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
        (read-sequence bytes stream)
        (encode-base91 bytes)))))
