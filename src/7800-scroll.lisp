(in-package :skyline-tool)

(defstruct (scroll-state
            (:constructor make-scroll-state
             (top-row top-line left-column left-pixel
              &key
              (map-rows 12)
              (map-height 64)
              (map-width 40)
              (viewport-columns 21))))
  (top-row 0 :type (integer 0))
  (top-line 0 :type (integer 0 15))
  (left-column 0 :type (integer 0))
  (left-pixel 0 :type (integer 0 7))
  (map-rows 12 :type (integer 1))
  (map-height 64 :type (integer 1))
  (map-width 40 :type (integer 1))
  (viewport-columns 21 :type (integer 1)))

(defun scroll-state-copy (state)
  (copy-scroll-state state))

(defun scroll-north (state)
  "ScrollMapUp: camera moves north; fine scroll decrements MapTopLine; coarse decrements MapTopRow."
  (when (zerop (scroll-state-top-row state))
    (return-from scroll-north (values state nil)))
  (let ((copy (scroll-state-copy state)))
    (if (plusp (scroll-state-top-line copy))
        (decf (scroll-state-top-line copy))
        (progn
          (setf (scroll-state-top-line copy) 15)
          (decf (scroll-state-top-row copy))))
    (values copy t)))

(defun scroll-south (state)
  "ScrollMapDown: camera moves south; fine scroll increments MapTopLine; coarse increments MapTopRow."
  (when (>= (+ (scroll-state-top-row state) (scroll-state-map-rows state))
            (scroll-state-map-height state))
    (return-from scroll-south (values state nil)))
  (let ((copy (scroll-state-copy state)))
    (if (< (scroll-state-top-line copy) 15)
        (incf (scroll-state-top-line copy))
        (progn
          (setf (scroll-state-top-line copy) 0)
          (incf (scroll-state-top-row copy))))
    (values copy t)))

(defun scroll-west (state)
  "ScrollMapLeft: camera moves west; fine scroll decrements MapLeftPixel; coarse decrements MapLeftColumn."
  (when (and (zerop (scroll-state-left-column state))
             (zerop (scroll-state-left-pixel state)))
    (return-from scroll-west (values state nil)))
  (let ((copy (scroll-state-copy state)))
    (decf (scroll-state-left-pixel copy))
    (when (< (scroll-state-left-pixel copy) 0)
      (setf (scroll-state-left-pixel copy) 7)
      (decf (scroll-state-left-column copy)))
    (values copy t)))

(defun scroll-east (state)
  "ScrollMapRight: camera moves east; fine scroll increments MapLeftPixel; coarse increments MapLeftColumn."
  (when (>= (scroll-state-left-column state)
            (- (scroll-state-map-width state)
               (scroll-state-viewport-columns state)))
    (return-from scroll-east (values state nil)))
  (let ((copy (scroll-state-copy state)))
    (incf (scroll-state-left-pixel copy))
    (when (>= (scroll-state-left-pixel copy) 8)
      (setf (scroll-state-left-pixel copy) 0)
      (incf (scroll-state-left-column copy)))
    (values copy t)))

(defun make-tile-row-headers (left-pixel &key (tile-count 22))
  "Build direct-draw tile headers matching BuildMapRow X placement."
  (let ((next-x (- left-pixel 8)))
    (coerce
     (loop for i below tile-count
           collect (let ((x next-x))
                     (setf next-x (+ next-x 8))
                     (list 0 0 #x19 x)))
     'vector)))

(defun header-xpos (header)
  (fourth header))

(defun dll-holey-byte (top-line)
  (logior (- 15 top-line) #x40))

(defun fine-scroll-headers-west (headers)
  (map 'vector (lambda (header)
                 (list (first header) (second header) (third header)
                       (1+ (fourth header))))
       headers))

(defun fine-scroll-headers-east (headers)
  (map 'vector (lambda (header)
                 (list (first header) (second header) (third header)
                       (1- (fourth header))))
       headers))
