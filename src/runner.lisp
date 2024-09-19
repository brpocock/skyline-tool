(in-package :skyline-tool)

(defun load-dump-into-mem (&optional (dump-file #p"/tmp/dump"))
  (let ((mem (make-array (expt 2 16) :element-type '(unsigned-byte 8))))
    (with-input-from-file (dump dump-file :element-type '(unsigned-byte 8))
      (loop for byte = (read-byte dump nil nil)
            for i from 0 below #x10000
            while byte
            do (setf (aref mem i) byte)))
    mem))

(defvar *run-script-frame* nil)

#+mcclim
(clim:define-application-frame run-script-frame ()
  ((%decal-index :initform 0 :accessor decal-index :initarg :index))
  (:panes (script-list-pane :application :height 700 :width 450
                                         :display-function 'display-script-list)
          (interactor :interactor :height 125 :width 450))
  (:layouts (default (clim:vertically () script-list-pane interactor))))

(defun all-script-names (&key (reloadp nil))
  (when reloadp
    (setf *assets-list* nil
          *asset-ids-seen* nil))
  (sort
   (remove-if-not #'script-asset-p
                  (copy-list (hash-table-keys (read-assets-list))))
   #'string<))

#+mcclim
(clim:define-presentation-type script-name () :inherit-from 'string)

#+mcclim
(clim:define-presentation-method clim:accept
    ((type script-name) stream view &key)
  (values (clim:completing-from-suggestions (stream)
                                            (dolist (script-name (all-script-names))
                                              (clim:suggest (subseq script-name (1+ (position #\/ script-name)))
                                                            script-name)))))

#+mcclim
(clim:define-presentation-method clim:present
    (script-full-name (type script-name) stream view &key)
  (clim:with-text-face (stream (if (or (search "Global/" script-full-name)
                                       (search "Testing" script-full-name))
                                   :bold
                                   :roman))
    (destructuring-bind (area name)
        (split-sequence #\/ (subseq script-full-name (1+ (position #\/ script-full-name))))
      (format stream "~4t~a: “~a”"
              (cl-change-case:title-case area)
              (cl-ppcre:regex-replace "\\bDont\\b"
                                      (cl-change-case:title-case name)
                                      "Don't")))))

#+mcclim
(defun run-script-in-playtest (script-full-name)
  (clim-sys:make-process (lambda ()
                           (uiop:run-program
                            (list "gnome-terminal"
                                  "--window"
                                  "--geometry" "80x50"
                                  "--hide-menubar"
                                  "--title" (format nil "Running ~a: ~a"
                                                    (cl-change-case:title-case *game-title*)
                                                    script-full-name)
                                  "--" "bin/playtest" 
                                  (format nil "NEWGAME=~a"
                                          (subseq script-full-name
                                                  (1+ (position #\/ script-full-name)))))))
                         :name (format nil "Running ~a" script-full-name)))

#+mcclim
(define-run-script-frame-command (com-run-script :menu t :name t)
    ((script-full-name 'script-name :gesture :select))
  (run-script script-full-name)
  (when *run-script-frame*
    (clim:frame-exit *run-script-frame*)))

#+mcclim
(define-run-script-frame-command (com-edit-script :menu t :name t) ((script-full-name 'script-name))
  (if swank::*emacs-connection*
      (swank:ed-in-emacs (format nil "Source/~a.fountain" script-full-name))
      (clim-sys:make-process (lambda ()
                               (uiop:run-program
                                (list "emacsclient" "-n"
                                      (format nil "Source/~a.fountain" script-full-name))))
                             :name (format nil "Running ~a" script-full-name))))

#+mcclim
(clim:define-presentation-to-command-translator click-to-edit
    (script-name com-edit-script run-script-frame
                 :gesture :edit :menu nil
                 :documentation "Edit this script")
  (script-name)
  (list script-name))

#+mcclim
(defmethod display-script-list (frame (pane clim:pane))
  (clim:with-text-face (pane :bold)
    (format pane "Click a script to run it in playtest mode~2%"))
  (let ((last-area nil))
    (dolist (script-name (all-script-names :reloadp t))
      (terpri pane)
      (let ((area (let ((parts (split-sequence #\/ script-name)))
                    (elt parts (- (length parts) 2)))))
        (unless (string-equal area last-area)
          (clim:with-text-face (pane :bold)
            (clim:with-text-size (pane :large)
              (format pane "~%~a~%" (cl-change-case:title-case area))))
          (setf last-area area)))
      (clim:present script-name 'script-name :stream pane))
    (format pane "~2%")))

#+mcclim
(defun run-script (&optional script-to-run)
  "Choose a script from a menu, and run it"
  (if script-to-run
      (progn
        (when *run-script-frame*
          (clim:frame-exit *run-script-frame*))
        (run-script-in-playtest
         (if (search "Scripts/" script-to-run)
             script-to-run
             (format nil "Scripts/~a" script-to-run))))
      
      (let ((frame (clim:make-application-frame 'run-script-frame)))
        (let ((*run-script-frame* frame))
          (setf (clim:frame-pretty-name frame)
                (format nil "~a: Run Script" (cl-change-case:title-case *game-title*)))
          (clim-sys:make-process (lambda () (clim:run-frame-top-level frame))
                                 :name "Script Runner (launcher)")))))

#+mcclim
(defmethod clim:text-size ((stream swank/gray::slime-output-stream) size &rest _))
#+mcclim
(defmethod clim:stream-vertical-spacing ((stream swank/gray::slime-output-stream)) 1)
#+mcclim
(defmethod clim:stream-cursor-position ((stream swank/gray::slime-output-stream)) 1)
#+mcclim
(defmethod clim:invoke-with-output-recording-options
    ((stream swank/gray::slime-output-stream) continuation _ __)
  (funcall continuation stream))
#+mcclim
(defmethod clim-internals::invoke-with-pristine-viewport
    ((stream swank/gray::slime-output-stream) continuation)
  (funcall continuation stream))
#+mcclim
(defmethod (setf clim::.stream-cursor-position-star.)
    (value _ (stream swank/gray::slime-output-stream)))
#+mcclim
(defmethod clim:sheet-direct-mirror ((stream swank/gray::slime-output-stream)))
#+mcclim
(defmethod clim:sheet-native-transformation ((stream swank/gray::slime-output-stream)))
#+mcclim
(defmethod clim:untransform-region (_ (stream swank/gray::slime-output-stream)))
#+mcclim
(defmethod clim:stream-close-text-output-record ((stream swank/gray::slime-output-stream)))
#+mcclim
(defmethod clim:stream-drawing-p ((stream swank/gray::slime-output-stream)) nil)
#+mcclim
(defmethod clim-internals::sheet-native-region* ((stream swank/gray::slime-output-stream))
  stream)
#+mcclim
(defmethod clim:invoke-with-new-output-record ((stream swank/gray::slime-output-stream)
                                               continuation record-type &rest initargs)
  (funcall continuation stream (apply #'make-instance record-type initargs)))
