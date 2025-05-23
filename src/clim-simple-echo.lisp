(map nil #'ql:quickload '(:clim :clim-lisp :cl-plumbing))

(defpackage clim-simple-echo
  (:use :clim :clim-lisp :clim-extensions)
  (:export #:run-in-simple-echo))

(in-package :clim-simple-echo)

(defclass echo-view (textual-view) ())

(define-presentation-method present :around
  ((object sequence) (type sequence) stream (view echo-view)
                     &key acceptably for-context-type)
  (present object 'expression :stream stream :view view
                              :acceptably acceptably :for-context-type for-context-type))

(define-application-frame simple-echo (standard-application-frame)
  ((pipe :initarg :pipe :reader frame-pipe))
  (:panes (echo-container :application :height 1000 :width 1000
                                       :display-function 'echo-echo))
  (:command-table (simple-echo))
  (:layouts (default echo-container)))

(defun run-in-simple-echo (function &key (width 800)
                                         (height 400)
                                         port
                                         frame-manager
                                         (process-name (format nil "Echo from ~s" function))
                                         (window-title process-name))
  (let* ((fm (or frame-manager (find-frame-manager :port (or port (find-port)))))
         (pipe function)
         (frame (make-application-frame 'simple-echo
                                        :pretty-name window-title
                                        :function function
                                        :frame-manager fm
                                        :pipe pipe
                                        :width width
                                        :height height)))
    (clim-sys:make-process (lambda ()
                             (run-frame-top-level frame))
                           :name process-name)))

(defun echo-echo (frame pane)
  (funcall (frame-pipe frame)))
