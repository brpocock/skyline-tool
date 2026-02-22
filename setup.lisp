(cl:in-package :cl-user)
(require 'asdf)
(format t "~&Skyline-Tool: Setup script… ")
(finish-output)
(unless (find-package :quicklisp)
  (format t "~&Loading Quicklisp… ")
  (handler-bind
      ((error (lambda (c)
                (format *error-output*
                        "~2%Error of type ~:(~a~):~%~a

Perhaps Quicklisp  is not installed,  or in installed in  a non-standard
place? Visit https://beta.quicklisp.com/ for installation instructions.~%"
                        (type-of c) c)
                (finish-output))))
    (load (merge-pathnames (make-pathname
                            :directory '(:relative "quicklisp")
                            :name "setup" :type "lisp")
                           (user-homedir-pathname)))))

(defmacro with-casual-handlers (&body body)
  `(handler-bind
       ((serious-condition
          (lambda (c)
            (print c *error-output*)
            (cond ((find-restart 'continue)
                   (princ " … attempting Continue restart … " *error-output*)
                   (finish-output *error-output*)
                   (invoke-restart 'continue))
                  ((find-restart 'accept)
                   (princ " … attempting Accept restart … " *error-output*)
                   (finish-output *error-output*)
                   (invoke-restart 'accept))))))
     ,@body))

;; Load skyline-tool system
(asdf:load-asd (merge-pathnames (make-pathname :directory '(:relative "eightbol")
                                               :name "eightbol"
                                               :type "asd")
                                *load-pathname*)
               :name :eightbol)
(asdf:load-asd (merge-pathnames (make-pathname :name "skyline-tool"
                                               :type "asd")
                                *load-pathname*)
               :name :skyline-tool)
(pushnew (asdf:system-relative-pathname :skyline-tool #p"./lib/") ql:*local-project-directories*)

;; Load eightbol and skyline-tool with handler to trap CLIM name-conflict
;; (INVOKE-WITH-PRISTINE-VIEWPORT already names...) and invoke CONTINUE restart.
(format t "~&Loading EIGHTBOL and Skyline-Tool… ")
(finish-output)
(handler-bind ((program-error
                (lambda (c)
                  (let ((msg (princ-to-string c))
                        (r (find-restart 'continue c)))
                    (when (and r (or (search "INVOKE-WITH-PRISTINE-VIEWPORT" msg)
                                     (search "already names" msg)))
                      (format *error-output*
                              "~&CLIM name conflict (~a): invoking CONTINUE restart~%"
                              (type-of c))
                      (finish-output *error-output*)
                      (invoke-restart r))))))
  (asdf:load-system :eightbol)
  (asdf:load-system :skyline-tool))
(load (merge-pathnames (make-pathname :name "compile" :type "lisp") *load-pathname*))
(format t "… done.~2%")
(finish-output)

#+ ()
(progn
  (format t "~&Quickloading Skyline-Tool System … ")
  (finish-output)
  (handler-case
      (progn (funcall (intern "QUICKLOAD" (find-package :quicklisp)) :eightbol)
             #+ () (funcall (intern "QUICKLOAD" (find-package :quicklisp)) :skyline-tool))
    (name-conflict (e) (error e)))
  (format t "… done with Quickload.~2%")
  ;; These are missing, but apparently also no-op works
  (defmethod clim-internals::note-output-record-got-sheet ((drei drei:drei-area) (pane clim:pane)))
  (defmethod clim-internals::note-output-record-lost-sheet ((drei drei:drei-area) (pane clim:pane)))
  (finish-output))
