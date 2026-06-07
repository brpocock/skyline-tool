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

;; Load dufy before building the system
(ql:quickload :dufy)

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
                   (invoke-restart 'accept)))
	  (error c))))
     ,@body))

;; Load skyline-tool system
(asdf:load-asd (merge-pathnames
	      (make-pathname :directory '(:relative "eightbol")
                               :name "eightbol"
                               :type "asd")
                *load-pathname*))
(asdf:load-asd (merge-pathnames (make-pathname :name "skyline-tool"
                                               :type "asd")
                                *load-pathname*))
(pushnew (asdf:system-relative-pathname :skyline-tool #p"./lib/")
         ql:*local-project-directories*)

;; Load eightbol and skyline-tool with handler to trap CLIM
;; name-conflict (INVOKE-WITH-PRISTINE-VIEWPORT already names...) and
;; invoke CONTINUE restart.
(format t "~&Loading EIGHTBOL and Skyline-Tool… ")
(finish-output)
(handler-bind ((program-error
                 (lambda (c)
                   (let ((msg (princ-to-string c))
                         (r (find-restart 'continue c)))
                     (when (and r (or (search "INVOKE-WITH-PRISTINE-VIEWPORT"
				      msg)
                                      (search "already names" msg)))
                       (format *error-output*
                               "~&CLIM name conflict (~a): ~
invoking CONTINUE restart~%"
                               (type-of c))
                       (finish-output *error-output*)
                       (invoke-restart r)))))
               (uiop/lisp-build:compile-file-error
                 (lambda (c)
                   (format *error-output*
                           "~&Warning: compilation error in dependent system (~a), continuing...~%"
                           c)
                   (finish-output *error-output*)))))

(format t "… done.~2%")
(finish-output)

