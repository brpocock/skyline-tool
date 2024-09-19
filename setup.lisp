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

(format t "~&Loading latest ZPB-TTF …")
(asdf:load-asd (merge-pathnames (make-pathname :name "skyline-tool"
                                               :type "asd")
                                (or *compile-file-pathname*
                                    *load-pathname*))
               :name :skyline-tool)
(pushnew (asdf:system-relative-pathname :skyline-tool #p"./lib/") ql:*local-project-directories*)
(funcall (intern "QUICKLOAD" (find-package :quicklisp)) :zpb-ttf)

(format t "~&Quickloading Skyline-Tool System … ")
(finish-output)
(funcall (intern "QUICKLOAD" (find-package :quicklisp)) :cl-plumbing)
#+ () (funcall (intern "QUICKLOAD" (find-package :quicklisp)) :clim)
#+ () (load (merge-pathnames (make-pathname :directory (list :relative)
			              :name "clim-simple-interactor" :type "lisp")
                             (or *compile-file-pathname*
                                 *load-pathname*)))
(funcall (intern "QUICKLOAD" (find-package :quicklisp)) :skyline-tool)
(format t "… done with Quickload.~2%")

;; These are missing, but apparently also no-op works
#+ () (defmethod clim-internals::note-output-record-got-sheet ((drei drei:drei-area) (pane clim:pane)))
#+ () (defmethod clim-internals::note-output-record-lost-sheet ((drei drei:drei-area) (pane clim:pane)))

(finish-output)
