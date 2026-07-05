;;; src/issue-tracking/package.lisp
;;; Issue tracking system for Skyline-Tool

(defpackage :skyline-tool.issue-tracking
  (:use :cl :alexandria :serapeum)
  (:import-from :uiop #:run-program #:directory-exists-p)
  (:export
   #:issue-id #:issue-title #:issue-status #:issue-priority #:issue-assignee
   #:issue-labels #:issue-created #:issue-updated
   #:make-issue #:create-issue #:update-issue #:close-issue #:list-issues
   #:search-issues #:get-issue-by-id
   #:bugzilla-client #:github-client #:gitlab-client
   #:with-issue-tracker))

(in-package :skyline-tool.issue-tracking)

;;-----------------------------------------------------------------------------
;; Issue Data Model
;;-----------------------------------------------------------------------------
(defclass issue ()
  ((id :reader issue-id :initarg :id)
   (title :reader issue-title :initarg :title)
   (status :reader issue-status :initarg :status :initform :open)
   (priority :reader issue-priority :initarg :priority :initform :medium)
   (assignee :reader issue-assignee :initarg :assignee :initform nil)
   (labels :reader issue-labels :initarg :labels :initform '())
   (created :reader issue-created :initarg :created :initform (get-universal-time))
   (updated :reader issue-updated :initarg :updated :initform (get-universal-time))))

(defun make-issue (&key id title status priority assignee labels created updated)
  (make-instance 'issue
                 :id id :title title :status status :priority priority
                 :assignee assignee :labels labels :created created :updated updated))

;;-----------------------------------------------------------------------------
;; Issue Tracker Backend Protocol
;;-----------------------------------------------------------------------------
(defclass issue-tracker-backend ()
  ((name :reader backend-name :initarg :name)
   (url :reader backend-url :initarg :url)))

(defgeneric create-issue (backend issue-data)
  (:documentation "Create a new issue in BACKEND with ISSUE-DATA plist"))

(defgeneric update-issue (backend issue-id update-data)
  (:documentation "Update ISSUE-ID in BACKEND with UPDATE-DATA plist"))

(defgeneric close-issue (backend issue-id)
  (:documentation "Close ISSUE-ID in BACKEND"))

(defgeneric list-issues (backend &key status labels assignee)
  (:documentation "List issues from BACKEND, optionally filtered"))

(defgeneric get-issue-by-id (backend issue-id)
  (:documentation "Retrieve a single issue by ID from BACKEND"))

(defgeneric search-issues (backend query)
  (:documentation "Search issues in BACKEND by QUERY string"))

;;-----------------------------------------------------------------------------
;; HTTP Client Base Class
;;-----------------------------------------------------------------------------
(defclass http-issue-tracker (issue-tracker-backend)
  ((auth-token :initarg :auth-token :accessor auth-token)
   (username :initarg :username :accessor username)
   (password :initarg :password :accessor password)))

(defmethod initialize-instance :after ((backend http-issue-tracker) &key)
  (unless (auth-token backend)
    (load-or-prompt-credentials backend)))

(defun load-or-prompt-credentials (backend)
  "Load credentials from config or prompt for new ones"
  (let ((config-path (merge-pathnames ".config/skyline-issue-tracking/" (user-homedir-pathname))))
    (when (probe-file config-path)
      (let ((creds (read-config-file config-path)))
        (setf (auth-token backend) (getf creds :token)
              (username backend) (getf creds :username))))))

;;-----------------------------------------------------------------------------
;; Bugzilla Client
;;-----------------------------------------------------------------------------
(defclass bugzilla-client (http-issue-tracker)
  ((api-endpoint :initform "https://bugzilla.example.com/rest.cgi")))

(defmethod create-issue ((backend bugzilla-client) issue-data)
  (let ((request `(("product" . ,(getf issue-data :product))
                   ("component" . ,(getf issue-data :component))
                   ("summary" . ,(getf issue-data :title))
                   ("description" . ,(getf issue-data :description))
                   ("priority" . ,(getf issue-data :priority)))))
    (post-json (backend-url backend) request)))

;;-----------------------------------------------------------------------------
;; GitHub Client
;;-----------------------------------------------------------------------------
(defclass github-client (http-issue-tracker)
  ((api-endpoint :initform "https://api.github.com")))

(defmethod create-issue ((backend github-client) issue-data)
  (let ((request `((:title . ,(getf issue-data :title))
                   (:body . ,(getf issue-data :description))
                   (:labels . ,(getf issue-data :labels)))))
    (gh-post (backend-url backend) request)))

;;-----------------------------------------------------------------------------
;; GitLab Client
;;-----------------------------------------------------------------------------
(defclass gitlab-client (http-issue-tracker)
  ((api-endpoint :initform "https://gitlab.com/api/v4")))

(defmethod create-issue ((backend gitlab-client) issue-data)
  (let ((request `((:title . ,(getf issue-data :title))
                   (:description . ,(getf issue-data :description))
                   (:labels . ,(getf issue-data :labels)))))
    (gl-post (backend-url backend) request)))

;;-----------------------------------------------------------------------------
;; Convenience Macros
;;-----------------------------------------------------------------------------
(defmacro with-issue-tracker ((var backend) &body body)
  "Execute BODY with VAR bound to issue tracker BACKEND"
  `(let ((,var ,backend))
     (unless (typep ,var 'issue-tracker-backend)
       (error "Invalid issue tracker backend"))
     ,@body))