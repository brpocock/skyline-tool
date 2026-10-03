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
   #:bugzilla-client #:github-client #:gitlab-client #:redmine-client
   #:make-github-client #:make-gitlab-client #:make-bugzilla-client #:make-redmine-client
   #:default-issue-tracker #:with-issue-tracker))

(in-package :skyline-tool.issue-tracking)

;; Issue Data Model
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

;; Issue Tracker Backend Protocol
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

;; HTTP Client Base Class
(defclass http-issue-tracker (issue-tracker-backend)
  ((auth-token :initarg :auth-token :accessor auth-token)
   (username :initarg :username :accessor username)
   (password :initarg :password :accessor password)))

(defmethod initialize-instance :after ((backend http-issue-tracker) &key)
  (unless (auth-token backend)
    (load-or-prompt-credentials backend)))

(defun load-or-prompt-credentials (backend)
  "Load credentials from config or prompt for new ones"
  (setf (auth-token backend) (get-pref'(:issue-tracker :token))
        (username backend) (get-pref '(:issue-tracker :username))))

;; Specific Clients
;; Specific clients (bugzilla-client, github-client, gitlab-client, redmine-client)
;; are defined in their respective files under src/issue-tracking/clients/.

;; Factory Functions
(defun make-github-client (&key repo-owner repo-name auth-token)
  "Create a GitHub issue tracker client."
  (make-instance 'github-client
                 :name "GitHub"
                 :url (format nil "https://github.com/~a/~a" repo-owner repo-name)
                 :repo-owner repo-owner
                 :repo-name repo-name
                 :auth-token auth-token))

(defun make-gitlab-client (&key (api-endpoint "https://gitlab.com/api/v4")
                           (project-id "Phantasia")
                           (private-token ""))
  "Create a GitLab issue tracker client with configurable endpoint and project"
  (make-instance 'gitlab-client
                 :name "GitLab"
                 :url api-endpoint
                 :api-endpoint api-endpoint
                 :project-id project-id
                 :private-token private-token))

(defun make-bugzilla-client (&key login password api-endpoint)
  "Create a Bugzilla issue tracker client."
  (make-instance 'bugzilla-client
                 :name "Bugzilla"
                 :url api-endpoint
                 :login login
                 :password password))

(defun make-redmine-client (&key api-endpoint auth-token)
  "Create a Redmine issue tracker client."
  (make-instance 'redmine-client
                 :name "Redmine"
                 :url api-endpoint
                 :auth-token auth-token))

;; Convenience Macros
(defmacro with-issue-tracker ((var backend) &body body)
  "Execute BODY with VAR bound to issue tracker BACKEND"
  `(let ((,var ,backend))
     (unless (typep ,var 'issue-tracker-backend)
       (error "Invalid issue tracker backend"))
     ,@body))
