;;; src/issue-tracking/clients/gitlab.lisp
;;; GitLab API client for issue tracking

(in-package :skyline-tool.issue-tracking)

(defclass gitlab-client (http-issue-tracker)
  ((api-endpoint :initform "https://gitlab.com/api/v4")
   (project-id :initarg :project-id :accessor project-id)
   (private-token :initarg :private-token :accessor private-token)))

;; Helper for GitLab API calls
(defun gitlab-api-call (client endpoint &key method data)
  "Make an API call to GitLab"
  (let ((url (format nil "~a/projects/~a/~a" (backend-url client) (project-id client) endpoint)))
    (ecase method
      (:GET (drakma:http-request url :method :GET :authorization (format nil "Bearer ~a" (private-token client))))
      (:POST (drakma:http-request url :method :POST :authorization (format nil "Bearer ~a" (private-token client))
                                  :content (json:encode-json-to-string data)))
      (:PUT (drakma:http-request url :method :PUT :authorization (format nil "Bearer ~a" (private-token client))
                                 :content (json:encode-json-to-string data))))))

;; Create issue
(defmethod create-issue ((backend gitlab-client) issue-data)
  (let ((response (gitlab-api-call backend "/issues" :method :POST :data issue-data)))
    (parse-gitlab-issue-response response)))

;; Update issue
(defmethod update-issue ((backend gitlab-client) issue-id update-data)
  (gitlab-api-call backend (format nil "/issues/~a" issue-id) :method :PUT :data update-data))

;; Close issue
(defmethod close-issue ((backend gitlab-client) issue-id)
  (update-issue backend issue-id `(:state_event . "close")))

;; List issues
(defmethod list-issues ((backend gitlab-client) &key status labels assignee)
  (let ((params (append (when status `(("state" . ,status)))
                        (when labels `(("labels" . ,(format nil "~{~a~^,~}" labels))))
                        (when assignee `(("assignee_id" . ,assignee))))))
    (let ((response (gitlab-api-call backend "/issues" :method :GET :data `(("params" . ,params)))))
      (mapcar #'parse-gitlab-issue-response response))))

;; Get single issue
(defmethod get-issue-by-id ((backend gitlab-client) issue-id)
  (let ((response (gitlab-api-call backend (format nil "/issues/~a" issue-id) :method :GET)))
    (parse-gitlab-issue-response response)))

;; Search issues
(defmethod search-issues ((backend gitlab-client) query)
  (gitlab-api-call backend "/issues" :method :GET :data `(("search" . ,query))))

;; Response parsing
(defun parse-gitlab-issue-response (response)
  "Parse GitLab API response into issue object"
  (make-instance 'issue
                 :id (getf response :iid)
                 :title (getf response :title)
                 :status (getf response :state)
                 :priority (getf response :priority)
                 :assignee (getf (getf response :assignee) :username)
                 :labels (getf response :labels)
                 :created (parse-iso8601 (getf response :created_at))
                 :updated (parse-iso8601 (getf response :updated_at))))