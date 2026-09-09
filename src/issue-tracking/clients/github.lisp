;;; src/issue-tracking/clients/github.lisp
;;; GitHub API client for issue tracking

(in-package :skyline-tool.issue-tracking)

(defclass github-client (http-issue-tracker)
  ((api-endpoint :initform "https://api.github.com")
   (repo-owner :initarg :repo-owner :accessor repo-owner)
   (repo-name :initarg :repo-name :accessor repo-name)))

;; Helper for GitHub API calls
(defun github-api-call (client endpoint &key method data)
  "Make an API call to GitHub"
  (let ((url (format nil "~a/~a/~a" 
                     (backend-url client)
                     (repo-owner client)
                     (repo-name client)
                     endpoint)))
    (ecase method
      (:GET (drakma:http-request url :authorization (format nil "token ~a" (auth-token client))))
      (:POST (drakma:http-request url 
                                  :method :POST
                                  :content-type "application/json"
                                  :authorization (format nil "token ~a" (auth-token client))
                                  :content (json:encode-json-to-string data)))
      (:PATCH (drakma:http-request url 
                                   :method :PATCH
                                   :content-type "application/json"
                                   :authorization (format nil "token ~a" (auth-token client))
                                   :content (json:encode-json-to-string data))))))

;; Create issue
(defmethod create-issue ((backend github-client) issue-data)
  (let ((response (github-api-call backend "/issues" 
                                  :method :POST
                                  :data `((:title . ,(getf issue-data :title))
                                          (:body . ,(getf issue-data :description))
                                          (:labels . ,(getf issue-data :labels))))))
    (parse-github-issue-response response)))

;; Update issue
(defmethod update-issue ((backend github-client) issue-id update-data)
  (github-api-call backend (format nil "/issues/~a" issue-id)
                  :method :PATCH
                  :data update-data))

;; Close issue
(defmethod close-issue ((backend github-client) issue-id)
  (update-issue backend issue-id `(:state . "closed")))

;; List issues
(defmethod list-issues ((backend github-client) &key status labels assignee)
  (let ((params (append (when status `(("state" . ,status)))
                        (when labels `(("labels" . ,(format nil "~{~a~^,~}" labels))))
                        (when assignee `(("assignee" . ,assignee))))))
    (let ((response (github-api-call backend "/issues" :method :GET)))
      (mapcar #'parse-github-issue-response response))))

;; Get single issue
(defmethod get-issue-by-id ((backend github-client) issue-id)
  (let ((response (github-api-call backend (format nil "/issues/~a" issue-id) :method :GET)))
    (parse-github-issue-response response)))

;; Search issues
(defmethod search-issues ((backend github-client) query)
  (let ((response (github-api-call backend (format nil "/search/issues?q=~a" query) :method :GET)))
    (mapcar #'parse-github-issue-response (getf response :items))))

;; Parse GitHub issue response
(defun parse-github-issue-response (response)
  "Parse GitHub API response into issue object"
  (make-instance 'issue
                 :id (getf response :number)
                 :title (getf response :title)
                 :status (getf response :state)
                 :priority (getf response :priority)
                 :assignee (getf (getf response :assignee) :login)
                 :labels (mapcar (lambda (label) (getf label :name)) (getf response :labels))
                 :created (parse-iso8601 (getf response :created_at))
                 :updated (parse-iso8601 (getf response :updated_at))))