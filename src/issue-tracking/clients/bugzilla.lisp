;;; src/issue-tracking/clients/bugzilla.lisp
;;; Bugzilla API client for issue tracking

(in-package :skyline-tool.issue-tracking)

(defclass bugzilla-client (http-issue-tracker)
  ((api-endpoint :initform "https://bugzilla.example.com/rest.cgi"
                 :initarg :api-endpoint :accessor bugzilla-api-endpoint)
   (login :initform "nobody@example.com"
          :initarg :login :accessor bugzilla-login)
   (password :initform "secret"
             :initarg :password :accessor bugzilla-password)))

;; Helper for Bugzilla API calls
(defun bugzilla-api-call (client endpoint &key method data)
  "Make an API call to Bugzilla"
  (let ((url (format nil "~a/~a" (backend-url client) endpoint)))
    (ecase method
      (:GET (drakma:http-request url :method :GET
                                     :basic-authorization (format nil "~a~a" (bugzilla-login client)
                                                                  (bugzilla-password client))))
      (:POST (drakma:http-request url :method :POST
                                      :basic-authorization (format nil "~a~a" (bugzilla-login client)
                                                                   (bugzilla-password client))
                                      :content (json:encode-json-to-string data)))
      (:PUT (drakma:http-request url :method :PUT
                                     :basic-authorization (format nil "~a~a" (bugzilla-login client)
                                                                  (bugzilla-password client))
                                     :content (json:encode-json-to-string data))))))

;; Create issue

(defmethod create-issue ((backend bugzilla-client) issue-data)
  (let ((response (bugzilla-api-call backend "/create" :method :POST :data issue-data)))
    (parse-bugzilla-issue-response response)))

  ;; Update issue
  
(defmethod update-issue ((backend bugzilla-client) issue-id update-data)
  (bugzilla-api-call backend (format nil "/update/~a" issue-id) :method :PUT :data update-data))

;; Close issue

(defmethod close-issue ((backend bugzilla-client) issue-id)
  (bugzilla-api-call backend (format nil "/close/~a" issue-id) :method :PUT))

;; List issues

(defmethod list-issues ((backend bugzilla-client) &key status labels assignee)
  (let ((params (append (when status (list (list :status status)))
                        (when labels (list (list :cf_state labels)))
                        (when assignee (list (list :assigned_to assignee))))))
    (let ((response (bugzilla-api-call backend "/query" :method :GET
                                                        :data (list :params params))))
      (mapcar #'parse-bugzilla-issue-response response))))

  ;; Get single issue
  
(defmethod get-issue-by-id ((backend bugzilla-client) issue-id)
  (let ((response (bugzilla-api-call backend (format nil "/show/~a" issue-id) :method :GET)))
    (parse-bugzilla-issue-response response)))

  ;; Search issues
  
(defmethod search-issues ((backend bugzilla-client) query)
  (bugzilla-api-call backend "/productive_search" :method :GET :data `(("query_string" . ,query)))) 

;; Response parsing

(defun parse-bugzilla-issue-response (response)
  "Parse Bugzilla API response into issue object"
  (list :id (getf response :bug_id)
        :title (getf response :short_desc)
        :status (getf response :status)
        :priority (getf response :priority)
        :assignee (getf response :assigned_to)
        :labels (getf response :cf_type)
        :created (local-time:parse-rfc3339-timestring (getf response :creation_ts))
        :updated (local-time:parse-rfc3339-timestring (getf response :last_updated_utc)))) 
