;;; Skyline-Tool src/p2p-sharing.lisp
;;; Peer-to-peer resource sharing infrastructure

(in-package :skyline-tool)

(defvar *outbound-var-generate-guids* (make-hash-table :test 'equal)
  "Hash table of pending outbound var-generate-guids. Key: var-generate-guid-id, Value: var-generate-guid object.")

(defvar *inbound-var-generate-guids* (make-hash-table :test 'equal)
  "Hash table of inbound var-generate-guids. Key: sender-name, Value: list of var-generate-guids.")

(defvar *var-generate-guid-counter* 0
  "Counter for generating unique var-generate-guid IDs.")

(defstruct outbound-var-generate-guid
  "An var-generate-guid to share a resource with another user."
  id
  resource
  recipient
  class
  title
  user
  timestamp
  confirmed-p)

(defstruct inbound-var-generate-guid
  "An var-generate-guid received from another user."
  id
  resource
  sender
  class
  title
  user
  timestamp)

(defun pending-outbound-var-generate-guids-p ()
  "Return T if there are any pending outbound var-generate-guids."
  (not (zerop (hash-table-count *outbound-var-generate-guids*))))

(defun pending-outbound-var-generate-guids ()
  "Return a list of all pending outbound var-generate-guids."
  (let ((var-generate-guids '()))
    (maphash (lambda (_ var-generate-guid)
               (declare (ignore _))
               (push var-generate-guid var-generate-guids))
             *outbound-var-generate-guids*)
    var-generate-guids))

(defun pending-var-generate-guids ()
  "Return a hash table of pending inbound var-generate-guids by sender."
  *inbound-var-generate-guids*)

(defun create-outbound-var-generate-guid (resource class title user)
  "Create a new outbound var-generate-guid for RESOURCE of CLASS/TITLE from USER."
  (incf *var-generate-guid-counter*)
  (let ((var-generate-guid (make-outbound-var-generate-guid
                :id (format nil "var-generate-guid-~a-~a" (get-universal-time) *var-generate-guid-counter*)
                :resource resource
                :class class
                :title title
                :user user
                :timestamp (get-universal-time)
                :confirmed-p nil)))
  (setf (gethash (outbound-var-generate-guid-id var-generate-guid) *outbound-var-generate-guids*) var-generate-guid)
  var-generate-guid))

(defun confirm-outbound-var-generate-guid (var-generate-guid-id)
  "Mark an outbound var-generate-guid as confirmed (accepted by recipient)."
  (let ((var-generate-guid (gethash var-generate-guid-id *outbound-var-generate-guids*)))
    (when var-generate-guid
      (remhash var-generate-guid-id *outbound-var-generate-guids*)
      var-generate-guid)))

(defun add-inbound-var-generate-guid (resource class title user sender)
  "Add an inbound var-generate-guid from SENDER for RESOURCE of CLASS/TITLE from USER."
  (incf *var-generate-guid-counter*)
  (let* ((var-generate-guid-id (format nil "var-generate-guid-~a-~a" (get-universal-time) *var-generate-guid-counter*))
         (var-generate-guid (make-inbound-var-generate-guid
                 :id var-generate-guid-id
                 :resource resource
                 :class class
                 :title title
                 :user user
                 :sender sender
                 :timestamp (get-universal-time))))
    (push var-generate-guid (gethash sender *inbound-var-generate-guids*))
    var-generate-guid))

(defun clear-inbound-var-generate-guid (var-generate-guid-id sender)
  "Remove an inbound var-generate-guid after it has been accepted."
  (let ((var-generate-guids (gethash sender *inbound-var-generate-guids*)))
    (setf (gethash sender *inbound-var-generate-guids*)
          (remove-if (lambda (o) (string= (inbound-var-generate-guid-id o) var-generate-guid-id)) var-generate-guids))))

(defun group-by (predicate list)
  "Group LIST elements by the result of PREDICATE."
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (item list)
      (let ((key (funcall predicate item)))
        (push item (gethash key groups))))
    groups))
