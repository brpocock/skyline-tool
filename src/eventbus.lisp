(in-package :skyline-tool)

(defvar *event-bus* (make-hash-table :test 'equal)
  "Global event bus hash table mapping event types to subscriber lists.")

(defvar *event-bus-lock* (bt:make-lock "event-bus-lock")
  "Lock for thread-safe event bus operations.")

(defstruct event
  type payload timestamp)

(defun publish (event-type &key payload)
  "Publish an event to subscribers of EVENT-TYPE.
   EVENT-TYPE is a keyword identifying the event.
   PAYLOAD is an arbitrary plist containing event data."
  (let ((event (make-event :type event-type :payload payload :timestamp (get-universal-time))))
    (bt:with-lock-held (*event-bus-lock*)
      (dolist (subscriber (gethash event-type *event-bus*))
        (ignore-errors (funcall subscriber event))))))

(defun subscribe (event-type subscriber)
  "Subscribe to EVENT-TYPE events.
   EVENT-TYPE is a keyword identifying the event type.
   SUBSCRIBER is a function of one argument (the event)."
  (bt:with-lock-held (*event-bus-lock*)
    (push subscriber (gethash event-type *event-bus*))))

(defun unsubscribe (event-type subscriber)
  "Remove SUBSCRIBER from EVENT-TYPE subscribers."
  (bt:with-lock-held (*event-bus-lock*)
    (setf (gethash event-type *event-bus*)
          (remove subscriber (gethash event-type *event-bus*)))))

(defun clear-subscriptions ()
  "Clear all event subscriptions."
  (bt:with-lock-held (*event-bus-lock*)
    (clrhash *event-bus*)))