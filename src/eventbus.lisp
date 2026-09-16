(in-package :skyline-tool)

(defvar *event-bus* (noloop.eventbus:make-eventbus)
  "Global event bus instance using noloop.eventbus.")

(defun publish (event-type &rest payload)
  "Publish an event to subscribers of EVENT-TYPE.
   EVENT-TYPE is a keyword identifying the event.
   PAYLOAD is an arbitrary plist containing event data."
  (if (eql :payload (first payload))
      (noloop.eventbus:emit *event-bus* event-type (second payload))
      (if (< 1 (length payload))
          (noloop.eventbus:emit *event-bus* event-type (first payload))
          (noloop.eventbus:emit *event-bus* event-type payload))))

(defun subscribe (event-type subscriber)
  "Subscribe to EVENT-TYPE events.
   EVENT-TYPE is a keyword identifying the event type.
   SUBSCRIBER is a function of one argument (the event)."
  (noloop.eventbus:on *event-bus* event-type subscriber))

(defun unsubscribe (event-type subscriber)
  "Remove SUBSCRIBER from EVENT-TYPE subscribers."
  (noloop.eventbus:off *event-bus* event-type subscriber))

(defun clear-subscriptions ()
  "Clear all event subscriptions."
  (setf *event-bus* (noloop.eventbus:make-eventbus)))
