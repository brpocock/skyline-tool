(in-package :skyline-tool)

;;; Offer handling - incoming resource offers via HTTP/avahi

(defvar *pending-offers* (make-hash-table :test 'equal)
  "Hash table mapping sender string to list of offer plists.")
(defvar *accepted-offers* (make-hash-table :test 'equal)
  "Hash table mapping sender string to list of accepted offer plists.")

;; Exported symbols for menu enabling/disabling
(defun any-pending-offers ()
  "Return T if there are any pending offers, NIL otherwise."
  (plusp (hash-table-count *pending-offers*)))

(defun handle-offer-json (payload request)
  "Parse the offer/id/resourceRef JSON.  Display the dialog."
  (let* ((offer (getf payload :offer))
         (meta   (getf payload :info))
         (ref    (make-resource-ref :class (getf meta :class)
                                    :moniker (getf payload :moniker)))
         (sender (accept 'string :prompt "Incoming Offer:" :value
                       (format nil "~a on ~a"
                               (get-user-full-name) (system-hostname)))))
    (add-pending-offer sender (list :offer offer :info meta :ref ref))
    ;; Optionally, notify UI that a new offer arrived
    (when (boundp 'clim:*application-frame*)
      (clim:redisplay-frame-panes clim:*application-frame* :force-p t))))

(defun add-pending-offer (sender offer-plist)
  "Add an offer plist to the pending offers hash table under sender."
  (push offer-plist (gethash sender *pending-offers* nil)))

(defun remove-pending-offer (sender offer-plist)
  "Remove a specific offer plist from the pending offers for sender."
  (setf (gethash sender *pending-offers*)
        (delete offer-plist (gethash sender *pending-offers* nil) :test #'equal)))

(defun get-pending-offers-for-sender (sender)
  "Return list of offer plists for sender, or nil."
  (gethash sender *pending-offers* nil))

(defun show-offers-window (sender)
  "Display a dialog with a table of offers from sender, allowing multi-select."
  (let ((offers (get-pending-offers-for-sender sender)))
    (when offers
      (clim-sys:make-process
       (lambda ()
         (let* ((fm (clim:find-frame-manager :port (or (clim:find-port) (clim:find-port :server-path :x))))
                (frame (clim:make-application-frame
                        'offers-frame
                        :frame-manager fm
                        :sender sender
                        :offers offers
                        :width 600 :height 400)))
           (clim:run-frame-top-level frame)))
       :name (format nil "Offers from ~a" sender)))))

;; Command to show offers window from menu
(clim:define-command (com-show-offers-window :command-table clim-internals::global-command-table
                                             :menu t :name t)
    ((sender 'string :documentation "Sender name"))
  "Show offers window for SENDER."
  (show-offers-window sender))

(clim:define-application-frame offers-frame ()
  ((%sender :initarg :sender :accessor frame-sender)
   (%offers :initarg :offers :accessor frame-offers))
  (:panes (offers-pane :application :display-function 'display-offers
                       :scroll-bars :vertical))
  (:layouts (default (clim:vertically () offers-pane)))
  (:menu-bar nil))

(defun display-offers (frame pane)
  (let ((offers (frame-offers frame))
        (sender (frame-sender frame)))
    (let ((*standard-output* pane))
      (clim:formatting-table
       :stream pane
       :x-spacing 20 :y-spacing 10
       (clim:formatting-row
        (clim:surrounding-output-with-border
         pane :draw t
         (clim:formatting-cell
          pane :x-align :left
            (format pane "Offered")))
        (clim:surrounding-output-with-border
         pane :draw t
         (clim:formatting-cell
          pane :x-align :center
            (format pane "▶")))
        (clim:surrounding-output-with-border
         pane :draw t
         (clim:formatting-cell
          pane :x-align :left
            (format pane "Accepting")))
        (clim:surrounding-output-with-border
         pane :draw t
         (clim:formatting-cell
          pane :x-align :center
            (format pane "◀")))
        (clim:surrounding-output-with-border
         pane :draw t
         (clim:formatting-cell
          pane :x-align :left
            (format pane "Accepted")))
       (dolist (offer offers)
         (clim:formatting-row
          (clim:surrounding-output-with-border
           pane :draw t
           (clim:formatting-cell
            pane :x-align :left
              (format pane "~a" (getf offer :moniker))))
          (clim:push-button
           pane
           :label "▶"
           :activate-callback (lambda ()
                                (accept-single-offer sender offer)))
          (clim:push-button
           pane
           :label "◀"
           :activate-callback (lambda ()
                                (reject-single-offer sender offer)))
          (clim:surrounding-output-with-border
           pane :draw t
           (clim:formatting-cell
            pane :x-align :left
              (format pane "~a" (getf offer :moniker)))))
         ;; Bottom button for downloading all accepted
         (clim:surrounding-output-with-border
          pane :draw t
          (clim:formatting-cell
           pane :col-span 5
            (clim:push-button
             pane
             :label (format nil "Download ~d resource~:p"
                            (length (get-accepted-offers-for-sender sender)))
             :activate-callback #'download-accepted-offers)))))))

(defun accept-single-offer (sender offer)
  (remove-pending-offer sender offer)
  ;; Move to accepted list (we could store in another hash table)
  (push offer (gethash sender *accepted-offers* nil))
  (when (boundp 'clim:*application-frame*)
    (clim:redisplay-frame-panes clim:*application-frame* :force-p t)))

(defun reject-single-offer (sender offer)
  (remove-pending-offer sender offer)
  (when (boundp 'clim:*application-frame*)
    (clim:redisplay-frame-panes clim:*application-frame* :force-p t)))

(defun get-accepted-offers-for-sender (sender)
  (gethash sender *accepted-offers* nil))

(defun download-accepted-offers ()
  ;; Placeholder: implement actual download via HTTP GET
  (format t "Downloading accepted offers...~%"))