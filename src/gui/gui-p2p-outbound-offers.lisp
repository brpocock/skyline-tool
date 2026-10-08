;;; Skyline-Tool src/gui/gui-p2p-outbound-offers.lisp
;;; GUI for managing outbound P2P resource offers

(in-package :skyline-tool)

(clim:define-application-frame p2p-outbound-offers-frame (uniform-inspector-frame)
  ()
  (:panes
   (offers-list :application
                :display-function 'display-outbound-offers
                :scroll-bars :vertical
                :height 500 :width 700)
   (status-bar :application
               :display-function 'display-outbound-status
               :height 30 :width 700))
  (:layouts (default (clim:vertically () offers-list status-bar)))
  (:menu-bar p2p-outbound-menu-bar)
  (:pretty-name "Outbound P2P Offers"))

;; Command tables
(clim:define-command-table p2p-outbound-file-menu
  :menu (("Refresh" :command com-refresh-outbound-offers)
         (nil :divider :line)
         ("Close" :command com-close-outbound-offers)))

(clim:define-command-table p2p-outbound-menu-bar
  :menu (("File" :menu p2p-outbound-file-menu)
         ("Help" :menu inspector-help-menu)))

;; Display functions
(defun display-outbound-offers (frame pane)
  (let ((*standard-output* pane))
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane)
          (clim:with-text-face (pane :bold)
            (format pane "Offer ID")))
        (clim:formatting-cell (pane)
          (clim:with-text-face (pane :bold)
            (format pane "Title")))
        (clim:formatting-cell (pane)
          (clim:with-text-face (pane :bold)
            (format pane "Class")))
        (clim:formatting-cell (pane)
          (clim:with-text-face (pane :bold)
            (format pane "User")))
        (clim:formatting-cell (pane)
          (clim:with-text-face (pane :bold)
            (format pane "Status"))))
      (dolist (offer (pending-outbound-offers))
        (clim:formatting-row (pane)
          (clim:formatting-cell (pane)
            (format pane "~a" (outbound-offer-id offer)))
          (clim:formatting-cell (pane)
            (format pane "~a" (outbound-offer-title offer)))
          (clim:formatting-cell (pane)
            (format pane "~a" (outbound-offer-class offer)))
          (clim:formatting-cell (pane)
            (format pane "~a" (outbound-offer-user offer)))
          (clim:formatting-cell (pane)
            (if (outbound-offer-confirmed-p offer)
                (format pane "Confirmed")
                (format pane "Pending"))))))))

(defun display-outbound-status (frame pane)
  (format pane "Outbound Offers: ~d pending" (hash-table-count *outbound-offers*)))

;; Commands
(clim:define-command (com-refresh-outbound-offers :command-table p2p-outbound-file-menu
                                                   :menu t :name t) ()
  (clim:redisplay-frame-panes clim:*application-frame* :force-p t))

(clim:define-command (com-close-outbound-offers :command-table p2p-outbound-file-menu
                                                 :menu t :name t) ()
  (clim:frame-exit clim:*application-frame*))

(defun open-p2p-outbound-offers ()
  "Open the outbound offers management window."
  (clim:run-frame-top-level
   (clim:make-application-frame 'p2p-outbound-offers-frame)))