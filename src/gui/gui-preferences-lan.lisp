;; LAN Sharing Tab - Preferences Inspector
;; Contains P2P/LAN sharing configuration: user name, domain, interface, ports, and key algorithm

(in-package :skyline-tool)

(defun display-lan-sharing-tab (frame pane)
  "Display the LAN Sharing tab content"
  (clim:formatting-table-pane (pane :name "lan-sharing-tab"))
  (make-section-header pane "LAN Sharing")
  (unless (p2p-enabled-checkbox frame)
    (setf (p2p-enabled-checkbox frame)
          (clim:make-pane 'clim:check-box
                          :label "Enable P2P Resource Sharing"
                          :value (get-pref '(:p2p :enabled) nil)
                          :value-changed-callback
                          (lambda (g v)
                            (declare (ignore g))
                            (setf (get-pref '(:p2p :enabled)) v)
                            (save-preferences-now frame)
                            (clim:redisplay-frame-panes frame :force-p t)))))
  (make-full-width-row pane (clim:note-gadget-activated (p2p-enabled-checkbox frame) pane))
  (make-label-value-row pane "Advertised User Name"
                        (unless (p2p-user-field frame)
                          (setf (p2p-user-field frame)
                                (clim:make-pane 'clim:text-field
                                                :value (or (get-pref '(:p2p :user) "") (user-full-name))
                                                :width 300
                                                :activate-callback
                                                (lambda (gadget value)
                                                  (declare (ignore gadget))
                                                  (setf (get-pref '(:p2p :user)) value)
                                                  (save-preferences-now frame))))))
  (clim:note-gadget-activated (p2p-user-field frame) pane)
  (make-label-value-row pane "Service Domain"
                        (unless (p2p-domain-field frame)
                          (setf (p2p-domain-field frame)
                                (clim:make-pane 'clim:text-field
                                                :value (get-pref '(:p2p :domain) "local.")
                                                :width 200
                                                :activate-callback
                                                (lambda (gadget value)
                                                  (declare (ignore gadget))
                                                  (setf (get-pref '(:p2p :domain)) value)
                                                  (save-preferences-now frame))))))
  (clim:note-gadget-activated (p2p-domain-field frame) pane)
  (make-label-value-row pane "Network Interface"
                        (unless (p2p-interface-field frame)
                          (setf (p2p-interface-field frame)
                                (clim:make-pane 'clim:text-field
                                                :value (get-pref '(:p2p :interface) "eth0")
                                                :width 200
                                                :activate-callback
                                                (lambda (gadget value)
                                                  (declare (ignore gadget))
                                                  (setf (get-pref '(:p2p :interface)) value)
                                                  (save-preferences-now frame))))))
  (clim:note-gadget-activated (p2p-interface-field frame) pane)
  (make-label-value-row pane "Advertise Interval (sec)"
                        (unless (p2p-advertise-interval-field frame)
                          (setf (p2p-advertise-interval-field frame)
                                (clim:make-pane 'clim:text-field
                                                :value (format nil "~a" (get-pref '(:p2p :advertise-interval) 30))
                                                :width 80
                                                :activate-callback
                                                (lambda (gadget value)
                                                  (declare (ignore gadget))
                                                  (let ((val (parse-integer (or value "30") :junk-allowed t)))
                                                    (setf (get-pref '(:p2p :advertise-interval)) (max 5 (min 300 val)))
                                                    (save-preferences-now frame))))))
  (clim:note-gadget-activated (p2p-advertise-interval-field frame) pane)
  (make-label-value-row pane "Discovery Interval (sec)"
                        (unless (p2p-discovery-interval-field frame)
                          (setf (p2p-discovery-interval-field frame)
                                (clim:make-pane 'clim:text-field
                                                :value (format nil "~a" (get-pref '(:p2p :discovery-interval) 60))
                                                :width 80
                                                :activate-callback
                                                (lambda (gadget value)
                                                  (declare (ignore gadget))
                                                  (let ((val (parse-integer (or value "60") :junk-allowed t)))
                                                    (setf (get-pref '(:p2p :discovery-interval)) (max 10 (min 600 val)))
                                                    (save-preferences-now frame))))))
  (clim:note-gadget-activated (p2p-discovery-interval-field frame) pane)
  (make-label-value-row pane "Port Range:"
                        (make-label-value-row (pane "Min:")
                                              (unless (p2p-min-port-field frame)
                                                (setf (p2p-min-port-field frame)
                                                      (clim:make-pane 'clim:text-field
                                                                      :value (format nil "~a" (get-pref '(:p2p :min-port) 50000))
                                                                      :width 80
                                                                      :activate-callback
                                                                      (lambda (gadget value)
                                                                        (declare (ignore gadget))
                                                                        (let ((val (parse-integer (or value "50000") :junk-allowed t)))
                                                                          (setf (get-pref '(:p2p :min-port)) (max 1024 (min 65535 val)))
                                                                          (save-preferences-now frame))))))
                                              (clim:note-gadget-activated (p2p-min-port-field frame) pane))
                        (make-label-value-row (pane "Max:")
                                              (unless (p2p-max-port-field frame)
                                                (setf (p2p-max-port-field frame)
                                                      (clim:make-pane 'clim:text-field
                                                                      :value (format nil "~a" (get-pref '(:p2p :max-port) 60000))
                                                                      :width 80
                                                                      :activate-callback
                                                                      (lambda (gadget value)
                                                                        (declare (ignore gadget))
                                                                        (let ((val (parse-integer (or value "60000") :junk-allowed t)))
                                                                          (setf (get-pref '(:p2p :max-port)) (max (get-pref '(:p2p :min-port)) (min 65535 val)))
                                                                          (save-preferences-now frame))))))
                                              (clim:note-gadget-activated (p2p-max-port-field frame) pane)))
  (make-label-value-row pane "Public Key Algorithm"
                        (unless (p2p-pubkey-algo-field frame)
                          (setf (p2p-pubkey-algo-field frame)
                                (clim:make-pane 'clim:option-pane
                                                :items '("ed25519" "rsa4096" "ecdsa")
                                                :value (get-pref '(:p2p :pubkey-algo) "ed25519")
                                                :value-changed-callback
                                                (lambda (g v)
                                                  (declare (ignore g))
                                                  (setf (get-pref '(:p2p :pubkey-algo)) v)
                                                  (save-preferences-now frame))))))