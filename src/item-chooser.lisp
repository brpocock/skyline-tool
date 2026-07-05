(in-package :skyline-tool)

(defun load-item-list (&optional (path "../Source/Tables/Inventory.txt"))
  "Return a list of item names from PATH, skipping empty lines and 'unused' items."
  (let ((full-path (merge-pathnames path (uiop:getcwd))))
    (when (probe-file full-path)
      (with-open-file (f full-path)
        (loop for line = (read-line f nil nil)
              while line
              unless (or (zerop (length (string-trim " " line)))
                         (search "unused" line :test #'char-equal))
              collect (string-trim " " line))))))

(defun load-key-list (&optional (path "../Source/Tables/Keys.txt"))
  "Return a list of key names from PATH."
  (let ((full-path (merge-pathnames path (uiop:getcwd))))
    (when (probe-file full-path)
      (with-open-file (f full-path)
        (loop for line = (read-line f nil nil)
              while line
              unless (zerop (length (string-trim " " line)))
              collect (string-trim " " line))))))

(defun normalize-search (s)
  "Remove spaces and punctuation for case-insensitive search."
  (remove-if (complement #'alphanumericp) (string-downcase (or s ""))))

(defun run-item-chooser (&key inventory equipped-item equipped-shield wearing-armor
                              (title "Choose Items"))
  "Open an interactive Item Chooser window.
   INVENTORY: list of item names the character has in their bag.
   EQUIPPED-ITEM: name of equipped weapon item, or NIL.
   EQUIPPED-SHIELD: name of equipped shield, or NIL.
   WEARING-ARMOR: name of worn armor, or NIL.
   Returns (values inventory equipped-item equipped-shield wearing-armor) or NIL."
  (let* ((all-items (load-item-list))
         (all-keys (load-key-list))
         (frame (clim:make-application-frame
                 'item-chooser-frame
                 :pretty-name title
                 :all-items all-items
                 :all-keys all-keys
                 :inventory (copy-list inventory)
                 :equipped-item equipped-item
                 :equipped-shield equipped-shield
                 :wearing-armor wearing-armor)))
    (clim:run-frame-top-level frame)))

(clim:define-application-frame item-chooser-frame ()
  ((all-items :initarg :all-items :accessor frame-all-items)
   (all-keys :initarg :all-keys :accessor frame-all-keys)
   (inventory :initarg :inventory :accessor frame-inventory)
   (equipped-item :initarg :equipped-item :accessor frame-equipped-item)
   (equipped-shield :initarg :equipped-shield :accessor frame-equipped-shield)
   (wearing-armor :initarg :wearing-armor :accessor frame-wearing-armor)
   (filter :initform "" :accessor frame-filter)
   (mode :initform nil :accessor frame-mode))
  (:panes
   (left-pane :application :display-function 'display-left-pane :height 500 :width 300)
   (right-pane :application :display-function 'display-right-pane :height 500 :width 300)
   (interactor :interactor :height 100 :width 600))
  (:layouts
   (default (clim:horizontally ()
              left-pane
              (clim:vertically ()
                (clim:make-pane :text-field :value ""
                                :value-changed-callback
                                (lambda (&key value &allow-other-keys)
                                  (setf (frame-filter clim:*application-frame*) value)
                                  (clim:redisplay-frame-panes clim:*application-frame*)))
                right-pane)
              interactor))))

(defun display-left-pane (frame pane)
  "Display the character's inventory with equipped slots and bag."
  (clim:window-clear pane)
  (let ((eq (frame-equipped-item frame))
        (es (frame-equipped-shield frame))
        (wa (frame-wearing-armor frame))
        (inv (frame-inventory frame)))
    (format pane "~&Equipped Item: ~:[(empty)~;~:*~a~]~%" eq)
    (format pane "~&Equipped Shield: ~:[(empty)~;~:*~a~]~%" es)
    (format pane "~&Wearing Armor: ~:[(empty)~;~:*~a~]~%" wa)
    (format pane "~&--- Bag ---~%")
    (dolist (item inv)
      (clim:with-output-as-presentation (pane item 'item-name)
        (format pane "~&  ~a~%" item)))))

(defun display-right-pane (frame pane)
  "Display available items not in inventory, filtered by search."
  (clim:window-clear pane)
  (let* ((filter (normalize-search (frame-filter frame)))
         (equipped (list (frame-equipped-item frame)
                         (frame-equipped-shield frame)
                         (frame-wearing-armor frame)))
         (inv (frame-inventory frame))
         (all (append (frame-all-items frame) (frame-all-keys frame))))
    (dolist (item (remove-duplicates all :test #'string-equal))
      (unless (or (member item inv :test #'string-equal)
                  (member item equipped :test #'string-equal)
                  (and (plusp (length filter))
                       (not (search filter (normalize-search item))))))
        (clim:with-output-as-presentation (pane item 'item-name)
          (format pane "~&~a~%" item)))))

(clim:define-presentation-type item-name () :inherit-from 'string)

(clim:define-command (com-item-click :command-table clim-internals::global-command-table
                                     :menu nil :name t)
    ((item 'item-name :gesture :select))
  (let* ((frame clim:*application-frame*)
         (eq (frame-equipped-item frame))
         (es (frame-equipped-shield frame))
         (wa (frame-wearing-armor frame))
         (inv (frame-inventory frame)))
    (cond
      ;; Item is in the equipped item slot — unequip it
      ((and eq (string-equal item eq))
       (setf (frame-equipped-item frame) nil)
       (push item (frame-inventory frame)))
      ;; Item is in the shield slot — unequip it
      ((and es (string-equal item es))
       (setf (frame-equipped-shield frame) nil)
       (push item (frame-inventory frame)))
      ;; Item is in the armor slot — unequip it
      ((and wa (string-equal item wa))
       (setf (frame-wearing-armor frame) nil)
       (push item (frame-inventory frame)))
      ;; Item is in the bag — offer equip/remove
      ((member item inv :test #'string-equal)
       (let ((choice (clim:menu-choose
                      (list (list "Equip as Weapon")
                            (list "Equip as Shield")
                            (list "Wear as Armor")
                            (list "Remove from Inventory"))
                      :label (format nil "~a:" item))))
         (setf (frame-inventory frame) (remove item inv :test #'string-equal))
         (cond ((string-equal choice "Equip as Weapon")
                (when eq (push eq (frame-inventory frame)))
                (setf (frame-equipped-item frame) item))
               ((string-equal choice "Equip as Shield")
                (when es (push es (frame-inventory frame)))
                (setf (frame-equipped-shield frame) item))
               ((string-equal choice "Wear as Armor")
                (when wa (push wa (frame-inventory frame)))
                (setf (frame-wearing-armor frame) item))
               (t ;; Remove from inventory — already removed above
                ))))
      ;; Item is on the right side — add to bag
      (t
       (push item (frame-inventory frame))))
    (clim:redisplay-frame-panes frame)))
