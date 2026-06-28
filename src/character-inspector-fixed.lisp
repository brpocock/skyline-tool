;;; Character Inspector — Single-Character Inspector
;;; Opens from All Resources context menu.

(in-package :skyline-tool)

;; ─── Color Constants ───────────────────────────────────

(define-constant +ntsc-col-names+
    '("Grey" "Yellow" "Brown" "Orange" "Red" "Magenta"
      "Purple" "Indigo" "Blue" "Turquoise" "Cyan" "Teal"
      "Seafoam" "Green" "Springgreen" "Gold")
  :test 'equalp)

(define-constant +pal-col-names+
    '("Grey" "Spinach" "Gold" "Orange" "Red" "Magenta"
      "Violet" "Purple" "Indigo" "Blue" "Stonewash"
      "Turquoise" "Green" "Seafoam" "Springgreen" "Algae")
  :test 'equalp)

(define-constant +all-col-names+
    (remove-duplicates (append +ntsc-col-names+ +pal-col-names+)
                       :test #'string-equal)
  :test 'equalp)

(defun col-rgb (hue-index luminance palette)
  "Get (R G B) for a COL HUE-INDEX at LUMINANCE 0-15 from PROSYSTEM PALETTE."
  (elt palette (+ (* hue-index 16) luminance)))

(define-constant +palette-color-rgb+
    '(("Peach" 255 203 164) ("Green" 0 160 0)
      ("Purple" 128 0 128) ("Silver" 192 192 192)
      ("Orange" 255 165 0) ("Brown" 139 69 19)
      ("White" 255 255 255) ("Gray" 128 128 128)
      ("Black" 0 0 0) ("Yellow" 255 255 0)
      ("Red" 255 0 0) ("Blue" 0 0 255))
  :test 'equalp)

(define-constant +equipment-names+
    '(("Knife" . "EquipKnife") ("Hammer" . "EquipHammer")
      ("Potion" . "EquipPotion") ("Sword" . "EquipSword")
      ("Bow" . "EquipBow") ("Chalice" . "EquipChalice")
      ("Staff" . "EquipStaff") ("Wand" . "EquipWand")
      ("Rope" . "EquipRope") ("Glass" . "EquipGlass")
      ("Wrench" . "EquipWrench") ("None" . "EquipNone"))
  :test 'equalp)

(define-constant +shield-names+
    '(("Small Shield" . "ShieldSmallShield")
      ("Large Shield" . "ShieldLargeShield")
      ("None" . "ShieldNoShield"))
  :test 'equalp)

(define-constant +decal-kind-names+
    '(("Human" . "DecalKindHuman") ("Enemy" . "DecalKindEnemy")
      ("NPC" . "DecalKindNPC") ("Boss" . "DecalKindBoss")
      ("Player" . "DecalKindPlayer") ("Item" . "DecalKindItem"))
  :test 'equalp)

(define-constant +palette-color-non-variable+
    '("Peach" "Green" "Purple" "Silver" "Orange" "Brown"
      "White" "Gray" "Black" "Yellow" "Red" "Blue")
  :test 'equalp)

;; ─── Data Loading ─────────────────────────────────────

(defun %find-char-in-ods (name)
  (ignore-errors
    (load-npc-stats)
    (let ((row (find (string name) *npc-stats* :test
                     (lambda (n r)
                       (or (string-equal n (getf r :name))
                           (member n (getf r :nicks) :test #'string-equal))))))
      (when row (load-actor (getf row :name))))))
  "Locate a character plist from NPCStats.ods by NAME string."

(defun %find-prototype-for-char-id (char-id)
  (dolist (file (list-object-prototype-json-files))
    (let* ((data (ignore-errors (load-prototype-file file)))
           (cid (cdr (assoc :|CharacterCharacterID| data))))
      (when (and cid (= cid char-id))
        (return-from %find-prototype-for-char-id
          (loop for (key . val) in data
                append (list key val)))))))
  "Find a JSON prototype plist by character ID."

(defun %ensure-items-list ()
  (let ((path (merge-pathnames "Source/Tables/Inventory.txt" (uiop:getcwd))))
    (when (probe-file path)
      (with-open-file (f path)
        (loop for line = (read-line f nil nil)
              while line
              unless (or (zerop (length (string-trim " " line)))
                         (search "unused" line :test #'char-equal))
                collect (string-trim " " line))))))
  "Load item names from Source/Tables/Inventory.txt, return list."

(defun %ensure-keys-list ()
  (let ((path (merge-pathnames "Source/Tables/Keys.txt" (uiop:getcwd))))
    (when (probe-file path)
      (with-open-file (f path)
        (loop for line = (read-line f nil nil)
              while line
              unless (zerop (length (string-trim " " line)))
                collect (string-trim " " line))))))
  "Load key names from Source/Tables/Keys.txt, return list."

;; ─── Bitset ─────────────────────────────────────────────

(defun %bitset->indices (bytes)
  (loop for byte across (coerce bytes 'vector)
        for offset from 0 by 8
        append (loop for bit from 0 below 8
                     when (logbitp bit byte) collect (+ offset bit))))
  "Convert a list of BYTES to list of set bit indices."

(defun %set-bit (bytes index value)
  (let* ((byte-pos (floor index 8)) (bit-pos (mod index 8))
         (result (copy-list bytes)))
    (when (< byte-pos (length result))
      (setf (elt result byte-pos)
            (if value (logior (elt result byte-pos) (ash 1 bit-pos))
                (logand (elt result byte-pos) (lognot (ash 1 bit-pos))))))
    (coerce result 'list)))
  "Return a fresh copy of BYTES with bit INDEX set to VALUE (t/nil)."

;; ─── AtariVox ─────────────────────────────────────────

(defun %test-atarivox (pitch speed bend phrase)
  (when *atarivox-port* (ignore-errors (close *atarivox-port*)) (setf *atarivox-port* nil))
  (handler-case
      (let ((stream (second (find-atarivox-serial-port))))
        (setf *atarivox-port* stream)
        (when pitch (write-byte 22 *atarivox-port*) (write-byte pitch *atarivox-port*))
        (when speed (write-byte 21 *atarivox-port*) (write-byte speed *atarivox-port*))
        (when bend (write-byte 23 *atarivox-port*) (write-byte bend *atarivox-port*))
        (speech-speak phrase :atarivox)
        (format *query-io* "~&AtariVox spoke: ~a~%" phrase)
  (when *atarivox-port* (ignore-errors (close *atarivox-port*)) (setf *atarivox-port* nil))
;; ─── Drawing Helpers ──────────────────────────────────

(defun draw-col-swatches (pane hue-index palette-name)
  (let* ((size 12)
         (pal (ecase palette-name (:ntsc +prosystem-ntsc-palette+) (:pal +prosystem-pal-palette+)))
         (light (col-rgb hue-index 15 pal))
         (dark (col-rgb hue-index 0 pal))
         (cpos (multiple-value-list (clim:stream-cursor-position pane)))
         (cx (first cpos)) (cy (second cpos)))
    (flet ((sw (rgb x)
             (clim:draw-rectangle* pane (+ cx x) cy (+ cx x size) (+ cy size)
                                   :ink (clim:make-rgb-color (/ (first rgb) 255.0)