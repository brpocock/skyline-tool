;;; Skyline-Tool src/gui/gui-character.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)
#|

### BEGIN ### NO ALTERATIONS PERMITTED ###

### IN-LINE REQUIREMENTS MUST NOT BE ALTERED NOR REMOVED


### WINDOW OVERALL

{ Title Bar            }
{ Menu Bar             }
[Tab Bar Pane----------]
[                      ]
[      Main Pane       ]
[                      ]
[OPTIONAL Search Bar   ]
[OPTIONAL Project Bar  ]

### TAB BAR
__________   ____________   ___________   ________
/ Identity \ / Appearance \ / Equipment \ / Speech \
|          | |            | |           | |        |
### IDENTITY TAB

Name:  ____________ (__/12)          # Name field allowing 12 minifont bytes
# (may be slightly more Unicode characters)
# if the name contains characters that cannot be represented an error appears here
# on the line beneath the name

Gender: <> Masculine (he)
<> Feminine (she)
<> Indefinite (they)
<> Impersonal (it)

Home: __________________________

Comments:
|| ____________________________________________________________ ||
|| ____________________________________________________________ ||
|| ____________________________________________________________ ||
|| ____________________________________________________________ ||
|| ____________________________________________________________ ||
|| ____________________________________________________________ ||

Movement
---------

Course Class: [ $(Course Classes) - ]

Prototype: <> Zeroes
<> [ Object-Prototypes of class $(Course Class)  - ]

Hit Points
-----------

HP: __.__ / __.__ max           # Fractional values $00.00 - $ff.ff (edit in decimal)

Faction
--------
[] Vizier's Forces | $80
[] Loyalists       | $40
[] Villagers       | $20
[] Merfolk         | $10
[] (unused)        | $08
[] (unused)        | $04
[] (unused)        | $02
[] (unused)        | $01

Character Flags                 # Note ... unrelated to Game-Resource-Flag game flags
----------------
[] Enemy Walk On | $01
[] (undefined)   | $02
[] (undefined)   | $04
[] (undefined)   | $08
[] (undefined)   | $10
[] (undefined)   | $20
[] (undefined)   | $40
[] (undefined)   | $80

#### APPEARANCE TAB

Appearance
-----------

Decal Kind: [ Player   - ]
[ Human      ]
[ Earl       ]
[ Captain    ]
[ Princess   ]
[ Elder      ]
[ Nefertem   ]
[ Vizier     ]
[ Sentinel   ]
[ Sailor     ]
[ Enemy      ]
[ Block1     ]
[ Block2     ]
[ Block3     ]
[ Block4     ]

Skin Color: [##]                          # color swatches go to palette color chooser
Hair Color: [##]                          # only from the 12 upper palette colors
Clothes Color: [##]                       # Peach, Green, Purple, Silver, Orange, ... Blue

# This is derived by extracting the  colors from the JSON file, and then
# finding the nearest possible color in each of NTSC and PAL
# eg: { ##  ##  Orange }
# Swatch, NTSC; Swatch, PAL; color name as per JSON.

# no Clear nor VarColor1/2/3 allowed

Head: [ Heads for Decal Kind - ]

Body: [ Bodies for Decal Kind - ]

#### FOR MOST KINDS ####

Head: ___                                 # number 0 - 255, usually zero
Body: ___                                 # number 0 - 255, usually zero

#### FOR HUMAN ####

Head: [ Head 1  - ]
[ Head 2    ]
[ Head 3    ]
...
[ Head 10   ]

Body: [ Robe  - ]
[ Tunic   ]

### FOR MERFOLK ###

Head: [ Head 1  - ]
[ Head 2    ]
...
[ Head 12   ]

Body: ___                              # usually zero, but can be 0-255

### FOR ENEMY ###

Memory Bank: [ $( Tileset memory banks ) - ]    # e.g. Sandy Island Bank ... stored in Head slot

Body: [ Body $00 - ]
[ Body $01   ]
...
[ Body $ff   ]

### CONTINUING ON ... ###

Animation Sequences
--------------------

For this Action: [ Idle - ]                                       # Switch which animation action shown

# For each action, the facings will show the animation sequence names and the animated
# preview of each sequence playing at a small size, each character pixel as a 4px (wide) ×
# 2px (high) rectangle, all four at the same time ...
# Clicking the sequence name brings up the existing chooser specialized on the
# decal kind &c. as per current practice.

# This section  inset with a small  margin and bordered with  a shadowed
# rectangle in black

North: { Animation Sequence Name }
{ PREVIEW }

West: { Animation Sequence Name } | East: {Animation Sequence Name}
{ PREVIEW }              |         { PREVIEW }

South: { Animation Sequence Name }
{ PREVIEW }

#### INVENTORY TAB

Shield: [ Shields in Inventory - ]
Equipment: [ Equippable Items in Inventory - ]

## for the player only:
Armor: [ Armor in Inventory - ]
## for all other characters:
Armor Class: ___
## for everyone, continue:

Consumables
------------

Crowns: _____
Arrows: ___
Potions: ___

Inventory
----------
| [] | $00 | Item 1 Name              | 👊 | # column at right shows fist for equippable items
| [] | $01 | Item 2 Name              | 🛡 | # shield icon for shield items
| [] | $02 | Item 3 Name              | ⛑ | # helmet icon for armor items
| [] | $03 | Item 4 Name              | 💍 | # ring icon for wearable, non-armor items
| [] | $04 | Item 5 Name              |     | # normal "quest" items have no icons
...
| [] | $1f | Item 64 Name              |    | # end of scrolling sub-window


Keys
-----
[] Key 1 Name
[] Key 2 Name
...
[] Key 32 Name


### SPEECH TAB

Pitch: <-------|--------> ____                                            # slider with number gadgets
Bend:  <-------|--------> ____
Speed: <-------|--------> ____

Sample Phrase: <> In the name of Nornornornornor, I vanquish you!          # These expressions
<> With tenure, Suzie’d have all the more leisure           # can be edited in the
for yachting, but her publications are no good.          # Preferences Inspector
<> The beige hue on the waters of the loch impressed
all, including the French queen, before she heard
that symphony again, just as young Arthur wanted.
<> ______________________________________                   # or the user can provide one
______________________________________                   # ad hoc for testing

Volume: <-------|--------> ___
AtariVox on [ ttyUSB0 - ]                           ( Speak... )
$(all serial ports on USB)

Color: [##]                     # color swatch -> palette color picker menu
# This chooses from the full 16 hues range
# gray, yellow, brown, orange, red, ... &c.
# in the palette (NTSC/PAL) of the current selected
# region on every Run menu (they must sync up)

# Color picker pop-up window for speech colors specifically:
# shows the color at two brightness levels for NTSC, space, same
# two brightness levels for PAL, then the color name.
# Note that some color names have the same colors as on another on
# either NTSC or PAL, so there are some duplicate colors on each
# set, but the user can choose from any on the list.



{ ##/##  ##/##  Grey                 }
{ ##/##  ##/##  Yellow               }
{ ##/##  ##/##  Spinach              }
{ ##/##  ##/##  Gold                 }
{ ##/##  ##/##  Brown                }
{ ##/##  ##/##  Orange               }
{ ##/##  ##/##  Red                  }
{ ##/##  ##/##  Magenta              }
{ ##/##  ##/##  Violet               }
{ ##/##  ##/##  Purple               }
{ ##/##  ##/##  Indigo               }
{ ##/##  ##/##  Blue                 }
{ ##/##  ##/##  Stonewash            }
{ ##/##  ##/##  Turquoise            }
{ ##/##  ##/##  Cyan                 }
{ ##/##  ##/##  Teal                 }
{ ##/##  ##/##  Green                }
{ ##/##  ##/##  Seafoam              }
{ ##/##  ##/##  Spring-Green         }
{ ##/##  ##/##  Algae                } 

### UNDER THE MAIN  EDITING PANE THERE MAY BE AN  OPTIONAL Project Pane ;
### AS IS COMMON TO ALL INSPECTORS (VC/issues)

### Below the following pipe-sharp sequence the code begins ###
### END of NO ALTERATIONS PERMITTED section ###

|#

;; Eventbus types for character inspector
(define-constant +character-inspector-event-types+
    '(:character-data-changed :equipment-changed :appearance-changed :speech-changed
      :inventory-changed :keys-changed :stats-changed :flags-changed)
  :test 'equalp)

(clim:define-presentation-type game-resource-character-reference ()
  :inherit-from 'game-resource-character)

(defmethod present-reference ((resource game-resource-character) stream)
  (clim:with-output-as-presentation
      (stream resource 'game-resource-character-reference)
    (clim:formatting-table (stream)
      (clim:formatting-row (stream)
        ;; Left Column: Icon (3 lines high, 4.8 line-heights wide)
        (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 0)
          (format stream "~3%"))
        (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 125)
          (game-resource-present-icon resource stream))
        (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 150)
          (clim:with-text-face (stream :bold)
            (game-resource-present-title resource stream))
          (format stream "~%~5t")
          (clim:with-text-size (stream :smaller)
            (clim:with-drawing-options (stream :ink (clim:make-gray-color 0.75))
              (game-resource-present-subheading resource stream))))
        (clim:formatting-cell (stream :align-x :right :align-y :top :min-height 90 :min-width 125)
          (game-resource-present-right-margin resource stream))))))

(defmethod open-resource-inspector ((resource game-resource-character) &optional (mode :editing))
  (declare (ignore mode))
  (open-character-inspector resource))

(defmethod present-reading ((resource game-resource-character) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Title: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-title resource))))))

(defmethod present-editing ((resource game-resource-character) stream)
  "Present character for editing — currently delegates to reference view."
  (present-reference resource stream))

;;; Character Inspector — Single-Character Inspector
;;; Opens from All Resources context menu.

(defun bitset->indices (bytes)
  "Convert a list of BYTES to a list of set bit indices (0-63)."
  (loop for byte across (coerce bytes 'vector) for offset from 0 by 8
        append (loop for bit from 0 below 8
                     when (logbitp bit byte) collect (+ offset bit))))

(defun %set-bit (bytes index value)
  "Return a fresh copy of BYTES with bit INDEX set to VALUE (t/nil)."
  (let* ((byte-pos (floor index 8)) (bit-pos (mod index 8))
                                    (result (copy-list bytes)))
    (when (< byte-pos (length result))
      (setf (elt result byte-pos)
            (if value (logior (elt result byte-pos) (ash 1 bit-pos))
                (logand (elt result byte-pos) (lognot (ash 1 bit-pos))))))
    (coerce result 'list)))

(defun %test-atarivox (pitch speed bend phrase)
  (when *atarivox-port* (ignore-errors (close *atarivox-port*)) (setf *atarivox-port* nil))
  (let ((stream (second (find-atarivox-serial-port))))
    (setf *atarivox-port* stream)
    (when pitch (write-byte 22 *atarivox-port*) (write-byte pitch *atarivox-port*))
    (when speed (write-byte 21 *atarivox-port*) (write-byte speed *atarivox-port*))
    (when bend (write-byte 23 *atarivox-port*) (write-byte bend *atarivox-port*))
    (speech-speak phrase :atarivox)
    t))

(clim:define-presentation-type char-cmd () :inherit-from 'symbol :description "Character inspector command")

(defclass tab-friendly-mixin ()
  ((current-tab :initarg :current-tab :accessor frame-current-tab :initform :identity))
  (:documentation "Mixin for inspector frames with tabbed interfaces."))

(defgeneric frame-tab-list (frame)
  (:documentation "Return the list of tab keywords for this inspector frame."))

(defgeneric switch-to-tab (frame tab)
  (:documentation "Switch FRAME to TAB and redisplay."))

(defmethod switch-to-tab ((frame tab-friendly-mixin) tab)
  (setf (frame-current-tab frame) tab)
  (clim:redisplay-frame-panes frame :force-p t))

(clim:define-command-table char-inspector-menu-bar
  :menu (("File" :menu char-inspector-file-menu)
         ("Edit" :menu char-inspector-edit-menu)
         ("Run" :menu char-inspector-run-menu)
         ("View" :menu char-inspector-view-menu)
         ("Voice" :menu char-inspector-voice-menu)
         ("Help" :menu char-inspector-help-menu)))

(clim:define-application-frame character-inspector-frame
    (tab-friendly-mixin resource-inspector-mixin clim:standard-application-frame)
  ((test-phrase :accessor frame-test-phrase))
  (:panes
   (tab-bar :application :display-function 'display-tab-bar :height 30 :width 400 :scroll-bars nil)
   (identity-pane :application :display-function 'display-identity-tab :height 600 :width 400 :scroll-bars :vertical)
   (appearance-pane :application :display-function 'display-appearance-tab :height 600 :width 400 :scroll-bars :vertical)
   (equipment-pane :application :display-function 'display-equipment-tab :height 600 :width 400 :scroll-bars :vertical)
   (speech-pane :application :display-function 'display-speech-tab :height 600 :width 400 :scroll-bars :vertical)
   (search-bar :application :display-function 'display-search-bar :height 30 :width 400 :scroll-bars nil)
   (project-bar :application :display-function 'display-project-bar :height 30 :width 400 :scroll-bars nil))
  (:layouts
   (default (clim:vertically ()
              tab-bar
              (ecase (frame-current-tab clim:*application-frame*)
                (:identity identity-pane)
                (:appearance appearance-pane)
                (:equipment equipment-pane)
                (:speech speech-pane))
              search-bar
              project-bar))
   (:menu-bar 'char-inspector-menu-bar)))

(defmethod frame-tab-list ((frame character-inspector-frame))
  '(:identity :appearance :equipment :speech))

(defmethod initialize-instance :after ((frame character-inspector-frame) &key)
  (call-next-method)
  (populate-char-print-menu frame)
  (subscribe :printer-list-changed
             (lambda (event)
               (declare (ignore event))
               (populate-char-print-menu frame)))
  (subscribe-to-tab-events frame))

(defun subscribe-to-tab-events (frame)
  "Subscribe to eventbus events for tab changes and data updates."
  (subscribe :character-data-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

;; Tab bar display - clean outlines, no fills, clickable
(defmethod display-tab-bar ((pane clim:application-pane) (frame character-inspector-frame))
  "Draw tab bar with rounded corner outlines. Each tab is a clickable presentation."
  (let* ((tabs '(:identity :appearance :equipment :speech))
         (labels '("Identity" "Appearance" "Equipment" "Speech"))
         (n (length tabs))
         (width (or (clim:bounding-rectangle-width (clim:sheet-region pane)) 780))
         (height 30)
         (tab-width (/ width n))
         (corner 5))
    (loop for tab in tabs for label in labels for i from 0
          for left = (* i tab-width) for right = (+ left tab-width)
          for sel = (eq tab (frame-current-tab frame))
do (clim:with-drawing-options (pane :ink (if sel (clim:make-gray-color 0) (clim:make-gray-color 0.25))
                                            :stroke-width 2 :filled nil)
               (clim:draw-rectangle pane left 0 right height
                                    :corner-radii (list corner corner corner corner)))
              (clim:with-output-as-presentation (pane tab 'char-cmd :background-mode :transparent)
                (clim:draw-text pane label (clim:make-point (+ left 7) 10))))))

(defmethod display-search-bar ((pane clim:application-pane) (frame character-inspector-frame))
  "Display a filter/search field above the tab content."
  (declare (ignore pane frame))
  ;; TODO: implement search/filter across all character fields
  )

(defmethod display-project-bar ((pane clim:application-pane) (frame character-inspector-frame))
  "Display a thin project-context bar below the search bar."
  (declare (ignore pane frame))
  ;; TODO: show current project name, build target, region
  )

(clim:define-command (com-tab-select :command-table clim-internals::global-command-table
                                     :menu nil :name t)
    ((tab-symbol 'symbol :gesture :select))
  "Switch to the clicked tab in the Character Inspector."
  (let ((frame clim:*application-frame*))
    (when (typep frame 'tab-friendly-mixin)
      (switch-to-tab frame tab-symbol))))


(clim:define-command-table char-inspector-save-as-menu
  :menu (("Text..." :command com-char-save-text) ("JSON..." :command com-char-save) ("PDF..." :command com-char-save-pdf)))

(clim:define-command-table char-inspector-print-to-menu :menu ())

(clim:define-command-table char-inspector-file-menu
  :menu (("New..." :command com-char-new)
         ("Import from JSON..." :command com-char-import-json)
         ("Duplicate..." :command com-char-duplicate)
         (nil :divider :line)
         ("Save" :command com-save-resource)
         ("Version" :menu inspector-vc-menu)
         ("Save As" :menu char-inspector-save-as-menu)
         ("Send to" :menu inspector-send-to-menu)
         ("Print To" :menu char-inspector-print-to-menu)
         (nil :divider :line)
         ("Close" :command com-char-close)))

(clim:define-command-table char-inspector-edit-menu
  :menu (("Edit HP..." :command com-char-edit-hp) ("Edit Crowns..." :command com-char-edit-crowns)
                                                   ("Edit Arrows..." :command com-char-edit-arrows) ("Edit Potions..." :command com-char-edit-potions)
                                                   ("Edit Kind..." :command com-char-edit-kind)
                                                   ("Edit Skin Color..." :command com-char-edit-skin) ("Edit Hair Color..." :command com-char-edit-hair)
                                                   ("Edit Clothes Color..." :command com-char-edit-clothes)
                                                   ("Edit Speech Color..." :command com-char-edit-speech-color)
                                                   ("Edit Voice Pitch..." :command com-char-edit-pitch) ("Edit Voice Speed..." :command com-char-edit-speed)
                                                   ("Edit Voice Bend..." :command com-char-edit-bend)
                                                   ("Edit Weapon..." :command com-char-edit-weapon) ("Edit Shield..." :command com-char-edit-shield)
                                                   ("Edit Armor..." :command com-char-edit-armor)))

(clim:define-command-table char-inspector-run-menu
  :menu (("Run..." :command com-run-resource)))

(clim:define-command-table char-inspector-view-menu
  :menu (("Editable" :command com-inspector-toggle-view :toggle t)
         ("Project Pane" :command com-toggle-project-pane :toggle t)))

(clim:define-command-table char-inspector-voice-menu
  :menu (("Test on AtariVox..." :command com-char-test-atarivox :keystroke (#\t :control))))

(clim:define-command-table char-inspector-help-menu
  :menu (("How to Edit Characters..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-scripting-guide)
         (nil :divider :line) ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command (com-char-test-atarivox :menu t :name t) ()
  (let* ((frame clim:*application-frame*)
         (char (inspector-resource frame))) 
    (let ((pitch (game-resource-character-speech-pitch char))
          (speed (game-resource-character-speech-speed char))
          (bend (game-resource-character-speech-bend char))
          (phrase (frame-test-phrase frame)))
      (when phrase (setf (frame-test-phrase frame) phrase) (%test-atarivox pitch speed bend phrase)))))

(clim:define-command (com-char-close :menu t :name t) ()
  (let ((frame clim:*application-frame*)) (when (typep frame 'character-inspector-frame) (clim:frame-exit frame))))

(clim:define-command (com-char-save-text :command-table clim-internals::global-command-table
                                         :menu nil :name t)
    ()
  "Export character as plain text."
  (let* ((frame clim:*application-frame*)
         (resource (inspector-resource frame)))
    (unless resource
      (error "No character resource to export."))
    (let ((path (prompt-save-pathname (format nil "~a.txt" (game-resource-title resource))
                                      (list :dir (game-resource-kind resource) :text))))
      (when path
        (export-resource-to-text-file resource path)
        (clim-simple-echo:run-in-simple-echo (lambda () (format t "Exported text to ~a" path)))))))

(clim:define-command (com-char-save :command-table clim-internals::global-command-table
                                    :menu nil :name t)
    ()
  "Export character as JSON."
  (let* ((frame clim:*application-frame*)
         (resource (inspector-resource frame)))
    (unless resource
      (error "No character resource to export."))
    (let ((path (prompt-save-pathname (format nil "~a.sky.json" (game-resource-title resource))
                                      (list :dir (game-resource-kind resource) :json))))
      (when path
        (export-resource-to-json-file resource path)
        (clim-simple-echo:run-in-simple-echo (lambda () (format t "Exported JSON to ~a" path)))))))

(clim:define-command (com-char-save-pdf :command-table clim-internals::global-command-table
                                        :menu nil :name t)
    ()
  "Export character as PDF via direct PostScript streaming to ps2pdf."
  (let* ((frame clim:*application-frame*)
         (resource (inspector-resource frame)))
    (unless resource
      (error "No character resource to export."))
    (let ((path (prompt-save-pathname (format nil "~a.pdf" (game-resource-title resource))
                                      (list :dir (game-resource-kind resource) :pdf))))
      (when path
        (export-resource-to-pdf-file resource path)
        (clim-simple-echo:run-in-simple-echo (lambda () (format t "Exported PDF to ~a" path)))))))

(defun %char-print-to-printer (printer-name)
  "Print the current character resource to PRINTER-NAME via direct PostScript streaming.
If PRINTER-NAME is NIL, use the default printer."
  (let* ((frame clim:*application-frame*)
         (resource (when (typep frame 'character-inspector-frame)
                     (inspector-resource frame))))
    (unless resource
      (error "No character resource to print."))
    (let ((printer (or printer-name
                       (cdar *ipp-printer-registry*))))
      (if (typep printer 'ipp-printer)
          (pipe-to-ipp printer resource)
          (pipe-to-lpr printer resource))
      (clim-simple-echo:run-in-simple-echo
       (lambda () (format t "Sent ~a to printer ~a" (game-resource-title resource)
                          (if (typep printer 'ipp-printer) (ipp-name printer) printer)))))))

(defun populate-char-print-menu (&optional frame)
  (declare (ignore frame))
  (let ((ct 'char-inspector-print-to-menu))
    ;; Remove stale printer entries
    (dolist (printer *ipp-printer-registry*)
      (ignore-errors (clim:remove-menu-item-from-command-table
                      ct (ipp-name (cdr printer)))))
    (ignore-errors (clim:remove-menu-item-from-command-table ct "Default Printer (lpr)"))
    (ensure-printer-scavenger-is-running)
    (if *ipp-printer-registry*
        (dolist (printer *ipp-printer-registry*)
          (let* ((struct (cdr printer))
                 (display (ipp-name struct)))
            (clim:add-menu-item-to-command-table ct display
                                                 :command `(com-char-print-to ,struct) :after :end)))
        (clim:add-menu-item-to-command-table ct "Default Printer (lpr)"
                                             :command 'com-char-print-to-default :after :end))))
(clim:define-command (com-char-print-to :command-table clim-internals::global-command-table :menu nil :name t)
    ((printer t))
  (%char-print-to-printer printer))

(clim:define-command (com-char-print-to-default :command-table clim-internals::global-command-table :menu nil :name t)
    ()
  (%char-print-to-printer nil))

(defun open-character-inspector (character-resource)
  "Open the Character Inspector for a CHARACTER-RESOURCE.
If CHARACTER-RESOURCE is NIL, open with a new, blank character"
  (let* ((name (or (game-resource-name character-resource) ""))
         (all-items (load-item-list)) ; FIXME: listen to eventbus
         (all-keys (load-key-list)) ; FIXME: listen to eventbus
         (frame (clim:make-application-frame 'character-inspector-frame
                                             :resource character-resource
                                             :pretty-name (format nil "~a — ~a ~a" 
                                                                  name *game-title* (machine-directory-name))
                                             :all-items all-items
                                             :all-keys all-keys
                                             :width 800 :height 800)))
    (clim:run-frame-top-level frame)))


