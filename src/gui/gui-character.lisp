;;; Skyline-Tool src/gui/gui-character.lisp
;;; Character Inspector with full CLIM integration


#|

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

Gender: <> Male (he)
<> Female (she)
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

Course Class: [ $(Course Classes) - ]   ;

Prototype: <> Zeroes
<> ___________________ (Choose...)   # String entry of symbol

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

|#


(in-package :skyline-tool)

;; Per-resource save-as menu for Character Inspector
(clim:define-command-table character-save-as-menu
  :menu (("Text..." :command com-character-save-text)
         ("JSON..." :command com-character-save-json)
         ("PDF..." :command com-character-save-pdf)))

(clim:define-command-table character-file-menu
  :menu (("New..." :command com-character-new)
         ("Import..." :command com-character-import)
         (nil :divider :line)
         ("Save" :command com-character-save)
         ("Version" :menu inspector-version-control-menu)
         ("Save as" :menu character-save-as-menu)
         (nil :divider :line)
         ("Print to" :menu inspector-print-to-menu)
         ("Send to" :menu inspector-send-to-menu)
         (nil :divider :line)
         ("Close" :command com-close-frame)))

(clim:define-command-table character-edit-menu
  :menu (("Cut" :command com-cut)
         ("Copy" :command com-copy)
         ("Paste" :command com-paste)
         (nil :divider :line)
         ("Find..." :command com-find)))

(clim:define-command-table character-view-menu
  :menu (("☐ Editable" :command com-inspector-toggle-view :name t)
         (nil :divider :line)
         ("☑ Project Pane" :command com-toggle-project-pane :name t)))

(clim:define-command-table character-run-menu
  :menu (("Build" :menu inspector-build-menu)
         ("Region" :menu inspector-region-menu)
         (nil :divider :line)
         ("Export to AtariVox..." :command com-character-export-atari-vox)))

(clim:define-command-table character-help-menu
  :menu (("How to Manage Characters..." :command com-help-for-window)
         ("Skyline-Tool Developers' Guide..." :command com-open-dev-guide)
         ("Skyline-Tool Scripting Guide..." :command com-open-fountain-manual)
         (nil :divider :line)
         ("About Skyline-Tool..." :command com-about-skyline-tool)))

(clim:define-command-table character-menu-bar
  :menu (("Character" :menu character-file-menu)
         ("Edit" :menu character-edit-menu)
         ("Run" :menu character-run-menu)
         ("View" :menu character-view-menu)
         ("Help" :menu character-help-menu)))

;; Tabbed interface for Character Inspector
(clim:define-application-frame character-inspector-frame (gui-inspector-frame clim:standard-application-frame)
  ((resource :initarg :resource :reader frame-resource)
   (current-tab :initarg :current-tab :accessor frame-current-tab :initform :identity))
  (:menu-bar character-menu-bar)
  (:panes
   (tab-bar :application :display-function 'display-tab-bar
                         :height 30 :width 400
                         :scroll-bars nil)
   (main-pane :application
              :display-function 'display-character-inspector
              :height 700 :width 400
              :scroll-bars :vertical)
   (find-bar :application :display-function 'display-find-bar
                          :height 30 :width 400
                          :scroll-bars nil)
   (project-bar :application :display-function 'display-project-bar
                             :height 30 :width 400
                             :scroll-bars nil))
  (:layouts
   (default (clim:vertically () tab-bar main-pane))
   (searching (clim:vertically () tab-bar main-pane find-bar))
   (searching+project (clim:vertically () tab-bar main-pane find-bar project-bar))
   (project (clim:vertically () tab-bar main-pane project-bar)))
  (:icon (skyline-tool-icon :resource :character))
  (:pretty-name "Character Inspector"))

(defmethod initialize-instance :after ((frame character-inspector-frame) &key)
  (call-next-method)
  (subscribe :resource-changed
             (lambda (event)
               (declare (ignore event))
               (ignore-errors (clim:redisplay-frame-panes frame :force-p t)))))

;; Tab switching commands
(clim:define-command (com-switch-to-identity :command-table clim-internals::global-command-table
                                                :menu t :name t)
  ()
  (let ((frame clim:*application-frame*))
    (when frame
      (setf (frame-current-tab frame) :identity)
      (clim:redisplay-frame-panes frame :force-p t))))

(clim:define-command (com-switch-to-appearance :command-table clim-internals::global-command-table
                                                :menu t :name t)
  ()
  (let ((frame clim:*application-frame*))
    (when frame
      (setf (frame-current-tab frame) :appearance)
      (clim:redisplay-frame-panes frame :force-p t))))

(clim:define-command (com-switch-to-equipment :command-table clim-internals::global-command-table
                                               :menu t :name t)
  ()
  (let ((frame clim:*application-frame*))
    (when frame
      (setf (frame-current-tab frame) :equipment)
      (clim:redisplay-frame-panes frame :force-p t))))

(clim:define-command (com-switch-to-speech :command-table clim-internals::global-command-table
                                            :menu t :name t)
  ()
  (let ((frame clim:*application-frame*))
    (when frame
      (setf (frame-current-tab frame) :speech)
      (clim:redisplay-frame-panes frame :force-p t))))

(defun open-character-inspector (resource)
  (open-resource-inspector (or resource
                               (make-instance 'game-resource-character))
                           :editing))

(defmethod open-resource-inspector ((resource game-resource-character)
                                    &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'character-inspector-frame
                                :resource resource
                                :view-mode mode
                                :pretty-name (format nil "~a — ~a ~a"
                                                     (game-resource-title resource)
                                                     *game-title* (machine-directory-name)))))

;; Tab bar display
(defgeneric display-tab-bar (pane frame))

(defmethod display-tab-bar ((pane clim:application-pane) (frame character-inspector-frame))
  "Draw custom tab bar with rounded corners and styling for Character Inspector."
  (let* ((tabs '(:identity :appearance :equipment :speech))
         (tab-labels '("Identity" "Appearance" "Equipment" "Speech"))
         (n (length tabs))
         (width (or (clim:bounding-rectangle-width (clim:sheet-region pane)) 400))
         (height 30)
         (tab-width (/ width n))
         (corner-radius 5))
    (loop for tab in tabs
          for label in tab-labels
          for i from 0
          for left = (* i tab-width)
          for right = (+ left tab-width)
          for selected = (eq tab (frame-current-tab frame))
          do (clim:with-drawing-options (pane
                                          :ink (if selected
                                                 (clim:make-gray-color 0) ; black for selected
                                                 (clim:make-gray-color 0.75))) ; 75% gray for unselected
               (clim:draw-rectangle pane left 0 right height
                                    :filled t
                                    :corner-radii (list corner-radius corner-radius corner-radius corner-radius)))
          ;; Draw label text
          (let* ((text-x (if selected
                           (+ left 5) ; slight inset for selected
                           (+ left 10))) ; 5px offset for unselected
                 (text-y 8))
            (clim:with-drawing-options (pane
                                        :ink (if selected
                                               (clim:make-gray-color 1) ; white text on black
                                               (clim:make-gray-color 0))) ; black text on gray
              (clim:draw-text pane label text-x text-y))))))
(defmethod display-current-tab ((frame character-inspector-frame) pane)
  "Display the current tab based on frame-current-tab slot."
  (let* ((resource (frame-resource frame))
         (tab (frame-current-tab frame))
         (*standard-output* pane))
    (when resource
      (ecase tab
        (:identity (display-identity-tab frame pane))
        (:appearance (display-appearance-tab frame pane))
        (:equipment (display-equipment-tab frame pane))
        (:speech (display-speech-tab frame pane))))))

;; Tab content display methods - READING mode (read-only)
(defmethod display-appearance-tab (frame pane)
  (let ((resource (frame-resource frame)))
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Hair Color: "))
        (clim:formatting-cell (pane :align-x :left)
          (destructuring-bind (name r g b)
              (elt *common-palette* (game-resource-character-hair-color resource))
            (print-wide-pixel (rgb->palette r g b) pane)
            (format pane "  ~a" (title-case (string name))))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Skin Color: "))
        (clim:formatting-cell (pane :align-x :left)
          (destructuring-bind (name r g b)
              (elt *common-palette* (game-resource-character-skin-color resource))
            (print-wide-pixel (rgb->palette r g b) pane)
            (format pane "  ~a" (title-case (string name))))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Clothes Color: "))
        (clim:formatting-cell (pane :align-x :left)
          (destructuring-bind (name r g b)
              (elt *common-palette* (game-resource-character-clothes-color resource))
            (print-wide-pixel (rgb->palette r g b) pane)
            (format pane "  ~a" (title-case (string name))))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Head: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-head resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Body: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-body resource)))))))

(defmethod display-equipment-tab (frame pane)
  (let ((resource (frame-resource frame)))
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Equipment: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-equipment resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Shield: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-shield resource)))))))

;; Identity tab - read-only display
(defmethod display-identity-tab ((frame character-inspector-frame) pane)
  (let ((resource (frame-resource frame)))
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Name: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-name resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "ID: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~d" (game-resource-character-id resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Decal: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-decal resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Gender: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-gender resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Home: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (or (game-resource-character-home resource) "None"))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Comments: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-memo resource)))))))

;; Speech tab - read-only display
(defmethod display-speech-tab ((frame character-inspector-frame) pane)
  (let ((resource (frame-resource frame)))
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Speech Pitch: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-speech-pitch resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Speech Speed: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-speech-speed resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Speech Bend: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-speech-bend resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Speech Color: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "[~2,'0X]" (game-resource-character-speech-color resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Nicks: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-nicks resource)))))))

;; Presentation types
(clim:define-presentation-type game-resource-character-reference ()
  :inherit-from 'game-resource-reference)

(clim:define-presentation-type game-resource-character-editable ()
  :inherit-from 'game-resource-editable)

(clim:define-presentation-type game-resource-character-viewing ()
  :inherit-from 'game-resource-viewing)

;; Reference Presentation for Character Resources
(clim:define-presentation-method clim:present ((resource game-resource-character) (type (eql 'game-resource-character-reference)) stream view &key)
  (declare (ignore view))
  (clim:with-output-as-presentation (stream resource 'game-resource-character-reference)
    (clim:formatting-table (stream)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 0)
          (format stream "~3%"))
        (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 125)
          (game-resource-present-icon resource stream))
        (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 150)
          (clim:with-text-size (stream :larger)
            (clim:with-text-face (stream :bold)
              (game-resource-present-title resource stream)))
          (format stream "~%~5t")
          (clim:with-text-size (stream :smaller)
            (clim:with-drawing-options (stream :ink (clim:make-gray-color 0.75))
              (game-resource-present-subheading resource stream))))
        (clim:formatting-cell (stream :align-x :right :align-y :top :min-height 90 :min-width 125)
          (game-resource-present-right-margin resource stream))))))

;; Viewing Presentation - READ-ONLY display of character resources
(clim:define-presentation-method clim:present ((resource game-resource-character) (type (eql 'game-resource-character-viewing)) stream view &key)
  (declare (ignore view))
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-name resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "ID: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~d" (game-resource-character-id resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Decal: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-decal resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Gender: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-gender resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "HP: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~d / ~d" (game-resource-character-hp resource) (game-resource-character-max-hp resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "AC: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~d" (game-resource-character-ac resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Crowns: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~d" (game-resource-character-crowns resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Arrows: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~d" (game-resource-character-arrows resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Potions: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~d" (game-resource-character-potions resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Chalice: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-chalice resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Hair Color: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "[~2,'0X]" (game-resource-character-hair-color resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Skin Color: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "[~2,'0X]" (game-resource-character-skin-color resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Clothes Color: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "[~2,'0X]" (game-resource-character-clothes-color resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Head: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-head resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Body: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-body resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Equipment: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-equipment resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Shield: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-shield resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Speech Pitch: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-speech-pitch resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Speech Speed: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-speech-speed resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Speech Bend: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-speech-bend resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Speech Color: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "[~2,'0X]" (game-resource-character-speech-color resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Nicks: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-nicks resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right) (format stream "Memo: "))
      (clim:formatting-cell (stream :align-x :left) (format stream "~a" (game-resource-character-memo resource))))))

(clim:define-presentation-method clim:present ((resource game-resource-character) (type (eql 'game-resource-character-editable)) stream view &key)
  (let ((frame clim:*application-frame*))
    (display-current-tab frame stream))
  (let* ((name (game-resource-character-name resource))
         (char-id (game-resource-character-id resource))
         (decal (game-resource-character-decal resource))
         (gender (game-resource-character-gender resource))
         (hp (game-resource-character-hp resource))
         (max-hp (game-resource-character-max-hp resource))
         (ac (game-resource-character-ac resource))
         (crowns (game-resource-character-crowns resource))
         (arrows (game-resource-character-arrows resource))
         (potions (game-resource-character-potions resource))
         (chalice (game-resource-character-chalice resource))
         (hair-color (game-resource-character-hair-color resource))
         (skin-color (game-resource-character-skin-color resource))
         (clothes-color (game-resource-character-clothes-color resource))
         (head (game-resource-character-head resource))
         (body (game-resource-character-body resource))
         (equipment (game-resource-character-equipment resource))
         (shield (game-resource-character-shield resource))
         (speech-pitch (game-resource-character-speech-pitch resource))
         (speech-speed (game-resource-character-speech-speed resource))
         (speech-bend (game-resource-character-speech-bend resource))
         (speech-color (game-resource-character-speech-color resource))
         (nicks (game-resource-character-nicks resource))
         (memo (game-resource-character-memo resource))) 
    (clim:formatting-table (stream)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Name: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:text-field-pane
                          :value name
                          :activation-callback
                          (lambda (pane)
                            (let ((new-value (clim:gadget-value pane)))
                              (when (validate-minifont-name new-value 12)
                                (setf (game-resource-character-name resource) new-value)
                                (publish-resource-changed resource)))))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "ID: "))
        (clim:formatting-cell (stream :align-x :left) (format stream "~d (read-only)" char-id)))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Decal: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:text-field-pane
                          :value decal
                          :activation-callback
                          (lambda (pane)
                            (setf (game-resource-character-decal resource) (clim:gadget-value pane))
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Gender: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:radio-box-pane
                          :items '(:male :female :other)
                          :current-value gender
                          :callback
                          (lambda (pane value)
                            (declare (ignore pane))
                            (setf (game-resource-character-gender resource) value)
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "HP: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:slider-pane
                          :value hp
                          :min-value 0
                          :max-value max-hp
                          :callback
                          (lambda (pane value)
                            (declare (ignore pane))
                            (setf (game-resource-character-hp resource) value)
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Max HP: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:slider-pane
                          :value max-hp
                          :min-value 1
                          :max-value 255
                          :callback
                          (lambda (pane value)
                            (declare (ignore pane))
                            (setf (game-resource-character-max-hp resource) value)
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "AC: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:slider-pane
                          :value ac
                          :min-value -10
                          :max-value 20
                          :callback
                          (lambda (pane value)
                            (declare (ignore pane))
                            (setf (game-resource-character-ac resource) value)
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Crowns: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:slider-pane
                          :value crowns
                          :min-value 0
                          :max-value 9999
                          :callback
                          (lambda (pane value)
                            (declare (ignore pane))
                            (setf (game-resource-character-crowns resource) value)
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Arrows: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:slider-pane
                          :value arrows
                          :min-value 0
                          :max-value 255
                          :callback
                          (lambda (pane value)
                            (declare (ignore pane))
                            (setf (game-resource-character-arrows resource) value)
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Potions: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:slider-pane
                          :value potions
                          :min-value 0
                          :max-value 99
                          :callback
                          (lambda (pane value)
                            (declare (ignore pane))
                            (setf (game-resource-character-potions resource) value)
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Chalice: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:check-box-pane
                          :value chalice
                          :callback
                          (lambda (pane value)
                            (declare (ignore pane))
                            (setf (game-resource-character-chalice resource) value)
                            (publish-resource-changed resource))))))
    (clim:formatting-table (stream)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Name: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:text-field-pane
                          :value name
                          :activation-callback
                          (lambda (pane)
                            (let ((new-value (clim:gadget-value pane)))
                              (when (validate-minifont-name new-value 12)
                                (setf (game-resource-character-name resource) new-value)
                                (publish-resource-changed resource)))))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "ID: "))
        (clim:formatting-cell (stream :align-x :left) (format stream "~d (read-only)" char-id)))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Decal: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:text-field-pane
                          :value decal
                          :activation-callback
                          (lambda (pane)
                            (setf (game-resource-character-decal resource) (clim:gadget-value pane))
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Gender: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:radio-box-pane
                          :items '(:male :female :other)
                          :current-value gender
                          :callback
                          (lambda (pane value)
                            (declare (ignore pane))
                            (setf (game-resource-character-gender resource) value)
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "HP: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:slider-pane
                          :value hp
                          :min-value 0
                          :max-value max-hp
                          :callback
                          (lambda (pane value)
                            (declare (ignore pane))
                            (setf (game-resource-character-hp resource) value)
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Max HP: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:slider-pane
                          :value max-hp
                          :min-value 1
                          :max-value 255
                          :callback
                          (lambda (pane value)
                            (declare (ignore pane))
                            (setf (game-resource-character-max-hp resource) value)
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "AC: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:slider-pane
                          :value ac
                          :min-value -10
                          :max-value 20
                          :callback
                          (lambda (pane value)
                            (declare (ignore pane))
                            (setf (game-resource-character-ac resource) value)
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Crowns: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:slider-pane
                          :value crowns
                          :min-value 0
                          :max-value 9999
                          :callback
                          (lambda (pane value)
                            (declare (ignore pane))
                            (setf (game-resource-character-crowns resource) value)
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Arrows: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:slider-pane
                          :value arrows
                          :min-value 0
                          :max-value 255
                          :callback
                          (lambda (pane value)
                            (declare (ignore pane))
                            (setf (game-resource-character-arrows resource) value)
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Potions: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:slider-pane
                          :value potions
                          :min-value 0
                          :max-value 99
                          :callback
                          (lambda (pane value)
                            (declare (ignore pane))
                            (setf (game-resource-character-potions resource) value)
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Chalice: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:check-box-pane
                          :value chalice
                          :callback
                          (lambda (pane value)
                            (declare (ignore pane))
                            (setf (game-resource-character-chalice resource) value)
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Hair Color: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim::menu-button
                          :value hair-color
                          :menu-items (lambda (stream)
                                        (declare (ignore stream))
                                        (let ((colors (assocdr :*common-palette *project.json*)))
                                          (clim:menu-choose
                                           (loop for color in colors
                                                 with p = 0
                                                 for c = 0 then (if (> c 2)
                                                                    (prog1 0 (incf p))
                                                                    (1+ c))
                                                 for i = (+ c (* p 3))
                                                 for (name r g b) = color
                                                 collect
                                                 (list (format nil "[#~2,'0x~2,'0x~2,'0x] ~a"
                                                               r g b (title-case (string name)))
                                                       :value i
                                                       :current-p (= i hair-color)
                                                       :command
                                                       (lambda (item stream)
                                                         (declare (ignore stream))
                                                         (setf (game-resource-character-hair-color resource)
                                                               (clim:gadget-value item))
                                                         (publish-resource-changed resource))))))))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Skin Color: "))
        (clim:formatting-cell (stream :align-x :left)
          (error "unimplemented")))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Clothes Color: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:slider-pane
                          :value clothes-color
                          :min-value 0
                          :max-value 15
                          :callback
                          (lambda (pane value)
                            (declare (ignore pane))
                            (setf (game-resource-character-clothes-color resource) value)
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Head: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:text-field-pane
                          :value head
                          :activation-callback
                          (lambda (pane)
                            (setf (game-resource-character-head resource) (clim:gadget-value pane))
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Body: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:text-field-pane
                          :value body
                          :activation-callback
                          (lambda (pane)
                            (setf (game-resource-character-body resource) (clim:gadget-value pane))
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Equipment: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:text-field-pane
                          :value equipment
                          :activation-callback
                          (lambda (pane)
                            (setf (game-resource-character-equipment resource) (clim:gadget-value pane))
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Shield: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:text-field-pane
                          :value shield
                          :activation-callback
                          (lambda (pane)
                            (setf (game-resource-character-shield resource) (clim:gadget-value pane))
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Speech Pitch: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:slider-pane
                          :value speech-pitch
                          :min-value 0
                          :max-value 15
                          :callback
                          (lambda (pane value)
                            (declare (ignore pane))
                            (setf (game-resource-character-speech-pitch resource) value)
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Speech Speed: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:slider-pane
                          :value speech-speed
                          :min-value 0
                          :max-value 10
                          :callback
                          (lambda (pane value)
                            (declare (ignore pane))
                            (setf (game-resource-character-speech-speed resource) value)
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Speech Bend: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:slider-pane
                          :value speech-bend
                          :min-value -2
                          :max-value 2
                          :callback
                          (lambda (pane value)
                            (declare (ignore pane))
                            (setf (game-resource-character-speech-bend resource) value)
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Speech Color: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:radio-box-pane
                          :items '(:white :red :green :blue :yellow :cyan :magenta)
                          :current-value speech-color
                          :callback
                          (lambda (pane value)
                            (declare (ignore pane))
                            (setf (game-resource-character-speech-color resource) value)
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Nicks: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:text-field-pane
                          :value nicks
                          :activation-callback
                          (lambda (pane)
                            (setf (game-resource-character-nicks resource) (clim:gadget-value pane))
                            (publish-resource-changed resource)))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (format stream "Memo: "))
        (clim:formatting-cell (stream :align-x :left)
          (clim:make-pane 'clim:text-field-pane
                          :value memo
                          :activation-callback
                          (lambda (pane)
                            (setf (game-resource-character-memo resource) (clim:gadget-value pane))
                            (publish-resource-changed resource))))))))

(defun character-to-plist (resource)
  "Convert character resource to plist for JSON export."
  (list :name (game-resource-character-name resource)
        :id (game-resource-character-id resource)
        :decal (game-resource-character-decal resource)
        :gender (game-resource-character-gender resource)
        :hp (game-resource-character-hp resource)
        :max-hp (game-resource-character-max-hp resource)
        :ac (game-resource-character-ac resource)
        :crowns (game-resource-character-crowns resource)
        :arrows (game-resource-character-arrows resource)
        :potions (game-resource-character-potions resource)
        :chalice (game-resource-character-chalice resource)
        :hair-color (game-resource-character-hair-color resource)
        :skin-color (game-resource-character-skin-color resource)
        :clothes-color (game-resource-character-clothes-color resource)
        :head (game-resource-character-head resource)
        :body (game-resource-character-body resource)
        :equipment (game-resource-character-equipment resource)
        :shield (game-resource-character-shield resource)
        :speech-pitch (game-resource-character-speech-pitch resource)
        :speech-speed (game-resource-character-speech-speed resource)
        :speech-bend (game-resource-character-speech-bend resource)
        :speech-color (game-resource-character-speech-color resource)
        :nicks (game-resource-character-nicks resource)
        :memo (game-resource-character-memo resource)))

(defun character-to-text (resource)
  "Convert character resource to plain text."
  (with-output-to-string (s)
    (format s "~&Character: ~a~%" (game-resource-character-name resource))
    (format s "ID: ~d~%" (game-resource-character-id resource))
    (format s "Decal: ~a~%" (game-resource-character-decal resource))
    (format s "Gender: ~a~%" (game-resource-character-gender resource))
    (format s "HP: ~d / ~d~%" (game-resource-character-hp resource) (game-resource-character-max-hp resource))
    (format s "AC: ~d~%" (game-resource-character-ac resource))
    (format s "Crowns: ~d~%" (game-resource-character-crowns resource))
    (format s "Arrows: ~d~%" (game-resource-character-arrows resource))
    (format s "Potions: ~d~%" (game-resource-character-potions resource))
    (format s "Chalice: ~a~%" (game-resource-character-chalice resource))
    (format s "Hair Color: ~a~%" (game-resource-character-hair-color resource))
    (format s "Skin Color: ~a~%" (game-resource-character-skin-color resource))
    (format s "Clothes Color: ~a~%" (game-resource-character-clothes-color resource))
    (format s "Head: ~a~%" (game-resource-character-head resource))
    (format s "Body: ~a~%" (game-resource-character-body resource))
    (format s "Equipment: ~a~%" (game-resource-character-equipment resource))
    (format s "Shield: ~a~%" (game-resource-character-shield resource))
    (format s "Speech Pitch: ~a~%" (game-resource-character-speech-pitch resource))
    (format s "Speech Speed: ~a~%" (game-resource-character-speech-speed resource))
    (format s "Speech Bend: ~a~%" (game-resource-character-speech-bend resource))
    (format s "Speech Color: ~a~%" (game-resource-character-speech-color resource))
    (format s "Nicks: ~a~%" (game-resource-character-nicks resource))
    (format s "Memo: ~a~%" (game-resource-character-memo resource))))

(defun character-to-postscript (resource)
  "Convert character resource to PostScript."
  (with-output-to-string (s)
    (format s "%%!PS-Adobe-3.0~%")
    (format s "%%Title: ~a~%" (game-resource-character-name resource))
    (format s "%%Creator: Skyline-Tool~%")
    (format s "%%Pages: 1~%")
    (format s "%%EndComments~%")
    (format s "/Helvetica findfont 12 scalefont setfont~%")
    (format s "72 720 moveto~%")
    (format s "(Character: ~a) show~%" (game-resource-character-name resource))
    (format s "72 700 moveto~%")
    (format s "(ID: ~d) show~%" (game-resource-character-id resource))
    (format s "72 680 moveto~%")
    (format s "(HP: ~d / ~d) show~%" (game-resource-character-hp resource) (game-resource-character-max-hp resource))
    (format s "72 660 moveto~%")
    (format s "(AC: ~d) show~%" (game-resource-character-ac resource))
    (format s "72 640 moveto~%")
    (format s "(Crowns: ~d) show~%" (game-resource-character-crowns resource))
    (format s "72 620 moveto~%")
    (format s "(Arrows: ~d) show~%" (game-resource-character-arrows resource))
    (format s "72 600 moveto~%")
    (format s "(Potions: ~d) show~%" (game-resource-character-potions resource))
    (format s "72 580 moveto~%")
    (format s "(Chalice: ~a) show~%" (game-resource-character-chalice resource))
    (format s "72 560 moveto~%")
    (format s "(Equipment: ~a) show~%" (game-resource-character-equipment resource))
    (format s "72 540 moveto~%")
    (format s "(Shield: ~a) show~%" (game-resource-character-shield resource))
    (format s "72 520 moveto~%")
    (format s "(Speech Pitch: ~a) show~%" (game-resource-character-speech-pitch resource))
    (format s "72 500 moveto~%")
    (format s "(Speech Speed: ~a) show~%" (game-resource-character-speech-speed resource))
    (format s "72 480 moveto~%")
    (format s "(Speech Bend: ~a) show~%" (game-resource-character-speech-bend resource))
    (format s "72 460 moveto~%")
    (format s "(Speech Color: ~a) show~%" (game-resource-character-speech-color resource))
    (format s "showpage~%")))

;; Save commands with atomic replacement
(defun write-character-atomically (resource)
  "Write character resource atomically using temp file + rename."
  (let ((path (first (game-resource-pathnames resource))))
    (when path
      (uiop/stream:with-temporary-file (:stream temp-stream :pathname temp-path
                                        :direction :output)
        (write-resource-to-stream resource temp-stream)
        (uiop:rename-file-overwriting-target temp-path path))
      (publish :resource-changed :payload resource)
      path)))

(defun write-resource-to-stream (resource stream)
  "Write character resource data to stream in ODS-compatible format."
  ;; This would write to the ODS format used by NPCStats.ods
  ;; For now, emit a simple text representation
  (format stream "~a" (character-to-text resource)))

(clim:define-command (com-character-save :command-table clim-internals::global-command-table :menu t :name t)
    ()
  ()
  "Save the character resource"
  (let ((frame clim:*application-frame*))
    (when frame
      (let ((resource (frame-resource frame)))
        (when resource
          (write-character-atomically resource)
          (clim-simple-echo:run-in-simple-echo
           (lambda () (format t "~&Saved ~a~%" (game-resource-title resource)))))))))

(clim:define-command (com-character-save-text :command-table clim-internals::global-command-table :menu t :name t)
    ()
  ()
  "Export character as plain text"
  (let* ((frame clim:*application-frame*)
         (resource (frame-resource frame))
         (path (prompt-save-pathname (format nil "~a.txt" (game-resource-title resource))
                                     (list :dir (game-resource-kind resource) :text))))
    (when path
      (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8)
        (princ (character-to-text resource) s)))))

(clim:define-command (com-character-save-json :command-table clim-internals::global-command-table :menu t :name t)
    ()
  ()
  "Export character as JSON"
  (let* ((frame clim:*application-frame*)
         (resource (frame-resource frame))
         (path (prompt-save-pathname (format nil "~a.json" (game-resource-title resource))
                                     (list :dir (game-resource-kind resource) :json))))
    (when path
      (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8)
        (json:encode-json (character-to-plist resource) s))
      (clim-simple-echo:run-in-simple-echo
       (lambda () (format t "~&Exported character as JSON to ~a~%" path))))))

(clim:define-command (com-character-save-pdf :command-table clim-internals::global-command-table :menu t :name t)
    ()
  ()
  "Export character as PDF via PostScript"
  (let* ((frame clim:*application-frame*)
         (resource (frame-resource frame))
         (pdf-path (prompt-save-pathname (format nil "~a.pdf" (game-resource-title resource))
                                         (list :dir (game-resource-kind resource) :pdf))))
    (when pdf-path
      (uiop:run-program (list "ps2pdf" :input (character-to-postscript resource)
                              pdf-path))
      (xdg-open pdf-path))))

(defun xdg-open (&rest args)
  (uiop:run-program (append (list "xdg-open") args)))

;; AtariVox integration


(defun parse-usb-serial-port (line)
  "Parse a lsusb line for serial port path."
  (let ((pos (search "/dev/" line)))
    (when pos
      (let ((end (or (position #\Space line :start pos) (length line))))
        (subseq line pos end)))))

(defun send-to-atari-vox (port byte-array)
  "Send byte array to AtariVox on specified serial port."
  (handler-case
      (uiop:with-temporary-file (:stream s :pathname p :direction :output
                                 :element-type '(unsigned-byte 8))
        (write-sequence byte-array s)
        (finish-output s)
        (uiop:run-program (list "cat" p ">" port) :input nil :output nil :error-output nil))
    (error (e)
      (format *error-output* "~&Failed to send to AtariVox on ~a: ~a~%" port e)
      nil)))

(defun read-speech-text ()
  "Read speech text from a CLIM text input pane."
  (error "unimplemented"))

(defun choose-serial-port (ports)
  "Present available serial ports via CLIM input gadget and return selected port."
  (let ((selected (first ports)))
    (dolist (p ports)
      (format t "  ~a~%" p))
    (format t "Select port: ")
    (let ((choice (error "unimplemented")))
      (when (find choice ports :test 'equal)
        (setf selected choice)))
    selected))

(defun encode-speech-for-atari-vox (text &key pitch speed bend volume)
  (error "implementation was destroyed by an idiot robot"))

(clim:define-command (com-character-export-atari-vox
                      :command-table clim-internals::global-command-table :menu t :name t)
    ()
  ()
  "Export current speech to AtariVox via serial port"
  (let* ((resource (frame-resource clim:*application-frame*))
         (ports (available-serial-ports))
         (byte-array (when (and resource ports)
                       (let* ((port (error "unimplemented"))
                              (text (error "unimplemented"))
                              (pitch (game-resource-character-speech-pitch resource))
                              (speed (game-resource-character-speech-speed resource))
                              (bend (game-resource-character-speech-bend resource))
                              (volume (error "unimplemented"))
                              (bytes (encode-speech-for-atari-vox text
                                                                  :pitch pitch
                                                                  :speech speed
                                                                  :bend bend
                                                                  :volume volume)))
                         (when (and port text)
                           (when (send-to-atari-vox port bytes)
                             (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Sent ~d bytes to AtariVox on ~a~%" (length bytes) port)))))))))))

(clim:define-command (com-character-new :command-table clim-internals::global-command-table :menu t :name t)
    ()
  (open-character-inspector nil))

;; Import command (stub)
(clim:define-command (com-character-import :command-table clim-internals::global-command-table :menu t :name t)
    ()
  (clim-simple-echo:run-in-simple-echo (lambda () (format t "~&Import functionality not yet implemented~%"))))

(defmethod game-resource-action-menu ((resource game-resource-character))
  (list (make-menu-item "Inspect..." (lambda () (open-character-inspector resource)))))

(defun validate-minifont-name (name max-bytes)
  "Validate that NAME encodes to MAX-BYTES or fewer in minifont.
Signals an error if the name is too long."
  (let* ((encoded (unicode->minifont name))
         (len (length encoded)))
    (<= len max-bytes)))

(define-condition minifont-name-too-long (error)
  ((name :initarg :name :reader error-name)
   (encoded-length :initarg :encoded-length :reader error-encoded-length)
   (max-length :initarg :max-length :reader error-max-length))
  (:report (lambda (condition stream)
             (format stream "Name ~a encodes to ~d bytes (max ~d)"
                     (error-name condition)
                     (error-encoded-length condition)
                     (error-max-length condition)))
   (:documentation "Signalled when a minifont name exceeds the byte limit.")))
