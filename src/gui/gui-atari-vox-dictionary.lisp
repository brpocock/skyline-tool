;;; Skyline-Tool src/gui/gui-atari-vox-dictionary.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

#|

Source/Tables/SpeakJet.dic - AtariVox Phonetic Dictionary

Word
-----
New...
Go to...
---
Save
Save Dictionary as > HTML...
Word Processing...
Spreadsheet...
PDF...
Text...
JSON...
SpeakJet format...
Version > [] Tracked
[] Staged
Send to > { p2p recipient list }
Print to > { printer list }
---
Close

Edit
-----
Cut
Copy
Paste


View
-----
[] Editable
---
[] Project Bar

Help
-----
How to Edit AtariVox Words...
SpeakJet Phoneme Documentation...
Skyline-Tool Developers' Guide...
Skyline-Tool Scripting Guide...

### Three panes (plus optional Project Bar pane below)
### Top pane is the word being edited
### Middle/Bottom pane is used to send the phonetics to AtariVox device
### AtariVox preferences persist, and volume & serial port sync between
### word inspectors as well as the character inspectors.

Word: _______________                           # in Unicode

Phonetic Spelling: ___________________          # in specific phonetic sequences only

================================================= pane

Pitch: <--------|--------> ____
Bend:  <--------|--------> ____
Speed: <--------|--------> ____

Volume:<--------|--------> ____
AtariVox on [ ttyUSB0 - ]              (Speak...)
              $(all serial ports on USB)

$(all phonetic sequences in a grid as a reminder)
|#


(clim:define-presentation-type game-resource-atari-vox-dictionary-reference ()
  :inherit-from 'game-resource-atari-vox-dictionary)

(defun open-atari-vox-dictionary-inspector (resource)
  (open-resource-inspector (or resource (make-instance 'game-resource-atari-vox-dictionary
                                                       :kind "AtariVox Dictionary"
                                                       :moniker "AtariVox Dictionary/SpeakJet.dic")) :editing))

(defmethod open-resource-inspector ((resource game-resource-atari-vox-dictionary) &optional (mode :editing))
  (open-atari-vox-dictionary-inspector resource))

(defmethod present-reading ((resource game-resource-atari-vox-dictionary) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Word: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-title resource))))
    ;; Show the raw file content for reading
    (when (typep resource 'game-resource-from-file)
      (let ((path (game-resource-full-path resource)))
        (when (and path (probe-file path))
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream :align-x :right)
              (format stream "Path: "))
            (clim:formatting-cell (stream :align-x :left)
              (format stream "~a" path))))))))

(defmethod present-editing ((resource game-resource-atari-vox-dictionary) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
        (fixme-interactive-editing-gadget-with-validation stream resource 'game-resource-title)))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Kind: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a (read-only)" (game-resource-kind resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Word: "))
      (clim:formatting-cell (stream :align-x :left)
        (insert-gadget stream
                       :label nil
                       :variable (game-resource-title resource)
                       :activation-callback
                       (lambda (gadget)
                         (declare (ignore gadget))
                         (format t "~&AtariVox dictionary editing not yet saving to file~%")))))))
