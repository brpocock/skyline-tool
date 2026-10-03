;;; src/version-control/machines.lisp
;;; Machine list utilities for version control
;;; Copyright © 2024-2026 Interworldly Adventuring, LLC

(in-package :skyline-tool.version-control)

;; Machine ID to display name mapping
(defparameter +all-machines+
    '(1 2 3 8 9 15 16 20 22 23 64 81 88 128 200 222 223 264 400 800 810 837 920
      1000 1080 1200 1601 1624
      2068 2110 2416 2600 2609
      3000 3010 3296
      4386 4800
      5200
      6122 6800
      7600 7800 7850 7801
      8011
      9001 9918)
  :test 'equalp)

(defun machine-long-name (&optional (machine *machine*))
  "Return the long name for MACHINE, defaulting to *machine*."
  (ecase machine
    (1 "Oric-1")
    (2 "Apple ][ (][plus, //c, //e)")
    (3 "Apple ///")
    (8 "Nintendo Entertainment System")
    (9 "Neo Geo")
    (15 "Fairchild Channel F")
    (16 "TurboGrafx-16 (PC Engine)")
    (20 "Commodore VIC-20 (VC-20)")
    (22 "Apple //c (//c Plus)")
    (23 "Apple //e (//e Plus, //e Enhanced)")
    (64 "Commodore 64 (64C, SX-64)")
    (81 "Sinclair ZX-81 (Timex Sinclair 1000)")
    (88 "Super Nintendo Entertainment System")
    (128 "Commodore 128 (128D, 128DCR)")
    (200 "Atari Lynx")
    (222 "Apple //gs")
    (223 "Acorn British Broadcasting Corporation Microcomputer")
    (264 "Commodore Plus/4 (16)")
    (400 "Atari 400")
    (800 "Atari 800")
    (810 "Nintendo Virtual Boy")
    (837 "Sega Game Gear")
    (920 "Nokia N-Gage")
    (1000 "Sega Game 1000")
    (1080 "Atari ST/TT")
    (1200 "Atari 1200")
    (1601 "Sega Genesis (Mega Drive)")
    (1624 "Sega 32X")
    (2068 "Sinclair Spectrum (Timex Sinclair 2068)")
    (2110 "Sega Game Gear")
    (2416 "Commander X-16")
    (2600 "Atari Video Computer System CX-2600")
    (2609 "Intellivision")
    (3000 "Vectrex")
    (3010 "Sega Master System")
    (3296 "Nintendo Game Boy Advance")
    (4386 "Mattel HyperScan")
    (4800 "Bandai WonderSwan")
    (5200 "Atari Video SuperSystem CX-5200")
    (6122 "V.smile")
    (6800 "Bandai WonderSwan Color")
    (7600 "Magnavox Odyssey 2")
    (7800 "Atari Video ProSystem CX-7800")
    (7850 "Atari VCS (Linux native / bundle-gen host)")
    (7801 "Bandai SwanCrystal")
    (8011 "Atari Jaguar")
    (9001 "Sony PlayStation")
    (9918 "ColecoVision")
    (t "no particular system at all")))

(defun get-all-machines ()
  "Return alist of (display-name . machine-id) for all supported machines"
  (map 'list (lambda (id) (cons (machine-long-name id) id)) +all-machines+))