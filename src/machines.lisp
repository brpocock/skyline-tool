(in-package :skyline-tool)

;; Machine  mappings: (NUMERIC-ID  DIRECTORY-NAME SHORT-NAME  LONG-NAME)
;; DIRECTORY-NAME  =  folder on  disk  (e.g.  "7800"), must  be  unique,
;; matches  Makefile PORT  SHORT-NAME  limited to  <=20 chars  (display)
;; LONG-NAME = full official name
(define-constant +machine-mappings+
    '((1 "Oric" "Oric-1" "Oric-1" :pal)
      (2 "A2" "Apple ][" "Apple ]["  :ntsc)
      (3 "A3" "Apple ///" "Apple ///" :ntsc)
      (8 "NES" "NES" "Nintendo Entertainment System" :ntsc :pal)
      (9 "NG" "Neo Geo" "Neo Geo" :ntsc :pal)
      (15 "F" "Channel F" "Fairchild Channel F" :ntsc :pal)
      (16 "TG16" "TurboGrafx-16" "TurboGrafx-16" :ntsc :pal)
      (20 "VIC20" "VIC-20" "Commodore VIC-20" :ntsc :pal)
      (23 "A2E" "Apple //e" "Apple //e" :ntsc :pal)
      (64 "C64" "C=64" "Commodore 64" :ntsc :pal)
      (81 "ZX81" "ZX81" "Sinclair ZX-81/Timex-Sinclair 1000" :ntsc :pal)
      (88 "SNES" "SNES" "Super Nintendo Entertainment System" :ntsc :pal)
      (128 "C128" "C=128" "Commodore 128" :ntsc :pal)
      (200 "Lynx" "Lynx" "Atari Lynx" :internal)
      (222 "2GS" "Apple //gs" "Apple //gs" :ntsc :pal)
      (223 "BBC" "BBC" "BBC Micro" :ntsc :pal)
      (264 "C16" "C=16" "Commodore 16 & Plus/4" :ntsc :pal)
      (400 "400" "Atari 400" "Atari 400" :ntsc :pal)
      (800 "800" "Atari 800" "Atari 800"  :ntsc :pal)
      (810 "VB" "Virtual Boy" "Nintendo Virtual Boy" :internal)
      (837 "GG" "Game Gear" "Sega Game Gear" :internal)
      (920 "NGP" "Neo Geo Pocket" "Neo Geo Pocket" :internal)
      (1000 "SG1000" "Sega SG-1000" "Sega SG-1000" :ntsc)
      (1080 "ST" "Atari ST" "Atari ST" :ntsc :pal)
      (1200 "1200" "Atari 1200XL" "Atari 1200XL" :ntsc :pal)
      (1601 "SMD" "Genesis" "Sega Genesis" :ntsc :pal)
      (1624 "32X" "32X" "Sega 32X" :ntsc :pal)
      (2068 "SPC" "Spectrum" "Sinclair Spectrum & Timex-Sinclair 2068" :ntsc :pal)
      (20952 "DS" "Nintendo DS" "Nintendo DS" :internal)
      (20953 "CGB" "GameBoy Color" "Nintendo GameBoy Color" :internal)
      (2416 "CDR" "Commander 16" "Commander 16 by Dave Murray" :ntsc)
      (2600 "2600" "Atari 2600" "Atari Video Computer System" :ntsc :pal :secam)
      (2609 "Intv" "Intellivision" "Intellivision" :ntsc :pal)
      (3000 "Vx" "Vectrex" "Vectrex" :internal)
      (3010 "SMS" "Master System" "Sega Master System" :ntsc)
      (3296 "GBA" "Game Boy Advance" "Nintendo Game Boy Advance" :internal)
      (35902 "DMG" "Game Boy" "Nintend Game Boy" :internal)
      (4386 "HS" "HyperScan" "Mattel HyperScan" :ntsc)
      (4800 "WS" "WonderSwan" "Bandai WonderSwan" :internal)
      (5200 "5200" "Atari 5200" "Atari SuperSystem CX-5200" :ntsc)
      (6122 "VS" "V.Smile" "VTech V.Smile" :ntsc :internal)
      (6800 "WSC" "WonderSwan Color" "Bandai WonderSwan Color" :internal)
      (7600 "O2" "Odyssey2" "Magnavox Odyssey 2" :ntsc)
      (7800 "7800" "Atari 7800" "Atari ProSystem CX-7800" :ntsc :pal)
      (7801 "SCV" "Super Cassette" "Super Cassette Vision" :ntsc :pal)
      (7850 "vcs800" "vcs800" "Atari vcs800 System" :hd)
      (8011 "JAG" "Jaguar" "Atari Jaguar" :ntsc :pal)
      (9001 "PSX" "PlayStation" "Sony PlayStation One" :ntsc :pal)
      (9918 "ClcV" "ColecoVision" "ColecoVision" :ntsc)
      (-1 "Lin" "Linux" "Linux Desktop" :hd)
      (-2 "mac" "macOS" "macOS Desktop" :hd)
      (-666 "Win" "Win" "MicroSoft Windows Desktop" :hd)
      (-10 "Android" "Android" "Android" :hd)
      (-11 "Fire" "FireTX" "Amazon FireTV" :hd)
      (-12 "iOS" "iOS" "Apple iOS" :hd)
      (-13 "iPad" "iPadOS" "Apple iPadOS" :hd)
      (-3 "BSD" "FreeBSD" "FreeBSD Desktop" :hd))
  :test 'equal
  :documentation
  "Bidirectional mappings: (numeric-id directory-name short-name long-name).
DIRECTORY-NAME is the on-disk folder and must be unique; SHORT-NAME <=20
chars; LONG-NAME official.")

(defun machine-for-port-string (dir)
  "Convert directory name (e.g. \"7800\") to numeric ID. Case-sensitive."
  (let ((found (find dir +machine-mappings+ :key #'second :test #'string=)))
    (when found
      (first found))))

(defun machine-directory-name (&optional (machine *machine*))
  "Return directory name (also used in various other filename parts) for numeric ID.
Returns NIL if MACHINE is NIL."
  (when machine
    (second (find machine +machine-mappings+ :key #'first :test #'=))))

(defun machine-short-name (&optional (machine *machine*))
  "Return short name (<=20 chars) for numeric ID."
  (third (find machine +machine-mappings+ :key #'first :test #'=)))

(defun machine-long-name (&optional (machine *machine*))
  "Return full official name for numeric ID."
  (fourth (find machine +machine-mappings+ :key #'first :test #'=)))

(defun all-regions-for-machine (&optional (machine *machine*))
  "Return the list of regions supported by *machine*.
For handhelds with internal screens, returns :internal."
  (subseq (find machine +machine-mappings+ :key #'first :test #'=) 4))
  
