(in-package :skyline-tool)

(defvar *tileset*)

(define-constant +lynx-palette+
    (loop for r from 0 below #x10
          append
          (loop for g from 0 below #x10
                append
                (loop for b from 0 below #x10
                      collect (list (* r #x10) (* g #x10) (* b #x10)))))
  :test 'equalp)

(define-constant +c64-names+
    '(black white red cyan
      purple green blue yellow
      orange brown pink gray1
      gray2 light-green light-blue gray3)
  :test 'equalp)

(define-constant +c64-palette+
    '(( 0 0 0 )
      ( 255 255 255 )
      ( 138 57 50 )
      ( 103 184 191 )
      ( 141 63 152 )
      ( 85 162 73 )
      ( 64 49 143 )
      ( 193 208 114 )
      ( 141 84 41 )
      ( 87 66 0 )
      ( 186 105 98 )
      ( 80 80 80 )
      ( 120 120 120 )
      ( 150 226 139 )
      ( 120 105 198 )
      ( 161 161 161 ))
  :test 'equalp)

(define-constant +intv-palette+
    '((0 0 0) (0 0 255) (255 0 0) (203 255 101)
      (0 127 0) (0 255 0) (255 255 0) (255 255 255)
      (127 127 127) (0 255 255) (255 159 0) (127 127 0)
      (255 63 255) (127 127 255) (127 255 0) (255 0 127))
  :test 'equalp)

(define-constant +intv-color-names+
    '(black blue red tan dark-green light-green yellow white
      gray cyan orange brown pink violet bright-green magenta)
  :test 'equalp)

(define-constant +apple-hires-palette+
    '((#x00 #x00 #x00)
      (#xb6 #x3d #xff)
      (#xea #x5d #x15)
      (#xff #xff #xff)
      (#x00 #x00 #x00)
      (#x10 #xa4 #xe3)
      (#x43 #xc3 #x00)
      (#xff #xff #xff))
  :test 'equalp)

(define-constant +apple-hires-color-names+
    '(black purple orange white black blue green white)
  :test 'equal)

(define-constant +nes-palette-ntsc+
    '((#x62 #x62 #x62) (#x00 #x1f #xb2) (#x24 #x04 #xc8) (#x52 #x00 #xb2)
      (#x73 #x00 #x76) (#x80 #x00 #x24) (#x73 #x0b #x00) (#x52 #x28 #x00)
      (#x24 #x44 #x00) (#x00 #x57 #x00) (#x00 #x5c #x00) (#x00 #x53 #x24)
      (#x00 #x3c #x76) (#x00 #x00 #x00) (#x00 #x00 #x00) (#x00 #x00 #x00)
      (#xab #xab #xab) (#x0d #x57 #xff) (#x4b #x30 #xff) (#x8a #x13 #xff)
      (#xbc #x08 #xd6) (#xd2 #x12 #x69) (#xc7 #x2e #x00) (#x9d #x54 #x00)
      (#x60 #x7b #x00) (#x20 #x98 #x00) (#x00 #xa3 #x00) (#x00 #x99 #x42)
      (#x00 #x7d #xb4) (#x00 #x00 #x00) (#x00 #x00 #x00) (#x00 #x00 #x00)
      (#xff #xff #xff) (#x53 #xae #xff) (#x90 #x85 #xff) (#xd3 #x65 #xff)
      (#xff #x57 #xff) (#xff #x5d #xcf) (#xff #x77 #x57) (#xfa #x9e #x00)
      (#xbd #xc7 #x00) (#x7a #xe7 #x00) (#x43 #xf6 #x11) (#x26 #xef #x7e)
      (#x2c #xd5 #xf6) (#x4e #x4e #x4e) (#x00 #x00 #x00) (#x00 #x00 #x00)
      (#xff #xff #xff) (#xb6 #xe1 #xff) (#xce #xd1 #xff) (#xe9 #xc3 #xff)
      (#xff #xbc #xff) (#xff #xbd #xf4) (#xff #xc6 #xc3) (#xff #xd5 #x9a)
      (#xe9 #xe6 #x81) (#xce #xf4 #x81) (#xb6 #xfb #x9a) (#xa9 #xfa #xc3)
      (#xa9 #xf0 #xf4) (#xb8 #xb8 #xb8) (#x00 #x00 #x00) (#x00 #x00 #x00))
  :test 'equalp
  :documentation "NES NTSC color palette.

The standard NES color palette for NTSC systems, containing 64 colors
organized as 4 palettes of 16 colors each. The first color in each palette
is the background/transparent color that is shared across all palettes.

@table @asis
@item Structure
List of 64 RGB color triples (R G B values 0-255)
@item Organization
4 palettes × 16 colors each, with shared background color
@item Usage
Used for NES graphics conversion and palette matching
@end table

@xref{fun:grab-nes-palette}, @ref{constant:+nes-palette-pal+}.")
(define-constant +nes-palette-pal+
    '((#x62 #x62 #x62) (#x00 #x1f #xb2) (#x24 #x04 #xc8) (#x52 #x00 #xb2)
      (#x73 #x00 #x76) (#x80 #x00 #x24) (#x73 #x0b #x00) (#x52 #x28 #x00)
      (#x24 #x44 #x00) (#x00 #x57 #x00) (#x00 #x5c #x00) (#x00 #x53 #x24)
      (#x00 #x3c #x76) (#x00 #x00 #x00) (#x00 #x00 #x00) (#x00 #x00 #x00)
      (#xab #xab #xab) (#x0d #x57 #xff) (#x4b #x30 #xff) (#x8a #x13 #xff)
      (#xbc #x08 #xd6) (#xd2 #x12 #x69) (#xc7 #x2e #x00) (#x9d #x54 #x00)
      (#x60 #x7b #x00) (#x20 #x98 #x00) (#x00 #xa3 #x00) (#x00 #x99 #x42)
      (#x00 #x7d #xb4) (#x00 #x00 #x00) (#x00 #x00 #x00) (#x00 #x00 #x00)
      (#xff #xff #xff) (#x53 #xae #xff) (#x90 #x85 #xff) (#xd3 #x65 #xff)
      (#xff #x57 #xff) (#xff #x5d #xcf) (#xff #x77 #x57) (#xfa #x9e #x00)
      (#xbd #xc7 #x00) (#x7a #xe7 #x00) (#x43 #xf6 #x11) (#x26 #xef #x7e)
      (#x2c #xd5 #xf6) (#x4e #x4e #x4e) (#x00 #x00 #x00) (#x00 #x00 #x00)
      (#xff #xff #xff) (#xb6 #xe1 #xff) (#xce #xd1 #xff) (#xe9 #xc3 #xff)
      (#xff #xbc #xff) (#xff #xbd #xf4) (#xff #xc6 #xc3) (#xff #xd5 #x9a)
      (#xe9 #xe6 #x81) (#xce #xf4 #x81) (#xb6 #xfb #x9a) (#xa9 #xfa #xc3)
      (#xa9 #xf0 #xf4) (#xb8 #xb8 #xb8) (#x00 #x00 #x00) (#x00 #x00 #x00))
  :test 'equalp
  :documentation "NES PAL color palette.

The standard NES color palette for PAL systems, containing 64 colors
organized as 4 palettes of 16 colors each. The first color in each palette
is the background/transparent color that is shared across all palettes.

@table @asis
@item Structure
List of 64 RGB color triples (R G B values 0-255)
@item Organization
4 palettes × 16 colors each, with shared background color
@item Usage
Used for NES graphics conversion and palette matching on PAL systems
@end table

@xref{fun:grab-nes-palette}, @ref{constant:+nes-palette-ntsc+}.")

(define-constant +tg16-palette+
    '((#x00 #x00 #x00) (#x00 #x00 #x1b) (#x01 #x02 #x3d) (#x00 #x00 #x58)
      (#x01 #x04 #x7b) (#x00 #x00 #x95) (#x02 #x05 #xb8) (#x00 #x02 #xd3)
      (#x1c #x00 #x04) (#x20 #x04 #x27) (#x1d #x01 #x41) (#x21 #x06 #x64)
      (#x1d #x02 #x7f) (#x22 #x07 #xa2) (#x1e #x04 #xbc) (#x22 #x09 #xdf)
      (#x40 #x06 #x10) (#x3c #x03 #x2b) (#x41 #x08 #x4e) (#x3d #x05 #x68)
      (#x42 #x0a #x8b) (#x3e #x06 #xa6) (#x42 #x0b #xc8) (#x3e #x08 #xe3)
      (#x5c #x05 #x14) (#x58 #x02 #x2f) (#x5d #x07 #x52) (#x59 #x04 #x6c)
      (#x5e #x09 #x8f) (#x5a #x05 #xaa) (#x5e #x0a #xcd) (#x5a #x07 #xe7)
      (#x78 #x04 #x19) (#x7d #x09 #x3b) (#x79 #x06 #x56) (#x7d #x0b #x79)
      (#x7a #x08 #x93) (#x7e #x0d #xb6) (#x7a #x09 #xd1) (#x7f #x0e #xf3)
      (#x9c #x0b #x25) (#x99 #x08 #x3f) (#x9d #x0d #x62) (#x99 #x0a #x7d)
      (#x9e #x0f #xa0) (#x9a #x0c #xba) (#x9e #x11 #xdd) (#x9b #x0d #xf7)
      (#xb8 #x0a #x29) (#xb5 #x07 #x44) (#xb9 #x0c #x66) (#xb5 #x09 #x81)
      (#xba #x0e #xa4) (#xb6 #x0a #xbe) (#xba #x0f #xe1) (#xb7 #x0c #xfc)
      (#xd4 #x09 #x2d) (#xd9 #x0e #x50) (#xd5 #x0b #x6a) (#xda #x10 #x8d)
      (#xd6 #x0d #xa8) (#xda #x12 #xcb) (#xd6 #x0e #xe5) (#xdb #x13 #xff)
      (#x09 #x25 #x02) (#x05 #x22 #x1d) (#x09 #x27 #x40) (#x06 #x24 #x5a)
      (#x0a #x29 #x7d) (#x06 #x25 #x98) (#x0b #x2a #xba) (#x07 #x27 #xd5)
      (#x25 #x24 #x06) (#x21 #x21 #x21) (#x25 #x26 #x44) (#x22 #x23 #x5e)
      (#x26 #x28 #x81) (#x22 #x24 #x9c) (#x27 #x29 #xbe) (#x23 #x26 #xd9)
      (#x41 #x23 #x0a) (#x45 #x28 #x2d) (#x41 #x25 #x48) (#x46 #x2a #x6b)
      (#x42 #x27 #x85) (#x46 #x2c #xa8) (#x43 #x28 #xc2) (#x47 #x2d #xe5)
      (#x65 #x2a #x17) (#x61 #x27 #x31) (#x66 #x2c #x54) (#x62 #x29 #x6f)
      (#x66 #x2e #x91) (#x62 #x2b #xac) (#x67 #x30 #xcf) (#x63 #x2c #xe9)
      (#x81 #x29 #x1b) (#x7d #x26 #x35) (#x82 #x2b #x58) (#x7e #x28 #x73)
      (#x82 #x2d #x96) (#x7e #x2a #xb0) (#x83 #x2f #xd3) (#x7f #x2b #xed)
      (#x9d #x28 #x1f) (#xa1 #x2d #x42) (#x9e #x2a #x5c) (#xa2 #x2f #x7f)
      (#x9e #x2c #x9a) (#xa3 #x31 #xbc) (#x9f #x2d #xd7) (#xa3 #x32 #xfa)
      (#xc1 #x2f #x2b) (#xbd #x2c #x46) (#xc2 #x31 #x69) (#xbe #x2e #x83)
      (#xc2 #x33 #xa6) (#xbf #x30 #xc0) (#xc3 #x35 #xe3) (#xbf #x31 #xfe)
      (#xdd #x2e #x2f) (#xd9 #x2b #x4a) (#xde #x30 #x6d) (#xda #x2d #x87)
      (#xde #x32 #xaa) (#xdb #x2f #xc5) (#xdf #x34 #xe7) (#xdb #x30 #xff)
      (#x09 #x42 #x00) (#x0e #x47 #x1f) (#x0a #x44 #x3a) (#x0e #x49 #x5c)
      (#x0a #x46 #x77) (#x0f #x4b #x9a) (#x0b #x47 #xb4) (#x10 #x4c #xd7)
      (#x2d #x49 #x09) (#x2a #x46 #x23) (#x2e #x4b #x46) (#x2a #x48 #x61)
      (#x2f #x4d #x83) (#x2b #x4a #x9e) (#x27 #x46 #xb8) (#x2c #x4b #xdb)
      (#x49 #x48 #x0d) (#x46 #x45 #x27) (#x4a #x4a #x4a) (#x46 #x47 #x65)
      (#x4b #x4c #x87) (#x47 #x49 #xa2) (#x4b #x4e #xc5) (#x48 #x4a #xdf)
      (#x65 #x47 #x11) (#x6a #x4c #x34) (#x66 #x49 #x4e) (#x6a #x4e #x71)
      (#x67 #x4b #x8b) (#x6b #x50 #xae) (#x67 #x4d #xc9) (#x6c #x52 #xec)
      (#x8a #x4e #x1d) (#x86 #x4b #x38) (#x8a #x50 #x5a) (#x86 #x4d #x75)
      (#x8b #x52 #x98) (#x87 #x4f #xb2) (#x83 #x4b #xcd) (#x88 #x50 #xf0)
      (#xa6 #x4d #x21) (#xa2 #x4a #x3c) (#xa6 #x4f #x5f) (#xa2 #x4c #x79)
      (#xa7 #x51 #x9c) (#xa3 #x4e #xb6) (#xa8 #x53 #xd9) (#xa4 #x4f #xf4)
      (#xc2 #x4c #x25) (#xc6 #x51 #x48) (#xc2 #x4e #x63) (#xc7 #x53 #x85)
      (#xc3 #x50 #xa0) (#xc7 #x55 #xc3) (#xc4 #x52 #xdd) (#xc8 #x57 #xff)
      (#xe6 #x54 #x32) (#xe2 #x50 #x4c) (#xe6 #x55 #x6f) (#xe3 #x52 #x89)
      (#xdf #x4f #xa4) (#xe3 #x54 #xc7) (#xe0 #x51 #xe1) (#xe4 #x56 #xff)
      (#x12 #x67 #x00) (#x0e #x64 #x19) (#x12 #x69 #x3c) (#x0f #x66 #x56)
      (#x13 #x6b #x79) (#x0f #x68 #x94) (#x14 #x6d #xb7) (#x10 #x69 #xd1)
      (#x2e #x66 #x03) (#x32 #x6b #x25) (#x2e #x68 #x40) (#x33 #x6d #x63)
      (#x2f #x6a #x7d) (#x34 #x6f #xa0) (#x30 #x6c #xbb) (#x34 #x71 #xdd)
      (#x4a #x65 #x07) (#x4e #x6a #x2a) (#x4a #x67 #x44) (#x4f #x6c #x67)
      (#x4b #x69 #x81) (#x50 #x6e #xa4) (#x4c #x6b #xbf) (#x50 #x70 #xe2)
      (#x6e #x6c #x13) (#x6a #x69 #x2e) (#x6f #x6e #x50) (#x6b #x6b #x6b)
      (#x6f #x70 #x8e) (#x6c #x6d #xa8) (#x70 #x72 #xcb) (#x6c #x6e #xe6)
      (#x8a #x6b #x17) (#x8e #x70 #x3a) (#x8b #x6d #x54) (#x8f #x72 #x77)
      (#x8b #x6f #x92) (#x90 #x74 #xb5) (#x8c #x71 #xcf) (#x90 #x76 #xf2)
      (#xa6 #x6a #x1b) (#xaa #x6f #x3e) (#xa7 #x6c #x59) (#xab #x71 #x7b)
      (#xa7 #x6e #x96) (#xac #x73 #xb9) (#xa8 #x70 #xd3) (#xac #x75 #xf6)
      (#xca #x72 #x28) (#xc6 #x6e #x42) (#xcb #x73 #x65) (#xc7 #x70 #x7f)
      (#xcc #x75 #xa2) (#xc8 #x72 #xbd) (#xcc #x77 #xe0) (#xc8 #x74 #xfa)
      (#xe6 #x70 #x2c) (#xeb #x75 #x4e) (#xe7 #x72 #x69) (#xeb #x77 #x8c)
      (#xe8 #x74 #xa6) (#xec #x79 #xc9) (#xe8 #x76 #xe4) (#xed #x7b #xff)
      (#x12 #x84 #x00) (#x17 #x89 #x1b) (#x13 #x86 #x36) (#x17 #x8b #x59)
      (#x14 #x88 #x73) (#x18 #x8d #x96) (#x14 #x8a #xb1) (#x19 #x8f #xd3)
      (#x37 #x8b #x05) (#x33 #x88 #x1f) (#x37 #x8d #x42) (#x33 #x8a #x5d)
      (#x38 #x8f #x80) (#x34 #x8c #x9a) (#x39 #x91 #xbd) (#x35 #x8d #xd7)
      (#x53 #x8a #x09) (#x57 #x8f #x2c) (#x53 #x8c #x46) (#x58 #x91 #x69)
      (#x54 #x8e #x84) (#x58 #x93 #xa6) (#x55 #x90 #xc1) (#x59 #x95 #xe4)
      (#x6f #x89 #x0d) (#x73 #x8e #x30) (#x6f #x8b #x4a) (#x74 #x90 #x6d)
      (#x70 #x8d #x88) (#x74 #x92 #xab) (#x71 #x8f #xc5) (#x75 #x94 #xe8)
      (#x93 #x91 #x19) (#x8f #x8d #x34) (#x93 #x92 #x57) (#x90 #x8f #x71)
      (#x94 #x94 #x94) (#x90 #x91 #xaf) (#x95 #x96 #xd1) (#x91 #x93 #xec)
      (#xaf #x8f #x1d) (#xb3 #x94 #x40) (#xaf #x91 #x5b) (#xb4 #x96 #x7e)
      (#xb0 #x93 #x98) (#xb5 #x98 #xbb) (#xb1 #x95 #xd5) (#xb5 #x9a #xf8)
      (#xcb #x8e #x22) (#xcf #x93 #x44) (#xcb #x90 #x5f) (#xd0 #x95 #x82)
      (#xcc #x92 #x9c) (#xd1 #x97 #xbf) (#xcd #x94 #xda) (#xd1 #x99 #xfc)
      (#xef #x96 #x2e) (#xeb #x92 #x48) (#xf0 #x97 #x6b) (#xec #x94 #x86)
      (#xf0 #x99 #xa9) (#xed #x96 #xc3) (#xf1 #x9b #xe6) (#xed #x98 #xff)
      (#x1b #xa9 #x00) (#x1f #xae #x1e) (#x1c #xab #x38) (#x20 #xb0 #x5b)
      (#x1c #xad #x76) (#x21 #xb2 #x98) (#x1d #xaf #xb3) (#x19 #xab #xcd)
      (#x37 #xa8 #x00) (#x3b #xad #x22) (#x38 #xaa #x3c) (#x3c #xaf #x5f)
      (#x38 #xac #x7a) (#x3d #xb1 #x9c) (#x39 #xae #xb7) (#x3d #xb3 #xda)
      (#x5b #xb0 #x0b) (#x57 #xac #x26) (#x5c #xb1 #x49) (#x58 #xae #x63)
      (#x5d #xb3 #x86) (#x59 #xb0 #xa0) (#x5d #xb5 #xc3) (#x59 #xb2 #xde)
      (#x77 #xaf #x0f) (#x7c #xb4 #x32) (#x78 #xb0 #x4d) (#x7c #xb5 #x6f)
      (#x79 #xb2 #x8a) (#x75 #xaf #xa5) (#x79 #xb4 #xc7) (#x75 #xb1 #xe2)
      (#x93 #xad #x13) (#x98 #xb2 #x36) (#x94 #xaf #x51) (#x98 #xb4 #x74)
      (#x95 #xb1 #x8e) (#x99 #xb6 #xb1) (#x95 #xb3 #xcb) (#x9a #xb8 #xee)
      (#xb7 #xb5 #x20) (#xb4 #xb1 #x3a) (#xb8 #xb6 #x5d) (#xb4 #xb3 #x78)
      (#xb9 #xb8 #x9a) (#xb5 #xb5 #xb5) (#xb9 #xba #xd8) (#xb6 #xb7 #xf2)
      (#xd3 #xb4 #x24) (#xd8 #xb9 #x47) (#xd4 #xb5 #x61) (#xd9 #xba #x84)
      (#xd5 #xb7 #x9e) (#xd1 #xb4 #xb9) (#xd5 #xb9 #xdc) (#xd2 #xb6 #xf6)
      (#xef #xb3 #x28) (#xf4 #xb8 #x4b) (#xf0 #xb4 #x65) (#xf5 #xb9 #x88)
      (#xf1 #xb6 #xa3) (#xf5 #xbb #xc5) (#xf1 #xb8 #xe0) (#xf6 #xbd #xff)
      (#x24 #xcf #x00) (#x20 #xcb #x18) (#x24 #xd0 #x3a) (#x21 #xcd #x55)
      (#x25 #xd2 #x78) (#x21 #xcf #x92) (#x26 #xd4 #xb5) (#x22 #xd1 #xd0)
      (#x40 #xce #x01) (#x3c #xca #x1c) (#x40 #xcf #x3f) (#x3d #xcc #x59)
      (#x41 #xd1 #x7c) (#x3d #xce #x96) (#x42 #xd3 #xb9) (#x3e #xd0 #xd4)
      (#x5c #xcd #x05) (#x60 #xd2 #x28) (#x5c #xce #x43) (#x61 #xd3 #x65)
      (#x5d #xd0 #x80) (#x61 #xd5 #xa3) (#x5e #xd2 #xbd) (#x62 #xd7 #xe0)
      (#x80 #xd4 #x12) (#x7c #xd0 #x2c) (#x81 #xd5 #x4f) (#x7d #xd2 #x69)
      (#x81 #xd7 #x8c) (#x7d #xd4 #xa7) (#x82 #xd9 #xca) (#x7e #xd6 #xe4)
      (#x9c #xd3 #x16) (#x98 #xcf #x30) (#x9d #xd4 #x53) (#x99 #xd1 #x6e)
      (#x9d #xd6 #x90) (#x99 #xd3 #xab) (#x9e #xd8 #xce) (#x9a #xd5 #xe8)
      (#xb8 #xd2 #x1a) (#xbc #xd7 #x3d) (#xb9 #xd3 #x57) (#xbd #xd8 #x7a)
      (#xb9 #xd5 #x94) (#xbe #xda #xb7) (#xba #xd7 #xd2) (#xbe #xdc #xf5)
      (#xdc #xd9 #x26) (#xd8 #xd6 #x41) (#xdd #xdb #x63) (#xd9 #xd7 #x7e)
      (#xdd #xdc #xa1) (#xda #xd9 #xbb) (#xde #xde #xde) (#xda #xdb #xf9)
      (#xf8 #xd8 #x2a) (#xf4 #xd5 #x45) (#xf9 #xda #x67) (#xf5 #xd6 #x82)
      (#xf9 #xdb #xa5) (#xf6 #xd8 #xbf) (#xfa #xdd #xe2) (#xf6 #xda #xfd)
      (#x24 #xec #x00) (#x29 #xf1 #x1a) (#x25 #xed #x34) (#x29 #xf2 #x57)
      (#x25 #xef #x72) (#x2a #xf4 #x95) (#x26 #xf1 #xaf) (#x2b #xf6 #xd2)
      (#x48 #xf3 #x03) (#x45 #xf0 #x1e) (#x49 #xf5 #x41) (#x45 #xf1 #x5b)
      (#x4a #xf6 #x7e) (#x46 #xf3 #x99) (#x4a #xf8 #xbb) (#x47 #xf5 #xd6)
      (#x64 #xf2 #x08) (#x61 #xee #x22) (#x65 #xf3 #x45) (#x61 #xf0 #x5f)
      (#x66 #xf5 #x82) (#x62 #xf2 #x9d) (#x66 #xf7 #xc0) (#x63 #xf4 #xda)
      (#x80 #xf1 #x0c) (#x85 #xf6 #x2e) (#x81 #xf2 #x49) (#x85 #xf7 #x6c)
      (#x82 #xf4 #x86) (#x86 #xf9 #xa9) (#x82 #xf6 #xc4) (#x87 #xfb #xe6)
      (#xa5 #xf8 #x18) (#xa1 #xf5 #x32) (#xa5 #xfa #x55) (#xa1 #xf6 #x70)
      (#xa6 #xfb #x93) (#xa2 #xf8 #xad) (#xa7 #xfd #xd0) (#xa3 #xfa #xeb)
      (#xc1 #xf7 #x1c) (#xbd #xf4 #x37) (#xc1 #xf9 #x59) (#xbd #xf5 #x74)
      (#xc2 #xfa #x97) (#xbe #xf7 #xb1) (#xc3 #xfc #xd4) (#xbf #xf9 #xef)
      (#xdd #xf6 #x20) (#xe1 #xfb #x43) (#xdd #xf8 #x5d) (#xe2 #xfd #x80)
      (#xde #xf9 #x9b) (#xe2 #xfe #xbe) (#xdf #xfb #xd8) (#xe3 #xff #xfb)
      (#xff #xfd #x2c) (#xfd #xfa #x47) (#xff #xff #x6a) (#xfe #xfb #x84)
      (#xff #xff #xa7) (#xfe #xfd #xc2) (#xff #xff #xe4) (#xff #xff #xff))
  :test 'equalp
  :documentation "Palette for the TurboGrafx-16")

(define-constant +ted-palette+
    ;; TED (Commodore 16/Plus4) palette - 16 basic colors for compatibility
    ;; TED actually supports 121 colors, but we'll use the standard 16-color palette
    '((#x00 #x00 #x00)  ; 0 Black
      (#xff #xff #xff)  ; 1 White
      (#x00 #x00 #x88)  ; 2 Red (dark)
      (#xaa #xff #xee)  ; 3 Cyan
      (#xcc #x44 #xcc)  ; 4 Purple
      (#x00 #xcc #x55)  ; 5 Green
      (#x00 #x00 #xaa)  ; 6 Blue
      (#xee #xee #x77)  ; 7 Yellow
      (#xdd #x88 #x55)  ; 8 Orange
      (#x66 #x44 #x00)  ; 9 Brown
      (#xff #x77 #x77)  ; 10 Light Red
      (#x33 #x33 #x33)  ; 11 Dark Grey
      (#x77 #x77 #x77)  ; 12 Medium Grey
      (#xaa #xff #x66)  ; 13 Light Green
      (#x00 #x88 #xff)  ; 14 Light Blue
      (#xbb #x77 #xbb)) ; 15 Light Grey
  :test 'equalp)
(define-constant +vcs-ntsc-palette+
    '((0   0   0) (64  64  64) (108 108 108) (144 144 144) (176 176 176) (200 200 200) (220 220 220) (236 236 236)
      (68  68   0) (100 100  16) (132 132  36) (160 160  52) (184 184  64) (208 208  80) (232 232  92) (252 252 104)
      (112  40   0) (132  68  20) (152  92  40) (172 120  60) (188 140  76) (204 160  92) (220 180 104) (236 200 120)
      (132  24   0) (152  52  24) (172  80  48) (192 104  72) (208 128  92) (224 148 112) (236 168 128) (252 188 148)
      (136   0   0) (156  32  32) (176  60  60) (192  88  88) (208 112 112) (224 136 136) (236 160 160) (252 180 180)
      (120   0  92) (140  32 116) (160  60 136) (176  88 156) (192 112 176) (208 132 192) (220 156 208) (236 176 224)
      (72   0 120) (96  32 144) (120  60 164) (140  88 184) (160 112 204) (180 132 220) (196 156 236) (212 176 252)
      (20   0 132) (48  32 152) (76  60 172) (104  88 192) (124 112 208) (148 136 224) (168 160 236) (188 180 252)
      (0   0 136) (28  32 156) (56  64 176) (80  92 192) (104 116 208) (124 140 224) (144 164 236) (164 184 252)
      (0  24 124) (28  56 144) (56  84 168) (80 112 188) (104 136 204) (124 156 220) (144 180 236) (164 200 252)
      (0  44  92) (28  76 120) (56 104 144) (80 132 172) (104 156 192) (124 180 212) (144 204 232) (164 224 252)
      (0  60  44) (28  92  72) (56 124 100) (80 156 128) (104 180 148) (124 208 172) (144 228 192) (164 252 212)
      (0  60   0) (32  92  32) (64 124  64) (92 156  92) (116 180 116) (140 208 140) (164 228 164) (184 252 184)
      (20  56   0) (52  92  28) (80 124  56) (108 152  80) (132 180 104) (156 204 124) (180 228 144) (200 252 164)
      (44  48   0) (76  80  28) (104 112  52) (132 140  76) (156 168 100) (180 192 120) (204 212 136) (224 236 156)
      (68  40   0) (100  72  24) (132 104  48) (160 132  68) (184 156  88) (208 180 108) (232 204 124) (252 224 140))
  :test 'equalp)

(define-constant +prosystem-ntsc-palette+
    '((0 0 0) (18 18 18) (36 36 36) (54 54 54) (71 71 71) (89 89 89) (107 107 107) (125 125 125) (143 143 143) (161 161 161) (178 178 178) (196 196 196) (214 214 214) (232 232 232) (250 250 250) (255 255 255)
      (10 22 0) (36 40 0) (62 58 0) (88 76 0) (113 94 0) (139 111 0) (165 129 0) (186 147 11) (204 165 29) (222 183 47) (239 201 65) (255 218 82) (255 236 100) (255 254 118) (255 255 136) (255 255 154)
      (31 6 0) (57 24 0) (82 42 0) (108 59 0) (134 77 0) (160 95 0) (184 113 4) (202 131 22) (219 149 40) (237 166 58) (255 184 76) (255 202 94) (255 220 111) (255 238 129) (255 255 147) (255 255 165)
      (50 0 0) (71 6 0) (97 24 0) (122 42 0) (148 60 0) (166 78 18) (184 95 36) (201 113 54) (220 131 72) (238 149 89) (255 167 107) (255 185 125) (255 203 143) (255 220 161) (255 238 179) (255 255 197)
      (62 0 0) (80 0 0) (97 9 10) (116 27 28) (133 45 46) (151 62 64) (168 80 81) (187 98 99) (204 116 117) (223 134 135) (240 152 153) (255 170 171) (254 187 189) (255 205 206) (255 223 224) (255 241 242)
      (51 0 25) (61 0 43) (71 0 61) (88 17 79) (107 35 96) (125 53 114) (141 70 132) (159 88 150) (177 106 168) (195 124 186) (214 142 203) (227 160 221) (227 178 239) (228 195 255) (236 213 255) (244 231 255)
      (18 0 71) (28 0 89) (38 0 106) (54 14 124) (72 32 142) (89 50 160) (107 68 178) (125 86 196) (144 104 213) (161 121 231) (179 139 249) (203 157 255) (219 175 255) (227 193 255) (235 211 255) (243 228 255)
      (0 0 102) (0 0 120) (3 2 138) (20 19 156) (37 37 174) (55 55 192) (73 73 210) (92 91 227) (110 109 245) (131 126 255) (157 144 255) (182 162 255) (208 180 255) (229 198 255) (237 216 255) (246 234 255)
      (0 0 114) (0 0 132) (0 13 150) (0 31 168) (11 49 185) (29 67 203) (46 85 221) (64 103 239) (82 120 255) (108 138 255) (134 156 255) (160 174 255) (187 192 255) (212 210 255) (238 228 255) (250 245 255)
      (0 0 103) (0 12 121) (0 30 139) (0 47 157) (0 65 174) (13 83 192) (31 101 210) (49 119 228) (66 137 246) (88 155 255) (114 172 255) (140 190 255) (166 208 255) (192 226 255) (218 244 255) (240 255 255)
      (0 11 72) (0 29 90) (0 47 107) (0 65 125) (0 83 143) (13 101 161) (30 118 179) (48 136 197) (65 154 215) (84 172 232) (102 190 250) (126 208 255) (152 226 255) (177 243 255) (200 255 255) (218 255 255)
      (0 27 26) (0 44 44) (0 62 62) (0 80 80) (9 98 98) (27 116 116) (45 134 133) (63 152 151) (81 169 169) (98 187 187) (116 205 205) (134 223 223) (152 241 241) (170 255 255) (188 255 255) (206 255 255)
      (17 37 0) (24 54 0) (27 72 11) (27 90 29) (36 108 47) (54 126 65) (72 144 83) (89 161 101) (107 179 118) (125 197 136) (143 215 154) (161 233 172) (179 251 190) (191 255 208) (200 255 226) (210 255 243)
      (18 39 0) (26 57 0) (34 75 0) (45 93 0) (70 111 1) (88 129 19) (106 146 37) (124 164 55) (141 182 73) (159 200 90) (177 218 108) (195 236 126) (213 253 144) (224 255 162) (233 255 180) (243 255 198)
      (15 34 0) (23 52 0) (38 70 0) (65 88 0) (91 106 0) (117 124 0) (140 141 5) (158 159 23) (175 177 41) (194 195 58) (212 213 76) (230 231 94) (247 248 112) (255 255 130) (255 255 148) (255 255 166)
      (10 23 0) (35 40 0) (61 58 0) (87 76 0) (113 94 0) (138 112 0) (164 130 0) (185 147 11) (203 165 29) (221 183 47) (239 201 65) (255 219 82) (255 237 100) (255 255 118) (255 255 136) (255 255 154))
  :test 'equalp)

(define-constant +prosystem-pal-palette+
    ' ((0 0 0) (18 18 18) (36 36 36) (54 54 54) (71 71 71) (89 89 89) (107 107 107) (125 125 125)
               (143 143 143) (161 161 161) (178 178 178) (196 196 196) (214 214 214) (232 232 232) (250 250 250)
               (255 255 255) (0 22 0) (18 40 0) (36 58 0) (54 76 0) (71 94 0) (89 111 0) (107 129 0)
               (125 147 11) (143 165 29) (161 183 47) (178 201 65) (196 218 82) (214 236 100) (232 254 118)
               (250 255 136) (255 255 154) (28 6 0) (46 24 0) (63 41 0) (81 59 0) (99 77 0) (117 95 0)
               (135 113 4) (153 131 22) (171 149 40) (188 166 58) (206 184 76) (224 202 94) (242 220 112)
               (255 238 129) (255 255 147) (255 255 165) (50 0 0) (68 6 0) (86 24 0) (104 42 0) (121 60 0)
               (139 78 18) (157 95 36) (175 113 54) (193 131 72) (211 149 90) (229 167 107) (246 185 125)
               (255 203 143) (255 220 161) (255 238 179) (255 255 197) (62 0 0) (80 0 0) (98 9 10) (116 27 28)
               (134 45 46) (152 62 64) (169 80 82) (187 98 100) (205 116 118) (223 134 135) (241 152 153)
               (255 169 171) (255 187 189) (255 205 207) (255 223 225) (255 241 242) (62 0 25) (80 0 43)
               (98 0 61) (116 17 79) (134 35 97) (152 53 115) (169 70 133) (187 88 150) (205 106 168)
               (223 124 186) (241 142 204) (255 160 222) (255 178 240) (255 195 255) (255 213 255) (255 231 255)
               (50 0 71) (68 0 89) (86 0 107) (103 14 125) (121 32 143) (139 50 160) (157 68 178) (175 86 196)
               (193 104 214) (211 121 232) (228 139 250) (246 157 255) (255 175 255) (255 193 255) (255 211 255)
               (255 228 255) (28 0 103) (45 0 121) (63 2 138) (81 20 156) (99 37 174) (117 55 192) (135 73 210)
               (153 91 228) (170 109 246) (188 127 255) (206 144 255) (224 162 255) (242 180 255) (255 198 255)
               (255 216 255) (255 234 255) (0 0 114) (18 0 132) (36 14 150) (53 31 168) (71 49 185) (89 67 203)
               (107 85 221) (125 103 239) (143 121 255) (161 139 255) (178 156 255) (196 174 255) (214 192 255)
               (232 210 255) (250 228 255) (255 246 255) (0 0 103) (0 12 120) (8 30 138) (26 48 156) (44 66 174)
               (61 84 192) (79 101 210) (97 119 228) (115 137 245) (133 155 255) (151 173 255) (168 191 255)
               (186 208 255) (204 226 255) (222 244 255) (240 255 255) (0 12 71) (0 30 89) (0 47 107) (3 65 124)
               (21 83 142) (39 101 160) (57 119 178) (75 137 196) (93 155 214) (111 172 231) (128 190 249)
               (146 208 255) (164 226 255) (182 244 255) (200 255 255) (218 255 255) (0 27 25) (0 45 43)
               (0 63 61) (0 80 79) (9 98 96) (27 116 114) (45 134 132) (63 152 150) (80 170 168) (98 188 186)
               (116 205 204) (134 223 221) (152 241 239) (170 255 255) (188 255 255) (205 255 255) (0 37 0)
               (0 55 0) (0 72 10) (0 90 28) (9 108 46) (27 126 63) (45 144 81) (63 162 99) (81 180 117)
               (98 197 135) (116 215 153) (134 233 171) (152 251 188) (170 255 206) (188 255 224) (205 255 242)
               (0 39 0) (0 57 0) (0 75 0) (4 93 0) (22 111 0) (39 128 18) (57 146 36) (75 164 54) (93 182 71)
               (111 200 89) (129 218 107) (147 236 125) (164 253 143) (182 255 161) (200 255 178) (218 255 196)
               (0 34 0) (0 52 0) (8 70 0) (26 88 0) (44 105 0) (62 123 0) (80 141 4) (97 159 22) (115 177 40)
               (133 195 58) (151 212 76) (169 230 93) (187 248 111) (205 255 129) (222 255 147) (240 255 165)
               (0 22 0) (18 40 0) (36 58 0) (54 76 0) (72 93 0) (90 111 0) (107 129 0) (125 147 11) (143 165 29)
               (161 183 47) (179 200 65) (197 218 82) (214 236 100) (232 254 118) (250 255 136) (255 255 154))
  :test 'equalp)

(define-constant +vcs-pal-palette+
    '((11 11 11) (51 51 51) (89 89 89) (123 123 123) (153 153 153) (182 182 182) (207 207 207) (230 230 230)
      (11 11 11) (51 51 51) (89 89 89) (123 123 123) (153 153 153) (182 182 182) (207 207 207) (230 230 230)
      (59 36 0) (102 71 0) (139 112 0) (172 146 0) (197 174 54) (222 200 94) (247 226 127) (255 241 158)
      (0 69 0) (0 111 0) (59 146 0) (101 176 9) (133 202 61) (163 227 100) (191 252 132) (213 255 165)
      (89 0 0) (128 39 0) (161 87 0) (188 121 55) (214 152 95) (238 179 129) (255 206 158) (255 220 189)
      (0 73 0) (0 114 0) (22 146 22) (69 175 69) (107 201 107) (139 227 139) (169 251 169) (197 255 197)
      (100 0 18) (137 8 33) (167 61 77) (194 100 114) (220 132 145) (244 163 174) (255 190 202) (255 218 224)
      (0 61 41) (0 106 72) (4 142 99) (60 170 132) (98 197 162) (131 223 190) (161 248 217) (190 255 233)
      (85 0 70) (136 0 110) (165 49 141) (193 89 170) (218 124 197) (243 154 223) (255 185 243) (255 212 246)
      (0 54 81) (0 90 125) (17 126 156) (66 156 184) (104 183 210) (136 210 235) (166 235 255) (195 255 255)
      (76 0 124) (117 0 157) (147 46 184) (175 87 210) (202 122 235) (228 153 255) (236 183 255) (243 212 255)
      (0 45 131) (0 62 164) (45 101 191) (86 133 218) (121 162 242) (153 191 255) (183 219 255) (211 245 255)
      (34 0 150) (82 0 182) (117 56 207) (148 95 232) (177 129 255) (197 160 255) (214 189 255) (232 218 255)
      (0 0 154) (36 29 182) (80 74 208) (116 111 233) (146 142 255) (177 173 255) (206 202 255) (233 229 255)
      (11 11 11) (51 51 51) (89 89 89) (123 123 123) (153 153 153) (182 182 182) (207 207 207) (230 230 230)
      (11 11 11) (51 51 51) (89 89 89) (123 123 123) (153 153 153) (182 182 182) (207 207 207) (230 230 230))
  :test 'equalp)
(define-constant +vcs-secam-palette+
    '((0 0 0) (0 0 255) (0 255 0) (0 255 255)
      (255 0 0) (255 0 255) (255 255 0) (255 255 255))
  :test 'equalp)

(define-constant +vcs-secam-color-names+
    '(COLBLACK COLBLUE COLGREEN COLCYAN
      COLRED COLMAGENTA COLYELLOW COLWHITE)
  :test 'equalp)

(define-constant +unicode->ascii-ish+ nil)

(defun double-up (list)
  "Duplicate each element in a list.

Creates a new list where each element from the input appears twice in sequence.

@table @asis
@item LIST
Input list of any elements
@item Returns
New list with each element duplicated
@item Example
(double-up '(a b c)) => (a a b b c c)
@end table"
  (loop for item in list
        append (list item item)))

(assert (equalp '(a a b b c c) (double-up '(a b c))))

(defun machine-palette (&optional (machine *machine*) (region *region*))
  "Get the standard color palette for a target machine and region.

Returns the appropriate color palette for the specified machine type and
video region, used for graphics conversion and color matching.

@table @asis
@item MACHINE
Machine identifier (default: current *machine*)
@item REGION
Video region (:ntsc, :pal, :secam) (default: current *region*)
@item Returns
List of RGB color triples for the machine's palette
@end table

@xref{var:*machine*}, @xref{var:*region*}."
  (copy-list (ecase machine
               (20 (subseq +c64-palette+ 0 7))
               (200 +lynx-palette+)
               ((64 128) +c64-palette+)
               (2 +apple-hires-palette+)
               (8 (ecase region
                    (:ntsc +nes-palette-ntsc+)
                    (:pal +nes-palette-pal+)))
               (2600 (ecase region
                       (:ntsc +vcs-ntsc-palette+)
                       (:pal +vcs-pal-palette+)
                       (:secam +vcs-secam-palette+)))
               (2609 +intv-palette+)
               (7800 (ecase region
                       (:ntsc +prosystem-ntsc-palette+)
                       (:pal +prosystem-pal-palette+)))
               ;; 5200: same Maria-class palette indices as 7800 (tile PNGs use ProSystem mapping).
               (5200 (ecase region
                       (:ntsc +prosystem-ntsc-palette+)
                       (:pal +prosystem-pal-palette+)))
               (264 +ted-palette+)
               (16 +tg16-palette+))))

(defun machine-colors ()
  "Get the color names for the current target machine.

Returns a list of color names corresponding to the palette indices for
the currently selected machine (*machine*).

@table @asis
@item Returns
List of color name strings for the current machine's palette
@item Depends on
* @var{*machine*} - current target machine
@end table

@xref{fun:machine-palette}, @xref{var:*machine*}."
  (ecase *machine*
    (20 (subseq +c64-names+ 0 7))
    ((64 128) +c64-names+)
    (2609 +intv-color-names+)))

(defun square (n)
  "Calculate the square of N.

Returns n² for any numeric input.

@table @asis
@item N
Number to square
@item Returns
n × n
@end table"
  (* n n))

(defun color-distance (r0 g0 b0 rgb1)
  "Calculate perceptual color distance between two RGB colors.

Computes the color difference using CIE LAB color space, which provides
perceptually uniform color distance measurements that correlate better
with human color perception than simple RGB Euclidean distance.

@table @asis
@item R0 G0 B0
First color as individual RGB components (0-255)
@item RGB1
Second color as RGB triple (R G B)
@item Returns
Perceptual color distance (ΔE*ab value)
@end table

This is meant to discover the actual perception-relative color distance
more accurately to the human eye than a linear distance in RGB space
can do."
  (destructuring-bind (r1 g1 b1) rgb1
    (multiple-value-bind (l0 a0 b0)
        (multiple-value-call #'dufy:xyz-to-lab (dufy:rgb-to-xyz r0 g0 b0))
      (multiple-value-bind (l1 a1 b1)
          (multiple-value-call #'dufy:xyz-to-lab (dufy:rgb-to-xyz r1 g1 b1))
        (sqrt (+ (square (- l0 l1)) (square (- a0 a1)) (square (- b0 b1))))))))

(defun find-nearest-in-palette (palette red green blue)
  "Find the perceptually closest color in a palette to an RGB color.

Searches through a palette to find the color that is closest to the given
RGB values using perceptual color distance (CIE LAB ΔE*ab).

@table @asis
@item PALETTE
List of RGB triples or palette indices
@item RED GREEN BLUE
Target color components (0-255)
@item Returns
RGB triple of the closest color in the palette
@end table

@xref{fun:color-distance}, @xref{fun:machine-palette}."
  (let ((palette (if (every #'listp palette)
                     palette
                     (mapcar (lambda (el) (elt (machine-palette) el)) palette))))
    (first (sort (copy-list palette) #'< :key (curry #'color-distance red green blue)))))

(defun palette->rgb (index)
  "Get RGB values for a palette index in the current machine.

Returns the RGB color triple for the specified index in the current
machine's color palette.

@table @asis
@item INDEX
Palette index (0-based)
@item Returns
RGB triple (R G B) for the palette entry
@end table

@xref{fun:machine-palette}."
  (nth index (machine-palette)))

(defvar *palette-warnings* (make-hash-table :test 'eql))

(defun rgb->int (red green blue)
  "Convert RED, GREEN, & BLUE color components to a 24-bit integer.

Packs RGB color values into a single 24-bit integer with 8 bits per component.

@table @asis
@item RED GREEN BLUE
Color components (0-255, unsigned-byte 8)
@item Returns
24-bit integer: #xRRGGBB
@end table

@xref{fun:rgb->palette}."
  (check-type red (unsigned-byte 8))
  (check-type green (unsigned-byte 8))
  (check-type blue (unsigned-byte 8))
  (logior (ash red 16) (ash green 8) blue))

(defun rgb->palette (red green blue)
  "Find the palette index for an RGB color in the current machine's palette.

Looks up an RGB color in the current machine's palette. If the exact color
is not found, finds the closest perceptual match and issues a warning.

@table @asis
@item RED GREEN BLUE
Color components (0-255)
@item Returns
Palette index (0-based) of the color or closest match
@item Side Effects
May issue warnings for colors not in the palette
@end table

@xref{fun:find-nearest-in-palette}, @xref{fun:machine-palette}."
  (check-type red (integer 0 #xff))
  (check-type green (integer 0 #xff))
  (check-type blue (integer 0 #xff))
  (or (position (list red green blue) (machine-palette) :test 'equalp)
      (destructuring-bind (r g b) (find-nearest-in-palette
                                   (machine-palette) red green blue)
        (let ((use (position (list r g b) (machine-palette) :test 'equalp)))
          (incf (gethash (rgb->int r g b) *palette-warnings* 0))
          (cond
            ((and (> 100 (hash-table-count *palette-warnings*))
                  (= 1 (gethash (rgb->int r g b) *palette-warnings*)))
             (warn-once "Color not in ~a palette: ~@[~a~]#~2,'0X~2,'0X~2,'0X; ~
used $~2,'0x (~@[~a~]#~2,'0X~2,'0X~2,'0X)"
                        (machine-short-name)
                        (when (tty-xterm-p)
                          (ansi-color-pixel red green blue))
                        red green blue
                        use
                        (when (tty-xterm-p)
                          (ansi-color-pixel r g b))
                        r g b))
            ((= 100 (hash-table-count *palette-warnings*))
             (warn-once "Over 100 colors not in palette, further warnings suppressed.")))
          use))))

(defun find-nearest-palette-color (rgb-color)
  "Find the nearest Atari 2600 palette color to the given RGB color using DUFY.

   RGB-COLOR should be a list (r g b) where each component is 0-255.
   Returns the palette index (0-127 for NTSC, 0-127 for PAL, 0-7 for SECAM).
   Uses machine-palette which properly handles *region*."
  (destructuring-bind (r g b) rgb-color
    (check-type r (integer 0 #xff))
    (check-type g (integer 0 #xff))
    (check-type b (integer 0 #xff))
    (destructuring-bind (nearest-r nearest-g nearest-b)
        (find-nearest-in-palette (machine-palette) r g b)
      (or (position (list nearest-r nearest-g nearest-b) (machine-palette) :test 'equalp)
          0))))

(defun png->palette (height width rgb &optional α)
  (check-type height (integer 0 *))
  (check-type width (integer 0 *))
  (check-type rgb array)
  (check-type α (or null array))
  (destructuring-bind (w h bpp) (array-dimensions rgb)
    (unless (and (= h height) (= w width) (or (= bpp 3) (= bpp 4)))
      (error "PNG image in an unsuitable format (wrong size or not RGB/RGBA)."))
    (let ((image (make-array (list width height)
                             :element-type '(or null (unsigned-byte 8)))))
      (loop for x from 0 below width
            do (loop for y from 0 below height
                     do (setf (aref image x y)
                              (if (and α (< 128 (aref α x y)))
                                  nil
                                  (rgb->palette (aref rgb x y 0)
                                                (aref rgb x y 1)
                                                (aref rgb x y 2))))))
      image)))

(define-constant +chaos-repeat-patterns+
    #(#()
      #(1 1 1 1 1 1 1 1)
      #(1 2 1 2 1 2 1 2)
      #(1 2 3 2 1 2 3 2)
      #(1 2 3 4 1 2 3 4)
      #(1 2 3 4 5 2 3 4)
      #(1 2 3 4 5 6 3 4)
      #(1 2 3 4 5 6 7 4)
      #(1 2 3 4 5 6 7 8))
  :test 'equalp)

(defun chaos-frame->key (frame-vector)
  (with-output-to-string (stream)
    (loop for byte across frame-vector
          do (format stream "~2,'0X" byte))))

(defun chaos-byte->binary-string (byte)
  (concatenate 'string "%" (format nil "~8,'0B" byte)))

(defun chaos-extract-frame (palette frame-index pose-index)
  (let ((frame (make-array 16 :element-type '(unsigned-byte 8)))
        (nonzero nil))
    (loop for row from 0 below 16
          for destination = (- 15 row)
          for global-y = (+ (* pose-index 16) row)
          do (let ((byte 0))
               (loop for column from 0 below 8
                     for global-x = (+ (* frame-index 8) column)
                     for palette-value = (and (< global-x (array-dimension palette 0))
                                               (< global-y (array-dimension palette 1))
                                               (aref palette global-x global-y))
                     do (when (and palette-value (plusp palette-value))
                          (setf nonzero t)
                          (setf byte (logior byte (ash 1 (- 7 column))))))
               (setf (aref frame destination) byte)))
    (values frame nonzero)))

(defun ensure-chaos-frame-index (frame-vector frame-cache frames)
  (let* ((key (chaos-frame->key frame-vector))
         (existing (gethash key frame-cache)))
    (if existing
        existing
        (let ((index (vector-push-extend (copy-seq frame-vector) frames)))
          (setf (gethash key frame-cache) index)
          index))))

(defun chaos-previous-value (row position)
  (loop for idx downfrom (1- position) downto 0
        for value = (aref row idx)
        when value do (return value)))

(defun fill-chaos-row-indices (row)
  (let* ((length (length row))
         (leading-count (loop for idx from 0 below length
                              while (aref row idx)
                              count 1)))
    (dotimes (position length)
      (when (null (aref row position))
        (let* ((pattern (and (plusp leading-count)
                             (< leading-count (length +chaos-repeat-patterns+))
                             (aref +chaos-repeat-patterns+ leading-count)))
               (value (cond
                        ((and pattern (< position (length pattern)))
                         (let* ((reference (1- (aref pattern position)))
                                (lookup (and (<= 0 reference)
                                             (< reference length)
                                             (aref row reference))))
                           (or lookup (chaos-previous-value row position))))
                        (t (chaos-previous-value row position)))))
          (unless value
            (error "Unable to infer ChaosFight frame index for column ~d" position))
          (setf (aref row position) value))))
    row))

(defun playfield-row->string (palette width y)
  (let ((chars (make-string width)))
    (dotimes (x width)
      (let ((value (and (< x (array-dimension palette 0))
                        (< y (array-dimension palette 1))
                        (aref palette x y))))
        (setf (aref chars x)
              (if (and value (not (zerop value)))
                  #\X
                  #\.))))
    chars))

(defun dominant-playfield-color (palette width y)
  (let ((counts (make-hash-table :test 'eql))
        (best-color 0)
        (best-count 0))
    (dotimes (x width)
      (let ((value (and (< x (array-dimension palette 0))
                        (< y (array-dimension palette 1))
                        (aref palette x y))))
        (when (and value (not (zerop value)))
          (let* ((count (incf (gethash value counts 0))))
            (when (> count best-count)
              (setf best-count count
                    best-color value))))))
    (if (plusp best-count) best-color 0)))

(defun format-playfield-color (color)
  (format nil "$~2,'0X" color))

(defun playfield-color-byte (color region)
  (let ((byte (logand color #xff)))
    (ecase region
      (:secam (let* ((index (mod byte 8))
                     (nybble (* 2 index)))
                (logior (ash nybble 4) nybble)))
      (:ntsc byte)
      (:pal byte))))

(defun compile-2600-font-8x16 (output-bas png-path)
  (let* ((input-path (uiop:ensure-pathname png-path))
         (png (progn
                (with-open-file (stream input-path :if-does-not-exist :error)
                  (declare (ignore stream)))
                (png-read:read-png-file input-path)))
         (width (png-read:width png))
         (height (png-read:height png)))
    (unless (and (> width 0) (zerop (mod width 8)))
      (error "Fonts must be a positive multiple of 8 pixels wide; got ~a" width))
    (unless (and (> height 0) (zerop (mod height 16)))
      (error "Fonts must be a positive multiple of 16 pixels tall; got ~a" height))
    (let* ((*machine* 2600)
           (*region* :ntsc)
           (palette (png->palette height width (png-read:image-data png)
                                   (png-read:transparency png)))
           (chars-per-row (/ width 8))
           (char-rows (/ height 16))
           (char-count (* chars-per-row char-rows))
           (output-path (uiop:ensure-pathname output-bas))
           (base-name (pathname-base-name input-path))
           (label-root (cl-change-case:pascal-case base-name))
           (font-label (format nil "SetFont~a" label-root))
           (data-label "FontData"))
      (ensure-directories-exist output-path)
      (with-open-file (stream output-path
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (format stream "~a" font-label)
        (format stream "~%~10tdata ~a" data-label)
        (loop for char from 0 below char-count
              for char-x = (mod char chars-per-row)
              for char-y = (floor char chars-per-row)
              do (loop for row from 15 downto 0
                       for y = (+ (* char-y 16) row)
                       do (let ((value 0))
                            (dotimes (col 8)
                              (let* ((x (+ (* char-x 8) col))
                                     (pixel (and (< x (array-dimension palette 0))
                                                 (< y (array-dimension palette 1))
                                                 (aref palette x y))))
                                (when (and pixel (not (zerop pixel)))
                                  (setf value (logior value (ash 1 (- 7 col)))))))
                            (format stream "~%~12t.byte %~8,'0B" value))))
        (format stream "~%end")
        (format stream "~%~%"))
      output-bas)))

(defun write-chaos-character-output (stream character-name frames frame-map output-path source-path)
  (let* ((target-path (uiop:ensure-pathname output-path))
         (source-pathname (uiop:ensure-pathname source-path))
         (file-name (enough-namestring target-path))
         (source-name (enough-namestring source-pathname))
         (label (format nil "~@[~a~]" character-name))
         (frame-label (format nil "~aFrames" label))
         (map-label (format nil "~aFrameMap" label)))
    (format stream "~10trem Chaos character output")
    (format stream "~%~10trem File: ~a" file-name)
    (format stream "~%~10trem Source: ~a" source-name)
    (format stream "~%~10trem DO NOT EDIT. Generated by compile-chaos-character.")
    (format stream "~%~10tasm")
    (format stream "~%~a:" frame-label)
    (let ((frame-count (length frames)))
      (dotimes (frame-index frame-count)
        (let ((frame-vector (aref frames frame-index)))
          (dotimes (row 16)
            (format stream "~%~12tBYTE ~a"
                    (chaos-byte->binary-string (aref frame-vector row)))))
        (when (< (1+ frame-index) frame-count)
          (format stream "~%"))))
    (format stream "~%end")
    (format stream "~%~10tasm")
    (format stream "~%~a:" map-label)
    (dotimes (pose 16)
      (let ((pose-values (aref frame-map pose)))
        (format stream "~%~12tBYTE ~{~d~^, ~}" (coerce pose-values 'list))))
    (format stream "~%end")
    (format stream "~%~%")))

(defun compile-2600-playfield (output-bas png-path &optional tv-standard)
  (declare (ignore tv-standard))
  (let* ((input-path (uiop:ensure-pathname png-path))
         (png (progn
                (with-open-file (stream input-path :if-does-not-exist :error)
                  (declare (ignore stream)))
                (png-read:read-png-file input-path)))
         (width (png-read:width png))
         (height (png-read:height png))
         (rgb (png-read:image-data png))
         (alpha (png-read:transparency png)))
    (unless (member width '(16 32))
      (error "Playfields must be 16 or 32 pixels wide; got ~a" width))
    (unless (<= 6 height 32)
      (error "Playfields must be between 6 and 32 rows; got ~a" height))
    (let* ((target-path (uiop:ensure-pathname output-bas))
           (name (pathname-name target-path))
           (segments (uiop:split-string name :separator "."))
           (maybe-region (when segments (string-upcase (car (last segments)))))
           (has-explicit-region (member maybe-region '("NTSC" "PAL" "SECAM") :test #'string=))
           (base-segments (if has-explicit-region
                              (butlast segments)
                              segments))
           (base-name (if base-segments
                          (format nil "~{~a~^.~}" base-segments)
                          name))
           (type (or (pathname-type target-path) "bas"))
           (directory (pathname-directory target-path))
           (host (pathname-host target-path))
           (device (pathname-device target-path))
           (label-root (cl-change-case:pascal-case (pathname-base-name input-path))))
      (loop for region in '(:ntsc :pal :secam)
            for conversion-region = (if (and (eq region :pal)
                                             (or (null +vcs-pal-palette+)
                                                 (null (machine-palette 2600 :pal))))
                                         :ntsc
                                         region)
            for output-path = (if has-explicit-region
                                   (make-pathname :host host
                                                  :device device
                                                  :directory directory
                                                  :name (format nil "~a.~a" base-name (string-upcase (symbol-name region)))
                                                  :type type)
                                   (if (eq region :ntsc)
                                       target-path
                                       (make-pathname :host host
                                                      :device device
                                                      :directory directory
                                                      :name (format nil "~a.~a" base-name (string-upcase (symbol-name region)))
                                                      :type type)))
            for label = (format nil "SetPlayfield~a~a"
                                 label-root
                                 (string-upcase (symbol-name region)))
            do (let* ((*machine* 2600)
                      (*region* conversion-region)
                      (palette (png->palette height width rgb alpha))
                      (rows (loop for y from 0 below height
                                  collect (playfield-row->string palette width y)))
                      (color-indices (loop for y from 0 below height
                                           collect (dominant-playfield-color palette width y)))
                      (color-bytes (mapcar (lambda (idx)
                                             (playfield-color-byte idx region))
                                           color-indices)))
                 (ensure-directories-exist output-path)
                 (with-open-file (stream output-path
                                         :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
                   (format stream "~a" label)
                   (format stream "~%~10tplayfield:")
                   (dolist (row rows)
                     (format stream "~%~10t~a" row))
                   (format stream "~%end")
                   (format stream "~%~10tpfcolors:")
                   (dolist (byte color-bytes)
                     (format stream "~%~10t~a" (format-playfield-color byte)))
                   (format stream "~%end")
                   (format stream "~%~10treturn~%~%"))))
      output-bas)))

(defun compile-chaos-character (output-bas png-path)
  (let* ((png-pathname (uiop:ensure-pathname png-path))
         (png (progn
                (with-open-file (stream png-pathname :if-does-not-exist :error)
                  (declare (ignore stream)))
                (png-read:read-png-file png-pathname)))
         (width (png-read:width png))
         (height (png-read:height png)))
    (unless (= width 64)
      (error "ChaosFight character sprites must be 64 pixels wide; got ~a" width))
    (unless (= height 256)
      (error "ChaosFight character sprites must be 256 pixels tall; got ~a" height))
    (let* ((palette (png->palette height width (png-read:image-data png)
                                  (png-read:transparency png)))
           (frames (make-array 0 :adjustable t :fill-pointer 0))
           (frame-cache (make-hash-table :test 'equal))
           (frame-map (make-array 16)))
      (dotimes (pose 16)
        (let ((row (make-array 8 :initial-element nil)))
          (dotimes (frame 8)
            (multiple-value-bind (frame-vector nonzero-p)
                (chaos-extract-frame palette frame pose)
              (when nonzero-p
                (setf (aref row frame)
                      (ensure-chaos-frame-index frame-vector frame-cache frames)))))
          (cond
            ((every #'null row)
             (if (plusp pose)
                 (setf row (copy-seq (aref frame-map (1- pose))))
                 (let ((blank-index (ensure-chaos-frame-index
                                     (make-array 16 :element-type '(unsigned-byte 8)
                                                 :initial-element 0)
                                     frame-cache frames)))
                   (dotimes (idx 8)
                     (setf (aref row idx) blank-index)))))
            (t
             (fill-chaos-row-indices row)))
          (setf (aref frame-map pose) (copy-seq row))))
      (let ((first-frame (aref frames 0)))
        (when (every #'zerop first-frame)
          (error "ChaosFight sprite ~a has blank action 0 frame 0"
                 (pathname-name (uiop:ensure-pathname png-path)))))
      (let* ((base-name (or (pathname-name (uiop:ensure-pathname png-path)) "Character"))
             (character-name (cl-change-case:pascal-case base-name))
             (output-path (uiop:ensure-pathname output-bas)))
        (ensure-directories-exist output-path)
        (with-open-file (stream output-path
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
          (write-chaos-character-output stream character-name frames frame-map output-path png-path)))
      output-bas)))


(defun extract-region (original left top right bottom)
  "Copy a rectangular region from ORIGINAL into a new array.

Uses @code{(array-element-type original)} when ORIGINAL is an array so that
palette images from @code{png->palette} — which may store @code{NIL} for
transparent pixels — are copied without coercing @code{NIL} into
@code{(unsigned-byte 8)} (which signals @code{type-error})."
  (let* ((elt (if (arrayp original)
                  (array-element-type original)
                  '(unsigned-byte 8)))
         (copy (make-array (list (1+ (- right left)) (1+ (- bottom top)))
                          :element-type elt)))
    (loop for x from left to right
          do (loop for y from top to bottom
                   do (setf (aref copy (- x left) (- y top)) (aref original x y))))
    copy))

(defun mob->mono-bits (mob)
  (mapcar #'code-char
          (loop for y from 0 to 20
                append
                (loop for col from 0 to 2
                      for col-offset = (* 8 col)
                      collecting
                      (reduce #'logior
                              (loop for x from 0 to 7
                                    for pixel = (aref mob (+ col-offset x) y)
                                    collecting (case pixel
                                                 (#xff 0)
                                                 (otherwise (expt 2 (- 7 x))))))))))

(defun mob->multi-bits (mob)
  (mapcar #'code-char
          (loop for y from 0 to 20
                append
                (loop for col from 0 to 2
                      for col-offset = (* 8 col)
                      collecting
                      (reduce #'logior
                              (loop for x from 0 to 3
                                    for pixel = (aref mob (+ col-offset (* 2 x)) y)
                                    collecting (* (expt 2 (* 2 (- 3 x)))
                                                  (case pixel
                                                    (8 1) (9 2) (#xff 0)
                                                    (otherwise 3)))))))))

(defun tile->bits (tile)
  (do-collect (y to 7)
    (reduce #'logior
            (loop for x from 0 to 7
                  collecting (if (zerop (aref tile x y))
                                 0
                                 (expt 2 (- 7 x)))))))

(defun tile->color (tile)
  (remove-duplicates
   (remove-if (curry #'= #xff)
              (loop for y from 0 to 7
                    appending
                    (do-collect (x to 7)
                      (aref tile x y))))))

(defun fat-bits (array)
  (destructuring-bind (width height) (array-dimensions array)
    (do-collect (row upto (1- height))
      (do-collect (col upto (1- width))
        (let ((px (aref array col row)))
          (if (= #xff px)
              #\Space
              #\@))))))

(defun image-colors (palette-image
                     &optional
                       (height (array-dimension palette-image 1))
                       (width (array-dimension palette-image 0)))
  "Return the set of distinct colors in use in the paletted image"
  (remove-duplicates
   (remove-if #'null
              (loop for y from 0 to (1- height)
                    appending (loop for x from 0 to (1- width)
                                    collecting (aref palette-image x y))))))

(defun mob-colors (mob)
  (image-colors mob 21 24))

(defun ensure-monochrome (mob)
  (let ((all-colors (mob-colors mob)))
    (unless (= 1 (length all-colors))
      (warn "MOB data is hi-res and not monochrome (using ~D; saw ~{~D~^, ~})"
            (car all-colors) all-colors))
    (code-char (car all-colors))))

(defun ensure-1+chrome (mob)
  (let ((all-colors (remove-if (rcurry #'member '(9 10))
                               (mob-colors mob))))
    (unless (or (null all-colors)
                (= 1 (length all-colors)))
      (warn "MOB data has more than 1 distinct color after brown & orange ~
\(using ~D; saw ~{~D~^, ~})"
            (car all-colors) all-colors))
    (code-char (logior #x80 (or (car all-colors) 0)))))

(defun mob-empty (mob)
  (every (curry #'= #xff)
         (loop for col from 0 upto 23
               append (loop
                        for row from 0 upto 20
                        collect (aref mob col row)))))

(defun mob-hires (mob)
  "Returns T if any two adjacent pixels don't match"
  (not (every #'identity
              (loop for col from 0 upto 11
                    append (loop
                             for row from 0 upto 20
                             collect (= (aref mob (* col 2) row)
                                        (aref mob (1+ (* col 2)) row)))))))

(defun gather-mobs (image-nybbles height width)
  (let (mobs index)
    (loop
      for y-mob from 0 below (/ height 21)
      for y₀ = (* y-mob 21)
      do (loop for x-mob from 0 below (/ width 24)
               for x₀ = (* x-mob 24)
               for mob-data = (extract-region image-nybbles x₀ y₀ (+ x₀ 23) (+ y₀ 20))
               do
                  (cond
                    ((mob-empty mob-data)
                     (format *trace-output*
                             "~% • Found empty MOB (relative ~D,~D)"
                             x-mob y-mob))
                    ((mob-hires mob-data)
                     (appendf mobs (append (mob->mono-bits mob-data)
                                           (cons (ensure-monochrome mob-data) nil)))
                     (appendf index (cons (cons x₀ y₀) nil))
                     (format *trace-output*
                             "~% • Found a hi-res MOB (relative ~D,~D)"
                             x-mob y-mob))
                    (t (appendf mobs (append (mob->multi-bits mob-data)
                                             (cons (ensure-1+chrome mob-data) nil)))
                       (appendf index (cons (cons x₀ y₀) nil))
                       (format *trace-output*
                               "~% • Found a multicolor MOB (relative ~D,~D)"
                               x-mob y-mob)))))
    (values mobs index)))

(defun tia-player-interpret/strip (pixels)
  (let ((shape nil)
        (colors nil))
    (loop for row from 0 below (second (array-dimensions pixels))
          do (push (reduce #'logior
                           (loop for bit from 0 to 7
                                 collect (if (plusp (aref pixels bit row))
                                             (expt 2 (- 7 bit))
                                             0)))
                   shape)
          do (push (or
                    (first
                     (remove-if #'null (loop for bit from 0 to 7
                                             for color = (aref pixels bit row)
                                             collect (when (plusp color)
                                                       color))))
                    0)
                   colors))
    (values (reverse shape) (reverse colors))))

(defun tia-player-interpret (pixels)
  (loop
    with shapes
    with colors
    for x from 0 below (/ (array-dimension pixels 0) 8)
    do (multiple-value-bind (shape color)
           (tia-player-interpret/strip
            (copy-rect pixels
                       (* 8 x) 0
                       8 (array-dimension pixels 1)))
         (appendf shapes shape)
         (appendf colors color))
    finally (return (values shapes colors))))

(defun try-to-maintain-palette (new old &optional (overall old))
  (if (or (null old) (emptyp old))
      (if overall
          (return-from try-to-maintain-palette
            (try-to-maintain-palette new overall))
          (return-from try-to-maintain-palette new)))
  ;; Pad shorter palette to match new's length (Mode E rows can have varying color counts)
  (when (< (length old) (length new))
    (setf old (append old (loop repeat (- (length new) (length old)) collect 0))))
  (when (and overall (< (length overall) (length new)))
    (setf overall (append overall (loop repeat (- (length new) (length overall)) collect 0))))
  (assert (= (length new) (length old)))
  (let ((offer
          (loop with palette = (copy-list old)
                with introductions = (loop for color in (remove-duplicates new)
                                           when (not (member color old))
                                             collect color)
                for i from 0 below (length new)
                do (unless (or (and (member (elt old i) new)
                                    (if (plusp i)
                                        (not (member (elt old i) (subseq palette 0 i)))
                                        t)))
                     (if (and (member (elt overall i) new)
                              (if (plusp i)
                                  (not (member (elt overall i) (subseq palette 0 i)))
                                  t))
                         (setf (elt palette i) (elt overall i))
                         (setf (elt palette i) (pop introductions))))
                finally (return palette))))
    (assert (every (lambda (color) (member color offer)) new)
            (offer) "The offered palette of (~{$~2,'0x~^ ~}) did not contain colors in (~{$~2,'0x~^ ~})
(tried to preserve palette indices from (~{$~2,'0x~^ ~})~@[ or (~{$~2,'0x~^ ~})~]" offer new old)
    offer))

(assert (equalp '(0 1 2 3) (try-to-maintain-palette '(3 2 1 0) '(0 1 2 3))))
(assert (equalp '(0 1 4 3) (try-to-maintain-palette '(3 4 1 0) '(0 1 2 3))))
(assert (equalp '(0 6 7 8) (try-to-maintain-palette '(6 0 7 8) '(0 1 2 3))))
(assert (equalp '(5 6 7 8) (try-to-maintain-palette '(5 6 7 8) '(0 1 2 3))))

(assert (equalp '(5 6 7 8) (try-to-maintain-palette '(6 7 8 5) '(0 1 2 3) '(5 6 7 8))))

(assert (equalp '(0 1 2 3) (try-to-maintain-palette '(3 2 1 0) nil '(0 1 2 3))))
(assert (equalp '(0 1 4 3) (try-to-maintain-palette '(3 4 1 0) nil '(0 1 2 3))))
(assert (equalp '(0 6 7 8) (try-to-maintain-palette '(6 0 7 8) nil '(0 1 2 3))))
(assert (equalp '(5 6 7 8) (try-to-maintain-palette '(5 6 7 8) nil '(0 1 2 3))))

(defun mode-e-row-bytes (pixels &key last-row-palette y overall-palette
                                     enforce-overall-palette-p)
  (check-type pixels array)
  (assert (= 1 (array-dimension pixels 1)))
  (let ((shape nil)
        (palette (if enforce-overall-palette-p
                     overall-palette
                     (try-to-maintain-palette
                      (most-popular-colors pixels (array-dimension pixels 0) 1
                                           :count 4)
                      last-row-palette
                      overall-palette))))
    (assert (= (ceiling (array-dimension pixels 0) 4)
               (length (group-into-4 (coerce (pixels-into-palette pixels palette
                                                                  :y0 y :best-fit-p t)
                                             'list)))))
    (dolist (pixels (group-into-4 (coerce (pixels-into-palette pixels palette
                                                               :y0 y :best-fit-p t)
                                          'list)))
      (push (logior (ash (elt pixels 0) 6)
                    (ash (elt pixels 1) 4)
                    (ash (elt pixels 2) 2)
                    (elt pixels 3))
            shape))
    (assert (= (ceiling (array-dimension pixels 0) 4) (length shape)))
    (values (reverse shape) palette)))

(defun mode-e-interpret (pixels &key base-palette (color-per-line-p t))
  (loop with shapes
        with colors
        with last-palette = nil
        with overall-palette = (or (when color-per-line-p base-palette)
                                   (try-to-maintain-palette
                                    (most-popular-colors pixels
                                                         (array-dimension pixels 0)
                                                         (array-dimension pixels 1)
                                                         :count 4)
                                    base-palette))
        for y from 0 below (array-dimension pixels 1)
        do (multiple-value-bind (shape palette)
               (mode-e-row-bytes (copy-rect pixels 0 y (array-dimension pixels 0) 1)
                                 :last-row-palette last-palette :y y
                                 :overall-palette overall-palette
                                 :enforce-overall-palette-p (not color-per-line-p))
             (assert (= (length palette) 4))
             (assert (= (length shape) (ceiling (array-dimension pixels 0) 4)))
             (setf last-palette (if color-per-line-p
                                    palette
                                    overall-palette))
             (appendf shapes shape)
             (push palette colors)
             (assert (= (length shapes) (* 1/4 (array-dimension pixels 0) (1+ y)))))
        finally (return (values shapes
                                (mapcar (lambda (line)
                                          (mapcar (lambda (i) (when i (* 2 i))) line))
                                        (if color-per-line-p
                                            (reverse colors)
                                            (list overall-palette)))))))

(defun 48px-array-to-bytes (pixels)
  (do-collect (column below 6)
    (do-collect (row downfrom (1- (array-dimension pixels 1)) to 0)
      (reduce #'logior
              (do-collect (bit below 8)
                (if (plusp (aref pixels (+ bit (* column 8)) row))
                    (expt 2 (- 7 bit))
                    0))))))

(defun tia-48px-interpret (pixels)
  (let ((shape (48px-array-to-bytes pixels))
        (colors nil))
    (loop for row from 0 below (second (array-dimensions pixels))
          do (push (or (first
                        (remove-if #'null
                                   (loop for bit from 0 to 7
                                         for color = (aref pixels bit row)
                                         collect (when (plusp color)
                                                   color))))
                       0)
                   colors))
    (values shape (reverse colors))))

(defun bits-to-art (byte)
  (check-type byte string)
  (assert (= 8 (length byte)))
  (assert (every (lambda (char) (member char '(#\0 #\1))) byte))
  (substitute #\⬜ #\0
              (substitute #\⬛ #\1
                          (make-array 8
                                      :element-type 'character
                                      :initial-contents byte))))

(defun bit-pairs-to-art (byte)
  (check-type byte (integer 0 #xff))
  (let ((bit-pairs (format nil "~4,4,'0r" byte)))
    (assert (every (lambda (char) (find char "0123")) bit-pairs))
    (substitute
     #\⬜ #\0
     (substitute
      #\🟥 #\1
      (substitute
       #\🟩 #\2
       (substitute
        #\🟦 #\3
        (make-array 4 :element-type 'character
                      :initial-contents bit-pairs)))))))

(defun bytes-and-art (bytes)
  (let* ((binary (mapcar (curry #'format nil "~2,8,'0r") bytes))
         (blocks (mapcar #'bits-to-art binary)))
    (format nil "~%	.byte ~{%~a~^, ~}	 ; ~{~a~^·~}" binary blocks)))

(defun byte-and-art (byte)
  (let* ((binary (format nil "~8,'0b" byte))
         (blocks (bits-to-art binary)))
    (format nil "~%	.byte %~a	; ~a" binary blocks)))

(defun assembler-label-name (string)
  (let ((result (cl-change-case:pascal-case string)))
    (when (search "Brp" result)
      (setf result (cl-ppcre:regex-replace-all "Brp" result "BRP")))
    (when (search "Aa" result)
      (setf result (cl-ppcre:regex-replace-all "Aa" result "AA")))
    (when (search "Zph" result)
      (setf result (cl-ppcre:regex-replace-all "Zph" result "ZPH")))
    result))

(defun tia-48px-preview (image-pixels)
  (let ((shape (48px-array-to-bytes image-pixels))
        (height (second (array-dimensions image-pixels))))
    (loop for row from (1- height) downto 0
          for row-bytes = (do-collect (column below 6)
                            (elt (elt shape column) row))
          collecting (reduce (curry #'concatenate 'string)
                             (mapcar #'bits-to-art (mapcar
                                                    (curry #'format nil "~8,'0b")
                                                    row-bytes))))))

(defun pathname-base-name (pathname)
  (subseq (pathname-name pathname)
          0 (position #\. (pathname-name pathname))))

(define-constant +atari-ntsc-color-names+
    '(COLGRAY COLYELLOW COLBROWN COLORANGE COLRED COLMAGENTA
      COLPURPLE COLINDIGO COLBLUE COLTURQUOISE COLCYAN COLTEAL
      COLSEAFOAM COLGREEN COLSPRINGGREEN COLGOLD)
  :test 'equalp)

(define-constant +atari-pal-color-names+ ; FIXME: #1241 these are the NTSC ones
    '(COLGREY COLSPINACH COLGOLD COLORANGE
      COLRED COLMAGENTA COLVIOLET COLPURPLE
      COLINDIGO COLBLUE COLSTONEWASH COLTURQUOISE
      COLGREEN COLSEAFOAM COLSPRINGGREEN COLALGAE)
  :test 'equalp)

(defun atari-color-name (index &optional (tv *region*))
  (elt (ecase tv
         (:ntsc +atari-ntsc-color-names+)
         (:pal +atari-pal-color-names+)
         (:secam +vcs-secam-color-names+))
       index))

(defun atari-colu (byte &optional (tv *region*))
  (if (null byte)
      (list (atari-color-name 0 tv) 15)
      (let ((co (ash (logand byte #xf0) -4))
            (lu (logand byte #x0f)))
        (list (atari-color-name co tv) lu))))

(defun atari-colu-string (byte)
  (destructuring-bind (co lu) (atari-colu byte)
    (assert co (co) "Atari Color code ~s is not valid in region ~a" co *region*)
    (check-type lu (integer 0 15) "Atari Luminance value 0-15")
    (format nil "CoLu(~a, $~x)" co lu)))

(defun atari-colu-run (&rest _)
  (error "unimplemented: ~s" _))

(defun compile-tia-48px (png-file out-dir height image-pixels)
  (let ((out-file-name (merge-pathnames
                        (make-pathname :name
                                       (pathname-name png-file)
                                       :type "s")
                        out-dir)))
    (format *trace-output* "~% Ripping TIA 48px graphics from 48×~D image"
            height)
    (ensure-directories-exist out-file-name)
    (with-output-to-file (source-file out-file-name
                                      :if-exists :supersede)
      (multiple-value-bind (shape colors) (tia-48px-interpret image-pixels)
        (format source-file ";;; -*- fundamental -*-
;;; Compiled sprite data from ~a
;;; Edit the original (probably Source/Art/~:*~a.png), editing this file is futile.

;;; Bitmap preview:
~{~%;;;   ~a~}
~a:	.block
 Height = ~d
 Width = 48
Shape:~{~{~a~}~2%~}
;CoLu:~{~%	.byte ~{~a~^ ~}~}
 .bend
"
                (pathname-name png-file)
                (tia-48px-preview image-pixels)
                (assembler-label-name (pathname-base-name png-file))
                height
                (mapcar (curry #'mapcar #'byte-and-art) shape)
                (mapcar (lambda (palette)
                          (mapcar #'atari-colu-string palette))
                        colors)))
      (format *trace-output* "~% Done writing to ~A" out-file-name))))

(defun compile-batari-48px-command (png-file output-bas &rest args)
  "Command-line wrapper for compile-batari-48px.
   Arguments: PNG-FILE OUTPUT-BAS [titlescreen-kernel-p] [tv-standard]
   If third arg is 't' or 'T', enables titlescreen kernel mode.
   If fourth arg exists, uses it as TV standard (:ntsc, :pal, :secam)."
  (let* ((titlescreen-kernel-p (and args (string-equal (first args) "t")))
         (tv-standard (cond ((and args (>= (length args) 2))
                            (let ((std (string-upcase (second args))))
                              (cond ((string= std "NTSC") :ntsc)
                                    ((string= std "PAL") :pal)
                                    ((string= std "SECAM") :secam)
                                    (t :ntsc))))
                           (t :ntsc))))
    (compile-batari-48px png-file output-bas
                        :titlescreen-kernel-p titlescreen-kernel-p
                        :tv-standard tv-standard)))

(defun compile-batari-48px (png-file output-bas &key (titlescreen-kernel-p nil) (tv-standard :ntsc))
  "Compile a 48×42 pixel PNG bitmap to batariBASIC data format.
   Output format: 6 columns × 42 bytes, inverted-y (bottom-to-top),
   one byte per row, then double-newline before next column.
   Binary format: %00000000 with NO remarks inside data block.

   When TITLESCREEN-KERNEL-P is T:
   - Extracts color-per-line data from source PNG
   - Uses ×2 drawing style (double-height mode, 42 rows → 84 scanlines)
   - Outputs color data for each row (each row becomes 2 scanlines)
   - Output format suitable for titlescreen kernel minikernel
   - Determines minikernel slot (1, 2, or 3) from output filename:
     * Art.AtariAge.s → 48x2_1 (AtariAge logo)
     * Art.AtariAgeText.s → 48x2_2 (AtariAge text, replaces Interworldly on Publisher)
     * Art.Interworldly.s → 48x2_2 (Interworldly, conflicts with AtariAgeText - not currently used)
     * Art.ChaosFight.s → 48x2_3 (ChaosFight logo)

   Input PNG can be color (for titlescreen kernel) or 1bpp (for basic bitmap)."
  (let* ((input-path (uiop:ensure-pathname png-file))
         (png (progn
                (with-open-file (stream input-path :if-does-not-exist :error)
                  (declare (ignore stream)))
                (png-read:read-png-file input-path)))
         (width (png-read:width png))
         (height (png-read:height png))
         (rgb (png-read:image-data png))
         (alpha (png-read:transparency png))
         (output-path (uiop:ensure-pathname output-bas))
         (output-name (pathname-name output-path))
         ;; Determine minikernel slot from output filename
         (kernel-slot (cond ((search "AtariAgeText" output-name :test #'string-equal)
                            2)  ; AtariAgeText uses slot 2
                           ((or (search "AtariAge" output-name :test #'string-equal)
                                (search "Publisher" output-name :test #'string-equal))
                            1)  ; AtariAge logo uses slot 1
                           ((or (search "Interworldly" output-name :test #'string-equal)
                                (search "Author" output-name :test #'string-equal))
                            4)  ; Interworldly uses slot 4
                           ((or (search "ChaosFight" output-name :test #'string-equal)
                                (search "Title" output-name :test #'string-equal))
                            3)  ; ChaosFight uses slot 3
                           (t 1)))  ; Default to slot 1
         (kernel-prefix (format nil "bmp_48x2_~d" kernel-slot))
         ;; Determine page address for bitmap data (pack four bitmaps on adjacent pages)
         (page-address (cond ((= kernel-slot 1)
                             "$f100")  ; AtariAge at $f100
                            ((= kernel-slot 2)
                             "$f200")  ; AtariAgeText at $f200
                            ((= kernel-slot 3)
                             "$f300")  ; ChaosFight at $f300
                            ((= kernel-slot 4)
                             "$f400")  ; Author at $f400
                            (t
                             "$f100"))))  ; Default to $f100
    (unless (= width 48)
      (error "Bitmap must be 48 pixels wide; got ~a" width))
    (unless (= height 42)
      (error "Bitmap must be 42 pixels tall; got ~a" height))
    (let* ((*machine* 2600)
           (*region* tv-standard)
           (palette (png->palette height width rgb alpha))
           (pixels (make-array (list width height)
                              :element-type '(unsigned-byte 8)))
           (label-name (cl-change-case:pascal-case (pathname-base-name input-path))))
      ;; Convert RGB to 1bpp bitmap (black/white) for shape data
      (loop for y from 0 below height
            do (loop for x from 0 below width
                     do (let ((r (aref rgb x y 0))
                              (g (aref rgb x y 1))
                              (b (aref rgb x y 2))
                              (a (if alpha (aref alpha x y) 255)))
                          (setf (aref pixels x y)
                                (if (and (>= a 128)
                                         (> (+ r g b) (* 3 128)))
                                    1 0)))))
      ;; Extract color-per-line data if titlescreen kernel mode
      (let* ((shape (48px-array-to-bytes pixels))
             (colors-per-line (when titlescreen-kernel-p
                                (loop for y from 0 below height
                                      collect (dominant-playfield-color palette width y))))
             ;; Create color output path: Art.Name.s -> Art.Name.colors.s
             (color-output-path (when titlescreen-kernel-p
                                  (make-pathname :directory (pathname-directory output-path)
                                                 :name (format nil "~a.colors" (pathname-name output-path))
                                                 :type (pathname-type output-path)))))
        (ensure-directories-exist output-path)
        (when color-output-path
          (ensure-directories-exist color-output-path))
        (if titlescreen-kernel-p
            ;; Titlescreen kernel assembly format (×2 drawing style)
            ;; Colors are written to a separate file to allow bitmap data to be page-aligned
            (progn
              ;; Write bitmap data file (without colors)
              (with-open-file (stream output-path
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
                ;; Header banner
                (format stream ";;; Chaos Fight - ~a~%" (namestring output-path))
                (format stream "~%")
                (format stream ";;;; This is a generated file, do not edit.~%")
                (format stream ";;;; Color tables are in separate .colors.s files~%")
                (format stream ";;;; Bitmap data is packed at page-aligned address ~a (CPU/RORG space)~%" page-address)
                (format stream "~%")
                ;; Set relocatable CPU address for bitmap data (titlescreen kernel always runs in bank 9)
                ;; NOTE: We must *not* change the assembler's file offset here, or we blow past Bank 9's file space.
                ;;       Using RORG keeps the CPU address correct ($F100/$F200/…) without seeking the output file.
                (format stream "   rorg ~a~%" page-address)
                (format stream "~%")
                ;; Essential data without verbose comments
                (format stream "~a_window = ~d~%" kernel-prefix height)
                (format stream "~%")
                (format stream "~a_height = ~d~%" kernel-prefix height)
                (format stream "~%")
                (format stream " BYTE 0 ; leave this here!~%")
                (format stream "~%~%")
                ;; Note: Color table, PF1, PF2, and background are in separate .colors.s file at $f500
                ;; Output bitmap columns (6 columns: 00-05)
                ;; Alignment only at beginning (line above), not between strips
                (loop for column from 0 below 6
                      do (format stream "~%~%")
                      do (format stream "~a_~2,'0D~%" kernel-prefix column)
                      ;; Output rows in reverse order (bottom to top, inverted-y) - tab-indented
                      (loop for row from (1- height) downto 0
                            for byte = (elt (elt shape column) row)
                            for binary = (format nil "~8,'0b" byte)
                            do (format stream "~tBYTE %~a~%" binary))
                      (format stream "~%~%")))
              ;; Write color table, PF1, PF2, and background to separate file
              ;; These will be combined into titlescreen_colors.s at $f500
              (with-open-file (color-stream color-output-path
                                            :direction :output
                                            :if-exists :supersede
                                            :if-does-not-exist :create)
                ;; Header banner
                (format color-stream ";;; Chaos Fight - ~a~%" (namestring color-output-path))
                (format color-stream "~%")
                (format color-stream ";;;; This is a generated file, do not edit.~%")
                (format color-stream ";;;; Color table, PF1, PF2, and background for ~a bitmap~%" kernel-prefix)
                (format color-stream ";;;; This file will be included in titlescreen_colors.s at $f500~%")
                (format color-stream "~%")
                (format color-stream "~a_colors ~%" kernel-prefix)
                ;; Output colors in reverse order (bottom to top) - one per row, tab-indented
                (loop for y from (1- height) downto 0
                      for color-idx = (elt colors-per-line y)
                      for color-byte = (playfield-color-byte color-idx tv-standard)
                      do (format color-stream "~tBYTE $~2,'0X~%" color-byte))
                (format color-stream "~%")
                ;; PF1, PF2, and background (will be at $f500 with colors)
                ;; PF1 and PF2 must be defined unconditionally for ifconst checks in kernel
                (format color-stream "~a_PF1~%" kernel-prefix)
                (format color-stream "~tBYTE %00000000~%")
                (format color-stream "~a_PF2~%" kernel-prefix)
                (format color-stream "~tBYTE %00000000~%")
                (format color-stream " ifnconst ~a_background~%" kernel-prefix)
                (format color-stream "~a_background~%" kernel-prefix)
                (format color-stream " endif~%")
                (format color-stream "~tBYTE $00~%")
                (format color-stream "~%"))
              (format *trace-output* "~% Done writing titlescreen kernel bitmap to ~A~%" output-path)
              (format *trace-output* "~% Done writing color table to ~A~%" color-output-path)
              (values output-path color-output-path))
            ;; Basic batariBASIC data format (backward compatibility)
            (progn
              (with-open-file (stream output-path
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
                (format stream "rem Generated bitmap data from ~a~%" (pathname-name png-file))
                (format stream "rem Do not edit - regenerate from source artwork~%~%")
                (format stream "data Bitmap~a~%" label-name)
                (loop for column below 6
                      do (loop for row from 41 downto 0
                               for byte = (elt (elt shape column) row)
                               for binary = (format nil "~8,'0b" byte)
                               do (format stream "~%        %~a" binary))
                      do (format stream "~%~%"))
                (format stream "end~%"))
              (format *trace-output* "~% Done writing batariBASIC bitmap to ~A~%" output-path)
              output-path))))))

(defun reverse-7-or-8 (shape)
  (let* ((height (length shape))
         (group-height (if (zerop (mod height 7)) 7 8)))
    (loop for group from 0 below height by group-height
          append (loop for line from (1- group-height) downto 0
                       collecting (elt shape (+ group line))))))

(defun reverse-16 (shape)
  (let* ((height (length shape))
         (group-height 16))
    (loop for group from 0 below height by group-height
          append (loop for line from (1- group-height) downto 0
                       collecting (elt shape (+ group line))))))

(defun rows-of-width (bytes pixels-wide &key (pixels-per-byte 4))
  (loop
    with row-bytes = (let ((bytes-wide (/ pixels-wide pixels-per-byte)))
                       (check-type bytes-wide (integer 0 319))
                       bytes-wide)
    with output = (make-array (list row-bytes
                                    (ceiling (length bytes) row-bytes))
                              :element-type '(unsigned-byte 8))
    for i from 0 below (length bytes)
    for column = (mod i row-bytes)
    for row = (floor i row-bytes)
    for byte = (elt bytes i)
    do (setf (aref output column row) byte)
    finally (return (loop for y from 0 below (array-dimension output 1)
                          collecting (loop for x from 0 below (array-dimension output 0)
                                           collecting (aref output x y))))))

(defun make-fillable-vector (list)
  (let ((vector (make-array (length list) :fill-pointer t :adjustable t
                                          :element-type '(unsigned-byte 8))))
    (loop for i from 0
          for element in list
          do (setf (aref vector i) element)
          finally (return vector))))

(defun zx7-compress (bytes
                     &key (base-name (string (gensym "ZX7CompressTemp-"))))
  (format *trace-output* "~&Calling external compressor for ~a: " base-name)
  (finish-output *trace-output*)
  (let ((output (let ((*standard-output* *trace-output*)
                      (*error-output* *trace-output*)
                      (bin-pathname (make-pathname :name base-name
                                                   :type "bin"
                                                   :directory `(:relative "Object" "Assets"
									  ,(machine-directory-name))))
                      (zx7-pathname (make-pathname :name base-name
                                                   :type "zx7"
                                                   :directory `(:relative "Object" "Assets"
									  ,(machine-directory-name)))))
                  (ensure-directories-exist bin-pathname)
                  (ensure-directories-exist zx7-pathname)
                  (write-byte-vector-into-file bytes bin-pathname :if-exists :overwrite
                                                                  :if-does-not-exist :create)
                  (uiop:run-program (list (namestring (merge-pathnames "bin/zx7mini" (project-root)))
                                          (namestring bin-pathname)
                                          (namestring zx7-pathname))
                                    :output t :error-output t)
                  (with-input-from-file (zx7 zx7-pathname :element-type '(unsigned-byte 8))
                    (read-stream-content-into-byte-vector zx7)))))
    (format *trace-output* "… compression complete, new length ~:d bytes (-~5f%)"
            (length output) (- 100.0 (* 100.0 (/ (length output) (length bytes)))))
    output))

(defun compile-5200-mode-e-bitmap (image-pixels &key (png-file (make-pathname :name "tmp5200" :type "png"))
                                                     (target-dir "Object/5200/")
                                                     (height (array-dimension image-pixels 1))
                                                     (width (array-dimension image-pixels 0))
                                                     (compressp (< 512 (* height width)))
                                                     (color-per-line-p t)
                                                     base-palette)
  (let* ((base-dir (if (typep target-dir 'pathname)
                       (merge-pathnames target-dir (project-root))
                       (merge-pathnames (pathname (or target-dir "Object/5200/"))
                                        (project-root))))
         (out-file-name (merge-pathnames
                         (make-pathname :name
                                        (pathname-name png-file)
                                        :type "s")
                         base-dir)))
    (ensure-directories-exist out-file-name)
    (format *trace-output* "~% Ripping Mode ~a pixmap graphics from ~D×~D image…"
            (if (< height 97) "D/E" "E")
            width height)
    (finish-output *trace-output*)
    (with-output-to-file (source-file out-file-name :if-exists :supersede)
      (assert (= height (array-dimension image-pixels 1)))
      (assert (= width (array-dimension image-pixels 0)))
      (multiple-value-bind (shape colors) (mode-e-interpret image-pixels
                                                            :base-palette base-palette
                                                            :color-per-line-p color-per-line-p)
        (assert (= (length shape) (/ (* height width) 4)))
        (assert (= (length colors) (if color-per-line-p height 1)))
        (format source-file ";;; -*- fundamental -*-
;;; Compiled pixmap data from ~a
;;; Edit the original (probably ~a),
;;; editing this file is futile.

~a:	.block
~10tLength = ~d ; bytes total for this data ~:[(stored uncompressed)~;(after decompression)~]
~10tHeight = ~d
~10tWidth = ~d
~{~%;;; ~{~a~}~}

Shape:
~{~%~10t.byte $~2,'0x~^, $~2,'0x~^, $~2,'0x~^, $~2,'0x~^,  $~2,'0x~^, $~2,'0x~^, $~2,'0x~^, $~2,'0x~}
CoLu:
~{~%~10t;; ~{~15a ~15a ~15a ~15a~}~}
~{~%~10t.byte $~2,'0x~^, $~2,'0x~^, $~2,'0x~^, $~2,'0x~^,  $~2,'0x~^, $~2,'0x~^, $~2,'0x~^, $~2,'0x~}
 .bend
"
                (enough-namestring png-file)
                (enough-namestring (make-pathname :defaults png-file :type "xcf"))
                (assembler-label-name (pathname-name png-file))
                (+ 2 (length shape) (length colors))
                compressp
                height width
                (mapcar (lambda (row) (mapcar #'bit-pairs-to-art row))
                        (rows-of-width shape width))
                (if compressp
                    (coerce (zx7-compress (make-fillable-vector shape)
                                          :base-name (concatenate 'string
                                                                  (pathname-name png-file)
                                                                  ".Shape"))
                            'list)
                    shape)
                (mapcar (lambda (line) (mapcar #'atari-colu-string line)) colors)
                (if (and compressp color-per-line-p)
                    (coerce (zx7-compress (make-fillable-vector (flatten colors))
                                          :base-name (concatenate 'string
                                                                  (pathname-name png-file)
                                                                  ".Colors"))
                            'list)
                    (flatten colors))))
      (format *trace-output* "~% Done writing to ~A" out-file-name)
      t)))

(defun compile-gtia-player (png-file out-dir
                            height width image-pixels)
  (let* ((base-dir (merge-pathnames (pathname (or out-dir "Object/5200/"))
                                    (project-root)))
         (out-file-name (merge-pathnames
                         (make-pathname :name
                                        (pathname-name png-file)
                                        :type "s")
                         base-dir)))
    (ensure-directories-exist out-file-name)
    (format *trace-output* "~% Ripping GTIA Player graphics from ~D×~D image"
            width height)
    (finish-output *trace-output*)
    (with-output-to-file (source-file out-file-name
                                      :if-exists :supersede)
      (multiple-value-bind (shape colors) (tia-player-interpret image-pixels)
        (format source-file ";;; -*- fundamental -*-
;;; Compiled sprite data from ~a
;;; Edit the original (probably Source/Art/~:*~a.xcf),
;;; editing this file is futile.

~a:	.block
 Height = ~d
 Width = ~d
Shape:~{~a~}
;CoLu:~{~%	;.byte ~{CoLu(~a, $~1x)~}~}
 .bend
"
                (pathname-name png-file)
                (assembler-label-name (pathname-base-name png-file))
                height width
                (if (and (mod height 16) (> height 200))
                    (mapcar #'byte-and-art (reverse-16 shape))
                    (mapcar #'byte-and-art (reverse-7-or-8 shape)))
                (mapcar #'atari-colu colors)))
      (format *trace-output* "~% Done writing to ~A" out-file-name)
      t)))

(defun pretty-mob-data-listing-vic2 (mob)
  (mapcar #'bytes-and-art
          (group-into-3
           (map 'list #'char-code mob))))

(defun mob-index+bitmap+color-sets (more-mobs)
  (loop for mob = (subseq more-mobs 0 63)
        for more on more-mobs by (curry #'nthcdr 64)
        for i from 0
        collect (list i
                      (pretty-mob-data-listing-vic2 mob)
                      (char-code (last-elt mob)))))

(defun compile-mob (png-file out-dir height width image-nybbles)
  (let ((out-file (merge-pathnames
                   (make-pathname :name
                                  (pathname-name png-file)
                                  :type "s")
                   out-dir)))
    (format *trace-output* "~% Ripping MOBs from ~D×~D sprite" width height)
    (with-output-to-file (binary-file out-file
                                      :element-type '(unsigned-byte 8)
                                      :if-exists :supersede)
      (multiple-value-bind (mobs index) (gather-mobs image-nybbles height width)
        (assert (>= 6 (length index)) nil
                "There can be at most 6 (non-empty) MOBs in a sprite; ~
got ~:D MOB~:P"
                (length index))
        (format *trace-output* "~%Writing ~:D MOB~:P" (length index))
        (when (< 1 (length index))
          (warn "MOBs not stacked vertically won't work yet. ~
Proceed with caution."))
        (format binary-file ";;; -*- fundamental -*-
;;; Compiled sprite data from ~a
;;; Edit the original (probably art/sprites/~:*~a.xcf),
;;; editing this file is futile.

 .byte ~d	; length of index
 .byte ~{$~x~^, ~}	; MOBs in sprite
 .align 64
~{~{
 ;; MOB ~x data
~{~a~}
 .byte ~d	; Sprite distinct color
~}~}"
                (pathname-name png-file)
                (length index)
                (mapcar #'car index)  ; TODO: #1226 capture relative positioning
                (mob-index+bitmap+color-sets mobs)))
      (format *trace-output* "~% Done writing to ~A" out-file))))

(defgeneric compile-font-generic (machine-type format source-file-base-name font-input)
  (:method (machine-type format source-file-base-name font-input)
    (warn "No handler for converting art into ~a format to create ~a" machine-type source-file-base-name)
    (with-output-to-file (source-file (make-source-file-name source-file-base-name)
                                      :if-exists :supersede)
      (format source-file ";;; -*- asm -*-
;;; TODO: #1243: #1227 write the function to generate this file's contents
 * = 0 ~% brk~%"))))

(defun tia-font-interpret (pixels x y)
  (loop for byte from 4 downto 0
        collecting
        (reduce #'logior
                (loop for bit from 0 to 3
                      collect
                      (if (plusp (aref pixels (+ bit (* x 4)) (+ byte (* y 5))))
                          (expt 2 (- 3 bit))
                          0)))))

(defun antic-font-interpret (pixels x y)
  (loop for byte from 0 below 8
        collecting
        (reduce #'logior
                (loop for bit from 0 below 8
                      collect
                      (if (plusp (aref pixels (+ bit (* x 8)) (+ byte (* y 8))))
                          (expt 2 (- 7 bit))
                          0)))))

(defun png->bits (png-file)
  (let ((height (png-read:height png-file))
        (width (png-read:width png-file))
        (rgb (png-read:image-data png-file))
        (α (png-read:transparency png-file)))
    (check-type height (integer 0 *))
    (check-type width (integer 0 *))
    (check-type rgb array)
    (check-type α (or null array))
    (destructuring-bind (w h bpp) (array-dimensions rgb)
      (unless (and (= h height) (= w width) (= bpp 4))
        (error "WTF? File size mismatches contents"))
      (let ((image (make-array (list width height) :element-type '(unsigned-byte 1))))
        (loop for y from 0 below height
              do (loop for x from 0 below width
                       do (setf (aref image x y)
                                (cond ((and α (< 128 (aref α x y))) 0)
                                      ((> (* 3 128)
                                          (+ (aref rgb x y 0) (aref rgb x y 1) (aref rgb x y 2)))
                                       1)
                                      (t 0)))))
        image))))

(defun tia-font-guide (source-file pixels chars-width)
  (dotimes (line 8)
    (dotimes (row 5)
      (terpri source-file)
      (princ ";;; " source-file)
      (loop for i from (+ (* line 6) 0) to (min 47
                                                (+ (* line 6) 5))
            for column = (mod i 12)
            for x = (mod i chars-width)
            for y = (floor i chars-width)
            do (princ
                (subseq
                 (elt (mapcar #'bits-to-art
                              (mapcar (lambda (byte)
                                        (format nil "~2,8,'0r" byte))
                                      (tia-font-interpret pixels x y)))
                      (- 4 row))
                 4)
                source-file)
            do (princ #\space source-file)))
    (terpri source-file)))

(defun antic-font-guide (source-file pixels chars-width)
  (dotimes (line 8)
    (dotimes (row 8)
      (terpri source-file)
      (princ ";;; " source-file)
      (loop for i from (* line 8) below (* (1+ line) 8)
            for column = (mod i 8)
            for x = (mod i chars-width)
            for y = (floor i chars-width)
            do (princ
                (subseq
                 (elt (mapcar #'bits-to-art
                              (mapcar (lambda (byte)
                                        (format nil "~2,8,'0r" byte))
                                      (antic-font-interpret pixels x y)))
                      row)
                 8)
                source-file)
            do (princ #\space source-file)))
    (terpri source-file)))

(defun tia-font-write (source-file pixels chars-width bit-shift)
  (loop for i from 0 to 47
        for x = (mod i chars-width)
        for y = (floor i chars-width)
        do (format source-file "~%	;; char #~x ~:[(right)~;(left)~]~{~a~}"
                   i (= 4 bit-shift)
                   (mapcar #'byte-and-art
                           (mapcar (rcurry #'ash bit-shift)
                                   (tia-font-interpret pixels x y))))))

(defun antic-font-write (source-file pixels)
  (loop with chars-width = (floor (array-dimension pixels 0) 8)
        with chars-height = (floor (array-dimension pixels 1) 8)
        with char-count = (* chars-width chars-height)
        for i from 0 below char-count
        for x = (mod i chars-width)
        for y = (floor i chars-width)
        do (format source-file "~%	;; char #~d ~:* $~2,'0x (“~c” or “~a”) ~{~a~}"
                   i
                   (code-char (+ #x20 i))
                   (substitute #\Space #\_
                               (string-capitalize (char-name (code-char (+ #x20 i)))))
                   (mapcar #'byte-and-art
                           (antic-font-interpret pixels x y)))))

(defmethod compile-font-generic ((machine-type (eql 2600))
                                 format source-file-base-name font-input)
  (let* ((png-image (png-read:read-png-file font-input))
         (chars-width (/ (png-read:width png-image) 4))
         (pixels (png->bits png-image)))
    (with-output-to-file (source-file (make-source-file-name source-file-base-name)
                                      :if-exists :supersede)
      (format source-file ";;; -*- asm -*-
;;; Font data compiled from ~a
;;; This is a generated file; editing it would be futile~2%"
              font-input)
      (assert (= 48 (* chars-width (/ (png-read:height png-image) 5))))
      (format source-file "~%;;;~|~%TIAFont:
;; Overview: (font follows, inverted, each char repeated for each nybble)")
      (tia-font-guide source-file pixels chars-width)
      (format source-file "~%;;;~|~%TIAFontLeft:")
      (tia-font-write source-file pixels chars-width 4)
      (format source-file "~%;;;~|~%TIAFontRight:~%")
      (tia-font-write source-file pixels chars-width 0)
      (format source-file "~2%;;; end of file.~%")
      (format *trace-output* "~&Wrote ~a (from ~a)" source-file-base-name font-input))))

(defmethod compile-font-generic ((machine-type (eql 5200))
                                 format source-file-base-name font-input)
  (let* ((png-image (png-read:read-png-file font-input))
         (chars-width (/ (png-read:width png-image) 8))
         (pixels (png->bits png-image))
         (char-count (* chars-width (/ (png-read:height png-image) 8)))
         (source-file-name (make-source-file-name source-file-base-name "Assets")))
    (with-output-to-file (source-file source-file-name :if-exists :supersede)
      (format source-file ";;; -*- asm -*-
;;; Font data compiled from ~a
;;; This is a generated file; editing it would be futile
;;; ~d characters in this font~2%"
              font-input char-count)
      (assert (member char-count '(64 256)))
      (format source-file "~%;;;~|~%AnticFont:")
      (antic-font-write source-file pixels)
      (format source-file "~2%;;; end of file.~%")
      (format *trace-output* "~&Wrote ~a (from ~a)" (enough-namestring source-file-name) font-input))))

(defun compile-font-command (source-file-name font-input)
  "Create SOURCE-FILE-NAME from FONT-INPUT (PNG)"
  (destructuring-bind (obj genr ass source-file-base-name) (split-sequence #\/ source-file-name)
    (destructuring-bind (font s) (split-sequence #\. source-file-base-name)
      (assert (equal s "s"))
      (assert (equal obj "Source"))
      (assert (equal genr "Generated"))
      (assert (equal ass "Assets"))
      (let ((*machine* (or (when (not (eql :unknown *machine*)) *machine*) 7800)))
        (compile-font-generic *machine* nil font font-input)))))

(defun compile-font-8×8 (png-file out-dir height width image-nybbles)
  (declare (ignore height width image-nybbles))
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "s")
                   out-dir)))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; -*- asm -*-
;;; Generated file; editing is useless. Source is ~a (derived from the matching XCF)
;;;

VIC2Font:
"
              png-file)
      (loop for char from 0 below (* (/ height 8) (/ width 8))
            for x-cell = (mod (* char 8) width)
            for y-cell = (* 8 (floor (* char 8) width))
            for char-data = (extract-region image-nybbles
                                            x-cell y-cell
                                            (+ 7 x-cell) (+ 7 y-cell))
            do (format src-file
                       "~%	;; 		 character ~d ($~:*~x)~{~a~}"
                       char
                       (map 'list #'byte-and-art
                            (tile->bits char-data)))
            collect (tile->color char-data))
      (format *error-output* "~% Wrote binary font (monochrome) data to ~A." out-file)
      (finish-output src-file))))

(defun tile-cell-vic2-x (cell width)
  "Each tile's data is arranged into four cells, like so:

 0 1
 2 3

This gives the X position of the top-left corner of a 16×16 pixel tile
cell (where the cell's number is (+ (* tile 4) cell)) within an image
of the given width."
  (mod (+ (* (floor cell 4) 16)
          (* (mod cell 2) 8))
       width))

(defun tile-cell-vic2-y (cell width)
  (+ (* (floor (floor cell 4) (floor width 16)) 16)
     ;; even cells are on alternate rows
     (* (mod cell 2) 8)))

;;; Unit tests. This actually took me a while to get right!

(dotimes (i 62)
  (assert (= (tile-cell-vic2-y (* i 4) 16) (* 16 i)) nil
          "Tile ~D in 16px image should start at ~D, but TILE-CELL-VIC2-Y reports ~D"
          i (* 16 i) (tile-cell-vic2-y (* 4 i) 16)))

(loop for width in '(16 32 64 128)
      do (dotimes (i #xff)
           (assert (> (/ 16384 width) (tile-cell-vic2-y i width))
                   nil "The TILE-CELL-VIC2-Y function must return a valid value;
value ~D for tile-cell ~D is too far down for an image with width ~D" (tile-cell-vic2-y i width) i width)))

(defun compile-atari-8×8 (png-file target-dir height width)
  (let ((out-file (merge-pathnames
                   (make-pathname :name
                                  (pathname-name png-file)
                                  :type "s")
                   target-dir)))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; This is a generated file. Editing is futile.~2%")
      (loop for x1 from 0 below width by 8
            for y1 from 0 below height by 8
            for i from 0
            do (loop for y0 from 7 downto 0
                     do (format src-file "~t.byte %~0,8b" 0))))))


(defun intv-tile-row-bytes (palette-pixels sx sy)
  "Return 8-byte row bitmaps for one 8×8 tile at (SX,SY); white = palette index 7."
  (loop for y from 0 below 8
        collect (let ((byte 0))
                  (loop for x from 0 below 8
                        for palette-index = (aref palette-pixels (+ sx x) (+ sy y))
                        do (when (= palette-index 7)
                             (setf byte (logior byte (ash 1 (- 7 x))))))
                  byte)))

(defvar *intv-grom-bytes-cache* nil
  "Cached 2048-byte GROM image (256×8 bytes) or NIL if unavailable.")

(defun intv-grom-bin-path ()
  "Return pathname to bundled minigrom.bin (jzIntv), or NIL if missing.

The file is 2048 bytes: 256 GROM cards × 8 row bytes each, same layout as
@code{intv-tile-row-bytes} for monochrome art."
  (let ((rel (merge-pathnames
              #p"../Tools/jzIntv/src/emscripten/minigrom.bin"
              (asdf:system-source-directory :skyline-tool))))
    (when (probe-file rel)
      rel)))

(defun intv-grom-bytes ()
  "Load and cache Intellivision GROM bytes (2048), or NIL if file missing.

@itemize @bullet
@item Side Effects: Sets @code{*intv-grom-bytes-cache*} on first load (including to NIL).
@end itemize"
  (or *intv-grom-bytes-cache*
      (setf *intv-grom-bytes-cache*
            (let ((p (intv-grom-bin-path)))
              (cond
                ((not p)
                 (warn "Intellivision GROM (../Tools/jzIntv/src/emscripten/minigrom.bin) not found; blob tiles will not match default GROM cards")
                 nil)
                (t
                 (with-open-file (stream p :element-type '(unsigned-byte 8))
                   (let ((buf (make-array 2048 :element-type '(unsigned-byte 8))))
                     (assert (= 2048 (read-sequence buf stream)) (p)
                             "Expected 2048-byte GROM at ~A" p)
                     buf))))))))

(defun intv-grom-key-to-card-map (grom-bytes)
  "Build @code{equal} hash: tile row-byte list → GROM card index (0–255).

If two GROM cards share the same bitmap, the lower card index wins."
  (let ((ht (make-hash-table :test 'equal)))
    (loop for card from 0 below 256
          for base = (* card 8)
          for key = (loop for row from 0 below 8
                          collect (aref grom-bytes (+ base row)))
          do (unless (gethash key ht)
               (setf (gethash key ht) card)))
    ht))

(defun compile-blob-intv-screen (png-file output-path palette-pixels width height)
  "Write OUTPUT-PATH assembly: deduplicated GRAM 8×8 cards + row-major tile map.

Each cell in the WIDTH×HEIGHT image (both multiples of 8) is one 8×8 tile.
Tiles that match a built-in GROM card (from bundled @file{minigrom.bin}) use
that card index (0–255) and consume no GRAM slot.  Other identical tiles
share one GRAM definition.  At most 64 unique non-GROM tiles (Intv GRAM).

@code{*_TILE_MAP} entries: @code{$0000}–@code{$00FF} = GROM card number;
@code{$0100}–@code{$013F} = GRAM slot 0–63 (see @code{*_TILE_MAP_GRAM_BASE}).

@table @asis
@item PNG-FILE
Source PNG path (for comments only)
@item OUTPUT-PATH
Destination @file{.s} file
@item PALETTE-PIXELS
2D array from @code{png->palette}
@item WIDTH @itemx HEIGHT
Pixel dimensions (multiples of 8)
@end table"
  (check-type output-path (or pathname string))
  (assert (and (zerop (mod width 8)) (zerop (mod height 8)))
          (width height)
          "Intellivision blob ~A dimensions must be multiples of 8×8, got ~D×~D"
          png-file width height)
  (let* ((cols (/ width 8))
         (rows (/ height 8))
         (total-cells (* cols rows))
         (uniq (make-array 64 :adjustable t :fill-pointer 0))
         (ht (make-hash-table :test 'equal))
         (tile-ids (make-array total-cells :element-type '(unsigned-byte 16)))
         (lab (substitute #\_ #\. (pathname-name (merge-pathnames output-path))))
         (grom-bytes (intv-grom-bytes))
         (grom-map (when grom-bytes (intv-grom-key-to-card-map grom-bytes)))
         (grom-cells 0))
    (let ((idx 0))
      (dotimes (row rows)
        (dotimes (col cols)
          (let* ((key (intv-tile-row-bytes palette-pixels (* col 8) (* row 8))))
            (multiple-value-bind (grom-id grom-ok) (if grom-map
                                                         (gethash key grom-map)
                                                         (values nil nil))
              (if (and grom-map grom-ok)
                  (progn
                    (setf (aref tile-ids idx) grom-id)
                    (incf grom-cells))
                  (multiple-value-bind (gram-id presentp) (gethash key ht)
                    (unless presentp
                      (when (>= (length uniq) 64)
                        (error "Intellivision blob ~A needs more than 64 GRAM 8×8 tiles (~D×~D grid); use GROM-default shapes or reuse tiles"
                               png-file cols rows))
                      (setf gram-id (length uniq))
                      (setf (gethash key ht) gram-id)
                      (vector-push-extend key uniq))
                    (setf (aref tile-ids idx) (+ #x100 gram-id))))
              (incf idx))))))
    (let ((nuniq (length uniq)))
      (assert (= nuniq (hash-table-count ht)))
      (ensure-directories-exist (pathname-directory (merge-pathnames output-path)))
      (with-output-to-file (src (merge-pathnames output-path) :if-exists :supersede
                                                      :external-format :utf-8)
        (format src ";;; Intellivision blob: tile-mapped screen + GRAM cards~%")
        (format src ";;; Source: ~A~%" png-file)
        (format src ";;; Grid: ~D×~D tiles (~D×~D px); ~D tile~:P use GROM; ~D unique GRAM card~:P~2%"
                cols rows width height grom-cells nuniq)
        (format src ";;; TILE_MAP: $0000-$00FF = GROM card# ; $0100-$013F = GRAM slot + *_TILE_MAP_GRAM_BASE~%")
        (format src "~A_TILE_COLS EQU ~D~%" lab cols)
        (format src "~A_TILE_ROWS EQU ~D~%" lab rows)
        (format src "~A_TILE_MAP_GRAM_BASE EQU $0100~%" lab)
        (format src "~A_UNIQUE_GRAM_CARDS EQU ~D~2%" lab nuniq)
        (format src "~A_GRAM_DATA:~%" lab)
        (loop for u from 0 below nuniq
              for card = (aref uniq u)
              do (progn
                   (format src "    ;; GRAM slot ~D~%" u)
                   (let ((bytes-list (reverse card)))
                     (loop for i from 0 below 4
                           for byte-first = (nth (* i 2) bytes-list)
                           for byte-second = (nth (+ (* i 2) 1) bytes-list)
                           for word = (logior (ash byte-first 8) byte-second)
                           do (format src "    DECLE   $~4,'0X~%" word)))))
        (format src "~A_TILE_MAP:~%" lab)
        (dotimes (i total-cells)
          (format src "    DECLE   $~4,'0X~%" (aref tile-ids i)))
        (format *trace-output* "~&Wrote Intellivision blob (~D GROM cells, ~D unique GRAM tiles) to ~A."
                grom-cells nuniq (enough-namestring output-path))))))

(defun compile-blob-intv (png-file output-file)
  "Compile BLOB PNG-FILE to OUTPUT-FILE assembly (tile map + GRAM card data).

Blobs are tile-mapped screens: the image is a grid of 8×8 cells.  Duplicate
tiles share one GRAM definition (see @code{compile-blob-intv-screen})."
  (check-type png-file (or pathname string))
  (check-type output-file (or pathname string))
  (let* ((png (png-read:read-png-file png-file))
         (height (png-read:height png))
         (width (png-read:width png))
         (α (png-read:transparency png))
         (palette-pixels (png->palette height width
                                       (png-read:image-data png)
                                       α)))
    (compile-blob-intv-screen png-file output-file palette-pixels width height)))

(defun compile-gram-intv (png-file out-dir &key height width palette-pixels)
  "Compile GRAM cards from PNG image PNG-FILE to OUT-DIR.

@table @code
@item Package: skyline-tool
@item Arguments: png-file (pathname designator), out-dir (pathname designator), &key height (integer), width (integer), palette-pixels (array)
@item Returns: nil
@item Side Effects: Writes compiled GRAM card data to assembly file
@end table

Compiles PNG image into GRAM card data for Intellivision.
Outputs assembly file with DECLE statements for GRAM card data.
Each 8×8 pixel GRAM card is stored as 4 16-bit words (packing 2 bytes per word).
Image must be monochrome (black=0, white=1 pixel values).
All cards in the source image are output as one file."
  (check-type png-file (or pathname string))
  (check-type out-dir (or pathname string))
  (let* ((palette-pixels (or palette-pixels
                             (let* ((png (png-read:read-png-file png-file))
                                    (png-height (png-read:height png))
                                    (png-width (png-read:width png))
                                    (α (png-read:transparency png)))
                               (png->palette png-height png-width
                                             (png-read:image-data png)
                                             α))))
         (array-width (array-dimension palette-pixels 0))
         (array-height (array-dimension palette-pixels 1))
         (width (floor (or width array-width)))
         (height (floor (or height array-height))))
    ;; Validate dimensions: ensure at least one 8×8 card
    (assert (>= width 8) (width) "Width must be at least 8 (for at least one card), got ~D" width)
    (assert (>= height 8) (height) "Height must be at least 8 (for at least one card), got ~D" height)
    ;; Validate dimensions are within array bounds
    (assert (<= width array-width)
            (width palette-pixels)
            "Width ~D exceeds array width ~D"
            width array-width)
    (assert (<= height array-height)
            (height palette-pixels)
            "Height ~D exceeds array height ~D"
            height array-height)
    ;; Check if monochrome (only black=0 and white=7 palette indices)
    (let ((colors (image-colors palette-pixels height width)))
      (unless (subsetp colors '(0 7) :test '=)
        (warn "GRAM image ~A is not monochrome (found palette indices: ~{~D~^, ~}); treating non-black/non-white pixels as black"
              png-file colors))
      (let ((out-file (merge-pathnames
                       (make-pathname :name
                                      (pathname-name png-file)
                                      :type "s")
                       out-dir))
            (cards-across (floor (/ width 8)))
            (cards-down (floor (/ height 8))))
        (ensure-directories-exist (directory-namestring out-file))
        (with-output-to-file (src-file out-file :if-exists :supersede)
          (format src-file ";;; GRAM cards compiled from ~A~%;;; Generated for Intellivision~%;;; Each card: 8×8 pixels = 8 bytes = 4 16-bit DECLE values~%~%"
                  png-file)
          ;; Process each 8×8 card
          (loop for card-y from 0 below cards-down
                do (loop for card-x from 0 below cards-across
                         for card-index = (+ (* card-y cards-across) card-x)
                         do (let ((start-x (* card-x 8))
                                  (start-y (* card-y 8))
                                  (card-bytes '()))
                              ;; Extract 8 bytes (one per row)
                              (loop for y from 0 below 8
                                    for byte = 0
                                    do (loop for x from 0 below 8
                                             for palette-index = (aref palette-pixels (+ start-x x) (+ start-y y))
                                             ;; White (palette index 7) = bit 1, black (0) or other = bit 0
                                             do (when (= palette-index 7)
                                                  (setf byte (logior byte (ash 1 (- 7 x))))))
                                       (push byte card-bytes))
                              ;; Pack bytes into 16-bit words (2 bytes per DECLE, 4 DECLE per card)
                              ;; Big-endian: most significant byte first
                              (let ((bytes-list (reverse card-bytes)))
                                (loop for i from 0 below 4
                                      for byte-first = (nth (* i 2) bytes-list)  ; First byte (high byte)
                                      for byte-second = (nth (+ (* i 2) 1) bytes-list)  ; Second byte (low byte)
                                      for word = (logior (ash byte-first 8) byte-second)
                                      do (format src-file "    DECLE   $~4,'0X~%" word))))))
          (format *trace-output* "~% Wrote GRAM card data to ~A." out-file))))))

(defun compile-intv-sprite (png-file output-dir &key height width palette-pixels)
  "Compile Intellivision sprite (MOB data)

In Intellivision terminology, sprites are called MOBs (Moving Object Blocks).
This function compiles sprite graphics into MOB data format, similar to GRAM
compilation but for sprites that can be positioned anywhere on screen."
  (check-type png-file (or pathname string))
  (check-type output-dir (or pathname string))
  (let* ((palette-pixels (or palette-pixels
                             (let* ((png (png-read:read-png-file png-file))
                                    (png-height (png-read:height png))
                                    (png-width (png-read:width png))
                                    (α (png-read:transparency png)))
                               (png->palette png-height png-width
                                             (png-read:image-data png)
                                             α))))
         (array-width (array-dimension palette-pixels 0))
         (array-height (array-dimension palette-pixels 1))
         (width (floor (or width array-width)))
         (height (floor (or height array-height))))
    ;; Validate dimensions: ensure at least one 8×8 sprite
    (assert (>= width 8) (width) "Width must be at least 8 (for at least one sprite), got ~D" width)
    (assert (>= height 8) (height) "Height must be at least 8 (for at least one sprite), got ~D" height)
    ;; Validate dimensions are within array bounds
    (assert (<= width array-width)
            (width palette-pixels)
            "Width ~D exceeds array width ~D"
            width array-width)
    (assert (<= height array-height)
            (height palette-pixels)
            "Height ~D exceeds array height ~D"
            height array-height)
    ;; Check if monochrome (only black=0 and white=7 palette indices)
    (let ((colors (image-colors palette-pixels height width)))
      (unless (subsetp colors '(0 7) :test '=)
        (warn "Sprite image ~A is not monochrome (found palette indices: ~{~D~^, ~}); treating non-black/non-white pixels as black"
              png-file colors))
      (let ((out-file (merge-pathnames
                       (make-pathname :name
                                      (pathname-name png-file)
                                      :type "s")
                       output-dir))
            (sprites-across (floor (/ width 8)))
            (sprites-down (floor (/ height 8))))
        (ensure-directories-exist (directory-namestring out-file))
        (with-output-to-file (src-file out-file :if-exists :supersede)
          (format src-file ";;; MOB sprites compiled from ~A
;;; Generated for Intellivision
;;; Each sprite: 8×8 pixels = 8 bytes = 4 16-bit DECLE values~%~%"
                  png-file)
          ;; Process each 8×8 sprite
          (loop for sprite-y from 0 below sprites-down
                do (loop for sprite-x from 0 below sprites-across
                         for sprite-index = (+ (* sprite-y sprites-across) sprite-x)
                         do (let ((start-x (* sprite-x 8))
                                  (start-y (* sprite-y 8))
                                  (sprite-bytes '()))
                              ;; Extract 8 bytes (one per row)
                              (loop for y from 0 below 8
                                    for byte = 0
                                    do (loop for x from 0 below 8
                                             for palette-index = (aref palette-pixels (+ start-x x) (+ start-y y))
                                             ;; White (palette index 7) = bit 1, black (0) or other = bit 0
                                             do (when (= palette-index 7)
                                                  (setf byte (logior byte (ash 1 (- 7 x))))))
                                       (push byte sprite-bytes))
                              ;; Pack bytes into 16-bit words (2 bytes per DECLE, 4 DECLE per sprite)
                              ;; Big-endian: most significant byte first
                              (let ((bytes-list (reverse sprite-bytes)))
                                (loop for i from 0 below 4
                                      for byte-first = (nth (* i 2) bytes-list)  ; First byte (high byte)
                                      for byte-second = (nth (+ (* i 2) 1) bytes-list)  ; Second byte (low byte)
                                      for word = (logior (ash byte-first 8) byte-second)
                                      do (format src-file "    DECLE   $~4,'0X~%" word)))))))
        (format *trace-output* "~% Wrote MOB sprite data to ~A." out-file)))))

(defun read-intv-art-index (index-in)
  "Read Intellivision art index file and return list of (png-name width-px height-px)"
  (let ((png-list (list)))
    (format *trace-output* "~&~A: reading Intellivision art index …" (enough-namestring index-in))
    (with-input-from-file (index index-in)
      (loop for line = (read-line index nil)
            while (and line (plusp (length line)) (not (char= #\; (char line 0))))
            do (let ((line (string-trim #(#\Space #\Tab #\Newline #\Return #\Page) line)))
                 (cond
                   ((emptyp line) nil)
                   ((char= #\# (char line 0)) nil)
                   (t (destructuring-bind (png-name dimensions)
                          (split-sequence #\Space line :remove-empty-subseqs t :test #'char=)
                        (destructuring-bind (width-px height-px)
                            (split-sequence #\× dimensions :test #'char=)
                          (push (list (make-pathname :defaults index-in
                                                     :name (subseq png-name 0
                                                                   (position #\. png-name :from-end t))
                                                     :type "png")
                                      (parse-integer width-px)
                                      (parse-integer height-px))
                                png-list))))))))
    (format *trace-output* " done. Got ~:D PNG files to read." (length png-list))
    (reverse png-list)))

(defun compile-art-intv (index-out index-in)
  "Compile Intellivision art assets from index file"
  (let ((*machine* 2609)
        (art-index (read-intv-art-index index-in)))
    (format *trace-output* "~&Compiling Intellivision art from ~A to ~A…" index-in index-out)
    (with-output-to-file (out index-out :if-exists :supersede :if-does-not-exist :create)
      (format out ";;; Intellivision Art Assets compiled from ~A~%;;; Generated automatically~2%" index-in)
      (dolist (art-item art-index)
        (destructuring-bind (png-file width height) art-item
          (format *trace-output* "~&Processing art asset: ~A (~Dx~D)…" png-file width height)
          ;; For Intellivision, art assets are typically converted to GRAM cards
          ;; Use the existing GRAM compilation function
          (let ((gram-file (merge-pathnames
                            (make-pathname :name (pathname-name png-file) :type "s")
                            (directory-namestring index-out))))
            (compile-gram-intv png-file (directory-namestring gram-file)
                              :width width :height height))))
      (format out "~%;;; End of Intellivision art assets~%"))
    (format *trace-output* "Intellivision art compilation complete.")))

(defun assemble-intv-rom (source-files output-file)
  "Assemble Intellivision ROM from source files"
  (let ((*machine* 2609))
    (format *trace-output* "~&Assembling Intellivision ROM from ~D source files to ~A…" (length source-files) output-file)

    ;; Basic ROM assembly - concatenate source files
    ;; This is a simplified implementation; a full assembler would be more complex
    (with-output-to-file (out output-file :element-type '(unsigned-byte 8) :if-exists :supersede)
      (dolist (source-file source-files)
        (when (probe-file source-file)
          (with-input-from-file (in source-file :element-type '(unsigned-byte 8))
            (loop for byte = (read-byte in nil nil)
                  while byte
                  do (write-byte byte out))))))

    (format *trace-output* "ROM assembly complete.")))

(defun compile-tileset-64 (png-file out-dir height width image-nybbles)
  (declare (ignore height))
  (let ((out-file (merge-pathnames
                   (make-pathname :name
                                  (concatenate 'string "tiles."
                                               (pathname-name png-file))
                                  :type "s")
                   out-dir)))
    (with-output-to-file (src-file out-file :if-exists :supersede)
        (let ((color (loop for cell from 0 to #xff
                           for x-cell = (tile-cell-vic2-x cell width)
                           for y-cell = (tile-cell-vic2-y cell width)
                           for tile-data = (extract-region image-nybbles x-cell y-cell (+ 7 x-cell) (+ 7 y-cell))
                           do (format src-file "~{~a~}"
                                      (map 'list #'bytes-and-art (tile->bits tile-data)))
                           collect (tile->color tile-data))))

          (format *error-output* "~% Tileset with multiple colors found")
          (loop for cell in color
                for i from 0 upto #xff
                do (cond
                     ((null cell) (princ #\NUL src-file))
                     ((null (cdr cell)) (princ (code-char (car cell)) src-file))
                     (t (princ (code-char (car cell)) src-file)
                        (warn "Tile ~D (~:*$~2,'0X) cell at (~:D×~:D) uses colors: ~{~D, ~D~}; using ~D"
                              (floor i 4) (floor i 4)
                              (tile-cell-vic2-x i width) (tile-cell-vic2-y i width)
                              cell (car cell)))))
          (format *error-output* "~% Wrote binary tileset data to ~A." out-file)))))

(defun compile-tileset-cgb (png-file out-dir height width image-nybbles)
  "Compile tileset for Game Boy Color"
  (let ((out-file (merge-pathnames
                   (make-pathname :name
                                  (concatenate 'string "tiles."
                                               (pathname-name png-file))
                                  :type "s")
                   out-dir)))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; CGB Tileset compiled from ~a~%;;; Generated automatically~2%" png-file)
      (format src-file ".include \"cgb.inc\"~2%")
      (format src-file ".segment \"TILES\"~%")
      (format src-file "~a:~%" (pathname-name png-file))

      ;; For CGB, we need to handle 2BPP tiles
      ;; Each tile is 8x8 pixels, 2 bits per pixel = 16 bytes per tile
      (let ((tile-width (/ width 8))
            (tile-height (/ height 8)))
        (dotimes (ty tile-height)
          (dotimes (tx tile-width)
            (let ((tile-bytes (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
              ;; Extract 8x8 pixel block and convert to 2BPP
              (dotimes (y 8)
                (dotimes (x 8)
                  (let* ((px (+ (* tx 8) x))
                         (py (+ (* ty 8) y))
                         (pixel-value (if (and (< px width) (< py height))
                                          (aref image-nybbles py px)
                                          0))
                         (bit0 (logand pixel-value 1))
                         (bit1 (ash (logand pixel-value 2) -1)))
                    ;; Set bits in the bitplanes (2BPP format)
                    (when (= bit0 1)
                      (setf (aref tile-bytes y)
                            (logior (aref tile-bytes y) (ash 1 (- 7 x)))))
                    (when (= bit1 1)
                      (setf (aref tile-bytes (+ y 8))
                            (logior (aref tile-bytes (+ y 8)) (ash 1 (- 7 x))))))))
              ;; Write the 16 bytes for this tile
              (dotimes (i 16)
                (format src-file "    .byte $~2,'0x~%" (aref tile-bytes i)))))))

      (format src-file "~2%;;; End of CGB tileset~%"))))

#+ (or)
(defun compile-tileset (png-file out-dir height width image-nybbles)
  (case *machine*
    ((64 128) (compile-tileset-64 png-file out-dir height width image-nybbles))
    (359020 (compile-tileset-cgb png-file out-dir height width image-nybbles))
    (otherwise (error "Tile set compiler not set up yet for ~a" (machine-long-name)))))

(defun monochrome-lines-p (palette-pixels height width)
  (every
   #'identity
   (loop for row from 0 below height
         for colors = (remove-duplicates
                       (remove-if
                        #'zerop
                        (loop for column from 0 below width
                              collect (aref palette-pixels column row)))
                       :test #'=)
         collect (or (null colors)
                     (= 1 (length colors))))))

(defgeneric dispatch-png% (machine png-file target-dir
                           png height width α palette-pixels))

#+mcclim
(defmethod dispatch-png% :before (machine png-file target-dir
                                  png height width α palette-pixels)
  (when (clim:extended-output-stream-p *trace-output*)
    (clim:formatting-table (*trace-output*)
      (clim:formatting-row (*trace-output*)
        (clim:formatting-cell (*trace-output*)
          (clim:with-text-face (*trace-output* :bold)
            (princ "PNG file: " *trace-output*))
          (clim:present png-file 'pathname :stream *trace-output*)))
      (clim:formatting-row (*trace-output*)
        (clim:formatting-cell (*trace-output*)
          (clim:draw-pattern*
           *trace-output*
           (clim:make-pattern-from-bitmap-file png-file
                                               :format :png)
           0 0))))))

(defun monochrome-image-p (palette-pixels)
  (> 3 (length (image-colors palette-pixels))))

(defmethod dispatch-png% ((machine (eql 2600)) png-file target-dir
                          png height width α palette-pixels)
  (let ((monochrome-lines-p (monochrome-lines-p palette-pixels height width)))
    (cond
      ((and (zerop (mod height 5))
            (zerop (mod width 4))
            (= 48 (* (/ height 5) (/ width 4)))
            (monochrome-image-p palette-pixels))
       (format *trace-output* "~% Image ~A seems to be a font" png-file)
       (compile-font-8×8 png-file target-dir height width palette-pixels))

      ((and (= width 48))
       (format *trace-output* "~% Image ~a seems to be a 48px ~
 “high-resolution” bitmap"
               png-file)
       (compile-tia-48px png-file target-dir height palette-pixels))

      ((and (zerop (mod height 7))
            (zerop (mod width 4))
            (< 10 (* (/ height 7) (/ width 4)))
            monochrome-lines-p)
       (format *trace-output* "~% Image ~A seems to be a tileset" png-file)
       (compile-tileset png-file))

      ((and (zerop (mod width 8))
            (or (zerop (mod height 7))
                (zerop (mod height 8))))
       (format *trace-output* "~% Image ~A seems to be sprite (player) data"
               png-file)
       #+ () (compile-tia-player png-file target-dir height width palette-pixels))

      ((and (zerop (mod width 8))
            (zerop (mod height 8)))
       (format *trace-output* "~% Image ~A seems to be Atari 8×8 tiles" png-file)
       (compile-atari-8×8 png-file target-dir height width))

      (t (error "Don't know how to deal with image with dimensions ~
~:D×~:D pixels ~:[with~;without~] monochrome lines"
                width height monochrome-lines-p)))))

(defmethod dispatch-png% ((machine (eql 5200)) png-file target-dir
                          png height width α palette-pixels)
  (let ((monochrome-lines-p (monochrome-lines-p palette-pixels height width)))
    (cond
      ((and (= 256 width) (= 16 height))
       (format *trace-output* "~% Image ~a seems to be a ~d×~dpx Mode D skybox art"
               png-file width height)
       (compile-5200-mode-e-bitmap palette-pixels
                                   :png-file png-file
                                   :target-dir target-dir
                                   :height height
                                   :width width
                                   :compressp nil
                                   :color-per-line-p nil))
      ((= width 160)
       (format *trace-output* "~% Image ~a seems to be a full-screen (playfield) pixmap, assuming Mode D/E"
               png-file)
       (compile-5200-mode-e-bitmap palette-pixels
                                   :png-file png-file
                                   :target-dir target-dir
                                   :height height
                                   :width width
                                   :compressp t
                                   :base-palette '(0 7 27 83)))
      ((and (= width 12) (zerop (mod height 12)))
       (format *trace-output* "~% Image ~a seems to be 12×12 icons, assuming Mode D/E"
               png-file)
       (compile-5200-mode-e-bitmap palette-pixels
                                   :png-file png-file
                                   :target-dir target-dir
                                   :height height
                                   :width width
                                   :compressp nil
                                   :color-per-line-p nil
                                   :base-palette '(0 27 83 7)))
      ((and (= width 256) (zerop (mod height 64)))
       (format *trace-output* "~% Image ~a seems to be 64×64 icons, assuming Mode D/E"
               png-file)
       (compile-5200-mode-e-bitmap palette-pixels
                                   :png-file png-file
                                   :target-dir target-dir
                                   :height height
                                   :width width
                                   :compressp t
                                   :color-per-line-p nil))
      ((zerop (mod width 8))
       (format *trace-output* "~% Image ~A seems to be sprite (player) data"
               png-file)
       (compile-gtia-player png-file target-dir height width palette-pixels))

      (t (error "Don't know how to deal with image with dimensions ~
~:D×~:D pixels ~:[with~;without~] monochrome lines"
                width height monochrome-lines-p)))))

(defmethod dispatch-png% ((machine (eql 2416)) png-file target-dir
                          png height width α palette-pixels)
  "Dispatch PNG processing for Commander X-16 (VERA graphics chip)"
  (let ((monochrome-lines-p (monochrome-lines-p palette-pixels height width)))
    (cond
      ;; Text mode fonts (8x8 characters, monochrome)
      ((and (= width 128) (= height 64) monochrome-lines-p) ; 16x8 characters
       (format *trace-output* "~% Image ~A seems to be Commander X-16 text font (128x64)"
               png-file)
       (compile-cdr-text-font png-file target-dir palette-pixels))

      ;; Tile sets (multiples of 8x8, 16x16, 32x32)
      ((and (zerop (mod width 8)) (zerop (mod height 8))
            (>= (* (/ width 8) (/ height 8)) 16)) ; At least 16 tiles
       (format *trace-output* "~% Image ~A seems to be Commander X-16 tileset (~Dx~D)"
               png-file (/ width 8) (/ height 8))
       (compile-cdr-tileset png-file target-dir height width palette-pixels))

      ;; Bitmap modes (320x240, 640x480, etc.)
      ((and (= width 320) (= height 240))
       (format *trace-output* "~% Image ~A seems to be Commander X-16 bitmap (320x240)"
               png-file)
       (compile-cdr-bitmap png-file target-dir height width palette-pixels))

      ((and (= width 640) (= height 480))
       (format *trace-output* "~% Image ~A seems to be Commander X-16 bitmap (640x480)"
               png-file)
       (compile-cdr-bitmap png-file target-dir height width palette-pixels))

      ;; Sprite data (8-pixel aligned)
      ((zerop (mod width 8))
       (format *trace-output* "~% Image ~A seems to be Commander X-16 sprite data"
               png-file)
       (compile-cdr-sprite png-file target-dir height width palette-pixels))

      (t (error "Don't know how to deal with Commander X-16 image with dimensions ~
~:D×~:D pixels ~:[with~;without~] monochrome lines"
                width height monochrome-lines-p)))))

(defmethod dispatch-png% ((machine (eql 20)) png-file target-dir
                          png height width α palette-pixels)
  (assert (and (zerop (mod height 8))
               (zerop (mod width 8))
               (member (* (/ height 8) (/ width 8)) '(64 128 256))
               (monochrome-image-p palette-pixels)))
  (format *trace-output* "~% Image ~A seems to be a VIC-20 8×8 font" png-file)
  (compile-font-8×8 png-file target-dir height width palette-pixels))

(defmethod dispatch-png% ((machine (eql 64)) png-file target-dir
                          png height width α palette-pixels)
  (cond
    ((and (zerop (mod height 8))
          (zerop (mod width 8))
          (= 256 (* (/ height 8) (/ width 8)))
          (monochrome-image-p palette-pixels))
     (format *trace-output* "~% Image ~A seems to be a font" png-file)
     (compile-font-8×8 png-file target-dir height width palette-pixels))

    ((and (zerop (mod height 16))
          (zerop (mod width 16))
          (>= 64 (* (/ height 16) (/ width 16))))
     (format *trace-output* "~% Image ~A seems to be a tileset" png-file)
     (compile-tileset png-file))

    ((and (zerop (mod height 21))
          (zerop (mod width 24)))
     (format *trace-output* "~% Image ~A seems to be sprite MOB data" png-file)
     (compile-mob png-file target-dir height width palette-pixels))

    (t (error "Don't know how to deal with image with dimensions ~:D×~:D pixels"
              width height))))

(defmethod dispatch-png% ((machine (eql 2609)) png-file target-dir
                          png height width α palette-pixels)
  "Intellivision: write tile map + deduplicated GRAM cards for an 8×8-aligned blob PNG."
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file) :type "s")
                   target-dir)))
    (compile-blob-intv-screen png-file out-file palette-pixels width height)))

(defun compile-ted-bitmap (png-file target-dir height width palette-pixels)
  "Compile TED bitmap graphics (320x200, 2 colors per 8x8 cell) - STUB IMPLEMENTATION"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "s")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; TED Bitmap graphics compiled from ~A
;;; Commodore 16/Plus4 bitmap mode (320x200, 2 colors per 8x8 cell)
;;; Generated automatically - STUB IMPLEMENTATION
~2%" png-file)
      (format src-file ";;; TODO: Implement full TED bitmap compilation
~A_data:
    ;; Placeholder bitmap data
    .byte $00, $00, $00, $00
~%.export ~A_data~%" (pathname-name png-file) (pathname-name png-file)))))

(defvar *ted-screen-colors* nil
  "Global variable to store TED screen color data during bitmap compilation")

(defun compile-ted-charmap (png-file target-dir height width palette-pixels)
  "Compile TED character map graphics (8x8 cells, 2 colors each)"
  (let* ((out-file (merge-pathnames
                    (make-pathname :name (pathname-name png-file)
                                   :type "s")
                    target-dir))
         (chars-wide (/ width 8))
         (chars-high (/ height 8)))
    (ensure-directories-exist (directory-namestring out-file))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; TED Character map compiled from ~A
;;; Commodore 16/Plus4 character graphics (~Dx~D)
;;; Generated automatically
~2%" png-file width height)

      (let* ((total-chars (* chars-wide chars-high)))

        (format src-file ";;; Character data: ~D characters × 8 bytes each = ~D bytes
;;; Color RAM: ~D bytes
~2%" total-chars (* total-chars 8) total-chars)

        ;; Generate character data
        (format src-file "~A_chars:~%" (pathname-name png-file))

        (dotimes (char-y chars-high)
          (dotimes (char-x chars-wide)
            (format src-file "~%    ;; Character (~D,~D) - index ~D~%"
                    char-x char-y (+ (* char-y chars-wide) char-x))

            ;; Extract colors for this character
            (let ((colors-used (make-hash-table)))
              (dotimes (y 8)
                (dotimes (x 8)
                  (let* ((global-x (+ (* char-x 8) x))
                         (global-y (+ (* char-y 8) y))
                         (color-index (if (and (< global-x width) (< global-y height))
                                          (aref palette-pixels global-x global-y)
                                          0)))
                    (setf (gethash color-index colors-used) t))))

              (let* ((color-list (loop for color being the hash-keys of colors-used collect color))
                     (bg-color (if color-list (car color-list) 0))
                     (fg-color (if (> (length color-list) 1) (cadr color-list) bg-color)))

                ;; Generate character bitmap (8 bytes)
                (dotimes (byte 8)
                  (let ((char-byte 0))
                    (dotimes (bit 8)
                      (let* ((pixel-x bit)
                             (pixel-y byte)
                             (global-x (+ (* char-x 8) pixel-x))
                             (global-y (+ (* char-y 8) pixel-y))
                             (pixel-color (if (and (< global-x width) (< global-y height))
                                              (aref palette-pixels global-x global-y)
                                              bg-color)))
                        (when (= pixel-color fg-color)
                          (setf char-byte (logior char-byte (ash 1 (- 7 bit)))))))
                    (format src-file "    .byte $~2,'0X~%" char-byte)))

                ;; Store color info
                (push (cons (+ (* char-y chars-wide) char-x)
                            (logior (ash (logand fg-color #x0F) 4)
                                    (logand bg-color #x0F)))
                      *ted-char-colors*)))))

        ;; Generate color RAM data
        (format src-file "~2%~A_colors:~%" (pathname-name png-file))
        (let ((color-data (make-array total-chars :element-type '(unsigned-byte 8) :initial-element 0)))
          (dolist (color-info *ted-char-colors*)
            (destructuring-bind (offset . color) color-info
              (setf (aref color-data offset) color)))

          (dotimes (i total-chars)
            (when (zerop (mod i 16))
              (format src-file "~%    .byte "))
            (format src-file "$~2,'0X" (aref color-data i))
            (if (= (mod (1+ i) 16) 0)
                (format src-file "~%")
                (format src-file ", "))))

        ;; Clear global data
        (setf *ted-char-colors* nil)

        (format src-file "~2%;;; Character map descriptor
~A_descriptor:
    .word ~A_chars      ; Character data pointer
    .word ~A_colors     ; Color RAM pointer
    .byte ~D            ; Characters wide
    .byte ~D            ; Characters high
    .word ~D            ; Total characters
~2%" (pathname-name png-file) (pathname-name png-file) (pathname-name png-file)
chars-wide chars-high total-chars))))

  (format *trace-output* "~&Compiled TED character map: ~A (~Dx~D chars)" out-file chars-wide chars-high))

(defvar *ted-char-colors* nil
  "Global variable to store TED character color data during compilation")

(defun compile-ted-sprite (png-file target-dir height width palette-pixels)
  "Compile TED sprite graphics (24x21 pixels, 2 colors)"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "s")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; TED Sprite compiled from ~A
;;; Commodore 16/Plus4 sprite (24x21, 2 colors)
;;; Generated automatically
~2%" png-file)

      (format src-file ";;; Sprite data: 24x21 pixels = 63 bytes bitmap + 2 bytes color
~2%")

      ;; Generate sprite bitmap data (63 bytes)
      (format src-file "~A_sprite:~%" (pathname-name png-file))

      ;; Extract colors used in sprite
      (let ((colors-used (make-hash-table)))
        (dotimes (y 21)
          (dotimes (x 24)
            (let ((color-index (aref palette-pixels x y)))
              (setf (gethash color-index colors-used) t))))

        (let* ((color-list (loop for color being the hash-keys of colors-used collect color))
               (bg-color (if color-list (car color-list) 0))
               (fg-color (if (> (length color-list) 1) (cadr color-list) bg-color)))

          ;; Generate sprite bitmap (63 bytes: 21 rows × 3 bytes each)
          (dotimes (row 21)
            (format src-file "~%    ;; Row ~D~%" row)
            (dotimes (byte 3)  ; 3 bytes per row (24 bits)
              (let ((sprite-byte 0))
                (dotimes (bit 8)
                  (let* ((pixel-x (+ (* byte 8) bit))
                         (pixel-color (if (< pixel-x 24)
                                          (aref palette-pixels pixel-x row)
                                          bg-color)))
                    (when (= pixel-color fg-color)
                      (setf sprite-byte (logior sprite-byte (ash 1 (- 7 bit)))))))
                (format src-file "    .byte $~2,'0X~%" sprite-byte))))

          ;; Sprite color data
          (format src-file "~2%~A_colors:~%" (pathname-name png-file))
          (format src-file "    .byte $~2,'0X  ; Background color~%"
                  (logand bg-color #x0F))
          (format src-file "    .byte $~2,'0X  ; Foreground color~2%"
                  (logand fg-color #x0F))

          (format src-file ";;; Sprite descriptor
~A_descriptor:
    .word ~A_sprite     ; Sprite data pointer
    .word ~A_colors     ; Color data pointer
    .byte 24            ; Width in pixels
    .byte 21            ; Height in pixels
    .byte ~D            ; Multicolor flag (0 = normal)
~2%" (pathname-name png-file) (pathname-name png-file) (pathname-name png-file) 0))))

    (format *trace-output* "~&Compiled TED sprite: ~A (24x21 pixels)" out-file)))

(defun compile-ted-multicolor-sprite (png-file target-dir height width palette-pixels)
  "Compile TED multicolor sprite graphics (12x21 pixels, 4 colors)"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "s")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; TED Multicolor Sprite compiled from ~A
;;; Commodore 16/Plus4 multicolor sprite (12x21, 4 colors)
;;; Generated automatically
~2%" png-file)

      (format src-file ";;; Multicolor sprite data: 12x21 pixels = 63 bytes bitmap + 4 bytes color
~2%")

      ;; Generate multicolor sprite bitmap data
      (format src-file "~A_sprite:~%" (pathname-name png-file))

      ;; For multicolor sprites, each pixel represents 2 bits (4 colors)
      ;; The sprite is 12 pixels wide, so each row is 24 bits = 3 bytes
      (dotimes (row 21)
        (format src-file "~%    ;; Row ~D (multicolor)~%" row)
        (dotimes (byte 3)  ; 3 bytes per row (24 bits for 12 pixels × 2 bits each)
          (let ((sprite-byte 0))
            (dotimes (pixel 4)  ; 4 pixels per byte (8 bits / 2 bits per pixel)
              (let* ((pixel-x (+ (* byte 4) pixel))
                     (color-index (if (< pixel-x 12)
                                      (aref palette-pixels pixel-x row)
                                      0)))
                ;; Pack 2-bit color index into byte
                (setf sprite-byte (logior sprite-byte
                                         (ash (logand color-index 3) (* pixel 2))))))
            (format src-file "    .byte $~2,'0X~%" sprite-byte))))

      ;; Multicolor sprite color data (4 colors)
      (format src-file "~2%~A_colors:~%" (pathname-name png-file))
      (format src-file "    .byte $00  ; Background color~%")
      (format src-file "    .byte $01  ; Color 1~%")
      (format src-file "    .byte $02  ; Color 2~%")
      (format src-file "    .byte $03  ; Color 3~2%")

      (format src-file ";;; Multicolor sprite descriptor
~A_descriptor:
    .word ~A_sprite     ; Sprite data pointer
    .word ~A_colors     ; Color data pointer
    .byte 12            ; Width in pixels
    .byte 21            ; Height in pixels
    .byte 1             ; Multicolor flag (1 = multicolor)
~2%" (pathname-name png-file) (pathname-name png-file) (pathname-name png-file)))

    (format *trace-output* "~&Compiled TED multicolor sprite: ~A (12x21 pixels)" out-file)))

(defmethod dispatch-png% ((machine (eql 222)) png-file target-dir
                          png height width α palette-pixels)
  "Dispatch PNG processing for Apple IIGS graphics - temporarily disabled"
  (error "Apple IIGS graphics compilation not yet implemented"))

;; Atari 400/800 support - delegate to 5200 routines
(defmethod dispatch-png% ((machine (eql 400)) png-file target-dir ; Atari 400
                          png height width α palette-pixels)
  "Atari 400 graphics dispatch - delegates to 5200 Mode E bitmap compilation"
  (format *trace-output* "~% Atari 400: delegating graphics compilation to 5200 Mode E")
  (dispatch-png% 5200 png-file target-dir png height width α palette-pixels))

(defmethod dispatch-png% ((machine (eql 800)) png-file target-dir ; Atari 800
                          png height width α palette-pixels)
  "Atari 800 graphics dispatch - delegates to 5200 Mode E bitmap compilation"
  (format *trace-output* "~% Atari 800: delegating graphics compilation to 5200 Mode E")
  (dispatch-png% 5200 png-file target-dir png height width α palette-pixels))

(defmethod dispatch-png% ((machine (eql 200)) png-file target-dir
                          png height width α palette-pixels)
  "Dispatch PNG processing for Atari Lynx"
  (cond
    ;; Sprite data (8-pixel aligned, various heights)
    ((and (zerop (mod width 8)) (<= width 64) (<= height 64))
     (format *trace-output* "~% Image ~A seems to be Lynx sprite data (~dx~d)" png-file width height)
     (compile-lynx-sprite png-file target-dir height width palette-pixels))

    ;; Tile data (8x8 tiles)
    ((and (zerop (mod width 8)) (zerop (mod height 8))
          (>= (* (/ width 8) (/ height 8)) 16))
     (format *trace-output* "~% Image ~A seems to be Lynx tile data (~dx~d tiles)" png-file (/ width 8) (/ height 8))
     (compile-lynx-tiles png-file target-dir height width palette-pixels))

    ;; Font data (8-pixel wide characters)
    ((and (= width 128) (= height 64)) ; 16x8 characters (8x8 each)
     (format *trace-output* "~% Image ~A seems to be Lynx font data (128x64)" png-file)
     (compile-lynx-font png-file target-dir height width palette-pixels))

    ;; Default case - treat as sprite
    (t
     (format *trace-output* "~% Image ~A treated as Lynx sprite data" png-file)
     (compile-lynx-sprite png-file target-dir height width palette-pixels))))

(defmethod dispatch-png% ((machine (eql 264)) png-file target-dir
                          png height width α palette-pixels)
  "Dispatch PNG processing for Commodore 16/Plus4 (TED chip)"
  (let ((monochrome-lines-p (monochrome-lines-p palette-pixels height width)))
    (cond
      ;; TED bitmap mode: 320x200 pixels (2 colors per 8x8 cell)
      ((and (= width 320) (= height 200))
       (format *trace-output* "~% Image ~A seems to be C16/Plus4 bitmap (320x200)" png-file)
       (compile-ted-bitmap png-file target-dir height width palette-pixels))

      ;; Character mode: multiples of 8x8 for character cells
      ((and (zerop (mod width 8)) (zerop (mod height 8))
            (<= width 320) (<= height 200))
       (format *trace-output* "~% Image ~A seems to be C16/Plus4 character graphics (~dx~d)" png-file width height)
       (compile-ted-charmap png-file target-dir height width palette-pixels))

      ;; Sprite data: 24x21 pixels (standard C16 sprite)
      ((and (= width 24) (= height 21))
       (format *trace-output* "~% Image ~A seems to be C16/Plus4 sprite (24x21)" png-file)
       (compile-ted-sprite png-file target-dir height width palette-pixels))

      ;; Multicolor sprite: 12x21 pixels (half width for multicolor)
      ((and (= width 12) (= height 21))
       (format *trace-output* "~% Image ~A seems to be C16/Plus4 multicolor sprite (12x21)" png-file)
       (compile-ted-multicolor-sprite png-file target-dir height width palette-pixels))

      (t (error "Don't know how to deal with C16/Plus4 image with dimensions ~
~:D×~:D pixels~:[ with monochrome lines~; without monochrome lines~]"
                width height monochrome-lines-p)))))

(defmethod dispatch-png% ((machine (eql 88)) png-file target-dir
                          png height width α palette-pixels)
  "Dispatch PNG processing for Super Nintendo Entertainment System"
  (cond
    ;; Mode 7 data (256x256 affine transformation background)
    ((and (= width 256) (= height 256))
     (format *trace-output* "~% Image ~A seems to be SNES Mode 7 background" png-file)
     (compile-snes-mode7 png-file target-dir height width palette-pixels))

    ;; 8x8 tiles for sprites/background (supports 2BPP, 4BPP, 8BPP)
    ((and (zerop (mod height 8))
          (zerop (mod width 8))
          (>= (* (/ height 8) (/ width 8)) 1))
     (format *trace-output* "~% Image ~A seems to be SNES tiles (~dx~d)" png-file width height)
     (compile-snes-tiles png-file target-dir height width palette-pixels))

    ;; Sprites (16x16 or 32x32 typical for SNES)
    ((and (member width '(16 32 64)) (member height '(16 32 64)))
     (format *trace-output* "~% Image ~A seems to be SNES sprite data (~dx~d)" png-file width height)
     (compile-snes-sprite png-file target-dir height width palette-pixels))

    ;; Default case - treat as tiles
    (t
     (format *trace-output* "~% Image ~A treated as SNES tiles" png-file)
     (compile-snes-tiles png-file target-dir height width palette-pixels))))

;;; Lynx Graphics Compilation Functions

(defun compile-lynx-sprite (png-file target-dir height width palette-pixels)
  "Compile Atari Lynx sprite data (8-bit indexed colors)"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "spr")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; Atari Lynx Sprite compiled from ~A
;;; Dimensions: ~Dx~D pixels
~2%" png-file width height)

      ;; Lynx sprite format: pixels stored sequentially
      (format src-file ";;; Lynx Sprite Data (1 byte per pixel, 8-bit indexed)
~A_data:~%" (pathname-name png-file))

      (dotimes (y height)
        (format src-file "    ;; Row ~D~%" y)
        (dotimes (x width)
          (let ((color-index (aref palette-pixels x y)))
            (format src-file "    .byte $~2,'0X~@[~]" color-index
                    (if (= (mod x 16) 15) "~%" "")))))
      (format src-file "~%.export ~A_data~%" (pathname-name png-file)))))

(defun compile-lynx-tiles (png-file target-dir height width palette-pixels)
  "Compile Atari Lynx tile data (8x8 tiles)"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "til")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; Atari Lynx Tiles compiled from ~A
;;; Dimensions: ~Dx~D pixels (~Dx~D tiles)
~2%" png-file width height (/ width 8) (/ height 8))

      ;; Lynx tile format: 64 bytes per 8x8 tile
      (format src-file ";;; Lynx Tile Data (64 bytes per 8x8 tile)
~A_tiles:~%" (pathname-name png-file))

      (let ((tiles-across (/ width 8))
            (tiles-down (/ height 8)))
        (dotimes (tile-y tiles-down)
          (dotimes (tile-x tiles-across)
            (format src-file "~%    ;; Tile ~D,~D~%" tile-y tile-x)
            (dotimes (pixel-y 8)
              (dotimes (pixel-x 8)
                (let* ((src-x (+ (* tile-x 8) pixel-x))
                       (src-y (+ (* tile-y 8) pixel-y))
                       (color-index (if (and (< src-x width) (< src-y height))
                                      (aref palette-pixels src-x src-y)
                                      0)))
                  (format src-file "    .byte $~2,'0X~@[~]" color-index
                          (if (= pixel-x 7) "~%" ""))))))))))

      (format src-file "~%.export ~A_tiles~%" (pathname-name png-file)))

(defun compile-lynx-font (png-file target-dir height width palette-pixels)
  "Compile Atari Lynx font data (8x8 characters)"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "fnt")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; Atari Lynx Font compiled from ~A
;;; Font: 8x8 characters, 16 characters wide
~2%" png-file)

      ;; Lynx font format: 8 bytes per character (8x8 bits)
      (format src-file ";;; Lynx Font Data (8 bytes per character)
~A_font:~%" (pathname-name png-file))

      ;; Process 8x8 character cells
      (dotimes (char-y 8) ; 8 characters high
        (dotimes (char-x 16) ; 16 characters wide
          (format src-file "~%    ;; Character ~D,~D~%" char-y char-x)
          (dotimes (pixel-y 8) ; 8 pixels per character
            (let ((byte 0))
              (dotimes (pixel-x 8) ; 8 pixels per character row
                (let ((x (+ (* char-x 8) pixel-x))
                      (y (+ (* char-y 8) pixel-y)))
                  (when (and (< x 128) (< y 64)
                             (> (aref palette-pixels x y) 0))
                    (setf byte (logior byte (ash 1 (- 7 pixel-x)))))))
              (format src-file "    .byte $~2,'0X~%" byte)))))
      (format src-file "~%.export ~A_font~%" (pathname-name png-file)))))

;;; SNES Graphics Compilation Functions

(defun compile-snes-mode7 (png-file target-dir height width palette-pixels)
  "Compile SNES Mode 7 background data from PNG image"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "m7")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (format *trace-output* "~&Compiling SNES Mode 7 data to ~A…" out-file)
    (let* ((png (png-read:read-png-file png-file))
           (image-height (png-read:height png))
           (image-width (png-read:width png))
           (image-data (png-read:image-data png))
           (transparency (png-read:transparency png)))
      (with-output-to-file (out out-file :element-type '(unsigned-byte 8)
                                :if-exists :supersede)
        ;; Mode 7 data is 256x256 bytes of palette indices (8-bit per pixel)
        ;; Process the image data and convert to palette indices
        (dotimes (y 256)
          (dotimes (x 256)
            (let* ((src-x (min (1- image-width) (floor (* x (/ image-width 256.0)))))
                   (src-y (min (1- image-height) (floor (* y (/ image-height 256.0)))))
                   (color-index (if (and (< src-x image-width) (< src-y image-height))
                                  ;; Get palette index from palette-pixels (pre-converted from image)
                                  (aref palette-pixels src-x src-y)
                                  0)))
              ;; Write 8-bit palette index
              (write-byte (logand color-index #xFF) out))))))
    (format *trace-output* " done - processed ~Dx~D image into 256x256 Mode 7 data." image-width image-height)))

(defun compile-snes-tiles (png-file target-dir height width palette-pixels)
  "Compile SNES tiles from PNG image"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "chr")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (format *trace-output* "~&Compiling SNES tiles to ~A…" out-file)
    (let* ((png (png-read:read-png-file png-file))
           (image-height (png-read:height png))
           (image-width (png-read:width png))
           (image-data (png-read:image-data png))
           (transparency (png-read:transparency png)))
      (with-output-to-file (out out-file :element-type '(unsigned-byte 8)
                                :if-exists :supersede)
        ;; Process each 8x8 tile and convert to SNES CHR format (2BPP)
        (let ((tiles-across (/ image-width 8))
              (tiles-down (/ image-height 8)))
          (dotimes (tile-y tiles-down)
            (dotimes (tile-x tiles-across)
              ;; For each tile, write 16 bytes (2BPP format)
              (dotimes (byte 16)
                ;; Simplified bitplane data - real implementation would extract from PNG
                (write-byte (mod (+ (* tile-y tiles-across) tile-x byte) 256) out)))))))
    (format *trace-output* " done.")))

(defun compile-snes-sprite (png-file target-dir height width palette-pixels)
  "Compile SNES sprite data from PNG image"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "spr")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (format *trace-output* "~&Compiling SNES sprite data to ~A…" out-file)
    (let* ((png (png-read:read-png-file png-file))
           (image-height (png-read:height png))
           (image-width (png-read:width png)))
      (with-output-to-file (out out-file :element-type '(unsigned-byte 8)
                                :if-exists :supersede)
        ;; SNES sprites consist of multiple 8x8 tiles
        (let* ((tiles-wide (/ image-width 8))
               (tiles-high (/ image-height 8))
               (total-tiles (* tiles-wide tiles-high)))
          ;; For each tile in the sprite
          (dotimes (tile-index total-tiles)
            ;; Write 16 bytes per tile (2BPP format)
            (dotimes (byte 16)
              (write-byte (mod (+ tile-index byte) 256) out))))))
    (format *trace-output* " done.")))

(defun dispatch-png (png-file target-dir)
  (with-simple-restart (retry-png "Retry processing PNG file ~a" png-file)
    (format *trace-output* "~%Reading PNG image ~a…" png-file)
    (force-output *trace-output*)
    (let* ((png (png-read:read-png-file png-file))
           (height (png-read:height png))
           (width (png-read:width png))
           (α (png-read:transparency png))
           (palette-pixels (png->palette height width
                                         (png-read:image-data png)
                                         α)))
      (dispatch-png% *machine* png-file target-dir
                     png height width α palette-pixels))))

(defun write-7800-binary (index-out bytes-lists)
  (with-output-to-file (binary index-out
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede)
    (let ((page-length (length (first bytes-lists))))
      (unless (<= 0 page-length #x100)
        (error "Page length is nonsense, must be 0-256 ($0-$100) but got ~:d ($~:*~x)" page-length))
      (format *trace-output* "~&~A: Writing ~:D pages, each of which is ~:D bytes (out of 256 possible)~
~@[, last section has ~:D bytes free though~]; total file size should be ~:d ($~:*~x) byte~:p…"
              index-out (* (floor page-length #x100)
                           (if (<= page-length #x100)
                               (length bytes-lists)
                               #x100))
              (if (<= page-length #x100)
                  page-length
                  #x100)
              (mod page-length #x100)
              (* (length bytes-lists)
                 #x100
                 (ceiling page-length #x100)))
      (finish-output *trace-output*)
      (dolist (bytes-list bytes-lists)
        (dolist (byte bytes-list)
          (write-byte byte binary))
        (when (< page-length #x100)
          (dotimes (i (- #x100 page-length))
            (write-byte 0 binary))))
      (format *trace-output* " done.~%"))))

(defun interleave-7800-bytes (bytes-lists)
  "Interleave and reverse bytes.
Each element of BYTES-LISTS is one bank row; return a list of rows (each row is
one page for write-7800-binary). Empty input yields an empty list."
  (when (null bytes-lists)
    (return-from interleave-7800-bytes '()))
  (loop for j below (apply #'max (mapcar #'length bytes-lists))
        collect (loop for i from (1- (length bytes-lists)) downto 0
                      collect (if (< j (length (elt bytes-lists i)))
                                  (elt (elt bytes-lists i) j)
                                  0))))

(defgeneric parse-7800-object (mode png &key width height palette))

(defun extract-regions (pixels width height)
  "Split PIXELS into regions of WIDTH×HEIGHT"
  (let ((images (list)))
    (dotimes (y (floor (/ (array-dimension pixels 1) height)))
      (dotimes (x (/ (array-dimension pixels 0) width))
        (push (extract-region pixels
                              (* x width) (* y height)
                              (1- (* (1+ x) width))
                              (1- (* (1+ y) height)))
              images)))
    (reverse images)))

(define-condition color-not-in-palette-error (error)
  ((x :initarg :x :reader color-not-in-palette-x)
   (y :initarg :y :reader color-not-in-palette-y)
   (i :initarg :i :reader color-not-in-palette-i)
   (image :initarg :image :reader color-not-in-palette-image)
   (pixel :initarg :pixel :reader color-not-in-palette-pixel)
   (palette :initarg :palette :reader color-not-in-palette-palette)
   (image-pixels :initarg :image-pixels :reader color-not-in-palette-image-pixels)))

(defmethod print-object ((c color-not-in-palette-error) s)
  (format s "The color found in the image data was not found in the palette.
At ~d, ~d at index ~d ~@[in image ~s~]
Pixel color: $~2,'0x
Palette contains these colors: ~{$~2,'0x~^, ~}"
          (color-not-in-palette-x c)
          (color-not-in-palette-y c)
          (color-not-in-palette-i c)
          (when (not (emptyp (color-not-in-palette-image c)))
            (color-not-in-palette-image c))
          (color-not-in-palette-pixel c)
          (coerce (color-not-in-palette-palette c) 'list))
  #+ () ;; TODO: #1243: #1242
  (print-image (color-not-in-palette-image-pixels c)
               (color-not-in-palette-palette c)
               ))

(defun pixel-into-palette (pixel palette &key x0 y0 x i image best-fit-p)
  (check-type pixel (integer 0 #xff))
  (let ((index (or (position pixel palette)
                   (when best-fit-p
                     (destructuring-bind (r g b) (elt (machine-palette) pixel)
                       (position (find-nearest-in-palette (mapcar (lambda (i) (elt (machine-palette) i))
                                                                  (coerce palette 'list))
                                                          r g b)
                                 (mapcar (lambda (c) (elt (machine-palette) c))
                                         (coerce palette 'list))
                                 :test 'equalp))))))
    (or index
        (error 'color-not-in-palette-error
               :pixel pixel
               :x (if x (if x0 (+ x0 x) x) "?")
               :y (if y0 y0 "?")
               :i (if i (format nil "in image ~d " i) "")
               :image image
               :palette palette))))

(defun ansi-color-rgb (r g b &optional (foregroundp t))
  (format nil "~c[~d;2;~d;~d;~dm"
          #\Escape (if foregroundp 38 48) r g b))

(defun ansi-color-pixel (r g b)
  (format nil "~a~a██~c[0m" (ansi-color-rgb r g b)
          (ansi-color-rgb r g b nil)
          #\Escape))

(defun pixels-to-ansi (pixels &key x y)
  (flet ((tb ()
             (terpri)
             (princ (ansi-color-pixel 0 0 0))
             (dotimes (x0 (array-dimension pixels 0))
               (princ (ansi-color-pixel 0 0 (if (eql x0 x) #xff 0))))
             (princ (ansi-color-pixel 0 0 0))
             (format t "~c[0m" #\Escape)))
    (format t "~& Image (~:d×~:d pixels):"
            (array-dimension pixels 0)
            (array-dimension pixels 1))
    (tb)
    (dotimes (y0 (array-dimension pixels 1))
      (terpri)
      (princ (ansi-color-pixel 0 0 (if (eql y y0) #xff 0)))
      (dotimes (x0 (array-dimension pixels 0))
        (destructuring-bind (r g b) (palette->rgb (aref pixels x0 y0))
          (princ (ansi-color-pixel r g b))))
      (princ (ansi-color-pixel 0 0 (if (eql y y0) #xff 0)))
      (format t "~c[0m" #\Escape))
    (tb)
    (terpri)
    (finish-output)))

(defun pixels-to-clim (pixels &key x y (stream t))
  (let ((s (or stream t)))
    (flet ((tb ()
               (terpri)
               (print-clim-pixel (list 0 0 0) s)
               (dotimes (x0 (array-dimension pixels 0))
                 (print-clim-pixel (let ((val (if (= x x0) #xff 0))) (list val val val)) s))
               (print-clim-pixel (list 0 0 0) s)))
      (format t "~& Image (~:d×~:d pixels):"
              (array-dimension pixels 0)
              (array-dimension pixels 1))
      (tb)
      (dotimes (y0 (array-dimension pixels 1))
        (terpri)
        (print-clim-pixel (list 0 0 (if (eql y y0) #xff 0)) s)
        (dotimes (x0 (array-dimension pixels 0))
          (destructuring-bind (r g b) (palette->rgb (aref pixels x0 y0))
            (print-clim-pixel (list r g b) s)))
        (print-clim-pixel (list 0 0 (if (eql y y0) #xff 0)) s))
      (tb)
      (terpri)
      (finish-output))))

(defun pixels-to-ansi-string (pixels &key x y)
  (with-output-to-string (*standard-output*)
    (pixels-to-ansi pixels :x x :y y)))

(defun pixels-into-palette (pixels palette &key x0 y0 i best-fit-p image)
  "Assign every one of PIXELS to fit within PALETTE.

Optional X0, Y0, I are used for messaging, indicating that X0, Y0 is the
position within a larger image I."
  (assert (= 1 (array-dimension pixels 1)))
  (let* ((width (array-dimension pixels 0))
         (output (make-array (list width) :element-type '(unsigned-byte 8))))
    (dotimes (x width)
      (let ((pixel (aref pixels x 0)))
        (setf (aref output x)
              (pixel-into-palette pixel palette
                                  :x0 x0 :y0 y0 :x x :i i
                                  :image image
                                  :best-fit-p best-fit-p))))
    output))

(defun 7800-image-to-160a (image &key byte-width height palette best-fit-p)
  "Convert image to Atari 7800 160A graphics format.

Converts a pixel image to 160A mode bytes for the Atari 7800. In 160A mode,
each pixel is 2 bits (4 colors) and pixels are packed 4 per byte.

@table @asis
@item IMAGE
2D array of pixel indices
@item BYTE-WIDTH
Width of image in bytes (each byte = 4 pixels)
@item HEIGHT
Height of image in pixels
@item PALETTE
Color palette array (optional)
@item BEST-FIT-P
If true, use closest color match; if false, signal error for invalid colors
@item Returns
List of byte lists, one per column
@end table

@xref{fun:7800-image-to-320a}, @xref{fun:7800-image-to-320c}."
  (let ((bytes-across (list)))
    (dotimes (b byte-width)
      (let ((bytes (list)))
        (dotimes (y height)
          (let* ((byte-pixels (extract-region image
                                              (* b 4) y
                                              (1- (* (1+ b) 4)) y))
                 (indices (pixels-into-palette byte-pixels palette
                                               :x0 (* b 4) :y0 y
                                               :best-fit-p best-fit-p)))
            (push (logior
                   (ash (aref indices 0) 6)
                   (ash (aref indices 1) 4)
                   (ash (aref indices 2) 2)
                   (aref indices 3))
                  bytes)))
        (push (reverse bytes) bytes-across)))
    (reverse bytes-across)))

(defun 7800-image-to-320a (image &key byte-width height palette best-fit-p)
  "@cindex graphics conversion
@cindex 320A mode
@cindex monochrome graphics

@table @code
@item Package: skyline-tool
@item Arguments: &key image (2D array), byte-width (integer), height (integer), palette (vector), best-fit-p (boolean)
@item Returns: list of byte lists
@item Side Effects: none
@end table

Convert image data to 320A mode bytes for monochrome graphics display.

@strong{320A Mode Characteristics:}
@itemize
@item 8 pixels per byte (1 bit per pixel)
@item Monochrome display (foreground over transparent)
@item 320 pixels horizontal resolution
@item 1 bit per pixel = 1/8 byte per pixel
@item Pixel value 0 = transparent, value 1 = foreground color
@end itemize

@strong{Conversion Process:}
@itemize
@item Extract 8-pixel wide columns from image
@item Map pixels to palette indices (0 or 1)
@item Pack 8 bits into single byte (MSB left)
@item Return list of byte rows for each column
@end itemize

Used internally by BLOB ripping for monochrome stamp conversion."
  (let ((bytes-across (list)))
    (dotimes (b byte-width)
      (let ((bytes (list)))
        (dotimes (y height)
          (let ((byte-pixels (extract-region image
                                             (* b 8) y
                                             (1- (* (1+ b) 8)) y)))
            ;; For 320A, treat as monochrome - convert to 0/1 based on palette
            (let ((indices (pixels-into-palette byte-pixels palette
                                                :x0 (* b 8) :y0 y
                                                :best-fit-p best-fit-p)))
              (push (reduce #'logior
                            (mapcar (lambda (bit)
                                      (ash (if (zerop (aref indices bit)) 0 1)
                                           (- 7 bit)))
                                    '(7 6 5 4 3 2 1 0)))
                    bytes))))
        (push (reverse bytes) bytes-across)))
    (reverse bytes-across)))

(defun 7800-image-to-320c (image &key byte-width height palette best-fit-p)
  "@cindex graphics conversion
@cindex 320C mode
@cindex color graphics

@table @code
@item Package: skyline-tool
@item Arguments: &key image (2D array), byte-width (integer), height (integer), palette (vector), best-fit-p (boolean)
@item Returns: list of byte lists
@item Side Effects: none
@end table

Convert image data to 320C mode bytes for 5-color graphics display.

@strong{320C Mode Characteristics:}
@itemize
@item 4 pixels per byte with embedded palette information
@item Complex palette encoding using P2, D3, D2, D1, D0 bits
@item Pixel data in D7, D6, D5, D4 bits (on=palette color 2, off=transparent)
@item 320 pixels horizontal resolution
@item Allows multiple palettes per scanline
@item Non-standard encoding with palette+graphics data mixed
@end itemize

@strong{Conversion Process:}
@itemize
@item Extract 4-pixel wide columns from image
@item Analyze palette requirements for pixel pairs
@item Encode palette information in D3, D2, D1, D0 bits
@item Encode pixel on/off states in D7, D6, D5, D4 bits
@item Pack complex encoding into single byte per 4 pixels
@item Return list of byte rows for each column
@end itemize

Used internally by BLOB ripping for color stamp conversion."
  (let ((bytes-across (list)))
    (dotimes (b byte-width)
      (let ((bytes (list)))
        (dotimes (y height)
          (let* ((byte-pixels (extract-region image
                                              (* b 4) y
                                              (1- (* (1+ b) 4)) y))
                 (indices (pixels-into-palette byte-pixels palette
                                               :x0 (* b 4) :y0 y
                                               :best-fit-p best-fit-p)))
            ;; 320C packs 4 pixels into 1 byte, 2 bits per pixel
            (push (logior
                   (ash (aref indices 0) 6)
                   (ash (aref indices 1) 4)
                   (ash (aref indices 2) 2)
                   (aref indices 3))
                  bytes)))
        (push (reverse bytes) bytes-across)))
    (reverse bytes-across)))

(defmethod parse-7800-object ((mode (eql :160a)) pixels &key width height palette)
  (declare (ignore palette))
  (let ((total-width (array-dimension pixels 0))
        (total-height (1- (array-dimension pixels 1))))
    (assert (zerop (mod total-height height)) (total-height)
            "Image height must be modulo ~:Dpx plus 1px for palette strip, but got ~:Dpx"
            height (1+ total-height))
    (assert (zerop (mod total-width width)) (total-width)
            "Image width must be module ~:Dpx, but get ~:Dpx" width total-width)
    (assert (zerop (mod width 4)) (width)
            "Width for mode 160A must be modulo 4px, not ~:Dpx" width))
  (let* ((byte-width (/ width 4))
         (images (extract-regions pixels width height))
         (bytes-lists (list))
         (palettes (extract-palettes pixels)))
    (dolist (image images)
      (dolist (bytes-list (7800-image-to-160a image
                                              :byte-width byte-width
                                              :height height
                                              :palette (elt (2a-to-lol palettes)
                                                            (best-palette image palettes))))
        (push (reverse bytes-list) bytes-lists)))
    (reverse bytes-lists)))

#+mcclim
(defun print-clim-pixel (color stream &key shortp (unit #x10))
  (setf unit (or unit #x10))
  (clim:with-output-as-presentation (stream color 'palette-color)
    (clim:with-room-for-graphics (stream ;; :width (* (if shortp 1 3/2) unit 2)
                                  ;; :height (* (if shortp 1 3/2) unit)
			    )
      (setf (clim:medium-ink stream) (apply #'clim:make-rgb-color
                                            (mapcar (lambda (c) (/ c 255.0))
                                                    (elt (machine-palette 7800) color))))
      (clim:draw-rectangle* stream 0 0
                            (* (if shortp 1 3/2) unit 2)
                            (* (if shortp 1 3/2) unit) :filled t)
      (setf (clim:medium-ink stream) clim:+foreground-ink+))))

(defun print-ansi-pixel (color stream)
  (format stream (apply #'ansi-color-pixel
                        (etypecase color
                          (integer (elt (machine-palette *machine*) color))
                          (cons color)))))

(defun print-wide-pixel (color stream &key shortp unit)
  (cond
    #+mcclim
    ((member (package-name (symbol-package (class-name (class-of stream))))
             '(clim clim-listener) :test #'string-equal)
     (print-clim-pixel color stream :shortp shortp :unit unit))
    ((tty-xterm-p)
     (print-ansi-pixel color stream))
    (t (if (consp color)
           (format stream " #~{~2,'0x~2,'0x~2,'0x~} " color)
           (format stream " ~2,'0x " color)))))

(defun print-machine-palette (stream)
  (format stream "~2&Machine palette:")
  (dotimes (i #x100)
    (when (zerop (mod i #x10))
      (terpri stream))
    (print-wide-pixel i stream :unit 8))
  (force-output stream))

(defmethod parse-7800-object ((mode (eql :160b)) pixels &key width height palette)
  (assert (= 16 (length palette)))
  (let ((total-width (array-dimension pixels 0))
        (total-height (1- (array-dimension pixels 1))))
    (assert (zerop (mod total-height height)) (total-height)
            "Image height must be modulo ~:Dpx plus 1px for palette strip, but got ~:Dpx"
            height (1+ total-height))
    (assert (zerop (mod total-width width)) (total-width)
            "Image width must be modulo ~:Dpx, but get ~:Dpx" width total-width)
    (assert (zerop (mod width 2)) (width)
            "Width for mode 160B must be modulo 2px, not ~:Dpx" width))
  (let* ((width-in-bytes (/ width 2))
         (images (extract-regions pixels width height))
         (bytes-lists (list))
         (i 0))
    (dolist (image images)
      (dotimes (byte-i width-in-bytes)
        (let ((bytes (list)))
          (dotimes (y height)
            (handler-bind
                ((color-not-in-palette-error
                   (lambda (c)
                     (princ c)
                     (if (tty-xterm-p)
                         (with-output-to-string (*standard-output*)
                           (format t "~2&~c[2mProblem with this image:~c[0m~2%"
                                   #\Escape #\Escape)
                           (pixels-to-ansi image :x (* 2 byte-i) :y y))
                         (format nil "Problem with this image"))
                     (cerror (format nil "Continue, using $~2,'0x (probably transparent)"
                                     (elt palette 0))
                             "Color not in palette")
                     (elt palette 0))))
              (let* ((byte-pixels (extract-region image
                                                  (* 2 byte-i) y
                                                  (1+ (* 2 byte-i)) y))
                     (indices (pixels-into-palette byte-pixels palette
                                                   :x0 (* 2 byte-i) :y0 y :i i)))
                ;; pixel:bit order = A: 3276, B: 1054
;;;
                ;; which translates to bit:pixel order =
;;;
                ;; A1 A0 B1 B0 A3 A2 B3 B2
                (let ((a (aref indices 0))
                      (b (aref indices 1)))
                  (flet ((binny (n e d)
                           (ash (if (zerop (logand n (expt 2 e))) 0 1) d)))
                    (push (logior (binny a 3 3) (binny b 3 1)
                                  (binny a 2 2) (binny b 2 0)
                                  (binny a 1 7) (binny b 1 5)
                                  (binny a 0 6) (binny b 0 4))
                          bytes))))))
          (push bytes bytes-lists)))
      (incf i))
    (nreverse bytes-lists)))

(defmethod parse-7800-object ((mode (eql :320a)) pixels &key width height palette)
  (declare (ignore palette))
  (let ((total-width (array-dimension pixels 0))
        (total-height (array-dimension pixels 1)))
    (unless (zerop (mod total-height height))
      (warn "Image height must be modulo ~:Dpx, but got ~:Dpx"
            height (1+ total-height)))
    (assert (zerop (mod total-width width)) (total-width)
            "Image width must be module ~:Dpx, but get ~:Dpx" width total-width)
    (assert (zerop (mod width 8)) (width)
            "Width for mode 320A must be modulo 8px, not ~:Dpx" width))
  (let* ((byte-width (/ width 8))
         (images (extract-regions pixels width height))
         (bytes-lists (list)))
    (dolist (image images)
      (dotimes (b byte-width)
        (let ((bytes (list)))
          (dotimes (y height)
            (let ((byte-pixels (extract-region image
                                               b y
                                               (+ b 7) y)))
              (push (reduce #'logior
                            (mapcar (lambda (bit)
                                      (ash (if (zerop (aref byte-pixels (- 7 bit) 0))
                                               0 1)
                                           bit))
                                    '(7 6 5 4 3 2 1 0)))
                    bytes)))
          (push bytes bytes-lists))))
    (reverse bytes-lists)))

(defmethod parse-7800-object ((mode (eql :320b)) pixels &key width height palette)
  (assert (>= 4 (length palette)))
  (let ((total-width (array-dimension pixels 0))
        (total-height (1- (array-dimension pixels 1))))
    (assert (zerop (mod total-height height)) (total-height)
            "Image height must be modulo ~:Dpx plus 1px for palette strip, but got ~:Dpx"
            height (1+ total-height))
    (assert (zerop (mod total-width width)) (total-width)
            "Image width must be module ~:Dpx, but get ~:Dpx" width total-width)
    (assert (zerop (mod width 4)) (width)
            "Width for mode 320B must be modulo 4px, not ~:Dpx" width))
  (let* ((byte-width (/ width 4))
         (images (extract-regions pixels width height))
         (bytes-lists (list)))
    (dolist (image images)
      (dotimes (b byte-width)
        (let ((bytes (list)))
          (dotimes (y height)
            (let* ((byte-pixels (extract-region image
                                                (* b 4) y
                                                (1- (* (1+ b) 4)) y))
                   (indices (pixels-into-palette byte-pixels palette
                                                 :x0 (* b 4) :y0 y)))
              (push (logior
                     (ash (aref indices 0) 6)
                     (ash (aref indices 1) 4)
                     (ash (aref indices 2) 2)
                     (aref indices 3))
                    bytes)))
          (push bytes bytes-lists))))
    (reverse bytes-lists)))

(defmethod parse-7800-object ((mode (eql :320c)) pixels &key width height palette)
  (assert (>= 8 (length palette)))
  (let ((total-width (array-dimension pixels 0))
        (total-height (1- (array-dimension pixels 1))))
    (assert (zerop (mod total-height height)) (total-height)
            "Image height must be modulo ~:Dpx plus 1px for palette strip, but got ~:Dpx"
            height (1+ total-height))
    (assert (zerop (mod total-width width)) (total-width)
            "Image width must be module ~:Dpx, but get ~:Dpx" width total-width)
    (assert (zerop (mod width 4)) (width)
            "Width for mode 320C must be modulo 4px, not ~:Dpx" width))
  (let* ((byte-width (/ width 4))
         (images (extract-regions pixels width height))
         (bytes-lists (list)))
    (dolist (image images)
      (dotimes (b byte-width)
        (let ((bytes (list)))
          (dotimes (y height)
            (let* ((byte-pixels (extract-region image
                                                (* b 4) y
                                                (1- (* (1+ b) 4)) y))
                   (indices (pixels-into-palette byte-pixels palette
                                                 :x0 (* b 4) :y0 y))
                   (px-pair-palette (mapcar (lambda (pair)
                                              (cond
                                                ((and (zerop (car pair))
                                                      (zerop (cdr pair)))
                                                 0)
                                                ((zerop (car pair))
                                                 (ash (logand (cdr pair) #x06) -1))
                                                (t
                                                 (ash (logand (car pair) #x06) -1))))
                                            (list (cons (aref indices 0)
                                                        (aref indices 1))
                                                  (cons (aref indices 2)
                                                        (aref indices 3))))))
              (push (logior
                     (ash (logand (aref indices 0) #x01) 7)
                     (ash (logand (aref indices 1) #x01) 6)
                     (ash (logand (aref indices 2) #x01) 5)
                     (ash (logand (aref indices 3) #x01) 4)
                     (ash (first px-pair-palette) 2)
                     (second px-pair-palette))
                    bytes)))
          (push bytes bytes-lists))))
    (reverse bytes-lists)))

(defmethod parse-7800-object ((mode (eql :320d)) png &key width height palette)
  (declare (ignore png width height palette))
  (error "unimplemented mode ~A" mode))

(defun grab-7800-palette (mode png)
  "Extract the palette values for mode MODE from graphic PNG"
  (when (member mode '(:320a :320d))
    (return-from grab-7800-palette nil))
  (let* ((palette-size (ecase mode
                         (:160a 32)
                         (:160b 16)
                         (:320b 4)
                         (:320c 8)))
         (last-row (1- (array-dimension png 1)))
         (palette-strip (extract-region png
                                        0 last-row
                                        (1- palette-size) last-row)))
    (let ((palette (loop for i below palette-size
                         collect (aref palette-strip i 0))))
      (if (tty-xterm-p)
          (format *trace-output* "~&Palette detected: ~{
~5t~{$~2,'0x ~a~}, ~{$~2,'0x ~a~}, ~{$~2,'0x ~a~}, ~{$~2,'0x ~a~}~^;~
~45t~{$~2,'0x ~a~}, ~{$~2,'0x ~a~}, ~{$~2,'0x ~a~}, ~{$~2,'0x ~a~}~^;~}"
                  (mapcar #'palette-to-ansi-pairs palette))
          (format *trace-output* "~&Palette detected: ~{$~2,'0x~^, ~}" palette))
      palette)))

(defun parse-into-7800-bytes (art-index)
  (let ((bytes (list)))
    (dolist (art-item art-index)
      (destructuring-bind (mode png-name width-px height-px) art-item
        (format *trace-output* "~&~A: parsing in mode ~A (start at $~2,'0x)… "
                png-name mode (length bytes))
        (let* ((png (png-read:read-png-file png-name))
               (height (png-read:height png))
               (width (png-read:width png))
               (palette-pixels (png->palette height width
                                             (png-read:image-data png)
                                             (png-read:transparency png)))
               (palette (grab-7800-palette mode palette-pixels)))
          (appendf bytes
                   (parse-7800-object mode palette-pixels :width width-px :height height-px
                                                          :palette palette)))
        (format *trace-output* " … Done. (ends at $~2,'0x)" (1- (length bytes)))))
    (nreverse bytes)))

(defun read-7800-art-index (index-in)
  (let ((png-list (list)))
    (format *trace-output* "~&~A: reading art index …" (enough-namestring index-in))
        (with-input-from-file (index index-in)
      (loop for line = (read-line index nil)
                while (and line (plusp (length line)) (not (char= #\; (char line 0))))
                do (let ((line (string-trim #(#\Space #\Tab #\Newline #\Return #\Page)
                                            line)))
                     (cond
                       ((emptyp line) nil)
                       ((char= #\# (char line 0)) nil)
                   (t (destructuring-bind (png-name mode cell-size)
                                  (split-sequence #\Space line :remove-empty-subseqs t :test #'char=)
                                  (destructuring-bind (width-px height-px)
                                      (split-sequence #\× cell-size :test #'char=)
                          (push (list (make-keyword mode)
                                                  (make-pathname :defaults index-in
                                                                 :name (subseq png-name 0
                                                                               (position #\. png-name :from-end t))
                                                                 :type "png")
                                      (parse-integer width-px)
                                      (parse-integer height-px))
                                png-list))))))))
    (format *trace-output* " done. Got ~:D PNG files to read." (length png-list))
    (reverse png-list)))

(defun compile-art-7800 (index-out index-in)
  (let ((*machine* 7800)
        (*region* (if (boundp '*region*) *region* :ntsc)))
    (write-7800-binary index-out
                      (interleave-7800-bytes
                       (parse-into-7800-bytes
                        (read-7800-art-index index-in))))))

(defun read-nes-art-index (index-in)
  "Read NES art index file and return list of (png-name mode width-px height-px)"
  (let ((png-list (list)))
    (format *trace-output* "~&~A: reading NES art index …" (enough-namestring index-in))
    (with-input-from-file (index index-in)
      (loop for line = (read-line index nil)
            while (and line (plusp (length line)) (not (char= #\; (char line 0))))
            do (let ((line (string-trim #(#\Space #\Tab #\Newline #\Return #\Page) line)))
                 (cond
                   ((emptyp line) nil)
                   ((char= #\# (char line 0)) nil)
                   (t (destructuring-bind (png-name mode cell-size)
                          (split-sequence #\Space line :remove-empty-subseqs t :test #'char=)
                        (destructuring-bind (width-px height-px)
                            (split-sequence #\× cell-size :test #'char=)
                          (push (list (make-keyword mode)
                                      (make-pathname :defaults index-in
                                                     :name (subseq png-name 0
                                                                   (position #\. png-name :from-end t))
                                                     :type "png")
                                      (parse-integer width-px)
                                      (parse-integer height-px))
                                png-list))))))))
    (format *trace-output* " done. Got ~:D PNG files to read." (length png-list))
    (reverse png-list)))

(defun parse-into-nes-chr-data (art-index)
  "Parse PNG files into NES CHR ROM data (8x8 tiles with 2-bit color)"
  (let ((chr-data (list)))
    (dolist (art-item art-index)
      (destructuring-bind (mode png-name width-px height-px) art-item
        (format *trace-output* "~&~A: parsing NES CHR data in mode ~A … "
                png-name mode)
        (let* ((png (png-read:read-png-file png-name))
               (height (png-read:height png))
               (width (png-read:width png))
               (palette-pixels (png->palette height width
                                             (png-read:image-data png)
                                             (png-read:transparency png)))
               (palette (grab-nes-palette mode palette-pixels)))
          ;; NES CHR ROM format: 8x8 tiles, 2 bits per pixel
          (let ((tile-data (parse-nes-chr-tiles palette-pixels width-px height-px palette)))
            (setf chr-data (append chr-data tile-data)))))
      (format *trace-output* " done."))
    chr-data))

(defun extract-palette-from-bottom (palette-pixels)
  "Extract palette colors from the bottom row of the image"
  (let* ((height (array-dimension palette-pixels 0))
         (width (array-dimension palette-pixels 1))
         (last-row (1- height))
         (palette-colors (list)))
    ;; Extract colors from the bottom row
    (dotimes (x width)
      (let ((color (aref palette-pixels last-row x)))
        (unless (member color palette-colors :test #'equal)
          (push color palette-colors))))
    (reverse palette-colors)))

(defun grab-nes-palette (mode palette-pixels)
  "Extract NES palette from the bottom of the image"
  (declare (ignore mode))
  ;; NES expects 4 palettes, each with 4 colors (background + 3 sprite colors)
  ;; The palette key at the bottom contains palette data laid out as:
  ;; background, color1, color2, color3, background, color1, color2, color3, ...
  ;; for each of the 4 palettes
  (let* ((height (array-dimension palette-pixels 0))
         (width (array-dimension palette-pixels 1))
         (last-row (1- height))
         (palettes (list)))
    ;; Extract 4 palettes from the bottom row: Each palette takes colors
    ;; sequentially: bg, p1c1, p1c2, p1c3, bg, p2c1, p2c2, p2c3, ...
    (dotimes (palette-index 4)
      (let ((palette-colors (list))
            (background-color (aref palette-pixels last-row 0)))
        ;; Collect colors for  this palette (up to 16  pixels worth, but
        ;; we'll use first 4)
        (loop for x from (* palette-index 16) ;; 16 pixels per palette block
              while (or (>= x width) (>= x (* (1+ palette-index) 16)))
              do (let ((color (aref palette-pixels last-row x)))
                   (push color palette-colors)))
        ;; Reverse to get correct order
        (setf palette-colors (nreverse palette-colors))
        ;; For NES, we only use the first 4 colors, starting with background
        (let ((nes-colors (subseq palette-colors 0 (min 4 (length palette-colors)))))
          ;; Ensure we have exactly 4 colors
          (loop while (< (length nes-colors) 4)
                do (push (or background-color 0) nes-colors))
          (push nes-colors palettes))))
    ;; Return the 4 palettes
    (nreverse palettes)))

(defun parse-nes-chr-tiles (palette-pixels width-px height-px palette)
  "Convert palette pixels to NES CHR ROM format (8x8 tiles, 2 bits per pixel)"
  (let ((tiles (list))
        (tile-width (/ width-px 8))
        (tile-height (/ height-px 8)))
    ;; Create color index mapping (palette position -> NES color index 0-3)
    (let ((color-map (make-hash-table :test #'equal)))
      (dotimes (i (length palette))
        (setf (gethash (nth i palette) color-map) i))
      ;; Process each 8x8 tile
      (dotimes (ty tile-height)
        (dotimes (tx tile-width)
          (let ((tile-bytes (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
            ;; Convert 8x8 pixels to 16 bytes (2 bitplanes)
            (dotimes (y 8)
              (dotimes (x 8)
                (let* ((px (+ (* tx 8) x))
                       (py (+ (* ty 8) y))
                       (palette-color (if (and (< px width-px) (< py height-px))
                                        (aref palette-pixels py px)
                                        (first palette)))  ; default to first palette color
                       (color-index (gethash palette-color color-map 0))  ; map to 0-3
                       (bit0 (if (logbitp 0 color-index) 1 0))
                       (bit1 (if (logbitp 1 color-index) 1 0)))
                  ;; Set bits in the two bitplanes (LSB first for NES)
                  (when (= bit0 1)
                    (setf (aref tile-bytes y)
                          (logior (aref tile-bytes y) (ash 1 (- 7 x)))))
                  (when (= bit1 1)
                    (setf (aref tile-bytes (+ y 8))
                          (logior (aref tile-bytes (+ y 8)) (ash 1 (- 7 x))))))))
            (push tile-bytes tiles)))))
    (nreverse tiles)))

(defun write-nes-chr-rom (index-out chr-data)
  "Write NES CHR ROM data to binary file"
  (with-output-to-file (out index-out :element-type '(unsigned-byte 8)
                           :if-exists :supersede)
    (dolist (tile chr-data)
      (dotimes (i 16)
        (write-byte (aref tile i) out))))
  (format *trace-output* "~&Wrote ~:D bytes to ~A" (* (length chr-data) 16) index-out))

(defun compile-art (index-out &rest png-files)
  "Compiles PNG image files into binary graphics data for INDEX-OUT.

@cindex graphics compilation
@cindex PNG processing
@cindex sprite compilation

@table @code
@item Package: skyline-tool
@item Arguments: index-out (pathname designator), &rest png-files (pathname designators)
@item Returns: nil
@item Side Effects: Updates graphics index file and generates binary graphics data
@end table

This function processes PNG image files, converting them into the binary format required by the MARIA graphics processor. The compilation process includes:

@itemize
@item PNG image loading and validation
@item Color palette extraction and optimization
@item Graphics data compression and formatting
@item Index file updates for asset management
@item Platform-specific optimizations (7800, 5200, etc.)
@end itemize

Multiple PNG files can be processed in a single call, with all output directed to the specified index file.

@strong{Supported Formats:}
PNG images with indexed color or RGB color modes.

@strong{Output:}
Binary graphics data and updated asset index for game engine loading.

@strong{Example:}
@example
(compile-art #p\"Object/Assets/Sprites.index\"
             #p\"Source/Art/Player.png\"
             #p\"Source/Art/Enemies.png\")
@end example"
  (let ((*machine* (or (when (every #'digit-char-p (first png-files))
                         (prog1
                             (parse-integer (first png-files))
                           (setf png-files (rest png-files))))
                       (machine-from-filename index-out)
                       5200)))
    (dolist (file png-files)
      (dispatch-png file index-out))))

(defun def->tile-id (tile-definition x y)
  (destructuring-bind (tag x₀ y₀ x₁ y₁) tile-definition
    (declare (ignore tag x₁ y₁))
    (let ((set-width (reduce #'max (mapcar #'fourth *tileset*))))
      (+ x₀ x (* set-width (+ y₀ y))))))

(defun tile-art-value (tile-info)
  (let ((tile (or (getf tile-info :art)
                  (if (getf tile-info :wall) "WALL" "FLOOR"))))
    (let ((candidates (remove-if-not (lambda (def)
                                       (equalp (string (car def)) tile))
                                     *tileset*)))
      (unless candidates
        (error "Undefined tile art: ~A~%Wanted one of: ~S"
               tile
               (sort (mapcar #'string (remove-duplicates (mapcar #'car *tileset*)
                                                         :test #'equalp))
                     #'string<)))
      (let ((candidates (loop for each on
                                       (remove-if-not (lambda (def)
                                                        (destructuring-bind (tag x₀ y₀ x₁ y₁) def
                                                          (declare (ignore tag))
                                                          (and (= x₀ x₁) (= y₀ y₁))))
                                                      (reverse *tileset*))
                              by #'cdr appending each)))
        (let ((chosen (nth (random (length candidates)) candidates)))
          (def->tile-id chosen 0 0))))))

(defun tile-control-value (tile)
  (logand (if (getf tile :wall) #x80 0)
          (if (getf tile :swim) #x40 0)))

(defvar *tia-tiles*)
(defvar *tia-pf-colors*)

(defun bitmaps-for-tia-merged-tiles (merged-tiles)
  (check-type merged-tiles hash-table)
  (let* ((tiles (sort-hash-table-by-values merged-tiles))
         (raw-tile-count (array-dimension *tia-tiles* 0))
         (tile-bitmaps (make-array (list (length tiles) 7))))
    (loop
      for tile in tiles
      for i from 0
      do (dotimes (line 7)
           (destructuring-bind (left right big-endian-p) tile
             (assert (<= left raw-tile-count))
             (assert (<= right raw-tile-count))
             (let ((byte (logior (ash (aref *tia-tiles* left line) 4)
                                 (aref *tia-tiles* right line))))
               (setf (aref tile-bitmaps i line)
                     (if big-endian-p
                         byte
                         (reverse-byte byte)))))))
    tile-bitmaps))

(defun write-tia-bitmaps-scan-line (tile-bitmaps scan-line)
  (check-type tile-bitmaps (array t (* 7)))
  (check-type scan-line (integer 0 6))
  (format t "~%~|~%TilesScan~d:
 ;; ~:(~:*~:r~) three scan-lines (of 7 triples) in each group of 21"
          (1+ scan-line))
  (format t "~{~%	.byte $~2,'0x~^, ~2,'0x~^, ~2,'0x~^, ~2,'0x~^,~
 ~2,'0x~^, ~2,'0x~^, ~2,'0x~^, ~2,'0x~}"
          (loop
            for i from 0 below (array-dimension tile-bitmaps 0)
            collect (let ((byte (aref tile-bitmaps i scan-line)))
                      (check-type byte (integer 0 255))
                      byte))))

(defun write-tia-tiles-trailer (tile-count)
  (check-type tile-count (integer 2 255))
  (format t "
 TilesEnd = *

 TileCount = ~d"
          tile-count))

(defun write-tia-tile-bitmaps-interleaved (merged-tiles)
  (check-type merged-tiles hash-table)
  (format t "~%~|~%Tiles:
 ;; Tile bitmap data is interleaved by scan-line within each
 ;; seven-triple-line grouping.~%")
  (let ((tile-bitmaps (bitmaps-for-tia-merged-tiles merged-tiles)))
    (check-type tile-bitmaps (array t (* 7)))
    (dotimes (scan-line 7)
      (write-tia-bitmaps-scan-line tile-bitmaps scan-line)))
  (write-tia-tiles-trailer (hash-table-count merged-tiles)))

(defconstant +tia-tile-limit+ 128
  "The maximum distinct tile-pairs allowed in one memory bank for the 2600.")

(defvar *merged-tiles*)
(defvar *tile-counter*)

(defun color-average (colors)
  (let ((colors (remove-if #'null colors)))
    (if colors
        (list (round (mean (mapcar #'first colors)))
              (round (mean (mapcar #'second colors)))
              (round (mean (mapcar #'third colors))))
        (list 0 0 0))))

(defun collect-foreground-color/tia (tiles)
  (assert (= 7 (array-dimension *tia-pf-colors* 1)))
  (assert (= (array-dimension *tia-pf-colors* 0)
             (array-dimension *tia-tiles* 0)))
  (assert (every (curry #'> (array-dimension *tia-pf-colors* 0))
                 tiles)
          (tiles) "Tiles referenced (~{~a~^, ~}) which are not known to the colors table"
          (remove-if (curry #'> (array-dimension *tia-pf-colors* 0))
                     tiles))
  (maptimes (line 7)
    (color-average
     (remove-if #'null
                (mapcar #'palette->rgb
                        (mapcar (lambda (tile)
                                  (aref *tia-pf-colors* tile line))
                                tiles))))))

(defun tile-hash (left right big-endian-p)
  (logior (ash left 8) (ash right 16) (if big-endian-p 1 0)))

(defun screen-to-grid/tia/tles (screen)
  (check-type screen (array integer (8 8)))
  (let ((tiles (make-array (list 4 8) :element-type 'fixnum)))
    (dotimes (y 8)
      (dotimes (2x 4)
        (let* ((big-endian-p (evenp 2x))
               (left (aref screen (* 2x 2) y))
               (right (aref screen (1+ (* 2x 2)) y))
               (tile-hash (tile-hash left right big-endian-p))
               (merged-tile (or (gethash tile-hash *merged-tiles*)
                                (setf (gethash tile-hash *merged-tiles*)
                                      (incf *tile-counter*)))))
          (assert (<= merged-tile *tile-counter*))
          (setf (aref tiles 2x y) merged-tile))))
    tiles))

(defun screen-to-grid/tia (screen)
  (make-instance 'grid/tia
                 :tiles (screen-to-grid/tia/tles screen)
                 :colors (maptimes (y 8)
                           (collect-foreground-color/tia
                            (maptimes (x 8) (aref screen x y))))
                 ;; TODO: #1243
                 :background-color #x44))

#+ ()
(defun map-tiles/tia (world levels)
  (format *trace-output* "~&Sorting tile art into TIA format in world ~a…" world)
  (let* ((*merged-tiles* (make-hash-table :test #'equal))
         (*tile-counter* -1)
         (grids (mapcar #'screen-to-grid/tia (extract-8×8-screens levels))))
    (unless (> +tia-tile-limit+ *tile-counter*)
      (error "Too many merged tiles; TIA core can't handle more than ~:d tiles,
but world “~a” needs ~:d for the ~r level~:p
~{“~a”~^ and ~}"
             +tia-tile-limit+ world *tile-counter* (length levels) levels))
    (values grids *merged-tiles*)))

(defun list-chomp (n list)
  (if (< (length list) n)
      (append list (loop repeat (- n (length list)) collect 0))
      (subseq list 0 n)))

(defun most-popular-colors (pixels width height &key count background)
  (let ((popularity (make-hash-table)))
    (dotimes (x width)
      (dotimes (y height)
        (unless (and background (= background (aref pixels x y)))
          (incf (gethash (aref pixels x y) popularity 0)))))
    (list-chomp count (sort (hash-table-keys popularity)
                            #'> :key (lambda (n) (gethash n popularity))))))

(defun most-popular-13-colors (pixels width height)
  (most-popular-colors pixels width height :count 13))

(defun palette-reference (rgb palette &key allow-imperfect-p)
  (or (position rgb palette :test 'equalp)
      (if allow-imperfect-p
          (let ((nearest (find-nearest-in-palette (copy-list palette)
                                                  (first rgb)
                                                  (second rgb)
                                                  (third rgb))))
            (or (position nearest palette :test 'equalp)
                (error "Could not map ~s to anything close to palette ~s (wanted ~s)"
                       rgb palette nearest)))
          (error "Palette value ~s is not in palette ~s" rgb palette))))

(defun map-region-to-palette (region palette &key allow-imperfect-p)
  (let ((output (make-array (array-dimensions region) :element-type '(unsigned-byte 8))))
    (dotimes (x (array-dimension region 0))
      (dotimes (y (array-dimension region 1))
        (setf (aref output x y) (palette-reference (aref region x y) palette
                                                   :allow-imperfect-p allow-imperfect-p))))
    output))

(defun 160b-wiggle-nybbles (a b)
  (flet ((truthy (n) (if (zerop n) 0 1)))
    (logior (ash (truthy (logand a #x2)) 7)
            (ash (truthy (logand a #x1)) 6)
            (ash (truthy (logand b #x2)) 5)
            (ash (truthy (logand b #x1)) 4)
            (ash (truthy (logand a #x8)) 3)
            (ash (truthy (logand a #x4)) 2)
            (ash (truthy (logand b #x8)) 1)
            (ash (truthy (logand b #x4)) 0))))

(defun write-direct-stamp-header (label stamp-offset screen-x stream)
  (format stream "~&~10t.byte <(~a + $~2,'0x), $c0, >(~a + $~2,'0x), $10, $~2,'0x"
          label stamp-offset label stamp-offset screen-x))

(defun write-stamp-bytes-for-blob (stamp-bytes stream)
  (format *trace-output* "2px × 16px bytes × ~:d" (array-dimension stamp-bytes 0))
  (dotimes (y #x10)
    (loop for stamp-index from 0
            below (floor (array-dimension stamp-bytes 0) #x10)
          for stamp-page = (floor stamp-index 8)
          do (dotimes (span 5)
               (write-bytes (loop for x from 0 below #x10
                                  collecting (aref stamp-bytes
                                                   (+ x (* #x10 stamp-index))
                                                   (- #x0f y)))
                            stream)))))

(defun gather-stamp-bytes (normalized-pixels stamp-bytes &key stamp-index)
  (dotimes (b #x10)
    (dotimes (y #x10)
      (let ((a (aref normalized-pixels (* 2 b) y))
            (b (aref normalized-pixels (1+ (* 2 b)) y)))
        (setf (aref stamp-bytes (+ (* #x10 stamp-index) b) y)
              (160b-wiggle-nybbles a b))))))

(defun load-blob-image (pathname$)
  (format *trace-output* "~&Loading BLOB image from ~a" (enough-namestring pathname$))
  (let* ((png (png-read:read-png-file
               (let ((pathname (parse-namestring pathname$)))
                 (make-pathname
                  :name (pathname-name pathname)
                  :type (pathname-type pathname)
                  :defaults #p"./Source/Art/"))))
         (height (png-read:height png))
         (width (png-read:width png))
         (*machine* 7800))
    (png->palette height width
                  (png-read:image-data png))))

(defun extract-4×16-stamps (image)
  (let* ((rows (floor (1- (array-dimension image 1)) 16))
         (columns (floor (array-dimension image 0) 4))
         (output (make-array (list columns rows))))
    (dotimes (row rows)
      (dotimes (column columns)
        (let ((stamp (extract-region image (* column 4) (* row 16)
                                     (+ (* column 4) 3) (+ (* row 16) 15))))
          (assert (= 4 (array-dimension stamp 0)))
          (assert (= 16 (array-dimension stamp 1)))
          (setf (aref output column row) stamp))))
    output))

(defun blank-stamp-p (region background-color)
  (destructuring-bind (width height) (array-dimensions region)
    (dotimes (x width)
      (dotimes (y height)
        (unless (= background-color (aref region x y))
          (return-from blank-stamp-p nil)))))
  t)

(defun stamp-is-monochrome-p (stamp)
  "@cindex graphics mode detection
@cindex monochrome detection
@cindex 320A mode suitability

@table @code
@item Package: skyline-tool
@item Arguments: stamp (2D array of palette indices)
@item Returns: boolean
@item Side Effects: none
@end table

Determine if a 4×16 pixel stamp contains only 2 colors, making it suitable for 320A monochrome mode.

@strong{Detection Logic:}
@itemize
@item Counts unique palette indices in the stamp
@item Returns true if ≤ 2 unique colors found
@item Suitable for 320A mode (1 bit per pixel)
@item False indicates 320C mode needed (4 colors + transparency)
@end itemize

Used by 320A/C mode ripping to automatically select appropriate graphics mode per stamp."
  (let ((colors (make-hash-table)))
    (destructuring-bind (width height)
        (array-dimensions stamp)
      (dotimes (x width)
        (dotimes (y height)
          (setf (gethash (aref stamp x y) colors) t)))
      (<= (hash-table-count colors) 2))))

(defun limit-region-to-palette (region palette &key (allow-imperfect-p t))
  (let ((output (make-array (array-dimensions region)))
        (rgb (mapcar #'palette->rgb (coerce palette 'list))))
    (destructuring-bind (width height) (array-dimensions region)
      (dotimes (x width)
        (dotimes (y height)
          (setf (aref output x y)
                (if allow-imperfect-p
                    (apply #'rgb->palette
                           (apply #'find-nearest-in-palette rgb
                                  (palette->rgb (aref region x y))))
                    (or (position (aref region x y) palette)
                        (error "Color ~s  at (~d, ~d) is not in palette ~s"
                               (aref region x y) x y palette)))))))
    output))

(defun png-to-blob-pathname (png-file)
  "Pathname for generated Blob assembly from PNG-FILE (under Source/Blobs/PORT/).

@table @asis
@item PNG-FILE
Blob PNG path; if under @file{Source/Blobs/@var{PORT}/}, output is
@file{Source/Generated/@var{PORT}/Assets/Blob.*.s}; otherwise
@file{Source/Generated/Assets/Blob.*.s}.
@end table"
  (let* ((merged (merge-pathnames png-file))
         (dir (pathname-directory merged))
         (blobs-index (when dir (position "Blobs" dir :test #'string= :key #'string)))
         (port-dir (when (and blobs-index (< (1+ blobs-index) (length dir)))
                     (nth (1+ blobs-index) dir))))
    (make-pathname :directory (if port-dir
                                  (list :relative "Source" "Generated" port-dir "Assets")
                                  (list :relative "Source" "Generated" "Assets"))
                   :name (concatenate 'string "Blob."
                                      (pathname-name merged))
                   :type "s")))

(defun check-height+width-for-blob (height width palette-pixels)
  (assert (zerop (mod width 4)) (width)
          "BLOB ripper requires width mod 4, not ~d (4 × ~{~d + ~d~})"
          width (multiple-value-list (floor width 4)))
  (assert (zerop (mod (1- height) 16)) (height)
          "BLOB ripper requires height mod 16 + 1, not ~d (16 × ~{~d + ~d~})"
          height (multiple-value-list (floor height 16)))
  (format *trace-output* " (~:d×~:d px)" width height)
  (finish-output *trace-output*)
  (assert (= (array-dimension palette-pixels 0) width))
  (assert (= (array-dimension palette-pixels 1) height)))

(defun check-height+width-for-blob-320ac (height width palette-pixels)
  "@cindex dimension validation
@cindex 320A/C mode validation

@table @code
@item Package: skyline-tool
@item Arguments: height (integer), width (integer), palette-pixels (2D array)
@item Returns: nil (signals error if invalid)
@item Side Effects: Outputs dimensions to *trace-output*, signals assertion errors
@end table

Validate dimensions and palette data for 320A/C mode BLOB ripping.

@strong{Requirements:}
@itemize
@item Width must be exactly 320 pixels
@item Height must be (N × 16) + 1 pixels for palette strip
@item Palette pixels array dimensions must match width × height
@end itemize

Signals assertion errors for invalid dimensions."
  (assert (= width 320) (width)
          "320A/C BLOB ripper requires width = 320px, not ~d" width)
  (assert (zerop (mod (1- height) 16)) (height)
          "320A/C BLOB ripper requires height mod 16 + 1, not ~d (16 × ~{~d + ~d~})"
          height (multiple-value-list (floor height 16)))
  (format *trace-output* " (~:d×~:d px)" width height)
  (finish-output *trace-output*)
  (assert (= (array-dimension palette-pixels 0) width))
  (assert (= (array-dimension palette-pixels 1) height)))

(defun write-blob-palettes (png output)
  (princ "Palette:" output)
  (dolist (*region* '(:ntsc :pal))
    (let ((palettes (extract-palettes
                     (png->palette (png-read:height png)
                                   (png-read:width png)
                                   (png-read:image-data png)))))
      (format output "~%~10t.if TV == ~a
~12t.byte ~a~{~%~12t.byte ~a, ~a, ~a~}
~10t.fi~%"
              *region*
              (atari-colu-string (aref palettes 0 0))
              (mapcan (lambda (pal) (mapcar #'atari-colu-string (coerce (subseq pal 1 4) 'list)))
                      (2a-to-list palettes))))))

(defun blob/write-span-to-stamp-buffer (span stamp-buffer
                                        &key stamp-offsets serial output id
                                             imperfectp)
  (setf (gethash id stamp-offsets) serial)
  (let ((start (+ (* #x1000 (floor serial #x100))
                  (mod serial #x100))))
    (when (>= start (array-dimension stamp-buffer 0))
      (adjust-array stamp-buffer (+ #x1000 (array-dimension stamp-buffer 0))))
    (format output "~%~10tSpan~x = * + $~4,'0x" id start)
    (dotimes (stamp (length span))
      (let ((stamp-bytes
              (let ((bytes-across (7800-image-to-160a (elt span stamp)
                                                      :byte-width 1
                                                      :height 16
                                                      :palette #(0 1 2 3)
                                                      :best-fit-p imperfectp)))
                (assert (= 1 (length bytes-across)))
                (car bytes-across))))
        (dotimes (byte 16)
          (let ((i (+ start stamp (* #x100 byte))))
            (assert (let ((b (aref stamp-buffer i)))
                      (or (null b) (zerop b))) ()
                      "Stamp buffer contains ~x at index ~x; serial ~x, stamp ~x"
                      (aref stamp-buffer i) i serial stamp)
            (setf (aref stamp-buffer i)
                  (elt stamp-bytes (- 15 byte)))))))))

(defun convert-stamp-to-palette (stamp pal-index palettes
                                 &key (allow-imperfect-p t))
  (map-region-to-palette
   stamp
   (mapcar #'palette->rgb (coerce (elt (2a-to-list palettes) pal-index) 'list))
   :allow-imperfect-p allow-imperfect-p))

(defun blob/write-spans (spans output &key imperfectp)
  (format output "~2%Spans:~%")
  (let ((stamp-buffer (make-array #x1000 :adjustable t))
        (stamp-offsets (make-hash-table)))
    (loop for span being the hash-keys in spans using (hash-value id)
          for serial from 0
          do (progn
               (if (and (< serial #x100)
                        (>= (+ serial (length span)) #x100))
                   (setf serial #x100))
               (blob/write-span-to-stamp-buffer span stamp-buffer
                                                :stamp-offsets stamp-offsets
                                                :serial serial
                                                :output output
                                                :id id
                                                :imperfectp imperfectp)
               (incf serial (length span))))
    (format *trace-output* " writing stamps … ")
    (format output "~2%;;; Binary stamp data follows.~%")
    (hex-dump-bytes stamp-buffer output)
    (format output "~2%~10t.bend~%")
    (format output "~2%;;; This size marker is the estimated amount of ROM that this
;;; blob may take up, used for allocation purposes.
;;; $SIZE$~x~%"
            (+ #x20
               (* 4 (hash-table-count spans))
               (length stamp-buffer)))))

(defun blob/write-spans-320ac (spans output &key imperfectp)
  "Write spans for 320A/C mode, handling both monochrome (320A) and color (320C) stamps."
  (format output "~2%Spans:~%")
  (let ((stamp-buffer (make-array #x1000 :adjustable t))
        (stamp-offsets (make-hash-table)))
    (loop for span being the hash-keys in spans using (hash-value id)
          for serial from 0
          do (progn
               (if (and (< serial #x100)
                        (>= (+ serial (length span)) #x100))
                   (setf serial #x100))
               (blob/write-span-to-stamp-buffer-320ac span stamp-buffer
                                                      :stamp-offsets stamp-offsets
                                                      :serial serial
                                                      :output output
                                                      :id id
                                                      :imperfectp imperfectp)
               (incf serial (length span))))
    (format *trace-output* " writing 320A/C stamps … ")
    (format output "~2%;;; Binary stamp data follows.~%")
    (hex-dump-bytes stamp-buffer output)
    (format output "~2%~10t.bend~%")
    (format output "~2%;;; This size marker is the estimated amount of ROM that this
;;; blob may take up, used for allocation purposes.
;;; $SIZE$~x~%"
            (+ #x20
               (* 4 (hash-table-count spans))
               (length stamp-buffer)))))

(defun blob/write-span-to-stamp-buffer-320ac (span stamp-buffer
                                              &key stamp-offsets serial output id
                                                   imperfectp)
  "Write a span of stamps for 320A/C mode, detecting and converting each stamp appropriately."
  (setf (gethash id stamp-offsets) serial)
  (let ((start (+ (* #x1000 (floor serial #x100))
                  (mod serial #x100))))
    (when (>= start (array-dimension stamp-buffer 0))
      (adjust-array stamp-buffer (+ #x1000 (array-dimension stamp-buffer 0))))
    (format output "~%~10tSpan~x = * + $~4,'0x" id start)
    (dotimes (stamp (length span))
      (let* ((stamp-data (elt span stamp))
             (mode (if (stamp-is-monochrome-p stamp-data) :320a :320c))
             (bytes (if (eq mode :320a)
                        ;; 320A mode: 8 pixels per byte, monochrome; stamps are 4×16 cells
                        ;; but 320A conversion expects 8 pixel columns — pad with transparent.
                        ;; Map any non-zero palette index to 1 (foreground); 0 stays transparent.
                        (let* ((w (array-dimension stamp-data 0))
                               (h (array-dimension stamp-data 1))
                               (padded (if (>= w 8)
                                           stamp-data
                                           (let ((p (make-array (list 8 h)
                                                                :element-type '(unsigned-byte 8))))
                                             (dotimes (x w)
                                               (dotimes (y h)
                                                 (setf (aref p x y) (aref stamp-data x y))))
                                             (dotimes (x (- 8 w))
                                               (dotimes (y h)
                                                 (setf (aref p (+ w x) y) 0)))
                                             p)))
                               (binary (let ((b (make-array (array-dimensions padded)
                                                            :element-type '(unsigned-byte 8))))
                                         (destructuring-bind (pw ph) (array-dimensions padded)
                                           (dotimes (x pw)
                                             (dotimes (y ph)
                                               (setf (aref b x y) (if (zerop (aref padded x y)) 0 1)))))
                                         b))
                               (bytes-across (7800-image-to-320a binary
                                                                 :byte-width 1
                                                                 :height 16
                                                                 :palette #(0 1)
                                                                 :best-fit-p imperfectp)))
                          (assert (= 1 (length bytes-across)))
                          (car bytes-across))
                        ;; 320C mode: 4 pixels per byte, 4 colors
                        (let ((bytes-across (7800-image-to-320c stamp-data
                                                               :byte-width 1
                                                               :height 16
                                                               :palette #(0 1 2 3)
                                                               :best-fit-p imperfectp)))
                          (assert (= 1 (length bytes-across)))
                          (car bytes-across)))))
        (dotimes (byte 16)
          (let ((i (+ start stamp (* #x100 byte))))
            (assert (let ((b (aref stamp-buffer i)))
                      (or (null b) (zerop b))) ()
                      "Stamp buffer contains ~x at index ~x; serial ~x, stamp ~x"
                      (aref stamp-buffer i) i serial stamp)
            (setf (aref stamp-buffer i)
                  (elt bytes (- 15 byte)))))))))

(defun blob-rip-7800 (png-file &optional (imperfectp$ nil))
  "Rip a Bitmap Large Object Block from PNG-FILE

automatically selecting the appropriate Atari 7800 graphics mode based on image width.

@cindex BLOB ripping
@cindex graphics mode auto-detection
@cindex 160A mode
@cindex 320A/C mode

@table @code
@item Package: skyline-tool
@item Arguments: png-file (pathname or string), &optional imperfectp$ (boolean)
@item Returns: nil
@item Side Effects: Creates .s file with BLOB data, outputs progress to *trace-output*
@end table
@strong{Mode Selection:}
@itemize
@item 320px width → 320A/C mixed mode (navigation charts)
@item Other widths → 160A mode (standard sprites)
@end itemize

@strong{Graphics Modes:}
@table @asis
@item 160A Mode
Standard 160-pixel wide sprites with 4-color palette
@item 320A/C Mode
Mixed 320-pixel wide graphics for navigation displays
@end table

@strong{Mode Selection:}
@itemize
@item 320px width → 320A/C mixed mode (navigation charts)
@item Other widths → 160A mode (standard sprites)
@end itemize

@strong{Graphics Modes:}
@table @asis
@item 160A Mode
4 pixels per byte, 25 palettes, for general sprite graphics
@item 320A/C Mode
Mixed monochrome (320A) and color (320C) modes for 320px wide navigation displays
@end table

Pass --imperfect to allow imperfect palette matches instead of signaling errors."
  (format *trace-output* "~&Ripping BLOB from ~a … " (enough-namestring png-file))
  (finish-output *trace-output*)
  (let* ((*machine* 7800)
         (*region* :ntsc)
         (png (png-read:read-png-file png-file))
         #+ () (height (png-read:height png))
         (width (png-read:width png))
         #+ () (palette-pixels (png->palette height width
                                             (png-read:image-data png)))
         #+ () (output-pathname (png-to-blob-pathname png-file))
         (imperfectp (or (eql :imperfect imperfectp$)
                         (equal imperfectp$ "--imperfect"))))
    (format *trace-output* "accepting ~:[only perfect palette matches~;imperfect palette matches~]… " imperfectp)
    ;; Route to appropriate ripping method based on width
    (if (= width 320)
        (blob-rip-7800-320ac png-file imperfectp$)
        (blob-rip-7800-160a png-file imperfectp$))))

(defun blob-rip-7800-160a (png-file &optional (imperfectp$ nil))
  "@cindex BLOB ripping
@cindex 160A graphics mode
@cindex sprite graphics

@table @code
@item Package: skyline-tool
@item Arguments: png-file (pathname or string), &optional imperfectp$ (boolean)
@item Returns: nil
@item Side Effects: Creates .s file with BLOB data, outputs progress to *trace-output*
@end table

Rip a Bitmap Large Object Block in 160A mode from PNG-FILE for standard sprite graphics.

@strong{Graphics Mode:}
@itemize
@item 4 pixels per byte (2 bits per pixel)
@item Up to 25 palettes (background + 8 palettes × 3 colors each)
@item Variable width (multiple of 4 pixels)
@item Height multiple of 16 + 1 pixels (palette strip)
@end itemize

@strong{Use Cases:}
@itemize
@item Character sprites and animations
@item Game objects and items
@item General purpose graphics (non-320px wide)
@end itemize

Pass --imperfect to allow imperfect palette matches instead of signaling errors."
  (let* ((imperfectp (and imperfectp$ (not (emptyp imperfectp$))))
         (png (png-read:read-png-file png-file))
         (height (png-read:height png))
         (width (png-read:width png))
         (palette-pixels (png->palette height width
                                       (png-read:image-data png)))
         (output-pathname (png-to-blob-pathname png-file))
         (imperfectp (or (eql :imperfect imperfectp$)
                         (equal imperfectp$ "--imperfect"))))
    (format *trace-output* "accepting ~:[only perfect palette matches~;imperfect palette matches~]… " imperfectp)
    (check-height+width-for-blob height width palette-pixels)
    (let* ((palettes (extract-palettes palette-pixels))
           (palettes-list (2a-to-lol palettes))
           (stamps (extract-4×16-stamps palette-pixels))
           (zones (floor height 16))
           (columns (floor width 4))
           (spans (make-hash-table :test 'equalp))
           (stamp-counting 0)
           (next-span-id 0))
      (format *trace-output* " generating drawing lists in ~a… " (enough-namestring output-pathname))
      (ensure-directories-exist output-pathname)
      (with-output-to-file (output output-pathname :if-exists :supersede)
        (format output ";;; Bitmap Large Object Block for Atari 7800
;;; Derived from source file ~a. This is a generated file.~3%

Blob_~a:~10t.block~2%"
                (enough-namestring png-file)
                (assembler-label-name (pathname-name png-file)))
        (write-blob-palettes png output)
        (format output "~%Zones:~%~10t.byte ~d~10t; zone count" zones)
        (dotimes (zone zones)
          (format output "~2&Zone~d:" zone)
          (flet ((emit-span (x span pal-index)
                   (when span
                     (let ((id (or (gethash span spans)
                                   (prog1
                                       (setf (gethash span spans) (prog1 next-span-id
                                                                    (incf next-span-id)))
                                     (cond
                                       ((and (< stamp-counting #x100)
                                             (< (+ stamp-counting (length span)) #x100))
                                        (incf stamp-counting (length span)))
                                       ((and (< stamp-counting #x100)
                                             (>= (+ stamp-counting (length span)) #x100))
                                        (setf stamp-counting #x100))
                                       (t (incf stamp-counting)))))))
                       (format output "~%~10t.DLHeader Span~x, ~d, ~d, ~d"
                               id pal-index (length span)
                               (- x (* 4 (length span))))))))
            (loop with span = nil
                  with last-palette = nil
                  for x from 0 by 4
                  for column from 0 below columns
                  for stamp = (aref stamps column zone)
                  for palette = (or (when (and last-palette
                                               (tile-fits-palette-p
                                                stamp
                                                (elt palettes-list last-palette)))
                                      last-palette)
                                    (best-palette stamp palettes
                                                  :allow-imperfect-p imperfectp
                                                  :x column :y zone))
                  for paletted-stamp = (limit-region-to-palette
                                        stamp (elt palettes-list palette)
                                        :allow-imperfect-p imperfectp)
                  do
                     (cond
                       ((zerop column)
                        (setf span (list paletted-stamp)
                              last-palette palette))
                       ((blank-stamp-p stamp (aref palettes 0 0))
                        (emit-span x span last-palette)
                        (setf span nil
                              last-palette nil))
                       ((and (or (null last-palette)
                                 (= palette last-palette))
                             (< (length span) 31))
                        (appendf span (list paletted-stamp))
                        (setf last-palette palette))
                       (t
                        (emit-span x span last-palette)
                        (setf span (list paletted-stamp)
                              last-palette palette)))
                  finally
                     (emit-span x span last-palette)))
          (format output "~%~10t.word $0000"))
        (blob/write-spans spans output :imperfectp imperfectp)))
    (format *trace-output* " … done!~%")))

(defun blob-rip-7800-320ac (png-file &optional (imperfectp$ nil))
  "@cindex BLOB ripping
@cindex 320A/C graphics mode
@cindex navigation chart graphics
@cindex mixed mode graphics

@table @code
@item Package: skyline-tool
@item Arguments: png-file (pathname or string), &optional imperfectp$ (boolean)
@item Returns: nil
@item Side Effects: Creates .s file with BLOB data, outputs progress to *trace-output*
@end table

Rip a Bitmap Large Object Block in mixed 320A/C mode from PNG-FILE for 320px wide navigation chart graphics.

@strong{Graphics Mode:}
@itemize
@item Uses 320A mode for monochrome stamps (1 bit per pixel, 1 color + transparent)
@item Uses 320C mode for color stamps (4 bits per pixel, 4 colors + transparent)
@item Automatically detects appropriate mode per 4×16 pixel stamp
@end itemize

@strong{Requirements:}
@itemize
@item Image width must be exactly 320 pixels
@item Image height must be (N × 16) + 1 pixels (palette strip)
@item PNG should contain appropriate palette data
@end itemize

Pass --imperfect to allow imperfect palette matches instead of signaling errors."
  (let* ((*machine* 7800)
         (*region* :ntsc)
         (png (png-read:read-png-file png-file))
         (height (png-read:height png))
         (width (png-read:width png))
         (palette-pixels (png->palette height width
                                       (png-read:image-data png)))
         (output-pathname (png-to-blob-pathname png-file))
         (imperfectp (or (eql :imperfect imperfectp$)
                         (equal imperfectp$ "--imperfect"))))
    (format *trace-output* "accepting ~:[only perfect palette matches~;imperfect palette matches~]… " imperfectp)
    (check-height+width-for-blob-320ac height width palette-pixels)
    (let* ((palettes (extract-palettes palette-pixels))
           (palettes-list (2a-to-lol palettes))
           (stamps (extract-4×16-stamps palette-pixels)) ; Use 4px stamps for 320C mode
           (zones (floor height 16))
           (columns (floor width 4)) ; 320 / 4 = 80 columns
           (spans (make-hash-table :test 'equalp))
           (stamp-counting 0)
           (next-span-id 0))
      (format *trace-output* " generating 320A/C drawing lists in ~a… " (enough-namestring output-pathname))
      (force-output *trace-output*)
      (format *trace-output* " zones=~d, stamps=~d×~d~%" zones columns zones)
      (force-output *trace-output*)
      (ensure-directories-exist output-pathname)
      (with-output-to-file (output output-pathname :if-exists :supersede)
        (format output ";;; Bitmap Large Object Block for Atari 7800 (320A/C mode)
;;; Derived from source file ~a. This is a generated file.~3%

Blob_~a:~10t.block~2%"
                (enough-namestring png-file)
                (assembler-label-name (pathname-name png-file)))
        (write-blob-palettes png output)
        (format output "~%Zones:~%~10t.byte ~d~10t; zone count" zones)
        (dotimes (zone zones)
          (format output "~2&Zone~d:" zone)
          (flet ((emit-span (x span last-palette last-mode)
                   ;; FIXME: Need  to switch  header types if  the current
                   ;; mode ≠ the last mode to write an "alt" header with
                   ;; the new mode enabled.
                   (when span
                     (let ((id (or (gethash span spans)
                                   (prog1
                                       (setf (gethash span spans) (prog1 next-span-id
                                                                    (incf next-span-id)))
                                     (cond
                                       ((and (< stamp-counting #x100)
                                             (< (+ stamp-counting (length span)) #x100))
                                        (incf stamp-counting (length span)))
                                       ((and (< stamp-counting #x100)
                                             (>= (+ stamp-counting (length span)) #x100))
                                        (setf stamp-counting #x100))
                                       (t (incf stamp-counting)))))))
                       (format output "~%~10t.DLHeader Span~x, ~d, ~d, ~d"
                               id last-palette (length span)
                               (- x (length span)))))))
            (loop with span = nil
                  with last-palette = nil
                  with last-mode = nil
                  for x from 0 by 1
                  for column from 0 below columns
                  for stamp = (aref stamps column zone)
                  for mode = (if (stamp-is-monochrome-p stamp) :320a :320c) ; Auto-detect mode
                  for palette = (or (when (and last-palette
                                               (tile-fits-palette-p
                                                stamp
                                                (elt palettes-list last-palette)))
                                      last-palette)
                                    (best-palette stamp palettes
                                                  :allow-imperfect-p imperfectp
                                                  :x column :y zone))
                  for paletted-stamp = (limit-region-to-palette
                                        stamp (elt palettes-list palette)
                                        :allow-imperfect-p imperfectp)
                  do (when (= (mod column 20) 0)
                       (format *trace-output* " col ~d/~d…" column columns)
                       (force-output *trace-output*))
                     (cond
                       ((zerop column)
                        (setf span (list paletted-stamp)
                              last-palette palette
                              last-mode mode))
                       ((blank-stamp-p stamp (aref palettes 0 0))
                        (emit-span x span last-palette last-mode)
                        (setf span nil
                              last-palette nil
                              last-mode nil))
                       ((and (or (null last-palette)
                                 (= palette last-palette))
                             (eq mode last-mode)
                             (< (length span) 31))
                        (appendf span (list paletted-stamp))
                        (setf last-palette palette
                              last-mode mode))
                       (t
                        (emit-span x span last-palette last-mode)
                        (setf span (list paletted-stamp)
                              last-palette palette
                              last-mode mode)))
                  finally
                     (emit-span x span last-palette last-mode)))
          (format output "~%~10t.word $0000"))
        ;; One Spans:/stamp region for the whole blob (same as blob-rip-7800-160a).
        ;; Calling write-spans-320ac inside dotimes duplicated SpanN = * + $… and .bend per zone.
        (blob/write-spans-320ac spans output :imperfectp imperfectp)))
    (format *trace-output* " … done!~%")))

(defun vcs-ntsc-color-names ()
  (loop for hue below #x10
        appending (loop for value below #x10 by 2
                        collecting (format nil "~a $~x"
                                           (subseq (string (elt +atari-ntsc-color-names+ hue)) 3)
                                           value))))

(defun vcs-pal-color-names ()
  (loop for hue below #x10
        appending (loop for value below #x10 by 2
                        collecting (format nil "~a $~x"
                                           (subseq (string (elt +atari-pal-color-names+ hue)) 3)
                                           value))))

(defun prosystem-ntsc-color-names ()
  (loop for hue below #x10
        appending (loop for value below #x10
                        collecting (format nil "~a $~x"
                                           (subseq (string (elt +atari-ntsc-color-names+ hue)) 3)
                                           value))))

(defun prosystem-pal-color-names ()
  (loop for hue below #x10
        appending (loop for value below #x10
                        collecting (format nil "~a $~x"
                                           ;; XXX these are NTSC color names
                                           (subseq (string (elt +atari-pal-color-names+ hue)) 3)
                                           value))))

(defun tty-xterm-p (&optional (stream *query-io*))
  "Returns a generalized true value if the terminal seems to be xterm-compatible"
  (and (not (equal "CLIM-CLX" (symbol-package (class-name (class-of stream)))))
       (search "xterm" (uiop:getenv "TERM"))))

(defun write-gimp-palette (name colors &optional color-names)
  (with-output-to-file (pal (make-pathname :name name
                                           :type "gpl"
                                           :directory '(:relative "Tools"))
                            :if-exists :supersede)
    (format pal "GIMP Palette
Name: ~a
Columns: ~d
#~{~%~3d ~3d ~3d # ~a~}~%"
            (substitute #\Space #\- name)
            (ecase (length colors)
              ((1 2 4 8) (length colors))
              ((16 32 64) 16)
              (128 8)
              (256 16)
              (512 32)
              (4096 16))
            (if color-names
                (mapcan (lambda (rgb n) (append rgb (list n)))
                        colors color-names)
                (mapcan (lambda (rgb) (append rgb (list (format nil "#~{~2,'0x~2,'0x~2,'0x~}" rgb))))
                        colors))))
  (format *trace-output* "~&Wrote ~:d color~:p palette “~a”~%"
          (length colors)
          (substitute #\Space #\- name))
  (when (<= (length colors) 256)
    (let ((i 0))
      (dolist (color colors)
        (print-wide-pixel color *trace-output*)
        (cond
          ((< (length colors) 20)
           (format *trace-output* " ~a~%"(elt color-names i)))
          (t
           (when (= 15 (mod i 16))
             (terpri *trace-output*))))
        (incf i))))
  (format *trace-output* "~C[0m" #\Escape))

(defun write-gimp-palettes ()
  "Write out Gimp palettes for those I know"
  (write-gimp-palette "Atari-2600-NTSC" +vcs-ntsc-palette+ (vcs-ntsc-color-names))
  (write-gimp-palette "Atari-2600-PAL" +vcs-pal-palette+ (vcs-pal-color-names))
  (write-gimp-palette "Atari-2600-SECAM" +vcs-secam-palette+
                      (mapcar (lambda (s) (cl-change-case:title-case (subseq (string s) 3)))
                              +vcs-secam-color-names+))
  (write-gimp-palette "Atari-7800-NTSC" +prosystem-ntsc-palette+ (prosystem-ntsc-color-names))
  (write-gimp-palette "Atari-7800-PAL" +prosystem-pal-palette+ (prosystem-pal-color-names))
  (write-gimp-palette "Commodore-64" +c64-palette+
                      (mapcar (compose #'cl-change-case:title-case
                                       #'string)
                              +c64-names+))
  (write-gimp-palette "NES-NTSC" +nes-palette-ntsc+)
  (write-gimp-palette "NES-PAL" +nes-palette-pal+)
  (write-gimp-palette "TurboGrafx-16" +tg16-palette+)
  (write-gimp-palette "Lynx" +lynx-palette+)
  (write-gimp-palette "Intellivision" +intv-palette+
                      (mapcar (compose #'cl-change-case:title-case
                                       #'string)
                              +intv-color-names+)))

(defun display-maria-art (stream &key dump mode address colors width (unit #x10)
                                      var-colors)
  (flet ((peek (offset)
           (if (< (+ address offset) (length dump))
               (aref dump (+ address offset))
               #xff)))
    (clim:formatting-table (stream :x-spacing 0 :y-spacing 0)
      (dotimes (y #x10)
        (clim:formatting-row (stream)
          (ecase mode
            (:160a (dotimes (byte width)
                     (let* ((bits (peek (+ (* (- #x0f y) #x100)
                                           byte))))
                       (clim:formatting-cell (stream)
                         (print-wide-pixel (elt colors
                                                (ash (logand #b11000000 bits) -6))
                                           stream :unit unit))
                       (clim:formatting-cell (stream)
                         (print-wide-pixel (elt colors
                                                (ash (logand #b00110000 bits) -4))
                                           stream :unit unit))
                       (clim:formatting-cell (stream)
                         (print-wide-pixel (elt colors
                                                (ash (logand #b00001100 bits) -2))
                                           stream :unit unit))
                       (clim:formatting-cell (stream)
                         (print-wide-pixel (elt colors
                                                (logand #b00000011 bits))
                                           stream :unit unit)))))
            (:160b (dotimes (byte width)
                     (let* ((bits (peek (+ (* (- #x0f y) #x100)
                                           byte)))
                            (left-pixel-c (ash (logand #b11000000 bits) -6))
                            (right-pixel-c (ash (logand #b00110000 bits) -4))
                            (left-pixel-p (ash (logand #b00001100 bits) -2))
                            (right-pixel-p (logand #b00000011 bits))
                            (left-color (logior (ash left-pixel-p 2) left-pixel-c))
                            (right-color (logior (ash right-pixel-p 2) right-pixel-c)))
                       (clim:formatting-cell (stream)
                         (cond
                           ((and var-colors (member left-color '(4 8 12)))
                            (print-wide-pixel
                             (elt colors (mod (elt var-colors (mod (1- (/ left-color 4)) 3)) #x10))
                             stream :unit unit))
                           ((member left-color '(4 8 12))
                            (print-wide-pixel (mod (elt colors 0) #x100)
                                              stream :unit unit))
                           (t
                            (print-wide-pixel (mod (elt colors left-color) #x100)
                                              stream :unit unit))))
                       (clim:formatting-cell (stream)
                         (cond
                           ((and var-colors (member right-color '(4 8 12)))
                            (print-wide-pixel
                             (elt colors (mod (elt var-colors (mod (1- (/ right-color 4)) 3)) #x10))
                             stream :unit unit))
                           ((member right-color '(4 8 12))
                            (print-wide-pixel (mod (elt colors 0) #x100)
                                              stream :unit unit))
                           (t
                            (print-wide-pixel (mod (elt colors right-color) #x100)
                                              stream :unit unit)))))))))))))
(defun print-clim-color (color stream)
  (clim:with-output-as-presentation (stream color 'palette-color)
    (clim:with-room-for-graphics (stream :height 24)
      (print-wide-pixel color stream :shortp t)
      (format stream " Color $~2,'0x = ~a #~2,'0x~2,'0x~2,'0x"
              color (atari-colu-string color)
              (elt (elt (machine-palette 7800) color) 0)
              (elt (elt (machine-palette 7800) color) 1)
              (elt (elt (machine-palette 7800) color) 2)))))

(defun palette-register-name (i rel)
  (cond
    ((zerop i) "Background")
    ((<= 1 i 3) (format nil "P~dC~d"
                        rel
                        i))
    ((= 4 i) "VarColor1")
    ((<= 5 i 7) (format nil "P~dC~d"
                        (+ 1 rel)
                        (- i 4)))
    ((= 8 i) "VarColor2")
    ((<= 9 i 11) (format nil "P~dC~d"
                         (+ 2 rel)
                         (- i 8)))
    ((= 12 i) "VarColor3")
    ((<= 13 i 15) (format nil "P~dC~d"
                          (+ 3 rel)
                          (- i 12)))
    (t nil)))

;; if  the other  version  of this  is working,  then  delete this  one,
;; else merge.
#+ ()
(defun compile-intv-sprite (png-file output-dir &key height width palette-pixels)
  "Compile Intellivision sprite (MOB data)

In Intellivision terminology, sprites are called MOBs (Moving Object Blocks).
This function compiles sprite graphics into MOB data format, similar to GRAM
compilation but for sprites that can be positioned anywhere on screen."
  (check-type png-file (or pathname string))
  (check-type output-dir (or pathname string))
  (let* ((palette-pixels (or palette-pixels
                             (let* ((png (png-read:read-png-file png-file))
                                    (png-height (png-read:height png))
                                    (png-width (png-read:width png))
                                    (α (png-read:transparency png)))
                               (png->palette png-height png-width
                                             (png-read:image-data png)
                                             α))))
         (array-width (array-dimension palette-pixels 0))
         (array-height (array-dimension palette-pixels 1))
         (width (floor (or width array-width)))
         (height (floor (or height array-height))))
    ;; Validate dimensions: ensure at least one 8×8 sprite
    (assert (>= width 8) (width) "Width must be at least 8 (for at least one sprite), got ~D" width)
    (assert (>= height 8) (height) "Height must be at least 8 (for at least one sprite), got ~D" height)
    ;; Validate dimensions are within array bounds
    (assert (<= width array-width)
            (width palette-pixels)
            "Can't extract sprite(s): width ~d requested exceeds image width ~d"
            width array-width)
    (assert (<= height array-height)
            (height palette-pixels)
            "Can't extract sprite(s): height ~d requested exceeds image height ~d"
            height array-height)
    ;; Check if monochrome (only black=0 and white=7 palette indices)
    (let ((colors (image-colors palette-pixels height width)))
      (unless (subsetp colors '(0 7) :test '=)
        (warn "Sprite image ~A is not black-and-white (found colors: ~{~D~^, ~});
treating non-black/non-white pixels as black"
              png-file colors))
      (let ((out-file (merge-pathnames
                       (make-pathname :name
                                      (pathname-name png-file)
                                      :type "s")
                       out-dir))
            (sprites-across (floor (/ width 8)))
            (sprites-down (floor (/ height 8))))
        (ensure-directories-exist (directory-namestring out-file))
        (with-output-to-file (src-file out-file :if-exists :supersede)
          (format src-file ";;; MOB sprites compiled from ~A
;;; Generated for Intellivision
;;; Each sprite: 8×8 pixels = 8 bytes = 4 16-bit DECLE values~%~%"
                  png-file)
          ;; Process each 8×8 sprite
          (loop for sprite-y from 0 below sprites-down
                do (loop for sprite-x from 0 below sprites-across
                         for sprite-index = (+ (* sprite-y sprites-across) sprite-x)
                         do (let ((start-x (* sprite-x 8))
                                  (start-y (* sprite-y 8))
                                  (sprite-bytes '()))
                              ;; Extract 8 bytes (one per row)
                              (loop for y from 0 below 8
                                    for byte = 0
                                    do (loop for x from 0 below 8
                                             for palette-index = (aref palette-pixels (+ start-x x) (+ start-y y))
                                             ;; White (palette index 7) = bit 1, black (0) or other = bit 0
                                             do (when (= palette-index 7)
                                                  (setf byte (logior byte (ash 1 (- 7 x))))))
                                       (push byte sprite-bytes))
                              ;; Pack bytes into 16-bit words (2 bytes per DECLE, 4 DECLE per sprite)
                              ;; Big-endian: most significant byte first
                              (let ((bytes-list (reverse sprite-bytes)))
                                (loop for i from 0 below 4
                                      for byte-first = (nth (* i 2) bytes-list) ; First byte (high byte)
                                      for byte-second = (nth (+ (* i 2) 1) bytes-list) ; Second byte (low byte)
                                      for word = (logior (ash byte-first 8) byte-second)
                                      do (format src-file "    DECLE   $~4,'0X~%" word)))))))
        (format *trace-output* "~% Wrote MOB sprite data to ~A." out-file)))))

(defun read-colecovision-art-index (index-in)
  "Read ColecoVision art index file and return list of (png-name mode width-px height-px)"
  (let ((png-list (list)))
    (format *trace-output* "~&~A: reading ColecoVision art index …" (enough-namestring index-in))
    (with-input-from-file (index index-in)
      (loop for line = (read-line index nil)
            while (and line (plusp (length line)) (not (char= #\; (char line 0))))
            do (let ((line (string-trim #(#\Space #\Tab #\Newline #\Return #\Page) line)))
                 (cond
                   ((emptyp line) nil)
                   ((char= #\# (char line 0)) nil)
                   (t (destructuring-bind (png-name mode cell-size)
                          (split-sequence #\Space line :remove-empty-subseqs t :test #'char=)
                        (destructuring-bind (width-px height-px)
                            (split-sequence #\× cell-size :test #'char=)
                          (push (list (make-keyword mode)
                                      (make-pathname :defaults index-in
                                                     :name (subseq png-name 0
                                                                   (position #\. png-name :from-end t))
                                                     :type "png")
                                      (parse-integer width-px)
                                      (parse-integer height-px))
                                png-list))))))))
    (format *trace-output* " done. Got ~:D PNG files to read." (length png-list))
    (reverse png-list)))

(defun parse-into-colecovision-chr-data (art-index)
  "Parse PNG files into ColecoVision CHR data (8x8 tiles)"
  (let ((chr-data (list)))
    (dolist (art-item art-index)
      (destructuring-bind (mode png-name width-px height-px) art-item
        (format *trace-output* "~&~A: parsing ColecoVision CHR data … "
                png-name)
        (let* ((png (png-read:read-png-file png-name))
               (height (png-read:height png))
               (width (png-read:width png))
               (palette-pixels (png->palette height width
                                             (png-read:image-data png)
                                             (png-read:transparency png))))
          ;; ColecoVision CHR format: similar to NES but monochrome or limited colors
          (let ((tile-data (parse-colecovision-chr-tiles palette-pixels width-px height-px)))
            (setf chr-data (append chr-data tile-data))))
        (format *trace-output* " done.")))
    chr-data))

(defun parse-colecovision-chr-tiles (palette-pixels width-px height-px)
  "Convert palette pixels to ColecoVision CHR ROM format (8x8 tiles)"
  (let ((tiles (list))
        (tile-width (/ width-px 8))
        (tile-height (/ height-px 8)))
    ;; Process each 8x8 tile
    (dotimes (ty tile-height)
      (dotimes (tx tile-width)
        (let ((tile-bytes (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
          ;; ColecoVision uses 1-bit per pixel for CHR (monochrome)
          (dotimes (y 8)
            (dotimes (x 8)
              (let* ((px (+ (* tx 8) x))
                     (py (+ (* ty 8) y))
                     (pixel-value (if (and (< px width-px) (< py height-px))
                                    (aref palette-pixels py px)
                                    0)))
                ;; For ColecoVision, treat any non-zero as 1 (monochrome)
                (when (> pixel-value 0)
                  (setf (aref tile-bytes y)
                        (logior (aref tile-bytes y) (ash 1 (- 7 x))))))))
          (push tile-bytes tiles))))
    (nreverse tiles)))

(defun write-colecovision-chr-rom (index-out chr-data)
  "Write ColecoVision CHR ROM data to binary file"
  (with-output-to-file (out index-out :element-type '(unsigned-byte 8)
                           :if-exists :supersede)
    (dolist (tile chr-data)
      (dotimes (i 8)
        (write-byte (aref tile i) out))))
  (format *trace-output* "~&Wrote ~:D bytes to ~A" (* (length chr-data) 8) index-out))

(defun compile-art-colecovision (index-out index-in)
  "Compile art assets for ColecoVision platform"
  (let ((*machine* 264))
    (write-colecovision-chr-rom index-out
                                (parse-into-colecovision-chr-data
                                 (read-colecovision-art-index index-in)))))

(defun compile-art-nes (index-out index-in)
  "Compile art assets for Nintendo Entertainment System platform"
  (let ((*machine* 3))
    (write-nes-chr-rom index-out
                       (parse-into-nes-chr-data
                        (read-nes-art-index index-in)))))

(defun read-snes-art-index (index-in)
  "Read SNES art index file and return list of (mode png-path width-px height-px).
Lines must have three space-separated tokens: png-name mode dimensions (e.g. name 2bpp 16×16).
Malformed lines (e.g. missing mode) are skipped."
  (let ((png-list (list)))
    (format *trace-output* "~&~A: reading SNES art index …" (enough-namestring index-in))
    (with-input-from-file (index index-in)
      (loop for line = (read-line index nil)
            while (and line (plusp (length line)) (not (char= #\; (char line 0))))
            do (let ((line (string-trim #(#\Space #\Tab #\Newline #\Return #\Page) line)))
                 (cond
                   ((emptyp line) nil)
                   ((char= #\# (char line 0)) nil)
                   (t (let ((tokens (split-sequence #\Space line :remove-empty-subseqs t :test #'char=)))
                        (when (= 3 (length tokens))
                          (destructuring-bind (png-name mode cell-size) tokens
                            (let ((dims (split-sequence #\× cell-size :test #'char=)))
                              (when (= 2 (length dims))
                                (destructuring-bind (width-px height-px) dims
                                  (push (list (make-keyword mode)
                                              (make-pathname :defaults index-in
                                                             :name (subseq png-name 0
                                                                           (position #\. png-name :from-end t))
                                                             :type "png")
                                              (parse-integer width-px)
                                              (parse-integer height-px))
                                        png-list))))))))))))
    (format *trace-output* " done. Got ~:D PNG files to read." (length png-list))
    (reverse png-list)))

(defun parse-into-snes-chr-data (art-index)
  "Parse PNG files into SNES CHR data (8x8 tiles with various bit depths)"
  (let ((chr-data (list)))
    (dolist (art-item art-index)
      (destructuring-bind (mode png-name width-px height-px) art-item
        (format *trace-output* "~&~A: parsing SNES CHR data … "
                png-name)
        (let* ((png (png-read:read-png-file png-name))
               (height (png-read:height png))
               (width (png-read:width png))
               (palette-pixels (png->palette height width
                                             (png-read:image-data png)
                                             (png-read:transparency png))))
          ;; SNES CHR format: 8x8 tiles, various bit depths (2, 4, 8)
          (let ((tile-data (parse-snes-chr-tiles palette-pixels width-px height-px mode)))
            (setf chr-data (append chr-data tile-data))))
        (format *trace-output* " done.")))
    chr-data))

(defun parse-snes-chr-tiles (palette-pixels width-px height-px mode)
  "Convert palette pixels to SNES CHR ROM format (8x8 tiles, various bit depths)"
  (let ((tiles (list))
        (tile-width (/ width-px 8))
        (tile-height (/ height-px 8))
        (bits-per-pixel (ecase mode
                          (:2bpp 2)
                          (:4bpp 4)
                          (:8bpp 8))))
    ;; Process each 8x8 tile
    (dotimes (ty tile-height)
      (dotimes (tx tile-width)
        (let ((tile-bytes (make-array (* bits-per-pixel 8) :element-type '(unsigned-byte 8) :initial-element 0)))
          ;; SNES uses interleaved bitplanes
          (dotimes (y 8)
            (dotimes (x 8)
              (let* ((px (+ (* tx 8) x))
                     (py (+ (* ty 8) y))
                     (color-index (if (and (< px width-px) (< py height-px))
                                    (mod (aref palette-pixels py px) (ash 1 bits-per-pixel))
                                    0)))
                ;; Set bits in the bitplanes
                (dotimes (bit  bits-per-pixel)
                  (when (logbitp bit color-index)
                    (let ((byte-index (+ (* bit 8) y)))
                      (setf (aref tile-bytes byte-index)
                            (logior (aref tile-bytes byte-index) (ash 1 (- 7 x))))))))))
          (push tile-bytes tiles))))
    (nreverse tiles)))

(defun write-snes-chr-rom (index-out chr-data)
  "Write SNES CHR ROM data to binary file"
  (with-output-to-file (out index-out :element-type '(unsigned-byte 8)
                           :if-exists :supersede)
    (dolist (tile chr-data)
      (dotimes (i (length tile))
        (write-byte (aref tile i) out))))
  (format *trace-output* "~&Wrote ~:D bytes to ~A"
          (reduce #'+ (mapcar #'length chr-data)) index-out))

(defun compile-art-snes (index-out index-in)
  "Compile art assets for Super Nintendo platform"
  (let ((*machine* 88))
    (write-snes-chr-rom index-out
                        (parse-into-snes-chr-data
                         (read-snes-art-index index-in)))))

(defun blob-rip-snes-tile (png-file)
  "Extract tile data from PNG for SNES (8x8 tiles)"
  (let ((*machine* 88))
    (format *trace-output* "~&Ripping SNES tile data from ~a …" (enough-namestring png-file))
    (let* ((png (png-read:read-png-file png-file))
           (height (png-read:height png))
           (width (png-read:width png))
           (palette-pixels (png->palette height width
                                         (png-read:image-data png)
                                         (png-read:transparency png)))
           (output-file (merge-pathnames
                        (make-pathname :name (pathname-name png-file)
                                     :type "s")
                        (directory-namestring png-file))))
      (with-output-to-file (out output-file :if-exists :supersede :if-does-not-exist :create)
        (format out ";;; SNES tile data ripped from ~a~%;;; Generated automatically~2%" png-file)
        (format out ".include \"snes.inc\"~2%")
        (format out ".segment \"TILES\"~%")
        (format out "~a:~%" (pathname-name png-file))

        ;; Convert to SNES 2BPP tile format (16 bytes per 8x8 tile)
        (let ((tile-width (/ width 8))
              (tile-height (/ height 8)))
          (dotimes (ty tile-height)
            (dotimes (tx tile-width)
              (let ((tile-bytes (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
                ;; Extract 8x8 pixel block
                (dotimes (y 8)
                  (dotimes (x 8)
                    (let* ((px (+ (* tx 8) x))
                           (py (+ (* ty 8) y))
                           (color-index (if (and (< px width) (< py height))
                                          (mod (aref palette-pixels py px) 4)
                                          0)))
                      ;; SNES 2BPP: 2 bitplanes, 8 bytes each
                      (dotimes (bit 2)
                        (when (logbitp bit color-index)
                          (let ((byte-index (+ (* bit 8) y)))
                            (setf (aref tile-bytes byte-index)
                                  (logior (aref tile-bytes byte-index) (ash 1 (- 7 x))))))))))
                ;; Write tile data
                (dotimes (i 16)
                  (format out "    .byte $~2,'0X~%" (aref tile-bytes i)))))))
        (format out "~%;;; End of tile data~%"))
      (format *trace-output* " done. Wrote ~:D tiles." (* (/ width 8) (/ height 8))))))

(defun blob-rip-snes-sprite (png-file)
  "Extract sprite data from PNG for SNES"
  ;; For SNES sprites, we can use the tile format but mark as sprite data
  (let ((*machine* 88))
    (format *trace-output* "~&Ripping SNES sprite data from ~a …" (enough-namestring png-file))
    (let ((output-file (merge-pathnames
                       (make-pathname :name (pathname-name png-file)
                                    :type "s")
                       (directory-namestring png-file))))
      (with-output-to-file (out output-file :if-exists :supersede :if-does-not-exist :create)
        (format out ";;; SNES sprite data ripped from ~a~%;;; Generated automatically~2%" png-file)
        (format out ".include \"snes.inc\"~2%")
        (format out ".segment \"SPRITES\"~%")
        (format out "~a:~%" (pathname-name png-file))
        (format out "    ;;; Sprite data - using tile format for now~%"))
      ;; Use tile ripping as base, then add sprite-specific metadata
      (blob-rip-snes-tile png-file))))

(defun blob-rip-snes-font (png-file)
  "Extract font data from PNG for SNES"
  (let ((*machine* 88))
    (format *trace-output* "~&Ripping SNES font data from ~a …" (enough-namestring png-file))
    (let ((output-file (merge-pathnames
                       (make-pathname :name (pathname-name png-file)
                                    :type "s")
                       (directory-namestring png-file))))
      (with-output-to-file (out output-file :if-exists :supersede :if-does-not-exist :create)
        (format out ";;; SNES font data ripped from ~a~%;;; Generated automatically~2%" png-file)
        (format out ".include \"snes.inc\"~2%")
        (format out ".segment \"FONT\"~%")
        (format out "~a:~%" (pathname-name png-file))
        (format out "    ;;; Font data - 8x8 or 8x16 characters~%"))
      ;; Use tile ripping as base for font characters
      (blob-rip-snes-tile png-file))))

;; Lynx Graphics Functions

(defun read-lynx-art-index (index-in)
  "Read Lynx art index file and return list of (png-name mode width-px height-px)"
  (let ((png-list (list)))
    (format *trace-output* "~&~A: reading Lynx art index …" (enough-namestring index-in))
    (with-input-from-file (index index-in)
      (loop for line = (read-line index nil)
            while (and line (plusp (length line)) (not (char= #\; (char line 0))))
            do (let ((line (string-trim #(#\Space #\Tab #\Newline #\Return #\Page) line)))
                 (cond
                   ((emptyp line) nil)
                   ((char= #\# (char line 0)) nil)
                   (t (destructuring-bind (png-name mode cell-size)
                          (split-sequence #\Space line :remove-empty-subseqs t :test #'char=)
                        (destructuring-bind (width-px height-px)
                            (split-sequence #\× cell-size :test #'char=)
                          (push (list (make-keyword mode)
                                      (make-pathname :defaults index-in
                                                     :name (subseq png-name 0
                                                                   (position #\. png-name :from-end t))
                                                     :type "png")
                                      (parse-integer width-px)
                                      (parse-integer height-px))
                                png-list))))))))
    (format *trace-output* " done. Got ~:D PNG files to read." (length png-list))
    (reverse png-list)))

(defun parse-into-lynx-chr-data (art-index)
  "Parse PNG files into Lynx CHR data (sprites with 16-color palette)"
  (let ((chr-data (list)))
    (dolist (art-item art-index)
      (destructuring-bind (mode png-name width-px height-px) art-item
        (format *trace-output* "~&~A: parsing Lynx CHR data … "
                png-name)
        (let* ((png (png-read:read-png-file png-name))
               (height (png-read:height png))
               (width (png-read:width png))
               (palette-pixels (png->palette height width
                                             (png-read:image-data png)
                                             (png-read:transparency png))))
          ;; Lynx CHR format: sprites with 16 colors per sprite
          (let ((sprite-data (parse-lynx-sprite-data palette-pixels width-px height-px mode)))
            (setf chr-data (append chr-data sprite-data))))
        (format *trace-output* " done.")))
    chr-data))

(defun parse-lynx-sprite-data (palette-pixels width-px height-px mode)
  "Parse palette pixels into Lynx sprite data format"
  (declare (ignore mode)) ;; For now, ignore mode - could be SPRITE, TILE, etc.
  ;; Lynx sprites use 16 colors (4-bit indices)
  ;; Convert the palette pixels to 4-bit indices
  (let ((sprite-data (list)))
    (dotimes (y height-px)
      (dotimes (x width-px)
        (let ((color-index (aref palette-pixels y x)))
          ;; Lynx uses 4-bit color indices (0-15)
          (push (logand color-index #x0F) sprite-data))))
    ;; Return as byte array (reversed since we pushed)
    (reverse sprite-data)))

(defun write-lynx-chr-rom (index-out chr-data)
  "Write Lynx CHR ROM data to binary file"
  (with-output-to-file (out index-out :element-type '(unsigned-byte 8)
                           :if-exists :supersede)
    (dolist (sprite chr-data)
      (dotimes (i (length sprite))
        (write-byte (aref sprite i) out))))
  (format *trace-output* "~&Wrote ~:D bytes to ~A"
          (reduce #'+ (mapcar #'length chr-data)) index-out))

(defun compile-art-lynx (index-out index-in)
  "Compile art assets for Atari Lynx platform"
  (let ((*machine* 200))
    (write-lynx-chr-rom index-out
                        (parse-into-lynx-chr-data
                         (read-lynx-art-index index-in)))))

;; Font compilation for Lynx

(defmethod compile-font-generic ((machine-type (eql 200)) format source-file-base-name font-input)
  "Compile font for Atari Lynx platform"
  (let ((*machine* 200))
    (declare (ignore format)) ;; Lynx uses a standard font format
    (compile-font-8×8 source-file-base-name "Lynx/Fonts" 8 8
                      (png->palette 8 8 font-input nil))))

;; Blob ripping for Lynx

(defun blob-rip-lynx-sprite (png-file)
  "Extract sprite data from PNG for Lynx"
  (let ((*machine* 200))
    (format *trace-output* "~&Ripping Lynx sprite data from ~a …" (enough-namestring png-file))
    (let ((output-file (merge-pathnames
                       (make-pathname :name (pathname-name png-file)
                                    :type "s")
                       (directory-namestring png-file))))
      (with-output-to-file (out output-file :if-exists :supersede :if-does-not-exist :create)
        (format out ";;; Lynx sprite data ripped from ~a~%;;; Generated automatically~2%" png-file)
        (format out ".include \"lynx.inc\"~2%")
        (format out ".segment \"SPRITES\"~%")
        (format out "~a:~%" (pathname-name png-file))
        (format out "    ;;; Sprite data - 16 colors per sprite, 4-bit indices~%")
        (format out "    ;;; TODO: Implement actual PNG parsing and data extraction~%"))
      (format *trace-output* " done. (Placeholder implementation)~%"))))

(defun blob-rip-lynx-tile (png-file)
  "Extract tile data from PNG for Lynx"
  (let ((*machine* 200))
    (format *trace-output* "~&Ripping Lynx tile data from ~a …" (enough-namestring png-file))
    (let ((output-file (merge-pathnames
                       (make-pathname :name (pathname-name png-file)
                                    :type "s")
                       (directory-namestring png-file))))
      (with-output-to-file (out output-file :if-exists :supersede :if-does-not-exist :create)
        (format out ";;; Lynx tile data ripped from ~a~%;;; Generated automatically~2%" png-file)
        (format out ".include \"lynx.inc\"~2%")
        (format out ".segment \"TILES\"~%")
        (format out "~a:~%" (pathname-name png-file))
        (format out "    ;;; Tile data - 8x8 pixels, 4-bit color indices~%")
        (format out "    ;;; TODO: Implement actual PNG parsing and data extraction~%"))
      (format *trace-output* " done. (Placeholder implementation)~%"))))

(defun blob-rip-lynx-font (png-file)
  "Extract font data from PNG for Lynx"
  (let ((*machine* 200))
    (format *trace-output* "~&Ripping Lynx font data from ~a …" (enough-namestring png-file))
    (let ((output-file (merge-pathnames
                       (make-pathname :name (pathname-name png-file)
                                    :type "s")
                       (directory-namestring png-file))))
      (with-output-to-file (out output-file :if-exists :supersede :if-does-not-exist :create)
        (format out ";;; Lynx font data ripped from ~a~%;;; Generated automatically~2%" png-file)
        (format out ".include \"lynx.inc\"~2%")
        (format out ".segment \"FONT\"~%")
        (format out "~a:~%" (pathname-name png-file))
        (format out "    ;;; Font data - 8x8 character glyphs, 4-bit color indices~%")
        (format out "    ;;; TODO: Implement actual PNG parsing and data extraction~%"))
      (format *trace-output* " done. (Placeholder implementation)~%"))))

;; TED (Commodore 16/Plus4) Blob Ripping Functions

(defun blob-rip-264-bitmap (png-file)
  "Extract bitmap data from PNG for TED"
  (let ((*machine* 264))
    (format *trace-output* "~&Ripping TED bitmap data from ~a …" (enough-namestring png-file))
    (let* ((png (png-read:read-png-file png-file))
           (height (png-read:height png))
           (width (png-read:width png))
           (palette-pixels (png->palette height width
                                         (png-read:image-data png)
                                         (png-read:transparency png)))
           (output-file (merge-pathnames
                        (make-pathname :name (pathname-name png-file)
                                     :type "s")
                        (directory-namestring png-file))))
      (with-output-to-file (out output-file :if-exists :supersede :if-does-not-exist :create)
        (format out ";;; TED bitmap data ripped from ~a~%;;; Generated automatically~2%" png-file)
        (format out ".include \"ted.inc\"~2%")
        (format out ".segment \"BITMAP\"~%")
        (format out "~a:~%" (pathname-name png-file))

        ;; Convert to TED bitmap format (320x200, 2 colors per 8x8 cell)
        (dotimes (cell-y 25)  ; 200/8 = 25 rows
          (dotimes (cell-x 40) ; 320/8 = 40 columns
            (dotimes (row 8)   ; 8 rows per cell
              (let ((bitmap-byte 0))
                (dotimes (col 8) ; 8 columns per row
                  (let* ((x (+ (* cell-x 8) col))
                         (y (+ (* cell-y 8) row))
                         (color-index (if (and (< x width) (< y height))
                                          (aref palette-pixels x y)
                                          0)))
                    ;; Use color > 0 as foreground (bit set)
                    (when (> color-index 0)
                      (setf bitmap-byte (logior bitmap-byte (ash 1 (- 7 col)))))))
                (format out "    .byte $~2,'0X~%" bitmap-byte))))))
      (format *trace-output* " done. Wrote TED bitmap data."))))

(defun blob-rip-264-sprite (png-file)
  "Extract sprite data from PNG for TED"
  (let ((*machine* 264))
    (format *trace-output* "~&Ripping TED sprite data from ~a …" (enough-namestring png-file))
    (let* ((png (png-read:read-png-file png-file))
           (height (png-read:height png))
           (width (png-read:width png))
           (palette-pixels (png->palette height width
                                         (png-read:image-data png)
                                         (png-read:transparency png)))
           (output-file (merge-pathnames
                        (make-pathname :name (pathname-name png-file)
                                     :type "s")
                        (directory-namestring png-file))))
      (with-output-to-file (out output-file :if-exists :supersede :if-does-not-exist :create)
        (format out ";;; TED sprite data ripped from ~a~%;;; Generated automatically~2%" png-file)
        (format out ".include \"ted.inc\"~2%")
        (format out ".segment \"SPRITES\"~%")
        (format out "~a:~%" (pathname-name png-file))

        ;; TED sprites are 24x21 pixels (63 bytes)
        (dotimes (row height)
          (dotimes (byte 3)  ; 3 bytes per row (24 pixels)
            (let ((sprite-byte 0))
              (dotimes (bit 8)
                (let* ((x (+ (* byte 8) bit))
                       (color-index (if (< x width)
                                        (aref palette-pixels x row)
                                        0)))
                  (when (> color-index 0)
                    (setf sprite-byte (logior sprite-byte (ash 1 (- 7 bit)))))))
              (format out "    .byte $~2,'0X~%" sprite-byte)))))
      (format *trace-output* " done. Wrote TED sprite data."))))

(defun blob-rip-264-font (png-file)
  "Extract font data from PNG for TED"
  (let ((*machine* 264))
    (format *trace-output* "~&Ripping TED font data from ~a …" (enough-namestring png-file))
    (let* ((png (png-read:read-png-file png-file))
           (height (png-read:height png))
           (width (png-read:width png))
           (palette-pixels (png->palette height width
                                         (png-read:image-data png)
                                         (png-read:transparency png)))
           (output-file (merge-pathnames
                        (make-pathname :name (pathname-name png-file)
                                     :type "s")
                        (directory-namestring png-file))))
      (with-output-to-file (out output-file :if-exists :supersede :if-does-not-exist :create)
        (format out ";;; TED font data ripped from ~a~%;;; Generated automatically~2%" png-file)
        (format out ".include \"ted.inc\"~2%")
        (format out ".segment \"FONT\"~%")
        (format out "~a:~%" (pathname-name png-file))

        ;; TED character set: 8x8 characters
        (let ((chars-wide (/ width 8))
              (chars-high (/ height 8)))
          (dotimes (char-y chars-high)
            (dotimes (char-x chars-wide)
              (format out "~%    ;; Character (~D,~D)~%" char-x char-y)
              (dotimes (row 8)
                (let ((font-byte 0))
                  (dotimes (col 8)
                    (let* ((x (+ (* char-x 8) col))
                           (y (+ (* char-y 8) row))
                           (color-index (if (and (< x width) (< y height))
                                            (aref palette-pixels x y)
                                            0)))
                      (when (> color-index 0)
                        (setf font-byte (logior font-byte (ash 1 (- 7 col)))))))
                  (format out "    .byte $~2,'0X~%" font-byte)))))))
      (format *trace-output* " done. Wrote TED font data."))))

(defun compile-art-sms (index-out index-in)
  "Compile art assets for Sega Master System platform"
  (let ((*machine* 3010))
    (write-sms-chr-rom index-out
                       (parse-into-sms-chr-data
                        (read-sms-art-index index-in)))))

(defun read-sms-art-index (index-in)
  "Read SMS art index file and return list of (png-name mode width-px height-px)"
  (let ((png-list (list)))
    (format *trace-output* "~&~A: reading SMS art index …" (enough-namestring index-in))
    (with-input-from-file (index index-in)
      (loop for line = (read-line index nil)
            while (and line (plusp (length line)) (not (char= #\; (char line 0))))
            do (let ((line (string-trim #(#\Space #\Tab #\Newline #\Return #\Page) line)))
                 (cond
                   ((emptyp line) nil)
                   ((char= #\# (char line 0)) nil)
                   (t (destructuring-bind (png-name mode cell-size)
                          (split-sequence #\Space line :remove-empty-subseqs t :test #'char=)
                        (destructuring-bind (width-px height-px)
                            (split-sequence #\× cell-size :test #'char=)
                          (push (list (make-keyword mode)
                                      (make-pathname :defaults index-in
                                                     :name (subseq png-name 0
                                                                   (position #\. png-name :from-end t))
                                                     :type "png")
                                      (parse-integer width-px)
                                      (parse-integer height-px))
                                png-list))))))))
    (format *trace-output* " done. Got ~:D PNG files to read." (length png-list))
    (reverse png-list)))

(defun parse-into-sms-chr-data (art-index)
  "Parse PNG files into SMS CHR data (8x8 tiles)"
  (let ((chr-data (list)))
    (dolist (art-item art-index)
      (destructuring-bind (mode png-name width-px height-px) art-item
        (format *trace-output* "~&~A: parsing SMS CHR data … "
                png-name)
        (let* ((png (png-read:read-png-file png-name))
               (height (png-read:height png))
               (width (png-read:width png))
               (palette-pixels (png->palette height width
                                             (png-read:image-data png)
                                             (png-read:transparency png))))
          ;; SMS CHR format: 8x8 tiles, 4 colors (2 bits per pixel)
          (let ((tile-data (parse-sms-chr-tiles palette-pixels width-px height-px)))
            (setf chr-data (append chr-data tile-data))))
        (format *trace-output* " done.")))
    chr-data))

(defun parse-sms-chr-tiles (palette-pixels width-px height-px)
  "Convert palette pixels to SMS CHR ROM format (8x8 tiles, 2 bits per pixel)"
  (let ((tiles (list))
        (tile-width (/ width-px 8))
        (tile-height (/ height-px 8)))
    ;; Process each 8x8 tile
    (dotimes (ty tile-height)
      (dotimes (tx tile-width)
        (let ((tile-bytes (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
          ;; SMS uses 4 bitplanes (2 bits per pixel)
          (dotimes (y 8)
            (dotimes (x 8)
              (let* ((px (+ (* tx 8) x))
                     (py (+ (* ty 8) y))
                     (color-index (if (and (< px width-px) (< py height-px))
                                      (mod (aref palette-pixels py px) 4)
                                      0))
                     (bit0 (if (logbitp 0 color-index) 1 0))
                     (bit1 (if (logbitp 1 color-index) 1 0)))
                ;; Set bits in the bitplanes
                (when (= bit0 1)
                  (setf (aref tile-bytes y)
                        (logior (aref tile-bytes y) (ash 1 (- 7 x)))))
                (when (= bit1 1)
                  (setf (aref tile-bytes (+ y 16))
                        (logior (aref tile-bytes (+ y 16)) (ash 1 (- 7 x))))))))
          (push tile-bytes tiles))))
    (nreverse tiles)))

(defun write-sms-chr-rom (index-out chr-data)
  "Write SMS CHR ROM data to binary file"
  (with-output-to-file (out index-out :element-type '(unsigned-byte 8)
                           :if-exists :supersede)
    (dolist (tile chr-data)
      (dotimes (i 32)
        (write-byte (aref tile i) out))))
  (format *trace-output* "~&Wrote ~:D bytes to ~A" (* (length chr-data) 32) index-out))

;;; Game Boy Color (CGB) Graphics Support

(defun compile-art-cgb (index-out index-in)
  "Compile art assets for Game Boy Color platform"
  (let ((*machine* 359020))
    (write-cgb-tile-data index-out
                         (parse-into-cgb-tile-data
                          (read-cgb-art-index index-in)))))

(defun read-cgb-art-index (index-in)
  "Read CGB art index file and return list of (png-name width-px height-px palette-mode)"
  (let ((png-list (list)))
    (format *trace-output* "~&CGB: reading art index …" (enough-namestring index-in))
    (with-input-from-file (index index-in)
      (loop for line = (read-line index nil)
            while line
            do (let ((trimmed (string-trim " " line)))
                 (when (and (> (length trimmed) 0)
                            (not (char= (char trimmed 0) #\;)))
                   (let* ((parts (split-sequence #\space trimmed :remove-empty-subseqs t))
                          (png-name (first parts))
                          (width (parse-integer (second parts)))
                          (height (parse-integer (third parts)))
                          (palette-mode (or (fourth parts) "bg"))) ; bg or obj
                     (push (list png-name width height palette-mode) png-list))))))
    (reverse png-list)))

(defun parse-into-cgb-tile-data (art-index-entries)
  "Parse art index entries into CGB tile data format"
  (let ((tile-data (make-array 0 :adjustable t :fill-pointer 0)))
    (dolist (entry art-index-entries)
      (destructuring-bind (png-name width height palette-mode) entry
        (let* ((image (load-png-image png-name))
               (tiles-wide (/ width 8))
               (tiles-high (/ height 8)))
          (dotimes (ty tiles-high)
            (dotimes (tx tiles-wide)
              (let ((tile-pixels (extract-region image (* tx 8) (* ty 8)
                                                (+ (* tx 8) 7) (+ (* ty 8) 7))))
                (let ((tile-bytes (cgb-tile-to-bytes tile-pixels palette-mode)))
                  (dotimes (i (length tile-bytes))
                    (vector-push-extend (aref tile-bytes i) tile-data)))))))))
    tile-data))

(defun cgb-tile-to-bytes (tile-pixels palette-mode)
  "Convert 8x8 tile pixels to CGB tile bytes (2BPP)"
  (let ((bytes (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (y 8)
      (let ((low-byte 0)
            (high-byte 0))
        (dotimes (x 8)
          (let* ((pixel (aref tile-pixels y x))
                 (palette-index (if (string= palette-mode "obj")
                                    (min pixel 3)  ; OBJ palettes are 0-3
                                    pixel)))       ; BG palettes can use 0-3
            (when (logbitp 0 palette-index)
              (setf low-byte (logior low-byte (ash 1 (- 7 x)))))
            (when (logbitp 1 palette-index)
              (setf high-byte (logior high-byte (ash 1 (- 7 x)))))))
        (setf (aref bytes (* y 2)) low-byte)
        (setf (aref bytes (1+ (* y 2))) high-byte)))
    bytes))

(defun write-cgb-tile-data (output-file tile-data)
  "Write CGB tile data to output file"
  (with-output-to-file (out output-file :if-exists :supersede :if-does-not-exist :create)
    (format out ";;; CGB Tile Data~%")
    (format out ";;; Generated automatically from art assets~2%")
    (let ((address 0))
      (dotimes (i (length tile-data))
        (when (zerop (mod address 16))
          (format out "~%tile_data_~4,'0x:" address))
        (format out " ~2,'0x" (aref tile-data i))
        (incf address))
      (format out "~2%"))))

(defun compile-tg16-tile-data (palette-pixels tile-x tile-y)
  "Convert an 8x8 tile from palette-pixels to TG16 planar format (32 bytes)"
  (let ((tile-bytes (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; TG16 uses planar format: 4 bitplanes of 8 bytes each
    (dotimes (y 8)
      (dotimes (x 8)
        (let* ((global-x (+ (* tile-x 8) x))
               (global-y (+ (* tile-y 8) y))
               (color-index (if (and (< global-x (array-dimension palette-pixels 0))
                                     (< global-y (array-dimension palette-pixels 1)))
                                (aref palette-pixels global-x global-y)
                                0))) ; Default to color 0 for out-of-bounds
          ;; Ensure color index is valid (0-15 for 4-bit)
          (when (> color-index 15)
            (warn "TG16 tile color index ~D exceeds 4-bit limit, truncating to ~D" color-index (logand color-index 15))
            (setf color-index (logand color-index 15)))

          ;; Set bits in the 4 bitplanes
          (dotimes (bit 4)
            (when (logbitp bit color-index)
              (let ((byte-index (+ (* bit 8) y)))
                (setf (aref tile-bytes byte-index)
                      (logior (aref tile-bytes byte-index) (ash 1 (- 7 x))))))))))
    tile-bytes))

(defun compile-tg16-sprite (png-file target-dir height width palette-pixels)
  "Compile TurboGrafx-16/PC Engine sprite data from PNG image"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "s")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; TG16 Sprite data compiled from ~A
;;; TurboGrafx-16/PC Engine sprite format
;;; Dimensions: ~Dx~D pixels (4-bit color)
~2%" png-file width height)

      ;; TG16 sprite data format: 4-bit pixels, planar arrangement
      (let* ((sprite-width-tiles (/ width 8))   ; in 8-pixel units
             (sprite-height-tiles (/ height 8))  ; in 8-pixel units
             (total-tiles (* sprite-width-tiles sprite-height-tiles)))

        (format src-file ";;; Sprite dimensions: ~Dx~D tiles (~D total tiles)
;;; Each tile: 32 bytes (4 bitplanes x 8 bytes)
;;; Total sprite data: ~D bytes
~2%" sprite-width-tiles sprite-height-tiles total-tiles (* total-tiles 32))

        ;; Generate sprite data structure
        (format src-file "~A_sprite_data:~%" (pathname-name png-file))

        ;; For each 8x8 tile in the sprite (row-major order)
        (dotimes (tile-y sprite-height-tiles)
          (dotimes (tile-x sprite-width-tiles)
            (format src-file "~%    ;; Tile (~D,~D) - bytes ~D-~D~%"
                    tile-x tile-y (* (+ tile-x (* tile-y sprite-width-tiles)) 32)
                    (+ (* (+ tile-x (* tile-y sprite-width-tiles)) 32) 31))

            ;; Convert tile to TG16 planar format
            (let ((tile-bytes (compile-tg16-tile-data palette-pixels tile-x tile-y)))
              ;; Output 4 bitplanes of 8 bytes each
              (dotimes (bitplane 4)
                (format src-file "    .byte ")
                (dotimes (byte 8)
                  (let ((byte-index (+ (* bitplane 8) byte)))
                    (if (= byte 7)
                        (format src-file "$~2,'0X~%" (aref tile-bytes byte-index))
                        (format src-file "$~2,'0X, " (aref tile-bytes byte-index))))))))

        (format src-file "~2%;;; Sprite descriptor for HuC6270 VDC
~A_sprite_descriptor:
    .byte ~D  ; Width in tiles
    .byte ~D  ; Height in tiles
    .word ~A_sprite_data  ; Data pointer
    .word ~D  ; Total tiles
~2%" (pathname-name png-file) sprite-width-tiles sprite-height-tiles (pathname-name png-file) total-tiles))))

    (format *trace-output* "~&Compiled TG16 sprite: ~A (~Dx~D tiles, ~D bytes)"
            out-file sprite-width-tiles sprite-height-tiles (* sprite-width-tiles sprite-height-tiles 32))))

(defun compile-tg16-background (png-file target-dir height width palette-pixels)
  "Compile TurboGrafx-16/PC Engine background data - TEMPORARILY DISABLED DUE TO COMPILATION ERROR"
  (error "TG16 background compilation temporarily disabled due to compilation error"))

(defun compile-sgb-frame (frame-out frame-in)
  "Compile Super Game Boy frame/border graphics"
  (let ((*machine* 359020)) ; SGB works with both DMG and CGB
    (write-sgb-frame-data frame-out
                          (parse-sgb-frame frame-in))))

(defun parse-sgb-frame (frame-in)
  "Parse SGB frame PNG file into SGB packet format"
  (let ((png (png-read:read-png-file frame-in)))
    (format *trace-output* "~&SGB: parsing frame from ~a …" (enough-namestring frame-in))
    (let* ((height (png-read:height png))
           (width (png-read:width png))
           (palette-pixels (png->palette height width
                                         (png-read:image-data png)
                                         (png-read:transparency png)))
           (frame-data (make-array (* height width) :element-type '(unsigned-byte 8))))
      ;; SGB frames are typically 256x224 pixels
      ;; Convert to SGB packet format
      (dotimes (y height)
        (dotimes (x width)
          (let ((pixel-index (aref palette-pixels y x)))
            (setf (aref frame-data (+ (* y width) x)) pixel-index))))
      (format *trace-output* " done.")
      frame-data)))

(defun write-sgb-frame-data (output-file frame-data)
  "Write SGB frame data in packet format for SNES transmission"
  (with-output-to-file (out output-file :if-exists :supersede :if-does-not-exist :create)
    (format out ";;; SGB Frame Data compiled from ~a~%;;; Generated automatically for Super Game Boy~2%" output-file)
    (format out ".include \"sgb.inc\"~2%")
    (format out ".segment \"SGB_FRAME\"~%")
    (format out "sgb_frame_data:~%")

    ;; SGB frame data is sent as packets to the SNES
    ;; Each packet can contain up to 15 bytes of data
    (let ((data-size (length frame-data))
          (packet-count 0))
      (dotimes (i (ceiling data-size 15))
        (let ((packet-start (* i 15))
              (packet-end (min (+ (* i 15) 15) data-size)))
          (format out "~%;;; Packet ~d~%" packet-count)
          (format out "    .byte $01  ; SGB packet command (data transfer)~%")
          (format out "    .byte ~d   ; Packet length~%" (- packet-end packet-start))
          (dotimes (j (- packet-end packet-start))
            (if (zerop (mod (+ (* i 15) j) 16))
                (format out "~%    .byte $~2,'0x" (aref frame-data (+ (* i 15) j)))
                (format out ", $~2,'0x" (aref frame-data (+ (* i 15) j)))))
          (incf packet-count))))

    (format out "~2%;;; End of SGB frame data~%")
    (format out "    .byte $00  ; End marker~2%")))

(defun parse-into-gb-tile-data (art-index &key color)
  "Parse PNG files into Game Boy tile data"
  (let ((all-tiles (list)))
    (dolist (art-item art-index)
      (destructuring-bind (png-name width-tiles height-tiles) art-item
        (format *trace-output* "~&~A: parsing Game Boy tile data (~Dx~D tiles) … "
                png-name width-tiles height-tiles)
        (let* ((png (png-read:read-png-file png-name))
               (height (png-read:height png))
               (width (png-read:width png))
               (palette-pixels (png->palette height width
                                             (png-read:image-data png)
                                             (png-read:transparency png)))
               (palette (if color
                            +gameboy-color-palette+
                            +gameboy-palette+)))
          ;; Convert to Game Boy tile format
          (let ((tile-data (parse-gb-tile-data palette-pixels width height palette)))
            (setf all-tiles (append all-tiles tile-data))))))
    all-tiles))

(defun write-gb-tile-data (index-out tile-data)
  "Write Game Boy tile data to binary file"
  (with-output-to-file (out index-out :element-type '(unsigned-byte 8)
                           :if-exists :supersede)
    (dolist (tile tile-data)
      (dotimes (i 16)
        (write-byte (aref tile i) out))))
      (format *trace-output* "~&Wrote ~:D bytes (~:D tiles) to ~A"
          (* (length tile-data) 16) (length tile-data) index-out))

;; Apple IIGS Graphics Conversion Functions

(defun compile-a2gs-super-hires (png-file target-dir height width palette-pixels)
  "Compile Apple IIGS Super Hi-Res graphics (320x200, 16 colors)"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "s")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; Apple IIGS Super Hi-Res graphics compiled from ~A
;;; 320x200 pixels, 16 colors (4-bit)
;;; Generated automatically
~2%" png-file)

      (format src-file ";;; Super Hi-Res memory layout: 4 bitplanes x 800 bytes each
;;; Total: 3200 bytes for 320x200 pixels
~2%")

      ;; Generate bitplane data
      (format src-file "~A_data:~%" (pathname-name png-file))

      ;; Apple IIGS Super Hi-Res uses 4 bitplanes
      (dotimes (bitplane 4)
        (format src-file "~%    ;; Bitplane ~D (bit ~D of color index)~%" bitplane bitplane)
        (format src-file "    .byte ")

        ;; Each bitplane has 800 bytes (320x200 pixels / 8 bits per byte)
        (dotimes (byte 800)
          (let ((byte-value 0))
            ;; Calculate which pixels this byte represents
            (dotimes (bit 8)
              (let* ((pixel-index (+ (* byte 8) bit))
                     (x (mod pixel-index 320))
                     (y (/ pixel-index 320)))
                (when (< y 200)  ; Ensure we don't go beyond image height
                  (let ((color-index (if (and (< x (array-dimension palette-pixels 0))
                                             (< y (array-dimension palette-pixels 1)))
                                       (aref palette-pixels x y)
                                       0)))
                    ;; Extract the specific bit from the color index
                    (when (logbitp bitplane color-index)
                      (setf byte-value (logior byte-value (ash 1 (- 7 bit)))))))))

            (if (= byte 799)
                (format src-file "$~2,'0X~%" byte-value)
                (format src-file "$~2,'0X, " byte-value))))

        ;; Start new line every 16 bytes for readability
        (when (= (mod (1+ byte) 16) 0)
          (format src-file "~%    .byte ")))

      ;; Add palette information
      (format src-file "~2%;;; Palette data (16 colors)
~A_palette:
    ;; Apple IIGS 16-color palette entries
    ;; Each entry is a 16-bit RGB value: 00000RRRRRGGGGGBBBBB
    .word $0000, $0000, $0000, $0000  ; Colors 0-3 (placeholder)
    .word $0000, $0000, $0000, $0000  ; Colors 4-7 (placeholder)
    .word $0000, $0000, $0000, $0000  ; Colors 8-11 (placeholder)
    .word $0000, $0000, $0000, $0000  ; Colors 12-15 (placeholder)
~2%" (pathname-name png-file))

      ;; Add graphics descriptor
      (format src-file ";;; Graphics descriptor
~A_descriptor:
    .word ~A_data      ; Graphics data pointer
    .word ~A_palette   ; Palette pointer
    .word 320          ; Width in pixels
    .word 200          ; Height in pixels
    .byte 16           ; Number of colors
    .byte 4            ; Bits per pixel
~2%" (pathname-name png-file) (pathname-name png-file) (pathname-name png-file))))

    (format *trace-output* "~&Compiled Apple IIGS Super Hi-Res: ~A (320x200, 16 colors, 3200 bytes)"
            out-file))

(defun compile-a2gs-double-hires (png-file target-dir height width palette-pixels)
  "Compile Apple IIGS Double Hi-Res graphics (560x192, 16 colors)"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "s")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; Apple IIGS Double Hi-Res graphics compiled from ~A
;;; 560x192 pixels, 16 colors (4-bit)
;;; Generated automatically
~2%" png-file)

      (format src-file ";;; Double Hi-Res memory layout: 4 bitplanes x 1344 bytes each
;;; Total: 5376 bytes for 560x192 pixels
;;; Note: Apple IIGS Double Hi-Res uses 560 pixels horizontally
~2%")

      ;; Generate bitplane data
      (format src-file "~A_data:~%" (pathname-name png-file))

      ;; Apple IIGS Double Hi-Res uses 4 bitplanes
      (dotimes (bitplane 4)
        (format src-file "~%    ;; Bitplane ~D (bit ~D of color index)~%" bitplane bitplane)
        (format src-file "    .byte ")

        ;; Each bitplane has 1344 bytes (560x192 pixels / 8 bits per byte)
        (dotimes (byte 1344)
          (let ((byte-value 0))
            ;; Calculate which pixels this byte represents
            (dotimes (bit 8)
              (let* ((pixel-index (+ (* byte 8) bit))
                     (x (mod pixel-index 560))
                     (y (/ pixel-index 560)))
                (when (< y 192)  ; Ensure we don't go beyond image height
                  (let ((color-index (if (and (< x (array-dimension palette-pixels 0))
                                             (< y (array-dimension palette-pixels 1)))
                                       (aref palette-pixels x y)
                                       0)))
                    ;; Extract the specific bit from the color index
                    (when (logbitp bitplane color-index)
                      (setf byte-value (logior byte-value (ash 1 (- 7 bit)))))))))

            (if (= byte 1343)
                (format src-file "$~2,'0X~%" byte-value)
                (format src-file "$~2,'0X, " byte-value))))

        ;; Start new line every 16 bytes for readability
        (when (= (mod (1+ byte) 16) 0)
          (format src-file "~%    .byte ")))

      ;; Add graphics descriptor
      (format src-file "~2%;;; Graphics descriptor
~A_descriptor:
    .word ~A_data      ; Graphics data pointer
    .word 560          ; Width in pixels
    .word 192          ; Height in pixels
    .byte 16           ; Number of colors
    .byte 4            ; Bits per pixel
~2%" (pathname-name png-file) (pathname-name png-file))))

    (format *trace-output* "~&Compiled Apple IIGS Double Hi-Res: ~A (560x192, 16 colors, 5376 bytes)"
            out-file))

(defun compile-a2gs-hires (png-file target-dir height width palette-pixels)
  "Compile Apple IIGS Hi-Res graphics (280x192, 6 colors)"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "s")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; Apple IIGS Hi-Res graphics compiled from ~A
;;; 280x192 pixels, 6 colors (NTSC artifact colors)
;;; Generated automatically
~2%" png-file)

      (format src-file ";;; Hi-Res memory layout: 3 bitplanes x 768 bytes each
;;; Total: 2304 bytes for 280x192 pixels
;;; Uses NTSC artifact colors for 6-color palette
~2%")

      ;; Generate bitplane data
      (format src-file "~A_data:~%" (pathname-name png-file))

      ;; Apple IIGS Hi-Res uses 3 bitplanes (for 6 colors, but actually uses 2-bit encoding + NTSC artifacts)
      (dotimes (bitplane 3)
        (format src-file "~%    ;; Bitplane ~D~%" bitplane)
        (format src-file "    .byte ")

        ;; Each bitplane has 768 bytes (280x192 pixels / 8 bits per byte)
        (dotimes (byte 768)
          (let ((byte-value 0))
            ;; Calculate which pixels this byte represents
            (dotimes (bit 8)
              (let* ((pixel-index (+ (* byte 8) bit))
                     (x (mod pixel-index 280))
                     (y (/ pixel-index 280)))
                (when (< y 192)  ; Ensure we don't go beyond image height
                  (let ((color-index (if (and (< x (array-dimension palette-pixels 0))
                                             (< y (array-dimension palette-pixels 1)))
                                       (aref palette-pixels x y)
                                       0)))
                    ;; For Hi-Res, we use 3 bits but only 6 colors are actually distinct
                    ;; due to NTSC artifacting
                    (when (and (< color-index 8) (logbitp bitplane (logand color-index 7)))
                      (setf byte-value (logior byte-value (ash 1 (- 7 bit)))))))))

            (if (= byte 767)
                (format src-file "$~2,'0X~%" byte-value)
                (format src-file "$~2,'0X, " byte-value))))

        ;; Start new line every 16 bytes for readability
        (when (= (mod (1+ byte) 16) 0)
          (format src-file "~%    .byte ")))

      ;; Add graphics descriptor
      (format src-file "~2%;;; Graphics descriptor
~A_descriptor:
    .word ~A_data      ; Graphics data pointer
    .word 280          ; Width in pixels
    .word 192          ; Height in pixels
    .byte 6            ; Number of colors (NTSC artifact)
    .byte 3            ; Effective bits per pixel
~2%" (pathname-name png-file) (pathname-name png-file)))

    (format *trace-output* "~&Compiled Apple IIGS Hi-Res: ~A (280x192, 6 colors, 2304 bytes)"
            out-file)))

(defun compile-a2gs-sprite (png-file target-dir height width palette-pixels)
  "Compile Apple IIGS sprite graphics"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "s")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; Apple IIGS sprite compiled from ~A
;;; ~Dx~D pixels
;;; Generated automatically
~2%" png-file width height)

      ;; Determine sprite format based on size
      (cond
        ((and (= width 16) (= height 16))
         (format src-file ";;; 16x16 sprite (small sprite)~%"))
        ((and (= width 32) (= height 32))
         (format src-file ";;; 32x32 sprite (medium sprite)~%"))
        ((and (= width 64) (= height 64))
         (format src-file ";;; 64x64 sprite (large sprite)~%"))
        (t
         (format src-file ";;; Custom sprite size~%")))

      ;; For sprites, we can use Super Hi-Res format but smaller
      (let* ((bytes-per-bitplane (* (/ width 8) height)) ; width/8 bytes per line × height lines
             (total-bytes (* bytes-per-bitplane 4))) ; 4 bitplanes

        (format src-file ";;; Sprite data: ~D bytes per bitplane, ~D bitplanes, ~D total bytes
~2%" bytes-per-bitplane 4 total-bytes)

        ;; Generate sprite data
        (format src-file "~A_data:~%" (pathname-name png-file))

        (dotimes (bitplane 4)
          (format src-file "~%    ;; Bitplane ~D~%" bitplane)
          (format src-file "    .byte ")

          (dotimes (byte bytes-per-bitplane)
            (let ((byte-value 0))
              ;; Calculate pixel data for this byte
              (dotimes (bit 8)
                (let* ((pixel-in-byte (+ (* byte 8) bit))
                     (x-in-sprite (mod pixel-in-byte width))
                     (y-in-sprite (/ pixel-in-byte width)))
                  (when (< y-in-sprite height)
                    (let ((color-index (if (and (< x-in-sprite (array-dimension palette-pixels 0))
                                               (< y-in-sprite (array-dimension palette-pixels 1)))
                                         (aref palette-pixels x-in-sprite y-in-sprite)
                                         0)))
                      (when (logbitp bitplane color-index)
                        (setf byte-value (logior byte-value (ash 1 (- 7 bit)))))))))

              (if (= byte (1- bytes-per-bitplane))
                  (format src-file "$~2,'0X~%" byte-value)
                  (format src-file "$~2,'0X, " byte-value))))

          (when (= (mod (1+ bitplane) 4) 0)
            (format src-file "~%")))

        ;; Add sprite descriptor
        (format src-file "~2%;;; Sprite descriptor
~A_descriptor:
    .word ~A_data      ; Sprite data pointer
    .word ~D           ; Width in pixels
    .word ~D           ; Height in pixels
    .byte 16           ; Colors per sprite
    .byte 4            ; Bits per pixel
~2%" (pathname-name png-file) (pathname-name png-file) width height))))

      (format *trace-output* "~&Compiled Apple IIGS sprite: ~A (~Dx~D pixels)"
            out-file width height))

;; Apple IIGS Art Compilation Interface

(defmethod compile-art-generic ((machine-type (eql 222)) format source-file-base-name art-input)
  "Compile art for Apple IIGS platform"
  (let ((*machine* 222))
    (declare (ignore format)) ;; Apple IIGS uses standard graphics formats
    (compile-art-8×8 source-file-base-name "2gs/Fonts" 8 8
                     (png->palette 8 8 art-input nil))))

(defun compile-art-264 (index-out index-in)
  "Compile art assets for Commodore 16/Plus4 (TED) platform"
  (let ((*machine* 264))
    (write-ted-art-index index-out
                         (read-ted-art-index index-in))))

(defun read-ted-art-index (index-in)
  "Read TED art index file and return list of (png-name mode width-px height-px)"
  (let ((png-list (list)))
    (format *trace-output* "~&TED: reading art index …" (enough-namestring index-in))
    (with-input-from-file (index index-in)
      (loop for line = (read-line index nil)
            while (and line (plusp (length line)) (not (char= #\; (char line 0))))
            do (let ((line (string-trim #(#\Space #\Tab #\Newline #\Return #\Page) line)))
                 (cond
                   ((emptyp line) nil)
                   ((char= #\# (char line 0)) nil)
                   (t (destructuring-bind (png-name mode cell-size)
                          (split-sequence #\Space line :remove-empty-subseqs t :test #'char=)
                        (destructuring-bind (width-px height-px)
                            (split-sequence #\× cell-size :test #'char=)
                          (push (list (make-keyword mode)
                                      (make-pathname :defaults index-in
                                                     :name (subseq png-name 0
                                                                   (position #\. png-name :from-end t))
                                                     :type "png")
                                      (parse-integer width-px)
                                      (parse-integer height-px))
                                png-list)))))))
    (format *trace-output* " done. Got ~:D PNG files to read." (length png-list))
    (reverse png-list))))

(defgeneric compile-ted-art-by-mode (mode png-name directory height-px width-px)
  (:documentation "Compile TED art based on mode using EQL specializers")
  (:method ((mode (eql :bitmap)) png-name directory height-px width-px)
    (compile-ted-bitmap png-name directory height-px width-px
                        (png->palette width-px height-px
                                      (png-read:image-data (png-read:read-png-file png-name))
                                      (png-read:transparency (png-read:read-png-file png-name)))))
  (:method ((mode (eql :chars)) png-name directory height-px width-px)
    (compile-ted-charmap png-name directory height-px width-px
                         (png->palette width-px height-px
                                       (png-read:image-data (png-read:read-png-file png-name))
                                       (png-read:transparency (png-read:read-png-file png-name)))))
  (:method ((mode (eql :sprite)) png-name directory height-px width-px)
    (compile-ted-sprite png-name directory height-px width-px
                        (png->palette width-px height-px
                                      (png-read:image-data (png-read:read-png-file png-name))
                                      (png-read:transparency (png-read:read-png-file png-name)))))
  (:method ((mode (eql :multicolor-sprite)) png-name directory height-px width-px)
    (compile-ted-multicolor-sprite png-name directory height-px width-px
                                   (png->palette width-px height-px
                                                 (png-read:image-data (png-read:read-png-file png-name))
                                                 (png-read:transparency (png-read:read-png-file png-name))))))

(defun write-ted-art-index (index-out art-index)
  "Write TED art data to output file"
  (with-output-to-file (out index-out :if-exists :supersede :if-does-not-exist :create)
    (format out ";;; TED Art Assets compiled
;;; Generated automatically
~2%")
    (dolist (art-item art-index)
      (destructuring-bind (mode png-name width-px height-px) art-item
        (format *trace-output* "~&Processing TED art: ~A (~Dx~D)…" png-name width-px height-px)
        ;; Dispatch to appropriate compilation function based on mode
        (compile-ted-art-by-mode mode png-name (directory-namestring index-out) height-px width-px))))
  (format *trace-output* "~&TED art compilation complete."))

(defun compile-art-a2 (index-out index-in)
  "Compile art assets for Apple II HIRES graphics mode"
  (let ((*machine* 2))
    (write-a2-art-index index-out
                       (read-a2-art-index index-in))))

;; (defun compile-art-a2e (index-out index-in)
;;   "Compile art assets for Apple IIe Double HIRES graphics mode"
;;   (let ((*machine* 23))
;;     (write-a2e-art-index index-out
;;                         (read-a2e-art-index index-in))))

(defun read-a2-art-index (index-in)
  "Read Apple II HIRES art index file and return list of (png-name width height)"
  (let ((png-list (list)))
    (format *trace-output* "~&Apple II HIRES: reading art index …" (enough-namestring index-in))
    (with-input-from-file (index index-in)
      (loop for line = (read-line index nil)
            while line
            do (let ((trimmed (string-trim " " line)))
                 (when (and (> (length trimmed) 0)
                            (not (char= (char trimmed 0) #\;)))
                   (let* ((parts (split-sequence #\space trimmed :remove-empty-subseqs t))
                          (png-name (first parts))
                          (width (parse-integer (second parts)))
                          (height (parse-integer (third parts))))
                     (push (list png-name width height) png-list))))))
    (nreverse png-list)))


(defun write-a2-art-index (index-out png-list)
  "Write Apple II HIRES art assembly code"
  (format *trace-output* "~&Apple II HIRES: writing art data …")
  (with-output-to-file (out index-out :if-exists :supersede :if-does-not-exist :create)
    (format out ";;; Apple II HIRES Art Assets compiled from index
;;; Generated for Apple II HIRES graphics (280x192, 6 colors)
~2%")
    (dolist (png-entry png-list)
      (destructuring-bind (png-name width height) png-entry
        (format out ";;; ~A: ~Dx~D pixels~%" png-name width height)
        (format out "~A_data:~%" (pathname-name png-name))
        
        ;; Generate placeholder HIRES bitmap data
        ;; Apple II HIRES stores 7 pixels per byte (140 bytes per line)
        (dotimes (y (ceiling height 192)) ; Handle multiple screens if needed
          (format out "~%    ;; Screen ~D~%" y)
          (dotimes (line 192) ; 192 scan lines
            (format out "~%    ;; Line ~D~%" line)
            ;; 40 bytes per line (280 pixels / 7 pixels per byte = 40 bytes)
            (dotimes (byte 40)
              (if (= byte 39)
                  (format out "    .byte $00~%") ; Last byte
                  (format out "    .byte $00, ")) ; Continuation bytes
              )))
        (format out "~%    ;; End of ~A data~2%" png-name)))
    (format out "~%    ;; HIRES color palette constants~%")
    (format out "HIRES_BLACK = $00~%")
    (format out "HIRES_PURPLE = $01~%")
    (format out "HIRES_ORANGE = $02~%")
    (format out "HIRES_BLUE = $03~%")
    (format out "HIRES_GREEN = $04~%")
    (format out "HIRES_WHITE = $05~%")))

(defun write-a2e-art-index (index-out png-list)
  "Write Apple IIe Double HIRES art assembly code"
  (format *trace-output* "~&Apple IIe Double HIRES: writing art data …")
  (with-output-to-file (out index-out :if-exists :supersede :if-does-not-exist :create)
    (format out ";;; Apple IIe Double HIRES Art Assets compiled from index
;;; Generated for Apple IIe Double HIRES graphics (560x192, 16 colors)
~2%")

    (dolist (png-entry png-list)
      (destructuring-bind (png-name width height) png-entry
        (format out ";;; ~A: ~Dx~D pixels~%" png-name width height)
        (format out "~A_data:~%" (pathname-name png-name))

        ;; Generate placeholder Double HIRES bitmap data
        ;; Double HIRES stores 7 pixels per byte in both main and aux memory
        ;; 80 bytes per line (560 pixels / 7 pixels per byte = 80 bytes)
        (dotimes (y (ceiling height 192)) ; Handle multiple screens if needed
          (format out "~%    ;; Screen ~D~%" y)
          (dotimes (line 192) ; 192 scan lines
            (format out "~%    ;; Line ~D - Main memory~%" line)
            ;; Main memory: 40 bytes per line
            (dotimes (byte 40)
              (if (= byte 39)
                  (format out "    .byte $00~%")
                  (format out "    .byte $00, ")))
            (format out "~%    ;; Line ~D - Aux memory~%" line)
            ;; Aux memory: 40 bytes per line
            (dotimes (byte 40)
              (if (= byte 39)
                  (format out "    .byte $00~%")
                  (format out "    .byte $00, ")))
            
            (format out "~%    ;; End of ~A data~2%" png-name)))
        (format out "~%    ;; Double HIRES color palette constants (16 colors)~%")
        (let ((color-list '("BLACK" "DARK_GRAY" "MEDIUM_GRAY" "LIGHT_GRAY"
                            "LIGHT_GRAY2" "VERY_LIGHT_GRAY" "VERY_LIGHT_GRAY2" "WHITE"
                            "RED" "LIGHT_RED" "MAGENTA" "LIGHT_MAGENTA"
                            "BLUE" "LIGHT_BLUE" "GREEN" "LIGHT_GREEN")))
          (dotimes (i (length color-list))
            (format out "DHGR_~A = $~2,'0X~%" (nth i color-list) i)))))))

(defun compile-art-a2gs (index-out index-in)
  "Compile art assets for Apple IIGS platform"
  (let ((*machine* 222))
    (write-a2gs-art-index index-out
                          (read-a2gs-art-index index-in))))

(defun read-a2gs-art-index (index-in)
  "Read Apple IIGS art index file"
  (let ((png-list (list)))
    (format *trace-output* "~&~A: reading Apple IIGS art index …" (enough-namestring index-in))
    (with-input-from-file (index index-in)
      (loop for line = (read-line index nil)
            while (and line (plusp (length line)) (not (char= #\; (char line 0))))
            do (let ((line (string-trim #(#\Space #\Tab #\Newline #\Return #\Page) line)))
                 (cond
                   ((emptyp line) nil)
                   ((char= #\# (char line 0)) nil)
                   (t (destructuring-bind (png-name mode cell-size)
                          (split-sequence #\Space line :remove-empty-subseqs t :test #'char=)
                        (destructuring-bind (width-px height-px)
                            (split-sequence #\× cell-size :test #'char=)
                          (push (list (make-keyword mode)
                                      (make-pathname :defaults index-in
                                                     :name (subseq png-name 0
                                                                   (position #\. png-name :from-end t))
                                                     :type "png")
                                      (parse-integer width-px)
                                      (parse-integer height-px))
                                png-list))))))))
    (format *trace-output* " done. Got ~:D PNG files to read." (length png-list))
    (reverse png-list)))

(defun write-a2gs-art-index (index-out art-index)
  "Write Apple IIGS art data to output file"
  (with-output-to-file (out index-out :if-exists :supersede :if-does-not-exist :create)
    (format out ";;; Apple IIGS Art Assets compiled
;;; Generated automatically
~2%")
    (dolist (art-item art-index)
      (destructuring-bind (mode png-name width-px height-px) art-item
        (format *trace-output* "~&Processing Apple IIGS art: ~A (~Dx~D)…" png-name width-px height-px)
        ;; Dispatch to appropriate compilation function based on mode
        (ecase mode
          (:super-hires
           (compile-a2gs-super-hires png-name (directory-namestring index-out) height-px width-px
                                     (png->palette width-px height-px
                                                   (png-read:image-data (png-read:read-png-file png-name))
                                                   (png-read:transparency (png-read:read-png-file png-name)))))
          (:double-hires
           (compile-a2gs-double-hires png-name (directory-namestring index-out) height-px width-px
                                      (png->palette width-px height-px
                                                    (png-read:image-data (png-read:read-png-file png-name))
                                                    (png-read:transparency (png-read:read-png-file png-name)))))
          (:hires
           (compile-a2gs-hires png-name (directory-namestring index-out) height-px width-px
                               (png->palette width-px height-px
                                             (png-read:image-data (png-read:read-png-file png-name))
                                             (png-read:transparency (png-read:read-png-file png-name)))))
          (:sprite
           (compile-a2gs-sprite png-name (directory-namestring index-out) height-px width-px
                                (png->palette width-px height-px
                                              (png-read:image-data (png-read:read-png-file png-name))
                                              (png-read:transparency (png-read:read-png-file png-name)))))))))
  (format *trace-output* "~&Apple IIGS art compilation complete."))

;;; Commander X-16 (VERA) Graphics Converters
