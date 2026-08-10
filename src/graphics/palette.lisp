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

;;; TMS9918A fixed 16-color palette (indices 0–15).  Same RGB values are used
;;; for tooling across ColecoVision (9918), SG-1000 (1000), SMS (3010), and
;;; Game Gear (837 / 2110) when mapping indexed PNG pixels — the VDP color
;;; numbers are hardware-defined (see Texas Instruments TMS9918A docs; widely
;;; reproduced on MSX/Coleco references).  Index 0 is transparent on hardware;
;;; we map it to black for nearest-color matching.
(define-constant +tms9918-palette+
    '((#x00 #x00 #x00) (#x00 #x00 #x00) (#x21 #xC8 #x42) (#x5E #xDC #x78)
      (#x54 #x55 #xED) (#x7D #x76 #xFC) (#xD4 #x52 #x4D) (#x42 #xEB #xF5)
      (#xFC #x55 #x55) (#xFF #x79 #x7C) (#xD4 #xC1 #x54) (#xE6 #xCE #x80)
      (#x21 #xB0 #x3B) (#xC9 #x5F #xF5) (#xBB #xBB #xBB) (#xFF #xFF #xFF))
  :test 'equalp
  :documentation "TMS9918A VDP 16-color RGB triples for MSX1-class fixed palette.

Used for ColecoVision / ClcV (9918), SG-1000 (1000), SMS (3010), and Game Gear
(837, 2110) graphics conversion where @code{machine-palette} must return the
canonical VDP colors.")

(define-constant +tms9918-color-names+
    '(transparency black medium-green light-green dark-blue light-blue dark-red
      cyan medium-red light-red dark-yellow light-yellow dark-green magenta gray
      white)
  :test 'equalp
  :documentation "Color names aligned with @ref{constant:+tms9918-palette+} indices.")

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
  (palette-for-machine-and-region machine region))

(defgeneric palette-for-machine-and-region (machine region)
  (:documentation
   "Get the standard color palette for a target machine and region.

Returns the appropriate color palette for the specified machine type and
video region, used for graphics conversion and color matching.

@table @asis
@item MACHINE
Machine identifier (default: current *machine*)
@item REGION
Video region (:ntsc, :pal, :secam) (default: current *region*).
@item Returns
List of RGB color triples for the machine's palette
@end table

@xref{var:*machine*}, @xref{var:*region*}.")
  (:method ((machine (eql 20)) region) (declare (ignore region))
    (subseq +c64-palette+ 0 7))
  (:method ((machine (eql 200)) region) (declare (ignore region))
    (copy-list +lynx-palette+))
  (:method ((machine (eql 64)) region) (declare (ignore region))
    (copy-list +c64-palette+))
  (:method ((machine (eql 128)) region) (declare (ignore region))
    (copy-list +c64-palette+))
  (:method ((machine (eql 2)) region) (declare (ignore region))
    (copy-list +apple-hires-palette+))
  (:method ((machine (eql 8)) region)
    (ecase region
      (:ntsc (copy-list +nes-palette-ntsc+))
      (:pal (copy-list +nes-palette-pal+))))
  (:method ((machine (eql 2600)) region)
    (ecase region
      (:ntsc (copy-list +vcs-ntsc-palette+))
      (:pal (copy-list +vcs-pal-palette+))
      (:secam (copy-list +vcs-secam-palette+))))
  (:method ((machine (eql 2609)) region) (declare (ignore region))
    (copy-list +intv-palette+))
  (:method ((machine (eql 7800)) region)
    (ecase region
      (:ntsc (copy-list +prosystem-ntsc-palette+))
      (:pal (copy-list +prosystem-pal-palette+))))
  (:method ((machine (eql 5200)) region)
    (ecase region
      (:ntsc (copy-list +prosystem-ntsc-palette+))
      (:pal (copy-list +prosystem-pal-palette+))))
  (:method ((machine (eql 400)) region)
    (ecase region
      (:ntsc (copy-list +prosystem-ntsc-palette+))
      (:pal (copy-list +prosystem-pal-palette+))))
  (:method ((machine (eql 800)) region)
    (ecase region
      (:ntsc (copy-list +prosystem-ntsc-palette+))
      (:pal (copy-list +prosystem-pal-palette+))))
  (:method ((machine (eql 7850)) region)
    (ecase region
      (:ntsc (copy-list +prosystem-ntsc-palette+))
      (:pal (copy-list +prosystem-pal-palette+))))
  (:method ((machine (eql 9918)) region) (declare (ignore region))
    (copy-list +tms9918-palette+))
  (:method ((machine (eql 3010)) region) (declare (ignore region))
    (copy-list +tms9918-palette+))
  (:method ((machine (eql 1000)) region) (declare (ignore region))
    (copy-list +tms9918-palette+))
  (:method ((machine (eql 837)) region) (declare (ignore region))
    (copy-list +tms9918-palette+))
  (:method ((machine (eql 2110)) region) (declare (ignore region))
    (copy-list +tms9918-palette+))
  (:method ((machine (eql 264)) region) (declare (ignore region))
    (copy-list +ted-palette+))
  (:method ((machine (eql 16)) region) (declare (ignore region))
    (copy-list +tg16-palette+)))

(defgeneric colors-for-machine-and-region (machine region)
  (:documentation
   "Get color names for a target machine and region.
Returns a list of color name strings corresponding to palette indices.")

  (:method ((machine (eql 20)) region) (declare (ignore region))
    (subseq +c64-names+ 0 7))
  (:method ((machine (eql 64)) region) (declare (ignore region))
    (copy-list +c64-names+))
  (:method ((machine (eql 128)) region) (declare (ignore region))
    (copy-list +c64-names+))
  (:method ((machine (eql 2609)) region) (declare (ignore region))
    (copy-list +intv-color-names+))
  (:method ((machine (eql 9918)) region) (declare (ignore region))
    (copy-list +tms9918-color-names+))
  (:method ((machine (eql 1000)) region) (declare (ignore region))
    (copy-list +tms9918-color-names+))
  (:method ((machine (eql 3010)) region) (declare (ignore region))
    (copy-list +tms9918-color-names+))
  (:method ((machine (eql 837)) region) (declare (ignore region))
    (copy-list +tms9918-color-names+))
  (:method ((machine (eql 2110)) region) (declare (ignore region))
    (copy-list +tms9918-color-names+))
  (:method ((machine (eql 2600)) (region (eql :ntsc)))
    (prosystem-ntsc-color-names))
  (:method ((machine (eql 2600)) (region (eql :pal)))
    (prosystem-pal-color-names))
  (:method ((machine (eql 400)) (region (eql :ntsc)))
    (prosystem-ntsc-color-names))
  (:method ((machine (eql 400)) (region (eql :pal)))
    (prosystem-pal-color-names))
  (:method ((machine (eql 800)) (region (eql :ntsc)))
    (prosystem-ntsc-color-names))
  (:method ((machine (eql 800)) (region (eql :pal)))
    (prosystem-pal-color-names))
  (:method ((machine (eql 5200)) (region (eql :ntsc)))
    (prosystem-ntsc-color-names))
  (:method ((machine (eql 5200)) (region (eql :pal)))
    (prosystem-pal-color-names))
  (:method ((machine (eql 7800)) (region (eql :ntsc)))
    (prosystem-ntsc-color-names))
  (:method ((machine (eql 7800)) (region (eql :pal)))
    (prosystem-pal-color-names))
  (:method ((machine (eql 7850)) (region (eql :ntsc)))
    (prosystem-ntsc-color-names))
  (:method ((machine (eql 7850)) (region (eql :pal)))
    (prosystem-pal-color-names))
  (:method ((machine (eql 2)) region) (declare (ignore region))
    (loop repeat 16 collect "Apple Monitor Color"))
  (:method ((machine (eql 264)) region) (declare (ignore region))
    (copy-list +ted-color-names+))
  (:method ((machine (eql 16)) region) (declare (ignore region))
    (copy-list +tg16-color-names+)))

(defun machine-colors ()
  "Get the color names for the current target machine.

Returns a list of color names corresponding to the palette indices for
the currently selected machine (*machine*).

@table @asis
@item Returns
List of color name strings for the current machine's palette
@item Depends on
* @var{*machine*} - current target machine
* @var{*region*} - :ntsc or :pal for Maria / ProSystem-class machines (400, 800, 5200, 7800, 7850).
For TMS9918-class machines (9918, 1000, 3010, 837, 2110), names match the fixed VDP palette regardless of REGION.
@end table

@xref{fun:machine-palette}, @xref{var:*machine*}, @xref{var:*region*}."
  (colors-for-machine-and-region *machine* *region*))

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
  (mapcar (lambda (n) (coerce n 'single-float))
          (nth index (machine-palette))))

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
        (check-type r (integer 0 #xff))
        (check-type g (integer 0 #xff))
        (check-type b (integer 0 #xff))
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

(defun ansi-color-pixel (r g b)
  "Return an ANSI color escape sequence for the given RGB components.
The escape sequence resets after the character."
  (format nil "~c[38;2;~d;~d;~dm" #\Escape (round r) (round g) (round b)))

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

(defun png->palette (rgb &optional α)
  (check-type rgb array)
  (check-type α (or null array))
  (destructuring-bind (width height bpp) (array-dimensions rgb)
    (unless (or (= bpp 3) (= bpp 4))
      (error "PNG image must have 3 or 4 bytes per pixel (RGB/RGBA), but got ~D" bpp))
    (let ((image (make-array (list width height)
                             :element-type '(or null (unsigned-byte 8)))))
      (loop for x from 0 below width
            do (loop for y from 0 below height
                     do (setf (aref image x y)
                              (if (or (and α (>= 128 (aref α x y)))
                                      (and (= bpp 4) (>= 128 (aref rgb x y 3))))
                                  nil
                                  (rgb->palette (aref rgb x y 0)
                                                (aref rgb x y 1)
                                                (aref rgb x y 2))))))
      image)))

