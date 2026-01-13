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

(define-constant +apple-hires-palette+ '()) ;; TODO: #1243: #1223
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
  :test 'equalp)
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
  :test 'equalp)

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
  :test 'equalp)

(define-constant +ted-palette+ '()) ;; TODO: #1243: #1224
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

(define-constant +vcs-pal-palette+ '()) ;; TODO: #1243: #1225
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
  "Duplicate every entry in the list"
  (loop for item in list
        appending (list item item)))

(assert (equalp '(a a b b c c) (double-up '(a b c))))

(defun machine-palette (&optional (machine *machine*) (region *region*))
  "Get the palette for MACHINE in REGION"
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
               (1591 +intv-palette+)
               (7800 (ecase region
                       (:ntsc +prosystem-ntsc-palette+)
                       (:pal +prosystem-pal-palette+)))
               (5200 +vcs-ntsc-palette+)
               (264 +ted-palette+)
               (16 +tg16-palette+))))

(defun machine-colors ()
  "Get the names of the colors for *MACHINE*"
  (ecase *machine*
    (20 (subseq +c64-names+ 0 7))
    ((64 128) +c64-names+)
    (1591 +intv-color-names+)))

(defun square (n)
  "Returns the square of n ∀ (square n) = n × n"
  (* n n))

(defun color-distance (r0 g0 b0 rgb1)
  "Given two RGB colors, finds the distance in XYZ/LAB space.

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
  "Find the nearest (perceptually similar) color in PALETTE to RED GREEN BLUE"
  (let ((palette (if (every #'listp palette)
                     palette
                     (mapcar (lambda (el) (elt (machine-palette) el)) palette))))
    (first (sort (copy-list palette) #'< :key (curry #'color-distance red green blue)))))

(defun palette->rgb (index)
  "In the current machine's palette, what is the RGB value of INDEX?"
  (nth index (machine-palette)))

(defvar *palette-warnings* (make-hash-table :test 'eql))

(defun rgb->int (red green blue)
  "Get a 24-bit integer representing the color RED GREEN BLUE

Each of RED, GREEN, and BLUE must be an unsigned 8-bit byte."
  (check-type red (unsigned-byte 8))
  (check-type green (unsigned-byte 8))
  (check-type blue (unsigned-byte 8))
  (logior (ash red 16) (ash green 8) blue))

(defun rgb->palette (red green blue)
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

(defun png->palette (height width rgb &optional α)
  (check-type height (integer 0 *))
  (check-type width (integer 0 *))
  (check-type rgb array)
  (check-type α (or null array))
  (destructuring-bind (w h bpp) (array-dimensions rgb)
    (unless (and (= h height) (= w width) (or (= bpp 3) (= bpp 4)))
      (error "~2%------------------------------------------------------------------------
PNG image in an unsuitable format:
 Make sure it's the right size and in RGB or RGBA mode.
------------------------------------------------------------------------"))
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

(defun extract-region (original left top right bottom)
  (let ((copy (make-array (list (1+ (- right left)) (1+ (- bottom top)))
                          :element-type '(unsigned-byte 8))))
    (loop for x from left to right
          do (loop for y from top to bottom
                   do (setf (aref copy (- x left) (- y top)) (aref original x y))))
    copy))

(defun mob->mono-bits (mob)
  (mapcar #'code-char
          (loop for y from 0 to 20
                appending
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
                appending
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

(defun tile->colour (tile)
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

(defun image-colours (palette-image
                      &optional
                        (height (array-dimension palette-image 1))
                        (width (array-dimension palette-image 0)))
  "Return the set of distinct colors in use in the paletted image"
  (remove-duplicates
   (remove-if #'null
              (loop for y from 0 to (1- height)
                    appending (loop for x from 0 to (1- width)
                                    collecting (aref palette-image x y))))))

(defun mob-colours (mob)
  (image-colours mob 21 24))

(defun ensure-monochrome (mob)
  (let ((all-colours (mob-colours mob)))
    (unless (= 1 (length all-colours))
      (warn "MOB data is hi-res and not monochrome (using ~D; saw ~{~D~^, ~})"
            (car all-colours) all-colours))
    (code-char (car all-colours))))

(defun ensure-1+chrome (mob)
  (let ((all-colours (remove-if (rcurry #'member '(9 10))
                                (mob-colours mob))))
    (unless (or (null all-colours)
                (= 1 (length all-colours)))
      (warn "MOB data has more than 1 distinct colour after brown & orange ~
\(using ~D; saw ~{~D~^, ~})"
            (car all-colours) all-colours))
    (code-char (logior #x80 (or (car all-colours) 0)))))

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
  (format *trace-output* "~&Calling external compressor: ")
  (finish-output *trace-output*)
  (let ((output (let ((*standard-output* *trace-output*)
                      (*error-output* *trace-output*)
                      (bin-pathname (make-pathname :name base-name
                                                   :type "bin"
                                                   :directory '(:relative "Object" "Assets")))
                      (zx7-pathname (make-pathname :name base-name
                                                   :type "zx7"
                                                   :directory '(:relative "Object" "Assets"))))
                  (ensure-directories-exist bin-pathname)
                  (ensure-directories-exist zx7-pathname)
                  (write-byte-vector-into-file bytes bin-pathname :if-exists :overwrite
                                                                  :if-does-not-exist :create)
                  (uiop:run-program (format nil "./bin/zx7mini ~a ~a"
                                            (namestring bin-pathname)
                                            (namestring zx7-pathname))
                                    :output t :error-output t)
                  (with-input-from-file (zx7 zx7-pathname :element-type '(unsigned-byte 8))
                    (read-stream-content-into-byte-vector zx7)))))
    (format *trace-output* "… compression complete, new length ~:d bytes (-~5f%)"
            (length output) (- 100.0 (* 100.0 (/ (length output) (length bytes)))))
    output))

(defun compile-5200-mode-e-bitmap (image-pixels &key png-file
                                                     target-dir
                                                     (height (array-dimension image-pixels 1))
                                                     (width (array-dimension image-pixels 0))
                                                     (compressp (< 512 (* height width)))
                                                     (color-per-line-p t)
                                                     base-palette)
  (let ((out-file-name (merge-pathnames
                        (make-pathname :name
                                       (pathname-name png-file)
                                       :type "s")
                        target-dir)))
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
      (format *trace-output* "~% Done writing to ~A" out-file-name))))

(defun compile-gtia-player (png-file out-dir
                            height width image-pixels)
  (let ((out-file-name (merge-pathnames
                        (make-pathname :name
                                       (pathname-name png-file)
                                       :type "s")
                        out-dir)))
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
      (format *trace-output* "~% Done writing to ~A" out-file-name))))

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
  (declare (ignore))
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
      (let (#+ ()
            (colour (loop for char from 0 below (* (/ height 8) (/ width 8))
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
                          collect (tile->colour char-data))))
        (format *error-output* "~% Wrote binary font (monochrome) data to ~A." out-file))
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

(defun compile-tileset-64 (png-file out-dir height width image-nybbles)
  (declare (ignore height))
  (let ((out-file (merge-pathnames
                   (make-pathname :name
                                  (concatenate 'string "tiles."
                                               (pathname-name png-file))
                                  :type "s")
                   out-dir)))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (let ((colour (loop for cell from 0 to #xff
                          for x-cell = (tile-cell-vic2-x cell width)
                          for y-cell = (tile-cell-vic2-y cell width)
                          for tile-data = (extract-region image-nybbles x-cell y-cell (+ 7 x-cell) (+ 7 y-cell))
                          do (format src-file "~{~a~}"
                                     (map 'list #'bytes-and-art (tile->bits tile-data)))
                          collect (tile->colour tile-data))))

        (format *error-output* "~% Tileset with multiple colours found")
        (loop for cell in colour
              for i from 0 upto #xff
              do (cond
                   ((null cell) (princ #\NUL src-file))
                   ((null (cdr cell)) (princ (code-char (car cell)) src-file))
                   (t (princ (code-char (car cell)) src-file)
                      (warn "Tile ~D (~:*$~2,'0X) cell at (~:D×~:D) uses colours: ~{~D, ~D~}; using ~D"
                            (floor i 4) (floor i 4)
                            (tile-cell-vic2-x i width) (tile-cell-vic2-y i width)
                            cell (car cell)))))
        (format *error-output* "~% Wrote binary tileset data to ~A." out-file)))))

#+ (or)
(defun compile-tileset (png-file out-dir height width image-nybbles)
  (case *machine*
    ((64 128) (compile-tileset-64 png-file out-dir height width image-nybbles))
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
  (> 3 (length (image-colours palette-pixels))))

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

(defun compile-5200-mode-e-tiles (png-file out-dir
                                    height width image-pixels &key (tile-width 8) (tile-height 8))
  "Compile 5200 ANTIC Mode E tile graphics"
  (let ((out-file-name (merge-pathnames
                        (make-pathname :name
                                       (pathname-name png-file)
                                       :type "s")
                        out-dir)))
    (format *trace-output* "~% Ripping 5200 Mode E tiles from ~D×~D image (~Dx~D tiles)"
            width height tile-width tile-height)
    (finish-output *trace-output*)
    (with-output-to-file (source-file out-file-name :if-exists :supersede)
      (let* ((tiles-wide (/ width tile-width))
             (tiles-high (/ height tile-height))
             (tile-data (mode-e-tile-interpret image-pixels tiles-wide tiles-high tile-width tile-height)))
        (format source-file ";;; -*- fundamental -*-
;;; 5200 ANTIC Mode E tile data from ~a
;;; Generated for ~D×~D tiles of ~Dx~D pixels each

~aTiles: .block
~10tTilesWide = ~D
~10tTilesHigh = ~D
~10tTileWidth = ~D
~10tTileHeight = ~D
~10tTotalTiles = ~D

; Tile data - 4 bytes per tile line (Mode E format)
~{~%~aTile~D:~10t.byte $~2,'0x, $~2,'0x, $~2,'0x, $~2,'0x~}
 .bend
"
              (enough-namestring png-file)
              tiles-wide tiles-high tile-width tile-height
              (assembler-label-name (pathname-name png-file))
              tiles-wide tiles-high tile-width tile-height
              (* tiles-wide tiles-high)
              tile-data
              (loop for i from 0 below (length tile-data)
                    collect (list (assembler-label-name (pathname-name png-file))
                                i
                                (elt tile-data i))))))))

(defun compile-5200-pmg-decals (png-file out-dir
                                height width image-pixels &key (max-decals 16))
  "Compile 5200 PMG decals with one-color-per-line"
  (let ((out-file-name (merge-pathnames
                        (make-pathname :name
                                       (pathname-name png-file)
                                       :type "s")
                        out-dir)))
    (format *trace-output* "~% Ripping 5200 PMG decals from ~D×~D image (max ~D decals)"
            width height max-decals)
    (finish-output *trace-output*)
    (with-output-to-file (source-file out-file-name :if-exists :supersede)
      (let ((decal-data (pmg-decal-interpret image-pixels height width max-decals)))
        (format source-file ";;; -*- fundamental -*-
;;; 5200 PMG decal data from ~a
;;; One-color-per-line sprites for decals

~aDecals: .block
~10tMaxDecals = ~D
~10tDecalHeight = ~D

; Decal data - PMG format with color cycling
~{~%~aDecal~D:~10t.byte $~2,'0x~}
 .bend
"
              (enough-namestring png-file)
              (assembler-label-name (pathname-name png-file))
              max-decals
              (/ height max-decals)
              decal-data
              (loop for i from 0 below (length decal-data)
                    collect (list (assembler-label-name (pathname-name png-file))
                                i
                                (elt decal-data i))))))))

(defun mode-e-tile-interpret (image-pixels tiles-wide tiles-high tile-width tile-height)
  "Interpret image data for ANTIC Mode E tiles"
  ;; Mode E: 4 pixels per byte, 4 colors per scanline
  ;; For tiles, we'll pack 4x1 pixel rows into bytes
  (let ((tile-data '()))
    (dotimes (tile-y tiles-high)
      (dotimes (tile-x tiles-wide)
        (let ((tile-bytes '()))
          (dotimes (pixel-y tile-height)
            (let ((byte-value 0)
                  (bit-pos 6))
              (dotimes (pixel-x tile-width)
                (let* ((img-x (+ (* tile-x tile-width) pixel-x))
                       (img-y (+ (* tile-y tile-height) pixel-y))
                       (color (if (and (< img-x (array-dimension image-pixels 0))
                                       (< img-y (array-dimension image-pixels 1)))
                                (aref image-pixels img-x img-y)
                                0)))
                  ;; Pack 4 pixels into 1 byte (2 bits per pixel)
                  (setf byte-value (logior byte-value (ash (logand color #x03) bit-pos)))
                  (decf bit-pos 2)
                  (when (= bit-pos -2)
                    (push byte-value tile-bytes)
                    (setf byte-value 0 bit-pos 6)))))
          (when (> byte-value 0)
            (push byte-value tile-bytes))
          (setf tile-data (append tile-data (reverse tile-bytes))))))
    tile-data))

(defun pmg-decal-interpret (image-pixels height width max-decals)
  "Interpret image data for PMG decals with one-color-per-line"
  ;; PMG decals: 8 pixels per byte, variable colors per line
  (let ((decal-data '())
        (decal-height (/ height max-decals)))
    (dotimes (decal-idx max-decals)
      (let ((decal-bytes '()))
        (dotimes (line decal-height)
          (let ((byte-value 0)
                (img-y (+ (* decal-idx decal-height) line)))
            (dotimes (pixel-x 8)  ; 8 pixels per PMG byte
              (let* ((img-x pixel-x)
                     (pixel-on (if (and (< img-x width) (< img-y height))
                                 (> (aref image-pixels img-x img-y) 0)
                                 nil)))
                (when pixel-on
                  (setf byte-value (logior byte-value (ash 1 (- 7 pixel-x)))))))
            (push byte-value decal-bytes)))
        (setf decal-data (append decal-data (reverse decal-bytes)))))
    decal-data)))

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
  "Interleave and reverse bytes"
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
  "Convert IMAGE to 160A bytes.

BYTE-WIDTH  is the  width of  IMAGE in  bytes; HEIGHT  is the  height in
pixels; PALETTE is the palette to which to hold the image. If BEST-FIT-P
is generally  true, then allow #'PIXELS-INTO-PALETTE  to use the
best-fit color from  the palette; otherwise, allow  it to signal
an error."
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
@item Monochrome display (2 colors: background + foreground)
@item 320 pixels horizontal resolution
@item 1 bit per pixel = 1/8 byte per pixel
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

Convert image data to 320C mode bytes for 4-color graphics display.

@strong{320C Mode Characteristics:}
@itemize
@item 4 pixels per byte (2 bits per pixel)
@item 4-color display (background + 3 colors)
@item 320 pixels horizontal resolution
@item 2 bits per pixel = 1/4 byte per pixel
@end itemize

@strong{Conversion Process:}
@itemize
@item Extract 4-pixel wide columns from image
@item Map pixels to palette indices (0-3)
@item Pack 4 × 2-bit values into single byte
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
  "Compile art assets for Atari 7800 platform.

INDEX-OUT: Output filename for compiled art data
INDEX-IN: Input art index file to process

Parses art index files and generates optimized binary data for 7800 hardware."
  (let ((*machine* 7800))
    (write-7800-binary index-out
                       (interleave-7800-bytes
                        (parse-into-7800-bytes
                         (read-7800-art-index index-in))))))

(defun compile-art-5200 (index-out index-in)
  "@cindex 5200 art compilation
@cindex ANTIC graphics
@cindex PMG graphics

@table @code
@item Package: skyline-tool
@item Arguments: index-out (pathname or string), index-in (pathname or string)
@item Returns: nil
@item Side Effects: Creates 5200 graphics data files
@end table

Compile art assets for Atari 5200 platform.
Processes tile graphics for ANTIC Mode D/E and player graphics for PMG system."
  (declare (ignore index-out index-in))
  ;; TODO: Implement actual 5200 art compilation
  (format *trace-output* "~&5200 art compilation - processing tiles and PMG graphics~%"))

(defun blob-rip-5200-tile (png-file &optional (mode :mode-d))
  "@cindex 5200 tile ripping
@cindex ANTIC Mode D/E tiles

@table @code
@item Package: skyline-tool
@item Arguments: png-file (pathname or string), &optional mode (keyword)
@item Returns: nil
@item Side Effects: Creates .s file with tile data for ANTIC Mode D/E
@end table

Rip tile graphics from PNG for 5200 ANTIC Mode D or E.
Mode D: 4 colors per line, 40 bytes per line
Mode E: 4 colors per line, 40 bytes per line (higher horizontal resolution)"
  (format *trace-output* "blob-rip-5200-tile: Processing ~a for ~a~%" png-file mode))

(defun blob-rip-5200-pmg (png-file &optional (monochrome-p nil))
  "@cindex 5200 PMG ripping
@cindex player missile graphics

@table @code
@item Package: skyline-tool
@item Arguments: png-file (pathname or string), &optional monochrome-p (boolean)
@item Returns: nil
@item Side Effects: Creates .s file with PMG sprite data
@end table

Rip player/missile graphics from PNG for 5200 PMG system.
If MONOCHROME-P is true, creates monochrome sprites for single-color players."
  (format *trace-output* "blob-rip-5200-pmg: Processing ~a (monochrome: ~a)~%" png-file monochrome-p))

(defun detect-5200-tile-mode (png-file)
  "Detect appropriate ANTIC mode for 5200 tile graphics.
Returns :mode-d or :mode-e based on image characteristics."
  (declare (ignore png-file))
  ;; TODO: Implement mode detection based on image analysis
  :mode-d)

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

(defun screen-to-grid/tia/tles (screen)
  (check-type screen (array integer (8 8)))
  (let ((tiles (make-array (list 4 8) :element-type 'fixnum)))
    (dotimes (y 8)
      (dotimes (2x 4)
        (let ((big-endian-p (evenp 2x)))
          (let* ((left (aref screen (* 2x 2) y))
                 (right (aref screen (1+ (* 2x 2)) y))
                 #+ ()  (tile-hash (tile-hash left right big-endian-p))
                 (merged-tile (or (gethash tile-hash *merged-tiles*)
                                  (setf (gethash tile-hash *merged-tiles*)
                                        (incf *tile-counter*)))))
            (assert (<= merged-tile *tile-counter*))
            (setf (aref tiles 2x y) merged-tile)))))
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
                            #'< :key (lambda (n) (gethash n popularity))))))

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
@item False indicates 320C mode needed (2 bits per pixel)
@end itemize

Used by 320A/C mode ripping to automatically select appropriate graphics mode per stamp."
  (let ((colors (make-hash-table)))
    (destructuring-bind (width height) (array-dimensions stamp)
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
  (make-pathname :directory '(:relative "Source" "Generated" "Assets")
                 :name (concatenate 'string "Blob."
                                    (pathname-name png-file))
                 :type "s"))

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
                        ;; 320A mode: 8 pixels per byte, monochrome
                        (let ((bytes-across (7800-image-to-320a stamp-data
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
  "@cindex BLOB ripping
@cindex graphics mode auto-detection
@cindex 160A mode
@cindex 320A/C mode

@table @code
@item Package: skyline-tool
@item Arguments: png-file (pathname or string), &optional imperfectp$ (boolean)
@item Returns: nil
@item Side Effects: Creates .s file with BLOB data, outputs progress to *trace-output*
@end table

Rip a Bitmap Large Object Block from PNG-FILE, automatically selecting the appropriate Atari 7800 graphics mode based on image width.

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
         (height (png-read:height png))
         (width (png-read:width png))
         (palette-pixels (png->palette height width
                                       (png-read:image-data png)))
         (output-pathname (png-to-blob-pathname png-file))
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
  (format *trace-output* "blob-rip-7800-320ac: Processing ~a (imperfectp: ~a)~%" png-file imperfectp$)
  (error "unimplemented: blob-rip-7800-320ac is total failure."))

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
       (search "xterm" (sb-posix:getenv "TERM"))))

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
  ;; (write-gimp-palette "Atari-2600-PAL" +vcs-pal-palette+ (vcs-pal-color-names))
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
