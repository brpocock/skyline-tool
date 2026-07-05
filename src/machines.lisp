(in-package :skyline-tool)

(defun get-all-machines ()
  "Return a list of all supported machine identifiers for skyline-tool configuration."
  (list :ntsc :pal :secam))

(defun get-all-machine-names ()
  "Return a list of all supported machine names."
  (list "NTSC" "PAL" "SECAM"))