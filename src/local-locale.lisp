;;; Local locale implementation for number formatting

(in-package :skyline-tool)

(defstruct (locale (:constructor make-locale (language decimal-separator grouping grouping-separator)))
  "Locale structure for number formatting"
  (language "en_US" :type string)
  (decimal-separator #\. :type character)
  (grouping 3 :type integer)
  (grouping-separator #\, :type character))
