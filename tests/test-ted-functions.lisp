;; Simple test to check if TED functions work
(require 'asdf)
(load "setup.lisp")

;; Test TED palette
(format t "Testing TED palette...~%")
(let ((palette (symbol-value (find-symbol "+TED-PALETTE+" :skyline-tool))))
  (if (and palette (= (length palette) 16))
      (format t "TED palette: OK (~D colors)~%" (length palette))
      (format t "TED palette: FAILED~%")))

;; Test TED color names
(format t "Testing TED color names...~%")
(let ((color-names (symbol-value (find-symbol "+TED-COLOR-NAMES+" :skyline-tool))))
  (if (and color-names (= (length color-names) 16))
      (format t "TED color names: OK (~D names)~%" (length color-names))
      (format t "TED color names: FAILED~%")))

;; Test function existence
(format t "Testing function existence...~%")
(let ((functions '(compile-art-264 compile-ted-bitmap compile-ted-sprite
                  blob-rip-264-bitmap blob-rip-264-sprite)))
  (dolist (func-name functions)
    (let ((func (find-symbol (string-upcase (string func-name)) :skyline-tool)))
      (if (and func (fboundp func))
          (format t "~A: OK~%" func-name)
          (format t "~A: FAILED~%" func-name)))))

(format t "~&TED implementation test complete.~%")
(sb-ext:exit)
