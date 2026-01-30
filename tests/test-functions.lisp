;; Simple test to check if our new functions work
(require 'asdf)
(load "setup.lisp")
(asdf:load-system :skyline-tool)

;; Test speech compilation
(format t "~&Testing speech compilation...~%")
(handler-case
    (let ((output-file "/tmp/test-speech.s")
          (input-file "/tmp/test-speech.txt"))
      ;; Create test input
      (with-open-file (out input-file :direction :output :if-exists :supersede)
        (write-line "Hello world" out))
      ;; Test compilation
      (skyline-tool::compile-speech-2609 output-file input-file)
      (format t "Speech compilation: SUCCESS~%"))
  (error (e) (format t "Speech compilation: FAILED - ~A~%" e)))

;; Test art compilation
(format t "~&Testing art compilation...~%")
(handler-case
    (let ((output-file "/tmp/test-art.s")
          (index-file "/tmp/test-art.index"))
      ;; Create test index
      (with-open-file (out index-file :direction :output :if-exists :supersede)
        (write-line "test.png 8×8" out))
      ;; Test compilation
      (skyline-tool::compile-art-intv output-file index-file)
      (format t "Art compilation: SUCCESS~%"))
  (error (e) (format t "Art compilation: FAILED - ~A~%" e)))

;; Test ROM assembly
(format t "~&Testing ROM assembly...~%")
(handler-case
    (let ((output-file "/tmp/test-rom.bin")
          (source-files '("/tmp/test-rom.s")))
      ;; Create test source
      (with-open-file (out "/tmp/test-rom.s" :direction :output :if-exists :supersede)
        (write-line "DECLE $1234" out))
      ;; Test assembly
      (skyline-tool::assemble-intv-rom source-files output-file)
      (format t "ROM assembly: SUCCESS~%"))
  (error (e) (format t "ROM assembly: FAILED - ~A~%" e)))

(format t "~&All tests completed.~%")
