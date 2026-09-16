;;; Test for string-slot-p
(in-package :skyline-tool/test)

(def-suite string-slot-p-tests
  :description "Tests for the string-slot-p function"
  :in skyline-tool/test)

(in-suite string-slot-p-tests)

(test string-slot-p-success
  "string-slot-p returns T for string slots"
  (let ((*classes-defs-cache* nil)
        (*classes-defs-timestamp* 0))
    ;; set up a fake class with slot that has type :string
    (setf (gethash :TestClass *classes-defs-cache*)
          '((:Name . (0 . :string))))
    (signals nil (string-slot-p :TestClass :Name))
    (is (string-slot-p :TestClass :Name) "Should be T for string slot")))