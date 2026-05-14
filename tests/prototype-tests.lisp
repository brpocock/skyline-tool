(in-package :skyline-tool/test)

(def-suite prototype-tests
  :description "JSON object prototype compiler and map spawn encoding")

(in-suite prototype-tests)

(test encode-map-spawn-entry-character
  "Character spawn encodes kind 0 with id in low byte"
  (is (equalp '(10 20 0 42 0)
              (skyline-tool::encode-map-spawn-entry 10 20 :character 42))))

(test encode-map-spawn-entry-object
  "Object spawn encodes kind 1 with 16-bit prototype index"
  (is (equalp '(3 4 1 5 1)
              (skyline-tool::encode-map-spawn-entry 3 4 :object #x105))))

(test read-object-prototype-json-parses-class
  "JSON reader extracts Class and field keys"
  (skyline-tool/test::with-temp-file (file "prototype" "json")
    (with-open-file (out file :direction :output :if-exists :supersede)
      (write-string "{\"Class\": \"BasicEnemy\", \"HP\": 10}" out))
    (let ((proto (skyline-tool::read-object-prototype-json file)))
      (is (string= "BasicEnemy" (getf proto :class)))
      (is (eql 10 (getf proto :hp))))))

(defun run-prototype-tests ()
  "Run prototype compiler unit tests."
  (fiveam:run! 'prototype-tests))
