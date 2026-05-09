;;; Phantasia SkylineTool/tests/cbm-tooling-tests.lisp
;;;; Regression tests for CBM PETSCII doc lines and GEOS stub sectors.
;;;; Copyright © 2026 Interworldly Adventuring, LLC.

(in-package :skyline-tool/test)

(fiveam:def-suite cbm-tooling-tests
  :description "Skyline-Tool CBM PETSCII documentation and GEOS VLIR stub helpers"
  :in skyline-tool/test)

(in-suite cbm-tooling-tests)

(test petscii-doc-line-uppercase-mapping
  "PETSCII doc line maps ASCII letters to unshifted PETSCII and ends with CR."
  (let ((v (skyline-tool::%ascii-doc-line-to-petscii-seq-bytes "Ab")))
    (is (= 3 (length v)))
    (is (= #xc1 (aref v 0)))
    (is (= #xc2 (aref v 1)))
    (is (= #x0d (aref v 2)))))

(test geos-record-index-placeholder
  "GEOS RECORD index sector begins with marker and T/S placeholder bytes."
  (let ((v (skyline-tool::%build-geos-record-index-sector 1 16)))
    (is (= 256 (length v)))
    (is (= #x00 (aref v 0)))
    (is (= #xff (aref v 1)))
    (is (= 1 (aref v 2)))
    (is (= 16 (aref v 3)))))
