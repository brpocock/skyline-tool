(defpackage :skyline-tool/forth-compilation-test
  (:use :cl :fiveam)
  (:import-from :skyline-tool
                :compile-forth-6502
                :compile-forth-z80
                :compile-forth-cp1610
                :forth-compilation-error
                :forth-syntax-error
                :forth-undefined-word)
  (:export :forth-compilation-suite))

(in-package :skyline-tool/forth-compilation-test)

(def-suite forth-compilation-suite
  :description "Comprehensive test suite for Forth bytecode compilation across all platforms")

(in-suite forth-compilation-suite)

;; ============================================================================
;; VALID INPUT TESTS
;; ============================================================================

(test forth-6502-basic-compilation
  "Test basic 6502 Forth compilation with valid input"
  (with-fixture test-forth-compilation-context ()
    ;; Create a simple Forth program
    (let ((forth-file (merge-pathnames "simple-6502.forth" temp-dir)))
      (with-open-file (f forth-file :direction :output :if-exists :supersede)
        (format f "42 dup +~%"))
      ;; Compile it
      (let ((asm-file (merge-pathnames "output-6502.s" temp-dir)))
        (finishes (compile-forth-6502 forth-file asm-file))
        ;; Check that output file was created and contains expected content
        (is-true (probe-file asm-file))
        (with-open-file (f asm-file :direction :input)
          (let ((content (read-line f)))
            (is (search "6502 Forth bytecode compilation" content))))))))

(test forth-z80-basic-compilation
  "Test basic Z80 Forth compilation with valid input"
  (with-fixture test-forth-compilation-context ()
    (let ((forth-file (merge-pathnames "simple-z80.forth" temp-dir)))
      (with-open-file (f forth-file :direction :output :if-exists :supersede)
        (format f "42 dup +~%"))
      (let ((asm-file (merge-pathnames "output-z80.asm" temp-dir)))
        (finishes (compile-forth-z80 forth-file asm-file))
        (is-true (probe-file asm-file)))))))

(test forth-cp1610-basic-compilation
  "Test basic CP1610 Forth compilation with valid input"
  (with-fixture test-forth-compilation-context ()
    (let ((forth-file (merge-pathnames "simple-cp1610.forth" temp-dir)))
      (with-open-file (f forth-file :direction :output :if-exists :supersede)
        (format f "42 dup +~%"))
      (let ((asm-file (merge-pathnames "output-cp1610.s" temp-dir)))
        (finishes (compile-forth-cp1610 forth-file asm-file))
        (is-true (probe-file asm-file)))))))

;; ============================================================================
;; INVALID INPUT TESTS - FILE ERRORS
;; ============================================================================

(test forth-compilation-file-not-found
  "Test that compilation fails gracefully when input file doesn't exist"
  (let ((nonexistent-file (make-pathname :name "does-not-exist" :type "forth"))
        (output-file (make-pathname :name "output" :type "s")))
    (signals forth-compilation-error
      (compile-forth-6502 nonexistent-file output-file))))

(test forth-compilation-output-directory-error
  "Test that compilation fails when output directory doesn't exist"
  (with-fixture test-forth-compilation-context ()
    (let ((forth-file (merge-pathnames "simple.forth" temp-dir))
          (bad-output-file (make-pathname :directory '(:absolute "nonexistent" "directory")
                                          :name "output" :type "s")))
      (with-open-file (f forth-file :direction :output :if-exists :supersede)
        (format f "42~%"))
      (signals forth-compilation-error
        (compile-forth-6502 forth-file bad-output-file)))))

;; ============================================================================
;; INVALID INPUT TESTS - SYNTAX ERRORS
;; ============================================================================

(test forth-6502-undefined-word
  "Test that 6502 compilation reports undefined words clearly"
  (with-fixture test-forth-compilation-context ()
    (let ((forth-file (merge-pathnames "undefined-word.forth" temp-dir)))
      (with-open-file (f forth-file :direction :output :if-exists :supersede)
        (format f "42 unknown-word~%"))
      (let ((asm-file (merge-pathnames "output.s" temp-dir)))
        (signals forth-undefined-word
          (compile-forth-6502 forth-file asm-file))))))

(test forth-6502-literal-too-large
  "Test that 6502 compilation rejects literals too large for 16-bit"
  (with-fixture test-forth-compilation-context ()
    (let ((forth-file (merge-pathnames "too-large-literal.forth" temp-dir)))
      (with-open-file (f forth-file :direction :output :if-exists :supersede)
        (format f "100000~%")) ; 100000 > 65535
      (let ((asm-file (merge-pathnames "output.s" temp-dir)))
        (signals forth-syntax-error
          (compile-forth-6502 forth-file asm-file))))))

;; ============================================================================
;; ERROR OBJECT VALIDATION TESTS
;; ============================================================================

(test forth-error-objects-have-proper-accessors
  "Test that Forth error objects have proper accessors and print nicely"
  (with-fixture test-forth-compilation-context ()
    ;; Test forth-undefined-word error
    (let ((error-obj (make-condition 'forth-undefined-word
                                     :forth-file "test.forth"
                                     :output-file "output.s"
                                     :architecture :6502
                                     :context "line 3"
                                     :word-name "badword"
                                     :available-words '("dup" "drop" "+" "numbers"))))
      (is (string= "test.forth" (forth-compilation-error-forth-file error-obj)))
      (is (string= "output.s" (forth-compilation-error-output-file error-obj)))
      (is (eql :6502 (forth-compilation-error-architecture error-obj)))
      (is (string= "line 3" (forth-compilation-error-context error-obj)))
      (is (string= "badword" (forth-undefined-word-word-name error-obj)))
      (is (equal '("dup" "drop" "+" "numbers") (forth-undefined-word-available-words error-obj)))
      ;; Test that print-object works
      (let ((output (with-output-to-string (s)
                      (print-object error-obj s))))
        (is (search "Undefined Forth word" output))
        (is (search "badword" output))
        (is (search "Available words:" output)))))

    ;; Test forth-syntax-error
    (let ((error-obj (make-condition 'forth-syntax-error
                                     :forth-file "syntax.forth"
                                     :output-file "output.asm"
                                     :architecture :z80
                                     :context "line 5"
                                     :invalid-token "999999"
                                     :expected-tokens '("16-bit value"))))
      (is (string= "999999" (forth-syntax-error-invalid-token error-obj)))
      (is (equal '("16-bit value") (forth-syntax-error-expected-tokens error-obj)))
      (let ((output (with-output-to-string (s)
                      (print-object error-obj s))))
        (is (search "Forth syntax error" output))
        (is (search "999999" output))))))

;; ============================================================================
;; CROSS-PLATFORM COMPILATION TESTS
;; ============================================================================

(test forth-cross-platform-consistency
  "Test that the same Forth program compiles successfully on all platforms"
  (with-fixture test-forth-compilation-context ()
    (let ((forth-program "1 2 + dup drop")
          (base-name "cross-platform"))
      (dolist (arch '(:6502 :z80 :cp1610))
        (let ((forth-file (merge-pathnames (format nil "~A-~A.forth" base-name arch) temp-dir)))
          (with-open-file (f forth-file :direction :output :if-exists :supersede)
            (format f "~A~%" forth-program))
          (let ((asm-file (merge-pathnames (format nil "~A-~A.s" base-name arch) temp-dir)))
            (ecase arch
              (:6502 (finishes (compile-forth-6502 forth-file asm-file)))
              (:z80 (finishes (compile-forth-z80 forth-file asm-file)))
              (:cp1610 (finishes (compile-forth-cp1610 forth-file asm-file))))
            (is-true (probe-file asm-file))))))))

;; ============================================================================
;; PERFORMANCE TESTS
;; ============================================================================

(test forth-compilation-performance
  "Test that Forth compilation completes within reasonable time limits"
  (with-fixture test-forth-compilation-context ()
    ;; Create a moderately complex Forth program
    (let ((forth-file (merge-pathnames "performance-test.forth" temp-dir)))
      (with-open-file (f forth-file :direction :output :if-exists :supersede)
        (dotimes (i 50)
          (format f "~D ~D + dup drop~%" i (random 1000))))
      ;; Test 6502 compilation performance
      (let ((asm-file (merge-pathnames "perf-6502.s" temp-dir))
            (start-time (get-internal-real-time)))
        (compile-forth-6502 forth-file asm-file)
        (let ((end-time (get-internal-real-time)))
          (is (< (/ (- end-time start-time) internal-time-units-per-second) 2.0))))))) ; Less than 2 seconds

;; ============================================================================
;; HELPER FIXTURE
;; ============================================================================

(def-fixture test-forth-compilation-context ()
  (let* ((temp-dir (ensure-directories-exist
                    (make-pathname :directory (list :relative "forth-test-temp")))))
    (&body)))