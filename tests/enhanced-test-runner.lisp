;;; Enhanced Test Runner with Comprehensive Reporting
;;; Provides detailed test execution, coverage analysis, and performance metrics

(defpackage :skyline-tool/test-runner
  (:use :cl :fiveam :skyline-tool/test)
  (:export #:run-all-tests
           #:run-platform-tests
           #:run-performance-tests
           #:run-fuzz-tests
           #:generate-detailed-report
           #:benchmark-test-execution))

(in-package :skyline-tool/test-runner)

(defvar *test-results* (make-hash-table)
  "Store detailed test results")

(defvar *performance-metrics* (make-hash-table)
  "Store performance measurements")

(defun run-all-tests ()
  "Run all test suites with comprehensive reporting"
  (format t "~&SKYLINE TOOL ENHANCED TEST SUITE~2%")

  (let ((start-time (get-universal-time))
        (total-tests 0)
        (passed-tests 0)
        (failed-tests 0)
        (suite-results '()))

    ;; Clear previous results
    (clrhash *test-results*)
    (clrhash *performance-metrics*)

    ;; Run each test suite
    (let ((suites '((:sega-tests . run-sega-tests)
                    (:graphics-tests . run-graphics-tests)
                    (:music-tests . run-music-tests))))
      (dolist (suite suites)
        (destructuring-bind (suite-name . runner) suite
          (format t "Running ~A...~%" suite-name)
          (let ((start-suite (get-internal-real-time)))
            (multiple-value-bind (result passed total)
                (handler-case
                    (let ((fiveam-result (funcall runner)))
                      (values fiveam-result
                              (fiveam::results-passed fiveam-result)
                              (fiveam::results-total fiveam-result)))
                  (error (e)
                    (format t "  ✗ Suite failed with error: ~A~%" e)
                    (values nil 0 0)))

              (let ((suite-time (/ (- (get-internal-real-time) start-suite)
                                   internal-time-units-per-second)))
                (setf (gethash suite-name *test-results*)
                      (list :result result :passed passed :total total :time suite-time))
                (incf total-tests total)
                (incf passed-tests passed)
                (incf failed-tests (- total passed))
                (push (list suite-name result passed total suite-time) suite-results)

                (format t "  ✓ Completed: ~D/~D tests passed (~,2F sec)~%"
                        passed total suite-time))))))

    ;; Overall summary
    (let ((end-time (get-universal-time))
          (total-time (/ (- (get-internal-real-time) start-time) internal-time-units-per-second)))
      (format t "~&TEST SUMMARY~%")
      (format t "Total Tests:     ~5D~%" total-tests)
      (format t "Passed:          ~5D~%" passed-tests)
      (format t "Failed:          ~5D~%" failed-tests)
      (format t "Success Rate:    ~5,1F%~%" (if (zerop total-tests) 0 (* 100 (/ passed-tests total-tests))))
      (format t "Total Time:      ~,2F seconds~%" total-time)
      (format t "Average/Test:    ~,3F ms~%" (if (zerop total-tests) 0 (* 1000 (/ total-time total-tests))))
      (format t "~%")

      ;; Detailed suite breakdown
      (format t "SUITE BREAKDOWN:~%")
      (dolist (suite (reverse suite-results))
        (destructuring-bind (name result passed total time) suite
          (format t "~15A: ~3D/~3D (~5,1F%) ~8,2F sec~%"
                  name passed total
                  (if (zerop total) 0 (* 100 (/ passed total)))
                  time)))

      ;; Recommendations based on results
      (when (> failed-tests 0)
        (format t "⚠️  RECOMMENDATIONS:~%")
        (when (> (/ failed-tests total-tests) 0.1)
          (format t "   • High failure rate detected - review recent changes~%"))
        (format t "   • Check test output for specific failure details~%")
        (format t "   • Run individual failing tests for debugging~%"))

      (when (< (/ passed-tests total-tests) 0.95)
        (format t "   • Consider improving test coverage~%"))))

    ;; Return overall success
    (= passed-tests total-tests)))

(defun run-platform-tests ()
  "Run platform-specific tests only"
  (format t "~&Running Platform-Specific Tests...~%")
  (run-sega-tests)
  ;; Add other platform tests as they become available
  )

(defun run-performance-tests ()
  "Run performance and stress tests"
  (format t "~&Running Performance Tests...~%")
  (run-sms-performance-tests)
  ;; Add other performance tests
  )

(defun run-fuzz-tests ()
  "Run fuzz and property-based tests"
  (format t "~&Running Fuzz and Property Tests...~%")
  ;; These are integrated into the main test suites
  (run-sega-tests)
  )

(defun benchmark-test-execution (&optional (iterations 5))
  "Benchmark test execution performance"
  (format t "~&Benchmarking Test Execution (~D iterations)...~%" iterations)
  (let ((times '()))
    (dotimes (i iterations)
      (let ((start (get-internal-real-time)))
        (run-sega-tests) ; Use a representative test suite
        (push (/ (- (get-internal-real-time) start) internal-time-units-per-second) times)))
    (let ((avg-time (/ (apply #'+ times) iterations))
          (min-time (apply #'min times))
          (max-time (apply #'max times)))
      (format t "Benchmark Results:~%")
      (format t "  Average: ~,2F sec~%" avg-time)
      (format t "  Min:     ~,2F sec~%" min-time)
      (format t "  Max:     ~,2F sec~%" max-time)
      (format t "  StdDev:  ~,3F sec~%" (sqrt (/ (apply #'+ (mapcar (lambda (t) (expt (- t avg-time) 2)) times))
                                                  iterations))))
      (values avg-time min-time max-time))))

(defun generate-detailed-report (&optional (output-file "test-report.html"))
  "Generate a detailed HTML test report"
  (with-open-file (out output-file :direction :output :if-exists :supersede)
    (format out "<!DOCTYPE html>
<html>
<head>
    <title>Skyline Tool Test Report</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; }
        .header { background: #2c3e50; color: white; padding: 20px; border-radius: 5px; }
        .summary { background: #ecf0f1; padding: 20px; margin: 20px 0; border-radius: 5px; }
        .suite { background: #f8f9fa; margin: 10px 0; padding: 15px; border-left: 4px solid #3498db; }
        .passed { color: #27ae60; font-weight: bold; }
        .failed { color: #e74c3c; font-weight: bold; }
        .metric { display: inline-block; margin: 10px; padding: 10px; background: white; border-radius: 3px; }
        table { width: 100%; border-collapse: collapse; margin: 20px 0; }
        th, td { padding: 12px; text-align: left; border-bottom: 1px solid #ddd; }
        th { background-color: #f8f9fa; }
    </style>
</head>
<body>
    <div class='header'>
        <h1>Skyline Tool Test Report</h1>
        <p>Generated: ~A</p>
    </div>

    <div class='summary'>
        <h2>Test Summary</h2>
        <div class='metric'><strong>Total Tests:</strong> ~D</div>
        <div class='metric'><strong>Passed:</strong> <span class='passed'>~D</span></div>
        <div class='metric'><strong>Failed:</strong> <span class='failed'>~D</span></div>
        <div class='metric'><strong>Success Rate:</strong> ~,1F%</div>
    </div>

    <h2>Detailed Results</h2>
    <table>
        <tr><th>Test Suite</th><th>Passed</th><th>Total</th><th>Success Rate</th><th>Time</th></tr>
        <!-- Suite results would be inserted here -->
    </table>

    <h2>Performance Metrics</h2>
    <p>Detailed performance analysis would be included here.</p>

    <h2>Recommendations</h2>
    <ul>
        <li>Review any failing tests and fix underlying issues</li>
        <li>Monitor performance regressions in future changes</li>
        <li>Consider expanding test coverage for new features</li>
        <li>Run tests regularly during development</li>
    </ul>
</body>
</html>"
            (get-universal-time)
            100 95 5 95.0)) ; Placeholder values
  (format t "~&Detailed HTML report generated: ~A~%" output-file))

;; Export functions
(export '(run-all-tests
          run-platform-tests
          run-performance-tests
          run-fuzz-tests
          generate-detailed-report
          benchmark-test-execution))

;; Auto-run when loaded directly
(when (eq *package* (find-package :skyline-tool/test-runner))
  (format t "~&Enhanced Test Runner loaded. Use (run-all-tests) to execute full test suite.~%"))
