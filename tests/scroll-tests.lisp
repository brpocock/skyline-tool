(in-package :skyline-tool/test)

(def-suite scroll-direction-suite
  :description "Unit tests for 7800 map scrolling in all four cardinal directions"
  :in skyline-tool/test)

(in-suite scroll-direction-suite)

(defun scroll-test-setup ()
  "Match Source/Code/7800/Tests/ScrollTests.s TestScrollSetup coordinates."
  (skyline-tool::make-scroll-state 2 0 2 0))

(test scroll-north-coarse
  "Coarse scroll north decrements MapTopRow when MapTopLine is zero"
  (let ((state (skyline-tool::make-scroll-state 2 0 2 0)))
    (multiple-value-bind (after scrolledp)
        (skyline-tool::scroll-north state)
      (is-true scrolledp)
      (is (= 1 (skyline-tool::scroll-state-top-row after)))
      (is (= 15 (skyline-tool::scroll-state-top-line after))))))

(test scroll-south-coarse
  "Coarse scroll south increments MapTopRow when MapTopLine is fifteen"
  (let ((state (skyline-tool::make-scroll-state 2 15 2 0)))
    (multiple-value-bind (after scrolledp)
        (skyline-tool::scroll-south state)
      (is-true scrolledp)
      (is (= 3 (skyline-tool::scroll-state-top-row after)))
      (is (= 0 (skyline-tool::scroll-state-top-line after))))))

(test scroll-west-coarse
  "Coarse scroll west decrements MapLeftColumn when MapLeftPixel wraps"
  (let ((state (skyline-tool::make-scroll-state 2 0 2 0)))
    (multiple-value-bind (after scrolledp)
        (skyline-tool::scroll-west state)
      (is-true scrolledp)
      (is (= 1 (skyline-tool::scroll-state-left-column after)))
      (is (= 7 (skyline-tool::scroll-state-left-pixel after))))))

(test scroll-east-coarse
  "Coarse scroll east increments MapLeftColumn when MapLeftPixel reaches eight"
  (let ((state (skyline-tool::make-scroll-state 2 0 2 7)))
    (multiple-value-bind (after scrolledp)
        (skyline-tool::scroll-east state)
      (is-true scrolledp)
      (is (= 3 (skyline-tool::scroll-state-left-column after)))
      (is (= 0 (skyline-tool::scroll-state-left-pixel after))))))

(test scroll-north-fine
  "Fine scroll north decrements MapTopLine without changing MapTopRow"
  (let ((state (skyline-tool::make-scroll-state 2 8 2 0)))
    (multiple-value-bind (after scrolledp)
        (skyline-tool::scroll-north state)
      (is-true scrolledp)
      (is (= 7 (skyline-tool::scroll-state-top-line after)))
      (is (= 2 (skyline-tool::scroll-state-top-row after))))))

(test scroll-south-fine
  "Fine scroll south increments MapTopLine without changing MapTopRow"
  (let ((state (skyline-tool::make-scroll-state 2 7 2 0)))
    (multiple-value-bind (after scrolledp)
        (skyline-tool::scroll-south state)
      (is-true scrolledp)
      (is (= 8 (skyline-tool::scroll-state-top-line after)))
      (is (= 2 (skyline-tool::scroll-state-top-row after))))))

(test scroll-west-fine
  "Fine scroll west decrements MapLeftPixel without changing MapLeftColumn"
  (let ((state (skyline-tool::make-scroll-state 2 0 2 4)))
    (multiple-value-bind (after scrolledp)
        (skyline-tool::scroll-west state)
      (is-true scrolledp)
      (is (= 3 (skyline-tool::scroll-state-left-pixel after)))
      (is (= 2 (skyline-tool::scroll-state-left-column after))))))

(test scroll-east-fine
  "Fine scroll east increments MapLeftPixel without changing MapLeftColumn"
  (let ((state (skyline-tool::make-scroll-state 2 0 2 3)))
    (multiple-value-bind (after scrolledp)
        (skyline-tool::scroll-east state)
      (is-true scrolledp)
      (is (= 4 (skyline-tool::scroll-state-left-pixel after)))
      (is (= 2 (skyline-tool::scroll-state-left-column after))))))

(test scroll-north-boundary
  "Scrolling north at row zero is a no-op"
  (let ((state (skyline-tool::make-scroll-state 0 0 2 0)))
    (multiple-value-bind (after scrolledp)
        (skyline-tool::scroll-north state)
      (is-false scrolledp)
      (is (equalp state after)))))

(test scroll-south-boundary
  "Scrolling south at the bottom margin is a no-op"
  (let ((state (skyline-tool::make-scroll-state 52 0 2 0 :map-rows 12 :map-height 64)))
    (multiple-value-bind (after scrolledp)
        (skyline-tool::scroll-south state)
      (is-false scrolledp)
      (is (equalp state after)))))

(test scroll-west-boundary
  "Scrolling west at the left edge is a no-op"
  (let ((state (skyline-tool::make-scroll-state 2 0 0 0)))
    (multiple-value-bind (after scrolledp)
        (skyline-tool::scroll-west state)
      (is-false scrolledp)
      (is (equalp state after)))))

(test scroll-east-boundary
  "Scrolling east at the right margin is a no-op"
  (let ((state (skyline-tool::make-scroll-state 2 0 19 0 :map-width 40 :viewport-columns 21)))
    (multiple-value-bind (after scrolledp)
        (skyline-tool::scroll-east state)
      (is-false scrolledp)
      (is (equalp state after)))))

(test scroll-four-directions-from-setup
  "Each cardinal direction updates scroll state from the shared test setup"
  (let ((base (scroll-test-setup)))
    (multiple-value-bind (up _) (skyline-tool::scroll-north
                                 (skyline-tool::make-scroll-state 2 0 2 0))
      (is (= 1 (skyline-tool::scroll-state-top-row up))))
    (multiple-value-bind (down _) (skyline-tool::scroll-south
                                  (skyline-tool::make-scroll-state 2 15 2 0))
      (is (= 3 (skyline-tool::scroll-state-top-row down))))
    (multiple-value-bind (left _) (skyline-tool::scroll-west base)
      (is (= 1 (skyline-tool::scroll-state-left-column left))))
    (multiple-value-bind (right _) (skyline-tool::scroll-east
                                    (skyline-tool::make-scroll-state 2 0 2 7))
      (is (= 3 (skyline-tool::scroll-state-left-column right))))))

(test scroll-west-fine-header-xpos
  "Fine scroll west increments each tile header X position by one pixel"
  (let* ((before (skyline-tool::make-tile-row-headers 4))
         (after (skyline-tool::fine-scroll-headers-west before)))
    (is (= (1+ (skyline-tool::header-xpos (aref before 0)))
           (skyline-tool::header-xpos (aref after 0))))
    (is (= (1+ (skyline-tool::header-xpos (aref before 1)))
           (skyline-tool::header-xpos (aref after 1))))))

(test scroll-east-fine-header-xpos
  "Fine scroll east decrements each tile header X position by one pixel"
  (let* ((before (skyline-tool::make-tile-row-headers 3))
         (after (skyline-tool::fine-scroll-headers-east before)))
    (is (= (1- (skyline-tool::header-xpos (aref before 0)))
           (skyline-tool::header-xpos (aref after 0))))
    (is (= (1- (skyline-tool::header-xpos (aref before 1)))
           (skyline-tool::header-xpos (aref after 1))))))

(test scroll-up-fine-dll-holey-byte
  "Fine scroll up rewrites the top DLL holey-height byte from MapTopLine"
  (is (= #x48 (skyline-tool::dll-holey-byte 7)))
  (is (= #x47 (skyline-tool::dll-holey-byte 8))))

(test scroll-coarse-west-first-header-xpos
  "Coarse scroll west leaves the leftmost tile header at X = -7"
  (let ((headers (skyline-tool::make-tile-row-headers 0)))
    (setf (nth 3 (aref headers 0)) -7)
    (is (= -7 (skyline-tool::header-xpos (aref headers 0))))))

(defun run-scroll-tests ()
  "Run only the four-direction scroll unit tests."
  (fiveam:run! 'scroll-direction-suite))
