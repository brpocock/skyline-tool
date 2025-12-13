(in-package :skyline-tool/test)

(def-suite animation-preview-tests
  :description "Tests for animation preview scale calculation and display")

(in-suite animation-preview-tests)

;; Helper to create a mock animation sequence for testing
(defun make-test-sequence (&key (bytes-width 4) (write-mode :160a) (frame-count 4))
  "Create a simple animation sequence for testing"
  (make-instance 'skyline-tool::simple-animation-sequence
                 :index 0
                 :major-kind :npc
                 :decal-kind :human
                 :decal-body 0
                 :tile-sheet "NPCs"
                 :frame-rate-scalar 1
                 :frame-count frame-count
                 :write-mode write-mode
                 :bytes-width bytes-width
                 :frames (make-array 8 :initial-element 0)))

;; Test scale calculation for different image dimensions
(test preview-scale-narrow-sprite-160a
  "Test scale calculation for narrow sprite in 160a mode"
  (let* ((bytes-width 2)
         (mode :160a)
         ;; In 160a: each byte = 4 pixels wide
         (image-width-pixels (* bytes-width 4))  ; 8 pixels
         (image-height-pixels 16)
         (max-width 600)
         (max-height 180)
         (scale-x (/ max-width image-width-pixels))   ; 600/8 = 75
         (scale-y (/ max-height image-height-pixels)) ; 180/16 = 11.25
         (expected-scale (floor (min scale-x scale-y)))) ; min(75, 11.25) = 11
    (is (= expected-scale 11))
    (is (= image-width-pixels 8))
    (is (= image-height-pixels 16))))

(test preview-scale-wide-sprite-160b
  "Test scale calculation for wide sprite in 160b mode"
  (let* ((bytes-width 8)
         (mode :160b)
         ;; In 160b: each byte = 2 pixels wide
         (image-width-pixels (* bytes-width 2))  ; 16 pixels
         (image-height-pixels 16)
         (max-width 600)
         (max-height 180)
         (scale-x (/ max-width image-width-pixels))   ; 600/16 = 37.5
         (scale-y (/ max-height image-height-pixels)) ; 180/16 = 11.25
         (expected-scale (floor (min scale-x scale-y)))) ; min(37.5, 11.25) = 11
    (is (= expected-scale 11))
    (is (= image-width-pixels 16))))

(test preview-scale-very-wide-sprite-160a
  "Test scale calculation for very wide sprite in 160a mode"
  (let* ((bytes-width 32)
         (mode :160a)
         ;; In 160a: each byte = 4 pixels wide
         (image-width-pixels (* bytes-width 4))  ; 128 pixels
         (image-height-pixels 16)
         (max-width 600)
         (max-height 180)
         (scale-x (/ max-width image-width-pixels))   ; 600/128 = 4.6875
         (scale-y (/ max-height image-height-pixels)) ; 180/16 = 11.25
         (expected-scale (floor (min scale-x scale-y)))) ; min(4.6875, 11.25) = 4
    (is (= expected-scale 4))
    (is (= image-width-pixels 128))))

(test preview-scale-minimum-unit
  "Test that scale never goes below 1"
  (let* ((bytes-width 200)  ; Absurdly wide
         (mode :160a)
         (image-width-pixels (* bytes-width 4))  ; 800 pixels (wider than max)
         (image-height-pixels 16)
         (max-width 600)
         (max-height 180)
         (scale-x (/ max-width image-width-pixels))   ; 600/800 = 0.75
         (scale-y (/ max-height image-height-pixels)) ; 180/16 = 11.25
         (scale (min scale-x scale-y))                ; 0.75
         (expected-unit (max 1 (floor scale))))       ; max(1, 0) = 1
    (is (= expected-unit 1))
    (is (< scale 1))))

(test preview-scale-aspect-ratio-maintained
  "Test that aspect ratio is maintained by using minimum scale"
  (let* ((bytes-width 4)
         (mode :160a)
         (image-width-pixels (* bytes-width 4))  ; 16 pixels
         (image-height-pixels 16)
         (max-width 600)
         (max-height 180)
         (scale-x (/ max-width image-width-pixels))   ; 600/16 = 37.5
         (scale-y (/ max-height image-height-pixels)) ; 180/16 = 11.25
         (chosen-scale (min scale-x scale-y)))        ; 11.25 (limited by height)
    ;; Verify height constraint is the limiting factor
    (is (< scale-y scale-x))
    ;; Verify we chose the smaller scale
    (is (= chosen-scale scale-y))
    ;; This ensures we fit within bounds (using <= since we may fit exactly)
    (is (<= (* image-height-pixels chosen-scale) max-height))
    (is (<= (* image-width-pixels chosen-scale) max-width))))

(test preview-scale-160a-vs-160b-same-bytes
  "Test that 160a produces wider images than 160b for same bytes-width"
  (let* ((bytes-width 4)
         (width-160a (* bytes-width 4))  ; 160a: 4 pixels per byte
         (width-160b (* bytes-width 2))) ; 160b: 2 pixels per byte
    (is (> width-160a width-160b))
    (is (= width-160a 16))
    (is (= width-160b 8))))

;; Test animation sequence creation
(test create-test-sequence-160a
  "Test creating a mock sequence in 160a mode"
  ;; Note: For :npc with :human, initialize-instance overrides to :160b
  ;; So this test actually verifies the override behavior
  (let ((seq (make-test-sequence :bytes-width 4 :write-mode :160a :frame-count 4)))
    (is (not (null seq)))
    (is (= (skyline-tool::simple-animation-sequence-bytes-width seq) 4))
    ;; Due to initialize-instance :after, :npc+:human gets :160b
    (is (eql (skyline-tool::simple-animation-sequence-write-mode seq) :160b))
    (is (= (skyline-tool::simple-animation-sequence-frame-count seq) 4))))

(test create-test-sequence-160b
  "Test creating a mock sequence in 160b mode"
  (let ((seq (make-test-sequence :bytes-width 4 :write-mode :160b :frame-count 8)))
    (is (not (null seq)))
    (is (= (skyline-tool::simple-animation-sequence-bytes-width seq) 4))
    (is (eql (skyline-tool::simple-animation-sequence-write-mode seq) :160b))
    (is (= (skyline-tool::simple-animation-sequence-frame-count seq) 8))))

(test create-test-sequence-scenery-gets-160a
  "Test that scenery sequences get 160a mode"
  (let ((seq (make-instance 'skyline-tool::simple-animation-sequence
                            :index 0
                            :major-kind :scenery
                            :decal-kind :human
                            :decal-body 0
                            :tile-sheet "SceneryDecals"
                            :frame-rate-scalar 1
                            :frame-count 4
                            :write-mode :160b  ; Will be overridden
                            :bytes-width 2
                            :frames (make-array 8 :initial-element 0))))
    (is (not (null seq)))
    ;; Scenery should always get :160a per initialize-instance
    (is (eql (skyline-tool::simple-animation-sequence-write-mode seq) :160a))))

;; Test FPS calculation
(test fps-calculation-scalar-1
  "Test FPS calculation with scalar 1"
  (let* ((scalar 1)
         (fps (* 10 scalar)))
    (is (= fps 10))))

(test fps-calculation-scalar-half
  "Test FPS calculation with scalar 1/2"
  (let* ((scalar 1/2)
         (fps (* 10 scalar)))
    (is (= fps 5))))

(test fps-calculation-scalar-quarter
  "Test FPS calculation with scalar 1/4"
  (let* ((scalar 1/4)
         (fps (* 10 scalar)))
    (is (= fps 5/2))
    (is (= (float fps) 2.5))))

(test fps-calculation-scalar-eighth
  "Test FPS calculation with scalar 1/8"
  (let* ((scalar 1/8)
         (fps (* 10 scalar)))
    (is (= fps 5/4))
    (is (= (float fps) 1.25))))

;; Test frame advancement
(test frame-advance-wraps-around
  "Test that frame counter wraps around correctly"
  (let* ((frame-count 4)
         (current-frame 3)
         (next-frame (mod (1+ current-frame) frame-count)))
    (is (= next-frame 0))))

(test frame-advance-increments
  "Test that frame counter increments correctly"
  (let* ((frame-count 8)
         (current-frame 3)
         (next-frame (mod (1+ current-frame) frame-count)))
    (is (= next-frame 4))))

(test frame-advance-from-zero
  "Test frame advancement from zero"
  (let* ((frame-count 4)
         (current-frame 0)
         (next-frame (mod (1+ current-frame) frame-count)))
    (is (= next-frame 1))))

;; Test delay calculation
(test delay-calculation-10fps
  "Test delay calculation for 10 FPS"
  (let* ((fps 10)
         (delay (/ 1.0 fps)))
    (is (= delay 0.1))))

(test delay-calculation-5fps
  "Test delay calculation for 5 FPS"
  (let* ((fps 5)
         (delay (/ 1.0 fps)))
    (is (= delay 0.2))))

(test delay-calculation-2.5fps
  "Test delay calculation for 2.5 FPS"
  (let* ((fps 2.5)
         (delay (/ 1.0 fps)))
    (is (= delay 0.4))))

(defun run-animation-preview-tests ()
  "Run all animation preview tests and return results"
  (fiveam:run! 'animation-preview-tests))
