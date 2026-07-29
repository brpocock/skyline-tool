(in-package :skyline-tool)

;; Define preserved dynamics with earmuff naming and equalp test
(define-constant */basic-dynamics-list/
  `(*machine *region *project.json *game-title *build *sound
    *part-number *studio *publisher *common-palette *default-skin-color
    *default-hair-color *default-clothes-color)
  :test 'equalp)

(defvar *worker-journal* nil
  "Journal for thread pool worker errors and events.")

(defun cpu-count ()
  "Return the number of logical CPU cores by reading /proc/cpuinfo."
  (with-open-file (f #p"/proc/cpuinfo" :direction :input)
    (loop for line = (read-line f nil nil)
          with count = 0
          while line
          when (and (> (length line) 9)
                    (string-equal (subseq line 0 9) "processor"))
            do (incf count)
          finally (return count))))

(defun ensure-worker-journal ()
  "Initialize the worker journal for logging thread pool events.
   Uses a PPRINT-JOURNAL (stateless, write-only) rather than a FILE-JOURNAL
   so multiple threads can log concurrently without WITH-JOURNALING
   state transitions (:NEW → :COMPLETED)."
  (unless *worker-journal*
    (setf *worker-journal* (journal:make-in-memory-journal :sync t))))

(defun log-worker-condition (condition)
  "Log a condition from a worker thread."
  (journal:journaled ((format nil "~a: ~a" (thread-name (current-thread)) condition)
                      :log-record *worker-journal*
                      :args (list :thread (current-thread)
                                  :backtrace
                                  (with-output-to-string (s)
                                    (trivial-backtrace:print-backtrace condition
                                                                       :output s :verbose t)))
                      :condition condition)))

(defclass task-queue ()
  ((head :accessor queue-head :initform nil)
   (tail :accessor queue-tail :initform nil)))

(defun make-task-queue ()
  (make-instance 'task-queue))

(defun queue-push (queue item)
  "Push ITEM onto QUEUE."
  (let ((node (cons item nil)))
    (if (null (queue-tail queue))
        (setf (queue-head queue) node)
        (setf (cdr (queue-tail queue)) node))
    (setf (queue-tail queue) node)))

(defun queue-pop (queue)
  "Remove ITEM from QUEUE."
  (let ((node (queue-head queue)))
    (when node
      (setf (queue-head queue) (cdr node))
      (when (null (queue-head queue))
        (setf (queue-tail queue) nil))
      (car node))))

(defun task-queue-empty-p (queue)
  "Return T if QUEUE is empty."
  (null (queue-head queue)))

(defclass thread-pool ()
  ((workers :accessor pool-workers :initform nil)
   (task-queue :accessor pool-task-queue :initform (make-task-queue))
   (lock :accessor pool-lock :initform (make-lock "thread-pool-lock"))
   (condition :accessor pool-condition :initform (make-condition-variable :name "task-queue-cv"))
   (capacity :accessor pool-capacity :initarg :capacity :initform 4)
   (shutdown :accessor pool-shutdown :initform nil)))

(defvar *global-thread-pool* nil
  "Global thread pool instance")

(defvar *pool-manager-thread* nil
  "Pool manager thread")

(defun ensure-thread-pool ()
  "Initialize the global thread pool"
  (unless *global-thread-pool*
    (setf *global-thread-pool* (make-instance 'thread-pool :capacity 14))
    (setf *pool-manager-thread* (make-thread #'pool-manager-loop :name "Pool-Manager"))
    (start-thread-pool-workers *global-thread-pool*))
  (ensure-worker-journal))

(defun start-thread-pool-workers (pool)
  "Start worker threads for the thread pool.
   Workers inherit the current dynamic project bindings (e.g. *machine*,
   *project.json*) from the creating thread; they do NOT reload the project."
  (dotimes (i (pool-capacity pool))
    (make-thread
     (lambda ()
       (thread-pool-worker-loop pool))
     :name (format nil "Background Worker +~2,'0d" i))))

(defun thread-pool-worker-loop (pool)
  "Main worker loop for the thread pool.
   Workers suppress console output by redirecting streams to /dev/null."
  (let* ((devnull (make-broadcast-stream))
         (*standard-output* devnull)
         (*error-output* devnull)
         (*query-io* devnull))
    (loop while (not (pool-shutdown pool))
          for task = (pop-task-from-pool pool)
          if task
            do (handler-case
                   (funcall task)
                 (condition (c)
                   (log-worker-condition c)))
          else
            do (sleep 1))))

(defun pop-task-from-pool (pool)
  "Remove task from pool"
  (with-lock-held ((pool-lock pool))
    (loop while (task-queue-empty-p (pool-task-queue pool))
          do (condition-wait (pool-condition pool) (pool-lock pool))
          when (pool-shutdown pool) return nil)
    (queue-pop (pool-task-queue pool))))

(defun pool-task-added (pool)
  (with-lock-held ((pool-lock pool))
    (condition-notify (pool-condition pool))))

(defun submit-task (fn)
  "Submit task FN to thread pool.
   Captures current dynamic bindings of *machine*, *game-title*,
   and *project.json* so worker threads inherit the project context."
  (ensure-thread-pool)
  (let ((task (lambda ()
                (let ((*machine* *machine*)
                      (*game-title* *game-title*)
                      (*project.json* *project.json*))
                  (funcall fn)))))
    (queue-push (pool-task-queue *global-thread-pool*) task)
    (pool-task-added *global-thread-pool*)))

(defun shutdown-thread-pool (&optional (pool *global-thread-pool*))
  "Shut down thread pool"
  (setf (pool-shutdown pool) t)
  (with-lock-held ((pool-lock pool))
    (condition-notify (pool-condition pool)))
  (dolist (worker (pool-workers pool))
    (ignore-errors (destroy-thread worker))))

(defun pool-manager-loop ()
  "Monitor and restart dead worker threads"
  (loop
     (sleep 1)
     (dolist (worker (pool-workers *global-thread-pool*))
        (when (not (thread-alive-p worker))
          (log-worker-condition
           (make-condition 'warning
                           :message (format nil "Restarting dead worker ~a" (thread-name worker))))
          (make-thread
           (lambda () (thread-pool-worker-loop *global-thread-pool*))
           :name (thread-name worker))))))

(defun thread-os-tid (thread)
  "Return the OS thread ID for THREAD."
  #+sbcl (sb-thread:thread-os-tid thread)
  #-sbcl (progn (declare (ignore thread)) -1))
