;;;; Thread Pool Utility for Skyline-Tool
;;;; Provides a simple on-demand thread pool for background tasks

(in-package :skyline-tool)

(defun ensure-thread-pool-kernel ()
  "Start a kernel for lparallel with one thread per CPU core."
  (unless lparallel:*kernel*
    (let* ((output (uiop:run-program '("nproc") :output :string :ignore-error-status t))
           (count (parse-integer (string-trim '(#\newline #\space #\tab) output)
                                 :junk-allowed t)))
      (lparallel:make-kernel (max 4 (or count 4)) :name "skyline-tool"))))

(defclass task-queue ()
  ((head :accessor queue-head :initform nil)
   (tail :accessor queue-tail :initform nil)))

(defun make-task-queue ()
  (make-instance 'task-queue))

(defun queue-push (queue item)
  "Push an item onto the task queue."
  (let ((node (cons item nil)))
    (if (null (queue-tail queue))
        (setf (queue-head queue) node)
        (setf (cdr (queue-tail queue)) node))
    (setf (queue-tail queue) node)))

(defun queue-pop (queue)
  "Pop an item from the task queue."
  (let ((node (queue-head queue)))
    (when node
      (setf (queue-head queue) (cdr node))
      (when (null (queue-head queue))
        (setf (queue-tail queue) nil))
      (car node))))

(defun task-queue-empty-p (queue)
  "Check if the task queue is empty."
  (null (queue-head queue)))

(defclass thread-pool ()
  ((workers :accessor pool-workers :initform nil)
   (task-queue :accessor pool-task-queue :initform (make-task-queue))
   (lock :accessor pool-lock :initform (bt:make-lock "thread-pool-lock"))
   (condition :accessor pool-condition :initform (bt:make-condition-variable :name "task-queue-cv"))
   (capacity :accessor pool-capacity :initarg :capacity :initform 4)
   (shutdown :accessor pool-shutdown :initform nil)))

(defvar *global-thread-pool* nil
  "The global thread pool instance for Skyline-Tool.")

(defun initialize-thread-pool ()
  "Create and initialize the global thread pool with CPU-count workers."
  (unless *global-thread-pool*
    (setf *global-thread-pool* (make-instance 'thread-pool))
    (start-thread-pool-workers *global-thread-pool*)))

(defun start-thread-pool-workers (pool)
  "Spawn worker threads for the thread pool."
  (dotimes (i (pool-capacity pool))
    (push (bt:make-thread 
            (lambda () (thread-pool-worker-loop pool))
            :name (format nil "Skyline-Worker-~d" i))
          (pool-workers pool))))

(defun thread-pool-worker-loop (pool)
  "Main loop for a thread pool worker."
  (loop
    (when (pool-shutdown pool) (return))
    (let ((task (pop-task-from-pool pool)))
      (when task
        (ignore-errors (apply (first task) (rest task)))))))

(defun pop-task-from-pool (pool)
  "Remove and return a task from the task queue."
  (bt:with-lock-held ((pool-lock pool))
    (loop while (task-queue-empty-p (pool-task-queue pool))
          do (bt:condition-wait (pool-condition pool) (pool-lock pool))
          when (pool-shutdown pool) return nil)
    (queue-pop (pool-task-queue pool))))

(defgeneric pool-task-added (pool)
  (:method ((pool thread-pool))
    (bt:with-lock-held ((pool-lock pool))
      (bt:condition-notify (pool-condition pool)))))

(defun submit-task (fn &rest args)
  "Submit a task to the global thread pool."
  (initialize-thread-pool)
  (let ((task (cons fn args)))
    (queue-push (pool-task-queue *global-thread-pool*) task)
    (pool-task-added *global-thread-pool*)))

(defun shutdown-thread-pool ()
  "Gracefully shut down the global thread pool."
  (when *global-thread-pool*
    (setf (pool-shutdown *global-thread-pool*) t)
    (bt:with-lock-held ((pool-lock *global-thread-pool*))
      (bt:condition-notify (pool-condition *global-thread-pool*)))
    (dolist (worker (pool-workers *global-thread-pool*))
      (ignore-errors (bt:destroy-thread worker)))
    (setf *global-thread-pool* nil)))