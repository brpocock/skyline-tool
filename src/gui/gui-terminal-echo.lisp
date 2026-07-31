(in-package :skyline-tool)

;; Macro for defining frame-specific commands for terminal-echo-frame
(defmacro define-terminal-echo-frame-command ((name &rest options) args &body body)
  "Define a CLIM command for the terminal-echo-frame command table."
  `(clim:define-command (,name :command-table terminal-echo-frame ,@options)
     ,args
     ,@body))

;; Terminal Echo Window - captures and displays terminal output using CLIM
;; Provides a dedicated window for running commands like make, showing live output
;; ANSI control codes translation:
;;   Form feed / clear screen = horizontal line
;;   stderr = red
;;   stdout = black
;;   ANSI color codes translated to CLIM colors
;;; ANSI escape code parser and translator

(defparameter *ansi-color-map*
  '((0 . :black) (1 . :red) (2 . :green) (3 . :yellow)
    (4 . :blue) (5 . :magenta) (6 . :cyan) (7 . :white)
    (8 . :bright-black) (9 . :bright-red) (10 . :bright-green) (11 . :bright-yellow)
    (12 . :bright-blue) (13 . :bright-magenta) (14 . :bright-cyan) (15 . :bright-white)))

(defun ansi-color->clim-ink (ansi-code)
  "Translate ANSI color code to CLIM ink."
  (let ((base (cdr (assoc ansi-code *ansi-color-map*))))
    (when base
      (clim:make-rgb-color
       (case base
         (:black 0.0)
         (:red 1.0)
         (:green 0.0)
         (:yellow 1.0)
         (:blue 0.0)
         (:magenta 1.0)
         (:cyan 0.0)
         (:white 1.0)
         (:bright-black 0.3)
         (:bright-red 1.0)
         (:bright-green 0.0)
         (:bright-yellow 1.0)
         (:bright-blue 0.0)
         (:bright-magenta 1.0)
         (:bright-cyan 0.0)
         (:bright-white 1.0))
       (case base
         (:black 0.0)
         (:red 0.0)
         (:green 1.0)
         (:yellow 1.0)
         (:blue 0.0)
         (:magenta 0.0)
         (:cyan 1.0)
         (:white 1.0)
         (:bright-black 0.3)
         (:bright-red 0.0)
         (:bright-green 1.0)
         (:bright-yellow 1.0)
         (:bright-blue 1.0)
         (:bright-magenta 1.0)
         (:bright-cyan 1.0)
         (:bright-white 1.0))
       (case base
         (:black 0.0)
         (:red 0.0)
         (:green 0.0)
         (:yellow 0.0)
         (:blue 1.0)
         (:magenta 1.0)
         (:cyan 1.0)
         (:white 1.0)
         (:bright-black 0.3)
         (:bright-red 0.0)
         (:bright-green 0.0)
         (:bright-yellow 0.0)
         (:bright-blue 1.0)
         (:bright-magenta 1.0)
         (:bright-cyan 1.0)
         (:bright-white 1.0))))))

(defclass ansi-parser ()
  ((buffer :initform (make-array 1024 :element-type 'character :fill-pointer 0 :adjustable t)
           :accessor parser-buffer)
   (current-ink :initform clim:+black+ :accessor parser-current-ink)
   (bold-p :initform nil :accessor parser-bold-p)
   (faint-p :initform nil :accessor parser-faint-p)
   (italic-p :initform nil :accessor parser-italic-p)
   (underline-p :initform nil :accessor parser-underline-p)
   (in-escape-p :initform nil :accessor parser-in-escape-p)
   (escape-buffer :initform (make-array 16 :element-type 'character :fill-pointer 0 :adjustable t)
                  :accessor parser-escape-buffer)))

(defun reset-ansi-parser (parser)
  (setf (parser-current-ink parser) clim:+black+
        (parser-bold-p parser) nil
        (parser-faint-p parser) nil
        (parser-italic-p parser) nil
        (parser-underline-p parser) nil
        (parser-in-escape-p parser) nil
        (fill-pointer (parser-escape-buffer parser)) 0))

(defun apply-ansi-sgr (parser params)
  "Apply SGR (Select Graphic Rendition) parameters."
  (dolist (param params)
    (cond
      ((= param 0)  ; Reset
       (reset-ansi-parser parser))
      ((= param 1)  ; Bold
       (setf (parser-bold-p parser) t))
      ((= param 2)  ; Faint
       (setf (parser-faint-p parser) t))
      ((= param 3)  ; Italic
       (setf (parser-italic-p parser) t))
      ((= param 4)  ; Underline
       (setf (parser-underline-p parser) t))
      ((= param 22) ; Normal intensity
       (setf (parser-bold-p parser) nil
             (parser-faint-p parser) nil))
      ((= param 23) ; Not italic
       (setf (parser-italic-p parser) nil))
      ((= param 24) ; Not underline
       (setf (parser-underline-p parser) nil))
      ((and (>= param 30) (<= param 37)) ; Foreground colors
       (let ((ink (ansi-color->clim-ink (- param 30))))
         (when ink (setf (parser-current-ink parser) ink))))
      ((and (>= param 40) (<= param 47)) ; Background colors (ignore for now)
       nil)
      ((and (>= param 90) (<= param 97)) ; Bright foreground
       (let ((ink (ansi-color->clim-ink (- param 90 + 8))))
         (when ink (setf (parser-current-ink parser) ink))))
      ((and (>= param 100) (<= param 107)) ; Bright background (ignore)
       nil))))

(defun parse-ansi-escape (parser char stream)
  "Parse ANSI escape sequence and apply to stream."
  (vector-push-extend char (parser-escape-buffer parser))
  (let ((seq (parser-escape-buffer parser)))
    (cond
      ((and (= (length seq) 2) (char= (char seq 1) #\[)) ; CSI sequence start
       (setf (parser-in-escape-p parser) t))
      ((and (parser-in-escape-p parser)
            (find char "0123456789;?" :test #'char=))
       nil) ; Continue collecting parameters
      ((and (parser-in-escape-p parser) (char= char #\m)) ; SGR end
       (let ((params-str (subseq (parser-escape-buffer parser) 2)))
         (let ((params (mapcar (alexandria:rcurry #'parse-integer :junk-allowed t)
                               (split-sequence:split-sequence #\; params-str))))
           (apply-ansi-sgr parser params)))
       (setf (parser-in-escape-p parser) nil
             (fill-pointer (parser-escape-buffer parser)) 0))
      ((and (parser-in-escape-p parser) (char= char #\J)) ; ED - Erase Display
       (when (and (= (length seq) 3) (char= (aref seq 1) #\[) (char= (aref seq 2) #\2))
         ;; ESC [ 2 J = clear screen -> horizontal line
         (clim:with-drawing-options (stream :ink clim:+black+)
           (clim:draw-line* stream
                            0 (second (clim:stream-cursor-position stream))
                            (clim:bounding-rectangle-width stream) (second (clim:stream-cursor-position stream)))))
       (setf (parser-in-escape-p parser) nil
             (fill-pointer (parser-escape-buffer parser)) 0))
      ((and (parser-in-escape-p parser) (char= char #\H)) ; CUP - Cursor Position (ignore)
       (setf (parser-in-escape-p parser) nil
             (fill-pointer (parser-escape-buffer parser)) 0))
      (t
       ;; Unknown sequence, output raw
       (write-string (coerce seq 'string) stream)
       (setf (parser-in-escape-p parser) nil
             (fill-pointer (parser-escape-buffer parser)) 0)))))

(defun write-char-with-ansi (parser char stream)
  "Write a character to stream, parsing ANSI escapes."
  (if (and (not (parser-in-escape-p parser)) (char= char #\Escape))
      (progn
        (setf (parser-in-escape-p parser) t
              (fill-pointer (parser-escape-buffer parser)) 0)
        (vector-push-extend char (parser-escape-buffer parser)))
      (if (parser-in-escape-p parser)
          (parse-ansi-escape parser char stream)
          (progn
            (when (char= char #\Newline)
              (terpri stream))
            (when (char= char #\Return)
              (clim:stream-set-cursor-position stream 0 (second (clim:stream-cursor-position stream))))
            (when (char= char #\Page) ; Form feed = horizontal line
              (clim:with-drawing-options (stream :ink clim:+black+)
                (clim:draw-line* stream 0 (second (clim:stream-cursor-position stream))
                                 (clim:bounding-rectangle-width stream)
                                 (second (clim:stream-cursor-position stream))))
              (terpri stream))
            (unless (or (char= char #\Newline) (char= char #\Return) (char= char #\Page))
              (clim:with-drawing-options (stream :ink (parser-current-ink parser))
                (let ((style (clim:make-text-style
                              (cond
                                ((and (parser-bold-p parser)
                                      (parser-italic-p parser))
                                 (list :bold :italic))
                                ((parser-bold-p parser) :bold)
                                ((parser-italic-p parser) :italic)
                                (t :normal))
                              :serif
                              :normal)))
                  (when (parser-underline-p parser)
                    (setf style (clim:make-text-style nil :serif :normal)))
                  (clim:with-text-style (stream style)
                    (clim:stream-write-char stream char)))))))))

(defun write-string-with-ansi (parser string stream)
  "Write a string to stream, parsing ANSI escapes."
  (loop for char across string
        do (write-char-with-ansi parser char stream)))

;;; CLIM Frame for Terminal Echo

(clim:define-application-frame terminal-echo-frame ()
  ((process :initform nil :accessor frame-process)
   (buffer :initform (make-string-output-stream) :accessor frame-buffer)
   (command :initform nil :accessor frame-command)
   (parser :initform (make-instance 'ansi-parser) :accessor frame-parser))
  (:panes (echo-pane :application
                     :display-function 'display-terminal-echo
                     :scroll-bars :vertical
                     :height 400 :width 800)
          (input-pane :interactor
                      :height 100 :width 800)
          (output-pane :application
                       :display-function 'display-terminal-output
                       :scroll-bars :vertical
                       :height 300 :width 800)
          (error-pane :application
                      :display-function 'display-terminal-error
                      :scroll-bars :vertical
                      :height 300 :width 800))
  (:command-table (terminal-echo-frame))
  (:menu-bar terminal-echo-menu-bar)
  (:layouts (default (clim:vertically ()
                       echo-pane
                       (clim:make-pane 'clim-extensions:box-adjuster-gadget)
                       input-pane))
            (with-input (clim:vertically ()
                          echo-pane
                          (clim:make-pane 'clim-extensions:box-adjuster-gadget)
                          input-pane))
            (split (clim:vertically () output-pane error-pane))
            (split-with-input (clim:vertically () output-pane error-pane input-pane))))

(clim:define-command-table terminal-echo-menu-bar
  :menu (("Terminal" :menu terminal-echo-file-menu)
         ("Edit" :menu terminal-echo-edit-menu)
         ("View" :menu terminal-echo-view-menu)
         ("Help" :menu terminal-echo-help-menu)))

(clim:define-command-table terminal-echo-file-menu
  :menu (("Save" :menu terminal-save-menu)
         ("Send to" :menu send-to-menu)
         ("Print to" :menu print-to-menu)
         (nil :divider :line)
         ("Close" :command com-terminal-echo-close)))

(clim:define-command-table terminal-echo-edit-menu
  :menu (("Copy" :command com-terminal-echo-copy)
         ("Paste" :command com-terminal-echo-paste)))

(clim:define-command-table terminal-echo-help-menu
  :menu (("How to Use this Terminal..." :command com-terminal-echo-about)
         ("Skyline-Tool Developers' Guide..." :command nil)
         ("Skyline-Tool Scripting Guide..." :command nil)
         (nil :divider :line)
         ("Skyline-Tool Developers' Guide..." :command nil)))

(defun display-terminal-echo (frame pane)
  "Display the terminal echo buffer."
  (let* ((capture (make-string-output-stream))
         (capturing-stream (make-instance 'clim-simple-echo:capturing-stream
                                          :target pane :capture capture))
         (error-capture (make-string-output-stream))
         (error-capturing-stream (make-instance 'clim-simple-echo:capturing-stream
                                                :target pane :capture error-capture)))
    (let ((*standard-output* capturing-stream)
          (*trace-output* capturing-stream)
          (*error-output* error-capturing-stream)
          (*query-io* capturing-stream)
          (*standard-input* (clim:frame-standard-input frame)))
      (setf (frame-captured-text frame) (get-output-stream-string capture))
      (setf (frame-captured-errors frame) (get-output-stream-string error-capture))
      (when (frame-command frame)
        (funcall (frame-command frame) pane)))))

(defun terminal-echo-pipe (frame pane)
  "Pipe function that reads from process and writes to pane with ANSI parsing.
   Reads character-by-character for real-time output (progress bars, \\r updates)."
  (let ((process (frame-process frame))
        (parser (frame-parser frame)))
    (when process
      (with-open-stream (input (process-output process))
        (loop for char = (read-char input nil nil)
              while char
              do (write-char-with-ansi parser char pane)
                 (force-output pane))
        ;; Process exited
        (let ((exit-status (process-exit-code process)))
          (unless (zerop exit-status)
            (clim:with-output-as-presentation (pane (princ-to-string exit-status) 'string)
              (clim:with-drawing-options (pane :ink clim:+red+)
                (format pane "~2&⚠ ~d~%" exit-status))
              (force-output pane))))))))

(defmethod initialize-instance :after ((frame terminal-echo-frame) &key command)
  (when command
    (setf (frame-command frame) command)
    (let ((cmd-spec (getf command :command))
          (args (getf command :args nil)))
      (let ((full-command (if (listp cmd-spec)
                              cmd-spec
                              (cons cmd-spec args))))
        (setf (frame-process frame)
              (uiop:run-program full-command
                           :input nil
                           :output (make-terminal-echo-pipe :output)
                           :error-output (make-terminal-echo-pipe :error)
                           :wait nil
                           :element-type 'character))))))

;;; Commands

(define-terminal-echo-frame-command (com-terminal-echo-clear :menu t :name t) ()
  "Clear the terminal echo buffer."
  (let ((pane (frame-standard-output *application-frame*)))
    (window-clear pane)
    (let ((parser (frame-parser *application-frame*)))
      (reset-ansi-parser parser))))

(define-terminal-echo-frame-command (com-terminal-echo-close :menu t :name t) ()
  "Close the terminal echo window."
  (frame-exit *application-frame*))

(define-terminal-echo-frame-command (com-terminal-echo-copy :menu t :name t) ()
  "Copy terminal content to clipboard."
  (let ((text (frame-captured-text *application-frame*)))
    (when text
      (%clipboard-copy text))))

(define-terminal-echo-frame-command (com-terminal-echo-kill :menu t :name t) ()
  "Kill the running process."
  (let ((process (frame-process *application-frame*)))
    (when process
      (process-kill process)
      (setf (frame-process *application-frame*) nil))))

(define-terminal-echo-frame-command (com-send-input :menu nil :name "Input")
    ((text 'string :prompt "> "))
  "Send TEXT to the running process's stdin."
  (let* ((frame *application-frame*)
         (process (frame-process frame)))
    (when process
      (with-open-stream (output (uiop:process-info-input process))
        (write-line text output)
        (force-output output)))))

;; Terminal Echo View menu

(defun setup-terminal-echo (&key command (name "Terminal Echo") (width 800) (height 600))
  "Create and show a terminal echo window for running commands.
   COMMAND should be a property list with keys :COMMAND (string or list) and optional :ARGS (list)."
  (let* ((frame (make-application-frame 'terminal-echo-frame
                                        :name "Skyline-Tool"
                                        :pretty-name name
                                        :command command
                                        :width width
                                        :height height)))
    (clim-sys:make-process (lambda ()
                             (run-frame-top-level frame))
                           :name (format nil "Terminal Echo: ~a" name))
    frame))

(defun terminal-echo-clear (frame)
  "Clear the terminal echo buffer."
  (let ((pane (frame-standard-output frame)))
    (window-clear pane)
    (reset-ansi-parser (frame-parser frame))))

(defun terminal-echo-kill (frame)
  "Kill the running process in the terminal echo window."
  (when (frame-process frame)
    (process-kill (frame-process frame))
    (setf (frame-process frame) nil)))

;;; Version Control Status Functions (exported from skyline-tool)

(defun vc-file-status (file-path)
  "Return the version control status of FILE-PATH as a keyword:
   :unmodified, :modified, :added, :deleted, :untracked, :ignored, or :unknown."
  (let ((vc-dir (vc-find-root file-path)))
    (if vc-dir
        (ecase (vc-backend vc-dir)
          (:git (vc-git-file-status file-path vc-dir))
          (:hg (vc-hg-file-status file-path vc-dir))
          (:svn (vc-svn-file-status file-path vc-dir))
          (:bzr (vc-bzr-file-status file-path vc-dir))
          (:unknown))
        :unknown)))

(defun vc-find-root (file-path)
  "Find the version control root directory for FILE-PATH."
  (let ((start-dir (pathname-directory (truename (etypecase file-path
                                                   (string (parse-namestring file-path))
                                                   (pathname file-path))))))
    (loop for dir = start-dir then (uiop:pathname-directory-pathname dir)
          while dir
            thereis (loop for vc-dir in '(".git" ".hg" ".svn" ".bzr")
                            thereis (probe-file (merge-pathnames vc-dir dir))))))

(defun vc-backend (vc-dir)
  "Determine the VC backend from the VC directory."
  (cond
    ((probe-file (merge-pathnames ".git/" vc-dir)) :git)
    ((probe-file (merge-pathnames ".hg/" vc-dir)) :hg)
    ((probe-file (merge-pathnames ".svn/" vc-dir)) :svn)
    ((probe-file (merge-pathnames ".bzr/" vc-dir)) :bzr)
    (t :unknown)))

(defun vc-git-file-status (file-path vc-dir)
  "Get git status for FILE-PATH relative to VC-DIR."
  (let* ((file-truename (truename file-path))
         (vc-truename (truename vc-dir))
         (relative-path (enough-namestring file-truename vc-truename))
         (output (uiop:run-program (list "git" "status" "--porcelain" "--" relative-path)
                                   :output :string
                                   :ignore-error-status t
                                   :directory-string vc-dir)))
    (cond
      ((string= output "") :unmodified)
      ((search "??" output) :untracked)
      ((search "M " output) :modified)
      ((search "A " output) :added)
      ((search "D " output) :deleted)
      ((search "!!" output) :ignored)
      (t :modified))))

(defun vc-hg-file-status (file-path vc-dir)
  (declare (ignore file-path vc-dir))
  :unknown)

(defun vc-svn-file-status (file-path vc-dir)
  (declare (ignore file-path vc-dir))
  :unknown)

(defun vc-bzr-file-status (file-path vc-dir)
  (declare (ignore file-path vc-dir))
  :unknown)

(defun present-vc-status-icon (status)
  "Return a string icon representing the VC STATUS."
  (ecase status
    (:unmodified "✓")
    (:modified "●")
    (:added "+")
    (:deleted "×")
    (:untracked "?")
    (:ignored "!")
    (:unknown "?")))

(defun terminal-echo-present-vc-status (frame file-path)
  "Display VC status icon for FILE-PATH in the terminal echo window."
  (let ((status (vc-file-status file-path)))
    (present-vc-status-icon status)))

(defun run-command-in-terminal-echo (command &key title)
  (make-window-thread
   (or title (format nil "Running command ~s" command))
   (lambda ()
     (let* ((fm (ignore-errors (clim:find-frame-manager :port (clim:find-port))))
            (frame (clim:make-application-frame 'terminal-echo-frame
                                                :pretty-name title
                                                :frame-manager fm)))
       (setf (frame-command frame) command)
       (clim:run-frame-top-level frame)))))
