(in-package :skyline-tool)

(defvar *launcher-frame* nil)

(clim:define-application-frame launcher-frame ()
  ((%decal-index :initform 0 :accessor decal-index :initarg :index))
  (:panes (menu-list-pane :application :height 700 :width 450
                                       :display-function 'display-launcher-menu)
          (interactor :interactor :height 125 :width 450
                                  :max-height 125))
  (:layouts (default (clim:vertically () menu-list-pane interactor)))
  (:icon (mapcar
          (lambda (name)
            (clim:make-pattern-from-bitmap-file
             (asdf:system-relative-pathname :skyline-tool name)))
          (list #p"../Tools/skyline-tool-icon-128.png"
                #p"../Tools/skyline-tool-icon-64.png"))))

(defun run-tiled ()
  "Open the project in Tiled"
  (uiop:run-program (list "make" "pngs"))
  (uiop:run-program (list "tiled" (format nil "Source/Maps/~a.tiled-project" *game-title*))))

(defun open-file-manager ()
  "Open your File Manager in the project folder"
  (uiop:run-program (list "xdg-open" "./")))

(define-constant +launcher-entries+
    '(skyline-tool
      (files
       open-file-manager
       run-tiled
       check-for-absent-assets-in-project-folder
       show-rom-budget)
      (animation-editor
       assign-animation-sequences
       edit-animation-sequence)
      (emulator
       run-script
       play-script-on-atarivox)
      (7800-game-drive
       push-binary-to-7800-game-drive
       shove-binary-into-running-7800-game-drive)
      (core-dump-display
       show-dll-from-dump
       show-dlbam
       copy-dump-as-dump2
       compare-dlls-from-dumps
       show-animation-buffer
       show-decal)
      (core-dump-general
       analyze-faults-from-dump
       show-dialogue-buffers
       show-map
       show-forth-stack)
      (core-dump-objects
       show-player-object
       show-self-object
       show-all-objects
       show-room-for-objects)
      (lisp
       run-repl
       show-lisp-room
       reload-skyline-tool-from-sources))
  :test 'equalp)

(defun display-launcher-menu-item (entry pane)
  (if (consp entry)
      (progn
        (clim:with-text-face (pane :bold)
          (clim:with-text-size (pane :larger)
            (format pane "~2%~a~%"
                    (cl-change-case:title-case (string (first entry))))))
        (clim:with-text-size (pane :smaller)
          (dolist (sub-entry (rest entry))
            (display-launcher-menu-item sub-entry pane))))
      (clim:present entry 'nullary-function-name :stream pane)))

(defmethod display-launcher-menu ((frame launcher-frame) (pane clim:pane))
  (clim:with-text-size (pane :larger)
    (display-launcher-menu-item (copy-list +launcher-entries+) pane))
  (clim:with-text-size (pane :small)
    (format pane "~3%Click the name of any function to launch it")))

(clim:define-presentation-type nullary-function-name () :inherit-from 'symbol)

(clim:define-presentation-method clim:present
    (function-name (type nullary-function-name) stream view &key)
  (clim:with-text-size (stream :larger)
    (format stream "~%~4t~a" (cl-change-case:title-case (string function-name))))
  (when-let (doc (documentation function-name 'function))
    (format stream "~%~a" (first-line doc)))
  (terpri stream))

(define-launcher-frame-command (com-run-nullary-function :menu nil :name t)
    ((function-name 'nullary-function-name :gesture :select))
  (clim-sys:make-process (lambda () (funcall function-name))
                         :name (cl-change-case:title-case (string function-name))))

(defun show-dll-from-dump ()
  "Show the decoded Display List List from the core dump"
  (clim-simple-echo:run-in-simple-echo #'decode-dll-from-dump
                                       :process-name "Display List List decoded"
                                       :height 768))

(defun show-dlbam ()
  "Show the status of the BAM for Display Lists"
  (clim-simple-echo:run-in-simple-echo #'decode-dlbam
                                       :process-name "Display List Block Allocation Map"
                                       :height 512))

(defun check-for-absent-assets-in-project-folder ()
  "Check the project folder for assets that are not mentioned in the Assets.index"
  (clim-simple-echo:run-in-simple-echo #'check-for-absent-assets
                                       :process-name "Check for absent assets"))

(defun show-lisp-room ()
  "Check how much room (in memory) this Lisp image is using"
  (clim-simple-echo:run-in-simple-echo (lambda ()
                                         (format t "~&Running a full garbage collection…")
                                         (force-output)
                                         (sb-ext:gc :full t)
                                         (format t "~&Ready. (ROOM) says:~%")
                                         (room))
                                       :process-name "Room"))

(defun push-binary-to-7800-game-drive ()
  "Push the latest binary to the 7800GD over its serial (debug) port"
  (clim-simple-echo:run-in-simple-echo
   (lambda () (push-7800gd-bin (format nil "Dist/~a.Public.NTSC.bin" *game-title*)))
   :process-name "Push binary"))

(defun shove-binary-into-running-7800-game-drive ()
  "Update the running game with a new binary image
 
Should try to just reload the current scene, if there were no code (only
asset) changes, this may work."
  (clim-simple-echo:run-in-simple-echo
   (lambda ()
     (push-7800gd-bin-no-execute (format nil "Dist/~a.Public.NTSC.bin" *game-title*)
                                 (find-7800gd-serial-port) t))
   :process-name "Shove binary into running system"))

(defun explain-error-code (error-code)
  (let ((codes-file (make-pathname :directory (list :relative "Dist")
				   :name (format nil "~a.error.codes" *game-title*)
                                   :type "tsv")))
    (unless (probe-file codes-file)
      (uiop:run-program (list "make" (enough-namestring codes-file))))
    (with-input-from-file (codes codes-file)
      (loop for line = (read-line codes nil nil)
            while line
            do (destructuring-bind (code text source) (split-sequence #\Tab line)
                 (when (string-equal code error-code)
                   (format t "~&~10tCode: “~@:(~a~)” — “~a”~%~14tFound in: ~a"
                           code text source)
                   (return-from explain-error-code t))))))
  (format t "~&~10t(code not identified)"))

(defun analyze-dump-faults (&optional (dump-pathname #p"/tmp/dump"))
  "Report on the fault codes logged in a core dump at DUMP-PATHNAME"
  (let ((mem (load-dump-into-mem dump-pathname))
        (break-signal (find-label-from-files "BreakSignal"))
        (minor-fault-count (find-label-from-files "MinorFaultCount"))
        (last-minor-fault (find-label-from-files "LastMinorFault")))
    (format t "~&Analyzing faults found in dump …")
    (let ((break-bytes (list (dump-peek break-signal mem)
                             (dump-peek (+ 1 break-signal) mem)
                             (dump-peek (+ 2 break-signal) mem)
                             (dump-peek (+ 3 break-signal) mem))))
      (unless (every #'zerop break-bytes)
        (let ((break-code (coerce (mapcar #'minifont->char break-bytes) 'string)))
          (format t "~2%A DebugBreak occurred, which will result in the crash screen.
The signal code was ~a" break-code)
          (explain-error-code break-code))))
    (let ((faults-count (+ (dump-peek minor-fault-count mem)
                           (* #x100 (dump-peek (+ 1 minor-fault-count) mem)))))
      (unless (zerop faults-count)
        (format t "~2%There have been ~:d minor fault~:p reported" faults-count)))
    (let ((fault-bytes (vector (dump-peek last-minor-fault mem) 
                               (dump-peek (+ 1 last-minor-fault) mem) 
                               (dump-peek (+ 2 last-minor-fault) mem) 
                               (dump-peek (+ 3 last-minor-fault) mem))))
      (unless (every #'zerop fault-bytes)
        (let ((fault-code (minifont->unicode fault-bytes :replace #\?)))
          (format t "~2%The last minor fault signal code was ~a" fault-code)
          (explain-error-code fault-code))))
    (terpri) (terpri) (force-output))) 

(defun analyze-faults-from-dump (&optional (dump-pathname #p"/tmp/dump"))
  "Report on the fault codes logged in a core dump at DUMP-PATHNAME in a new window"
  (clim-simple-echo:run-in-simple-echo (lambda ()
                                         (analyze-dump-faults dump-pathname))
                                       :process-name "Analyze Dump Faults"))

(defun reload-skyline-tool-from-sources ()
  "Recompile and reload this utility from the current sources on disk."
  (clim-simple-echo:run-in-simple-echo (lambda ()
                                         (when *launcher-frame*
                                           (clim:frame-exit *launcher-frame*))
                                         (recompile-tool))
                                       :process-name "Recompile Skyline-Tool"))

(defun read-asset-bank-size (bank build region)
  (with-input-from-file (size-file (allocation-size-name bank build region))
    (loop for line = (read-line size-file)
          when (let ((at-pos (position #\@ line))
                     (tab-pos (position #\Tab line)))
                 (and at-pos (zerop at-pos) tab-pos (= 1 tab-pos)))
            do (return-from read-asset-bank-size (parse-integer line :start 2)))
    (error "Could not figure out size of bank $~2,'0x for ~a ~a" bank build region)))

(defun rom-budget ()
  (format t "(writing master Makefile first …")
  (force-output)
  (write-master-makefile)
  (format t "ready.)")
  (dolist (build '("Public"))
    (dolist (region '(ntsc) )
      (format t "~2&Build: ~a~20tRegion: ~a" build region)
      (let ((sum 0))
        (dotimes (bank #x40)
          (cond
            ((or (< bank (first-assets-bank build)) (= bank #x3f))
             (let ((size (ignore-errors
                          (parse-integer
                           (remove-if-not #'digit-char-p
                                          (read-file-into-string
                                           (make-pathname :directory '(:relative "Source" "Generated")
                                                          :type "size"
                                                          :name (format nil "Bank~(~2,'0x~).Public.NTSC" bank))))))))
               (unless size
                 (setf size #x4000)
                 (format t "~%Bank $~2,'0x *size file not parsed" bank))
               (when (= bank #x3f)
                 (let ((sh-size (with-input-from-file (sh #p"Object/Stagehand.o")
                                  (file-length sh))))
                   (format t "~%Stagehand included in Bank $3F — $~4,'0x (~:d)" sh-size sh-size)
                   (incf size sh-size)))
               (incf sum size)
               (format t "~%Bank $~2,'0x — $~4,'0x (~:d) (~d%)"
                       bank size size (round (/ size 163.84)))))
            ((= bank #x3e)
             (format t "~%Bank $3E — unavailable on 7800GD")
             (incf sum #x4000))
            (t
             (let ((size (read-asset-bank-size bank build region)))
               (format t "~&Bank $~2,'0x — $~4,'0x (~:d) (~d%)" bank size size (round (/ size 163.84)))
               (incf sum size)))))
        (format t "~% … total for ~a ~a: $~6,'0x = ~:d = ~:d kiB (~d%)"
                build region sum sum (floor sum 1024) (round (* 100 (/ sum (* 1024 1024)))))))))

(defun show-rom-budget ()
  "ROM Budget report"
  (clim-simple-echo:run-in-simple-echo (lambda ()
                                         (rom-budget))
                                       :process-name "Show ROM Budget"))

(defun launcher ()
  "Open the Skyline Tool launcher (main menu)"
  (let* ((frame (clim:make-application-frame 'launcher-frame))
         (*launcher-frame* frame))
    (setf (clim:frame-pretty-name frame) (format nil "Skyline Tool for ~a: Launcher"
                                                 (cl-change-case:title-case *game-title*))
          *default-pathname-defaults*
          (let ((skyline-dir (asdf:system-source-directory 
                              (asdf:find-system :skyline-tool))))
            (make-pathname :defaults skyline-dir
                           :directory (butlast (pathname-directory skyline-dir)))))
    (sb-posix:chdir (namestring *default-pathname-defaults*))
    (clim:run-frame-top-level frame)))

(defun run-launcher ()
  "Open the Skyline Tool launcher (main menu) in its own thread"
  (let ((*trace-output* (make-synonym-stream '*trace-output*)))
    (handler-case
        (clim-sys:make-process #'launcher
                               :name "Skyline Tool GUI Launcher")
      (xlib:window-error ()
        (invoke-restart 'restart-event-loop) ))))

(defun cl-user::skyline-tool ()
  "Open the Skyline Tool launcher (main menu)"
  (run-launcher))

