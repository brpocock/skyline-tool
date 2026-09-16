;;; gui/keyboard-shortcuts.lisp
;;; Keyboard shortcuts themes for Skyline-Tool
;;; Implements Gnome (Windows), macOS, and Emacs key bindings
;;;
;;; Modifier mapping:
;;; - Gnome: :control (shown as ^)
;;; - macOS: :meta (shown as ⌘, represents Command key) and :alt (shown as ⌥, represents Option key) and :control (shown as ^)
;;; - Emacs: :control and :meta (shown as C- and M-)

(in-package :skyline-tool)

;; Use ESA's set-key for proper multi-key chord handling

;; Sun keys support
(clim:define-command (com-sun-stop :command-table clim-internals::global-command-table) ()
  "Sun key: Stop/cancel/abort."
  (com-stop-build))

(clim:define-command (com-sun-again :command-table clim-internals::global-command-table) ()
  "Sun key: Redo."
  (com-redo))

(clim:define-command (com-sun-props :command-table clim-internals::global-command-table) ()
  "Sun key: Properties/Information."
  (com-help-for-window))

(clim:define-command (com-sun-undo :command-table clim-internals::global-command-table) ()
  "Sun key: Undo."
  (com-undo))

(clim:define-command (com-sun-cut :command-table clim-internals::global-command-table) ()
  "Sun key: Cut."
  (com-cut))

(clim:define-command (com-sun-copy :command-table clim-internals::global-command-table) ()
  "Sun key: Copy."
  (com-copy))

(clim:define-command (com-sun-paste :command-table clim-internals::global-command-table) ()
  "Sun key: Paste."
  (com-paste))

(clim:define-command (com-sun-open :command-table clim-internals::global-command-table) ()
  "Sun key: Open."
  (com-open-go-to))

(clim:define-command (com-sun-find :command-table clim-internals::global-command-table) ()
  "Sun key: Find."
  (com-find))

;;; Command definitions for actions that may not exist yet

(clim:define-command (com-new-resource :command-table clim-internals::global-command-table) ()
  "Create a new resource/file."
  (com-create-new-resource))

(clim:define-command (com-import-file :command-table clim-internals::global-command-table) ()
  "Open/import a file."
  (com-open-go-to))

(clim:define-command (com-edit-preferences :command-table clim-internals::global-command-table) ()
  "Edit preferences."
  (com-edit-skyline-config-prefs))

(clim:define-command (com-stop-build :command-table clim-internals::global-command-table) ()
  "Stop/cancel/abort current operation."
  (error "unimplemented"))

(clim:define-command (com-select-all :command-table clim-internals::global-command-table) ()
  "Select all content."
  (error "unimplemented"))

(clim:define-command (com-redo :command-table clim-internals::global-command-table) ()
  "Redo last undone action."
  (error "unimplemented"))

(clim:define-command (com-new-tab :command-table clim-internals::global-command-table) ()
  "New tab / switch tab."
  (error "unimplemented"))

(clim:define-command (com-tab-prev :command-table clim-internals::global-command-table) ()
  "Switch to previous tab in the current inspector."
  (let ((frame clim:*application-frame*))
    (when (and frame (typep frame 'tab-friendly-mixin))
      (let* ((tabs (frame-tab-list frame))
             (current (frame-current-tab frame))
             (pos (position current tabs)))
        (when (and tabs pos)
          (switch-to-tab frame (elt tabs (mod (1- pos) (length tabs)))))))))

(clim:define-command (com-tab-next :command-table clim-internals::global-command-table) ()
  "Switch to next tab in the current inspector."
  (let ((frame clim:*application-frame*))
    (when (and frame (typep frame 'tab-friendly-mixin))
      (let* ((tabs (frame-tab-list frame))
             (current (frame-current-tab frame))
             (pos (position current tabs)))
        (when (and tabs pos)
          (switch-to-tab frame (elt tabs (mod (1+ pos) (length tabs)))))))))

(clim:define-command (com-switch-location :command-table clim-internals::global-command-table) ()
  "Switch location / go to resource/buffer/file by name/id."
  (com-open-go-to))

(clim:define-command (com-zoom-in :command-table clim-internals::global-command-table) ()
  "Zoom in."
  (error "unimplemented"))

(clim:define-command (com-zoom-out :command-table clim-internals::global-command-table) ()
  "Zoom out."
  (error "unimplemented"))

(clim:define-command (com-zoom-reset :command-table clim-internals::global-command-table) ()
  "Reset zoom level to default."
  (error "unimplemented"))

(clim:define-command (com-run-make :command-table clim-internals::global-command-table) ()
  "Run 'make' build for current game and region."
  (error "unimplemented"))

;;; Cursor navigation commands (stubs)
(clim:define-command (com-beginning-of-line :command-table clim-internals::global-command-table) ()
  "Move to beginning of line."
  (error "unimplemented"))

(clim:define-command (com-end-of-line :command-table clim-internals::global-command-table) ()
  "Move to end of line."
  (error "unimplemented"))

(clim:define-command (com-backward-char :command-table clim-internals::global-command-table) ()
  "Move backward one character."
  (error "unimplemented"))

(clim:define-command (com-forward-char :command-table clim-internals::global-command-table) ()
  "Move forward one character."
  (error "unimplemented"))

(clim:define-command (com-previous-line :command-table clim-internals::global-command-table) ()
  "Move to previous line."
  (error "unimplemented"))

(clim:define-command (com-next-line :command-table clim-internals::global-command-table) ()
  "Move to next line."
  (error "unimplemented"))

(clim:define-command (com-delete-backward :command-table clim-internals::global-command-table) ()
  "Delete character backward."
  (error "unimplemented"))

(clim:define-command (com-delete-forward :command-table clim-internals::global-command-table) ()
  "Delete character forward."
  (error "unimplemented"))

;; Helper function to bind a key to a command in the global command table
(defun bind-key (key modifier command)
  "Bind KEY with MODIFIER to COMMAND in the global command table."
  (let ((gct 'clim-internals::global-command-table))
    (clim:add-keystroke-to-command-table gct :keystroke (list key modifier) command)))

;; Helper to bind ESA multi-key chords properly
(defun bind-esa-chord (keys command)
  "Bind a multi-key chord sequence using ESA's proper set-key mechanism."
  (esa:set-key command 'clim-internals::global-command-table keys))

;; Define key bindings for each theme
(defun apply-gnome-bindings ()
  "Apply Gnome-style key bindings using :control modifier."
  ;; Quit application
  (bind-key #\q :control 'com-quit-skyline-tool)
  ;; Close window
  (bind-key #\w :control 'com-close-frame)
  ;; Information/Help
  (bind-key #\i :control 'com-help-for-window)
  ;; Open/import file
  (bind-key #\o :control 'com-import-file)
  ;; Select all
  (bind-key #\a :control 'com-select-all)
  ;; Save
  (bind-key #\s :control 'com-save-default)
  ;; Duplicate
  (bind-key #\d :control 'com-duplicate)
  ;; Find
  (bind-key #\f :control 'com-find)
  ;; Redo
  (bind-key #\r :control 'com-redo)
  ;; New tab / switch tab
  (bind-key #\t :control 'com-new-tab)
  ;; Switch location
  (bind-key #\l :control 'com-switch-location)
  ;; Undo
  (bind-key #\z :control 'com-undo)
  ;; Cut
  (bind-key #\x :control 'com-cut)
  ;; Copy
  (bind-key #\c :control 'com-copy)
  ;; Paste
  (bind-key #\v :control 'com-paste)
  ;; New resource/file
  (bind-key #\n :control 'com-new-resource)
  ;; Preferences
  (bind-key #\, :control 'com-edit-preferences)
  ;; Stop/cancel/abort
  (bind-key #\. :control 'com-stop-build)
  ;; Escape cancel
  (bind-key #\Escape :none 'com-stop-build)
  ;; Zoom in
  (bind-key #\+ :control 'com-zoom-in)
  ;; Zoom out
  (bind-key #\- :control 'com-zoom-out)
  ;; Reset zoom
  (bind-key #\0 :control 'com-zoom-reset)
  ;; Cursor navigation
  (bind-key #\a :control 'com-beginning-of-line)
  (bind-key #\e :control 'com-end-of-line)
  (bind-key #\b :control 'com-backward-char)
  (bind-key #\f :control 'com-forward-char)
  (bind-key #\p :control 'com-previous-line)
  (bind-key #\n :control 'com-next-line)
  (bind-key #\h :control 'com-delete-backward)
  (bind-key #\d :control 'com-delete-forward)
  ;; Help keys
  (bind-key #\/ :control 'com-help-for-window)
  (bind-key #\? :control 'com-help-for-window)
  (bind-key #\/ :meta 'com-help-for-window)
  (bind-key #\? :meta 'com-help-for-window)
  (bind-key #\h :meta 'com-help-for-window)
  (bind-key :f1 :none 'com-help-for-window)
  ;; Build key: F5, M-Return, Execute
  (bind-key :f5 :none 'com-run-make)
  (bind-key #\Return :meta 'com-run-make)
  (bind-key :execute :none 'com-run-make)
  ;; Tab switching: C-PgUp, C-PgDn
  (bind-key :prior :control 'com-tab-prev)
  (bind-key :next :control 'com-tab-next))

(defun apply-macos-bindings ()
  "Apply macOS style key bindings using :meta modifier (Command key).
   As per user specification: 'macOS uses Command (Meta)' - so we bind to :meta
   and show ⌘ symbol in presentation."
  ;; Quit application
  (bind-key #\q :meta 'com-quit-skyline-tool)
  ;; Close window
  (bind-key #\w :meta 'com-close-frame)
  ;; Information/Help
  (bind-key #\i :meta 'com-help-for-window)
  ;; Open/import file
  (bind-key #\o :meta 'com-import-file)
  ;; Select all
  (bind-key #\a :meta 'com-select-all)
  ;; Save
  (bind-key #\s :meta 'com-save-default)
  ;; Duplicate
  (bind-key #\d :meta 'com-duplicate)
  ;; Find
  (bind-key #\f :meta 'com-find)
  ;; Redo
  (bind-key #\r :meta 'com-redo)
  ;; New tab / switch tab
  (bind-key #\t :meta 'com-new-tab)
  ;; Switch location
  (bind-key #\l :meta 'com-switch-location)
  ;; Undo
  (bind-key #\z :meta 'com-undo)
  ;; Cut
  (bind-key #\x :meta 'com-cut)
  ;; Copy
  (bind-key #\c :meta 'com-copy)
  ;; Paste
  (bind-key #\v :meta 'com-paste)
  ;; New resource/file
  (bind-key #\n :meta 'com-new-resource)
  ;; Preferences
  (bind-key #\, :meta 'com-edit-preferences)
  ;; Stop/cancel/abort
  (bind-key #\. :meta 'com-stop-build)
  ;; Escape cancel
  (bind-key #\Escape :none 'com-stop-build)
  ;; Zoom in
  (bind-key #\+ :meta 'com-zoom-in)
  ;; Zoom out
  (bind-key #\- :meta 'com-zoom-out)
  ;; Reset zoom
  (bind-key #\0 :meta 'com-zoom-reset)
  ;; Cursor navigation (as specified: works in both macOS and Emacs mode)
  (bind-key #\a :control 'com-beginning-of-line)
  (bind-key #\e :control 'com-end-of-line)
  (bind-key #\b :control 'com-backward-char)
  (bind-key #\f :control 'com-forward-char)
  (bind-key #\p :control 'com-previous-line)
  (bind-key #\n :control 'com-next-line)
  (bind-key #\h :control 'com-delete-backward)
  (bind-key #\d :control 'com-delete-forward)
  ;; Help keys
  (bind-key #\/ :meta 'com-help-for-window)
  (bind-key #\? :meta 'com-help-for-window)
  (bind-key #\h :meta 'com-help-for-window)
  (bind-key :f1 :none 'com-help-for-window)
  ;; Build key
  (bind-key :f5 :none 'com-run-make)
  (bind-key #\Return :meta 'com-run-make)
  (bind-key :execute :none 'com-run-make)
  ;; Tab switching: M-PgUp, M-PgDn
  (bind-key :prior :meta 'com-tab-prev)
  (bind-key :next :meta 'com-tab-next))

(defun apply-emacs-bindings ()
  "Apply Emacs style key bindings using :control and :meta modifiers.
   Emacs uses C- for Control, M- for Meta as per user specification."
  ;; Information/Help
  (bind-key #\h :control 'com-help-for-window)
  ;; Find
  (bind-key #\s :control 'com-find)
  ;; Undo
  (bind-key #\_ :control 'com-undo)
  ;; Cut
  (bind-key #\w :control 'com-cut)
  ;; Copy
  (bind-key #\w :meta 'com-copy)
  ;; Paste
  (bind-key #\y :control 'com-paste)
  ;; Preferences
  (bind-key #\, :control 'com-edit-preferences)
  ;; Stop/cancel/abort
  (bind-key #\. :control 'com-stop-build)
  ;; Escape cancel
  (bind-key #\Escape :none 'com-stop-build)
  ;; Zoom in
  (bind-key #\+ :control 'com-zoom-in)
  ;; Zoom out
  (bind-key #\- :control 'com-zoom-out)
  ;; Reset zoom
  (bind-key #\0 :control 'com-zoom-reset)
  ;; Cursor navigation (same as in macOS mode, as specified)
  (bind-key #\a :control 'com-beginning-of-line)
  (bind-key #\e :control 'com-end-of-line)
  (bind-key #\b :control 'com-backward-char)
  (bind-key #\f :control 'com-forward-char)
  (bind-key #\p :control 'com-previous-line)
  (bind-key #\n :control 'com-next-line)
  (bind-key #\d :control 'com-delete-forward)
  ;; Note: C-h for delete backward is omitted because C-h is used for help.
  ;; Help keys
  (bind-key #\/ :control 'com-help-for-window)
  (bind-key #\? :control 'com-help-for-window)
  (bind-key #\/ :meta 'com-help-for-window)
  (bind-key #\? :meta 'com-help-for-window)
  (bind-key #\h :meta 'com-help-for-window)
  (bind-key :f1 :none 'com-help-for-window)
  ;; Build key
  (bind-key :f5 :none 'com-run-make)
  (bind-key #\Return :meta 'com-run-make)
  (bind-key :execute :none 'com-run-make)
  ;; Tab switching: M-<, M->
  (bind-key #\< :meta 'com-tab-prev)
  (bind-key #\> :meta 'com-tab-next)
  ;; C-x prefix chords using ESA for proper multi-key sequence support
  (bind-esa-chord '((#\x :control) (#\c :control)) 'com-quit-skyline-tool)
  (bind-esa-chord '((#\x :control) (#\k :control)) 'com-close-frame)
  (bind-esa-chord '((#\x :control) (#\i :control)) 'com-import-file)
  (bind-esa-chord '((#\x :control) (#\h :control)) 'com-select-all)
  (bind-esa-chord '((#\x :control) (#\s :control)) 'com-save-default)
  (bind-esa-chord '((#\x :control) (#\t :control)) 'com-duplicate)
  (bind-esa-chord '((#\x :control) (#\b :control)) 'com-switch-location)
  (bind-esa-chord '((#\x :control) (#\f :control)) 'com-new-resource))

;;; Apply platform-appropriate default only when preferences are missing.
;;; This default is used as fallback; it does NOT overwrite existing preferences.
(defun software-is-mac-os-p ()
  "Return T if running on macOS/Darwin."
  (or (search "darwin" (string-downcase (software-type)))
      (search "macos" (string-downcase (software-type)))))

(defun default-shortcut-theme ()
  (if (software-is-mac-os-p)
      :macos
      :gnome))

(defun apply-shortcut-theme-bindings ()
  "Apply key bindings for the current shortcut theme.
Uses the stored preference if present, otherwise falls back to the platform default."
  (let ((theme (get-pref :shortcut-theme (default-shortcut-theme))))
    (case theme
      (:gnome (apply-gnome-bindings))
      (:macos (apply-macos-bindings))
      (:emacs (apply-emacs-bindings))
      (t (apply-gnome-bindings)))))
