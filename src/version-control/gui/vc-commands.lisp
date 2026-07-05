;;; src/version-control/gui/vc-commands.lisp
;;; Command definitions for Version Control menu actions
;;;
;;; These commands are registered in the global command table and
;;; used by the resource presentation frames.

(in-package :skyline-tool.version-control)

(clim:define-command (com-vc-staged :command-table clim-internals::global-command-table :name t)
  ((resource "unified-asset-entry" "gesture" "select"))
  (lambda ()
    (when file-path
      (case status
        (:staged (vc-reset (make-git-backend) (list file-path)))
        ((:modified :untracked) (vc-add (make-git-backend) (list file-path))
                                  (vc-commit (make-git-backend) "Staged via menu"))
        (t (format t "Cannot stage ~a in status ~a~%" file-path status))))))

(clim:define-command (com-vc-revert :command-table clim-internals::global-command-table :name t)
  ((resource "unified-asset-entry" "gesture" "select"))
  (lambda ()
    (when file-path
      (show-vc-revert-dialog file-path))))

(clim:define-command (com-vc-compare :command-table clim-internals::global-command-table :name t)
    ((resource "unified-asset-entry" "gesture" "select"))
  (lambda ()
    (when file-path
      (show-vc-compare-dialog file-path))))

(clim:define-command (com-vc-commit :command-table clim-internals::global-command-table :name t)
    ((resource "unified-asset-entry" "gesture" "select"))
  (lambda ()
    (show-vc-commit-dialog file-path)))

(clim:define-command (com-vc-track :command-table clim-internals::global-command-table :name t)
    ((resource "unified-asset-entry" "gesture" "select"))
  (lambda ()
    (when file-path
      (confirm-dialog (format nil "Track ~a in Version Control?" file-path)
                      :default-action :leave
                      :danger-action :remove
                      :action (lambda (action)
                                (vc-set-tracked-status file-path (eq action :remove))
                                (fixme-restart-app *application-frame*))))))

(clim:define-command (com-vc-ignore :command-table clim-internals::global-command-table :name t)
    ((resource "unified-asset-entry" "gesture" "select"))
  (lambda ()
    (when file-path
      (let ((new-state (not (vc-get-ignored-status file-path)))
            (msg (format nil "Set ~a as Ignored?" file-path)))
        (clim:accepting-values (stream)
          (format stream "~a~%" msg)
          (when (clim:accept 'symbol :prompt "Confirm" 
                                     :possibilities '(Yes No) :default 'Yes)
            (vc-set-ignored-status file-path new-state)
            (fixme-restart-app *application-frame*)))))))
