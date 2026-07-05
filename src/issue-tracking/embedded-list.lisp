;;; src/issue-tracking/embedded-list.lisp
;;; Embedded issue list for Skyline-Tool presentations

(in-package :skyline-tool.issue-tracking)

;;
;; Issue List Presentation Type
;;

(clim:define-presentation-type embedded-issue-list ()
  :options ((project-root nil)) :description "Path to game project root")

;;
;; Fetch and Filter Issues
;;

(defun fetch-current-user-issues (&optional (backend (default-issue-tracker)))
  "Fetch open issues assigned to current user from BACKEND"
  (let* ((user (get-current-user backend))
         (all-issues (list-issues backend :status :open)))
    (remove-if-not (lambda (issue)
                     (string-equal (issue-assignee issue) user))
                   all-issues)))

(defun get-current-user (backend)
  "Get current user identifier for BACKEND"
  (or (vc-user-name (make-git-backend))
      (username backend)
      "unknown"))

(defun default-issue-tracker ()
  "Return the configured issue tracker for the current project"
  (let ((config (load-vc-config)))
    (ecase (getf config :issue-tracker)
      (:github (make-github-client))
      (:gitlab (make-gitlab-client))
      (:bugzilla (make-bugzilla-client))
      (nil (error "No issue tracker configured")))))

;;
;; Issue List Presentation
;;

(defun present-embedded-issue-list (stream project-root &key (max-items 10) (sort-by :updated))
  "Present an embedded list of issues for PROJECT-ROOT.
   Shows brief descriptions with branch link action.
   Format: (Assigned / Total) e.g. (7 / 15)
   SORT-BY can be :updated, :id, or :priority."
  (let* ((issues (fetch-current-user-issues))
         (all-issues (list-issues (default-issue-tracker) :status :open))
         (assigned-count (length issues))
         (total-count (length all-issues))
         (last-updated (find-last-updated-issue all-issues)))
    ;; Header with assignment stats
    (clim:formatting-table (stream :x-spacing 10 :y-spacing 2)
      (clim:formatting-row
          (stream)
        (clim:formatting-cell (stream) (format stream "(~a / ~a)" assigned-count total-count)))
      ;; Subheading for last updated
      (when last-updated
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream :align-x :left)
            (let ((age (issue-age last-updated))
                  (color (issue-age-color last-updated)))
              (clim:with-drawing-options (stream :ink color)
                (format stream "#~a updated: ~a" 
                        (issue-id last-updated)
                        (format-timestring nil (issue-updated last-updated)))))))))
    ;; Issue list
    (clim:formatting-table (stream :x-spacing 10 :y-spacing 2)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream) (write-string "Issue" stream))
        (clim:formatting-cell (stream) (write-string "Title" stream))
        (clim:formatting-cell (stream) (write-string "Status" stream)))
      (let ((sorted-issues (sort issues 
                                 (lambda (a b)
                                   (case sort-by
                                     (:id (< (issue-id a) (issue-id b)))
                                     (:priority (< (issue-priority a) (issue-priority b)))
                                     (:updated (> (issue-updated a) (issue-updated b)))
                                     (t (> (issue-updated a) (issue-updated b))))))))
        (dolist (issue (subseq sorted-issues 0 (min max-items (length sorted-issues))))
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream)
              (format stream "~a" (issue-id issue)))
            (clim:formatting-cell (stream)
              (format stream "~a" (issue-title issue)))
            (clim:formatting-cell (stream)
              (format stream "~a" (issue-status issue)))))))))


(defun find-last-updated-issue (issues)
  "Find the most recently updated issue from ISSUES list"
  (when issues
    (reduce (lambda (a b)
              (if (> (issue-updated a) (issue-updated b)) a b))
            issues)))

(defun issue-age (issue)
  "Return the age of ISSUE in seconds since last update"
  (- (get-universal-time) (issue-updated issue)))

(defun issue-age-color (issue)
  "Return CLIM color based on issue age.
   Red if < 30 min, black if < 24 hrs, gray otherwise"
  (let ((age (issue-age issue)))
    (cond
      ((< age (* 30 60)) clim:+red+)        ; 30 minutes
      ((< age (* 24 60 60)) clim:+black+)   ; 24 hours
      (t clim:+gray+))))

;;
;; Branch Linking Action
;;

(defun link-issue-to-branch (issue &optional (project-root (uiop:getcwd)))
  "Switch the game project to a new branch linked with ISSUE.
   Creates branch named 'issue-<id>-<slug>' if it doesn't exist."
  (let* ((branch-name (format nil "issue-~a-~a" 
                              (issue-id issue)
                              (slugify (issue-title issue))))
         (git (skyline-tool.version-control:make-git-backend)))
    ;; Check if branch exists
    (let ((branches (skyline-tool.version-control:vc-branch git :list t)))
      (unless (member branch-name branches :test #'string-equal)
        ;; Create new branch from current HEAD
        (skyline-tool.version-control:vc-branch git :create branch-name)))
    ;; Checkout the branch
    (skyline-tool.version-control:vc-checkout git branch-name)
    ;; Store issue reference in config
    (setf (gethash :linked-issue *vc-config*) (issue-id issue))
    (format t "Switched to branch ~a linked with issue ~a~%" branch-name (issue-id issue))))

(defun slugify (string)
  "Convert STRING to a URL-safe slug"
  (cl-ppcre:regex-replace-all "[^a-zA-Z0-9]+" 
                              (string-downcase string) "-"))

;;
;; Menu Commands for Issue Items
;;

(defun issue-menu-commands (issue project-root)
  "Return menu commands for an ISSUE presentation"
  (list (cons "Open in Browser" 
              (lambda () (xdg-open (issue-url issue))))
        (cons "Link Game Branch" 
              (lambda () (link-issue-to-branch issue project-root)))
        (cons "Copy Issue ID" 
              (lambda () (copy-to-clipboard (format nil "~a" (issue-id issue)))))))

(defun xdg-open (url)
  "Open URL in system browser"
  (uiop:run-program (list "xdg-open" url) :output nil :error-output nil))

(defun copy-to-clipboard (text)
  "Copy TEXT to clipboard"
  (uiop:run-program (list "xclip" "-selection" "clipboard") 
                    :input text :output nil))

;;
;; Inspector Integration
;;

(defun draw-issue-inspector (stream issue project-root)
  "Draw issue details in inspector pane"
  (clim:formatting-table (stream :x-spacing 20)
    (dolist (field '(id title status priority assignee labels created updated))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "~a:" (string-capitalize (symbol-name field))))
        (clim:formatting-cell (stream)
          (format stream "~a" (funcall field issue)))))
    ;; Branch link status
    (let ((linked-branch (gethash :linked-issue *vc-config*)))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right) (write-string "Linked Branch:" stream))
        (clim:formatting-cell (stream)
          (if linked-branch
              (format stream "~a" linked-branch)
              (write-string "None" stream)))))
    ;; Action buttons
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream) (write-string "" stream))
      (clim:formatting-cell (stream)
        (clim:with-output-as-presentation
            (stream issue 'embedded-issue-list)
          (format stream "Link Branch")))
      (clim:formatting-cell (stream)
        (clim:with-output-as-presentation
            (stream (issue-url issue) 'url)
          (format stream "Open in Browser"))))))
