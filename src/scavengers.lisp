;;; Skyline-Tool src/scavengers.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC
;;; Consolidated scavenger functions for resource parsing and publishing

(in-package :skyline-tool)

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defun start-resource-directory-watcher (directories &key (name "resource-watcher"))
  "Start a thread watching DIRECTORIES via inotify, redisplaying the frame on changes.
Returns the new thread."
  (make-thread
   (lambda ()
     (handler-case
         (let* ((paths (mapcar (lambda (d)
                                 (probe-file (merge-pathnames d (truename "."))))
                               directories))
                (valid (remove-if-not #'identity paths))
                (frame (and (boundp 'clim:*application-frame*)
                            (ignore-errors clim:*application-frame*))))
           (when valid
             (inotify:with-inotify (inot (mapcar (lambda (p) (list p inotify:in-all-events))
                                                 valid))
               (loop for ev = (inotify:read-events inot)
                     do (ignore-errors
                          (when (and frame (typep frame 'clim:application-frame))
                            (clim:redisplay-frame-panes frame :force-p t)))))))
       (error (c)
         (format t "~a: ~a ended:~%  ~a~%" name c c)))
   (format t "~a thread exiting~%" name)
   :name name))

;;; ============================================================================
;;; Resource Scavenger Implementations
;;; ============================================================================

(defun start-item-scavenger ()
  "Publish item resources from Source/Tables/EquipmentIndex.ods."
  (let ((items (mapcar #'equipment-plist->game-resource (read-equipment-stats))))
    (with-input-from-file (inventory #p"Source/Tables/Inventory.txt")
      (loop for name = (read-line inventory nil nil)
            for i from 0
            while name
            do (unless (find i items :key #'game-resource-item-id)
                 (push (make-instance 'game-resource-item
                                      :name name :index i)
                       items))))
    (dolist (item items)
      (publish-resource-added item))))

(defun start-boat-scavenger ()
  "Publish boat resources from Source/Tables/Boats.ods and Inventory.txt."
  (let ((boats (mapcar #'boat-plist->game-resource (read-boats-data))))
    (with-input-from-file (inventory #p"Source/Tables/Inventory.txt")
      (loop for name = (read-line inventory nil nil)
            for i from 0
            while name
            do (unless (find i boats :key #'game-resource-item-id)
                 (push (make-instance 'game-resource-boat
                                      :name name :index i)
                       boats))))
    (dolist (boat boats)
      (publish-resource-added boat))))

(defun start-blob-scavenger ()
  "Publish blob resources from Source/Blobs/$(machine)/*.xcf and Assets.index."
  (let* ((machine (machine-instance))
         (blob-dir (format nil "Source/Blobs/~a" machine))
         (asset-index (make-hash-table :test 'equal)))
    ;; Read Assets.index to map filename basename → moniker
    (with-open-file (index-file #p"Source/Assets.index")
      (loop for line = (read-line index-file nil nil)
            while line
            do (let* ((trimmed (string-trim " " line))
                      (parts (split-sequence #\space trimmed)))
                 (when (and (consp parts) (>= (length parts) 2))
                   (setf (gethash (first parts) asset-index) (second parts))))))
    ;; Scan blobs per machine
    (loop for file in (directory (make-pathname :name :wild :type "xcf" :defaults blob-dir))
          do (let* ((filename (pathname-name file))
                    (moniker (gethash filename asset-index)))
               (when moniker
                 (let ((resource (make-instance 'game-resource-blob
                                                :name filename
                                                :moniker moniker
                                                :full-path (truename file))))
                   (unless (find resource *all-resources* :key #'game-resource-id)
                     (publish-resource-added resource)))))))

(defun start-character-scavenger ()
  "Publish character resources from Source/Tables/NPCStats.ods."
  (let ((characters (mapcar #'npcstats-plist->game-resource-character
                           (read-character-stats))))
    (dolist (character characters)
      (unless (find character *all-resources* :key #'game-resource-id)
        (publish-resource-added character)))))

(defun start-script-scavenger ()
  "Publish script resources from Source/Scripts/ directory."
  (loop for file in (directory (make-pathname :name :wild :type "sky" :defaults "Source/Scripts/"))
        do (let* ((resource (make-instance 'game-resource-script
                                           :name (pathname-name file)
                                           :moniker (pathname-name file)
                                           :full-path (truename file))))
             (unless (find resource *all-resources* :key #'game-resource-id)
               (publish-resource-added resource)))))

(defun start-song-scavenger ()
  "Publish song resources from Source/Songs/ directory."
  (loop for file in (directory (make-pathname :name :wild :type "mscz" :defaults "Source/Songs/"))
        do (let* ((resource (make-instance 'game-resource-song
                                           :name (pathname-name file)
                                           :moniker (pathname-name file)
                                           :full-path (truename file))))
             (unless (find resource *all-resources* :key #'game-resource-id)
               (publish-resource-added resource)))))

(defun start-map-scavenger ()
  "Publish map resources from Source/Maps/ directory."
  (loop for file in (directory (make-pathname :name :wild :type "tmx" :defaults "Source/Maps/"))
        do (let* ((resource (make-instance 'game-resource-map
                                           :name (pathname-name file)
                                           :moniker (pathname-name file)
                                           :full-path (truename file))))
             (unless (find resource *all-resources* :key #'game-resource-id)
               (publish-resource-added resource)))))

(defun start-tileset-scavenger ()
  "Publish tileset resources from Source/Maps/Tiles/ directory."
  (loop for file in (directory (make-pathname :name :wild :type "art" :defaults "Source/Maps/Tiles/"))
        do (let* ((resource (make-instance 'game-resource-tileset
                                           :name (pathname-name file)
                                           :moniker (pathname-name file)
                                           :full-path (truename file))))
             (unless (find resource *all-resources* :key #'game-resource-id)
               (publish-resource-added resource)))))

(defun start-sprite-sheet-scavenger ()
  "Publish sprite sheet resources from Source/Art/ directory."
  (loop for file in (directory (make-pathname :name :wild :type "png" :defaults "Source/Art/"))
        do (let* ((resource (make-instance 'game-resource-sprite-sheet
                                           :name (pathname-name file)
                                           :moniker (pathname-name file)
                                           :full-path (truename file))))
             (unless (find resource *all-resources* :key #'game-resource-id)
               (publish-resource-added resource)))))

(defun start-routine-run-command-scavenger ()
  "Publish routine-run-command resources from Source/Routines/ directory."
  (loop for file in (directory (make-pathname :name :wild :type "cob" :defaults "Source/Routines/"))
        do (let* ((resource (make-instance 'game-resource-routine-run-command
                                           :name (pathname-name file)
                                           :moniker (pathname-name file)
                                           :full-path (truename file))))
             (unless (find resource *all-resources* :key #'game-resource-id)
               (publish-resource-added resource)))))

(defun start-routine-forth-library-scavenger ()
  "Publish routine-forth-library resources from Source/Routines/ directory."
  (loop for file in (directory (make-pathname :name :wild :type "fth" :defaults "Source/Routines/"))
        do (let* ((resource (make-instance 'game-resource-routine-forth-library
                                           :name (pathname-name file)
                                           :moniker (pathname-name file)
                                           :full-path (truename file))))
             (unless (find resource *all-resources* :key #'game-resource-id)
               (publish-resource-added resource)))))

(defun start-class-scavenger ()
  "Publish class resources from Source/Classes/ directory."
  (loop for file in (directory (make-pathname :name :wild :type "cob" :defaults "Source/Classes/"))
        do (let* ((resource (make-instance 'game-resource-class
                                           :name (pathname-name file)
                                           :moniker (pathname-name file)
                                           :full-path (truename file))))
             (unless (find resource *all-resources* :key #'game-resource-id)
               (publish-resource-added resource)))))

(defun start-instrument-scavenger ()
  "Publish instrument resources from Source/SpecialResources/Instruments.ods."
  (let ((instruments (mapcar #'instrument-plist->game-resource-instrument
                            (read-instrument-data))))
    (dolist (instrument instruments)
      (unless (find instrument *all-resources* :key #'game-resource-id)
        (publish-resource-added instrument)))))

(defun start-flag-scavenger ()
  "Publish flag resources from Source/Tables/Flags.ods."
  (let ((flags (mapcar #'flag-plist->game-resource-flag
                      (read-flag-data))))
    (dolist (flag flags)
      (unless (find flag *all-resources* :key #'game-resource-id)
        (publish-resource-added flag)))))

(defun start-key-scavenger ()
  "Publish key resources from Source/Tables/Keys.ods."
  (let ((keys (mapcar #'key-plist->game-resource-key
                     (read-key-data))))
    (dolist (key keys)
      (unless (find key *all-resources* :key #'game-resource-id)
        (publish-resource-added key)))))

(defun start-atari-vox-dictionary-scavenger ()
  "Publish AtariVox dictionary resources from Source/SpecialResources/AtariVoxDict.txt."
  (let ((dicts (mapcar #'atari-vox-dict-plist->game-resource-atari-vox-dictionary
                      (read-atari-vox-dictionary-data))))
    (dolist (dict dicts)
      (unless (find dict *all-resources* :key #'game-resource-id)
        (publish-resource-added dict)))))

(defun start-intellivoice-dictionary-scavenger ()
  "Publish Intellivoice dictionary resources from Source/SpecialResources/IntellivoiceDict.txt."
  (let ((dicts (mapcar #'intellivoice-dict-plist->game-resource-intellivoice-dictionary
                      (read-intellivoice-dictionary-data))))
    (dolist (dict dicts)
      (unless (find dict *all-resources* :key #'game-resource-id)
        (publish-resource-added dict)))))

(defun start-object-prototype-scavenger ()
  "Publish object prototype resources from Source/Objects/ directory."
  (loop for file in (directory (make-pathname :name :wild :type "json" :defaults "Source/Objects/"))
        do (let* ((resource (make-instance 'game-resource-object-prototype
                                           :name (pathname-name file)
                                           :moniker (pathname-name file)
                                           :full-path (truename file))))
             (unless (find resource *all-resources* :key #'game-resource-id)
               (publish-resource-added resource)))))

(defun start-phrasebook-scavenger ()
  "Publish phrasebook resources from Source/Phrasebooks/ directory."
  (loop for file in (directory (make-pathname :name :wild :type "txt" :defaults "Source/Phrasebooks/"))
        do (let* ((resource (make-instance 'game-resource-phrasebook
                                           :name (pathname-name file)
                                           :moniker (pathname-name file)
                                           :full-path (truename file))))
             (unless (find resource *all-resources* :key #'game-resource-id)
               (publish-resource-added resource)))))

(defun start-translation-scavenger ()
  "Publish translation resources from Source/Translations/ directory."
  (loop for file in (directory (make-pathname :name :wild :type "po" :defaults "Source/Translations/"))
        do (let* ((resource (make-instance 'game-resource-translation
                                           :name (pathname-name file)
                                           :moniker (pathname-name file)
                                           :full-path (truename file))))
             (unless (find resource *all-resources* :key #'game-resource-id)
               (publish-resource-added resource)))))

(defun start-preferences-scavenger ()
  "Publish preferences resources from .config/Skyline-Tool/Preferences.lisp."
  (let ((pref-path (merge-pathnames #p".config/Skyline-Tool/Preferences.lisp"
                                    (user-homedir-pathname))))
    (when (probe-file pref-path)
      (with-open-file (pref-file pref-file)
        (let ((prefs (read pref-file)))
          (unless (find prefs *all-resources* :key #'game-resource-id)
            (publish-resource-added prefs)))))))

(defun start-all-resources-scavengers (frame)
  "Start all resource scavenger threads."
  (declare (ignore frame))
  (dolist (scavenger '(start-item-scavenger
                       start-boat-scavenger
                       start-blob-scavenger
                       start-character-scavenger
                       start-script-scavenger
                       start-song-scavenger
                       start-map-scavenger
                       start-tileset-scavenger
                       start-sprite-sheet-scavenger
                       start-routine-run-command-scavenger
                       start-routine-forth-library-scavenger
                       start-class-scavenger
                       start-instrument-scavenger
                       start-flag-scavenger
                       start-key-scavenger
                       start-atari-vox-dictionary-scavenger
                       start-intellivoice-dictionary-scavenger
                       start-object-prototype-scavenger
                       start-phrasebook-scavenger
                       start-translation-scavenger
                       start-preferences-scavenger))
    (submit-task scavenger)))