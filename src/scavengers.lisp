;;; Skyline-Tool src/scavengers.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC
;;; Consolidated scavenger functions for resource parsing and publishing

(in-package :skyline-tool)

;;; 
;;; Utility Functions
;;; 

(defun start-resource-directory-watcher (directories &key (name "resource-watcher"))
  "Start a thread watching DIRECTORIES via inotify, redisplaying the frame on changes.
Returns the new thread."
  (ensure-worker-journal)
  (journal:journaled (watcher-start
                      :log-record *worker-journal*
                      :args (list :name name :dir-count (length directories)
                                        :thread (list :id (thread-os-tid (current-thread))
                                                      :name (thread-name (current-thread))))))
  (make-thread
   (lambda ()
     (journal:journaled (watcher-thread-entered)
       :log-record *worker-journal*
       :args (list :name name
                         :thread (list :id (thread-os-tid (current-thread))
                                       :name (thread-name (current-thread)))))
     (let* ((paths (mapcar (lambda (d)
                             (probe-file (merge-pathnames d (truename "."))))
                           directories))
            (valid (remove-if-not #'identity paths))
            (frame (and (boundp 'clim:*application-frame*)
                        (ignore-errors clim:*application-frame*))))
       (when valid
         (inotify:with-inotify (inot (mapcar (lambda (p) (list p inotify:in-all-events))
                                             valid))
           (journal:journaled (watcher-inotify-open)
             :log-record *worker-journal*
             :args (list :name name :paths valid
                               :thread (list :id (thread-os-tid (current-thread))
                                             :name (thread-name (current-thread)))))
           (loop for ev = (inotify:read-events inot)
                 do (ignore-errors
                     (journal:journaled (watcher-event)
                       :log-record *worker-journal*
                       :args (list :name name :event ev
                                         :thread (list :id (thread-os-tid (current-thread))
                                                       :name (thread-name (current-thread)))))
                     (when (and frame (typep frame 'clim:application-frame))
                       (clim:redisplay-frame-panes frame :force-p t)))))))
     (journal:journaled (watcher-thread-exited)
       :log-record *worker-journal*
       :args (list :name name
                         :thread (list :id (thread-os-tid (current-thread))
                                       :name (thread-name (current-thread))))))))

;;; 
;;; Resource Scavenger Implementations
;;; 

(defun start-item-scavenger ()
  "Publish item resources from Source/Tables/EquipmentIndex.ods and Inventory.txt."
  (let ((items (mapcar #'equipment-plist->game-resource (read-equipment-stats))))
    (with-input-from-file (inventory #p"Source/Tables/Inventory.txt")
      (loop for name = (read-line inventory nil nil)
            for i from 0
            while name
            do (unless (find i items :key #'game-resource-offset)
                 (push (make-instance 'game-resource-item
                                      
                                      :collective-path #p"Source/Tables/EquipmentIndex.ods"
                                      :offset i
                                      :name (string-trim +whitespace+ name)
                                      :item-id i)
                       items))))
    (dolist (item items)
      (cache-and-publish-resource item))
    (start-resource-directory-watcher
     (list #p"Source/Tables/EquipmentIndex.ods" #p"Source/Tables/Inventory.txt")
     :name "item-scavenger-watcher")))

(defun start-boat-scavenger ()
  "Publish boat resources from Source/Tables/Boats.ods."
  (let ((boats (mapcar #'boat-plist->game-resource (read-boats-data))))
    (dolist (boat boats)
      (cache-and-publish-resource boat))
    (start-resource-directory-watcher
     (list #p"Source/Tables/Boats.ods")
     :name "boat-scavenger-watcher")))

(defun start-blob-scavenger ()
  "Publish blob resources from Source/Blobs/$(machine)/*.xcf and Assets.index."
  (let* ((machine (machine-instance))
         (blob-dir (format nil "Source/Blobs/~a" machine))
         (asset-index (make-hash-table :test 'equal)))
    (with-open-file (index-file #p"Source/Assets.index")
      (loop for line = (read-line index-file nil nil)
            while line
            do (let* ((trimmed (string-trim " " line))
                      (parts (split-sequence #\space trimmed)))
                 (when (and (consp parts) (>= (length parts) 2))
                   (setf (gethash (first parts) asset-index) (second parts))))))
    (loop for file in (directory (make-pathname :name :wild :type "xcf" :defaults blob-dir))
          do (let* ((filename (pathname-name file))
                    (moniker (gethash filename asset-index)))
               (when moniker
                 (let ((resource (make-instance 'game-resource-blob
                                                :moniker moniker
                                                :full-path (truename file))))
                   (cache-and-publish-resource resource)))))))

(defun start-character-scavenger ()
  "Publish character resources from Source/Tables/NPCStats.ods."
  (let ((characters (mapcar #'npcstats-plist->game-resource-character
                            (read-character-stats))))
    (dolist (character characters)
      (cache-and-publish-resource character))
    (start-resource-directory-watcher
     (list #p"Source/Tables/NPCStats.ods")
     :name "character-scavenger-watcher")))

(defun start-script-scavenger ()
  "Publish script resources from Source/Scripts/ directory (recursive)."
  (loop for file in (directory #p"Source/Scripts/**/*.fountain")
        do (let ((resource (make-instance 'game-resource-script
                                          :moniker (namestring file)
                                          :full-path (truename file))))
             (cache-and-publish-resource resource)))
  (start-resource-directory-watcher
   (list #p"Source/Scripts/")
   :name "script-scavenger-watcher"))

(defun start-song-scavenger ()
  "Publish song resources from Source/Songs/ directory."
  (loop for file in (directory (make-pathname :name :wild :type "mscz" :defaults "Source/Songs/"))
        do (let* ((moniker (namestring file))
                  (metadata (ignore-errors (read-mscz-metadata file)))
                  (lyrics (ignore-errors (read-mscz-lyrics file)))
                  (resource (make-instance 'game-resource-song
                                           :moniker moniker
                                           :full-path (truename file)
                                           :asset-id (ignore-errors
                                                       (get-asset-id :song moniker))
                                           :mscz-title (getf metadata :title)
                                           :mscz-subtitle (getf metadata :subtitle)
                                           :mscz-composer (getf metadata :composer)
                                           :mscz-copyright (getf metadata :copyright)
                                           :mscz-lyrics lyrics)))
             (cache-and-publish-resource resource)))
  (start-resource-directory-watcher
   (list #p"Source/Songs/")
   :name "song-scavenger-watcher"))

(defun start-map-scavenger ()
  "Publish map resources from Source/Maps/ directory (recursive)."
  (loop for file in (directory #p"Source/Maps/**/*.tmx")
        do (let ((resource (make-instance 'game-resource-map
                                          :moniker (namestring file)
                                          :full-path (truename file))))
             (cache-and-publish-resource resource)))
  (start-resource-directory-watcher
   (list #p"Source/Maps/")
   :name "map-scavenger-watcher"))

(defun start-tileset-scavenger ()
  "Publish tileset resources from Source/Maps/Tiles/ directory."
  (loop for file in (directory #p"Source/Maps/Tiles/**/*.tsx")
        do (let ((resource (make-instance 'game-resource-tileset
                                          :full-path (truename file))))
             (cache-and-publish-resource resource)))
  (start-resource-directory-watcher
   (list #p"Source/Maps/Tiles/")
   :name "tileset-scavenger-watcher"))

(defun start-sprite-sheet-scavenger ()
  "Publish sprite sheet resources from Source/Art/ directory."
  (loop for file in (directory (make-pathname :name :wild :type "png" :defaults "Source/Art/"))
        do (let ((resource (make-instance 'game-resource-sprite-sheet
                                          :full-path (truename file))))
             (cache-and-publish-resource resource)))
  (start-resource-directory-watcher
   (list #p"Source/Art/")
   :name "sprite-sheet-scavenger-watcher"))

(defun start-routine-run-command-scavenger ()
  "Publish routine-run-command resources from Source/Routines/ directory."
  (loop for file in (directory (make-pathname :name :wild :type "cob" :defaults "Source/Routines/"))
        do (let ((resource (make-instance 'game-resource-routine-run-commands
                                          :full-path (truename file))))
             (cache-and-publish-resource resource)))
  (start-resource-directory-watcher
   (list #p"Source/Routines/")
   :name "routine-run-command-scavenger-watcher"))

(defun start-routine-forth-library-scavenger ()
  "Publish routine-forth-library resources from Source/Scripts/Forth/ directory."
  (loop for file in (directory (make-pathname :name :wild :type "forth" :defaults "Source/Scripts/Forth/"))
        do (let ((resource (make-instance 'game-resource-routine-forth-library
                                          :full-path (truename file))))
             (cache-and-publish-resource resource)))
  (start-resource-directory-watcher
   (list #p"Source/Scripts/Forth/")
   :name "routine-forth-library-scavenger-watcher"))

(defun start-class-scavenger ()
  "Publish class resources from Source/Classes/ directory."
  (loop for file in (directory (make-pathname :name :wild :type "cob" :defaults "Source/Classes/"))
        do (let ((resource (make-instance 'game-resource-class
                                          :full-path (truename file))))
             (cache-and-publish-resource resource)))
  (start-resource-directory-watcher
   (list #p"Source/Classes/")
   :name "class-scavenger-watcher"))

(defun start-instrument-scavenger ()
  "Publish instrument resources from Source/Tables/Instruments.ods."
  (let ((data (read-instrument-data)))
    (when data
      (let ((instruments (mapcar #'instrument-plist->game-resource-instrument data)))
        (dolist (instrument instruments)
          (cache-and-publish-resource instrument))))
    (start-resource-directory-watcher
     (list #p"Source/Tables/Instruments.ods")
     :name "instrument-scavenger-watcher")))

(defun start-flag-scavenger ()
  "Publish flag resources from Source/Tables/Flags.txt."
  (let ((flags (mapcar #'flag-plist->game-resource-flag
                       (read-flag-data))))
    (dolist (flag flags)
      (cache-and-publish-resource flag))
    (start-resource-directory-watcher
     (list #p"Source/Tables/Flags.txt")
     :name "flag-scavenger-watcher")))

(defun start-key-scavenger ()
  "Publish key resources from Source/Tables/Keys.txt."
  (let ((keys (mapcar #'key-plist->game-resource-key
                      (read-key-data))))
    (dolist (key keys)
      (cache-and-publish-resource key))
    (start-resource-directory-watcher
     (list #p"Source/Tables/Keys.txt")
     :name "key-scavenger-watcher")))

(defun start-atari-vox-dictionary-scavenger ()
  "Publish AtariVox dictionary resource from Source/Tables/SpeakJet.dic."
  (let ((data (read-atari-vox-dictionary-data)))
    (when data
      (cache-and-publish-resource
       (atari-vox-dict-plist->game-resource-atari-vox-dictionary data)))
    (start-resource-directory-watcher
     (list #p"Source/Tables/SpeakJet.dic")
     :name "atari-vox-dictionary-scavenger-watcher")))

(defun start-intellivoice-dictionary-scavenger ()
  "Publish Intellivoice dictionary resource from Source/Tables/IntelliVoice.dic."
  (let ((data (read-intellivoice-dictionary-data)))
    (when data
      (cache-and-publish-resource
       (intellivoice-dict-plist->game-resource-intellivoice-dictionary data)))
    (start-resource-directory-watcher
     (list #p"Source/Tables/IntelliVoice.dic")
     :name "intellivoice-dictionary-scavenger-watcher")))

(defun start-object-prototype-scavenger ()
  "Publish object prototype resources from Source/Objects/ directory."
  (loop for file in (directory (make-pathname :name :wild :type "json" :defaults "Source/Objects/"))
        do (let ((resource (make-instance 'game-resource-object-prototype
                                          
                                          :full-path (truename file))))
             (cache-and-publish-resource resource)))
  (start-resource-directory-watcher
   (list #p"Source/Objects/")
   :name "object-prototype-scavenger-watcher"))

(defun start-phrasebook-scavenger ()
  "Publish phrasebook resources from Source/Phrasebooks/ directory."
  (progn
    (loop for file in (directory (make-pathname :name :wild :type "txt" :defaults "Source/Phrasebooks/"))
          do (let ((resource (make-instance 'game-resource-phrasebook
                                            
                                            :full-path (truename file))))
               (cache-and-publish-resource resource)))
    (start-resource-directory-watcher
     (list #p"Source/Phrasebooks/")
     :name "phrasebook-scavenger-watcher")))

(defun start-translation-scavenger ()
  "Publish translation resources from Source/Translations/ directory."
  (loop for file in (directory (make-pathname :name :wild :type "po" :defaults "Source/Translations/"))
        do (let ((resource (make-instance 'game-resource-translation
                                          
                                          :full-path (truename file))))
             (cache-and-publish-resource resource)))
  (start-resource-directory-watcher
   (list #p"Source/Translations/")
   :name "translation-scavenger-watcher"))

(defun start-preferences-scavenger ()
  "Publish preferences resource from ~/.config/Skyline-Tool/<GAME>/<PORT>.lisp."
  (start-resource-directory-watcher
   (list (prefs-pathname))
   :name "preferences-scavenger-watcher"))

(defun cache-and-publish-resource (resource)
  "Add RESOURCE to the global cache and publish a resource-added event."
  (cache-add-resource (game-resource-kind resource) resource)
  (publish-resource-added resource))

(defun start-all-resources-scavengers (frame)
  "Start all resource scavenger threads."
  (declare (ignore frame))
  (ensure-worker-journal)
  (let ((thread-id (thread-os-tid (current-thread)))
        (thread-name (thread-name (current-thread))))
    (journal:journaled (all-scavengers-start)
      :log-record *worker-journal*
      :args (list :thread (list :id thread-id :name thread-name)
                        :operation "start"))
    (ensure-thread-pool-kernel)
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
      (submit-task scavenger))
    (journal:journaled (all-scavengers-complete)
      :log-record *worker-journal*
      :args (list :thread (list :id thread-id :name thread-name)
                        :operation "complete"))))
