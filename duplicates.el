;;;; Duplicates - handling duplicate files in dired

;;;; Find marked files in a dired buffer and display which ones have identical contents
;;;; (C)2011 Justin Heyes-Jones
;;;; How to use... mark files you're interest in the dired window, perhaps with `dired-mark-files-regexp'
;;;; then `dired-show-marked-duplicate-files' will open a buffer with a list of duplicated files
;;;; Also `dired-mark-duplicate-files' will mark only superfluous duplicates of the files allowing you to move
;;;; them to another folder or delete them 

(defvar *duplicate-buffer* nil)

(defun md5-file(filename)
  "Open FILENAME, load it into a buffer and generate the md5 of its contents"
  (interactive "f")
  (with-temp-buffer 
    (insert-file-contents filename)
    (md5 (current-buffer))))

(defun dired-get-duplicate-marked-file-map()
  "return a hashmap of files in the current dired buffer keyed by the md5 of the contents of each file. Where
multiple files share the same md5 they will all be present in the value for that key"
  (let ((md5-map (make-hash-table :test 'equal :size 40)))
    (if (eq major-mode 'dired-mode)
   (let ((filenames (dired-get-marked-files)))
     (dolist (fn filenames)
       (if (file-regular-p fn)
      (let ((md5 (md5-file fn)))
        (let ((map-entry (gethash md5 md5-map nil)))
          (puthash md5 (cons fn map-entry) md5-map)))))))
    md5-map))

(defun show-duplicate(key value)
  "Given the KEY and VALUE of a map entry for a given md5, if there is more than one filename in the list 
of files then display them as duplicates"
  (if (> (length value) 1)
      (let ((str (format "%d duplicates of %s\n" (length value) (first value))))
   (dolist (filename (rest value))
     (setf str (concat str (format "%s\n" filename))))
   (insert str))))

(defun dired-show-marked-duplicate-files() 
  "For each marked file in a dired buffer determine which have the same contents"
  (interactive)
  (if (eq major-mode 'dired-mode)
      (let ((md5-map (dired-get-duplicate-marked-file-map)))
   (setf *duplicate-buffer* (get-buffer-create "Duplicated files"))
   (goto-line 1 *duplicate-buffer*)
   (erase-buffer)
   (maphash #'show-duplicate md5-map))
    (error (format "Not a Dired buffer \(%s\)" major-mode))))

(defun dired-mark-duplicates(key value)
  "KEY is the MD5 of some set of 1 or more files in the dired buffer, while VALUE is a list of filenames. In order to mark 
only duplicates we'll ignore the first file arbitrarily and mark the remaining ones one. More complicated or interactive 
strategies could be considered such as keeping the one with the shorter filename, most recent modified date and so on."
  (let ((rest (rest value)))
    (when rest 
      (dolist (file rest)
   (dired-goto-file file)
   (dired-mark 1)))))

(defun dired-mark-duplicate-files() 
  "For each marked file in a dired buffer determine which have the same contents and then leave only the duplicates marked"
  (interactive)
  (if (eq major-mode 'dired-mode)
      (let ((md5-map (dired-get-duplicate-marked-file-map)))
   (dired-unmark-all-marks)
   (maphash #'dired-mark-duplicates md5-map))
    (error (format "Not a Dired buffer \(%s\)" major-mode))))
