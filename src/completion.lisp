(in-package #:mtm.completion)

;; Store the relative directory for user Completion dictionary files.
(defparameter +completion-dictionary-relative-directory+
  ".mtm/dictionaries/")

;; Return the user's Completion dictionary directory.
(defun get-completion-dictionary-directory ()
  (merge-pathnames
   +completion-dictionary-relative-directory+
   (user-homedir-pathname)))

;; Return direct Completion dictionary files in filename order.
(defun get-completion-dictionary-files
    (&optional (directory (get-completion-dictionary-directory)))
  (sort
   (remove-if #'uiop:directory-pathname-p
              (directory (merge-pathnames "*.txt" directory)))
   #'string<
   :key #'namestring))

;; Return true when TEXT is a valid Completion candidate.
(defun get-completion-candidate-p (text)
  (and (plusp (length text))
       (not (char= (char text 0) #\#))))

;; Return a candidate, code, and optional weight from one dictionary line.
(defun get-completion-dictionary-entry (line)
  (let (;; Find the candidate and code separator.
        (first-tab (position #\Tab line)))
    (when first-tab
      (let* (;; Read the candidate before its first tab.
             (candidate (subseq line 0 first-tab))
             ;; Start reading the completion code after the candidate.
             (code-start (1+ first-tab))
             ;; Find the separator before the optional weight column.
             (code-end (position #\Tab line :start code-start))
             ;; Start reading the weight after the completion code.
             (weight-start (and code-end (1+ code-end)))
             ;; Normalize the code for case-insensitive matching.
             (code (string-downcase
                    (string-trim '(#\Space #\Tab #\Return)
                                 (subseq line code-start
                                         (or code-end (length line))))))
             ;; Read the optional weight field when the line contains one.
             (weight-text
               (and weight-start
                    (string-trim '(#\Space #\Tab #\Return)
                                 (subseq line weight-start))))
             ;; Treat missing or empty weights as zero.
             (weight (if (or (null weight-text)
                             (string= weight-text ""))
                         0
                         (ignore-errors (parse-integer weight-text)))))
        (when (and (get-completion-candidate-p candidate)
                   (plusp (length code))
                   (integerp weight)
                   (not (minusp weight)))
          (values candidate code weight))))))

;; Read valid Completion dictionary entries in file and line order.
(defun new-completion-dictionary-entries (paths)
  (let (;; Preserve dictionary order for candidate ranking.
        (entries nil))
    (dolist (path (if (listp paths) paths (list paths)))
      (with-open-file
          ;; Read each user dictionary file as UTF-8 text.
          (stream path :direction :input :external-format :utf-8)
        (loop
          ;; Read one dictionary line.
          for line = (read-line stream nil)
          while line
          do (multiple-value-bind
                 ;; Extract one valid candidate entry.
                 (candidate code weight)
                 (get-completion-dictionary-entry line)
               (when weight
                 ;; Store code, candidate, and weight for matching.
                 (push (list code candidate weight) entries))))))
    (nreverse entries)))

;; Return true when CODE starts with PREFIX.
(defun get-completion-prefix-p (prefix code)
  (and (plusp (length prefix))
       (<= (length prefix) (length code))
       (string= prefix code :end2 (length prefix))))

;; Return dictionary candidates matching a Completion code prefix.
(defun get-completion-candidates (entries prefix)
  (let* (;; Normalize user input before dictionary matching.
         (normalized-prefix (string-downcase prefix))
         ;; Collect matching entries before sorting and deduplication.
         (matches nil)
         ;; Avoid showing one candidate more than once.
         (seen (make-hash-table :test #'equal))
         ;; Collect candidates before restoring source order.
         (candidates nil))
    (dolist (entry entries)
      (let (;; Read the Completion code for this entry.
            (code (first entry)))
        (when (get-completion-prefix-p normalized-prefix code)
          (push entry matches))))
    ;; Sort high weights first while preserving source order for ties.
    (setf matches
          (stable-sort (nreverse matches) #'> :key #'third))
    (dolist (entry matches (nreverse candidates))
      (let (;; Read the candidate for this entry.
            (candidate (second entry)))
        (unless (gethash candidate seen)
          (setf (gethash candidate seen) t)
          (push candidate candidates))))))

;; Create one Completion provider backed by a dictionary snapshot.
(defun new-completion-provider
    (&optional path)
  (let (;; Load all user files unless a test supplies one path.
        (entries
          (handler-case
              (new-completion-dictionary-entries
               (if path
                   (list path)
                   (get-completion-dictionary-files)))
            ;; Missing data disables completion without blocking the Editor.
            (file-error () nil))))
    (lambda (prefix)
      (get-completion-candidates entries prefix))))

;; Cache the default provider for all local Editor areas.
(defvar *completion-provider* nil)

;; Return the shared default Completion provider.
(defun get-completion-provider ()
  (or *completion-provider*
      (setf *completion-provider*
            (new-completion-provider))))
