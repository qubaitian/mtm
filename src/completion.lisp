(in-package #:mtm.completion)

;; Store the relative directory for user Pinyin dictionary files.
(defparameter +pinyin-dictionary-relative-directory+
  ".mtm/dictionaries/")

;; Return the user's Pinyin dictionary directory.
(defun get-pinyin-dictionary-directory ()
  (merge-pathnames
   +pinyin-dictionary-relative-directory+
   (user-homedir-pathname)))

;; Return direct user dictionary files in deterministic filename order.
(defun get-pinyin-dictionary-files
    (&optional (directory (get-pinyin-dictionary-directory)))
  (sort
   (remove-if #'uiop:directory-pathname-p
              (directory (merge-pathnames "*.txt" directory)))
   #'string<
   :key #'namestring))

;; Return true when TEXT contains one Han character.
(defun get-hanzi-character-p (text)
  (and (= (length text) 1)
       (let (;; Read the only character's Unicode code point.
             (code-point (char-code (char text 0))))
         (or (<= #x3400 code-point #x4dbf)
             (<= #x4e00 code-point #x9fff)
             (<= #xf900 code-point #xfaff)
             (<= #x20000 code-point #x323af)
             (<= #x2f800 code-point #x2fa1f)))))

;; Return true when TEXT is an ASCII English Completion candidate.
(defun get-english-word-p (text)
  (and (plusp (length text))
       (not (char= (char text 0) #\#))
       (some #'alpha-char-p text)
       (every (lambda (character)
                (<= (char-code character) 127))
              text)))

;; Return a candidate, code, and optional weight from one dictionary line.
(defun get-pinyin-dictionary-entry (line)
  (let (;; Find the candidate and code separator.
        (first-tab (position #\Tab line)))
    (when first-tab
      (let* (;; Read the candidate before its first tab.
             (candidate (subseq line 0 first-tab))
             ;; Start reading the Pinyin code after the candidate.
             (code-start (1+ first-tab))
             ;; Find the separator before the required weight column.
             (code-end (position #\Tab line :start code-start))
             ;; Start reading the weight after the Pinyin code.
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
        (when (and (or (get-hanzi-character-p candidate)
                       (get-english-word-p candidate))
                   (plusp (length code))
                   (integerp weight)
                   (not (minusp weight)))
          (values candidate code weight))))))

;; Read valid Completion dictionary entries in file and line order.
(defun new-pinyin-dictionary-entries (paths)
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
                 (get-pinyin-dictionary-entry line)
               (when weight
                 ;; Store code, candidate, and weight for matching.
                 (push (list code candidate weight) entries))))))
    (nreverse entries)))

;; Return true when CODE starts with PREFIX.
(defun get-pinyin-prefix-p (prefix code)
  (and (plusp (length prefix))
       (<= (length prefix) (length code))
       (string= prefix code :end2 (length prefix))))

;; Return dictionary candidates matching a Pinyin prefix.
(defun get-pinyin-completion-candidates (entries prefix)
  (let* (;; Normalize user input before dictionary matching.
         (normalized-prefix (string-downcase prefix))
         ;; Collect matching entries before sorting and deduplication.
         (matches nil)
         ;; Avoid showing one character more than once.
         (seen (make-hash-table :test #'equal))
         ;; Collect candidates before restoring source order.
         (candidates nil))
    (dolist (entry entries)
      (let (;; Read the Pinyin code for this entry.
            (code (first entry)))
        (when (get-pinyin-prefix-p normalized-prefix code)
          (push entry matches))))
    ;; Sort high weights first while preserving source order for ties.
    (setf matches
          (stable-sort (nreverse matches) #'> :key #'third))
    (dolist (entry matches (nreverse candidates))
      (let (;; Read the single-character candidate for this entry.
            (candidate (second entry)))
        (unless (gethash candidate seen)
          (setf (gethash candidate seen) t)
          (push candidate candidates))))))

;; Create one Completion provider backed by a dictionary snapshot.
(defun new-pinyin-completion-provider
    (&optional path)
  (let (;; Load all user files unless a test supplies one path.
        (entries
          (handler-case
              (new-pinyin-dictionary-entries
               (if path
                   (list path)
                   (get-pinyin-dictionary-files)))
            ;; Missing data disables completion without blocking the Editor.
            (file-error () nil))))
    (lambda (prefix)
      (get-pinyin-completion-candidates entries prefix))))

;; Cache the default provider for all local Editor areas.
(defvar *pinyin-completion-provider* nil)

;; Return the shared default Pinyin Completion provider.
(defun get-pinyin-completion-provider ()
  (or *pinyin-completion-provider*
      (setf *pinyin-completion-provider*
            (new-pinyin-completion-provider))))
