(in-package #:mtm.completion)

;; Store the relative path for the per-user 8105 dictionary.
(defparameter +pinyin-dictionary-relative-path+
  ".mtm/dictionaries/8105.dict.yaml")

;; Return the per-user Pinyin dictionary path.
(defun get-pinyin-dictionary-path ()
  (merge-pathnames
   +pinyin-dictionary-relative-path+
   (user-homedir-pathname)))

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

;; Return the active candidate and Pinyin code from one dictionary line.
(defun get-pinyin-dictionary-entry (line)
  (let (;; Find the candidate and code separator.
        (first-tab (position #\Tab line)))
    (when first-tab
      (let* (;; Read the candidate before its first tab.
             (candidate (subseq line 0 first-tab))
             ;; Start reading the Pinyin code after the candidate.
             (code-start (1+ first-tab))
             ;; Stop before an optional weight column.
             (code-end (or (position #\Tab line :start code-start)
                           (length line)))
             ;; Normalize the code for case-insensitive matching.
             (code (string-downcase
                    (string-trim '(#\Space #\Tab #\Return)
                                 (subseq line code-start code-end)))))
        (when (and (get-hanzi-character-p candidate)
                   (plusp (length code)))
          (values candidate code))))))

;; Read active single-character entries in source order.
(defun new-pinyin-dictionary-entries (path)
  (let (;; Preserve dictionary order for candidate ranking.
        (entries nil))
    (with-open-file
        ;; Read the user dictionary as UTF-8 text.
        (stream path :direction :input :external-format :utf-8)
      (loop
        ;; Read one dictionary line.
        for line = (read-line stream nil)
        while line
        do (multiple-value-bind
               ;; Extract one active candidate entry.
               (candidate code)
               (get-pinyin-dictionary-entry line)
             (when code
               ;; Store code first for cheap provider matching.
               (push (cons code candidate) entries)))))
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
         ;; Avoid showing one character more than once.
         (seen (make-hash-table :test #'equal))
         ;; Collect candidates before restoring source order.
         (candidates nil))
    (dolist (entry entries (nreverse candidates))
      (let (;; Read the Pinyin code for this entry.
            (code (car entry))
            ;; Read the single-character candidate for this entry.
            (candidate (cdr entry)))
        (when (and (get-pinyin-prefix-p normalized-prefix code)
                   (not (gethash candidate seen)))
          (setf (gethash candidate seen) t)
          (push candidate candidates))))))

;; Create one Completion provider backed by a dictionary snapshot.
(defun new-pinyin-completion-provider
    (&optional path)
  (let* (;; Use the user dictionary unless a test supplies another path.
         (dictionary-path (or path (get-pinyin-dictionary-path)))
         ;; Load the dictionary once for this provider.
         (entries
           (handler-case
               (new-pinyin-dictionary-entries dictionary-path)
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
