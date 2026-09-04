(in-package #:mtm.editor)

(defun new-octets (&optional (length 0))
  (make-array length :element-type '(unsigned-byte 8)))

(defun new-octets-copy (octets)
  (copy-seq octets))

(defun octets-equal-p (left right)
  (equalp left right))

(defun octets-prefix-p (prefix octets)
  (and (<= (length prefix) (length octets))
       (= (mismatch prefix octets) (length prefix))))

(defun new-octets-with-octet (octets octet)
  (let ((result (new-octets (1+ (length octets)))))
    (replace result octets)
    (setf (aref result (length octets)) octet)
    result))

;; Return true for Unicode characters rendered across two terminal cells.
;; ponytail: use a fixed wide-character table; add full Unicode width data later.
(defun get-editor-wide-character-p (code)
  (or (<= #x1100 code #x115f)
      (= code #x2329)
      (= code #x232a)
      (<= #x2e80 code #x303e)
      (<= #x3040 code #xa4cf)
      (<= #xac00 code #xd7a3)
      (<= #xf900 code #xfaff)
      (<= #xfe10 code #xfe19)
      (<= #xfe30 code #xfe6f)
      (<= #xff00 code #xff60)
      (<= #xffe0 code #xffe6)
      (<= #x20000 code #x2fffd)
      (<= #x30000 code #x3fffd)))

;; Account for tabs, caret notation, and double-width Unicode.
(defun get-editor-cell-width (octets position column &optional character-length)
  (cond
    ((= (aref octets position) 9)
     (- 8 (mod column 8)))
    ((< (aref octets position) 32) 2)
    ((= (aref octets position) 127) 2)
    (t
     (let* (;; Read the current character's byte length.
            (length (or character-length
                        (get-utf8-character-length octets position)))
            ;; Decode the current character for the width lookup.
            (character
              (when length
                (nth-value
                 0
                 (get-utf8-chunk
                  (subseq octets position (+ position length)))))))
       (if (and character
                (plusp (length character))
                (get-editor-wide-character-p
                 (char-code (char character 0))))
           2
           1)))))

;; Store Editor completion state beside the Edit buffer.
;; Keep bytes so submissions preserve spaces and UTF-8 text.
(defstruct (editor
            (:constructor new-editor
                (&key (history-box (cons nil nil)) completion-provider))
            (:conc-name get-editor-))
  ;; Store the complete Edit buffer as raw UTF-8 bytes.
  (buffer (new-octets))
  ;; Store cursor offsets as byte positions.
  (cursor 0)
  ;; Shared Session History lives in the car of this box.
  ;; Share this History box across Attachments.
  history-box
  ;; NIL means the cursor is outside history navigation.
  history-index
  ;; Restore the edit buffer after leaving history.
  (history-draft (new-octets))
  ;; Keep the requested display column during vertical movement.
  preferred-column
  ;; Store the mouse anchor while selecting text.
  selection-anchor
  ;; Track whether the mouse is selecting text.
  mouse-selecting-p
  ;; Call this function with the current Completion prefix.
  completion-provider
  ;; Keep complete candidates currently shown by the Completion menu.
  completion-candidates
  ;; Store the selected candidate's absolute index.
  (completion-index 0)
  ;; Store the byte range replaced by the selected candidate.
  (completion-start 0)
  (completion-end 0))

(defconstant +editor-completion-max-visible+ 9)

;; Return true when the Completion menu has candidates to show.
(defun get-editor-completion-active-p (editor)
  (not (null (get-editor-completion-candidates editor))))

;; Close the Completion menu and clear its selection state.
(defun del-editor-completion (editor)
  (when (get-editor-completion-active-p editor)
    (setf (get-editor-completion-candidates editor) nil
          (get-editor-completion-index editor) 0
          (get-editor-completion-start editor) 0
          (get-editor-completion-end editor) 0)
    :changed))

;; Set the callback that supplies Completion candidates.
(defun set-editor-completion-provider (editor provider)
  "Set EDITOR's Completion provider.
The provider receives a prefix string and returns complete strings."
  (check-type provider (or null function))
  (setf (get-editor-completion-provider editor) provider)
  (del-editor-completion editor)
  provider)

;; Return true for bytes allowed inside a Completion prefix.
(defun get-editor-completion-byte-p (octet)
  (or (<= 65 octet 90)
      (<= 97 octet 122)
      (<= 48 octet 57)
      (member octet '(33 36 37 38 42 43 45 46 47 58 60 61 62 63 64
                      94 95 126))))

;; Return the current Completion prefix and its byte range.
(defun get-editor-completion-prefix (editor)
  (let* (;; Read the current Edit buffer without changing it.
         (buffer (get-editor-buffer editor))
         ;; Read the Insertion point as a byte position.
         (cursor (get-editor-cursor editor))
         ;; Stop at the current logical line.
         (line-start (get-editor-line-start buffer cursor))
         ;; Move backward across word-like prefix bytes.
         (start cursor))
    (loop while (and (> start line-start)
                     (get-editor-completion-byte-p
                      (aref buffer (1- start))))
          do (decf start))
    (values (nth-value 0
                       (get-utf8-chunk (subseq buffer start cursor)))
            start
            cursor)))

;; Return provider candidates that match the current Completion prefix.
(defun get-completion-candidates (editor prefix)
  (let* (;; Read the configured Completion provider.
         (provider (get-editor-completion-provider editor)))
    (when provider
      (handler-case
          (let (;; Ask the provider for complete candidate strings.
                (candidates (funcall provider prefix)))
            (when (listp candidates)
              (remove-duplicates
               (remove-if-not
                ;; Validate candidates without assuming text-prefix matching.
                (lambda (candidate)
                  (and (stringp candidate)
                       (plusp (length candidate))))
                candidates)
               :test #'string=)))
        (error () nil)))))

;; Open the Completion menu for one prefix range.
(defun set-editor-completion-menu (editor candidates start end)
  (setf (get-editor-completion-candidates editor) candidates
        (get-editor-completion-index editor) 0
        (get-editor-completion-start editor) start
        (get-editor-completion-end editor) end)
  :changed)

;; Accept the selected Completion candidate and close the menu.
(defun set-editor-completion-accept (editor)
  (let* (;; Read the selected candidate.
         (candidate
           (nth (get-editor-completion-index editor)
                (get-editor-completion-candidates editor)))
         ;; Read the prefix range captured when the menu opened.
         (start (get-editor-completion-start editor))
         ;; Read the prefix end captured when the menu opened.
         (end (get-editor-completion-end editor)))
    (set-editor-buffer-octets editor (get-utf8 candidate) start end)
    (del-editor-completion editor)
    :changed))

;; Accept the Completion candidate selected by a one-based number.
(defun set-editor-completion-number (editor number)
  (let* (;; Convert the displayed number to a zero-based candidate index.
         (index (1- number))
         ;; Read the complete candidate list.
         (candidates (get-editor-completion-candidates editor)))
    (when (and (<= 1 number +editor-completion-max-visible+)
               (< index (length candidates)))
      (setf (get-editor-completion-index editor) index)
      (set-editor-completion-accept editor))))

;; Query and open the Completion menu from the current Insertion point.
(defun set-editor-completion (editor)
  (multiple-value-bind (prefix start end)
      (get-editor-completion-prefix editor)
    (if (plusp (length prefix))
        (let (;; Read matching candidates from the configured provider.
              (candidates (get-completion-candidates editor prefix)))
          (if candidates
              (set-editor-completion-menu editor candidates start end)
              (del-editor-completion editor)))
        (del-editor-completion editor))))

(defun get-editor-history-entries (editor)
  (car (get-editor-history-box editor)))

(defun editor-empty-p (editor)
  (zerop (length (get-editor-buffer editor))))

(defun editor-at-end-p (editor)
  (= (get-editor-cursor editor) (length (get-editor-buffer editor))))

(defun get-editor-line-start (buffer position)
  (loop with start = position
        while (and (> start 0)
                   (/= (aref buffer (1- start)) 10))
        do (decf start)
        finally (return start)))

(defun get-editor-line-end (buffer position)
  (loop with end = position
        while (and (< end (length buffer))
                   (/= (aref buffer end) 10))
        do (incf end)
        finally (return end)))

;; Measure terminal cells across the buffer's byte range.
(defun get-editor-display-column (buffer start end)
  (loop with column = 0
        for position = start then (+ position length)
        for length = (and (< position end)
                          (get-utf8-character-length buffer position end))
        while length
        do (incf column
                 (get-editor-cell-width buffer position column length))
        finally (return column)))

;; Choose a byte boundary nearest the requested display column.
(defun get-editor-position-at-column (buffer start end target)
  (loop with column = 0
        for position = start then (+ position length)
        for length = (and (< position end)
                          (get-utf8-character-length buffer position end))
        while length
        for width = (get-editor-cell-width buffer position column length)
        do (when (>= column target)
             (return position))
           (incf column width)
           (when (>= column target)
             (return (+ position length)))
        finally (return end)))

(defun del-editor-selection-state (editor)
  (setf (get-editor-selection-anchor editor) nil
        (get-editor-mouse-selecting-p editor) nil))

(defun get-editor-selection-range (editor)
  (let ((anchor (get-editor-selection-anchor editor))
        (cursor (get-editor-cursor editor)))
    (if (and anchor (/= anchor cursor))
        (values (min anchor cursor) (max anchor cursor))
        (values nil nil))))

(defun get-editor-selection-text (editor)
  (multiple-value-bind (start end)
      (get-editor-selection-range editor)
    (when start
      (nth-value 0
                 (get-utf8-chunk
                  (subseq (get-editor-buffer editor) start end))))))

;; Copy buffers so callers cannot mutate the edit buffer indirectly.
(defun set-editor-buffer (editor buffer &optional (cursor (length buffer)))
  (setf (get-editor-buffer editor) (new-octets-copy buffer)
        (get-editor-cursor editor) cursor
        (get-editor-preferred-column editor) nil)
  (del-editor-selection-state editor))

;; Leave history navigation and discard its saved draft.
(defun set-editor-history-navigation (editor)
  (setf (get-editor-history-index editor) nil
        (get-editor-history-draft editor) (new-octets))
  (del-editor-selection-state editor))

;; Insert raw bytes at the cursor or replace explicit bounds.
(defun set-editor-buffer-octets
    (editor octets &optional explicit-start explicit-end)
  (multiple-value-bind (selection-start selection-end)
      (get-editor-selection-range editor)
    (let* ((old (get-editor-buffer editor))
           ;; Prefer explicit bounds for Completion replacement.
           (start (or explicit-start selection-start (get-editor-cursor editor)))
           ;; Otherwise replace the current Text selection.
           (end (or explicit-end selection-end start))
           (inserted (length octets))
           (result (new-octets (+ (- (length old) (- end start)) inserted))))
      (when (or (< start end) (plusp inserted))
        (set-editor-history-navigation editor)
        (replace result old :end1 start :end2 start)
        (replace result octets :start1 start)
        (replace result old
                 :start1 (+ start inserted)
                 :start2 end)
        (setf (get-editor-buffer editor) result
              (get-editor-cursor editor) (+ start inserted)
              (get-editor-preferred-column editor) nil)
        (del-editor-selection-state editor)
        :changed))))

;; Callers must pass UTF-8-aligned byte boundaries.
(defun del-editor-buffer-range (editor start end)
  (when (< start end)
    (let* ((old (get-editor-buffer editor))
           (result (new-octets (- (length old) (- end start)))))
      (replace result old :end1 start :end2 start)
      (replace result old :start1 start :start2 end)
      (setf (get-editor-buffer editor) result
            (get-editor-cursor editor) start
            (get-editor-preferred-column editor) nil)
      (set-editor-history-navigation editor)
      :changed)))

(defun del-editor-selection (editor)
  (multiple-value-bind (start end)
      (get-editor-selection-range editor)
    (when start
      (del-editor-buffer-range editor start end))))

(defun del-editor-buffer (editor)
  (unless (editor-empty-p editor)
    (set-editor-buffer editor (new-octets))
    (set-editor-history-navigation editor)
    :changed))

;; Find the previous Editor character through Babel's boundary query.
(defun get-editor-previous-character-start (editor)
  (get-utf8-previous-character-start
   (get-editor-buffer editor)
   (get-editor-cursor editor)))

;; Return the next UTF-8 boundary, or NIL at buffer end.
(defun get-editor-next-character-end (editor)
  (let ((cursor (get-editor-cursor editor)))
    (when (< cursor (length (get-editor-buffer editor)))
      (+ cursor
         (get-utf8-character-length (get-editor-buffer editor) cursor)))))

(defun set-editor-cursor-left (editor)
  (multiple-value-bind (selection-start selection-end)
      (get-editor-selection-range editor)
    (declare (ignore selection-end))
    (let ((position (or selection-start
                        (get-editor-previous-character-start editor))))
      (when position
        (setf (get-editor-cursor editor) position
              (get-editor-preferred-column editor) nil)
        (del-editor-selection-state editor)
        :changed))))

(defun set-editor-cursor-right (editor)
  (multiple-value-bind (selection-start selection-end)
      (get-editor-selection-range editor)
    (declare (ignore selection-start))
    (let ((position (or selection-end
                        (get-editor-next-character-end editor))))
      (when position
        (setf (get-editor-cursor editor) position
              (get-editor-preferred-column editor) nil)
        (del-editor-selection-state editor)
        :changed))))

(defun set-editor-cursor-home (editor)
  (let ((position (get-editor-line-start (get-editor-buffer editor)
                                     (get-editor-cursor editor))))
    (when (or (/= position (get-editor-cursor editor))
              (nth-value 0 (get-editor-selection-range editor)))
      (setf (get-editor-cursor editor) position
            (get-editor-preferred-column editor) nil)
      (del-editor-selection-state editor)
      :changed)))

(defun set-editor-cursor-end (editor)
  (let ((position (get-editor-line-end (get-editor-buffer editor)
                                   (get-editor-cursor editor))))
    (when (or (/= position (get-editor-cursor editor))
              (nth-value 0 (get-editor-selection-range editor)))
      (setf (get-editor-cursor editor) position
            (get-editor-preferred-column editor) nil)
      (del-editor-selection-state editor)
      :changed)))

;; Keep the display column while moving between logical lines.
(defun set-editor-cursor-vertical (editor direction)
  (let* ((buffer (get-editor-buffer editor))
         (cursor (get-editor-cursor editor))
         (start (get-editor-line-start buffer cursor))
         (end (get-editor-line-end buffer cursor))
         (column (or (get-editor-preferred-column editor)
                     (get-editor-display-column buffer start cursor)))
         (target-start (if (eq direction :up)
                           (when (> start 0)
                             (get-editor-line-start buffer (1- start)))
                           (when (< end (length buffer))
                             (1+ end)))))
    (when target-start
      (let ((target (get-editor-position-at-column
                     buffer target-start
                     (get-editor-line-end buffer target-start)
                     column)))
        (setf (get-editor-cursor editor) target
              (get-editor-preferred-column editor) column)
        (del-editor-selection-state editor)
        :changed))))

(defun set-editor-history-entry (editor index)
  (let ((entry (nth index (get-editor-history-entries editor))))
    (when entry
      (set-editor-buffer editor entry)
      (setf (get-editor-history-index editor) index)
      :changed)))

;; Save the active edit buffer before entering history.
(defun set-editor-history-up (editor)
  (let ((history (get-editor-history-entries editor)))
    (when history
      (if (null (get-editor-history-index editor))
          (progn
            (setf (get-editor-history-draft editor)
                  (new-octets-copy (get-editor-buffer editor)))
            (set-editor-history-entry editor (1- (length history))))
          (set-editor-history-entry
           editor (max 0 (1- (get-editor-history-index editor))))))))

;; Restore the draft after moving below the newest history entry.
(defun set-editor-history-down (editor)
  (let ((index (get-editor-history-index editor))
        (history (get-editor-history-entries editor)))
    (when index
      (if (< index (1- (length history)))
          (set-editor-history-entry editor (1+ index))
          (progn
            (setf (get-editor-history-index editor) nil)
            (set-editor-buffer editor (get-editor-history-draft editor))
            :changed)))))

(defun del-editor-character-backward (editor)
  (or (del-editor-selection editor)
      (let ((start (get-editor-previous-character-start editor)))
        (when start
          (del-editor-buffer-range editor start (get-editor-cursor editor))))))

(defun del-editor-character-forward (editor)
  (or (del-editor-selection editor)
      (let ((end (get-editor-next-character-end editor)))
        (when end
          (del-editor-buffer-range editor (get-editor-cursor editor) end)))))

;; Apply keys that do not require Completion menu handling.
(defun set-editor-standard-key (editor key)
  (case key
    (:left (set-editor-cursor-left editor))
    (:right (set-editor-cursor-right editor))
    (:up (or (set-editor-cursor-vertical editor :up)
             (set-editor-history-up editor)))
    (:down (or (set-editor-cursor-vertical editor :down)
               (set-editor-history-down editor)))
    (:home (set-editor-cursor-home editor))
    (:end (set-editor-cursor-end editor))
    (:tab (set-editor-buffer-octets editor (vector 9)))
    (:delete (del-editor-character-forward editor))
    (:copy (let ((text (get-editor-selection-text editor)))
             (when text
               (values :copy text))))
    (:cut (let ((text (get-editor-selection-text editor)))
            (when text
              (values :cut text))))
    (:paste (values :paste-system nil))
    (:enter (set-editor-enter editor))
    (:escape (values :forward (vector 27)))
    (:history-up (set-editor-history-up editor))
    (:history-down (set-editor-history-down editor))))

;; Return true for bytes that can change the Completion prefix.
(defun get-editor-auto-completion-byte-p (octet)
  (or (>= octet 32)
      (= octet 4)
      (= octet 8)
      (= octet 127)))

;; Refresh the Completion menu after a changed text edit.
(defun set-editor-auto-completion (editor action)
  (when (eq action :changed)
    (set-editor-completion editor))
  action)

;; Map decoded terminal keys to Editor actions and History navigation.
(defun set-editor-key (editor key)
  (if (get-editor-completion-active-p editor)
      (if (eq key :escape)
          (del-editor-completion editor)
          (let (;; Remember the redraw caused by menu dismissal.
                (closed (del-editor-completion editor)))
            (multiple-value-bind (action data)
                (set-editor-standard-key editor key)
              (when (eq key :delete)
                (set-editor-auto-completion editor action))
              (values (or action closed) data))))
      (if (eq key :tab)
          (set-editor-completion editor)
          (multiple-value-bind (action data)
              (set-editor-standard-key editor key)
            (when (eq key :delete)
              (set-editor-auto-completion editor action))
            (values action data)))))

;; Copy submitted bytes because later edits replace the active buffer.
(defun set-editor-history (editor)
  (let ((entries
          (append (get-editor-history-entries editor)
                  (list (new-octets-copy (get-editor-buffer editor))))))
    (setf (car (get-editor-history-box editor)) entries)))

;; An odd run of backslashes escapes the following newline.
(defun get-submission-octets (octets)
  (let ((result (new-octets-copy octets))
        (backslash-count 0))
    (loop for index from 0 below (length result)
          for octet = (aref result index)
          do (cond
               ((= octet 92)
                (incf backslash-count))
               ((= octet 10)
                (unless (oddp backslash-count)
                  (setf (aref result index) 32))
                (setf backslash-count 0))
               (t
                (setf backslash-count 0))))
    result))

(defun new-editor-submission (editor)
  (if (editor-empty-p editor)
      (values :none nil)
      (let ((submission (get-submission-octets (get-editor-buffer editor))))
        (set-editor-history editor)
        (set-editor-buffer editor (new-octets))
        (set-editor-history-navigation editor)
        (values :submit submission))))

;; Enter submits only at the absolute buffer end.
(defun set-editor-enter (editor)
  (if (editor-at-end-p editor)
      (new-editor-submission editor)
      (values (set-editor-buffer-octets editor (vector 10)) nil)))

;; Normalize CRLF and CR while preserving all other bytes.
(defun get-pasted-newlines (octets)
  (let ((result (new-octets (length octets)))
        (count 0))
    (loop for index from 0 below (length octets)
          for octet = (aref octets index)
          unless (= octet 13)
            do (setf (aref result count) octet)
               (incf count)
          when (= octet 13)
            do (setf (aref result count) 10)
               (incf count)
               (when (and (< (1+ index) (length octets))
                          (= (aref octets (1+ index)) 10))
                 (incf index)))
    (subseq result 0 count)))

;; Insert Pasted content after closing any active Completion menu.
(defun set-editor-paste (editor octets)
  (let (;; Remember the redraw caused by menu dismissal.
        (closed (del-editor-completion editor)))
    (or (set-editor-buffer-octets editor (get-pasted-newlines octets))
        closed)))

;; Handle control bytes locally; forward unsupported bytes to the shell.
(defun set-editor-standard-byte (editor octet)
  (cond
    ((or (= octet 10) (= octet 13)) (set-editor-enter editor))
    ((or (= octet 8) (= octet 127)) (values (del-editor-character-backward editor) nil))
    ((= octet 1) (values (set-editor-cursor-home editor) nil))
    ((= octet 2) (values (set-editor-cursor-left editor) nil))
    ((= octet 3) (values (or (del-editor-buffer editor) :none) nil))
    ((= octet 4)
     (if (editor-empty-p editor)
         (values :detach nil)
         (values (or (del-editor-character-forward editor) :none) nil)))
    ((= octet 5) (values (set-editor-cursor-end editor) nil))
    ((= octet 6) (values (set-editor-cursor-right editor) nil))
    ((= octet 9) (values (set-editor-buffer-octets editor (vector 9)) nil))
    ((= octet 14) (values (set-editor-history-down editor) nil))
    ((= octet 16) (values (set-editor-history-up editor) nil))
    ((>= octet 32) (values (set-editor-buffer-octets editor (vector octet)) nil))
    (t (values :forward (vector octet)))))

;; Handle Completion keys before normal byte insertion.
(defun set-editor-byte (editor octet)
  (cond
    ((= octet 9)
     (or (set-editor-key editor :tab)
         (set-editor-standard-byte editor octet)))
    ((or (= octet 10) (= octet 13))
     (if (get-editor-completion-active-p editor)
         (set-editor-key editor :enter)
         (set-editor-standard-byte editor octet)))
    ((and (= octet 27) (get-editor-completion-active-p editor))
     (set-editor-key editor :escape))
    ((and (get-editor-completion-active-p editor)
          (<= 49 octet 57))
     (or (set-editor-completion-number editor (- octet 48))
         (let (;; Remember the redraw caused by menu dismissal.
               (closed (del-editor-completion editor)))
           (multiple-value-bind (action data)
               (set-editor-standard-byte editor octet)
             (values (or action closed) data)))))
    ((get-editor-completion-active-p editor)
     (let (;; Remember the redraw caused by menu dismissal.
           (closed (del-editor-completion editor)))
       (multiple-value-bind (action data)
           (set-editor-standard-byte editor octet)
         (when (get-editor-auto-completion-byte-p octet)
           (set-editor-auto-completion editor action))
         (values (or action closed) data))))
    ((= octet 27)
     (values :forward (vector octet)))
    (t
     (multiple-value-bind (action data)
         (set-editor-standard-byte editor octet)
       (when (get-editor-auto-completion-byte-p octet)
         (set-editor-auto-completion editor action))
       (values action data)))))

;; Delimiters for terminal bracketed paste mode.
(defparameter +paste-start+ #(27 91 50 48 48 126))
(defparameter +paste-end+ #(27 91 50 48 49 126))

;; Hold escape sequences until the terminal sends the complete key.
(defstruct (input-parser
            (:constructor new-input-parser ())
            (:conc-name get-input-parser-))
  ;; Bytes not yet classified as an input event.
  (pending (new-octets))
  ;; Bytes collected until the paste terminator arrives.
  (paste-buffer (new-octets))
  ;; True while the parser collects bracketed paste data.
  paste-p)

;; Support both CSI and SS3 forms sent by terminals.
(defparameter +escape-keys+
  '((#(27 91 65) . :up)
    (#(27 91 66) . :down)
    (#(27 91 67) . :right)
    (#(27 91 68) . :left)
    (#(27 91 72) . :home)
    (#(27 91 49 126) . :home)
    (#(27 91 70) . :end)
    (#(27 91 52 126) . :end)
    (#(27 91 51 126) . :delete)
    (#(27 91 53 126) . :history-up)
    (#(27 91 54 126) . :history-down)
    ;; Kitty keyboard protocol: Super is modifier value 9.
    (#(27 91 57 57 59 57 117) . :copy)
    (#(27 91 49 49 56 59 57 117) . :paste)
    (#(27 91 49 50 48 59 57 117) . :cut)
    (#(27 79 65) . :up)
    (#(27 79 66) . :down)
    (#(27 79 67) . :right)
    (#(27 79 68) . :left)))

(defun get-escape-key (octets)
  (cdr (assoc octets +escape-keys+ :test #'equalp)))

(defun escape-sequence-final-p (octet)
  (<= #x40 octet #x7e))

;; Recognize supported prefixes before forwarding complete sequences.
(defun known-escape-prefix-p (octets)
  (or (octets-prefix-p octets +paste-start+)
      (some (lambda (entry)
              (octets-prefix-p octets (car entry)))
            +escape-keys+)))

;; Preserve partial terminal input across reads.
(defun set-input-parser-events (parser octets &key flush-p)
  "Parse OCTETS into Editor events.
FLUSH-P resolves one pending standalone Escape key."
  (let (events)
    (labels ((set-parser-event (type payload)
               (push (list type payload) events))
             (set-parser-pending ()
               (let ((pending (get-input-parser-pending parser)))
                 (cond
                   ((/= (aref pending 0) 27)
                    (set-parser-event :byte (aref pending 0))
                    (setf (get-input-parser-pending parser)
                          (subseq pending 1)))
                   ((= (length pending) 1) nil)
                   ;; Keep CSI and SS3 escape sequences pending.
                   ((not (member (aref pending 1) '(91 79)))
                    (set-parser-event :forward (new-octets-copy pending))
                    (setf (get-input-parser-pending parser) (new-octets)))
                   ((octets-equal-p pending +paste-start+)
                    (setf (get-input-parser-paste-p parser) t
                          (get-input-parser-paste-buffer parser) (new-octets)
                          (get-input-parser-pending parser) (new-octets)))
                   ((get-escape-key pending)
                    (set-parser-event :key (get-escape-key pending))
                    (setf (get-input-parser-pending parser) (new-octets)))
                   ((and (escape-sequence-final-p (aref pending (1- (length pending))))
                         (not (known-escape-prefix-p pending)))
                    (set-parser-event :forward (new-octets-copy pending))
                    (setf (get-input-parser-pending parser) (new-octets)))
                   ((> (length pending) 8)
                    (set-parser-event :forward (new-octets-copy pending))
                    (setf (get-input-parser-pending parser) (new-octets)))))))
      (loop for octet across octets
            do (if (get-input-parser-paste-p parser)
                   (let ((paste (new-octets-with-octet
                                 (get-input-parser-paste-buffer parser)
                                 octet)))
                     (if (and (>= (length paste) (length +paste-end+))
                              (octets-equal-p
                               (subseq paste (- (length paste)
                                                (length +paste-end+)))
                               +paste-end+))
                         (progn
                           (set-parser-event :paste
                                 (subseq paste 0
                                         (- (length paste)
                                            (length +paste-end+))))
                           (setf (get-input-parser-paste-p parser) nil
                                 (get-input-parser-paste-buffer parser)
                                 (new-octets)))
                         (setf (get-input-parser-paste-buffer parser) paste)))
                   (progn
                     (setf (get-input-parser-pending parser)
                           (new-octets-with-octet
                            (get-input-parser-pending parser) octet))
                     (loop while (and (plusp (length (get-input-parser-pending parser)))
                                      (not (get-input-parser-paste-p parser)))
                           do (let ((before (get-input-parser-pending parser)))
                                (set-parser-pending)
                                (when (eq before (get-input-parser-pending parser))
                                  (return)))))))
      (when (and flush-p
                 (octets-equal-p
                  (get-input-parser-pending parser)
                  (vector 27)))
        (set-parser-event :key :escape)
        (setf (get-input-parser-pending parser) (new-octets)))
      (nreverse events))))

;; Allow rendering tests to run without an output descriptor.
(defun set-terminal-octets (fd octets)
  (when fd
    (set-fd fd octets)))

(defun set-terminal-ascii (fd text)
  (set-terminal-octets fd (get-utf8 text)))

;; Keep shell carriage returns distinct from editor CRLF line endings.
(defparameter +terminal-return+ (string (code-char 13)))
(defparameter +terminal-return-linefeed+
  (format nil "~C~C" (code-char 13) (code-char 10)))

;; Emit relative cursor movement only for positive counts.
(defun set-terminal-cursor (fd direction count)
  (when (plusp count)
    (set-terminal-ascii
     fd (format nil "~C[~D~A" (code-char 27) count direction))))

;; Track the visible edit buffer so redraws can erase it.
(defstruct (editor-render
            (:constructor new-editor-render)
            (:conc-name get-editor-render-))
  ;; One-based terminal row where the render starts.
  start-row
  ;; One-based terminal row where the Editor text starts.
  buffer-start-row
  ;; Column where the visible edit buffer begins.
  start-column
  ;; Column where the complete Editor render starts.
  render-column
  ;; Number of terminal rows occupied by the render.
  rows
  ;; Cursor position relative to the visible rows.
  cursor-row
  ;; Cursor column relative to its terminal row.
  cursor-column
  ;; Snapshot used to map mouse coordinates to byte positions.
  buffer
  ;; Visible screen lines in the buffer snapshot.
  lines)

;; Represent one visual screen line with half-open byte bounds.
(defstruct (editor-screen-line
            (:constructor new-editor-screen-line)
            (:conc-name get-editor-screen-line-))
  ;; Inclusive byte offset where this line starts.
  start
  ;; Exclusive byte offset where this line ends.
  end
  ;; Terminal column occupied before this line starts.
  start-column)

;; Split the edit buffer into newline and width-based screen lines.
;; Return one line even when the edit buffer is empty.
(defun get-editor-screen-lines (buffer width start-column)
  (let ((lines nil)
        (start 0)
        (column start-column)
        (index 0))
    (loop while (< index (length buffer))
          do (let ((octet (aref buffer index)))
               (if (= octet 10)
                   (progn
                     (push (new-editor-screen-line
                            :start start
                            :end index
                            :start-column start-column)
                           lines)
                     (setf start (1+ index)
                           column 0
                           start-column 0)
                     (incf index))
                   (let* (;; Read the current character's byte length.
                          (length (get-utf8-character-length buffer index))
                          ;; Measure the current character in terminal cells.
                          (cell-width
                            (get-editor-cell-width buffer index column length)))
                     (when (> (+ column cell-width) width)
                       (push (new-editor-screen-line
                              :start start
                              :end index
                              :start-column start-column)
                             lines)
                       (setf start index
                             column 0
                             start-column 0))
                     (incf column cell-width)
                     (incf index length)))))
    (nreverse
     (cons (new-editor-screen-line
            :start start
            :end (length buffer)
            :start-column start-column)
           lines))))

;; Prefer the next line when a visual wrap shares this position.
(defun get-editor-screen-line-index (lines cursor)
  (or (loop for line in (rest lines)
            for index from 1
            when (= cursor (get-editor-screen-line-start line))
              do (return index))
      (loop for line in lines
            for index from 0
            when (and (<= (get-editor-screen-line-start line) cursor)
                      (<= cursor (get-editor-screen-line-end line)))
              do (return index)
            finally (return (1- (length lines))))))

;; Map terminal cells to byte positions in the Editor text.
(defun get-editor-render-position (render column row &key clamp-p)
  (let* ((lines (get-editor-render-lines render))
         ;; Map mouse rows from the Editor text, not its Completion menu.
         (line-index (- row (get-editor-render-buffer-start-row render))))
    (when (and lines
               (>= row (get-editor-render-buffer-start-row render))
               (or clamp-p
                   (<= 0 line-index (1- (length lines)))))
      (setf line-index
            (if clamp-p
                (max 0 (min line-index (1- (length lines))))
                line-index))
      (let ((line (nth line-index lines)))
        (get-editor-position-at-column
         (get-editor-render-buffer render)
         (get-editor-screen-line-start line)
         (get-editor-screen-line-end line)
         (- (1- column)
            (get-editor-screen-line-start-column line)))))))

(defun editor-left-mouse-p (button)
  (and (zerop (logand button 3))
       (zerop (logand button 28))
       (zerop (logand button 64))))

;; Apply one local mouse report to the Editor area.
(defun set-editor-mouse (editor render button column row press-p)
  (let (;; Close the Completion menu for every local left-button action.
        (closed
          (and (get-editor-completion-active-p editor)
               (editor-left-mouse-p button)
               (del-editor-completion editor))))
    (or
     (when (and render (editor-left-mouse-p button))
       (let (;; Track drag reports separately from button presses.
             (motion-p (plusp (logand button 32))))
         (cond
           ((and press-p (not motion-p))
            (let ((position (get-editor-render-position render column row)))
              (when position
                (setf (get-editor-cursor editor) position
                      (get-editor-selection-anchor editor) position
                      (get-editor-mouse-selecting-p editor) t
                      (get-editor-preferred-column editor) nil)
                :changed)))
           ((and press-p (get-editor-mouse-selecting-p editor))
            (let ((old (get-editor-cursor editor))
                  (position
                    (get-editor-render-position
                     render column row :clamp-p t)))
              (when (and position (/= old position))
                (setf (get-editor-cursor editor) position
                      (get-editor-preferred-column editor) nil)
                :changed)))
           ((and (not press-p) (get-editor-mouse-selecting-p editor))
            (let ((old (get-editor-cursor editor))
                  (position
                    (get-editor-render-position
                     render column row :clamp-p t)))
              (when position
                (setf (get-editor-cursor editor) position))
              (setf (get-editor-mouse-selecting-p editor) nil)
              (when (= (get-editor-cursor editor)
                       (or (get-editor-selection-anchor editor)
                           (get-editor-cursor editor)))
                (setf (get-editor-selection-anchor editor) nil))
              (when (/= old (get-editor-cursor editor))
                :changed))))))
     closed)))

;; Render control bytes visibly while tracking the terminal cursor column.
(defun set-editor-display
    (buffer output-fd width start-column buffer-start selection-start selection-end)
  (loop with row = 0
        with column = start-column
        with index = 0
        with highlighted-p = nil
        while (< index (length buffer))
        do (let ((octet (aref buffer index)))
             (cond
               ((= octet 10)
                (set-terminal-ascii output-fd +terminal-return-linefeed+)
                (incf row)
                (setf column 0)
                (incf index))
               (t
                (let* (;; Read the current character's byte length.
                       (length (get-utf8-character-length buffer index))
                       ;; Measure the current character in terminal cells.
                       (cell-width
                         (get-editor-cell-width buffer index column length))
                       (selected-p
                         (and selection-start
                              (<= selection-start (+ buffer-start index))
                              (< (+ buffer-start index) selection-end))))
                  (when (> (+ column cell-width) width)
                    (set-terminal-ascii output-fd +terminal-return-linefeed+)
                    (incf row)
                    (setf column 0))
                  (when (and selected-p (not highlighted-p))
                    (set-terminal-ascii output-fd
                                        (format nil "~C[7m" (code-char 27))))
                  (when (and (not selected-p) highlighted-p)
                    (set-terminal-ascii output-fd
                                        (format nil "~C[27m" (code-char 27))))
                  (cond
                    ((= octet 9)
                     (set-terminal-octets output-fd (vector 9)))
                    ((or (< octet 32) (= octet 127))
                     (set-terminal-octets
                      output-fd
                      (if (= octet 127)
                          (vector 94 63)
                          (vector 94 (+ octet 64)))))
                    (t
                     (set-terminal-octets
                      output-fd
                      (subseq buffer index (+ index length)))))
                  (setf highlighted-p selected-p)
                  (incf column cell-width)
                  (incf index length)))))
        finally
           (when highlighted-p
             (set-terminal-ascii output-fd
                                 (format nil "~C[27m" (code-char 27))))
           (return (values row column))))

;; Align the Completion menu with the current Completion prefix.
(defun get-editor-completion-column
    (editor buffer lines cursor-line start-column width)
  (let* (;; Read the captured prefix start.
         (prefix-start (get-editor-completion-start editor))
         ;; Read the visual line containing the Insertion point.
         (line (nth cursor-line lines))
         ;; Read that visual line's byte bounds.
         (line-start (get-editor-screen-line-start line))
         (line-end (get-editor-screen-line-end line))
         ;; Keep the first visual line's starting column.
         (line-start-column
           (if (zerop cursor-line)
               start-column
               (get-editor-screen-line-start-column line)))
         ;; Place the menu at the prefix start when it remains visible.
         (column
           (if (<= line-start prefix-start line-end)
               (+ line-start-column
                  (get-editor-display-column
                   buffer line-start prefix-start))
               line-start-column)))
    (max 0 (min column (1- width)))))

;; Draw visible Completion candidates as ANSI menu rows.
(defun set-editor-completion-display (editor output-fd row column count)
  (let* (;; Read all candidates in provider order.
         (candidates (get-editor-completion-candidates editor))
         ;; Stop before the next hidden candidate.
         (end (min (length candidates) count))
         ;; Read the selected candidate's absolute index.
         (selected (get-editor-completion-index editor))
         ;; Use one-based terminal columns.
         (terminal-column (1+ column)))
    (loop for candidate in (subseq candidates 0 end)
          for index from 0
          for menu-row from row
          do (set-terminal-ascii
              output-fd
              (format nil "~C[~D;~DH~C[K~C[~Am"
                      (code-char 27)
                      menu-row
                      terminal-column
                      (code-char 27)
                      (code-char 27)
                      (if (= index selected) 7 2)))
             (set-terminal-ascii
              output-fd
              (format nil "~D " (1+ index)))
             (set-terminal-ascii output-fd candidate)
             (set-terminal-ascii
              output-fd
              (format nil "~C[0m~C[K" (code-char 27) (code-char 27))))))

;; Choose menu rows that remain inside the Editor area Viewport.
(defun get-editor-completion-layout
    (editor buffer lines cursor-line cursor-row start-column width height
     buffer-start-row)
  (let* (;; Keep the buffer's final visible row inside the Viewport.
         (buffer-end-row (+ buffer-start-row (1- (length lines))))
         ;; Convert the cursor row to one-based terminal coordinates.
         (cursor-absolute-row (+ buffer-start-row cursor-row))
         ;; Read all candidates to calculate remaining menu rows.
         (candidates (get-editor-completion-candidates editor))
         ;; Reserve at most the configured number of menu rows.
         (desired-count
           (min +editor-completion-max-visible+
                (length candidates)))
         ;; Count rows available below the cursor.
         (below-count (max 0 (- height cursor-absolute-row)))
         ;; Count rows available above the cursor.
         (above-count (max 0 (1- cursor-absolute-row)))
         ;; Prefer a complete menu below the cursor.
         (menu-count
           (cond
             ((>= below-count desired-count) desired-count)
             ((>= above-count desired-count) desired-count)
             ((>= below-count above-count) below-count)
             (t above-count)))
         ;; Place the menu above only when below space is insufficient.
         (above-p (and (plusp menu-count)
                       (< below-count desired-count)
                       (>= above-count below-count)))
         ;; Compute the first menu row.
         (menu-row
           (when (plusp menu-count)
             (if above-p
                 (- cursor-absolute-row menu-count)
                 (1+ cursor-absolute-row))))
         ;; Align the menu with the current Completion prefix.
         (menu-column
           (when (plusp menu-count)
             (get-editor-completion-column
              editor buffer lines cursor-line start-column width)))
         ;; Extend the render to include the menu rows.
         (render-start-row (if menu-row
                               (min buffer-start-row menu-row)
                               buffer-start-row))
         ;; Find the last row occupied by text or the menu.
         (render-end-row (if menu-row
                             (max buffer-end-row (+ menu-row (1- menu-count)))
                             buffer-end-row)))
    (values menu-row
            menu-column
            menu-count
            render-start-row
            (1+ (- render-end-row render-start-row))
            (- cursor-absolute-row render-start-row))))

;; Truncate PROMPT so it stays on one terminal row.
;; ponytail: accept plain text; add ANSI width parsing when required.
(defun get-editor-prompt-octets (prompt width start-column)
  (let* (;; Encode the Prompt line as terminal UTF-8 bytes.
         (octets (get-utf8 prompt))
         ;; Reserve one cell for the Editor cursor.
         (limit (max 0 (- width start-column 1)))
         ;; Track the next complete UTF-8 character.
         (end 0)
         ;; Track the Prompt line's terminal width.
         (column start-column))
    (loop while (< end (length octets))
          for length = (get-utf8-character-length octets end)
          for cell-width = (get-editor-cell-width octets end column length)
          do (cond
               ((= (aref octets end) 10)
                (return))
               ((> (+ column cell-width) (+ start-column limit))
                (return))
               (t
                (incf column cell-width)
                (incf end length))))
    (subseq octets 0 end)))

;; Erase the visible Editor area and restore the shell position.
(defun del-editor-render (render output-fd)
  (when render
    (set-terminal-ascii output-fd +terminal-return+)
    (set-terminal-cursor output-fd "A" (get-editor-render-cursor-row render))
    (set-terminal-cursor output-fd "C" (get-editor-render-render-column render))
    (set-terminal-ascii output-fd (format nil "~C[K" (code-char 27)))
    (loop repeat (1- (get-editor-render-rows render))
          do (set-terminal-ascii output-fd +terminal-return+)
             (set-terminal-cursor output-fd "B" 1)
             (set-terminal-ascii output-fd
                                   (format nil "~C[2K" (code-char 27))))
    (set-terminal-ascii output-fd +terminal-return+)
    (set-terminal-cursor output-fd "A" (1- (get-editor-render-rows render)))
    (set-terminal-cursor output-fd "C" (get-editor-render-render-column render))))

;; Render the Prompt line and screen lines around the cursor.
(defun set-editor-render
    (editor output-fd width height start-column &optional (start-row 1) (prompt ""))
  (let* (;; Read the complete Edit buffer.
         (buffer (get-editor-buffer editor))
         ;; Keep the Prompt line inside one terminal row.
         (prompt-octets
           (get-editor-prompt-octets prompt width start-column))
         ;; Measure the visible Prompt line in terminal cells.
         (prompt-width
           (get-editor-display-column
            prompt-octets 0 (length prompt-octets)))
         ;; Start the Edit buffer after the Prompt line.
         (buffer-start-column (+ start-column prompt-width))
         ;; Split the buffer into visible terminal rows.
         (lines (get-editor-screen-lines buffer width buffer-start-column))
         ;; Find the visual row containing the Insertion point.
         (cursor-line
           (get-editor-screen-line-index lines (get-editor-cursor editor)))
         ;; Keep the cursor near the bottom of the Viewport.
         (first-line (max 0 (- cursor-line (1- height))))
         ;; Select the visible Editor rows.
         (visible-lines
           (subseq lines first-line (min (length lines) (+ first-line height))))
         ;; Preserve the starting column after vertical scrolling.
         (visible-start-column
           (if (zerop first-line)
               buffer-start-column
               (get-editor-screen-line-start-column (first visible-lines))))
         ;; Convert the one-based terminal row to a zero-based offset.
         (visible-start-row
           (max 0
                (min (1- start-row)
                     (- height (length visible-lines)))))
         ;; Store the buffer's one-based terminal start row.
         (buffer-start-row (1+ visible-start-row)))
    (multiple-value-bind (selection-start selection-end)
        (get-editor-selection-range editor)
      (set-terminal-ascii
       output-fd
       (format nil "~C[~D;1H" (code-char 27) buffer-start-row))
      (set-terminal-ascii output-fd +terminal-return+)
      (set-terminal-ascii output-fd (format nil "~C[2K" (code-char 27)))
      (set-terminal-cursor output-fd "C" start-column)
      (set-terminal-octets output-fd prompt-octets)
      (loop for line in visible-lines
            for index from 0
            do (set-editor-display
                (subseq buffer
                        (get-editor-screen-line-start line)
                        (get-editor-screen-line-end line))
                output-fd width
                (if (zerop index)
                    visible-start-column
                    0)
                (get-editor-screen-line-start line)
                selection-start
                selection-end)
               (when (< (1+ index) (length visible-lines))
                 (set-terminal-ascii output-fd +terminal-return-linefeed+)))
      (let* (;; Store the cursor row inside the visible buffer.
             (cursor-row (- cursor-line first-line))
             ;; Read the visual line containing the cursor.
             (line (nth cursor-line lines))
             ;; Preserve the visual line's starting terminal column.
             (line-start-column
               (if (zerop cursor-line)
                   buffer-start-column
                   (get-editor-screen-line-start-column line)))
             ;; Measure the cursor column in terminal cells.
             (cursor-column
               (+ line-start-column
                  (get-editor-display-column
                   buffer
                   (get-editor-screen-line-start line)
                   (get-editor-cursor editor))))
             ;; Convert the cursor row to one-based terminal coordinates.
             (cursor-absolute-row (+ buffer-start-row cursor-row)))
        (set-terminal-ascii output-fd +terminal-return+)
        (set-terminal-cursor output-fd "A"
                              (- (1- (length visible-lines)) cursor-row))
        (set-terminal-cursor output-fd "C" cursor-column)
        (multiple-value-bind
              (menu-row menu-column menu-count render-start-row render-rows
               render-cursor-row)
            (get-editor-completion-layout
             editor buffer lines cursor-line cursor-row buffer-start-column width height
             buffer-start-row)
          (when (and menu-row (plusp menu-count))
            (set-editor-completion-display
             editor output-fd menu-row menu-column menu-count))
          ;; Restore the Editor cursor after drawing the Completion menu.
          (set-terminal-ascii
           output-fd
           (format nil "~C[~D;~DH"
                   (code-char 27)
                   cursor-absolute-row
                   (1+ cursor-column)))
          (new-editor-render
           :start-row render-start-row
           :buffer-start-row buffer-start-row
           :start-column visible-start-column
           :render-column
           (if (zerop first-line) start-column visible-start-column)
           :rows render-rows
           :cursor-row render-cursor-row
           :cursor-column cursor-column
           :buffer buffer
           :lines visible-lines))))))
