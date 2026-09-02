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

(defun utf8-continuation-p (octet)
  (= (logand octet #xc0) #x80))

;; Treat malformed or incomplete UTF-8 input as one byte.
(defun get-utf8-sequence-length (octets start)
  (let* ((first (aref octets start))
         (expected (cond
                     ((<= #x00 first #x7f) 1)
                     ((<= #xc2 first #xdf) 2)
                     ((<= #xe0 first #xef) 3)
                     ((<= #xf0 first #xf4) 4)
                     (t 1))))
    (if (and (> expected 1)
             (<= (+ start expected) (length octets))
             (loop for index from (1+ start) below (+ start expected)
                   always (utf8-continuation-p (aref octets index))))
        expected
        1)))

;; Account for tabs and caret notation.
(defun get-editor-cell-width (octets position column)
  (cond
    ((= (aref octets position) 9)
     (- 8 (mod column 8)))
    ((< (aref octets position) 32) 2)
    ((= (aref octets position) 127) 2)
    ;; ponytail: one-cell Unicode width; add a width table if needed.
    (t 1)))

;; Keep bytes so submissions preserve spaces and UTF-8 text.
(defstruct (editor
            (:constructor new-editor (&key (history-box (cons nil nil))))
            (:conc-name get-editor-))
  (buffer (new-octets))
  ;; Store cursor offsets as byte positions.
  (cursor 0)
  ;; Shared Session History lives in the car of this box.
  history-box
  ;; NIL means the cursor is outside history navigation.
  history-index
  ;; Restore the edit buffer after leaving history.
  (history-draft (new-octets))
  ;; Keep the requested display column during vertical movement.
  preferred-column
  ;; Store the mouse anchor while selecting text.
  selection-anchor
  mouse-selecting-p)

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
                          (get-utf8-sequence-length buffer position))
        while length
        do (incf column (get-editor-cell-width buffer position column))
        finally (return column)))

;; Choose a byte boundary nearest the requested display column.
(defun get-editor-position-at-column (buffer start end target)
  (loop with column = 0
        for position = start then (+ position length)
        for length = (and (< position end)
                          (get-utf8-sequence-length buffer position))
        while length
        for width = (get-editor-cell-width buffer position column)
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

;; Insert raw bytes at the cursor without decoding them.
(defun set-editor-buffer-octets (editor octets)
  (multiple-value-bind (selection-start selection-end)
      (get-editor-selection-range editor)
    (let* ((old (get-editor-buffer editor))
           (start (or selection-start (get-editor-cursor editor)))
           (end (or selection-end start))
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

;; Walk backward across UTF-8 continuation bytes.
(defun get-editor-previous-character-start (editor)
  (let ((cursor (get-editor-cursor editor))
        (buffer (get-editor-buffer editor)))
    (when (> cursor 0)
      (loop with start = (1- cursor)
            while (and (> start 0)
                       (utf8-continuation-p (aref buffer start)))
            do (decf start)
            finally (return start)))))

;; Return the next UTF-8 boundary, or NIL at buffer end.
(defun get-editor-next-character-end (editor)
  (let ((cursor (get-editor-cursor editor)))
    (when (< cursor (length (get-editor-buffer editor)))
      (+ cursor
         (get-utf8-sequence-length (get-editor-buffer editor) cursor)))))

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

;; Map decoded terminal keys to Editor actions and History navigation.
(defun set-editor-key (editor key)
  (case key
    (:left (set-editor-cursor-left editor))
    (:right (set-editor-cursor-right editor))
    (:up (or (set-editor-cursor-vertical editor :up)
             (set-editor-history-up editor)))
    (:down (or (set-editor-cursor-vertical editor :down)
               (set-editor-history-down editor)))
    (:home (set-editor-cursor-home editor))
    (:end (set-editor-cursor-end editor))
    (:delete (del-editor-character-forward editor))
    (:copy (let ((text (get-editor-selection-text editor)))
             (when text
               (values :copy text))))
    (:cut (let ((text (get-editor-selection-text editor)))
            (when text
              (values :cut text))))
    (:paste (values :paste-system nil))
    (:history-up (set-editor-history-up editor))
    (:history-down (set-editor-history-down editor))))

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

(defun set-editor-paste (editor octets)
  (set-editor-buffer-octets editor (get-pasted-newlines octets)))

;; Handle control bytes locally; forward unsupported bytes to the shell.
(defun set-editor-byte (editor octet)
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
(defun set-input-parser-events (parser octets)
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
  ;; Column where the visible edit buffer begins.
  start-column
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
                   (let* ((length (get-utf8-sequence-length buffer index))
                          (cell-width (get-editor-cell-width buffer index column)))
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

(defun get-editor-render-position (render column row &key clamp-p)
  (let* ((lines (get-editor-render-lines render))
         (line-index (- row (get-editor-render-start-row render))))
    (when (and lines
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

(defun set-editor-mouse (editor render button column row press-p)
  (when (and render (editor-left-mouse-p button))
    (let ((motion-p (plusp (logand button 32))))
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
             :changed)))))))

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
                (let* ((length (get-utf8-sequence-length buffer index))
                       (cell-width (get-editor-cell-width buffer index column))
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

;; Clear only the editor text, then leave the shell prompt intact.
;; Erase the visible Editor area and restore the shell position.
(defun del-editor-render (render output-fd)
  (when render
    (set-terminal-ascii output-fd +terminal-return+)
    (set-terminal-cursor output-fd "A" (get-editor-render-cursor-row render))
    (set-terminal-cursor output-fd "C" (get-editor-render-start-column render))
    (set-terminal-ascii output-fd (format nil "~C[K" (code-char 27)))
    (loop repeat (1- (get-editor-render-rows render))
          do (set-terminal-ascii output-fd +terminal-return+)
             (set-terminal-cursor output-fd "B" 1)
             (set-terminal-ascii output-fd
                                   (format nil "~C[2K" (code-char 27))))
    (set-terminal-ascii output-fd +terminal-return+)
    (set-terminal-cursor output-fd "A" (1- (get-editor-render-rows render)))
    (set-terminal-cursor output-fd "C" (get-editor-render-start-column render))))

;; Render only the screen lines around the cursor.
(defun set-editor-render
    (editor output-fd width height start-column &optional (start-row 1))
  (let* ((buffer (get-editor-buffer editor))
         (lines (get-editor-screen-lines buffer width start-column))
         (cursor-line (get-editor-screen-line-index lines (get-editor-cursor editor)))
         (first-line (max 0 (- cursor-line (1- height))))
         (visible-lines
           (subseq lines first-line (min (length lines) (+ first-line height))))
         (visible-start-column
           (if (zerop first-line)
               start-column
               (get-editor-screen-line-start-column (first visible-lines))))
         (visible-start-row
           ;; Convert the one-based terminal row to a zero-based offset.
           (max 0
                (min (1- start-row)
                     (- height (length visible-lines))))))
    (multiple-value-bind (selection-start selection-end)
        (get-editor-selection-range editor)
      (set-terminal-ascii
       output-fd
       (format nil "~C[~D;1H" (code-char 27) (1+ visible-start-row)))
      (set-terminal-ascii output-fd +terminal-return+)
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
      (let* ((cursor-row (- cursor-line first-line))
             (line (nth cursor-line lines))
             (line-start-column
               (if (zerop cursor-line)
                   start-column
                   (get-editor-screen-line-start-column line)))
             (cursor-column
               (+ line-start-column
                  (get-editor-display-column
                   buffer
                   (get-editor-screen-line-start line)
                   (get-editor-cursor editor)))))
        (set-terminal-ascii output-fd +terminal-return+)
        (set-terminal-cursor output-fd "A"
                              (- (1- (length visible-lines)) cursor-row))
        (set-terminal-cursor output-fd "C" cursor-column)
        (new-editor-render
         :start-row (1+ visible-start-row)
         :start-column visible-start-column
         :rows (length visible-lines)
         :cursor-row cursor-row
         :cursor-column cursor-column
         :buffer buffer
         :lines visible-lines)))))
