(in-package #:mtm.frontend)

(defconstant +frontend-poll-timeout+ 100)
(defconstant +status-refresh-interval+ 1)
(defconstant +status-input-delay+ 0.05)
(defconstant +mtm-control-max-bytes+ 128)

(defparameter +mtm-control-prefix+
  (get-utf8 (format nil "~C]MTM;" #\Escape)))

(defstruct (session-frontend
            (:constructor new-session-frontend
                (&key name
                      attachment
                      socket-fd
                      input-fd
                      output-fd
                      sessions
                      (application-p nil)
                      (socket-control-p nil)
                      (rows 24)
                      (columns 80)
                      (input-buffer
                       (make-array 0 :element-type '(unsigned-byte 8))))))
  name
  attachment
  socket-fd
  input-fd
  output-fd
  sessions
  (application-p nil)
  (socket-control-p nil)
  expanded-p
  (rows 24 :type integer)
  (columns 80 :type integer)
  (next-refresh 0)
  (drawn-session-count 0)
  input-buffer
  (control-buffer
   (make-array 0 :element-type '(unsigned-byte 8)))
  (control-since nil)
  (last-application-rows 0)
  (last-application-columns 0)
  (pending-input-since nil)
  (mouse-captured-p nil)
  requested-name
  editor
  input-parser
  editor-render)

(defun event-readable-p (events fd)
  (let ((revents (cdr (assoc fd events))))
    (and revents
         (plusp (logand revents
                        (logior +pollin+ +pollerr+ +pollhup+ +pollnval+))))))

(defun set-raw-input (input-fd function)
  (if input-fd
      (set-raw-terminal function :fd input-fd)
      (funcall function)))

(defun get-mtm-control-prefix-suffix-start (bytes)
  "Return the start of a partial MTM control prefix in BYTES."
  (let ((length (length bytes))
        (prefix-length (length +mtm-control-prefix+)))
    (loop for start from (max 0 (- length (1- prefix-length))) below length
          when (and (< (- length start) prefix-length)
                    (loop for prefix-index below (- length start)
                          always (= (aref bytes (+ start prefix-index))
                                    (aref +mtm-control-prefix+ prefix-index))))
            do (return start))))

(defun get-mtm-control (bytes start)
  "Return the status, end, and payload of an MTM control."
  (let* ((payload-start (+ start (length +mtm-control-prefix+)))
         (end (position 7 bytes :start payload-start)))
    (cond
      (end
       (values :complete (1+ end)
               (map 'string #'code-char
                    (subseq bytes payload-start end))))
      ((> (- (length bytes) start) +mtm-control-max-bytes+)
       (values :invalid payload-start nil))
      (t
       (values :incomplete start nil)))))

(defun set-mtm-controls (frontend bytes ordinary-function control-function)
  "Process MTM controls while preserving ordinary BYTES."
  (let* ((buffer
           (concatenate '(vector (unsigned-byte 8))
                        (session-frontend-control-buffer frontend)
                        bytes))
         (offset 0)
         (activity-p nil)
         (result nil))
    (labels ((set-ordinary-output (start end)
               (when (< start end)
                 (setf activity-p t
                       result (or result
                                  (funcall ordinary-function
                                           (subseq buffer start end))))))
             (set-pending-control (start)
               (set-ordinary-output offset start)
               (setf (session-frontend-control-buffer frontend)
                     (subseq buffer start)
                     (session-frontend-control-since frontend)
                     (or (session-frontend-control-since frontend)
                         (get-internal-real-time)))))
      (loop
        (let ((start (search +mtm-control-prefix+ buffer
                             :start2 offset
                             :test #'=)))
          (cond
            (start
             (set-ordinary-output offset start)
             (multiple-value-bind (status end payload)
                (get-mtm-control buffer start)
                (case status
                  (:complete
                   (setf activity-p t
                         offset end)
                   (funcall control-function payload))
                  (:invalid
                   (set-ordinary-output offset end)
                   (setf offset end))
                  (:incomplete
                   (set-pending-control start)
                   (return)))))
            (t
             (let ((partial-start
                     (get-mtm-control-prefix-suffix-start
                      (subseq buffer offset))))
               (if partial-start
                   (set-pending-control (+ offset partial-start))
                   (progn
                     (set-ordinary-output offset (length buffer))
                     (setf (session-frontend-control-buffer frontend)
                           (make-array 0 :element-type '(unsigned-byte 8))
                           (session-frontend-control-since frontend) nil)))
               (return))))))
    (values result activity-p))))

(defun set-pending-mtm-control-output (frontend ordinary-function)
  "Forward an incomplete MTM control after a short delay."
  (let ((since (session-frontend-control-since frontend)))
    (when (and since
               (>= (/ (- (get-internal-real-time) since)
                      internal-time-units-per-second)
                   +status-input-delay+))
      (let ((bytes (session-frontend-control-buffer frontend)))
        (setf (session-frontend-control-buffer frontend)
              (make-array 0 :element-type '(unsigned-byte 8))
              (session-frontend-control-since frontend) nil)
        (when (plusp (length bytes))
          (values (funcall ordinary-function bytes) t))))))

(defun set-mtm-control (fd payload)
  "Write one MTM control to FD."
  (when fd
    (set-fd fd
            (get-utf8
             (format nil "~C]MTM;~A~C"
                     #\Escape payload (code-char 7))))))

(defun set-frontend-application-mode (frontend application-p)
  "Set FRONTEND's local Application passthrough state."
  (let ((application-p (not (null application-p))))
    (setf (session-frontend-last-application-rows frontend) 0
          (session-frontend-last-application-columns frontend) 0)
    (unless (eql application-p (session-frontend-application-p frontend))
      (setf (session-frontend-application-p frontend) application-p)
      (if application-p
          (del-frontend-editor-render frontend)
          (set-frontend-editor frontend)))
    application-p))

(defun set-frontend-mode-from-session (frontend)
  "Synchronize FRONTEND's mode with its attached Session."
  (let ((attachment (session-frontend-attachment frontend)))
    (when attachment
      (let ((application-p
              (session-application-p (attachment-session attachment))))
        (unless (eql application-p
                     (session-frontend-application-p frontend))
          (set-frontend-application-mode frontend application-p)
          (when (session-frontend-socket-control-p frontend)
            (set-mtm-control
             (session-frontend-output-fd frontend)
             (format nil "mode;~A"
                     (if application-p "application" "editor"))))))))
  frontend)

(defun get-status-bar-row-count (frontend)
  "Return the number of rows reserved by FRONTEND's status bar."
  (1+ (if (session-frontend-expanded-p frontend)
          (session-frontend-drawn-session-count frontend)
          0)))

(defun set-application-size (frontend)
  "Keep the PTY below FRONTEND's reserved status bar rows."
  (when (session-frontend-application-p frontend)
    (let ((rows (max 1 (- (session-frontend-rows frontend)
                          (get-status-bar-row-count frontend))))
          (columns (session-frontend-columns frontend)))
      (unless (and (= rows (session-frontend-last-application-rows frontend))
                   (= columns
                      (session-frontend-last-application-columns frontend)))
        (cond
          ((session-frontend-attachment frontend)
           (set-attachment-terminal-size
            (session-frontend-attachment frontend) rows columns))
          ((and (session-frontend-socket-fd frontend)
                (not (session-frontend-socket-control-p frontend)))
           (set-mtm-control
            (session-frontend-socket-fd frontend)
            (format nil "resize;~D;~D" rows columns))))
        (setf (session-frontend-last-application-rows frontend) rows
              (session-frontend-last-application-columns frontend) columns))))
  frontend)

(defun get-status-row-text (text columns)
  "Fit TEXT to one terminal row of COLUMNS cells."
  (let* ((text (princ-to-string text))
         (width (max 1 columns)))
    (format nil "~vA" width
            (if (> (length text) width)
                (subseq text 0 width)
                text))))

(defun get-green-status-row (row columns text &key bright-p)
  "Return ANSI output for one green status row."
  (format nil "~C[~D;1H~C[30;~Am~A~C[0m"
          #\Escape
          row
          #\Escape
          (if bright-p 102 42)
          (get-status-row-text text columns)
          #\Escape))

(defun get-session-manager-status-bar
    (sessions current-name rows columns expanded-p
     &optional (previous-session-count 0))
  "Return ANSI output for the Session manager status bar."
  (let ((visible-session-count
          (min (max 0 (1- rows)) (length sessions))))
    (with-output-to-string (output)
      (format output "~C[s" #\Escape)
      ;; Clear rows left by a shorter Session list.
      (when expanded-p
        (loop for index from (1+ visible-session-count)
                to (min previous-session-count (max 0 (1- rows)))
              do (format output "~C[~D;1H~C[2K"
                         #\Escape (- rows index) #\Escape)))
      (write-string
       (get-green-status-row
        rows
        columns
        (format nil " session-manager: ~D sessions " (length sessions)))
       output)
      (when expanded-p
        (loop for entry in sessions
              for index from 1
              while (< index rows)
              for row = (- rows index)
              do (write-string
                  (get-green-status-row
                   row
                   columns
                   (format nil " ~A [~A] "
                           (car entry)
                           (string-downcase (princ-to-string (cdr entry))))
                   :bright-p (and current-name
                                  (string= current-name (car entry))))
                  output)))
      (format output "~C[u" #\Escape))))

(defun set-status-bar-size (frontend)
  "Refresh FRONTEND's terminal dimensions when its output is a TTY."
  (let ((output-fd (session-frontend-output-fd frontend)))
    (when output-fd
      (handler-case
          (multiple-value-bind (rows columns)
              (get-terminal-size output-fd)
            (when (and (plusp rows) (plusp columns))
              (setf (session-frontend-rows frontend) rows
                    (session-frontend-columns frontend) columns)))
        (error () nil))))
  frontend)

(defun set-session-manager-status-bar (frontend)
  "Draw FRONTEND's local Session manager status bar."
  (let ((output-fd (session-frontend-output-fd frontend)))
    (when output-fd
      (set-status-bar-size frontend)
      (set-fd
       output-fd
       (get-utf8
        (get-session-manager-status-bar
         (session-frontend-sessions frontend)
         (session-frontend-name frontend)
         (session-frontend-rows frontend)
         (session-frontend-columns frontend)
         (session-frontend-expanded-p frontend)
         (session-frontend-drawn-session-count frontend))))
      (setf (session-frontend-drawn-session-count frontend)
            (if (session-frontend-expanded-p frontend)
                (min (max 0 (1- (session-frontend-rows frontend)))
                     (length (session-frontend-sessions frontend)))
                0))
      (set-application-size frontend))))

(defun set-status-bar-sessions (frontend sessions)
  "Store SESSIONS on FRONTEND and schedule the next refresh."
  (setf (session-frontend-sessions frontend) sessions
        (session-frontend-next-refresh frontend)
        (+ (get-universal-time) +status-refresh-interval+)))

(defun set-frontend-control (frontend text)
  "Write local terminal control TEXT without sending it to a Session."
  (let ((output-fd (session-frontend-output-fd frontend)))
    (when output-fd
      (set-fd output-fd (get-utf8 text)))))

(defun set-bracketed-paste (frontend)
  "Ask the local TTY to wrap pasted content."
  (set-frontend-control
   frontend
   (format nil "~C[?2004h" #\Escape)))

(defun del-bracketed-paste (frontend)
  "Disable bracketed paste after the frontend stops."
  (set-frontend-control
   frontend
   (format nil "~C[?2004l" #\Escape)))

(defun set-status-bar-mouse (frontend)
  "Enable SGR mouse reports for the local frontend."
  (set-frontend-control
   frontend
   (format nil "~C[?1002h~C[?1006h" #\Escape #\Escape)))

(defun del-status-bar-mouse (frontend)
  "Disable SGR mouse reports after the frontend stops."
  (set-frontend-control
   frontend
   (format nil "~C[?1006l~C[?1002l" #\Escape #\Escape)))

(defun del-session-manager-status-bar (frontend)
  "Clear FRONTEND's drawn status rows before frontend shutdown."
  (let ((rows (session-frontend-rows frontend))
        (count (session-frontend-drawn-session-count frontend)))
    (when (session-frontend-output-fd frontend)
      (set-frontend-control
       frontend
       (with-output-to-string (output)
         (format output "~C[s" #\Escape)
         (loop for index from 0 to count
               do (format output "~C[~D;1H~C[2K"
                          #\Escape (- rows index) #\Escape))
         (format output "~C[u" #\Escape))))
    (setf (session-frontend-drawn-session-count frontend) 0)))

(defun get-status-bar-session-at-row (frontend row)
  "Return the Session entry under ROW, or NIL."
  (let* ((rows (session-frontend-rows frontend))
         (sessions (session-frontend-sessions frontend))
         (index (- rows row 1)))
    (when (and (session-frontend-expanded-p frontend)
               (< 0 row rows)
               (<= 0 index)
               (< index (length sessions)))
      (nth index sessions))))

(defun set-frontend-requested-session (frontend name)
  "Queue NAME as FRONTEND's next Session."
  (when name
    (setf (session-frontend-requested-name frontend) name)))

(defun set-status-bar-mouse-event
    (frontend button column row press-p)
  "Handle one mouse report and return true when it belongs to the bar."
  (declare (ignore column))
  (let ((manager-row (session-frontend-rows frontend))
        (status-row-p
          (or (= row (session-frontend-rows frontend))
              (get-status-bar-session-at-row frontend row))))
    (cond
      ((not (and (= (logand button 3) 0)
                 (zerop (logand button 32))))
       nil)
      ((not press-p)
       (when (session-frontend-mouse-captured-p frontend)
         (setf (session-frontend-mouse-captured-p frontend) nil)
         t))
      ((= row manager-row)
       (setf (session-frontend-mouse-captured-p frontend) t)
       (if (session-frontend-expanded-p frontend)
           (set-frontend-requested-session frontend (session-frontend-name frontend))
           (progn
             (setf (session-frontend-expanded-p frontend) t
                   (session-frontend-next-refresh frontend) 0)
             (set-session-manager-status-bar frontend)
             t)))
      ((and (session-frontend-expanded-p frontend)
            status-row-p)
       (setf (session-frontend-mouse-captured-p frontend) t)
       (set-frontend-requested-session frontend (car status-row-p))
       t)
      (t nil))))

(defun get-decimal-mouse-field (bytes start end)
  "Parse one decimal field from BYTES."
  (when (< start end)
    (ignore-errors
      (parse-integer (map 'string #'code-char (subseq bytes start end))))))

(defun get-sgr-mouse-fields (bytes start end)
  "Parse semicolon-separated SGR mouse fields."
  (let ((fields nil)
        (field-start start))
    (loop for index from start below end
          when (= (aref bytes index) (char-code #\;))
            do (push (get-decimal-mouse-field bytes field-start index)
                     fields)
               (setf field-start (1+ index)))
    (push (get-decimal-mouse-field bytes field-start end) fields)
    (nreverse fields)))

(defun get-sgr-mouse-report (bytes start)
  "Parse an SGR mouse report beginning at START."
  (unless (and (<= (+ start 3) (length bytes))
               (= (aref bytes start) 27)
               (= (aref bytes (1+ start)) 91)
               (= (aref bytes (+ start 2)) 60))
    (return-from get-sgr-mouse-report (values :not-a-report start)))
  (loop for end from (+ start 3) below (length bytes)
        for byte = (aref bytes end)
        when (or (= byte (char-code #\M))
                 (= byte (char-code #\m)))
          do (let ((fields (get-sgr-mouse-fields bytes (+ start 3) end)))
               (if (and (= (length fields) 3)
                        (every #'integerp fields))
                   (return-from get-sgr-mouse-report
                     (values :event
                             (1+ end)
                             (first fields)
                             (second fields)
                             (third fields)
                             (= byte (char-code #\M))))
                   (return-from get-sgr-mouse-report
                     (values :invalid (1+ end)))))
        finally (return (values :incomplete start))))

(defun set-session-bytes (frontend bytes)
  "Write BYTES to FRONTEND's Session source."
  (when (and bytes (plusp (length bytes)))
    (cond
      ((session-frontend-attachment frontend)
       (set-attachment-input (session-frontend-attachment frontend) bytes))
      ((session-frontend-socket-fd frontend)
       (set-fd (session-frontend-socket-fd frontend) bytes)))))

(defun set-frontend-editor (frontend)
  "Attach a local Editor area when FRONTEND owns an Attachment."
  (let ((attachment (session-frontend-attachment frontend)))
    (setf (session-frontend-editor-render frontend) nil)
    (if attachment
        (progn
          (unless (session-frontend-editor frontend)
            (setf (session-frontend-editor frontend)
                  (new-editor :history-box
                              (session-history-box
                               (attachment-session attachment)))))
          (unless (session-frontend-input-parser frontend)
            (setf (session-frontend-input-parser frontend)
                  (new-input-parser))))
        (setf (session-frontend-editor frontend) nil
              (session-frontend-input-parser frontend) nil))))

(defun del-frontend-editor (frontend)
  "Forget FRONTEND's Editor area before changing Attachments."
  (del-frontend-editor-render frontend)
  (setf (session-frontend-editor frontend) nil
        (session-frontend-input-parser frontend) nil))

(defun get-editor-viewport-height (frontend)
  "Return overlay rows that leave the status bar visible."
  (max 1
       (- (session-frontend-rows frontend)
          1
          (session-frontend-drawn-session-count frontend))))

(defun get-frontend-terminal-cursor-row (frontend)
  "Return the current terminal cursor row for FRONTEND."
  (let ((attachment (session-frontend-attachment frontend)))
    (if attachment
        (handler-case
            (multiple-value-bind (row column)
                (get-terminal-cursor-position
                 (get-retained-screen (attachment-session attachment)))
              (declare (ignore column))
              (1- row))
          (error () 0))
        0)))

(defun del-frontend-editor-render (frontend)
  "Erase FRONTEND's Editor area overlay."
  (let ((render (session-frontend-editor-render frontend)))
    (when render
      (del-editor-render render (session-frontend-output-fd frontend))
      (setf (session-frontend-editor-render frontend) nil))))

(defun set-frontend-editor-render (frontend)
  "Draw FRONTEND's Editor area at column 0."
  (let ((editor (session-frontend-editor frontend))
        (output-fd (session-frontend-output-fd frontend)))
    (when (and editor
               output-fd
               (not (session-frontend-application-p frontend)))
      (if (editor-empty-p editor)
          (del-frontend-editor-render frontend)
          (progn
            (set-status-bar-size frontend)
            (del-frontend-editor-render frontend)
            (setf (session-frontend-editor-render frontend)
                  (set-editor-render
                   editor
                   output-fd
                   (session-frontend-columns frontend)
                   (get-editor-viewport-height frontend)
                   0
                   (1+ (get-frontend-terminal-cursor-row frontend)))))))))

(defun set-editor-mouse-event
    (frontend button column row press-p)
  "Handle mouse input for the local Editor area."
  (let ((editor (session-frontend-editor frontend)))
    (when editor
      (when (eq (set-editor-mouse
                 editor
                 (session-frontend-editor-render frontend)
                 button column row press-p)
                :changed)
        (set-frontend-editor-render frontend))
      t)))

(defun set-editor-action (frontend action payload)
  "Apply one Editor area ACTION and return :detach when leaving."
  (case action
    (:changed
     (set-frontend-editor-render frontend)
     nil)
    (:submit
     (del-frontend-editor-render frontend)
     (when payload
       (let ((line (make-array (1+ (length payload))
                               :element-type '(unsigned-byte 8))))
         (replace line payload)
         (setf (aref line (length payload)) 10)
         (set-session-bytes frontend line)))
     nil)
    (:copy
     (when payload
       (set-system-clipboard payload))
     nil)
    (:cut
     (when (and payload
                (set-system-clipboard payload))
       (del-editor-selection (session-frontend-editor frontend))
       (set-frontend-editor-render frontend))
     nil)
    (:paste-system
     (let ((text (get-system-clipboard)))
       (when text
         (when (set-editor-paste
                (session-frontend-editor frontend)
                (get-utf8 text))
           (set-frontend-editor-render frontend))))
     nil)
    (:detach
     (del-frontend-editor-render frontend)
     :detach)
    (:forward
     (del-frontend-editor-render frontend)
     (set-session-bytes frontend payload)
     nil)
    (t nil)))

(defun set-editor-events (frontend events)
  "Handle parsed Editor area events. Return true to detach."
  (let ((detach-p nil))
    (dolist (event events)
      (let ((type (first event))
            (payload (second event)))
        (multiple-value-bind (action data)
            (ecase type
              (:byte (set-editor-byte
                      (session-frontend-editor frontend) payload))
              (:key (or (set-editor-key
                         (session-frontend-editor frontend) payload)
                        :none))
              (:paste (or (set-editor-paste
                           (session-frontend-editor frontend) payload)
                          :none))
              (:forward (values :forward payload)))
          (when (eq (set-editor-action frontend action data) :detach)
            (setf detach-p t)))))
    detach-p))

(defun set-editor-chunk (frontend bytes)
  "Parse BYTES for FRONTEND's Editor area. Return true to detach."
  (set-editor-events
   frontend
   (set-input-parser-events
    (session-frontend-input-parser frontend)
    bytes)))

(defun set-frontend-chunk (frontend bytes)
  "Send BYTES to Application passthrough or the Editor area."
  (cond
    ((or (null bytes) (zerop (length bytes))) nil)
    ((session-frontend-application-p frontend)
     (set-session-bytes frontend bytes)
     nil)
    ((session-frontend-editor frontend)
     (set-editor-chunk frontend bytes))
    (t
     (set-session-bytes frontend bytes)
     nil)))

(defun get-application-resize-control (payload)
  "Return row and column counts from a resize control payload."
  (let* ((prefix "resize;")
         (start (length prefix))
         (separator (and (uiop:string-prefix-p prefix payload)
                         (position #\; payload :start start))))
    (when separator
      (ignore-errors
        (let ((rows (parse-integer payload :start start :end separator))
              (columns (parse-integer payload :start (1+ separator))))
          (when (and (plusp rows) (plusp columns))
            (values rows columns)))))))

(defun set-frontend-control-event (frontend payload)
  "Apply one manager control received from a local frontend."
  (multiple-value-bind (rows columns)
      (get-application-resize-control payload)
    (when (and rows columns (session-frontend-attachment frontend))
      (set-attachment-terminal-size
       (session-frontend-attachment frontend) rows columns))))

(defun set-application-mode-control (frontend payload)
  "Apply one Application mode control received from the manager."
  (cond
    ((string= payload "mode;application")
     (set-frontend-application-mode frontend t))
    ((string= payload "mode;editor")
     (set-frontend-application-mode frontend nil))
    (t nil)))

(defun set-session-frontend-input (frontend bytes)
  "Consume status bar input, then the Editor area. Return true to detach."
  (let* ((buffer
           (concatenate '(vector (unsigned-byte 8))
                        (session-frontend-input-buffer frontend)
                        bytes))
         (length (length buffer))
         (offset 0)
         (forward-start 0)
         (detach-p nil))
    (labels ((set-frontend-input-range (end)
               (when (> end forward-start)
                 (when (set-frontend-chunk
                        frontend
                        (subseq buffer forward-start end))
                   (setf detach-p t)))
               (setf forward-start end)))
      (loop while (< offset length)
            do (cond
                 ((and (= (aref buffer offset) 27)
                       (<= (+ offset 3) length)
                       (= (aref buffer (1+ offset)) 91)
                       (= (aref buffer (+ offset 2)) 60))
                  (set-frontend-input-range offset)
                  (multiple-value-bind (kind end button column row press-p)
                      (get-sgr-mouse-report buffer offset)
                    (case kind
                      (:incomplete (return))
                      (:event
                       (unless (or (set-status-bar-mouse-event
                                    frontend button column row press-p)
                                   (set-editor-mouse-event
                                    frontend button column row press-p))
                         (when (set-frontend-chunk
                                frontend
                                (subseq buffer offset end))
                           (setf detach-p t)))
                       (setf offset end
                             forward-start end))
                      (:invalid
                       (when (set-frontend-chunk
                              frontend
                              (subseq buffer offset end))
                         (setf detach-p t))
                       (setf offset end
                             forward-start end)))))
                 ((and (= (aref buffer offset) 27)
                       (or (= (1+ offset) length)
                           (and (= (aref buffer (1+ offset)) 91)
                                (= (+ offset 2) length))))
                  (set-frontend-input-range offset)
                  (return))
                 ((and (not (session-frontend-application-p frontend))
                       (session-frontend-expanded-p frontend)
                       (= (aref buffer offset) 27)
                       (or (= (1+ offset) length)
                           (/= (aref buffer (1+ offset)) 91)))
                  (set-frontend-input-range offset)
                  (set-frontend-requested-session frontend (session-frontend-name frontend))
                  (incf offset)
                  (setf forward-start offset))
                 (t
                  (incf offset))))
      (set-frontend-input-range offset)
      (let ((remaining (subseq buffer offset)))
        (setf (session-frontend-input-buffer frontend) remaining
              (session-frontend-pending-input-since frontend)
              (when (and (= (length remaining) 1)
                         (= (aref remaining 0) 27))
                (or (session-frontend-pending-input-since frontend)
                    (get-internal-real-time)))))
      detach-p)))

(defun set-pending-frontend-escape (frontend)
  "Resolve a standalone pending escape after a short input delay."
  (let ((since (session-frontend-pending-input-since frontend)))
    (when (and since
               (>= (/ (- (get-internal-real-time) since)
                      internal-time-units-per-second)
                   +status-input-delay+))
      (setf (session-frontend-input-buffer frontend)
            (make-array 0 :element-type '(unsigned-byte 8))
            (session-frontend-pending-input-since frontend) nil)
      (if (and (not (session-frontend-application-p frontend))
               (session-frontend-expanded-p frontend))
          (set-frontend-requested-session frontend (session-frontend-name frontend))
          (set-frontend-chunk
           frontend
           (make-array 1
                       :element-type '(unsigned-byte 8)
                       :initial-element 27))))))

(defun set-requested-session (frontend list-sessions enter-session)
  "Enter FRONTEND's queued Session name, if any."
  (let ((name (session-frontend-requested-name frontend)))
    (when name
      (setf (session-frontend-requested-name frontend) nil)
      (handler-case
          (progn
            (funcall enter-session frontend name)
            (setf (session-frontend-expanded-p frontend) nil
                  (session-frontend-drawn-session-count frontend) 0
                  (session-frontend-control-buffer frontend)
                  (make-array 0 :element-type '(unsigned-byte 8))
                  (session-frontend-control-since frontend) nil)
            (set-frontend-mode-from-session frontend)
            (set-frontend-editor frontend))
        (error () nil))
      (set-status-bar-sessions frontend (funcall list-sessions))
      (set-session-manager-status-bar frontend))))

(defun get-frontend-events (frontend)
  "Poll FRONTEND's input and socket descriptors."
  (let ((descriptors nil)
        (input-fd (session-frontend-input-fd frontend))
        (socket-fd (session-frontend-socket-fd frontend)))
    (when input-fd
      (push (cons input-fd +pollin+) descriptors))
    (when socket-fd
      (push (cons socket-fd +pollin+) descriptors))
    (if descriptors
        (get-poll-events descriptors :timeout +frontend-poll-timeout+)
        (progn
          (sleep 0.01)
          nil))))

(defun set-socket-output (frontend)
  "Forward one available socket chunk and return its end state."
  (multiple-value-bind (bytes eof-p)
      (get-fd (session-frontend-socket-fd frontend) :wait-p nil)
    (multiple-value-bind (result activity-p)
        (if (and bytes (plusp (length bytes)))
            (set-mtm-controls
             frontend
             bytes
             (lambda (ordinary)
               (when (session-frontend-output-fd frontend)
                 (set-fd (session-frontend-output-fd frontend) ordinary)))
             (lambda (payload)
               (set-application-mode-control frontend payload)))
            (values nil nil))
      (declare (ignore result))
      (multiple-value-bind (ignored flushed-p)
          (set-pending-mtm-control-output
           frontend
           (lambda (ordinary)
             (when (session-frontend-output-fd frontend)
               (set-fd (session-frontend-output-fd frontend) ordinary))))
        (declare (ignore ignored))
        (values eof-p (or activity-p flushed-p))))))

(defun set-status-bar-loop (frontend list-sessions enter-session)
  "Run FRONTEND until its Session source or input ends."
  (loop
    (set-frontend-mode-from-session frontend)
    (when (session-frontend-attachment frontend)
      (multiple-value-bind (eof-p output-p)
          (set-passthrough-output
           (session-frontend-attachment frontend)
           (session-frontend-output-fd frontend))
        (when output-p
          (set-frontend-mode-from-session frontend)
          (set-session-manager-status-bar frontend)
          (set-frontend-editor-render frontend))
        (when eof-p
          (return))))
    (let ((events (get-frontend-events frontend)))
      (when (and (session-frontend-socket-fd frontend)
                 (event-readable-p
                  events
                  (session-frontend-socket-fd frontend)))
        (multiple-value-bind (eof-p output-p)
            (set-socket-output frontend)
          (when output-p
            (set-session-manager-status-bar frontend)
            (set-frontend-editor-render frontend))
          (when eof-p
            (return))))
      (when (and (session-frontend-input-fd frontend)
                 (event-readable-p
                  events
                  (session-frontend-input-fd frontend)))
        (multiple-value-bind (bytes eof-p)
            (get-fd (session-frontend-input-fd frontend) :wait-p nil)
          (if eof-p
              (return)
              (when (and bytes (plusp (length bytes)))
                (when (set-session-frontend-input frontend bytes)
                  (return))))))
      (set-pending-frontend-escape frontend)
      (set-requested-session frontend list-sessions enter-session)
      (when (>= (get-universal-time)
                (session-frontend-next-refresh frontend))
        (set-status-bar-sessions
         frontend
         (funcall list-sessions))
        (set-session-manager-status-bar frontend)))))

(defun set-status-bar-frontend (frontend list-sessions enter-session)
  "Run FRONTEND's status bar around its Session source."
  (set-raw-input
   (session-frontend-input-fd frontend)
   (lambda ()
     (unwind-protect
          (progn
            (set-frontend-mode-from-session frontend)
            (set-frontend-editor frontend)
            (set-status-bar-sessions
             frontend
             (funcall list-sessions))
            (set-status-bar-mouse frontend)
            (set-bracketed-paste frontend)
            (set-session-manager-status-bar frontend)
            (set-status-bar-loop frontend list-sessions enter-session))
       (del-frontend-editor-render frontend)
       (del-session-manager-status-bar frontend)
       (del-bracketed-paste frontend)
       (del-status-bar-mouse frontend)))))

(defun set-socket-passthrough (frontend)
  "Forward a manager socket without a status bar."
  (set-raw-input
   (session-frontend-input-fd frontend)
   (lambda ()
     (let ((input-fd (session-frontend-input-fd frontend))
           (output-fd (session-frontend-output-fd frontend)))
       (loop
         (let* ((socket-fd (session-frontend-socket-fd frontend))
                (events
                  (get-poll-events
                   (list (cons input-fd +pollin+)
                         (cons socket-fd +pollin+))
                   :timeout +frontend-poll-timeout+)))
           (when (event-readable-p events socket-fd)
             (multiple-value-bind (bytes eof-p)
                 (get-fd socket-fd :wait-p nil)
               (when (and bytes (plusp (length bytes)))
                 (set-mtm-controls
                  frontend
                  bytes
                  (lambda (ordinary)
                    (when output-fd
                      (set-fd output-fd ordinary)))
                  (lambda (payload)
                    (set-application-mode-control frontend payload))))
                (when eof-p
                 (return))))
           (when (and input-fd (event-readable-p events input-fd))
             (multiple-value-bind (bytes eof-p)
                 (get-fd input-fd :wait-p nil)
               (if eof-p
                   (return)
                   (when (and bytes (plusp (length bytes)))
                     (set-session-bytes frontend bytes)))))
           (set-pending-mtm-control-output
            frontend
            (lambda (ordinary)
              (when output-fd
                (set-fd output-fd ordinary))))))))))

(defun set-socket-frontend (socket-fd name list-sessions enter-session
                            &key (input-fd 0) (output-fd 1)
                              (application-p nil))
  "Run a local frontend for a manager socket."
  (let ((frontend
          (new-session-frontend
           :name name
           :socket-fd socket-fd
           :input-fd input-fd
           :output-fd output-fd
           :application-p application-p)))
    (if (and input-fd
             output-fd
             (tty-p input-fd)
             (tty-p output-fd))
        (set-status-bar-frontend frontend list-sessions enter-session)
        (set-socket-passthrough frontend))))

(defun set-terminal-output (terminal output-fd)
  "Write TERMINAL's retained display to OUTPUT-FD."
  (when output-fd
    (set-fd output-fd
            (get-utf8 (get-terminal-render terminal)))))

(defun set-passthrough-output (attachment output-fd)
  "Forward available bytes from ATTACHMENT and report its end state."
  (let ((output-p nil))
    (loop
      (multiple-value-bind (bytes eof-p)
          (get-attachment-output attachment :wait-p nil)
        (when (and bytes (plusp (length bytes)) output-fd)
          (set-fd output-fd bytes)
          (setf output-p t))
        (cond
          (eof-p (return (values t output-p)))
          ((or (null bytes) (zerop (length bytes)))
           (return (values nil output-p))))))))

(defun set-attachment-loop (frontend)
  "Run FRONTEND until its Attachment or input ends."
  (set-frontend-editor frontend)
  (let ((input-fd (session-frontend-input-fd frontend))
        (input-open-p (not (null (session-frontend-input-fd frontend)))))
    (loop
      (set-frontend-mode-from-session frontend)
      (multiple-value-bind (eof-p output-p)
          (set-passthrough-output
           (session-frontend-attachment frontend)
           (session-frontend-output-fd frontend))
        (when output-p
          (set-frontend-mode-from-session frontend)
          (set-frontend-editor-render frontend))
        (when eof-p
          (return)))
      (let ((events
              (if input-open-p
                  (get-poll-events (list (cons input-fd +pollin+))
                                   :timeout +frontend-poll-timeout+)
                  (progn
                    (sleep 0.01)
                    nil))))
        (when (and input-open-p
                   (event-readable-p events input-fd))
          (multiple-value-bind (bytes eof-p)
              (get-fd input-fd :wait-p nil)
            (if eof-p
                (return)
                (when (and bytes (plusp (length bytes)))
                  (multiple-value-bind (result ignored)
                      (set-mtm-controls
                       frontend
                       bytes
                       (lambda (ordinary)
                         (set-frontend-chunk frontend ordinary))
                       (lambda (payload)
                         (set-frontend-control-event frontend payload)))
                    (declare (ignore ignored))
                    (when result
                      (return)))))))
          (multiple-value-bind (result ignored)
              (set-pending-mtm-control-output
               frontend
               (lambda (ordinary)
                 (set-frontend-chunk frontend ordinary)))
            (declare (ignore ignored))
            (when result
              (return)))))))

(defun set-attachment-session (frontend name)
  "Replace FRONTEND's Attachment with NAME."
  (let ((old (session-frontend-attachment frontend)))
    (when (and old
               (string= name
                        (session-name
                         (attachment-session old))))
      (return-from set-attachment-session old))
    (let ((new (new-attachment name)))
      (del-frontend-editor frontend)
      (set-current-attachment new)
      (setf (session-frontend-attachment frontend) new
            (session-frontend-name frontend) name)
      (set-frontend-application-mode
       frontend
       (session-application-p (attachment-session new)))
      (ignore-errors (del-attachment old))
      (set-terminal-output
       (get-attachment-start-screen new)
       (session-frontend-output-fd frontend)))))

(defun set-passthrough-frontend (&key
                                  (attachment nil)
                                  (input-fd 0)
                                  (output-fd 1)
                                  (application-p nil)
                                  (socket-control-p nil))
  "Run a managed passthrough frontend for ATTACHMENT."
  (unless attachment
    (error "The passthrough frontend requires a managed Attachment."))
  (let* ((tty-frontend-p
           (and input-fd
                output-fd
                (tty-p input-fd)
                (tty-p output-fd)))
         (frontend
           (new-session-frontend
            :name (session-name (attachment-session attachment))
            :attachment attachment
            :input-fd input-fd
            :output-fd output-fd
            :application-p (or application-p
                               (session-application-p
                                (attachment-session attachment)))
            :socket-control-p socket-control-p)))
    (unwind-protect
         (progn
           (set-terminal-output
            (get-attachment-start-screen attachment)
            output-fd)
           (if tty-frontend-p
               (set-status-bar-frontend
                frontend
                #'get-session-list
                #'set-attachment-session)
               (set-raw-input
                input-fd
                (lambda ()
                  (set-attachment-loop frontend)))))
      (del-frontend-editor-render frontend)
      ;; Close the current Attachment after a status bar Session change.
      (ignore-errors
        (del-attachment (session-frontend-attachment frontend)))))
  nil)

(defun set-current-session (name &key (application-p nil) (input-fd 0) (output-fd 1))
  "Enter the named Session through the process-global current position."
  (let ((attachment (new-attachment name :application-p application-p)))
    (set-current-attachment attachment)
    (set-passthrough-frontend :attachment attachment
                              :input-fd input-fd
                              :output-fd output-fd)
    name))
