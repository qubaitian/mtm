(in-package #:mtm.frontend)

(defconstant +frontend-poll-timeout+ 100)
(defconstant +status-refresh-interval+ 1)
(defconstant +status-input-delay+ 0.05)
(defconstant +mtm-control-max-bytes+ 128)

(defparameter +mtm-control-prefix+
  (get-utf8 (format nil "~C]MTM;" #\Escape)))

;; Store local input and transport state for one frontend.
(defstruct (session-frontend
            (:constructor new-session-frontend
                (&key name
                      attachment
                      socket-fd
                      input-fd
                      output-fd
                      sessions
                      services
                      (full-screen-p nil)
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
  ;; Store named shell Session rows.
  sessions
  ;; Store named Service rows.
  services
  ;; Track automatic full-screen terminal input.
  (full-screen-p nil)
  (socket-control-p nil)
  expanded-p
  (rows 24 :type integer)
  (columns 80 :type integer)
  (next-refresh 0)
  ;; Count all rows drawn below the manager row.
  (drawn-session-count 0)
  input-buffer
  (control-buffer
   (make-array 0 :element-type '(unsigned-byte 8)))
  (control-since nil)
  ;; Cache the last full-screen PTY size.
  (last-full-screen-rows 0)
  ;; Cache the last full-screen PTY width.
  (last-full-screen-columns 0)
  (pending-input-since nil)
  (mouse-captured-p nil)
  ;; Queue the next named shell Session.
  requested-name
  ;; Queue the next named Service log.
  requested-service
  ;; Mark the current view as read-only Service output.
  (service-log-p nil)
  ;; Remember the Session to restore from a Service log.
  return-session-name
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

;; Synchronize FRONTEND with automatic full-screen transport.
(defun set-frontend-full-screen-mode (frontend full-screen-p)
  "Set FRONTEND's automatic full-screen input state."
  (let ((full-screen-p (not (null full-screen-p))))
    (setf (session-frontend-last-full-screen-rows frontend) 0
          (session-frontend-last-full-screen-columns frontend) 0)
    (unless (eql full-screen-p (session-frontend-full-screen-p frontend))
      (setf (session-frontend-full-screen-p frontend) full-screen-p)
      (if full-screen-p
          (del-frontend-editor-render frontend)
          (set-frontend-editor frontend)))
    full-screen-p))

;; Synchronize FRONTEND with its Session's display state.
(defun set-frontend-mode-from-session (frontend)
  "Synchronize FRONTEND's mode with its attached Session."
  (let ((attachment
          (unless (session-frontend-service-log-p frontend)
            (session-frontend-attachment frontend))))
    (when attachment
      (let ((full-screen-p
              (session-full-screen-p (attachment-session attachment))))
        (unless (eql full-screen-p
                     (session-frontend-full-screen-p frontend))
          (set-frontend-full-screen-mode frontend full-screen-p)
          (when (session-frontend-socket-control-p frontend)
            (set-mtm-control
             (session-frontend-output-fd frontend)
             (format nil "mode;~A"
                     (if full-screen-p "full-screen" "editor"))))))))
  frontend)

(defun get-status-bar-row-count (frontend)
  "Return the number of rows reserved by FRONTEND's status bar."
  (1+ (if (session-frontend-expanded-p frontend)
          (session-frontend-drawn-session-count frontend)
          0)))

;; Reserve status rows when FRONTEND uses full-screen transport.
(defun set-full-screen-size (frontend)
  "Keep the PTY below FRONTEND's reserved status bar rows."
  (when (session-frontend-full-screen-p frontend)
    (let ((rows (max 1 (- (session-frontend-rows frontend)
                          (get-status-bar-row-count frontend))))
          (columns (session-frontend-columns frontend)))
      (unless (and (= rows (session-frontend-last-full-screen-rows frontend))
                   (= columns
                      (session-frontend-last-full-screen-columns frontend)))
        (cond
          ((session-frontend-attachment frontend)
           (set-attachment-terminal-size
            (session-frontend-attachment frontend) rows columns))
          ((and (session-frontend-socket-fd frontend)
                (not (session-frontend-socket-control-p frontend)))
           (set-mtm-control
            (session-frontend-socket-fd frontend)
            (format nil "resize;~D;~D" rows columns))))
        (setf (session-frontend-last-full-screen-rows frontend) rows
              (session-frontend-last-full-screen-columns frontend) columns))))
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

;; Return display entries for named Sessions and Services.
(defun get-status-bar-entries (sessions services)
  "Return display entries for named Sessions and Services."
  (append
   (mapcar (lambda (entry)
             (list :session (car entry) (cdr entry)))
           sessions)
   (mapcar (lambda (entry)
             (list :service (car entry) (cdr entry)))
           services)))

;; Render the Session manager status bar.
(defun get-session-manager-status-bar
    (sessions active-name rows columns expanded-p
     &optional (previous-session-count 0) (services nil))
  "Return ANSI output for the Session manager status bar."
  (let* ((entries (get-status-bar-entries sessions services))
         (visible-entry-count
           (min (max 0 (1- rows)) (length entries))))
    (with-output-to-string (output)
      (format output "~C[s" #\Escape)
      ;; Clear rows left by a shorter list.
      (when expanded-p
        (loop for index from (1+ visible-entry-count)
                to (min previous-session-count (max 0 (1- rows)))
              do (format output "~C[~D;1H~C[2K"
                         #\Escape (- rows index) #\Escape)))
      (write-string
       (get-green-status-row
        rows
        columns
        (if services
            (format nil " session-manager: ~D sessions, ~D services "
                    (length sessions)
                    (length services))
            (format nil " session-manager: ~D sessions " (length sessions))))
       output)
      (when expanded-p
        (loop for entry in entries
              for index from 1
              while (< index rows)
              for row = (- rows index)
              do (write-string
                  (get-green-status-row
                   row
                   columns
                   (if (eq (first entry) :service)
                       (format nil " service ~A [~A] "
                               (second entry)
                               (string-downcase
                                (princ-to-string (third entry))))
                       (format nil " ~A [~A] "
                               (second entry)
                               (string-downcase
                                (princ-to-string (third entry)))))
                   :bright-p (and active-name
                                  (string= active-name (second entry))))
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

;; Draw the status bar and update the full-screen PTY size.
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
         (session-frontend-drawn-session-count frontend)
         (session-frontend-services frontend))))
      (setf (session-frontend-drawn-session-count frontend)
            (if (session-frontend-expanded-p frontend)
                (min (max 0 (1- (session-frontend-rows frontend)))
                     (+ (length (session-frontend-sessions frontend))
                        (length (session-frontend-services frontend))))
                0))
      (set-full-screen-size frontend))))

;; Store Session and Service rows and schedule their next refresh.
(defun set-status-bar-sessions (frontend list-entries)
  "Store Session and Service rows and schedule their next refresh."
  ;; Read current Session and Service rows.
  (multiple-value-bind (sessions services)
      (funcall list-entries)
    (setf (session-frontend-sessions frontend) sessions
          (session-frontend-services frontend) services
          (session-frontend-next-refresh frontend)
          (+ (get-universal-time) +status-refresh-interval+)))
  frontend)

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
  ;; Enable compatible terminal mouse reports for FRONTEND.
  "Enable compatible mouse reports for the local frontend."
  (set-frontend-control
   frontend
   (format nil "~C[?1000h~C[?1002h~C[?1006h"
           #\Escape #\Escape #\Escape)))

(defun del-status-bar-mouse (frontend)
  ;; Disable compatible terminal mouse reports after FRONTEND stops.
  "Disable compatible mouse reports after the frontend stops."
  (set-frontend-control
   frontend
   (format nil "~C[?1006l~C[?1002l~C[?1000l"
           #\Escape #\Escape #\Escape)))

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

;; Return one expanded status entry under ROW, or NIL.
(defun get-status-bar-entry-at-row (frontend row)
  "Return one expanded status entry under ROW, or NIL."
  (let* ((rows (session-frontend-rows frontend))
         (entries
           (get-status-bar-entries
            (session-frontend-sessions frontend)
            (session-frontend-services frontend)))
         (index (- rows row 1)))
    (when (and (session-frontend-expanded-p frontend)
               (< 0 row rows)
               (<= 0 index)
               (< index (length entries)))
      (nth index entries))))

;; Queue NAME as FRONTEND's next Session.
(defun set-frontend-requested-session (frontend name)
  "Queue NAME as FRONTEND's next Session."
  (when name
    (setf (session-frontend-requested-name frontend) name)))

;; Queue NAME as FRONTEND's next Service log.
(defun set-frontend-requested-service (frontend name)
  "Queue NAME as FRONTEND's next Service log."
  (when name
    (setf (session-frontend-requested-service frontend) name)))

;; Return the Session name used when leaving a Service log.
(defun get-frontend-return-session-name (frontend)
  "Return the Session name used when leaving a Service log."
  (or (session-frontend-return-session-name frontend)
      (unless (session-frontend-service-log-p frontend)
        (session-frontend-name frontend))))

;; Handle one mouse report for the status bar.
(defun set-status-bar-mouse-event
    (frontend button column row press-p)
  "Handle one mouse report and return true when it belongs to the bar."
  (declare (ignore column))
  (let* ((manager-row (session-frontend-rows frontend))
         ;; Read the status entry under the pointer.
         (entry (get-status-bar-entry-at-row frontend row)))
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
           (set-frontend-requested-session
            frontend
            (get-frontend-return-session-name frontend))
           (progn
             (setf (session-frontend-expanded-p frontend) t
                   (session-frontend-next-refresh frontend) 0)
             (set-session-manager-status-bar frontend)
             t)))
      ((and (session-frontend-expanded-p frontend)
            (eq (first entry) :session))
       (setf (session-frontend-mouse-captured-p frontend) t)
       (set-frontend-requested-session frontend (second entry))
       t)
      ((and (session-frontend-expanded-p frontend)
            (eq (first entry) :service))
       (setf (session-frontend-mouse-captured-p frontend) t)
       (set-frontend-requested-service frontend (second entry))
       t)
      (t nil))))

(defun get-decimal-mouse-field (bytes start end)
  "Parse one decimal field from BYTES."
  (when (< start end)
    (ignore-errors
      (parse-integer (map 'string #'code-char (subseq bytes start end))))))

;; Collect the numeric fields needed by SGR mouse parsing.
(defun get-sgr-mouse-fields (bytes start end)
  "Parse semicolon-separated SGR mouse fields."
  (let (;; Collect every numeric field before selecting coordinates.
        (fields nil)
        (field-start start))
    (loop for index from start below end
          when (= (aref bytes index) (char-code #\;))
            do (push (get-decimal-mouse-field bytes field-start index)
                     fields)
               (setf field-start (1+ index)))
    (push (get-decimal-mouse-field bytes field-start end) fields)
    (nreverse fields)))

(defun get-sgr-mouse-report (bytes start)
  ;; Parse one SGR report without changing its raw byte boundaries.
  "Parse an SGR mouse report beginning at START."
  (unless (and (<= (+ start 3) (length bytes))
               (= (aref bytes start) 27)
               (= (aref bytes (1+ start)) 91)
               (= (aref bytes (+ start 2)) 60))
    (return-from get-sgr-mouse-report (values :not-a-report start)))
  ;; Scan bytes until a complete SGR terminator appears.
  (loop for end from (+ start 3) below (length bytes)
        for byte = (aref bytes end)
        when (or (= byte (char-code #\M))
                 (= byte (char-code #\m)))
          do (let ((fields (get-sgr-mouse-fields bytes (+ start 3) end)))
               (if (and (>= (length fields) 3)
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

(defun get-legacy-mouse-report (bytes start)
  ;; Parse one six-byte legacy mouse report beginning at START.
  "Parse a legacy X10 mouse report beginning at START."
  (unless (and (<= (+ start 3) (length bytes))
               (= (aref bytes start) 27)
               (= (aref bytes (1+ start)) 91)
               (= (aref bytes (+ start 2)) 77))
    (return-from get-legacy-mouse-report (values :not-a-report start)))
  (let (;; Keep the complete report boundary for forwarding.
        (report-end (+ start 6)))
    (if (> report-end (length bytes))
        (values :incomplete start)
        (let* (;; Read the encoded button and modifier flags.
               (encoded-button (aref bytes (+ start 3)))
               ;; Read the encoded one-based terminal column.
               (encoded-column (aref bytes (+ start 4)))
               ;; Read the encoded one-based terminal row.
               (encoded-row (aref bytes (+ start 5)))
               ;; Remove the protocol offset from the button byte.
               (button (- encoded-button 32))
               ;; Remove the protocol offset from the column byte.
               (column (- encoded-column 32))
               ;; Remove the protocol offset from the row byte.
               (row (- encoded-row 32)))
          (if (and (>= encoded-button 32)
                   (plusp column)
                   (plusp row))
              (values :event report-end
                      ;; Normalize the legacy release marker for local handlers.
                      (if (= (logand button 3) 3)
                          (logand button -4)
                          button)
                      column
                      row
                      (/= (logand button 3) 3))
              (values :invalid report-end))))))

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

;; Read the retained terminal cursor row for FRONTEND.
(defun get-frontend-terminal-cursor-row (frontend)
  "Return the terminal cursor row for FRONTEND."
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

;; Draw the Editor area unless full-screen transport is active.
(defun set-frontend-editor-render (frontend)
  "Draw FRONTEND's Editor area at column 0."
  (let ((editor (session-frontend-editor frontend))
        (output-fd (session-frontend-output-fd frontend)))
    (when (and editor
               output-fd
               (not (session-frontend-full-screen-p frontend)))
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

;; Send input BYTES through full-screen transport or the Editor area.
;; Send input BYTES through the current frontend mode.
(defun set-frontend-chunk (frontend bytes)
  "Send BYTES to full-screen transport or the Editor area."
  (cond
    ((or (null bytes) (zerop (length bytes))) nil)
    ((session-frontend-service-log-p frontend) nil)
    ((session-frontend-full-screen-p frontend)
     (set-session-bytes frontend bytes)
     nil)
    ((session-frontend-editor frontend)
     (set-editor-chunk frontend bytes))
    (t
     (set-session-bytes frontend bytes)
     nil)))

;; Parse a resize control payload.
(defun get-resize-control (payload)
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

;; Apply one resize control received from a manager.
(defun set-frontend-control-event (frontend payload)
  "Apply one manager control received from a local frontend."
  (multiple-value-bind (rows columns)
      (get-resize-control payload)
    (when (and rows columns (session-frontend-attachment frontend))
      (set-attachment-terminal-size
       (session-frontend-attachment frontend) rows columns))))

;; Apply a Session transport control from the manager.
(defun set-session-mode-control (frontend payload)
  "Apply one Session mode control received from the manager."
  (cond
    ((string= payload "mode;full-screen")
     (set-frontend-full-screen-mode frontend t))
    ((string= payload "mode;editor")
     (set-frontend-full-screen-mode frontend nil))
    (t nil)))

;; Route local input, including complete mouse reports, to its destination.
(defun set-session-frontend-input (frontend bytes &key (status-bar-p t))
  "Route input to local controls, the Editor area, or the Session."
  (let* (;; Combine bytes split across frontend reads.
         (buffer (concatenate '(vector (unsigned-byte 8))
                              (session-frontend-input-buffer frontend)
                              bytes))
         ;; Cache the combined input length for the scan.
         (length (length buffer))
         ;; Track the next byte that still needs classification.
         (offset 0)
         ;; Track the start of bytes sent to ordinary input.
         (forward-start 0)
         ;; Remember whether ordinary input requested detachment.
         (detach-p nil))
    (labels (;; Forward ordinary bytes before the next special report.
             (set-frontend-input-range (end)
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
                       (or (= (aref buffer (+ offset 2)) 60)
                           (= (aref buffer (+ offset 2)) 77)))
                  (set-frontend-input-range offset)
                  ;; Decode one complete report and route its semantic event.
                  (multiple-value-bind (kind end button column row press-p)
                      (if (= (aref buffer (+ offset 2)) 60)
                          (get-sgr-mouse-report buffer offset)
                          (get-legacy-mouse-report buffer offset))
                    (case kind
                      (:incomplete (return))
                      (:event
                       (if (session-frontend-full-screen-p frontend)
                           (when (set-frontend-chunk
                                  frontend
                                  (subseq buffer offset end))
                             (setf detach-p t))
                           (unless (or (and status-bar-p
                                            (set-status-bar-mouse-event
                                             frontend button column row press-p))
                                       (set-editor-mouse-event
                                        frontend button column row press-p))
                             (when (set-frontend-chunk
                                    frontend
                                    (subseq buffer offset end))
                               (setf detach-p t))))
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
                 ((and (not (session-frontend-full-screen-p frontend))
                       (session-frontend-expanded-p frontend)
                       (= (aref buffer offset) 27)
                       (or (= (1+ offset) length)
                           (/= (aref buffer (1+ offset)) 91)))
                  (set-frontend-input-range offset)
                  (set-frontend-requested-session
                   frontend
                   (get-frontend-return-session-name frontend))
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

;; Resolve one delayed Escape according to frontend state.
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
      (if (and (not (session-frontend-full-screen-p frontend))
               (or (session-frontend-expanded-p frontend)
                   (and (session-frontend-service-log-p frontend)
                        (get-frontend-return-session-name frontend))))
          (set-frontend-requested-session
           frontend
           (get-frontend-return-session-name frontend))
          (set-frontend-chunk
           frontend
           (make-array 1
                       :element-type '(unsigned-byte 8)
                       :initial-element 27))))))

;; Enter FRONTEND's queued Session name, if any.
(defun set-requested-session (frontend list-entries enter-session)
  "Enter FRONTEND's queued Session name, if any."
  (let ((name (session-frontend-requested-name frontend)))
    (when name
      (setf (session-frontend-requested-name frontend) nil)
      (handler-case
          (progn
            (funcall enter-session frontend name)
            (setf (session-frontend-expanded-p frontend) nil
                  (session-frontend-drawn-session-count frontend) 0
                  (session-frontend-requested-service frontend) nil
                  (session-frontend-service-log-p frontend) nil
                  (session-frontend-return-session-name frontend) nil
                  (session-frontend-control-buffer frontend)
                  (make-array 0 :element-type '(unsigned-byte 8))
                  (session-frontend-control-since frontend) nil)
            (set-frontend-mode-from-session frontend)
            (set-frontend-editor frontend))
        (error () nil))
      (set-status-bar-sessions
       frontend
       list-entries)
      (set-session-manager-status-bar frontend))))

;; Enter FRONTEND's queued Service log, if any.
(defun set-requested-service (frontend list-entries enter-service)
  "Enter FRONTEND's queued Service log, if any."
  (let ((name (session-frontend-requested-service frontend)))
    (when name
      (setf (session-frontend-requested-service frontend) nil)
      (handler-case
          (progn
            (funcall enter-service frontend name)
            (setf (session-frontend-expanded-p frontend) nil
                  (session-frontend-drawn-session-count frontend) 0
                  (session-frontend-requested-name frontend) nil
                  (session-frontend-control-buffer frontend)
                  (make-array 0 :element-type '(unsigned-byte 8))
                  (session-frontend-control-since frontend) nil)
            (set-frontend-full-screen-mode frontend nil)
            (del-frontend-editor frontend))
        (error () nil))
      (set-status-bar-sessions
       frontend
       list-entries)
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

;; Forward manager output and consume transport controls.
(defun set-socket-output (frontend)
  "Forward one available socket chunk and return its end state."
  (multiple-value-bind (bytes eof-p)
      (get-fd (session-frontend-socket-fd frontend) :wait-p nil)
    (when (session-frontend-service-log-p frontend)
      (when (and bytes
                 (plusp (length bytes))
                 (session-frontend-output-fd frontend))
        (set-fd (session-frontend-output-fd frontend) bytes))
      (return-from set-socket-output (values eof-p (plusp (length bytes)))))
    (multiple-value-bind (result activity-p)
        (if (and bytes (plusp (length bytes)))
            (set-mtm-controls
             frontend
             bytes
             (lambda (ordinary)
               (when (session-frontend-output-fd frontend)
                 (set-fd (session-frontend-output-fd frontend) ordinary)))
             (lambda (payload)
               (set-session-mode-control frontend payload)))
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

;; Run the interactive status bar frontend loop.
(defun set-status-bar-loop
    (frontend list-entries enter-session enter-service)
  "Run FRONTEND until its Session source or input ends."
  (loop
    (unless (session-frontend-service-log-p frontend)
      (set-frontend-mode-from-session frontend))
    (when (and (session-frontend-attachment frontend)
               (not (session-frontend-service-log-p frontend)))
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
      (set-requested-service
       frontend
       list-entries
       enter-service)
      (set-requested-session frontend list-entries enter-session)
      (when (>= (get-universal-time)
                (session-frontend-next-refresh frontend))
        (set-status-bar-sessions frontend list-entries)
        (set-session-manager-status-bar frontend)))))

;; Run FRONTEND's status bar around its Session source.
(defun set-status-bar-frontend
    (frontend list-entries enter-session enter-service)
  "Run FRONTEND's status bar around its Session source."
  (set-raw-input
   (session-frontend-input-fd frontend)
   (lambda ()
     (unwind-protect
          (progn
            (set-frontend-mode-from-session frontend)
            (set-frontend-editor frontend)
            (set-status-bar-sessions frontend list-entries)
            (set-status-bar-mouse frontend)
            (set-bracketed-paste frontend)
            (set-session-manager-status-bar frontend)
            (set-status-bar-loop
             frontend list-entries enter-session enter-service))
       (del-frontend-editor-render frontend)
       (del-session-manager-status-bar frontend)
       (del-bracketed-paste frontend)
       (del-status-bar-mouse frontend)))))

;; Forward a non-TTY manager socket without a status bar.
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
               (if (session-frontend-service-log-p frontend)
                   (when (and bytes (plusp (length bytes)) output-fd)
                     (set-fd output-fd bytes))
                   (when (and bytes (plusp (length bytes)))
                     (set-mtm-controls
                      frontend
                      bytes
                      (lambda (ordinary)
                        (when output-fd
                          (set-fd output-fd ordinary)))
                      (lambda (payload)
                        (set-session-mode-control frontend payload)))))
                (when eof-p
                 (return))))
           (when (and input-fd
                      (event-readable-p events input-fd))
             (multiple-value-bind (bytes eof-p)
                 (get-fd input-fd :wait-p nil)
               (if eof-p
                   (return)
                   (when (and (not (session-frontend-service-log-p frontend))
                              bytes
                              (plusp (length bytes)))
                     (set-session-bytes frontend bytes)))))
           (set-pending-mtm-control-output
            frontend
            (lambda (ordinary)
              (when output-fd
                (set-fd output-fd ordinary))))))))))

;; Run a local frontend around a manager socket.
(defun set-socket-frontend
    (socket-fd name list-entries enter-session enter-service
     &key (input-fd 0) (output-fd 1) (full-screen-p nil))
  "Run a local frontend for a manager socket."
  (let ((frontend
          (new-session-frontend
           :name name
           :socket-fd socket-fd
           :input-fd input-fd
           :output-fd output-fd
           :full-screen-p full-screen-p)))
    (if (and input-fd
             output-fd
             (tty-p input-fd)
             (tty-p output-fd))
        (set-status-bar-frontend
         frontend list-entries enter-session enter-service)
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

;; Run attachment input through the shared mouse parser.
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
                         (set-session-frontend-input
                          frontend ordinary :status-bar-p nil))
                       (lambda (payload)
                         (set-frontend-control-event frontend payload)))
                    (declare (ignore ignored))
                    (when result
                      (return)))))))
          (multiple-value-bind (result ignored)
              (set-pending-mtm-control-output
               frontend
               (lambda (ordinary)
                 (set-session-frontend-input
                  frontend ordinary :status-bar-p nil)))
            (declare (ignore ignored))
            (when result
              (return)))))))

;; Enter NAME through an existing local frontend.
(defun set-attachment-session (frontend name)
  "Replace FRONTEND's Attachment with NAME."
  (let ((old (session-frontend-attachment frontend)))
    (when (and old
               (string= name
                        (session-name
                         (attachment-session old))))
      (set-terminal-output
       (get-attachment-start-screen old)
       (session-frontend-output-fd frontend))
      (setf (session-frontend-name frontend) name)
      (return-from set-attachment-session old))
    (let ((new (new-attachment name)))
      (del-frontend-editor frontend)
      (set-active-attachment new)
      (setf (session-frontend-attachment frontend) new
            (session-frontend-name frontend) name)
      (set-frontend-full-screen-mode
       frontend
       (session-full-screen-p (attachment-session new)))
      (ignore-errors (del-attachment old))
      (set-terminal-output
       (get-attachment-start-screen new)
       (session-frontend-output-fd frontend)))))

;; Show recent output for NAME while keeping the current Session attached.
(defun set-attachment-service (frontend name)
  "Show recent output for NAME while keeping the current Session attached."
  (let ((attachment (session-frontend-attachment frontend)))
    (setf (session-frontend-service-log-p frontend) t
          (session-frontend-return-session-name frontend)
          (and attachment
               (session-name (attachment-session attachment)))
          (session-frontend-name frontend) name)
    (del-frontend-editor frontend)
    (set-frontend-full-screen-mode frontend nil)
    (set-frontend-control
     frontend
     (format nil "~C[2J~C[H" #\Escape #\Escape))
    (let ((output (get-service-output name)))
      (when (and output
                 (plusp (length output))
                 (session-frontend-output-fd frontend))
        (set-fd (session-frontend-output-fd frontend) output)))))

;; Run a frontend for one managed Attachment.
(defun set-passthrough-frontend (&key
                                  (attachment nil)
                                  (input-fd 0)
                                  (output-fd 1)
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
            :full-screen-p (session-full-screen-p
                            (attachment-session attachment))
            :socket-control-p socket-control-p)))
    (unwind-protect
         (progn
           (set-terminal-output
            (get-attachment-start-screen attachment)
            output-fd)
           (if tty-frontend-p
               (set-status-bar-frontend
                frontend
                (lambda ()
                  (values (get-session-list) (get-service-list)))
                #'set-attachment-session
                #'set-attachment-service)
               (set-raw-input
                input-fd
                (lambda ()
                  (set-attachment-loop frontend)))))
      (del-frontend-editor-render frontend)
      ;; Close the active Attachment after a status bar Session change.
      (ignore-errors
        (del-attachment (session-frontend-attachment frontend)))))
  nil)

;; Enter an existing Session through the Terminal frontend.
(defun get-session (name &key (input-fd 0) (output-fd 1))
  "Enter the named Session through the Terminal frontend."
  (let ((attachment (new-attachment name)))
    (set-active-attachment attachment)
    (set-passthrough-frontend :attachment attachment
                              :input-fd input-fd
                              :output-fd output-fd)
    name))

;; Ensure NAME exists, then enter its Session.
(defun new-session (name &key
                           (shell (get-shell))
                           (width 80)
                           (height 24)
                           (input-fd 0)
                           (output-fd 1))
  "Ensure NAME exists, then enter its Session."
  (new-session-value name :shell shell :width width :height height)
  (get-session name :input-fd input-fd :output-fd output-fd))
