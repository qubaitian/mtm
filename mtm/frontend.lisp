(defpackage #:mtm.frontend
  (:use #:cl)
  (:import-from #:mtm.platform
                #:+pollerr+
                #:+pollhup+
                #:+pollin+
                #:+pollnval+
                #:get-fd
                #:get-poll-events
                #:get-terminal-size
                #:set-fd
                #:set-raw-terminal)
  (:import-from #:bordeaux-threads
                #:join-thread
                #:make-thread)
  (:import-from #:mtm.session
                #:attachment-attached-p
                #:del-attachment-value
                #:get-attachment-output
                #:get-attachment-start-screen
                #:get-shell
                #:new-attachment
                #:new-session-value
                #:set-attachment-input
                #:set-attachment-terminal-size
                #:set-active-attachment)
  (:import-from #:mtm.terminal
                #:get-terminal-render)
  (:import-from #:mtm.utf8
                #:get-utf8)
  (:export
   #:get-session
   #:new-session
   #:set-socket-frontend
   #:set-passthrough-frontend))

(in-package #:mtm.frontend)

(defconstant +frontend-poll-timeout+ 100)
(defconstant +frontend-control-delay+ 0.05)
(defconstant +mtm-control-max-bytes+ 128)

(defparameter +mtm-control-prefix+
  (get-utf8 (format nil "~C]MTM;" #\Escape)))

;; Store the transport state for one Terminal frontend.
(defstruct (session-frontend
            (:constructor new-session-frontend
                (&key attachment
                      socket-fd
                      input-fd
                      output-fd
                      (rows 24)
                      (columns 80))))
  ;; Store the managed Attachment, when this frontend owns one.
  attachment
  ;; Store the manager socket descriptor, when this frontend uses one.
  socket-fd
  ;; Store the local input descriptor.
  input-fd
  ;; Store the local output descriptor.
  output-fd
  ;; Store the current Terminal frontend row count.
  (rows 24 :type integer)
  ;; Store the current Terminal frontend column count.
  (columns 80 :type integer)
  ;; Store bytes waiting for an MTM control terminator.
  (control-buffer
   (make-array 0 :element-type '(unsigned-byte 8)))
  ;; Store when the pending MTM control first arrived.
  (control-since nil)
  ;; Cache the last size sent to the Session.
  (last-rows 0)
  ;; Cache the last width sent to the Session.
  (last-columns 0))

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
         (result nil))
    (labels ((set-ordinary-output (start end)
               (when (< start end)
                 (setf result
                       (or result
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
                  (setf offset end)
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
      result)))

(defun set-pending-mtm-control-output (frontend ordinary-function)
  "Forward an incomplete MTM control after a short delay."
  (let ((since (session-frontend-control-since frontend)))
    (when (and since
               (>= (/ (- (get-internal-real-time) since)
                      internal-time-units-per-second)
                   +frontend-control-delay+))
      (let ((bytes (session-frontend-control-buffer frontend)))
        (setf (session-frontend-control-buffer frontend)
              (make-array 0 :element-type '(unsigned-byte 8))
              (session-frontend-control-since frontend) nil)
        (when (plusp (length bytes))
          (funcall ordinary-function bytes))))))

(defun set-mtm-control (fd payload)
  "Write one MTM control to FD."
  (when fd
    (set-fd fd
            (get-utf8
             (format nil "~C]MTM;~A~C"
                     #\Escape payload (code-char 7))))))

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

;; Apply one resize control received from a remote Terminal frontend.
(defun set-frontend-control-event (frontend payload)
  "Apply one control received through FRONTEND's input."
  (multiple-value-bind (rows columns)
      (get-resize-control payload)
    (when (and rows columns (session-frontend-attachment frontend))
      (set-attachment-terminal-size
       (session-frontend-attachment frontend) rows columns))))

;; Refresh FRONTEND's terminal dimensions from its output TTY.
(defun set-frontend-size (frontend)
  "Refresh FRONTEND's dimensions and report a real terminal size.
A frontend without a terminal keeps the size sent by its remote frontend."
  (let ((output-fd (session-frontend-output-fd frontend)))
    (when output-fd
      (handler-case
          (multiple-value-bind (rows columns)
              (get-terminal-size output-fd)
            (when (and (plusp rows) (plusp columns))
              (setf (session-frontend-rows frontend) rows
                    (session-frontend-columns frontend) columns)
              t))
        (error () nil)))))

;; Give the Session FRONTEND's complete terminal size.
(defun set-session-size (frontend)
  "Send FRONTEND's terminal size to its Session source."
  (let ((rows (max 1 (session-frontend-rows frontend)))
        (columns (max 1 (session-frontend-columns frontend))))
    (unless (and (= rows (session-frontend-last-rows frontend))
                 (= columns (session-frontend-last-columns frontend)))
      (cond
        ((session-frontend-attachment frontend)
         (set-attachment-terminal-size
          (session-frontend-attachment frontend) rows columns))
        ((session-frontend-socket-fd frontend)
         (set-mtm-control
          (session-frontend-socket-fd frontend)
          (format nil "resize;~D;~D" rows columns))))
      (setf (session-frontend-last-rows frontend) rows
            (session-frontend-last-columns frontend) columns)))
  frontend)

(defun set-session-bytes (frontend bytes)
  "Write BYTES to FRONTEND's Session source."
  (when (and bytes (plusp (length bytes)))
    (cond
      ((session-frontend-attachment frontend)
       (set-attachment-input (session-frontend-attachment frontend) bytes))
      ((session-frontend-socket-fd frontend)
       (set-fd (session-frontend-socket-fd frontend) bytes)))))

;; Send local input to the Session without rewriting it.
(defun set-frontend-input-bytes (frontend bytes)
  "Send input BYTES to FRONTEND's Session source.
An Attachment consumes MTM controls sent by a remote Terminal frontend."
  (if (session-frontend-attachment frontend)
      (set-mtm-controls
       frontend
       bytes
       (lambda (ordinary) (set-session-bytes frontend ordinary))
       (lambda (payload) (set-frontend-control-event frontend payload)))
      (set-session-bytes frontend bytes)))

(defun set-frontend-output-bytes (frontend bytes)
  "Write Session output BYTES to FRONTEND's terminal."
  (let ((output-fd (session-frontend-output-fd frontend)))
    (when (and output-fd bytes (plusp (length bytes)))
      (set-fd output-fd bytes))))

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

;; Forward Attachment output as soon as its Session produces it.
(defun set-attachment-output-loop (frontend)
  "Forward Attachment bytes until the Session or Attachment ends."
  (let ((attachment (session-frontend-attachment frontend)))
    (loop
      (multiple-value-bind (bytes eof-p)
          (get-attachment-output attachment :wait-p t)
        (set-frontend-output-bytes frontend bytes)
        (when eof-p
          (return))))))

(defun set-socket-output (frontend)
  "Forward one available socket chunk and report its end state."
  (multiple-value-bind (bytes eof-p)
      (get-fd (session-frontend-socket-fd frontend) :wait-p nil)
    (set-frontend-output-bytes frontend bytes)
    eof-p))

(defun set-frontend-input (frontend)
  "Read one available input chunk and report its end state."
  (multiple-value-bind (bytes eof-p)
      (get-fd (session-frontend-input-fd frontend) :wait-p nil)
    (if eof-p
        t
        (progn
          (set-frontend-input-bytes frontend bytes)
          nil))))

;; Run the transport loop for one Terminal frontend.
(defun set-frontend-loop (frontend)
  "Forward input until either side ends.
This loop also forwards output from a socket source.
An Attachment source forwards its output from a separate thread."
  (loop
    (when (set-frontend-size frontend)
      (set-session-size frontend))
    (let ((attachment (session-frontend-attachment frontend)))
      (when (and attachment (not (attachment-attached-p attachment)))
        (return)))
    (let ((events (get-frontend-events frontend)))
      (when (and (session-frontend-socket-fd frontend)
                 (event-readable-p
                  events
                  (session-frontend-socket-fd frontend))
                 (set-socket-output frontend))
        (return))
      (when (and (session-frontend-input-fd frontend)
                 (event-readable-p
                  events
                  (session-frontend-input-fd frontend))
                 (set-frontend-input frontend))
        (return))
      (set-pending-mtm-control-output
       frontend
       (lambda (ordinary) (set-session-bytes frontend ordinary))))))

;; Run a local frontend around a manager socket.
(defun set-socket-frontend (socket-fd &key (input-fd 0) (output-fd 1))
  "Forward a manager socket between the local terminal and a Session."
  (let ((frontend
          (new-session-frontend
           :socket-fd socket-fd
           :input-fd input-fd
           :output-fd output-fd)))
    (set-raw-input input-fd (lambda () (set-frontend-loop frontend)))))

(defun set-terminal-output (frontend terminal)
  "Write TERMINAL's retained display to FRONTEND."
  (set-frontend-output-bytes
   frontend
   (get-utf8 (get-terminal-render terminal))))

;; Run a frontend for one managed Attachment.
(defun set-passthrough-frontend (&key
                                  (attachment nil)
                                  (input-fd 0)
                                  (output-fd 1))
  "Forward one managed Attachment between a terminal and its Session."
  (unless attachment
    (error "The passthrough frontend requires a managed Attachment."))
  (let* ((frontend
           (new-session-frontend
            :attachment attachment
            :input-fd input-fd
            :output-fd output-fd))
         ;; Forward output from its own thread, which waits without a poll.
         (output-thread nil))
    (unwind-protect
         (progn
           (set-terminal-output
            frontend
            (get-attachment-start-screen attachment))
           (setf output-thread
                 (make-thread
                  (lambda ()
                    (ignore-errors (set-attachment-output-loop frontend)))
                  :name "mtm frontend output"))
           (set-raw-input input-fd
                          (lambda () (set-frontend-loop frontend))))
      ;; Close the active Attachment, which ends the output thread.
      (ignore-errors (del-attachment-value attachment))
      (when output-thread
        (ignore-errors (join-thread output-thread)))))
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
