(in-package #:mtm.pty)

(defclass shell-session ()
  ((master
    :initarg :master
    :reader pty-master)
   (process-id
    :initarg :process-id
    :reader session-process-id)
   (open-p
    :initform t
    :accessor session-open-p)))

(defun get-current-shell ()
  "Return the shell selected by the environment."
  (or (uiop:getenv "SHELL") "/bin/sh"))

(defun new-shell-session (&key (shell (get-current-shell)) (width 80) (height 24))
  "Start the current shell inside a PTY."
  (multiple-value-bind (master process-id)
      (new-pty shell width height)
    (make-instance 'shell-session
                   :master master
                   :process-id process-id)))

(defun get-open-shell-session (session)
  "Signal an error when SESSION no longer accepts I/O."
  (unless (session-open-p session)
    (error "The shell session is closed."))
  session)

(defun get-shell-output-bytes (session &key (max-bytes 4096) (wait-p t))
  "Read PTY bytes and return bytes plus an end-of-file flag."
  (get-open-shell-session session)
  (get-fd (pty-master session) :max-bytes max-bytes :wait-p wait-p))

(defun valid-byte-vector-p (bytes)
  (and (vectorp bytes)
       (every (lambda (byte)
               (and (integerp byte) (<= 0 byte 255)))
              bytes)))

(defun set-shell-input (session bytes)
  "Write raw octets into SESSION."
  (get-open-shell-session session)
  (unless (valid-byte-vector-p bytes)
    (error "Input must be an octet vector."))
  (set-fd (pty-master session) bytes))

(defun set-shell-size (session rows columns)
  "Set SESSION's PTY row and column counts."
  (get-open-shell-session session)
  (set-terminal-size (pty-master session) rows columns))

(defun del-shell-session (session)
  "Close SESSION and reap its shell process."
  (when (session-open-p session)
    (setf (session-open-p session) nil)
    (unwind-protect
        (progn
          (ignore-errors (del-pty (pty-master session)))
          (ignore-errors (del-process (session-process-id session)))
          (ignore-errors
            (get-process-status (session-process-id session))))
      (setf (session-open-p session) nil)))
  t)
