(defpackage #:mtm.platform
  (:use #:cl)
  (:export
   #:+pollerr+
   #:+pollhup+
   #:+pollin+
   #:+pollnval+
   #:set-raw-terminal
   #:del-pty
   #:get-poll-events
   #:get-terminal-size
   #:set-terminal-size
   #:set-socket-sigpipe
   #:get-fd
   #:set-close-on-exec
   #:new-detached-process
   #:new-pty
   #:new-pty-process
   #:del-process
   #:del-process-handle
   #:set-process-signal
   #:tty-p
   #:get-process-alive-p
   #:get-process-id
   #:get-process-pty
   #:get-process-status
   #:set-fd))

(in-package #:mtm.platform)

#-(and sbcl darwin)
(error "MTM currently requires SBCL on macOS.")

;; Use SBCL's existing OS interfaces.  
;; MTM defines no foreign routines.  
(defconstant +pollin+ sb-unix:pollin)
(defconstant +pollout+ sb-unix:pollout)
(defconstant +pollerr+ sb-unix:pollerr)
(defconstant +pollhup+ sb-unix:pollhup)
(defconstant +pollnval+ sb-unix:pollnval)
(defconstant +sigkill+ sb-posix:sigkill)
;; Darwin ioctl request numbers.  
;; winsize is four native unsigned shorts.  
(defconstant +tiocgwinsz+ #x40087468)
(defconstant +tiocswinsz+ #x80087467)

(defun get-descriptor (terminal)
  (etypecase terminal
    (integer terminal)
    (file-stream (sb-sys:fd-stream-fd terminal))))

(defun tty-p (terminal)
  ;; The C isatty returns 0 for other descriptors, and 0 is true in Lisp.
  (eql 1 (sb-unix:unix-isatty (get-descriptor terminal))))

(defun get-terminal-size (terminal)
  "Return the terminal row and column counts."
  (let ((size (make-array 4 :element-type '(unsigned-byte 16) :initial-element 0)))
    (sb-sys:with-pinned-objects (size)
      (sb-posix:ioctl (get-descriptor terminal) +tiocgwinsz+
                     (sb-alien:sap-alien (sb-sys:vector-sap size) (* t))))
    (values (aref size 0) (aref size 1))))

(defun set-terminal-size (terminal rows columns)
  "Set terminal dimensions using SB-POSIX's existing ioctl binding."
  (check-type rows (integer 1 65535))
  (check-type columns (integer 1 65535))
  (let ((size (make-array 4 :element-type '(unsigned-byte 16)
                          :initial-contents (list rows columns 0 0))))
    (sb-sys:with-pinned-objects (size)
      (sb-posix:ioctl (get-descriptor terminal) +tiocswinsz+
                     (sb-alien:sap-alien (sb-sys:vector-sap size) (* t)))))
  (values rows columns))

(defun set-nonblocking (terminal)
  (let ((fd (get-descriptor terminal)))
    (sb-posix:fcntl fd sb-posix:f-setfl
                   (logior (sb-posix:fcntl fd sb-posix:f-getfl)
                           sb-posix:o-nonblock))))

(defun set-close-on-exec (terminal)
  "Prevent the descriptor from leaking into an exec'ed child."
  (let ((fd (get-descriptor terminal)))
    (sb-posix:fcntl fd sb-posix:f-setfd
                   (logior (sb-posix:fcntl fd sb-posix:f-getfd)
                           1))))

(defun set-detached-process ()
  "Leave the controlling terminal and drop standard streams.  "
  (sb-posix:setsid)
  (let ((null-fd (sb-posix:open "/dev/null" sb-posix:o-rdwr)))
    (sb-posix:dup2 null-fd 0)
    (sb-posix:dup2 null-fd 1)
    (sb-posix:dup2 null-fd 2)
    (unless (member null-fd '(0 1 2))
      (sb-posix:close null-fd)))
  t)

(defun new-detached-process (function)
  "Fork FUNCTION into a detached child process.  "
  (let ((pid (sb-posix:fork)))
    (cond
      ((minusp pid)
       (error "The detached process cannot be created."))
      ((plusp pid)
       pid)
      (t
       (set-detached-process)
       (handler-case
           (funcall function)
         (error ()
           (sb-ext:exit :code 1 :abort t)))
       (sb-ext:exit :code 0)))))

(defun get-process-pty (process)
  "Return PROCESS's SBCL-owned PTY stream."
  (sb-ext:process-pty process))

(defun get-process-id (process)
  "Return PROCESS's operating system identifier."
  (sb-ext:process-pid process))

(defun get-process-alive-p (process)
  (sb-ext:process-alive-p process))

(defun del-process-handle (process)
  "Release SBCL's process object after the child is reaped."
  (sb-ext:process-close process)
  t)

(defun get-process-status (process &key (no-hang-p nil))
  "Return waitpid-style exit status, or NIL while PROCESS is running."
  ;; SBCL owns child reaping.  
  ;; Do not race its SIGCHLD handler with waitpid.  
  (unless no-hang-p (sb-ext:process-wait process))
  (case (sb-ext:process-status process)
    (:exited (ash (sb-ext:process-exit-code process) 8))
    (:signaled (sb-ext:process-exit-code process))
    (otherwise nil)))

(defun set-process-signal (process signal)
  "Send SIGNAL to a live SBCL process."
  (when (get-process-alive-p process)
    (sb-ext:process-kill process signal))
  t)

(defun del-process (process)
  (set-process-signal process sb-posix:sighup))

(defun set-pty-process (process width height)
  (handler-case
      (let ((master (get-process-pty process)))
        (set-nonblocking master)
        (set-close-on-exec master)
        ;; Wait until the child stty applies the size.  
        ;; Returning earlier lets a later resize race that stty command.
        (loop repeat 50
              do (multiple-value-bind (rows columns)
                     (ignore-errors (get-terminal-size master))
                   (when (and rows (= rows height) (= columns width))
                     (return)))
                 (sleep 0.01)
              finally (set-terminal-size master height width))
        (values master process))
    (error (condition)
      (ignore-errors (set-process-signal process +sigkill+))
      (ignore-errors (get-process-status process))
      (ignore-errors (del-process-handle process))
      (error condition))))

(defun new-pty-process (program width height &key working-directory)
  "Return the SBCL PTY stream and process for PROGRAM."
  (check-type program cons)
  (unless (every #'stringp program)
    (error "PTY programs must contain only strings."))
  (check-type width (integer 1 65535))
  (check-type height (integer 1 65535))
  (when working-directory (check-type working-directory string))
  ;; Initialize before exec, so even a short-lived command sees the right size.  
  ;; SBCL disables PTY echo.  
  ;; stty sane restores an ordinary interactive terminal.  
  ;; All user arguments are positional parameters, never shell source text.  
  (set-pty-process
   (sb-ext:run-program
    "/bin/sh"
    (append (list "-c"
                  "/bin/stty sane rows \"$1\" cols \"$2\" || exit; shift 2; exec \"$@\""
                  "mtm-pty" (write-to-string height) (write-to-string width))
            program)
    :pty t :wait nil :directory working-directory)
   width height))

(defun new-pty (shell width height)
  "Start SHELL inside a non-blocking PTY."
  (check-type shell string)
  (new-pty-process (list shell "-i") width height))

(defun get-poll-events (descriptors &key (timeout -1))
  "Return ready descriptors. EOF is reported as readable, as with select."
  (check-type timeout integer)
  (when descriptors
    (let ((deadline (unless (minusp timeout)
                      (+ (get-internal-real-time)
                         (* timeout (/ internal-time-units-per-second 1000))))))
      (labels ((ready (entry seconds)
                 (let ((events 0))
                   (dolist (direction '(:input :output))
                     (let ((flag (if (eq direction :input) +pollin+ +pollout+)))
                       (when (and (logtest flag (cdr entry))
                                  (sb-sys:wait-until-fd-usable
                                   (get-descriptor (car entry)) direction seconds nil))
                         (setf events (logior events flag)))))
                   (when (plusp events) (cons (car entry) events)))))
        (loop for result = (remove nil (mapcar (lambda (entry) (ready entry 0))
                                               descriptors))
              when result return result
              when (and deadline (>= (get-internal-real-time) deadline)) return nil
              ;; A bounded wait also lets a concurrently closed stream wake its reader.  
              do (if (= (length descriptors) 1)
                     (let ((result (ready (first descriptors) 0.01)))
                       (when result (return (list result))))
                     (sleep 0.01)))))))

(defun get-fd (terminal &key (max-bytes 4096) (wait-p nil))
  "Read raw octets, without the PTY stream's character decoding or buffering."
  (check-type max-bytes (integer 1))
  (let ((bytes (make-array max-bytes :element-type '(unsigned-byte 8))))
    (loop
      ;; Never pin memory across a blocking wait.  
      (multiple-value-bind (count errno)
          (sb-sys:with-pinned-objects (bytes)
            (sb-unix:unix-read (get-descriptor terminal) (sb-sys:vector-sap bytes) max-bytes))
        (cond
          (count (return (values (subseq bytes 0 count) (zerop count))))
          ((= errno sb-posix:eintr))
          ((= errno sb-posix:eagain)
           (if wait-p
               (get-poll-events (list (cons terminal +pollin+)))
               (return (values #() nil))))
          ((= errno sb-posix:eio) (return (values #() t)))
          (t (error "read failed: ~A" (sb-int:strerror errno))))))))

(defun set-fd (terminal bytes)
  "Write all raw octets, handling partial writes and backpressure."
  (let* ((buffer (coerce bytes '(simple-array (unsigned-byte 8) (*))))
         (length (length buffer)))
    (loop with offset = 0 while (< offset length)
          do (multiple-value-bind (count errno)
                 (sb-unix:unix-write (get-descriptor terminal) buffer offset (- length offset))
               (cond
                 ((and count (plusp count)) (incf offset count))
                 ((eql count 0) (error "PTY write made no progress."))
                 ((= errno sb-posix:eintr))
                 ((= errno sb-posix:eagain)
                  (get-poll-events (list (cons terminal +pollout+))))
                 (t (error "write failed: ~A" (sb-int:strerror errno)))))
          finally (return offset))))

(defun set-socket-sigpipe (fd)
  "Prevent a closed macOS socket from terminating the Lisp process."
  (let ((value (make-array 1 :element-type '(signed-byte 32) :initial-element 1)))
    (sb-sys:with-pinned-objects (value)
      (when (minusp (sb-bsd-sockets-internal::setsockopt
                    fd #xffff #x1022
                    (sb-alien:sap-alien (sb-sys:vector-sap value) (* t)) 4))
        (error "setsockopt(SO_NOSIGPIPE) failed."))))
  t)

(defun del-pty (terminal)
  "Close a PTY stream through its SBCL owner."
  (etypecase terminal
    (file-stream (close terminal :abort t))
    (integer (sb-posix:close terminal)))
  t)

(defun set-raw-terminal (function &key (fd 0))
  "Run FUNCTION in raw mode, restoring the saved SB-POSIX termios object."
  (let ((descriptor (get-descriptor fd)))
    (if (not (tty-p descriptor))
        (funcall function)
        (let ((saved (sb-posix:tcgetattr descriptor))
              (raw (sb-posix:tcgetattr descriptor)))
          (setf (sb-posix:termios-iflag raw)
                (logandc2 (sb-posix:termios-iflag raw)
                          (logior sb-posix:ignbrk sb-posix:brkint sb-posix:parmrk
                                  sb-posix:istrip sb-posix:inlcr sb-posix:igncr
                                  sb-posix:icrnl sb-posix:ixon))
                (sb-posix:termios-oflag raw)
                (logandc2 (sb-posix:termios-oflag raw) sb-posix:opost)
                (sb-posix:termios-lflag raw)
                (logandc2 (sb-posix:termios-lflag raw)
                          (logior sb-posix:echo sb-posix:echonl sb-posix:icanon
                                  sb-posix:isig sb-posix:iexten))
                (sb-posix:termios-cflag raw)
                (logior (logandc2 (sb-posix:termios-cflag raw)
                                 (logior sb-posix:csize sb-posix:parenb))
                        sb-posix:cs8)
                (aref (sb-posix:termios-cc raw) sb-posix:vmin) 1
                (aref (sb-posix:termios-cc raw) sb-posix:vtime) 0)
          (unwind-protect
               (progn (sb-posix:tcsetattr descriptor sb-posix:tcsanow raw)
                      (funcall function))
            (handler-case (sb-posix:tcsetattr descriptor sb-posix:tcsanow saved)
              (error (condition) (warn "Terminal restoration failed: ~A" condition))))))))
