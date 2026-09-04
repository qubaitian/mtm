(in-package #:mtm.platform)

(define-foreign-library libc
  (:darwin (:default "libc")))

(use-foreign-library libc)

;; These layouts match macOS arm64 system headers.
(defcstruct winsize
  (rows :unsigned-short)
  (columns :unsigned-short)
  (x-pixels :unsigned-short)
  (y-pixels :unsigned-short))

(defcstruct termios
  (input-flags :unsigned-long)
  (output-flags :unsigned-long)
  (control-flags :unsigned-long)
  (local-flags :unsigned-long)
  (control-chars (:array :unsigned-char 20))
  (input-speed :unsigned-long)
  (output-speed :unsigned-long))

(defcstruct pollfd
  (fd :int)
  (events :short)
  (revents :short))

(defcfun ("forkpty" %forkpty) :int
  (master :pointer)
  (name :pointer)
  (termios :pointer)
  (winsize :pointer))

(defcfun ("execvp" %execvp) :int
  (file :pointer)
  (argv :pointer))

(defcfun ("chdir" %chdir) :int
  (path :pointer))

(defcfun ("_exit" %exit) :void
  (status :int))

(defcfun ("read" %get-fd) :long
  (fd :int)
  (buffer :pointer)
  (count :unsigned-long))

(defcfun ("write" %set-fd) :long
  (fd :int)
  (buffer :pointer)
  (count :unsigned-long))

(defcfun ("close" %close-fd) :int
  (fd :int))

(defcfun ("fcntl" %fcntl) :int
  (fd :int)
  (command :int)
  &rest)

(defcfun ("poll" %poll) :int
  (fds :pointer)
  (count :unsigned-int)
  (timeout :int))

(defcfun ("waitpid" %waitpid) :int
  (pid :int)
  (status :pointer)
  (options :int))

(defcfun ("kill" %kill) :int
  (pid :int)
  (signal :int))

(defcfun ("setsockopt" %setsockopt) :int
  (socket :int)
  (level :int)
  (option :int)
  (value :pointer)
  (length :unsigned-int))

(defcfun ("isatty" %isatty) :int
  (fd :int))

(defcfun ("ioctl" %ioctl) :int
  (fd :int)
  (request :unsigned-long)
  &rest)

(defcfun ("tcgetattr" %tcgetattr) :int
  (fd :int)
  (termios :pointer))

(defcfun ("tcsetattr" %tcsetattr) :int
  (fd :int)
  (action :int)
  (termios :pointer))

(defcfun ("cfmakeraw" %cfmakeraw) :void
  (termios :pointer))

(defcfun ("__error" %errno-location) :pointer)

(defcfun ("strerror" %strerror) :string
  (errno :int))

(defconstant +f-getfl+ 3)
(defconstant +f-setfl+ 4)
(defconstant +f-getfd+ 1)
(defconstant +f-setfd+ 2)
(defconstant +fd-cloexec+ 1)
(defconstant +o-nonblock+ 4)
(defconstant +pollin+ #x0001)
(defconstant +pollout+ #x0004)
(defconstant +pollerr+ #x0008)
(defconstant +pollhup+ #x0010)
(defconstant +pollnval+ #x0020)
(defconstant +wait-nohang+ 1)
(defconstant +tcsanow+ 0)
(defconstant +eintr+ 4)
(defconstant +eio+ 5)
(defconstant +esrch+ 3)
(defconstant +eagain+ 35)
(defconstant +sighup+ 1)
(defconstant +sigterm+ 15)
(defconstant +sigkill+ 9)
(defconstant +sol-socket+ #xffff)
(defconstant +so-nosigpipe+ #x1022)
(defconstant +tiocgwinsz+ #x40087468)
(defconstant +tiocswinsz+ #x80087467)

;; These values match macOS arm64 terminal and polling headers.

(define-condition platform-error (error)
  ((operation
    :initarg :operation
    :reader platform-error-operation)
   (errno
    :initarg :errno
    :reader platform-error-errno)
   (message
    :initarg :message
    :reader platform-error-message))
  (:report (lambda (condition stream)
             (format stream "~A failed with errno ~D: ~A"
                     (platform-error-operation condition)
                     (platform-error-errno condition)
                     (platform-error-message condition)))))

;; Return the operating system error number.
(defun get-errno ()
  (mem-ref (%errno-location) :int))

;; Signal one operating system error with its saved errno.
(defun set-platform-error (operation)
  (let ((errno (get-errno)))
    (error 'platform-error
           :operation operation
           :errno errno
           :message (%strerror errno))))

(defun tty-p (fd)
  (plusp (%isatty fd)))

(defun get-system-clipboard ()
  "Return plain text from the macOS system clipboard."
  (ignore-errors
    (uiop:run-program (list "pbpaste")
                      :output :string
                      :error-output nil)))

(defun set-system-clipboard (text)
  "Store plain TEXT in the macOS system clipboard."
  (check-type text string)
  (handler-case
      (with-input-from-string (input text)
        (uiop:run-program (list "pbcopy")
                          :input input
                          :output nil
                          :error-output nil)
        t)
    (error () nil)))

(defun get-terminal-size (fd)
  "Return the terminal row and column counts for FD."
  (check-type fd integer)
  (with-foreign-object (winsize '(:struct winsize))
    (when (= (%ioctl fd +tiocgwinsz+ :pointer winsize) -1)
      (set-platform-error "ioctl(TIOCGWINSZ)"))
    (values
     (foreign-slot-value winsize '(:struct winsize) 'rows)
     (foreign-slot-value winsize '(:struct winsize) 'columns))))

(defun set-terminal-size (fd rows columns)
  "Set FD's terminal row and column counts."
  (check-type fd integer)
  (check-type rows (integer 1))
  (check-type columns (integer 1))
  (with-foreign-object (winsize '(:struct winsize))
    (setf (foreign-slot-value winsize '(:struct winsize) 'rows) rows
          (foreign-slot-value winsize '(:struct winsize) 'columns) columns
          (foreign-slot-value winsize '(:struct winsize) 'x-pixels) 0
          (foreign-slot-value winsize '(:struct winsize) 'y-pixels) 0)
    (when (= (%ioctl fd +tiocswinsz+ :pointer winsize) -1)
      (set-platform-error "ioctl(TIOCSWINSZ)")))
  (values rows columns))

(defun set-nonblocking (fd)
  (let ((flags (%fcntl fd +f-getfl+)))
    (when (= flags -1)
      (set-platform-error "fcntl(F_GETFL)"))
    (when (= (%fcntl fd +f-setfl+ :int (logior flags +o-nonblock+)) -1)
      (set-platform-error "fcntl(F_SETFL)"))))

(defun set-close-on-exec (fd)
  "Prevent FD from leaking into an exec'ed child process."
  (let ((flags (%fcntl fd +f-getfd+)))
    (when (= flags -1)
      (set-platform-error "fcntl(F_GETFD)"))
    (when (= (%fcntl fd +f-setfd+ :int (logior flags +fd-cloexec+)) -1)
      (set-platform-error "fcntl(F_SETFD)"))))

;; Start PROGRAM inside a non-blocking PTY.
(defun new-pty-process (program width height &key working-directory)
  "Start PROGRAM inside a non-blocking PTY."
  (check-type program cons)
  (unless (every #'stringp program)
    (error "PTY programs must contain only strings."))
  (when working-directory
    (check-type working-directory string))
  (check-type width (integer 1))
  (check-type height (integer 1))
  (let ((argument-pointers (mapcar #'foreign-string-alloc program))
        (working-directory-pointer
          (when working-directory
            (foreign-string-alloc working-directory)))
        (argv nil))
    (unwind-protect
         (progn
           ;; Build the null-terminated argv array before forking.
           (setf argv (foreign-alloc :pointer :count (1+ (length program))))
           (loop for pointer in argument-pointers
                 for index from 0
                 do (setf (mem-aref argv :pointer index) pointer))
           (setf (mem-aref argv :pointer (length program)) (null-pointer))
           (with-foreign-object (master :int)
             (with-foreign-object (winsize '(:struct winsize))
               (setf (foreign-slot-value winsize '(:struct winsize) 'rows) height
                     (foreign-slot-value winsize '(:struct winsize) 'columns) width
                     (foreign-slot-value winsize '(:struct winsize) 'x-pixels) 0
                     (foreign-slot-value winsize '(:struct winsize) 'y-pixels) 0)
               (let ((pid (%forkpty master (null-pointer) (null-pointer) winsize)))
                 (cond
                   ((= pid 0)
                    ;; The child changes directory before replacing itself.
                    (when (and working-directory-pointer
                               (= (%chdir working-directory-pointer) -1))
                      (%exit 127))
                    (%execvp (first argument-pointers) argv)
                    (%exit 127))
                   ((minusp pid)
                    (set-platform-error "forkpty"))
                   (t
                    (let ((fd (mem-ref master :int)))
                      (handler-case
                          (progn
                            (set-nonblocking fd)
                            (set-close-on-exec fd)
                            (values fd pid))
                        (error (condition)
                          (ignore-errors (%close-fd fd))
                          (ignore-errors (%kill pid +sigkill+))
                          (ignore-errors
                            (with-foreign-object (status :int)
                              (%waitpid pid status 0)))
                          (error condition))))))))))
      (when argv
        (foreign-free argv))
      (when working-directory-pointer
        (foreign-string-free working-directory-pointer))
      (dolist (pointer argument-pointers)
        (foreign-string-free pointer)))))

;; Start the selected shell inside a non-blocking PTY.
(defun new-pty (shell width height &key environment)
  "Start SHELL inside a non-blocking PTY."
  (check-type shell string)
  (new-pty-process
   (if environment
       (append (list "/usr/bin/env")
               ;; Convert each environment pair into one env argument.
               (mapcar (lambda (entry)
                         (format nil "~A=~A" (car entry) (cdr entry)))
                       environment)
               (list shell "-i"))
       (list shell "-i"))
   width
   height))

;; Wait for descriptor events and return their readiness flags.
(defun get-poll-events (descriptors &key (timeout -1))
  "Wait for DESCRIPTORS and return their nonzero revents."
  (check-type timeout integer)
  (if (null descriptors)
      nil
      (with-foreign-object (poll-array '(:struct pollfd) (length descriptors))
        (loop for (fd . events) in descriptors
              for index from 0
              for item = (mem-aptr poll-array '(:struct pollfd) index)
              do (setf (foreign-slot-value item '(:struct pollfd) 'fd) fd
                       (foreign-slot-value item '(:struct pollfd) 'events) events
                       (foreign-slot-value item '(:struct pollfd) 'revents) 0))
        (loop for result = (%poll poll-array (length descriptors) timeout)
              do (cond
                   ((plusp result)
                    (return
                      (loop for (fd . events) in descriptors
                            for index from 0
                            for item = (mem-aptr poll-array '(:struct pollfd) index)
                            for revents = (foreign-slot-value item '(:struct pollfd) 'revents)
                            when (plusp revents)
                              collect (cons fd revents))))
                   ((zerop result) (return nil))
                   ((= (get-errno) +eintr+) nil)
                   (t (set-platform-error "poll")))))))

;; Read bytes from FD and report end of file.
(defun get-fd (fd &key (max-bytes 4096) (wait-p nil))
  "Read bytes from FD and return bytes plus an end-of-file flag."
  (check-type max-bytes (integer 1))
  (with-foreign-object (buffer `(:array :unsigned-char ,max-bytes))
    (loop for count = (%get-fd fd buffer max-bytes)
          do (cond
               ((plusp count)
                (let ((bytes (make-array count
                                         :element-type '(unsigned-byte 8))))
                  (loop for index below count
                        do (setf (aref bytes index)
                                 (mem-aref buffer :unsigned-char index)))
                  (return (values bytes nil))))
               ((zerop count) (return (values #() t)))
               (t
                (let ((errno (get-errno)))
                  (cond
                    ((= errno +eintr+) nil)
                    ((= errno +eagain+)
                     (if wait-p
                         (get-poll-events (list (cons fd +pollin+)) :timeout -1)
                         (return (values #() nil))))
                    ((= errno +eio+) (return (values #() t)))
                    (t (set-platform-error "read")))))))))

;; Write all BYTES to FD.
(defun set-fd (fd bytes)
  "Write all BYTES to FD and return the byte count."
  (check-type bytes vector)
  (let ((length (length bytes)))
    (if (zerop length)
        0
        (with-foreign-object (buffer `(:array :unsigned-char ,length))
          (loop for index below length
                do (setf (mem-aref buffer :unsigned-char index)
                         (aref bytes index)))
          (loop with offset = 0
                while (< offset length)
                for count = (%set-fd fd (inc-pointer buffer offset)
                                      (- length offset))
                do (cond
                     ((plusp count) (incf offset count))
                     ((zerop count) (return offset))
                     (t
                      (let ((errno (get-errno)))
                        (cond
                          ((= errno +eintr+) nil)
                          ((= errno +eagain+)
                           (get-poll-events (list (cons fd +pollout+)) :timeout -1))
                          (t (set-platform-error "write"))))))
                finally (return offset))))))

;; Read the process status, optionally without waiting.
(defun get-process-status (pid &key (no-hang-p nil))
  "Wait for PID and return its status, or NIL while it runs."
  (with-foreign-object (status :int)
    (loop for result = (%waitpid pid status (if no-hang-p +wait-nohang+ 0))
          do (cond
               ((= result pid) (return (mem-ref status :int)))
               ((zerop result) (return nil))
               ((and (= result -1) (= (get-errno) +eintr+)) nil)
               (t (set-platform-error "waitpid"))))))

;; Send SIGNAL to PID when the process still exists.
(defun set-process-signal (pid signal)
  "Send SIGNAL to PID when the process still exists."
  (check-type pid integer)
  (check-type signal integer)
  (when (and (minusp (%kill pid signal))
             (/= (get-errno) +esrch+))
    (set-platform-error "kill"))
  t)

;; Send a hangup signal to PID.
(defun del-process (pid)
  "Send SIGHUP to PID."
  (set-process-signal pid +sighup+))

(defun set-socket-sigpipe (fd)
  "Prevent a closed socket from terminating the Lisp process."
  (with-foreign-object (value :int)
    (setf (mem-ref value :int) 1)
    (when (minusp (%setsockopt fd
                               +sol-socket+
                               +so-nosigpipe+
                               value
                               (foreign-type-size :int)))
      (set-platform-error "setsockopt(SO_NOSIGPIPE)")))
  t)

;; Close the PTY master descriptor FD.
(defun del-pty (fd)
  "Close the PTY master FD."
  (when (and (minusp (%close-fd fd))
             (/= (get-errno) 9))
    (set-platform-error "close"))
  t)

;; Run FUNCTION with FD in raw mode and restore it.
(defun set-raw-terminal (function &key (fd 0))
  "Run FUNCTION with FD in raw mode and always restore its settings."
  (if (not (tty-p fd))
      (funcall function)
      (with-foreign-object (saved '(:struct termios))
        (when (= (%tcgetattr fd saved) -1)
          (set-platform-error "tcgetattr"))
        (unwind-protect
            (with-foreign-object (raw '(:struct termios))
              (when (= (%tcgetattr fd raw) -1)
                (set-platform-error "tcgetattr"))
              (%cfmakeraw raw)
              (when (= (%tcsetattr fd +tcsanow+ raw) -1)
                (set-platform-error "tcsetattr(raw)"))
              (funcall function))
          ;; Keep the original error when restoration itself fails.
          (when (= (%tcsetattr fd +tcsanow+ saved) -1)
            (warn "Terminal restoration failed with errno ~D."
                  (get-errno)))))))
