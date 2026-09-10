(defpackage #:mtm.test.platform
  (:use #:cl)
  (:import-from #:mtm.test
                #:check
                #:deftest
                #:wait-until)
  (:import-from #:mtm.pty
                #:del-shell-session
                #:new-shell-session
                #:pty-master)
  (:import-from #:mtm.platform
                #:new-detached-process
                #:set-raw-terminal
                #:tty-p))

(in-package #:mtm.test.platform)

(defun get-detached-report-path (pid)
  "Return a temporary report path for detached process PID.  "
  (merge-pathnames
   (format nil "mtm-detached-~A" pid)
   (uiop:temporary-directory)))

(deftest detached-process-leaves-the-parent-session ()
  (let ((pid nil)
        (path nil))
    (unwind-protect
         (progn
           (setf pid (new-detached-process
                      (lambda ()
                        (sleep 0.2)
                        (with-open-file (out (get-detached-report-path
                                              (sb-posix:getpid))
                                             :direction :output
                                             :if-exists :supersede
                                             :if-does-not-exist :create)
                          (format out "~A ~A ~A"
                                  (sb-posix:getpid)
                                  (sb-posix:getsid 0)
                                  (sb-unix:unix-isatty 0))))))
           (setf path (get-detached-report-path pid))
           (check (and (integerp pid) (plusp pid))
                  "The parent does not receive a child process identifier.")
           (check (not (probe-file path))
                  "The parent waits for the detached child.")
           (check (wait-until (lambda () (probe-file path)))
                  "The detached child does not run.")
           (sb-posix:waitpid pid 0)
           (let ((parts (uiop:split-string (uiop:read-file-string path))))
             (check (= 3 (length parts))
                    "The detached child writes an invalid report.")
             (let ((child-pid (parse-integer (first parts)))
                   (child-sid (parse-integer (second parts)))
                   (stdin-tty (parse-integer (third parts))))
               (check (= pid child-pid)
                      "The detached child has the wrong process identifier.")
               (check (= child-pid child-sid)
                      "The detached child remains in the parent session.")
               (check (zerop stdin-tty)
                      "The detached child keeps a terminal on stdin."))))
      (when path
        (ignore-errors (delete-file path))))))

(deftest terminal-test-separates-a-pty-from-a-socket ()
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream
                               :protocol :tcp))
        (session (new-shell-session :shell "/bin/sh")))
    (unwind-protect
         (progn
           (check (not (tty-p (sb-bsd-sockets:socket-file-descriptor socket)))
                  "A socket reports itself as a terminal.")
           (check (tty-p (pty-master session))
                  "A PTY master does not report itself as a terminal."))
      (sb-bsd-sockets:socket-close socket)
      (del-shell-session session))))

(deftest raw-terminal-passes-through-a-socket ()
  (let* ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                :type :stream
                                :protocol :tcp))
         (fd (sb-bsd-sockets:socket-file-descriptor socket)))
    (unwind-protect
         (check (eq :ran (set-raw-terminal (lambda () :ran) :fd fd))
                "Raw mode on a socket does not pass through.")
      (sb-bsd-sockets:socket-close socket))))

(deftest terminal-reports-its-size ()
  (let ((session (new-shell-session :shell "/bin/sh" :width 37 :height 9)))
    (unwind-protect
         (multiple-value-bind (rows columns)
             (mtm.platform:get-terminal-size (pty-master session))
           (check (and (= rows 9) (= columns 37))
                  "The terminal reports the wrong size."))
      (del-shell-session session))))

(defun terminal-settings (terminal)
  "Return terminal settings for TERMINAL as text."
  (let ((fd (etypecase terminal
              (integer terminal)
              (file-stream (sb-sys:fd-stream-fd terminal)))))
    (uiop:run-program (list "stty" "-g")
                      :input (format nil "/dev/fd/~D" fd)
                      :output :string)))

(deftest raw-terminal-restores-after-normal-exit-and-error ()
  (let ((session (new-shell-session :shell "/bin/sh" :width 80 :height 24)))
    (unwind-protect
        (let ((fd (pty-master session)))
          (let ((before (terminal-settings fd)))
            (set-raw-terminal (lambda () (values)) :fd fd)
            (check (string= before (terminal-settings fd))
                   "Normal raw-terminal exit does not restore settings.")
            (let ((raised nil))
              (handler-case
                  (set-raw-terminal
                   (lambda () (error "expected raw-terminal error"))
                   :fd fd)
                (error () (setf raised t)))
              (check raised
                     "Error raw-terminal body does not propagate.")
              (check (string= before (terminal-settings fd))
                     "Error raw-terminal exit does not restore settings."))))
      (del-shell-session session))))

