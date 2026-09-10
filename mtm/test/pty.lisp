(defpackage #:mtm.test.pty
  (:use #:cl)
  (:import-from #:mtm.test
                #:bytes-to-string
                #:check
                #:deftest)
  (:import-from #:mtm.pty
                #:del-shell-session
                #:get-shell-output-bytes
                #:new-shell-session
                #:pty-master
                #:session-open-p
                #:set-shell-input
                #:set-shell-size)
  (:import-from #:mtm.platform
                #:tty-p)
  (:import-from #:mtm.utf8
                #:get-utf8))

(in-package #:mtm.test.pty)

(deftest pty-session-forwards-raw-bytes ()
  (let ((session (new-shell-session :shell "/bin/sh" :width 100 :height 40)))
    (unwind-protect
        (progn
          (check (session-open-p session)
                 "The PTY Session does not start open.")
          (check (tty-p (pty-master session))
                 "The PTY master is not a terminal descriptor.")
          (set-shell-input
           session
           (get-utf8
            (format nil "stty size; printf '~A033[31mraw-marker~A033[0m'; exit~%"
                    (string (code-char 92))
                    (string (code-char 92)))))
          (let ((output ""))
            (loop
              (multiple-value-bind (bytes eof-p)
                  (get-shell-output-bytes session)
                (when (and bytes (plusp (length bytes)))
                  (setf output
                        (concatenate 'string output (bytes-to-string bytes))))
                (when eof-p
                  (return))))
            (check (search "40 100" output)
                   "The shell does not observe its fixed PTY size.")
            (check (search (format nil "~C[31mraw-marker~C[0m" #\Escape #\Escape)
                           output)
                   "The PTY output does not preserve control bytes.")))
      (del-shell-session session))
    (check (not (session-open-p session))
           "The PTY Session remains open after close.")))

(deftest pty-session-updates-terminal-size ()
  (let ((session (new-shell-session :shell "/bin/sh" :width 100 :height 40)))
    (unwind-protect
        (progn
          (set-shell-size session 11 42)
          (set-shell-input session (get-utf8 (format nil "stty size; exit~%")))
          (let ((output ""))
            (loop
              (multiple-value-bind (bytes eof-p)
                  (get-shell-output-bytes session)
                (when (and bytes (plusp (length bytes)))
                  (setf output
                        (concatenate 'string output (bytes-to-string bytes))))
                (when eof-p
                  (return))))
            (check (search "11 42" output)
                   "The shell does not observe the updated PTY size.")))
      (del-shell-session session))))

