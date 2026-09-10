(defpackage #:mtm.test.integration
  (:use #:cl)
  (:import-from #:mtm
                #:attachment-attached-p
                #:new-attachment
                #:session-name
                #:session-running-p)
  (:import-from #:mtm.test
                #:attachment-output-has-text-p
                #:check
                #:deftest
                #:error-signals-p
                #:new-test-octets
                #:screen-has-text-p
                #:wait-until)
  (:import-from #:mtm.session
                #:attachment-size-owner-p
                #:del-attachment-value
                #:del-session
                #:del-session-manager
                #:get-attachment-output
                #:get-attachment-start-screen
                #:get-retained-screen
                #:get-session-manager-value
                #:get-session-value
                #:new-session-manager
                #:new-session-value
                #:set-active-attachment
                #:set-attachment-input)
  (:import-from #:mtm.terminal
                #:get-terminal-render
                #:new-terminal-emulator)
  (:import-from #:mtm.pty
                #:pty-master)
  (:import-from #:mtm.utf8
                #:get-utf8)
  (:import-from #:bordeaux-threads
                #:join-thread
                #:make-thread))

(in-package #:mtm.test.integration)

(deftest first-attachment-owns-terminal-size ()
  (new-session-manager)
  (unwind-protect
       (let* ((session (new-session-value "sized" :shell "/bin/sh"))
              (owner (new-attachment (session-name session)))
              (guest (new-attachment (session-name session))))
         (check (attachment-size-owner-p owner)
                "The first Attachment does not own the terminal size.")
         (check (not (attachment-size-owner-p guest))
                "A later Attachment owns the terminal size.")
         (del-attachment-value owner)
         (check (attachment-size-owner-p guest)
                "Detachment does not pass the terminal size to the next.")
         (del-attachment-value guest))
    (del-session-manager)))

(deftest session-size-follows-frontend ()
  (new-session-manager)
  (unwind-protect
       (let* ((session (new-session-value "resize" :shell "/bin/sh"))
              (owner (new-attachment (session-name session)))
              (frontend
                (mtm.frontend::new-session-frontend
                 :attachment owner
                 :rows 10
                 :columns 80)))
         (mtm.frontend::set-session-size frontend)
         (multiple-value-bind (rows columns)
             (mtm.platform:get-terminal-size
              (pty-master
               (mtm.session::managed-shell-session
                session)))
           (check (and (= rows 10) (= columns 80))
                  "The PTY does not follow the Terminal frontend size.")))
    (del-session-manager)))

;; Return the milliseconds a passthrough frontend needs to echo one byte.
(defun get-echo-milliseconds (name)
  "Return the round-trip time of one byte through a passthrough frontend."
  (multiple-value-bind (input-read input-write) (sb-posix:pipe)
    (multiple-value-bind (output-read output-write) (sb-posix:pipe)
      (let ((frontend
              (make-thread
               (lambda ()
                 (ignore-errors
                   (mtm:set-passthrough-frontend
                    :attachment (new-attachment name)
                    :input-fd input-read
                    :output-fd output-write)))
               :name "mtm test frontend")))
        (unwind-protect
             (progn
               ;; Wait for the shell to start before timing one byte.
               (sleep 1)
               (mtm.platform:set-fd input-write (get-utf8 "x"))
               (let ((start (get-internal-real-time)))
                 (loop
                   (let ((bytes (nth-value 0 (mtm.platform:get-fd output-read
                                                                  :wait-p t))))
                     (when (find (char-code #\x) bytes)
                       (return (round (* 1000
                                         (/ (- (get-internal-real-time) start)
                                            internal-time-units-per-second)))))))))
          (sb-posix:close input-write)
          (join-thread frontend)
          (sb-posix:close output-read))))))

(deftest passthrough-echoes-without-added-delay ()
  (new-session-manager)
  (unwind-protect
       (progn
         (new-session-value "echo-delay" :shell "/bin/sh")
         (let ((milliseconds (get-echo-milliseconds "echo-delay")))
           (check (< milliseconds 60)
                  "The frontend adds ~D ms to one echoed byte."
                  milliseconds)))
    (del-session-manager)))

(deftest managed-session-flushes-pending-utf8 ()
  (let* ((manager (new-session-manager))
         (terminal (new-terminal-emulator :width 8 :height 2))
         (session
           (make-instance
            'mtm.session::managed-session
            :name "utf8-flush"
            :manager manager
            :shell-session nil
            :terminal terminal)))
    (unwind-protect
         (progn
           (mtm.session::set-session-pty-output
            session
            (new-test-octets #xe4))
           (mtm.session::set-session-terminated session)
           (check (screen-has-text-p terminal (string (code-char #xfffd)))
                  "Session termination must flush pending UTF-8 bytes."))
      (del-session-manager))))

(defun session-value-or-nil (name)
  "Return the named Session, or NIL when it is missing."
  (handler-case
      (get-session-value name)
    (error () nil)))

(deftest managed-session-forwards-raw-input-and-output ()
  (new-session-manager)
  (unwind-protect
       (let* ((session (new-session-value "raw"
                                    :shell "/bin/sh"
                                    :width 30
                                    :height 6))
              (attachment (new-attachment "raw")))
         (check attachment
                "The Session does not accept its first Attachment.")
         (check (set-attachment-input
                 attachment
                 (get-utf8
                  (format nil "printf '~A033[31mraw-marker~A033[0m'; sleep 1~%"
                          (string (code-char 92))
                          (string (code-char 92)))))
                "The Attachment rejects raw input.")
         (check
          (attachment-output-has-text-p
           attachment
           (format nil "~C[31mraw-marker~C[0m" #\Escape #\Escape))
          "The Attachment rewrites raw shell output.")
         (check (session-running-p session)
                "The healthy Session stops unexpectedly.")
         (check (attachment-attached-p attachment)
                "The healthy Attachment disconnects unexpectedly."))
    (del-session-manager)))

(deftest detached-session-restores-retained-display ()
  (new-session-manager)
  (unwind-protect
       (let* ((session (new-session-value "screen"
                                    :shell "/bin/sh"
                                    :width 20
                                    :height 4))
              (first (new-attachment "screen")))
         (check (set-attachment-input
                 first
                 (get-utf8 (format nil "printf screen-marker; sleep 1~%")))
                "The first Attachment rejects input.")
         (check (wait-until
                 (lambda ()
                   (screen-has-text-p (get-retained-screen session)
                                      "screen-marker")))
                "The Session does not update its retained display.")
         (set-active-attachment first)
         (check (del-attachment-value first)
                "The active Attachment does not detach.")
         (check (session-running-p session)
                "Detaching terminates the Session.")
         (let ((second (new-attachment "screen")))
           (check (screen-has-text-p
                   (get-attachment-start-screen second)
                   "screen-marker")
                  "Reattachment loses the retained display.")
           (check (search "screen-marker"
                          (get-terminal-render
                           (get-attachment-start-screen second)))
                  "The retained display cannot render for reattachment.")
           (del-attachment-value second)))
    (del-session-manager)))

(deftest managed-session-broadcasts-output ()
  (new-session-manager)
  (unwind-protect
       (let* ((session (new-session-value "broadcast" :shell "/bin/sh"))
              (first (new-attachment (session-name session)))
              (second (new-attachment (session-name session))))
         (check (and first second)
                "The Session does not accept multiple Attachments.")
         (check (set-attachment-input
                 first
                 (get-utf8 (format nil "printf broadcast-marker; sleep 1~%")))
                "The writer Attachment rejects input.")
         (check (and (attachment-output-has-text-p first "broadcast-marker")
                     (attachment-output-has-text-p second "broadcast-marker"))
                "The Session does not broadcast output."))
    (del-session-manager)))

(deftest slow-attachment-disconnects-after-buffer-overflow ()
  (new-session-manager :max-buffer-bytes 16384)
  (unwind-protect
       (let* ((session (new-session-value "overflow" :shell "/bin/sh"))
              (slow (new-attachment (session-name session)))
              (writer (new-attachment (session-name session)))
              (stop-drainer-p nil)
              (drainer
                (make-thread
                 (lambda ()
                   (loop until stop-drainer-p
                         do (get-attachment-output writer :wait-p nil)
                            (sleep 0.001)))
                 :name "mtm test output drainer")))
         (unwind-protect
              (progn
                (check (set-attachment-input
                        writer
                        (get-utf8
                         (format nil
                                 "i=0; while [ $i -lt 2000 ]; do printf 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'; i=$((i+1)); done~%")))
                       "The writer Attachment rejects input.")
                (check (wait-until
                        (lambda () (not (attachment-attached-p slow))))
                       "A slow Attachment survives buffer overflow.")
                (check (attachment-attached-p writer)
                       "A healthy Attachment disconnects with the slow one."))
           (setf stop-drainer-p t)
           (join-thread drainer)))
    (del-session-manager)))

(deftest last-input-detach-closes-one-attachment ()
  (new-session-manager)
  (unwind-protect
       (let* ((session (new-session-value "last-input" :shell "/bin/sh"))
              (first (new-attachment "last-input"))
              (second (new-attachment "last-input")))
         (check (error-signals-p
                 (lambda () (mtm.session:del-attachment)))
                "Detach without input succeeds.")
         (check (set-attachment-input first (get-utf8 (format nil ":~%")))
                "The first Attachment rejects input.")
         (check (string= "last-input" (mtm.session:del-attachment))
                "Last-input detach returns the wrong name.")
         (check (and (not (attachment-attached-p first))
                     (attachment-attached-p second))
                "Last-input detach closes the wrong Attachment.")
         (check (session-running-p session)
                "Last-input detach terminates the Session.")
         (check (set-attachment-input second (get-utf8 (format nil ":~%")))
                "The second Attachment rejects input.")
         (check (string= "last-input" (mtm.session:del-attachment))
                "Last-input detach misses the later writer.")
         (check (not (attachment-attached-p second))
                "Last-input detach leaves the later writer connected.")
         (check (error-signals-p
                 (lambda () (mtm.session:del-attachment)))
                "Detach after the last writer succeeds."))
    (del-session-manager)))

(deftest last-input-detach-follows-the-writing-session ()
  (new-session-manager)
  (unwind-protect
       (let ((first (new-attachment
                     (session-name (new-session-value "first-input"
                                                      :shell "/bin/sh"))))
             (second (new-attachment
                      (session-name (new-session-value "second-input"
                                                       :shell "/bin/sh")))))
         (check (set-attachment-input first (get-utf8 (format nil ":~%")))
                "The first Session Attachment rejects input.")
         (check (set-attachment-input second (get-utf8 (format nil ":~%")))
                "The second Session Attachment rejects input.")
         (check (string= "second-input" (mtm.session:del-attachment))
                "Last-input detach does not follow the later Session.")
         (check (and (attachment-attached-p first)
                     (not (attachment-attached-p second)))
                "Last-input detach closes the wrong Session Attachment."))
    (del-session-manager)))

(deftest natural-exit-removes-session ()
  (new-session-manager)
  (unwind-protect
       (let* ((session (new-session-value "exit" :shell "/bin/sh"))
              (attachment (new-attachment "exit")))
         (check (set-attachment-input attachment
                                      (get-utf8 (format nil "exit~%")))
                "The Attachment rejects exit input.")
         (check (wait-until (lambda () (null (session-value-or-nil "exit"))))
                "Natural shell exit keeps the Session record.")
         (check (not (attachment-attached-p attachment))
                "Natural shell exit keeps the Attachment connected.")
         (check (not (session-running-p session))
                "Natural shell exit leaves the Session running."))
    (del-session-manager)))

(deftest explicit-termination-stops-session ()
  (new-session-manager)
  (unwind-protect
       (let* ((session (new-session-value "delete" :shell "/bin/sh"))
              (attachment (new-attachment "delete")))
         (check (eq session (del-session "delete"))
                "Explicit Session deletion fails.")
         (check (not (session-running-p session))
                "Explicit deletion leaves the Session running.")
         (check (not (attachment-attached-p attachment))
                "Explicit deletion leaves the Attachment connected.")
         (check (null (session-value-or-nil "delete"))
                "Explicit deletion keeps the named Session."))
    (del-session-manager)))

(deftest session-manager-stops-all-sessions ()
  (new-session-manager)
  (unwind-protect
       (let* ((manager (get-session-manager-value))
              (session (new-session-value "managed" :shell "/bin/sh")))
         (check manager
                "The Session manager does not start.")
         (check (eq manager (get-session-manager-value))
                "The process does not share its Session manager.")
         (check (eq manager (del-session-manager))
                "The Session manager does not stop.")
         (check (null (get-session-manager-value))
                "The Session manager remains globally stored.")
         (check (not (session-running-p session))
                "Stopping the manager leaves a Session running."))
    (when (get-session-manager-value)
      (del-session-manager))))

