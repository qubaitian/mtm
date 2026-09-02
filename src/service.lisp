(in-package #:mtm.service)

;; Limit each supervisor read to a small PTY chunk.
(defconstant +service-read-size+ 4096)
;; Wait one fixed second before restarting a failed Service.
(defconstant +service-restart-delay+ 1)

;; Store one managed program, its PTY, and its recent output.
(defclass managed-service ()
  (;; Keep the unique Service name.
   (name
    :initarg :name
    :reader get-service-runtime-name)
   ;; Keep the executable path and argument vector.
   (program
    :initarg :program
    :accessor get-service-runtime-program)
   ;; Keep the optional child working directory.
   (working-directory
    :initarg :working-directory
    :accessor get-service-runtime-working-directory)
   ;; Keep the source that owns this Service.
   (source-path
    :initarg :source-path
    :accessor get-service-runtime-source-path)
   ;; Protect Service state and output.
   (lock
    :initform (make-lock "mtm managed service")
    :reader get-service-runtime-lock)
   ;; Wake the supervisor after lifecycle changes.
   (condition
    :initform (make-condition-variable :name "mtm service state")
    :reader get-service-runtime-condition)
   ;; Hold the current child PTY process.
   (process
    :initform nil
    :accessor get-service-runtime-process)
   ;; Run the Service supervisor loop.
   (reader-thread
    :initform nil
    :accessor get-service-runtime-reader-thread)
   ;; Store the requested lifecycle state.
   (desired-state
    :initform :running
    :accessor get-service-runtime-desired-state)
   ;; Store the observed lifecycle state.
   (state
    :initform :stopped
    :accessor get-service-runtime-state)
   ;; Request a process replacement after specification changes.
   (restart-p
    :initform nil
    :accessor get-service-runtime-restart-p)
   ;; Mark the Service closed during manager shutdown.
   (closed-p
    :initform nil
    :accessor get-service-runtime-closed-p)
   ;; Limit retained Service output.
   (max-buffer-bytes
    :initarg :max-buffer-bytes
    :reader get-service-runtime-max-buffer-bytes)
   ;; Keep output chunks in arrival order.
   (output-chunks
    :initform nil
    :accessor get-service-runtime-output-chunks)
   ;; Track the retained output size.
   (output-bytes
    :initform 0
    :accessor get-service-runtime-output-bytes)))

;; Append BYTES and trim old output beyond the configured limit.
(defun set-service-runtime-output (service bytes)
  (when (and bytes (plusp (length bytes)))
    (with-lock-held ((get-service-runtime-lock service))
      ;; Keep a copy because callers may reuse their input vector.
      (setf (get-service-runtime-output-chunks service)
            (nconc (get-service-runtime-output-chunks service)
                   (list (copy-seq bytes))))
      (incf (get-service-runtime-output-bytes service) (length bytes))
      ;; ponytail: trim with a linear chunk scan; add a ring buffer if needed.
      (loop while (> (get-service-runtime-output-bytes service)
                     (get-service-runtime-max-buffer-bytes service))
            for chunk = (first (get-service-runtime-output-chunks service))
            for excess = (- (get-service-runtime-output-bytes service)
                            (get-service-runtime-max-buffer-bytes service))
            do (if (<= (length chunk) excess)
                   (progn
                     (setf (get-service-runtime-output-chunks service)
                           (rest (get-service-runtime-output-chunks service)))
                     (decf (get-service-runtime-output-bytes service)
                           (length chunk)))
                   (progn
                     (setf (first (get-service-runtime-output-chunks service))
                           (subseq chunk excess))
                     (decf (get-service-runtime-output-bytes service) excess))))
      (condition-notify (get-service-runtime-condition service)))))

;; Return a copy of SERVICE's retained output.
(defun get-service-runtime-output (service)
  "Return a copy of SERVICE's retained output."
  (with-lock-held ((get-service-runtime-lock service))
    (let ((result
            (make-array (get-service-runtime-output-bytes service)
                        :element-type '(unsigned-byte 8)))
          (offset 0))
      (dolist (chunk (get-service-runtime-output-chunks service))
        (replace result chunk :start1 offset)
        (incf offset (length chunk)))
      result)))

;; Return a stable snapshot of SERVICE's public state.
(defun get-service-runtime-snapshot (service)
  "Return a stable snapshot of SERVICE's public state."
  (with-lock-held ((get-service-runtime-lock service))
    ;; Read the child process once for a consistent PID.
    (let ((process (get-service-runtime-process service)))
      (list :name (get-service-runtime-name service)
            :desired-state (get-service-runtime-desired-state service)
            :state (get-service-runtime-state service)
            :pid (when process
                   (get-process-id process))))))

;; Replace SERVICE's executable specification on the next loop turn.
(defun set-service-runtime-specification
    (service program working-directory)
  "Replace SERVICE's executable specification on the next loop turn."
  (with-lock-held ((get-service-runtime-lock service))
    (unless (and (equal program (get-service-runtime-program service))
                 (equal working-directory
                        (get-service-runtime-working-directory service)))
      (setf (get-service-runtime-program service) program
            (get-service-runtime-working-directory service)
            working-directory
            (get-service-runtime-restart-p service)
            (not (null (get-service-runtime-process service))))
      (condition-notify (get-service-runtime-condition service))))
  service)

;; Set SERVICE's desired lifecycle state.
(defun set-service-runtime-desired-state (service state)
  "Set SERVICE's desired lifecycle state."
  (unless (member state '(:running :stopped) :test #'eq)
    (error "Service state must be RUNNING or STOPPED."))
  (with-lock-held ((get-service-runtime-lock service))
    (when (get-service-runtime-closed-p service)
      (error "The Service is closed."))
    (setf (get-service-runtime-desired-state service) state)
    (condition-notify (get-service-runtime-condition service)))
  service)

;; Start SERVICE's current executable and retain it when still wanted.
(defun set-service-runtime-process (service)
  "Start SERVICE's current executable and retain it when still wanted."
  (handler-case
      (let ((program nil)
            (working-directory nil))
        (with-lock-held ((get-service-runtime-lock service))
          ;; Copy the specification before forking the child.
          (setf program (get-service-runtime-program service)
                working-directory
                (get-service-runtime-working-directory service)))
        (let ((process
                (new-process-session
                 :program program
                 :working-directory working-directory
                 :width 80
                 :height 24)))
          (let ((keep-p
                  (with-lock-held ((get-service-runtime-lock service))
                    (if (and (not (get-service-runtime-closed-p service))
                             (eq (get-service-runtime-desired-state service)
                                 :running)
                             (equal program
                                    (get-service-runtime-program service))
                             (equal working-directory
                                    (get-service-runtime-working-directory
                                     service))
                             (null (get-service-runtime-process service)))
                        (progn
                          (setf (get-service-runtime-process service) process
                                (get-service-runtime-state service) :running)
                          t)
                        nil))))
            (unless keep-p
              (del-process-session process)))))
    (error ()
      (with-lock-held ((get-service-runtime-lock service))
        (unless (get-service-runtime-closed-p service)
          (setf (get-service-runtime-state service) :failed)))
      (sleep +service-restart-delay+))))

;; Mark PROCESS exited and return true when it should restart.
(defun set-service-runtime-exited (service process)
  "Mark PROCESS exited and return true when it should restart."
  (with-lock-held ((get-service-runtime-lock service))
    (when (eq process (get-service-runtime-process service))
      (setf (get-service-runtime-process service) nil
            (get-service-runtime-state service)
            (if (and (not (get-service-runtime-closed-p service))
                     (eq (get-service-runtime-desired-state service)
                         :running))
                :failed
                :stopped))
      (and (not (get-service-runtime-closed-p service))
           (eq (get-service-runtime-desired-state service) :running)))))

;; Run SERVICE's process and restart it while requested.
(defun set-service-runtime-loop (service)
  "Run SERVICE's process and restart it while requested."
  (handler-case
      (loop
        (let (;; Snapshot whether manager shutdown requested closure.
              (closed-p nil)
              ;; Hold the child selected for termination.
              (close-process nil)
              ;; Mark whether the next loop must start a child.
              (start-p nil))
          (with-lock-held ((get-service-runtime-lock service))
            (setf closed-p (get-service-runtime-closed-p service))
            (cond
              (closed-p
               (setf close-process (get-service-runtime-process service)
                     (get-service-runtime-process service) nil
                     (get-service-runtime-state service) :stopped))
              ((and (get-service-runtime-process service)
                    (or (get-service-runtime-restart-p service)
                        (eq (get-service-runtime-desired-state service)
                            :stopped)))
               (setf close-process (get-service-runtime-process service)
                     (get-service-runtime-process service) nil
                     (get-service-runtime-restart-p service) nil
                     (get-service-runtime-state service)
                     (if (eq (get-service-runtime-desired-state service)
                             :stopped)
                         :stopped
                         :failed)))
              ((and (null (get-service-runtime-process service))
                    (eq (get-service-runtime-desired-state service)
                        :running))
               (setf start-p t))
              ((null (get-service-runtime-process service))
               (setf (get-service-runtime-state service) :stopped))))
          (when close-process
            (del-process-session close-process))
          (when closed-p
            (return))
          (when start-p
            (set-service-runtime-process service))
          (let ((process
                  (with-lock-held ((get-service-runtime-lock service))
                    (get-service-runtime-process service))))
            (if process
                (multiple-value-bind (bytes eof-p)
                    (get-process-output-bytes
                     process
                     :max-bytes +service-read-size+
                     :wait-p nil)
                  (when (and bytes (plusp (length bytes)))
                    (set-service-runtime-output service bytes))
                  (if eof-p
                      (progn
                        (let ((restart-p
                                (set-service-runtime-exited service process)))
                          (del-process-session process :terminate-p nil)
                          (when restart-p
                            (sleep +service-restart-delay+))))
                      (when (or (null bytes) (zerop (length bytes)))
                        (sleep 0.01))))
                (with-lock-held ((get-service-runtime-lock service))
                  (unless (get-service-runtime-closed-p service)
                    (condition-wait
                     (get-service-runtime-condition service)
                     (get-service-runtime-lock service))))))))
    (error ()
      (let ((process nil)
            (closed-p nil))
        (with-lock-held ((get-service-runtime-lock service))
          ;; Detach a failed child before closing its PTY.
          (setf process (get-service-runtime-process service)
                closed-p (get-service-runtime-closed-p service)
                (get-service-runtime-process service) nil
                (get-service-runtime-state service)
                (if closed-p :stopped :failed)))
        (when process
          (ignore-errors (del-process-session process)))))))

;; Stop SERVICE's supervisor thread and child process.
(defun del-service-runtime (service)
  "Stop SERVICE's supervisor thread and child process."
  (let ((thread nil))
    (with-lock-held ((get-service-runtime-lock service))
      (setf (get-service-runtime-closed-p service) t
            (get-service-runtime-desired-state service) :stopped
            thread (get-service-runtime-reader-thread service))
      (condition-notify (get-service-runtime-condition service)))
    (when (and thread
               (not (eq thread (current-thread))))
      (ignore-errors (join-thread thread))))
  t)

;; Start one managed Service and its supervisor thread.
(defun new-service-runtime
    (name program working-directory max-buffer-bytes &key source-path)
  "Start one managed Service and its supervisor thread."
  (check-type name string)
  (check-type max-buffer-bytes (integer 1))
  (let ((service
          (make-instance
           'managed-service
           :name name
           :program program
           :working-directory working-directory
           :source-path source-path
           :max-buffer-bytes max-buffer-bytes)))
    (setf (get-service-runtime-reader-thread service)
          (make-thread
           (lambda ()
             (set-service-runtime-loop service))
           :name (format nil "mtm service ~A" name)))
    service))
