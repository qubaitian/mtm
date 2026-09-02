(in-package #:mtm.session)

(defconstant +default-buffer-bytes+ (* 1024 1024))
(defconstant +session-read-size+ 4096)

(defvar *session-state-lock* (make-lock "mtm process state"))
(defvar *session-manager* nil)
;; Store the Attachment used by this Lisp process.
(defvar *active-attachment* nil)

(defclass session-manager ()
  ((named-sessions
    :initform (make-hash-table :test #'equal)
    :accessor manager-named-sessions)
   (lock
    :initform (make-lock "mtm session manager")
    :reader manager-lock)
   (max-buffer-bytes
    :initarg :max-buffer-bytes
    :reader manager-max-buffer-bytes)
   (closed-p
    :initform nil
    :accessor manager-closed-p))
  (:documentation "Own managed shell sessions and their attachments."))

;; Store one named shell Session and its shared display state.
(defclass managed-session ()
  ((name
    :initarg :name
    :reader session-name)
   (manager
    :initarg :manager
    :reader session-manager)
   (shell-session
    :initarg :shell-session
    :reader managed-shell-session)
   (terminal
    :initarg :terminal
    :reader managed-terminal)
   (lock
    :initform (make-lock "mtm managed session")
    :reader session-lock)
   (input-lock
    :initform (make-lock "mtm session input")
    :reader session-input-lock)
   (read-lock
    :initform (make-lock "mtm session PTY read")
    :reader session-read-lock)
   (write-lock
    :initform (make-lock "mtm session PTY write")
    :reader session-write-lock)
   (attachments
    :initform nil
    :accessor session-attachments)
   (running-p
    :initform t
    :accessor managed-session-running-p)
   (terminated-p
    :initform nil
    :accessor managed-session-terminated-p)
   ;; Track automatic full-screen terminal transport.
   (full-screen-p
    :initform nil
    :accessor managed-session-full-screen-p)
   ;; Track the Attachment that controls full-screen terminal size.
   (full-screen-owner
    :initform nil
    :accessor managed-session-full-screen-owner)
   (reader-thread
    :initform nil
    :accessor session-reader-thread)
   (pending-bytes
    :initform nil
    :accessor session-pending-bytes)
   (history-box
    :initform (cons nil nil)
    :reader session-history-box)))

(defclass attachment ()
  ((session
    :initarg :session
    :reader attachment-session)
   (start-screen
    :initarg :start-screen
    :reader stored-attachment-start-screen)
   (condition
    :initform (make-condition-variable :name "mtm attachment output")
    :reader attachment-condition)
   (buffer
    :initform nil
    :accessor attachment-buffer)
   (buffer-bytes
    :initform 0
    :accessor attachment-buffer-bytes)
   (max-buffer-bytes
    :initarg :max-buffer-bytes
    :reader attachment-max-buffer-bytes)
   (attached-p
    :initform t
    :accessor managed-attachment-attached-p)))

;; Ensure the process-global Session manager exists.
(defun new-session-manager (&key
                               (max-buffer-bytes +default-buffer-bytes+))
  "Ensure and return the process-global Session manager."
  (check-type max-buffer-bytes (integer 1))
  (with-lock-held (*session-state-lock*)
    (or *session-manager*
        (setf *session-manager*
              (make-instance 'session-manager
                             :max-buffer-bytes max-buffer-bytes)))))

;; Return the process-global Session manager object for internal callers.
(defun get-session-manager-value ()
  "Return the process-global Session manager object, or NIL."
  (with-lock-held (*session-state-lock*)
    *session-manager*))

;; Return named Sessions while MANAGER's lock is held.
(defun get-session-list-under-lock (manager)
  "Return MANAGER's named Sessions and running states."
  (unless (manager-closed-p manager)
    (sort
     (loop for session being the hash-values of (manager-named-sessions manager)
           collect (cons (session-name session) :running))
     #'string<
     :key #'car)))

;; Return the Session manager state and all named Sessions.
(defun get-session-manager ()
  "Return the Session manager state snapshot."
  (with-lock-held (*session-state-lock*)
    (let ((manager *session-manager*))
      (list :state (if manager :running :stopped)
            :sessions (if manager
                          (with-lock-held ((manager-lock manager))
                            (get-session-list-under-lock manager))
                          nil)))))

(defun session-running-under-lock-p (session)
  "Return true when SESSION accepts attachments and input."
  (and (managed-session-running-p session)
       (not (managed-session-terminated-p session))))

;; Return the shell selected by the environment.
(defun get-shell ()
  "Return the shell selected by the environment."
  (or (uiop:getenv "SHELL") "/bin/sh"))

(defun valid-session-name-p (name)
  "Return true when NAME is a simple session name."
  (and (stringp name)
       (plusp (length name))
       (every (lambda (character)
                (or (alphanumericp character)
                    (find character "._-" :test #'char=)))
              name)))

(defun get-valid-session-name (name)
  "Signal an error when NAME is not a valid session name."
  (unless (valid-session-name-p name)
    (error "Session names use letters, digits, dots, dashes, and underscores."))
  name)

;; Return the managed Session value named NAME.
(defun get-session-value (name)
  "Return the managed Session value named NAME."
  (get-valid-session-name name)
  (let ((manager (get-session-manager-value)))
    (or (when manager
          (with-lock-held ((manager-lock manager))
            (unless (manager-closed-p manager)
              (gethash name (manager-named-sessions manager)))))
        (if manager
            (error "The Session named ~A does not exist." name)
            (error "The Session manager is stopped.")))))

;; Return every named Session and its running state.
(defun get-session-list ()
  "Return active named Sessions as NAME and STATE conses."
  (let ((manager (get-session-manager-value)))
    (when manager
      (with-lock-held ((manager-lock manager))
        (get-session-list-under-lock manager)))))

(defun del-session-registry-entry (session)
  "Remove SESSION from its manager after termination."
  (let ((manager (session-manager session)))
    (with-lock-held ((manager-lock manager))
      (remhash (session-name session)
               (manager-named-sessions manager)))))

(defun del-managed-shell-session (session)
  "Close SESSION after its PTY reads and writes finish."
  (with-lock-held ((session-read-lock session))
    (with-lock-held ((session-write-lock session))
      (ignore-errors (del-shell-session (managed-shell-session session))))))

;; Ensure a fixed-size named shell and return its Session value.
(defun new-session-value (name &key
                                 (shell (get-shell))
                                 (width 80)
                                 (height 24))
  "Ensure a fixed-size named shell and return its Session value."
  (get-valid-session-name name)
  (check-type width (integer 1))
  (check-type height (integer 1))
  ;; Reuse the process-global manager for every named Session.
  (let ((manager (new-session-manager))
        (managed-session nil))
    ;; Serialize lookup and creation under the manager lock.
    (with-lock-held ((manager-lock manager))
      (when (manager-closed-p manager)
        (error "The Session manager is stopped."))
      ;; Reuse the named Session when it already exists.
      (let ((existing-session (gethash name (manager-named-sessions manager))))
        (if existing-session
            (setf managed-session existing-session)
            (let ((shell-session (new-shell-session :shell shell
                                                    :width width
                                                    :height height))
                  (success-p nil))
              ;; Clean up the PTY if Session construction fails.
              (unwind-protect
                   (progn
                     (setf managed-session
                           (make-instance
                            'managed-session
                            :name name
                            :manager manager
                            :shell-session shell-session
                            :terminal
                            (new-terminal-emulator
                             :width width
                             :height height)))
                     (setf (gethash name (manager-named-sessions manager))
                           managed-session
                           (session-reader-thread managed-session)
                           (make-thread
                            (lambda () (set-session-reader managed-session))
                            :name name)
                           success-p t))
                (unless success-p
                  (remhash name (manager-named-sessions manager))
                  (del-shell-session shell-session)))))))
    managed-session))

;; Report whether SESSION accepts Attachments and input.
(defun session-running-p (session)
  "Return true while SESSION accepts attachments and input."
  (check-type session managed-session)
  (with-lock-held ((session-lock session))
    (session-running-under-lock-p session)))

;; Return SESSION's terminal width and height.
(defun get-session-size (session)
  "Return SESSION's fixed terminal width and height."
  (check-type session managed-session)
  (with-lock-held ((session-lock session))
    (values (terminal-width (managed-terminal session))
            (terminal-height (managed-terminal session)))))

;; Return SESSION's latest retained display projection.
(defun get-retained-screen (session)
  "Return a copy of SESSION's latest display projection."
  (check-type session managed-session)
  (with-lock-held ((session-lock session))
    (get-terminal-copy (managed-terminal session))))

(defun get-attachment-start-screen (attachment)
  "Return the display projection captured when ATTACHMENT started."
  (check-type attachment attachment)
  (get-terminal-copy (stored-attachment-start-screen attachment)))

(defun attachment-attached-p (attachment)
  "Return true while ATTACHMENT remains connected."
  (check-type attachment attachment)
  (let ((session (attachment-session attachment)))
    (with-lock-held ((session-lock session))
      (managed-attachment-attached-p attachment))))

;; Attach to the running Session named NAME.
(defun new-attachment (name)
  "Attach to the running Session named NAME."
  (let* ((manager (or (get-session-manager-value)
                      (error "The Session manager is stopped.")))
         (session (get-session-value name)))
    (with-lock-held ((session-lock session))
      (unless (session-running-under-lock-p session)
        (error "The Session named ~A is stopped." name))
      (let* ((start-screen (get-terminal-copy (managed-terminal session)))
             (attachment
               (make-instance 'attachment
                              :session session
                              :start-screen start-screen
                              :max-buffer-bytes
                              (manager-max-buffer-bytes manager))))
        (when (and (managed-session-full-screen-p session)
                   (null (managed-session-full-screen-owner session)))
          (setf (managed-session-full-screen-owner session) attachment))
        (push attachment (session-attachments session))
        attachment))))

;; Return true while SESSION displays a full-screen terminal application.
(defun session-full-screen-p (session)
  "Return true while SESSION displays a full-screen terminal application."
  (check-type session managed-session)
  (with-lock-held ((session-lock session))
    (managed-session-full-screen-p session)))

;; Return true when ATTACHMENT controls full-screen terminal size.
(defun attachment-full-screen-owner-p (attachment)
  "Return true when ATTACHMENT controls full-screen terminal size."
  (check-type attachment attachment)
  (let ((session (attachment-session attachment)))
    (with-lock-held ((session-lock session))
      (eq attachment (managed-session-full-screen-owner session)))))

;; Clear ATTACHMENT from the process-global active position.
(defun del-active-attachment (attachment)
  "Clear ATTACHMENT from the process-global active position."
  (with-lock-held (*session-state-lock*)
    (when (eq attachment *active-attachment*)
      (setf *active-attachment* nil))))

;; Store ATTACHMENT as the process-global active Attachment.
(defun set-active-attachment (attachment)
  "Store ATTACHMENT as the process-global active Attachment."
  (check-type attachment attachment)
  (del-active-session)
  (with-lock-held (*session-state-lock*)
    (setf *active-attachment* attachment))
  attachment)

;; Detach the process's active Attachment.
(defun del-active-session ()
  "Detach the process-global active Attachment."
  (let ((attachment
          (with-lock-held (*session-state-lock*)
            (prog1 *active-attachment*
              (setf *active-attachment* nil)))))
    (when attachment
      (del-attachment attachment))))

;; Detach ATTACHMENT while its Session lock is held.
(defun del-attachment-under-lock (attachment)
  "Detach ATTACHMENT while its Session lock is held."
  (let ((attached-p (managed-attachment-attached-p attachment)))
    (setf (managed-attachment-attached-p attachment) nil
          (attachment-buffer attachment) nil
          (attachment-buffer-bytes attachment) 0
          (session-attachments (attachment-session attachment))
          (delete attachment
                  (session-attachments (attachment-session attachment))
                  :test #'eq))
    (when (eq attachment
              (managed-session-full-screen-owner (attachment-session attachment)))
      (setf (managed-session-full-screen-owner (attachment-session attachment)) nil))
    (condition-notify (attachment-condition attachment))
    attached-p))

;; Detach ATTACHMENT without closing its Session.
(defun del-attachment (attachment)
  "Detach ATTACHMENT without closing its Session."
  (check-type attachment attachment)
  (let ((session (attachment-session attachment)))
    (with-lock-held ((session-lock session))
      (del-attachment-under-lock attachment)))
  (del-active-attachment attachment)
  t)

(defun set-attachment-output (attachment bytes)
  "Queue BYTES, or disconnect ATTACHMENT after overflow."
  (let ((new-size (+ (attachment-buffer-bytes attachment)
                     (length bytes))))
    (cond
      ((> new-size (attachment-max-buffer-bytes attachment))
       (setf (managed-attachment-attached-p attachment) nil
             (attachment-buffer attachment) nil
             (attachment-buffer-bytes attachment) 0)
       (condition-notify (attachment-condition attachment))
       nil)
      (t
       (setf (attachment-buffer attachment)
             (nconc (attachment-buffer attachment)
                    (list (copy-seq bytes)))
             (attachment-buffer-bytes attachment) new-size)
       (condition-notify (attachment-condition attachment))
       t))))

(defun set-session-output-under-lock (session bytes)
  "Broadcast BYTES to every Attachment while SESSION is locked."
  (let ((detached nil))
    (when (and bytes (plusp (length bytes)))
      (dolist (attachment (copy-list (session-attachments session)))
        (unless (set-attachment-output attachment bytes)
          (del-attachment-under-lock attachment)
          (push attachment detached))))
    detached))

;; Update SESSION's projection and detect alternate-screen changes.
(defun set-session-pty-output (session bytes)
  "Update SESSION's display projection and broadcast raw BYTES."
  (let ((detached nil))
    (with-lock-held ((session-lock session))
      (when (session-running-under-lock-p session)
        (multiple-value-bind (text pending)
            (get-utf8-chunk bytes (session-pending-bytes session))
          (setf (session-pending-bytes session) pending)
          (when (plusp (length text))
            (set-terminal-input (managed-terminal session) text))
            (dolist (event (get-terminal-screen-events (managed-terminal session)))
            (case event
              (:enter
               (setf (managed-session-full-screen-p session) t
                     (managed-session-full-screen-owner session)
                     (or (managed-session-full-screen-owner session)
                         (first (session-attachments session)))))
              (:leave
               (setf (managed-session-full-screen-p session) nil
                     (managed-session-full-screen-owner session) nil))))
          (setf detached (set-session-output-under-lock session bytes)))))
    (dolist (attachment detached)
      (del-active-attachment attachment))))

;; Mark SESSION terminated and wake every Attachment.
(defun set-session-terminated (session)
  "Close SESSION and wake every attached frontend."
  (let ((attachments nil))
    (with-lock-held ((session-lock session))
      (unless (managed-session-terminated-p session)
        (setf (managed-session-running-p session) nil
              (managed-session-terminated-p session) t
              attachments (session-attachments session)
              (session-attachments session) nil)
        (dolist (attachment attachments)
          (setf (managed-attachment-attached-p attachment) nil)
          (condition-notify (attachment-condition attachment)))))
    (dolist (attachment attachments)
      (del-active-attachment attachment))
    (del-session-registry-entry session)
    t))

(defun set-session-reader (session)
  "Read PTY output in the background for SESSION."
  (handler-case
      (loop while (session-running-p session)
            do (multiple-value-bind (bytes eof-p)
                 (with-lock-held ((session-read-lock session))
                   (get-shell-output-bytes
                    (managed-shell-session session)
                    :max-bytes +session-read-size+
                    :wait-p nil))
               (when (and bytes (plusp (length bytes)))
                 (set-session-pty-output session bytes))
               (when eof-p
                 (return))
               (when (or (null bytes) (zerop (length bytes)))
                 (sleep 0.01))))
    (error () nil))
  (set-session-terminated session)
  (del-managed-shell-session session))

(defun get-attachment-output-chunk (attachment max-bytes)
  "Remove up to MAX-BYTES from ATTACHMENT's output buffer."
  (let* ((count (min max-bytes (attachment-buffer-bytes attachment)))
         (result (make-array count :element-type '(unsigned-byte 8)))
         (offset 0))
    (loop while (< offset count)
          for chunk = (first (attachment-buffer attachment))
          for amount = (min (- count offset) (length chunk))
          do (replace result chunk
                      :start1 offset
                      :end1 (+ offset amount)
                      :start2 0
                      :end2 amount)
             (incf offset amount)
             (decf (attachment-buffer-bytes attachment) amount)
             (if (= amount (length chunk))
                 (setf (attachment-buffer attachment)
                       (rest (attachment-buffer attachment)))
                 (setf (first (attachment-buffer attachment))
                       (subseq chunk amount))))
    result))

(defun get-attachment-output (attachment &key (max-bytes 4096) (wait-p t))
  "Read broadcast PTY bytes from ATTACHMENT."
  (check-type attachment attachment)
  (check-type max-bytes (integer 1))
  (let ((session (attachment-session attachment)))
    (with-lock-held ((session-lock session))
      (loop
        (cond
          ((plusp (attachment-buffer-bytes attachment))
           (return (values (get-attachment-output-chunk attachment max-bytes)
                           nil)))
          ((or (not (managed-attachment-attached-p attachment))
               (managed-session-terminated-p session))
           (return (values nil t)))
          ((not wait-p)
           (return (values #() nil)))
          (t
           (condition-wait (attachment-condition attachment)
                          (session-lock session))))))))

(defun set-attachment-input (attachment bytes)
  "Write raw BYTES from ATTACHMENT to its Session."
  (check-type attachment attachment)
  (unless (and (vectorp bytes)
               (every (lambda (byte)
                       (and (integerp byte) (<= 0 byte 255)))
                      bytes))
    (error "Input must be an octet vector."))
  (let ((session (attachment-session attachment))
        (input (copy-seq bytes)))
    (with-lock-held ((session-input-lock session))
      (with-lock-held ((session-lock session))
        (unless (and (session-running-under-lock-p session)
                     (managed-attachment-attached-p attachment))
          (return-from set-attachment-input nil)))
      (handler-case
          (with-lock-held ((session-write-lock session))
            (set-shell-input (managed-shell-session session) input)
            t)
        (error () nil)))))

;; Set the PTY size controlled by ATTACHMENT.
(defun set-attachment-terminal-size (attachment rows columns)
  "Set the PTY size controlled by ATTACHMENT."
  (check-type attachment attachment)
  (check-type rows (integer 1))
  (check-type columns (integer 1))
  (let ((session (attachment-session attachment)))
    (with-lock-held ((session-input-lock session))
      (with-lock-held ((session-lock session))
        (unless (and (session-running-under-lock-p session)
                     (managed-attachment-attached-p attachment)
                     (eq attachment (managed-session-full-screen-owner session)))
          (return-from set-attachment-terminal-size nil))
        (handler-case
            (with-lock-held ((session-write-lock session))
              (set-shell-size (managed-shell-session session) rows columns)
              (set-terminal-size (managed-terminal session) columns rows)
              t)
          (error () nil))))))

(defun del-managed-session (session)
  "Terminate SESSION and wait for its reader thread."
  (let ((thread (session-reader-thread session)))
    (with-lock-held ((session-lock session))
      (setf (managed-session-running-p session) nil))
    (del-managed-shell-session session)
    (when (and thread
               (not (eq thread (current-thread))))
      (ignore-errors (join-thread thread)))
    (set-session-terminated session)
    t))

;; Delete the named Session when it exists.
(defun del-session (name)
  "Terminate the Session named NAME when it exists."
  (get-valid-session-name name)
  (let ((manager (get-session-manager-value))
        (session nil))
    (when manager
      (with-lock-held ((manager-lock manager))
        (unless (manager-closed-p manager)
          (setf session (gethash name (manager-named-sessions manager)))))
      (when session
        (del-managed-session session)))
    session))

(defun del-managed-session-manager (manager)
  "Terminate every Session and stop the manager."
  (check-type manager session-manager)
  (let ((sessions nil))
    (with-lock-held ((manager-lock manager))
      (unless (manager-closed-p manager)
        (setf (manager-closed-p manager) t)
        (maphash (lambda (name session)
                   (declare (ignore name))
                   (push session sessions))
                 (manager-named-sessions manager))))
    (mapc #'del-managed-session sessions)
    (with-lock-held ((manager-lock manager))
      (clrhash (manager-named-sessions manager)))
    t))

;; Stop the manager and every named Session.
(defun del-session-manager ()
  "Terminate every Session and clear the process-global Manager."
  (let ((manager nil))
    (with-lock-held (*session-state-lock*)
      (setf manager *session-manager*
            *session-manager* nil
            *active-attachment* nil))
    (when manager
      (del-managed-session-manager manager))
    manager))
