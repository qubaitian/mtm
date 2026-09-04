(in-package #:mtm.pty)

;; Store one PTY master and its child process.
(defclass pty-process ()
  ((master
    ;; Store the PTY master file descriptor.
    :initarg :master
    :reader pty-master)
   (process-id
    ;; Store the direct child process identifier.
    :initarg :process-id
    :reader session-process-id)
   (open-p
    ;; Track whether the PTY still accepts I/O.
    :initform t
    :accessor session-open-p)))

;; Represent one interactive shell inside a PTY.
(defclass shell-session (pty-process) ())

;; Represent one arbitrary program inside a PTY.
(defclass process-session (pty-process) ())

;; Return true when SHELL names the supported zsh adapter target.
;; ponytail: adapt zsh first; add other shells when required.
(defun zsh-shell-p (shell)
  (string-equal "zsh" (pathname-name (pathname shell))))

;; Return the directory used for MTM's zsh startup files.
(defun get-zsh-adapter-directory ()
  (merge-pathnames ".mtm/zsh/" (user-homedir-pathname)))

;; Store one zsh adapter startup file.
(defun set-zsh-adapter-file (path contents)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string contents stream)))

;; Create the zsh files that preserve user startup configuration.
(defun set-zsh-adapter ()
  (let (;; Keep the adapter directory path for both startup files.
        (directory (get-zsh-adapter-directory)))
    (ensure-directories-exist (merge-pathnames ".zshenv" directory))
    (set-zsh-adapter-file
     (merge-pathnames ".zshenv" directory)
     (format nil
             "setopt rcs~%if [[ -n \"$MTM_ORIGINAL_ZDOTDIR\" && -r \"$MTM_ORIGINAL_ZDOTDIR/.zshenv\" ]]; then~%  ZDOTDIR=\"$MTM_ORIGINAL_ZDOTDIR\"~%  source \"$ZDOTDIR/.zshenv\"~%fi~%setopt rcs~%ZDOTDIR=\"$MTM_ADAPTER_ZDOTDIR\"~%"))
    (set-zsh-adapter-file
     (merge-pathnames ".zshrc" directory)
     (format nil
             "setopt rcs~%if [[ -n \"$MTM_ORIGINAL_ZDOTDIR\" && -r \"$MTM_ORIGINAL_ZDOTDIR/.zshrc\" ]]; then~%  ZDOTDIR=\"$MTM_ORIGINAL_ZDOTDIR\"~%  source \"$ZDOTDIR/.zshrc\"~%fi~%PROMPT=''~%PS1=''~%RPROMPT=''~%RPS1=''~%PS2=''~%PS3=''~%PS4=''~%PROMPT_EOL_MARK=''~%unsetopt prompt_sp~%ZDOTDIR=\"$MTM_ORIGINAL_ZDOTDIR\"~%unset MTM_ORIGINAL_ZDOTDIR MTM_ADAPTER_ZDOTDIR~%"))
    directory))

;; Return environment values for the zsh Prompt adapter.
(defun get-zsh-adapter-environment ()
  (let* (;; Preserve the user's original zsh startup directory.
         (original-directory
           (or (uiop:getenv "ZDOTDIR")
               (namestring (user-homedir-pathname))))
         ;; Point zsh at the MTM startup wrapper.
         (adapter-directory (namestring (set-zsh-adapter))))
    (list (cons "ZDOTDIR" adapter-directory)
          (cons "MTM_ORIGINAL_ZDOTDIR" original-directory)
          (cons "MTM_ADAPTER_ZDOTDIR" adapter-directory))))

;; Return the shell selected by the environment.
(defun get-shell ()
  "Return the shell selected by the environment."
  (or (uiop:getenv "SHELL") "/bin/sh"))

;; Start one selected shell inside a PTY.
(defun new-shell-session (&key (shell (get-shell))
                               (width 80)
                               (height 24)
                               (prompt-adapter-p nil))
  "Start the selected shell inside a PTY."
  (multiple-value-bind (master process-id)
      (new-pty shell
               width
               height
               :environment
               (when (and prompt-adapter-p (zsh-shell-p shell))
                 (handler-case
                     (get-zsh-adapter-environment)
                   (error () nil))))
    (make-instance 'shell-session
                   :master master
                   :process-id process-id)))

;; Start PROGRAM inside a fixed-size PTY.
(defun new-process-session (&key program working-directory (width 80) (height 24))
  "Start PROGRAM inside a fixed-size PTY."
  (multiple-value-bind (master process-id)
      (new-pty-process program width height
                       :working-directory working-directory)
    (make-instance 'process-session
                   :master master
                   :process-id process-id)))

;; Return PROCESS's child process identifier.
(defun get-process-id (process)
  "Return PROCESS's child process identifier."
  (check-type process pty-process)
  (session-process-id process))

(defun get-open-shell-session (session)
  "Signal an error when SESSION no longer accepts I/O."
  (unless (session-open-p session)
    (error "The shell session is closed."))
  session)

(defun get-shell-output-bytes (session &key (max-bytes 4096) (wait-p t))
  "Read PTY bytes and return bytes plus an end-of-file flag."
  (get-open-shell-session session)
  (get-fd (pty-master session) :max-bytes max-bytes :wait-p wait-p))

;; Read raw PTY bytes from PROCESS.
(defun get-process-output-bytes (process &key (max-bytes 4096) (wait-p t))
  "Read raw PTY bytes from PROCESS."
  (unless (session-open-p process)
    (error "The PTY process is closed."))
  (get-fd (pty-master process) :max-bytes max-bytes :wait-p wait-p))

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

;; Stop PROCESS gracefully, then close and reap its PTY child.
(defun del-process-session (process &key (terminate-p t))
  "Stop PROCESS gracefully, then close and reap its PTY child."
  (check-type process process-session)
  (when (session-open-p process)
    (setf (session-open-p process) nil)
    (let ((status nil)
          (process-id (get-process-id process)))
      (when terminate-p
        (set-process-signal process-id 15)
        (loop repeat 200
              do (setf status
                       (get-process-status process-id :no-hang-p t))
              when status
                do (return)
              do (sleep 0.01))
        (unless status
          (set-process-signal process-id 9)))
      (ignore-errors (del-pty (pty-master process)))
      (unless status
        (ignore-errors (get-process-status process-id)))))
  t)
