(in-package #:mtm.app)

(defparameter +manager-server-argument+ "--manager-server")
(defparameter +manager-connect-attempts+ 100)
(defparameter +manager-connect-delay+ 0.05)
(defparameter +manager-socket-name+ "manager.sock")

(defun get-manager-socket-path ()
  "Return the per-user manager socket path."
  (merge-pathnames
   (format nil ".mtm/~A" +manager-socket-name+)
   (user-homedir-pathname)))

(defun get-manager-socket-fd (socket)
  "Return SOCKET's file descriptor on SBCL."
  #+sbcl
  (sb-bsd-sockets:socket-file-descriptor (usocket:socket socket))
  #-sbcl
  (declare (ignore socket))
  #-sbcl
  (error "The MTM manager requires SBCL."))

(defun set-protocol-line (fd line)
  "Write one ASCII protocol line to FD."
  (set-fd fd (get-utf8 (format nil "~A~%" line))))

(defun get-protocol-line (fd)
  "Read one ASCII protocol line from FD."
  (with-output-to-string (output)
    (loop
      (multiple-value-bind (bytes eof-p)
          (get-fd fd :max-bytes 1 :wait-p t)
        (when eof-p
          (return-from get-protocol-line nil))
        (let ((byte (aref bytes 0)))
          (cond
            ((= byte 10)
             (return))
            ((/= byte 13)
             (write-char (code-char byte) output))))))))

(defun del-manager-socket (socket)
  "Close SOCKET without masking an earlier condition."
  (when socket
    (ignore-errors (usocket:socket-close socket))))

(defun new-manager-connection ()
  "Connect to the running manager, or return NIL."
  (handler-case
      (let ((socket
              (usocket:socket-connect
               (get-manager-socket-path)
               nil
               :element-type '(unsigned-byte 8))))
        (handler-case
            (progn
              (set-socket-sigpipe (get-manager-socket-fd socket))
              (set-close-on-exec (get-manager-socket-fd socket))
              socket)
          (error (condition)
            (del-manager-socket socket)
            (error condition))))
    (error () nil)))

(defun manager-responding-p ()
  "Return true when the manager socket accepts a protocol request."
  (handler-case
      (let ((socket (new-manager-connection)))
        (when socket
          (unwind-protect
               (let ((fd (get-manager-socket-fd socket)))
                 (set-protocol-line fd "PING")
                 (string= "PONG" (get-protocol-line fd)))
            (del-manager-socket socket))))
    (error () nil)))

(defun get-manager-program ()
  "Return the standalone executable used to start the manager."
  (or (uiop:argv0)
      (error "The manager requires the standalone mtm executable.")))

(defun new-manager-process ()
  "Start the manager as a detached child process."
  (uiop:launch-program
   (list (get-manager-program) +manager-server-argument+)
   :input nil
   :output nil
   :error-output nil
   :wait nil))

(defun get-manager-startup-connection ()
  "Wait until the manager accepts connections."
  (loop repeat +manager-connect-attempts+
        for socket = (new-manager-connection)
        when socket
          do (return socket)
        do (sleep +manager-connect-delay+)
        finally (error "The MTM Session manager did not start.")))

(defun new-cli-session-manager ()
  "Start the Session manager and print its state."
  (when (manager-responding-p)
    (error "The Session manager is already running."))
  (new-manager-process)
  (let ((socket (get-manager-startup-connection)))
    (unwind-protect
         (format t "running~%web ~A~%" (get-browser-url))
      (del-manager-socket socket))))

(defun get-manager-connection ()
  "Return a connection to the running manager."
  (or (new-manager-connection)
      (error "The Session manager is stopped. Run mtm new session-manager.")))

(defun protocol-command-p (parts command count)
  "Return true when PARTS starts with COMMAND and has COUNT items."
  (and (= (length parts) count)
       (string= (first parts) command)))

(defun get-session-state-name (state)
  "Return STATE as lower-case protocol text."
  (string-downcase (symbol-name state)))

(defun set-value-response (fd path value)
  "Write one scalar value response to FD."
  (set-protocol-line fd (format nil "VALUE ~A ~A" path value)))

(defun set-session-list-response (fd)
  "Write the manager's named Session list to FD."
  (dolist (entry (get-session-list))
    (set-protocol-line
     fd
     (format nil "SESSION ~A ~A"
             (car entry)
             (get-session-state-name (cdr entry)))))
  (set-protocol-line fd "END"))

(defun new-session-request (fd parts)
  "Create the named Session requested by PARTS."
  (unless (protocol-command-p parts "NEW" 3)
    (error "The manager received an invalid NEW request."))
  (unless (string= (second parts) "SESSION")
    (error "NEW supports only the SESSION position."))
  (let ((session (new-session (third parts))))
    (set-value-response fd "SESSION" (session-name session))))

(defun get-session-request (fd parts)
  "Return the value requested by a GET operation."
  (unless (protocol-command-p parts "GET" 2)
    (error "The manager received an invalid GET request."))
  (let ((path (second parts)))
    (if (string= path "SESSION")
        (set-session-list-response fd)
        (error "GET does not support position ~A." path))))

(defun set-current-session-request (fd parts)
  "Enter the existing Session requested by PARTS."
  (unless (and (member (length parts) '(3 4))
               (string= (first parts) "SET"))
    (error "The manager received an invalid SET request."))
  (unless (string= (second parts) "CURRENT-SESSION")
    (error "SET does not support position ~A." (second parts)))
  (let ((application-p
          (cond
            ((= (length parts) 3) nil)
            ((string= (fourth parts) "APPLICATION") t)
            (t (error "SET CURRENT-SESSION accepts only APPLICATION."))))
        (attachment nil))
    (setf attachment
          (new-attachment (third parts) :application-p application-p))
    (handler-case
        (progn
          (set-protocol-line
           fd
           (format nil "READY ~A"
                   (if (session-application-p
                        (attachment-session attachment))
                       "APPLICATION"
                       "EDITOR")))
          ;; The frontend speaks directly through the socket descriptor.
          (set-passthrough-frontend
           :attachment attachment
           :input-fd fd
           :output-fd fd
           :socket-control-p t))
      (error (condition)
        ;; A failed handoff must not leave a stale Attachment.
        (ignore-errors (del-attachment attachment))
        (error condition)))))

(defun del-session-request (fd parts)
  "Delete the named Session requested by PARTS."
  (unless (protocol-command-p parts "DEL" 3)
    (error "The manager received an invalid DEL request."))
  (unless (string= (second parts) "SESSION")
    (error "DEL cannot remove position ~A." (second parts)))
  (let ((session (del-session (third parts))))
    (set-value-response fd "SESSION" (session-name session))))

(defun del-session-manager-request (fd parts stop-function)
  "Stop the manager requested by PARTS and close its listener."
  (unless (protocol-command-p parts "DEL" 2)
    (error "The manager received an invalid DEL request."))
  (unless (string= (second parts) "SESSION-MANAGER")
    (error "DEL cannot remove position ~A." (second parts)))
  (unwind-protect
       (progn
         (del-session-manager)
         (set-value-response fd "SESSION-MANAGER" "stopped"))
    ;; Closing the listener wakes the accept loop after the response.
    (when stop-function
      (funcall stop-function))))

(defun set-manager-error-response (fd line condition)
  "Write CONDITION as an error response for LINE."
  (declare (ignore line))
  (ignore-errors
    (set-protocol-line fd (format nil "ERROR ~A" condition))))

(defun set-manager-client (socket &optional stop-function)
  "Handle one manager protocol connection."
  (let ((fd (get-manager-socket-fd socket)))
    (set-socket-sigpipe fd)
    (unwind-protect
         (let ((line (get-protocol-line fd)))
           (when line
             (let ((parts (uiop:split-string line)))
               (cond
                 ((protocol-command-p parts "PING" 1)
                  (set-protocol-line fd "PONG"))
                 ((and (protocol-command-p parts "NEW" 3)
                       (string= (second parts) "SESSION"))
                  (handler-case
                      (new-session-request fd parts)
                    (error (condition)
                      (set-manager-error-response fd line condition))))
                 ((protocol-command-p parts "GET" 2)
                  (handler-case
                      (get-session-request fd parts)
                    (error (condition)
                      (set-manager-error-response fd line condition))))
                 ((and (member (length parts) '(3 4))
                       (string= (first parts) "SET")
                       (string= (second parts) "CURRENT-SESSION"))
                  (handler-case
                      (set-current-session-request fd parts)
                    (error (condition)
                      (set-manager-error-response fd line condition))))
                 ((and (protocol-command-p parts "DEL" 3)
                       (string= (second parts) "SESSION"))
                  (handler-case
                      (del-session-request fd parts)
                    (error (condition)
                      (set-manager-error-response fd line condition))))
                 ((and (protocol-command-p parts "DEL" 2)
                       (string= (second parts) "SESSION-MANAGER"))
                  (handler-case
                      (del-session-manager-request
                       fd parts stop-function)
                    (error (condition)
                      (set-manager-error-response fd line condition))))
                 (t
                  (set-manager-error-response
                   fd line
                   (make-condition 'simple-error
                                   :format-control "Invalid manager request."
                                   :format-arguments nil)))))))
      (del-manager-socket socket))))

(defun new-manager-listener ()
  "Start the manager listener, or return NIL for a live manager."
  (let ((path (get-manager-socket-path)))
    (ensure-directories-exist path)
    (loop repeat +manager-connect-attempts+
          do (handler-case
                 (return
                   (let ((listener
                           (usocket:socket-listen
                            path
                            nil
                            :element-type '(unsigned-byte 8)
                            :backlog 16)))
                     (handler-case
                         (progn
                           (set-close-on-exec (get-manager-socket-fd listener))
                           listener)
                       (error (condition)
                         (del-manager-socket listener)
                         (error condition)))))
               (error ()
                 (when (manager-responding-p)
                   (return nil))
                 (ignore-errors (delete-file path))
                 (sleep +manager-connect-delay+)))
          finally (error "The manager socket cannot be opened."))))

(defun set-manager-accept-loop (listener)
  "Accept manager protocol connections forever."
  (let ((stopping-p nil))
    (labels ((del-manager-listener ()
               (setf stopping-p t)
               ;; Closing the listener interrupts the blocking accept call.
               (del-manager-socket listener)))
      (loop
        while (not stopping-p)
        do (handler-case
               (let ((client (usocket:socket-accept listener)))
                 (when client
                   (handler-case
                       (progn
                         (set-close-on-exec (get-manager-socket-fd client))
                         (make-thread
                          (lambda ()
                            (handler-case
                                (set-manager-client client #'del-manager-listener)
                              (error ()
                                (del-manager-socket client))))
                          :name "mtm manager client"))
                     (error ()
                       (del-manager-socket client)))))
             (error (condition)
               (unless stopping-p
                 (error condition))))))))

(defun set-manager-server ()
  "Run the background Session manager process."
  (let ((listener (new-manager-listener)))
    (when listener
      (new-session-manager)
      (let ((browser-server nil))
        (unwind-protect
             (progn
               (setf browser-server (new-browser-server))
               (set-manager-accept-loop listener))
          (del-manager-socket listener)
          (ignore-errors (delete-file (get-manager-socket-path)))
          (when (get-session-manager)
            (del-session-manager))
          (when browser-server
            (del-browser-server browser-server)))))))

(defun get-cli-value-response (socket path)
  "Read and validate one scalar value response from SOCKET."
  (let ((line (get-protocol-line (get-manager-socket-fd socket))))
    (cond
      ((null line)
       (error "The manager closed the response connection."))
      ((uiop:string-prefix-p "ERROR " line)
       (error "~A" (subseq line (length "ERROR "))))
      (t
       (let ((parts (uiop:split-string line)))
         (unless (and (= (length parts) 3)
                      (string= (first parts) "VALUE")
                      (string= (second parts) path))
           (error "The manager returned an invalid value."))
         (third parts))))))

(defun new-cli-session (name)
  "Create NAME through the manager and print its value."
  (let* ((socket (get-manager-connection))
         (fd (get-manager-socket-fd socket)))
    (unwind-protect
         (progn
           (set-protocol-line fd (format nil "NEW SESSION ~A" name))
           (format t "~A~%" (get-cli-value-response socket "SESSION")))
      (del-manager-socket socket))))

(defun get-cli-session-manager ()
  "Print the Session manager state."
  (format t "~A~%"
          (if (manager-responding-p)
              "running"
              "stopped")))

(defun get-cli-session-list-values ()
  "Return the manager's named Session list."
  (let* ((socket (get-manager-connection))
         (fd (get-manager-socket-fd socket))
         (sessions nil))
    (unwind-protect
         (progn
           (set-protocol-line fd "GET SESSION")
           (loop for line = (get-protocol-line fd)
                 do (when (null line)
                      (error "The manager closed the Session list connection."))
                    (cond
                      ((string= line "END")
                       (return (nreverse sessions)))
                      ((uiop:string-prefix-p "SESSION " line)
                       (let ((parts (uiop:split-string line)))
                         (unless (= (length parts) 3)
                           (error "The manager returned an invalid Session."))
                         (push (cons (second parts) (third parts))
                               sessions)))
                      ((uiop:string-prefix-p "ERROR " line)
                       (error "~A" (subseq line (length "ERROR "))))
                      (t
                       (error "The manager returned an invalid Session list.")))))
      (del-manager-socket socket))))

(defun get-cli-session-list ()
  "Print the manager's named Session list."
  (let ((sessions (get-cli-session-list-values)))
    (if sessions
        (dolist (session sessions)
          (format t "~A~C~A~%"
                  (car session)
                  #\Tab
                  (cdr session)))
        (format t "No sessions.~%"))))

(defun set-cli-current-session (name &key application-p)
  "Enter NAME and return its manager connection."
  (let* ((socket (get-manager-connection))
         (fd (get-manager-socket-fd socket)))
    (handler-case
        (progn
          (set-protocol-line
           fd
           (format nil "SET CURRENT-SESSION ~A~A"
                   name
                   (if application-p " APPLICATION" "")))
          (let* ((response (get-protocol-line fd))
                 (parts (and response (uiop:split-string response))))
            (cond
              ((and parts
                    (= (length parts) 2)
                    (string= (first parts) "READY")
                    (member (second parts)
                            '("APPLICATION" "EDITOR")
                            :test #'string=))
               (values socket
                       (string= (second parts) "APPLICATION")))
              ((string= response "READY")
               (values socket nil))
              ((and response (uiop:string-prefix-p "ERROR " response))
               (error "~A" (subseq response (length "ERROR "))))
              (t
               (error "The manager rejected the current-session request.")))))
      (error (condition)
        (del-manager-socket socket)
        (error condition)))))

(defun set-cli-current-session-frontend (name &key application-p)
  "Enter NAME through the local terminal."
  (multiple-value-bind (socket session-application-p)
      (set-cli-current-session name :application-p application-p)
    (set-client-frontend socket name
                         :application-p session-application-p)))

(defun del-cli-session-manager ()
  "Stop the Session manager and print its state."
  (let* ((socket (get-manager-connection))
         (fd (get-manager-socket-fd socket)))
    (unwind-protect
         (progn
           (set-protocol-line fd "DEL SESSION-MANAGER")
           (format t "~A~%"
                   (get-cli-value-response socket "SESSION-MANAGER")))
      (del-manager-socket socket))))

(defun del-cli-session (name)
  "Delete NAME through the manager and print its value."
  (let* ((socket (get-manager-connection))
         (fd (get-manager-socket-fd socket)))
    (unwind-protect
         (progn
           (set-protocol-line fd (format nil "DEL SESSION ~A" name))
           (format t "~A~%" (get-cli-value-response socket "SESSION")))
      (del-manager-socket socket))))

(defun set-client-frontend (socket name &key application-p)
  "Run the local frontend around manager SOCKET."
  (let ((current socket)
        (fd (get-manager-socket-fd socket)))
    (set-socket-sigpipe fd)
    (flet ((set-cli-session (frontend new-name)
             (unless (string= new-name (session-frontend-name frontend))
               (multiple-value-bind (new-socket new-application-p)
                   (set-cli-current-session new-name)
                 (let ((new-fd (get-manager-socket-fd new-socket)))
                   (set-socket-sigpipe new-fd)
                   (del-manager-socket current)
                   (setf current new-socket
                         (session-frontend-socket-fd frontend) new-fd
                         (session-frontend-name frontend) new-name)
                   (set-frontend-application-mode frontend new-application-p))))))
      (unwind-protect
           (set-socket-frontend
            fd
            name
            #'get-cli-session-list-values
            #'set-cli-session
            :application-p application-p)
        (del-manager-socket current)))))
