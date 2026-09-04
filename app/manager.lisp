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

;; Return the text after PREFIX in one protocol line.
(defun get-protocol-tail (line prefix)
  "Return the text after PREFIX in one protocol line."
  (when (uiop:string-prefix-p prefix line)
    (string-trim " " (subseq line (length prefix)))))

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

;; Start the detached Session manager when no manager responds.
(defun new-cli-session-manager-process ()
  "Start the detached Session manager when it is absent."
  (unless (manager-responding-p)
    (new-manager-process)
    ;; Close the readiness connection after startup succeeds.
    (let ((socket (get-manager-startup-connection)))
      (del-manager-socket socket))))

;; Ensure the detached Session manager and print its snapshot.
(defun new-cli-session-manager ()
  "Ensure the detached Session manager and print its snapshot."
  (new-cli-session-manager-process)
  (get-cli-session-manager))

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

;; Send the manager snapshot through the protocol.
(defun set-session-manager-response (fd)
  "Write the Manager state, Sessions, and Services to FD."
  (let ((snapshot (get-session-manager)))
    (set-protocol-line
     fd
     (format nil "STATE ~A"
             (get-session-state-name (getf snapshot :state))))
    (dolist (entry (getf snapshot :sessions))
      (set-protocol-line
       fd
       (format nil "SESSION ~A ~A"
               (car entry)
               (get-session-state-name (cdr entry)))))
    (dolist (entry (get-service-list))
      (set-protocol-line
       fd
       (format nil "SERVICE ~A ~A"
               (car entry)
               (get-session-state-name (cdr entry)))))
    (set-protocol-line fd "END")))

;; Load one Service source requested by a client.
(defun new-service-request (fd path)
  "Load one Service source requested by a client."
  (unless (and path (plusp (length path)))
    (error "The manager received an empty Service source path."))
  (new-service-source path)
  (set-value-response fd "SERVICE" "loaded"))

;; Return one Service snapshot through the manager protocol.
(defun set-service-response (fd snapshot)
  "Write one Service snapshot to FD."
  (set-protocol-line
   fd
   (format nil "SERVICE ~A ~A ~A ~A"
           (getf snapshot :name)
           (get-session-state-name (getf snapshot :desired-state))
           (get-session-state-name (getf snapshot :state))
           (or (getf snapshot :pid) "nil"))))

;; Handle one Service state query.
(defun get-service-request (fd parts)
  "Return the Service requested by PARTS."
  (unless (protocol-command-p parts "GET" 3)
    (error "The manager received an invalid Service GET request."))
  (unless (string= (second parts) "SERVICE")
    (error "GET requires a Service name."))
  (set-service-response fd (get-service (third parts))))

;; Keep a read-only Service log connection until the client closes it.
(defun set-service-log-client (fd)
  "Keep a read-only Service log connection until the client closes it."
  (loop
    (multiple-value-bind (bytes eof-p)
        (get-fd fd :max-bytes 1 :wait-p t)
      (declare (ignore bytes))
      (when eof-p
        (return)))))

;; Send recent Service output and hold its read-only connection.
(defun get-service-log-request (fd parts)
  "Send recent Service output and hold its read-only connection."
  (unless (protocol-command-p parts "GET" 3)
    (error "The manager received an invalid Service log request."))
  (unless (string= (second parts) "SERVICE-LOG")
    (error "GET requires a Service log name."))
  (set-protocol-line fd "READY SERVICE")
  (let ((output (get-service-output (third parts))))
    (when (plusp (length output))
      (set-fd fd output)))
  (set-service-log-client fd))

;; Convert protocol text into one supported Service state.
(defun get-service-state-value (text)
  "Convert protocol TEXT into one supported Service state."
  (let ((state (intern (string-upcase text) "KEYWORD")))
    (unless (member state '(:running :stopped) :test #'eq)
      (error "Service state must be RUNNING or STOPPED."))
    state))

;; Change one Service state requested by a client.
(defun set-service-request (fd parts)
  "Change one Service state requested by PARTS."
  (unless (protocol-command-p parts "SET" 4)
    (error "The manager received an invalid Service SET request."))
  (unless (string= (second parts) "SERVICE")
    (error "SET requires a Service name."))
  (set-service-response
   fd
   (set-service (third parts)
                (get-service-state-value (fourth parts)))))

;; Delete one Service requested by a client.
(defun del-service-request (fd parts)
  "Delete one Service requested by PARTS."
  (unless (protocol-command-p parts "DEL" 3)
    (error "The manager received an invalid Service DEL request."))
  (unless (string= (second parts) "SERVICE")
    (error "DEL requires a Service name."))
  (let ((snapshot (del-service (third parts))))
    (set-value-response fd "SERVICE" (or (getf snapshot :name)
                                          (third parts)))))

;; Ensure the named Session requested by one client.
(defun new-session-request (fd parts)
  "Ensure the named Session requested by PARTS."
  (unless (protocol-command-p parts "NEW" 3)
    (error "The manager received an invalid NEW request."))
  (unless (string= (second parts) "SESSION")
    (error "NEW supports only the SESSION position."))
  (let ((session (new-session-value (third parts))))
    (set-value-response fd "SESSION" (session-name session))))

;; Handle named Session and manager GET requests.
(defun get-session-request (fd parts)
  "Return the value requested by a GET operation."
  (cond
    ((protocol-command-p parts "GET" 2)
     (unless (string= (second parts) "SESSION-MANAGER")
       (error "GET requires a Session manager name."))
     (set-session-manager-response fd))
    ((protocol-command-p parts "GET" 3)
     (unless (string= (second parts) "SESSION")
       (error "GET requires a Session name."))
     (get-session-attachment-request fd (third parts)))
    (t
     (error "The manager received an invalid GET request."))))

;; Enter the named Session through the manager connection.
(defun get-session-attachment-request (fd name)
  "Enter the named Session through FD."
  ;; Create the Attachment before transferring the socket.
  (let* ((attachment (new-attachment name))
         ;; Tell the client which input transport to use.
         (full-screen-p
           (session-full-screen-p (attachment-session attachment))))
    (handler-case
        (progn
          (set-protocol-line
           fd
           (format nil "READY ~A"
                   (if full-screen-p
                       "FULL-SCREEN"
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

;; Handle deletion of one named Session.
(defun del-session-request (fd parts)
  "Delete the named Session requested by PARTS."
  (unless (protocol-command-p parts "DEL" 3)
    (error "The manager received an invalid DEL request."))
  (unless (string= (second parts) "SESSION")
    (error "DEL cannot remove position ~A." (second parts)))
  (let ((name (third parts)))
    (del-session name)
    (set-value-response fd "SESSION" name)))

;; Handle deletion of the manager and every named Session.
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

(defun set-manager-error-response (fd condition)
  "Write CONDITION as an error response."
  (ignore-errors
    (set-protocol-line fd (format nil "ERROR ~A" condition))))

;; Handle one request from a manager client.
(defun set-manager-client (socket &optional stop-function)
  "Handle one manager protocol connection."
  (let ((fd (get-manager-socket-fd socket)))
    (set-socket-sigpipe fd)
    (unwind-protect
         (handler-case
             (let ((line (get-protocol-line fd)))
               (when line
                 (let* ((parts (uiop:split-string line))
                        ;; Preserve source paths that contain spaces.
                        (service-path
                          (get-protocol-tail line "NEW SERVICE ")))
                   (cond
                     ((protocol-command-p parts "PING" 1)
                      (set-protocol-line fd "PONG"))
                     (service-path
                      (new-service-request fd service-path))
                     ((protocol-command-p parts "NEW" 3)
                      (new-session-request fd parts))
                     ((and (protocol-command-p parts "GET" 3)
                           (string= (second parts) "SERVICE-LOG"))
                      (get-service-log-request fd parts))
                     ((and (protocol-command-p parts "GET" 3)
                           (string= (second parts) "SERVICE"))
                      (get-service-request fd parts))
                     ((or (protocol-command-p parts "GET" 2)
                          (protocol-command-p parts "GET" 3))
                      (get-session-request fd parts))
                     ((and (protocol-command-p parts "SET" 4)
                           (string= (second parts) "SERVICE"))
                      (set-service-request fd parts))
                     ((and (protocol-command-p parts "DEL" 3)
                           (string= (second parts) "SERVICE"))
                      (del-service-request fd parts))
                     ((and (protocol-command-p parts "DEL" 3)
                           (string= (second parts) "SESSION"))
                      (del-session-request fd parts))
                     ((and (protocol-command-p parts "DEL" 2)
                           (string= (second parts) "SESSION-MANAGER"))
                      (del-session-manager-request fd parts stop-function))
                     (t
                      (set-manager-error-response
                       fd
                       (make-condition 'simple-error
                                       :format-control
                                       "Invalid manager request."
                                       :format-arguments nil)))))))
           (error (condition)
             (set-manager-error-response fd condition)))
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

;; Run the detached Session manager and Browser server.
(defun set-manager-server ()
  "Run the background Session manager process."
  (let ((listener (new-manager-listener)))
    (when listener
      (new-session-manager)
      (set-prompt-config)
      (let ((browser-server nil))
        (unwind-protect
             (progn
               (setf browser-server (new-browser-server))
               (set-manager-accept-loop listener))
          (del-manager-socket listener)
          (ignore-errors (delete-file (get-manager-socket-path)))
          (when (get-session-manager-value)
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

;; Ensure NAME exists and enter it through the local terminal.
(defun new-cli-session (name)
  "Ensure NAME exists, then enter it through the local terminal."
  (new-cli-session-manager-process)
  (let* ((socket (get-manager-connection))
         (fd (get-manager-socket-fd socket)))
    (unwind-protect
         (progn
           (set-protocol-line fd (format nil "NEW SESSION ~A" name))
           (get-cli-value-response socket "SESSION"))
      (del-manager-socket socket)))
  (get-cli-session-frontend name))

;; Read the Manager snapshot returned by SOCKET.
(defun get-cli-session-manager-values (socket)
  "Return the Manager state, Sessions, and Services from SOCKET."
  ;; Collect the manager state and its named rows.
  (let ((state nil)
        (sessions nil)
        (services nil))
    (loop for line = (get-protocol-line (get-manager-socket-fd socket))
          do (when (null line)
               (error "The manager closed the state connection."))
             (cond
               ((uiop:string-prefix-p "STATE " line)
                (setf state (subseq line (length "STATE "))))
               ((string= line "END")
                (return (values state
                                (nreverse sessions)
                                (nreverse services))))
               ((uiop:string-prefix-p "SESSION " line)
                (let ((parts (uiop:split-string line)))
                  (unless (= (length parts) 3)
                    (error "The manager returned an invalid Session."))
                  (push (cons (second parts) (third parts)) sessions)))
               ((uiop:string-prefix-p "SERVICE " line)
                (let ((parts (uiop:split-string line)))
                  (unless (= (length parts) 3)
                    (error "The manager returned an invalid Service."))
                  (push (cons (second parts) (third parts)) services)))
               ((uiop:string-prefix-p "ERROR " line)
                (error "~A" (subseq line (length "ERROR "))))
               (t
                (error "The manager returned an invalid state."))))))

;; Print the manager snapshot for the CLI.
(defun get-cli-session-manager ()
  "Print the Manager state, Sessions, and Services."
  (if (not (manager-responding-p))
      (format t "state stopped~%")
      (let ((socket (get-manager-connection)))
        (unwind-protect
             (progn
               (set-protocol-line
                (get-manager-socket-fd socket)
                "GET SESSION-MANAGER")
               (multiple-value-bind (state sessions services)
                   (get-cli-session-manager-values socket)
                 (format t "state ~A~%" state)
                 (dolist (session sessions)
                   (format t "session ~A ~A~%"
                           (car session)
                           (cdr session)))
                 (dolist (service services)
                   (format t "service ~A ~A~%"
                           (car service)
                           (cdr service)))))
          (del-manager-socket socket)))))

;; Open a named Session through the manager.
(defun get-cli-session (name)
  "Enter NAME and return its manager connection."
  (let* ((socket (get-manager-connection))
         (fd (get-manager-socket-fd socket)))
    (handler-case
        (progn
          (set-protocol-line fd (format nil "GET SESSION ~A" name))
          (let* ((response (get-protocol-line fd))
                 (parts (and response (uiop:split-string response))))
            (cond
              ((and parts
                    (= (length parts) 2)
                    (string= (first parts) "READY")
                    (member (second parts)
                            '("FULL-SCREEN" "EDITOR")
                            :test #'string=))
               (values socket
                       (string= (second parts) "FULL-SCREEN")))
              ((and response (uiop:string-prefix-p "ERROR " response))
               (error "~A" (subseq response (length "ERROR "))))
              (t
               (error "The manager rejected the Session request.")))))
      (error (condition)
        (del-manager-socket socket)
        (error condition)))))

;; Run the local terminal frontend for NAME.
(defun get-cli-session-frontend (name)
  "Enter NAME through the local terminal."
  (multiple-value-bind (socket full-screen-p)
      (get-cli-session name)
    (set-client-frontend socket name :full-screen-p full-screen-p)))

;; Load one Service source through the manager.
(defun new-cli-service-source (path)
  "Load one Service source through the manager."
  (new-cli-session-manager-process)
  (let* ((socket (get-manager-connection))
         (fd (get-manager-socket-fd socket)))
    (unwind-protect
         (progn
           (set-protocol-line fd (format nil "NEW SERVICE ~A" path))
           (format t "~A~%" (get-cli-value-response socket "SERVICE")))
      (del-manager-socket socket))))

;; Open recent output for one named Service.
(defun get-cli-service-log (name)
  "Open recent output for one named Service."
  (let* ((socket (get-manager-connection))
         (fd (get-manager-socket-fd socket)))
    (handler-case
        (progn
          (set-protocol-line fd (format nil "GET SERVICE-LOG ~A" name))
          (let* ((response (get-protocol-line fd))
                 (parts (and response (uiop:split-string response))))
            (if (and parts
                     (= (length parts) 2)
                     (string= (first parts) "READY")
                     (string= (second parts) "SERVICE"))
                socket
                (if (and response (uiop:string-prefix-p "ERROR " response))
                    (error "~A" (subseq response (length "ERROR ")))
                    (error "The manager rejected the Service log request.")))))
      (error (condition)
        (del-manager-socket socket)
        (error condition)))))

;; Read one Service state response from SOCKET.
(defun get-cli-service-response (socket name)
  "Return one Service state response from SOCKET."
  (let ((line (get-protocol-line (get-manager-socket-fd socket))))
    (cond
      ((null line)
       (error "The manager closed the Service response connection."))
      ((uiop:string-prefix-p "ERROR " line)
       (error "~A" (subseq line (length "ERROR "))))
      (t
       (let ((parts (uiop:split-string line)))
         (unless (and (= (length parts) 5)
                      (string= (first parts) "SERVICE")
                      (string= (second parts) name))
           (error "The manager returned an invalid Service."))
         (list :name (second parts)
               :desired-state (third parts)
               :state (fourth parts)
               :pid (fifth parts)))))))

;; Print one Service snapshot.
(defun set-cli-service-output (snapshot)
  "Print one Service snapshot."
  (format t "service ~A desired ~A state ~A pid ~A~%"
          (getf snapshot :name)
          (getf snapshot :desired-state)
          (getf snapshot :state)
          (getf snapshot :pid)))

;; Print one named Service state through the manager.
(defun get-cli-service (name)
  "Print one named Service state through the manager."
  (let* ((socket (get-manager-connection))
         (fd (get-manager-socket-fd socket)))
    (unwind-protect
         (progn
           (set-protocol-line fd (format nil "GET SERVICE ~A" name))
           (set-cli-service-output
            (get-cli-service-response socket name)))
      (del-manager-socket socket))))

;; Change one named Service state through the manager.
(defun set-cli-service (name state)
  "Change one named Service state through the manager."
  (let* ((socket (get-manager-connection))
         (fd (get-manager-socket-fd socket)))
    (unwind-protect
         (progn
           (set-protocol-line fd (format nil "SET SERVICE ~A ~A" name state))
           (set-cli-service-output
            (get-cli-service-response socket name)))
      (del-manager-socket socket))))

;; Delete one named Service through the manager.
(defun del-cli-service (name)
  "Delete one named Service through the manager."
  (if (not (manager-responding-p))
      (format t "~A~%" name)
      (let* ((socket (get-manager-connection))
             (fd (get-manager-socket-fd socket)))
        (unwind-protect
             (progn
               (set-protocol-line fd (format nil "DEL SERVICE ~A" name))
               (format t "~A~%" (get-cli-value-response socket "SERVICE")))
          (del-manager-socket socket)))))

;; Stop the manager and print the stopped state.
(defun del-cli-session-manager ()
  "Stop the Session manager and print its state."
  (if (not (manager-responding-p))
      (format t "state stopped~%")
      (let* ((socket (get-manager-connection))
             (fd (get-manager-socket-fd socket)))
        (unwind-protect
             (progn
               (set-protocol-line fd "DEL SESSION-MANAGER")
               (get-cli-value-response socket "SESSION-MANAGER")
               (format t "state stopped~%"))
          (del-manager-socket socket)))))

;; Delete NAME through the manager.
(defun del-cli-session (name)
  "Delete NAME through the manager and print its value."
  (if (not (manager-responding-p))
      (format t "~A~%" name)
      ;; Keep the client connection private to this deletion.
      (let* ((socket (get-manager-connection))
             (fd (get-manager-socket-fd socket)))
        (unwind-protect
             (progn
               (set-protocol-line fd (format nil "DEL SESSION ~A" name))
               (format t "~A~%" (get-cli-value-response socket "SESSION")))
          (del-manager-socket socket)))))

;; Run the local frontend around manager SOCKET.
(defun set-client-frontend
    (socket name &key (full-screen-p nil))
  "Run the local frontend around manager SOCKET."
  (let ((fd (get-manager-socket-fd socket)))
    (set-socket-sigpipe fd)
    (unwind-protect
         (set-socket-frontend
          fd
          name
          :full-screen-p full-screen-p)
      (del-manager-socket socket))))
