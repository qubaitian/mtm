(defpackage #:mtm.client
  (:use #:cl)
  (:import-from #:mtm.platform
                #:get-fd
                #:new-detached-process
                #:set-close-on-exec
                #:set-fd
                #:set-socket-sigpipe)
  (:import-from #:mtm.utf8
                #:get-utf8)
  (:import-from #:mtm.frontend
                #:set-socket-frontend)
  (:export
   #:del-attachment
   #:del-manager-socket
   #:del-service
   #:del-session
   #:del-session-manager
   #:get-manager-socket-fd
   #:get-manager-socket-path
   #:get-protocol-line
   #:get-protocol-tail
   #:get-service
   #:get-service-list
   #:get-service-output
   #:get-session
   #:get-session-list
   #:get-session-manager
   #:manager-responding-p
   #:new-manager-connection
   #:new-service-source
   #:new-session
   #:new-session-manager
   #:set-protocol-line
   #:set-service))

(in-package #:mtm.client)

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

(defun new-manager-process ()
  "Start the manager as a detached child process."
  (let ((symbol (find-symbol "SET-MANAGER-SERVER" "MTM.MANAGER")))
    (unless (and symbol (fboundp symbol))
      (error "The manager server is not loaded."))
    (new-detached-process (symbol-function symbol))))

(defun get-manager-startup-connection ()
  "Wait until the manager accepts connections."
  (loop repeat +manager-connect-attempts+
        for socket = (new-manager-connection)
        when socket
          do (return socket)
        do (sleep +manager-connect-delay+)
        finally (error "The MTM Session manager did not start.")))

(defun new-session-manager ()
  "Ensure the detached Session manager and return its snapshot."
  (unless (manager-responding-p)
    (new-manager-process)
    (let ((socket (get-manager-startup-connection)))
      (del-manager-socket socket)))
  (get-session-manager))

(defun get-manager-connection ()
  "Return a connection to the running manager."
  (or (new-manager-connection)
      (error "The Session manager is stopped. Run mtm new session-manager.")))

(defun get-protocol-error (line)
  "Signal LINE when it is an ERROR response."
  (when (and line (uiop:string-prefix-p "ERROR " line))
    (error "~A" (subseq line (length "ERROR ")))))

(defun get-value-response (socket path)
  "Read and validate one scalar value response from SOCKET."
  (let ((line (get-protocol-line (get-manager-socket-fd socket))))
    (cond
      ((null line)
       (error "The manager closed the response connection."))
      (t
       (get-protocol-error line)
       (let ((parts (uiop:split-string line)))
         (unless (and (= (length parts) 3)
                      (string= (first parts) "VALUE")
                      (string= (second parts) path))
           (error "The manager returned an invalid value."))
         (third parts))))))

(defun get-state-keyword (text)
  "Convert protocol TEXT into a state keyword."
  (intern (string-upcase text) "KEYWORD"))

(defun get-session-manager-values (socket)
  "Return the Manager state, Sessions, and Services from SOCKET."
  (let ((state nil)
        (sessions nil)
        (services nil))
    (loop for line = (get-protocol-line (get-manager-socket-fd socket))
          do (when (null line)
               (error "The manager closed the state connection."))
             (get-protocol-error line)
             (cond
               ((uiop:string-prefix-p "STATE " line)
                (setf state (get-state-keyword
                             (subseq line (length "STATE ")))))
               ((string= line "END")
                (return (values state
                                (nreverse sessions)
                                (nreverse services))))
               ((uiop:string-prefix-p "SESSION " line)
                (let ((parts (uiop:split-string line)))
                  (unless (= (length parts) 3)
                    (error "The manager returned an invalid Session."))
                  (push (cons (second parts)
                              (get-state-keyword (third parts)))
                        sessions)))
               ((uiop:string-prefix-p "SERVICE " line)
                (let ((parts (uiop:split-string line)))
                  (unless (= (length parts) 3)
                    (error "The manager returned an invalid Service."))
                  (push (cons (second parts)
                              (get-state-keyword (third parts)))
                        services)))
               (t
                (error "The manager returned an invalid state."))))))

(defun get-stopped-session-manager ()
  "Return the snapshot used when no manager is listening."
  (list :state :stopped :sessions nil :services nil))

(defun get-session-manager ()
  "Return the Manager snapshot from the detached manager."
  (if (not (manager-responding-p))
      (get-stopped-session-manager)
      (let ((socket (get-manager-connection)))
        (unwind-protect
             (progn
               (set-protocol-line
                (get-manager-socket-fd socket)
                "GET SESSION-MANAGER")
               (multiple-value-bind (state sessions services)
                   (get-session-manager-values socket)
                 (list :state state
                       :sessions sessions
                       :services services)))
          (del-manager-socket socket)))))

(defun get-session-list ()
  "Return named Sessions from the detached manager."
  (getf (get-session-manager) :sessions))

(defun get-service-list ()
  "Return named Services from the detached manager."
  (getf (get-session-manager) :services))

(defun del-session-manager ()
  "Stop the detached manager and return the stopped snapshot."
  (if (not (manager-responding-p))
      (get-stopped-session-manager)
      (let ((socket (get-manager-connection)))
        (unwind-protect
             (progn
               (set-protocol-line
                (get-manager-socket-fd socket)
                "DEL SESSION-MANAGER")
               (get-value-response socket "SESSION-MANAGER")
               (get-stopped-session-manager))
          (del-manager-socket socket)))))

(defun del-session (name)
  "Delete NAME through the detached manager."
  (check-type name string)
  (if (not (manager-responding-p))
      name
      (let ((socket (get-manager-connection)))
        (unwind-protect
             (progn
               (set-protocol-line
                (get-manager-socket-fd socket)
                (format nil "DEL SESSION ~A" name))
               (get-value-response socket "SESSION"))
          (del-manager-socket socket)))))

(defun get-detached-report (name sessions)
  "Return detached text when NAME is in SESSIONS."
  (when (assoc name sessions :test #'string=)
    (format nil "detached ~A" name)))

(defun del-attachment ()
  "Detach the last-input Attachment through the detached manager."
  (let ((socket (get-manager-connection)))
    (unwind-protect
         (progn
           (set-protocol-line
            (get-manager-socket-fd socket)
            "DEL ATTACHMENT")
           (get-value-response socket "ATTACHMENT"))
      (del-manager-socket socket))))

(defun set-client-frontend (socket)
  "Run the local frontend around manager SOCKET."
  (let ((fd (get-manager-socket-fd socket)))
    (set-socket-sigpipe fd)
    (unwind-protect
         (set-socket-frontend fd)
      (del-manager-socket socket))))

(defun get-session (name)
  "Enter NAME through the detached manager."
  (check-type name string)
  (let* ((socket (get-manager-connection))
         (fd (get-manager-socket-fd socket)))
    (handler-case
        (progn
          (set-protocol-line fd (format nil "GET SESSION ~A" name))
          (let ((response (get-protocol-line fd)))
            (get-protocol-error response)
            (unless (equal "READY SESSION" response)
              (error "The manager rejected the Session request."))
            (set-client-frontend socket)
            (let ((report (get-detached-report name (get-session-list))))
              (when report
                (format t "~A~%" report)))
            name))
      (error (condition)
        (del-manager-socket socket)
        (error condition)))))

(defun new-session (name)
  "Ensure NAME exists, then enter it through the detached manager."
  (check-type name string)
  (new-session-manager)
  (let ((socket (get-manager-connection)))
    (unwind-protect
         (progn
           (set-protocol-line
            (get-manager-socket-fd socket)
            (format nil "NEW SESSION ~A" name))
           (get-value-response socket "SESSION"))
      (del-manager-socket socket)))
  (get-session name))

(defun new-service-source (path)
  "Load one Service source through the detached manager."
  (check-type path (or string pathname))
  (new-session-manager)
  (let ((socket (get-manager-connection)))
    (unwind-protect
         (progn
           (set-protocol-line
            (get-manager-socket-fd socket)
            (format nil "NEW SERVICE ~A" (namestring path)))
           (get-value-response socket "SERVICE"))
      (del-manager-socket socket))))

(defun get-service-response (socket name)
  "Return one Service snapshot from SOCKET."
  (let ((line (get-protocol-line (get-manager-socket-fd socket))))
    (cond
      ((null line)
       (error "The manager closed the Service response connection."))
      (t
       (get-protocol-error line)
       (let ((parts (uiop:split-string line)))
         (unless (and (= (length parts) 5)
                      (string= (first parts) "SERVICE")
                      (string= (second parts) name))
           (error "The manager returned an invalid Service."))
         (list :name (second parts)
               :desired-state (get-state-keyword (third parts))
               :state (get-state-keyword (fourth parts))
               :pid (if (string= (fifth parts) "nil")
                        nil
                        (parse-integer (fifth parts)))))))))

(defun get-service (name)
  "Return one Service snapshot from the detached manager."
  (check-type name string)
  (let ((socket (get-manager-connection)))
    (unwind-protect
         (progn
           (set-protocol-line
            (get-manager-socket-fd socket)
            (format nil "GET SERVICE ~A" name))
           (get-service-response socket name))
      (del-manager-socket socket))))

(defun set-service (name state)
  "Change one Service state through the detached manager."
  (check-type name string)
  (let ((text (if (symbolp state)
                  (symbol-name state)
                  state)))
    (let ((socket (get-manager-connection)))
      (unwind-protect
           (progn
             (set-protocol-line
              (get-manager-socket-fd socket)
              (format nil "SET SERVICE ~A ~A" name text))
             (get-service-response socket name))
        (del-manager-socket socket)))))

(defun del-service (name)
  "Delete one Service through the detached manager."
  (check-type name string)
  (if (not (manager-responding-p))
      name
      (let ((socket (get-manager-connection)))
        (unwind-protect
             (progn
               (set-protocol-line
                (get-manager-socket-fd socket)
                (format nil "DEL SERVICE ~A" name))
               (get-value-response socket "SERVICE"))
          (del-manager-socket socket)))))

(defun get-service-output (name)
  "Return recent Service output from the detached manager."
  (check-type name string)
  (let* ((socket (get-manager-connection))
         (fd (get-manager-socket-fd socket)))
    (handler-case
        (progn
          (set-protocol-line fd (format nil "GET SERVICE-LOG ~A" name))
          (let* ((response (get-protocol-line fd))
                 (parts (and response (uiop:split-string response))))
            (get-protocol-error response)
            (unless (and parts
                         (= (length parts) 2)
                         (string= (first parts) "READY")
                         (string= (second parts) "SERVICE"))
              (error "The manager rejected the Service log request."))
            (let ((chunks nil))
              (loop
                (multiple-value-bind (bytes eof-p)
                    (get-fd fd :max-bytes 4096 :wait-p nil)
                  (when (and bytes (plusp (length bytes)))
                    (push bytes chunks))
                  (when (or eof-p
                            (null bytes)
                            (zerop (length bytes)))
                    (return))))
              (if chunks
                  (apply #'concatenate
                         '(vector (unsigned-byte 8))
                         (nreverse chunks))
                  (make-array 0 :element-type '(unsigned-byte 8))))))
      (error (condition)
        (del-manager-socket socket)
        (error condition))
      (:no-error (output)
        (del-manager-socket socket)
        output))))
