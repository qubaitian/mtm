(defpackage #:mtm.manager
  (:use #:cl)
  (:import-from #:mtm.client
                #:del-manager-socket
                #:get-manager-socket-fd
                #:get-manager-socket-path
                #:get-protocol-line
                #:get-protocol-tail
                #:manager-responding-p
                #:set-protocol-line)
  (:import-from #:mtm.frontend
                #:set-passthrough-frontend)
  (:import-from #:mtm.platform
                #:get-fd
                #:set-close-on-exec
                #:set-fd
                #:set-socket-sigpipe)
  (:import-from #:mtm.session
                #:del-attachment
                #:del-attachment-value
                #:del-service
                #:del-session
                #:del-session-manager
                #:get-service
                #:get-service-list
                #:get-service-output
                #:get-session-manager
                #:get-session-manager-value
                #:new-attachment
                #:new-service-source
                #:new-session-manager
                #:new-session-value
                #:session-name
                #:set-service)
  (:import-from #:bordeaux-threads
                #:make-thread)
  (:export
   #:set-manager-server))

(in-package #:mtm.manager)

(defparameter +manager-connect-attempts+ 100)
(defparameter +manager-connect-delay+ 0.05)

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
  (let ((attachment (new-attachment name)))
    (handler-case
        (progn
          (set-protocol-line fd "READY SESSION")
          (set-passthrough-frontend
           :attachment attachment
           :input-fd fd
           :output-fd fd))
      (error (condition)
        (ignore-errors (del-attachment-value attachment))
        (error condition)))))

;; Detach the Attachment that last sent input.
(defun del-attachment-request (fd parts)
  "Detach the Attachment that last sent input."
  (unless (protocol-command-p parts "DEL" 2)
    (error "The manager received an invalid DEL request."))
  (unless (string= (second parts) "ATTACHMENT")
    (error "DEL cannot remove position ~A." (second parts)))
  (set-value-response fd "ATTACHMENT" (del-attachment)))

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
                           (string= (second parts) "ATTACHMENT"))
                      (del-attachment-request fd parts))
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

;; Run the detached Session manager.
(defun set-manager-server ()
  "Run the background Session manager process."
  (let ((listener (new-manager-listener)))
    (when listener
      (new-session-manager)
      (unwind-protect
           (set-manager-accept-loop listener)
        (del-manager-socket listener)
        (ignore-errors (delete-file (get-manager-socket-path)))
        (when (get-session-manager-value)
          (del-session-manager))))))
