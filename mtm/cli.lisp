(defpackage #:mtm.cli
  (:use #:cl)
  (:import-from #:clingon
                #:command-arguments
                #:make-command
                #:run)
  (:export
   #:main))

(in-package #:mtm.cli)

(defparameter *version* "0.1.0")

(defun set-unhandled-condition-report (condition)
  "Print CONDITION to the process error stream."
  (format *error-output* "MTM error: ~A~%" condition)
  (finish-output *error-output*))

(defun set-cli-session-manager-output (snapshot)
  "Print one Manager snapshot."
  (format t "state ~A~%"
          (string-downcase (symbol-name (getf snapshot :state))))
  (dolist (session (getf snapshot :sessions))
    (format t "session ~A ~A~%"
            (car session)
            (string-downcase (symbol-name (cdr session)))))
  (dolist (service (getf snapshot :services))
    (format t "service ~A ~A~%"
            (car service)
            (string-downcase (symbol-name (cdr service))))))

(defun set-cli-service-output (snapshot)
  "Print one Service snapshot."
  (format t "service ~A desired ~A state ~A pid ~A~%"
          (getf snapshot :name)
          (string-downcase (symbol-name (getf snapshot :desired-state)))
          (string-downcase (symbol-name (getf snapshot :state)))
          (or (getf snapshot :pid) "nil")))

;; Signal the valid CLI forms for one operation.
(defun set-application-usage-error (operation)
  "Signal the complete usage for an operation."
  (declare (ignore operation))
  (error "Usage:~%  mtm new session-manager~%  mtm new session <name>~%  mtm new service <path>~%  mtm get session-manager~%  mtm get session <name>~%  mtm get service <name>~%  mtm set service <name> <state>~%  mtm del session-manager~%  mtm del session <name>~%  mtm del service <name>~%  mtm del attachment"))

;; Dispatch one command using the named Session operations.
(defun set-application-operation (command)
  "Apply one NEW, GET, SET, or DEL CLI operation."
  (let ((arguments (command-arguments command)))
    (unless arguments
      (error "The CLI requires NEW, GET, SET, or DEL."))
    (let ((operation (first arguments)))
      (cond
        ((string-equal operation "NEW")
         (cond
           ((and (= (length arguments) 2)
                 (string-equal (second arguments) "SESSION-MANAGER"))
            (set-cli-session-manager-output (mtm:new-session-manager)))
           ((and (= (length arguments) 3)
                 (string-equal (second arguments) "SESSION"))
            (mtm:new-session (third arguments)))
           ((and (= (length arguments) 3)
                 (string-equal (second arguments) "SERVICE"))
            (format t "~A~%"
                    (mtm:new-service-source (third arguments))))
           (t
            (set-application-usage-error "new"))))
        ((string-equal operation "GET")
         (unless (member (length arguments) '(2 3))
           (set-application-usage-error "get"))
         (let ((path (second arguments)))
           (cond
             ((and (= (length arguments) 2)
                   (string-equal path "SESSION-MANAGER"))
              (set-cli-session-manager-output (mtm:get-session-manager)))
             ((and (= (length arguments) 3)
                   (string-equal path "SESSION"))
              (mtm:get-session (third arguments)))
             ((and (= (length arguments) 3)
                   (string-equal path "SERVICE"))
              (set-cli-service-output (mtm:get-service (third arguments))))
             (t
              (set-application-usage-error "get")))))
        ((string-equal operation "SET")
         (if (and (= (length arguments) 4)
                  (string-equal (second arguments) "SERVICE"))
             (set-cli-service-output
              (mtm:set-service (third arguments) (fourth arguments)))
             (set-application-usage-error "set")))
        ((string-equal operation "DEL")
         (cond
           ((and (= (length arguments) 2)
                 (string-equal (second arguments) "SESSION-MANAGER"))
            (set-cli-session-manager-output (mtm:del-session-manager)))
           ((and (= (length arguments) 3)
                 (string-equal (second arguments) "SESSION"))
            (format t "~A~%" (mtm:del-session (third arguments))))
           ((and (= (length arguments) 3)
                 (string-equal (second arguments) "SERVICE"))
            (format t "~A~%" (mtm:del-service (third arguments))))
           ((and (= (length arguments) 2)
                 (string-equal (second arguments) "ATTACHMENT"))
            (format t "~A~%" (mtm:del-attachment)))
           (t
            (set-application-usage-error "del"))))
        (t
         (error "Unknown CLI operation ~A." operation))))))

;; Build the root MTM command without mode options.
(defun new-application-command ()
  "Return the root command for the MTM application."
  (make-command :name "mtm"
                :description "Manage named shell Sessions and Services."
                :usage "<new|get|set|del> <path> [value]"
                :version *version*
                :handler #'set-application-operation))

;; Run the parsed MTM command.
(defun main ()
  "Start the MTM command-line application."
  (handler-case
      (run (new-application-command))
    (error (condition)
      (set-unhandled-condition-report condition)
      (uiop:quit 1))))
