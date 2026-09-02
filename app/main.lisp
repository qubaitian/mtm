(in-package #:mtm.app)

(defparameter *version* "0.1.0")

(defun set-unhandled-condition-report (condition)
  "Print CONDITION to the process error stream."
  (format *error-output* "MTM error: ~A~%" condition)
  (finish-output *error-output*))

;; Signal the valid CLI forms for one operation.
(defun set-application-usage-error (operation)
  "Signal the complete usage for an operation."
  (error "Usage:~%  mtm ~A session-manager~%  mtm ~A session <name>"
         operation
         operation))

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
            (new-cli-session-manager))
           ((and (= (length arguments) 3)
                 (string-equal (second arguments) "SESSION"))
            (new-cli-session (third arguments)))
           (t
            (set-application-usage-error "new"))))
        ((string-equal operation "GET")
         (unless (member (length arguments) '(2 3))
           (set-application-usage-error "get"))
         (let ((path (second arguments)))
           (cond
             ((and (= (length arguments) 2)
                   (string-equal path "SESSION-MANAGER"))
              (get-cli-session-manager))
             ((and (= (length arguments) 3)
                   (string-equal path "SESSION"))
              (get-cli-session-frontend (third arguments)))
             (t
              (set-application-usage-error "get")))))
        ((string-equal operation "SET")
         (error "SET has no supported Session position."))
        ((string-equal operation "DEL")
         (cond
           ((and (= (length arguments) 2)
                 (string-equal (second arguments) "SESSION-MANAGER"))
            (del-cli-session-manager))
           ((and (= (length arguments) 3)
                 (string-equal (second arguments) "SESSION"))
            (del-cli-session (third arguments)))
           (t
            (set-application-usage-error "del"))))
        (t
         (error "Unknown CLI operation ~A." operation))))))

;; Build the root MTM command without mode options.
(defun new-application-command ()
  "Return the root command for the MTM application."
  (make-command :name "mtm"
                :description "Manage named shell Sessions."
                :usage "<new|get|set|del> <path> [value]"
                :version *version*
                :handler #'set-application-operation))

(defun main ()
  "Start the MTM command-line application."
  (handler-case
      (if (member +manager-server-argument+
                  (uiop:command-line-arguments)
                  :test #'string=)
          (set-manager-server)
          (run (new-application-command)))
    (error (condition)
      (set-unhandled-condition-report condition)
      (uiop:quit 1))))
