(in-package #:mtm.app)

(defparameter *version* "0.1.0")

(defun set-unhandled-condition-report (condition)
  "Print CONDITION to the process error stream."
  (format *error-output* "MTM error: ~A~%" condition)
  (finish-output *error-output*))

(defun set-application-usage-error (operation)
  "Signal the complete usage for an operation with two forms."
  (error "Usage:~%  mtm ~A session-manager~%  mtm ~A session <name>"
         operation
         operation))

(defun set-application-operation (command)
  "Apply one NEW, GET, SET, or DEL CLI operation."
  (let ((arguments (command-arguments command))
        (application-p (getopt command :application)))
    (unless arguments
      (error "The CLI requires NEW, GET, SET, or DEL."))
    (let ((operation (first arguments)))
      (when (and application-p
                 (not (string-equal operation "SET")))
        (error "--application only supports SET CURRENT-SESSION."))
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
         (unless (= (length arguments) 2)
           (error "Usage: mtm get <session-manager|session>"))
         (let ((path (second arguments)))
           (cond
             ((string-equal path "SESSION-MANAGER")
              (get-cli-session-manager))
             ((string-equal path "SESSION")
              (get-cli-session-list))
             ((string-equal path "CURRENT-SESSION")
              (error "The CLI current-session position is local to one process."))
             (t
              (error "GET does not support position ~A." path)))))
        ((string-equal operation "SET")
         (unless (and (= (length arguments) 3)
                      (string-equal (second arguments) "CURRENT-SESSION"))
           (error "Usage: mtm set current-session <name> [--application]"))
         (set-cli-current-session-frontend
          (third arguments)
          :application-p application-p))
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

(defun new-application-command ()
  "Return the root command for the MTM application."
  (make-command :name "mtm"
                :description "Manage named shell Sessions."
                :usage "<new|get|set|del> <path> [value] [--application]"
                :options
                (list (make-option
                       :flag
                       :long-name "application"
                       :description "Enter Application passthrough."
                       :key :application))
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
