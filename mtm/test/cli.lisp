(defpackage #:mtm.test.cli
  (:use #:cl)
  (:import-from #:mtm.test
                #:check
                #:deftest
                #:error-signals-p))

(in-package #:mtm.test.cli)

(defun get-cli-usage-error (operation)
  "Return the usage error for OPERATION."
  (handler-case
      (progn
        (mtm.cli::set-application-usage-error operation)
        nil)
    (error (condition)
      (princ-to-string condition))))

(deftest cli-usage-lists-session-manager-forms ()
  (let ((new-usage (get-cli-usage-error "new"))
        (get-usage (get-cli-usage-error "get"))
        (del-usage (get-cli-usage-error "del")))
    (check (and (search "mtm new session-manager" new-usage)
                (search "mtm new session <name>" new-usage)
                (search "mtm new service <path>" new-usage))
           "The NEW usage omits a valid form.")
    (check (and (search "mtm get session-manager" get-usage)
                (search "mtm get session <name>" get-usage)
                (search "mtm get service <name>" get-usage))
           "The GET usage omits a valid form.")
    (check (and (search "mtm del session-manager" del-usage)
                (search "mtm del session <name>" del-usage)
                (search "mtm del attachment" del-usage)
                (not (search "mtm del attachment <" del-usage)))
           "The DEL usage omits a valid form.")))

(deftest cli-command-wires-clingon ()
  (let ((command (mtm.cli::new-application-command)))
    (check (string= "mtm" (clingon:command-name command))
           "The CLI command has the wrong name.")
    (check (string= "0.1.0" (clingon:command-version command))
           "The CLI command has the wrong version.")
    (check (functionp (clingon:command-handler command))
           "The CLI command has no handler.")
    (check (null (clingon:command-sub-commands command))
           "The CLI command exposes subcommands.")
    (check (string= "<new|get|set|del> <path> [value]"
                    (clingon:command-usage command))
           "The CLI command has the wrong operation usage.")))

(deftest cli-command-rejects-unsupported-option ()
  (check (error-signals-p
          (lambda ()
            (let ((command (mtm.cli::new-application-command)))
              (clingon:parse-command-line
               command
               '("set" "session" "s1" "--unsupported")))))
         "The CLI accepts an unsupported option."))

(deftest cli-command-rejects-manager-server-flag ()
  (check (error-signals-p
          (lambda ()
            (let ((command (mtm.cli::new-application-command)))
              (clingon:parse-command-line
               command
               '("--manager-server")))))
         "The CLI still accepts the manager-server flag."))

(deftest client-omits-manager-server-command ()
  (dolist (name '("+MANAGER-SERVER-ARGUMENT+"
                  "DUMPED-EXECUTABLE-P"
                  "GET-MANAGER-COMMAND"))
    (let ((symbol (find-symbol name "MTM.CLIENT")))
      (check (not (and symbol (or (boundp symbol) (fboundp symbol))))
             "The client still keeps ~A."
             name))))

(defun get-cli-operation-error (arguments)
  "Return the CLI error for ARGUMENTS."
  (handler-case
      (let ((command (clingon:make-command :name "mtm")))
        (setf (clingon:command-arguments command) arguments)
        (mtm.cli::set-application-operation command)
        nil)
    (error (condition)
      (princ-to-string condition))))

(deftest cli-del-attachment-rejects-a-name-argument ()
  (check (search "Usage:"
                 (get-cli-operation-error '("del" "attachment" "s1")))
         "The CLI accepts an Attachment name."))

(deftest client-reports-detach-for-a-running-session ()
  (check (string= "detached s1"
                  (mtm.client::get-detached-report
                   "s1" '(("s1" . :running))))
         "A running Session does not report detachment.")
  (check (null (mtm.client::get-detached-report "s1" nil))
         "A missing Session reports detachment."))

(deftest cli-rejects-removed-debug-operation ()
  (let ((command (clingon:make-command
                  :name "mtm")))
    (setf (clingon:command-arguments command)
          '("get" "debug"))
    (handler-case
        (progn
          (mtm.cli::set-application-operation command)
          (error "The CLI accepts removed debug."))
      (error () t))))

(defun get-cli-script-path ()
  "Return the shebang script path."
  (merge-pathnames "bin/mtm" (asdf:system-source-directory "mtm")))

(deftest cli-script-prints-help ()
  (let ((output (uiop:run-program
                 (list (uiop:native-namestring (get-cli-script-path))
                       "--help")
                 :output :string
                 :error-output :string)))
    (check (search "Manage named shell Sessions and Services." output)
           "The shebang script does not print the command description.")))
