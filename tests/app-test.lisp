(in-package #:mtm.tests)

(deftest browser-page-loads-xterm-from-cdn ()
  (let ((page (mtm.app::new-browser-session-page "demo" 80 24)))
    (check (search "@xterm/xterm@5.5.0/lib/xterm.js" page)
           "The Browser page does not load the pinned xterm.js version.")
    (check (search "@xterm/xterm@5.5.0/css/xterm.css" page)
           "The Browser page does not load the pinned xterm.css version.")
    (check (search "const columns = 80;" page)
           "The Browser page loses the Session width.")
    (check (search "const rows = 24;" page)
           "The Browser page loses the Session height.")
    (check (search "html, body { width: 100%; height: 100%;" page)
           "The Browser page has invalid CSS percentages.")
    (check (search "event.code === 1008" page)
           "The Browser page retries a rejected connection.")
    (check (search "new Terminal" page)
           "The Browser page does not initialize xterm.js.")))

(deftest browser-uses-tty-compatible-output-frames ()
  (let ((frame (mtm.app::get-browser-output-frame (get-utf8 "abc"))))
    (check (equalp #(0 97 98 99) frame)
           "The Browser output frame has the wrong prefix or payload.")))

(deftest browser-parses-terminal-handshake ()
  (multiple-value-bind (columns rows)
      (mtm.app::get-browser-handshake-size
       (get-utf8 "{\"columns\":80,\"rows\":24}"))
    (check (and (= columns 80) (= rows 24))
           "The Browser handshake returns the wrong terminal size.")))

(deftest browser-rejects-invalid-terminal-handshake ()
  (check (error-signals-p
          (lambda ()
            (mtm.app::get-browser-handshake-size
             "{\"columns\":0,\"rows\":24}")))
         "The Browser accepts an invalid terminal size."))

(deftest browser-serves-session-list-page ()
  (let ((response
          (mtm.app::set-browser-request
           (list :request-method :get
                 :path-info "/"
                 :headers (make-hash-table :test #'equal)))))
    (check (= 200 (first response))
           "The Browser index route does not return HTTP 200.")
    (check (search "MTM sessions" (first (third response)))
           "The Browser index route returns the wrong page.")))

(deftest browser-returns-404-for-missing-session ()
  (let ((response
          (mtm.app::set-browser-request
           (list :request-method :get
                 :path-info "/session/missing"
                 :headers (make-hash-table :test #'equal)))))
    (check (= 404 (first response))
           "The Browser accepts a missing Session route.")))

(defun get-application-usage-error (operation)
  "Return the usage error for OPERATION."
  (handler-case
      (progn
        (mtm.app::set-application-usage-error operation)
        nil)
    (error (condition)
      (princ-to-string condition))))

(deftest application-usage-lists-session-manager-forms ()
  (let ((new-usage (get-application-usage-error "new"))
        (del-usage (get-application-usage-error "del")))
    (check (and (search "mtm new session-manager" new-usage)
                (search "mtm new session <name>" new-usage))
           "The NEW usage omits a valid form.")
    (check (and (search "mtm del session-manager" del-usage)
                (search "mtm del session <name>" del-usage))
           "The DEL usage omits a valid form.")))

(deftest application-command-wires-clingon ()
  (let ((command (mtm.app::new-application-command)))
    (check (string= "mtm" (clingon:command-name command))
           "The application command has the wrong name.")
    (check (string= "0.1.0" (clingon:command-version command))
           "The application command has the wrong version.")
    (check (functionp (clingon:command-handler command))
           "The application command has no handler.")
    (check (null (clingon:command-sub-commands command))
           "The application command exposes subcommands.")
    (check (string= "<new|get|set|del> <path> [value] [--application]"
                    (clingon:command-usage command))
           "The application command has the wrong operation usage.")))

(deftest application-command-parses-application-flag ()
  (let ((command (mtm.app::new-application-command)))
    (clingon:parse-command-line
     command
     '("set" "current-session" "s1" "--application"))
    (check (clingon:getopt command :application)
           "The application command loses the Application flag.")
    (check (equal '("set" "current-session" "s1")
                  (clingon:command-arguments command))
           "The application flag remains a positional argument.")))

(deftest application-rejects-removed-debug-operation ()
  (let ((command (clingon:make-command
                  :name "mtm")))
    (setf (clingon:command-arguments command)
          '("get" "debug"))
    (handler-case
        (progn
          (mtm.app::set-application-operation command)
          (error "The CLI accepts removed debug."))
      (error () t))))
