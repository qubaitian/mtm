(in-package #:mtm.tests)

;; Check that the Browser terminal page starts as an ordinary terminal.
(deftest browser-page-loads-xterm-from-cdn ()
  (let ((page (mtm.app::new-browser-terminal-page)))
    (check (search "@xterm/xterm@5.5.0/lib/xterm.js" page)
           "The Browser page does not load the pinned xterm.js version.")
    (check (search "@xterm/xterm@5.5.0/css/xterm.css" page)
           "The Browser page does not load the pinned xterm.css version.")
    (check (search "const columns = 80;" page)
           "The Browser page loses its default terminal width.")
    (check (search "const rows = 24;" page)
           "The Browser page loses its default terminal height.")
    (check (search "html, body { width: 100%; height: 100%;" page)
           "The Browser page has invalid CSS percentages.")
    (check (search "[Shell exited]" page)
           "The Browser page does not report shell exit.")
    (check (search "new Terminal" page)
           "The Browser page does not initialize xterm.js.")
    (check (not (search "/session/" page))
           "The Browser page keeps a Session-specific route.")))

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

;; Check that the Browser root serves the ordinary terminal page.
(deftest browser-serves-terminal-page ()
  (let ((response
          (mtm.app::set-browser-request
           (list :request-method :get
                 :path-info "/"
                 :headers (make-hash-table :test #'equal)))))
    (check (= 200 (first response))
           "The Browser terminal route does not return HTTP 200.")
    (check (search "MTM browser terminal" (first (third response)))
           "The Browser root returns the wrong page.")
    (check (not (search "MTM sessions" (first (third response))))
           "The Browser root still renders a Session list.")))

;; Check that Browser routes cannot select a Session by URL.
(deftest browser-rejects-session-route ()
  (let ((response
          (mtm.app::set-browser-request
           (list :request-method :get
                 :path-info "/session/missing"
                 :headers (make-hash-table :test #'equal)))))
    (check (= 404 (first response))
           "The Browser accepts a Session-specific route.")))

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
