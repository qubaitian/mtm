(defpackage #:mtm.app
  (:use #:cl)
  (:import-from #:clingon
                #:command-arguments
                #:make-command
                #:run)
  (:import-from #:mtm
                #:attachment-session
                #:del-attachment
                #:del-session
                #:del-session-manager
                #:get-session
                #:get-attachment-output
                #:get-attachment-start-screen
                #:get-session-manager
                #:get-session-size
                #:get-service
                #:get-service-list
                #:get-service-output
                #:new-attachment
                #:new-service-source
                #:session-name
                #:set-passthrough-frontend
                #:set-service
                #:del-service)
  (:import-from #:mtm.pty
                #:del-shell-session
                #:get-shell-output-bytes
                #:new-shell-session
                #:set-shell-input)
  (:import-from #:mtm.session
                #:get-session-manager-value
                #:get-session-value
                #:new-session-value
                #:new-session-manager
                #:session-full-screen-p)
  (:import-from #:mtm.frontend
                #:set-socket-frontend)
  (:import-from #:mtm.platform
                #:get-fd
                #:set-close-on-exec
                #:set-fd
                #:set-socket-sigpipe)
  (:import-from #:mtm.utf8
                #:get-utf8)
  (:import-from #:bordeaux-threads
                #:current-thread
                #:join-thread
                #:make-thread)
  (:export
   #:main))
