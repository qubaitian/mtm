(defpackage #:mtm.tests
  (:use #:cl)
  (:import-from #:mtm
                #:attachment-attached-p
                #:del-attachment
                #:del-current-session
                #:del-session
                #:del-session-manager
                #:get-current-session
                #:get-session
                #:get-session-list
                #:get-session-size
                #:get-session-manager
                #:new-attachment
                #:new-session
                #:new-session-manager
                #:session-application-p
                #:session-name
                #:session-running-p
                #:set-current-session)
  (:import-from #:mtm.pty
                #:del-shell-session
                #:get-shell-output-bytes
                #:new-shell-session
                #:pty-master
                #:session-open-p
                #:set-shell-input
                #:set-shell-size)
  (:import-from #:mtm.terminal
                #:get-terminal-cell
                #:get-terminal-cursor-position
                #:get-terminal-render
                #:get-terminal-screen-lines
                #:get-terminal-screen-events
                #:new-terminal-emulator
                #:screen-cell-character
                #:screen-cell-style
                #:set-terminal-input
                #:terminal-alternate-screen-p)
  (:import-from #:bordeaux-threads
                #:join-thread
                #:make-thread)
  (:import-from #:mtm.session
                #:get-attachment-output
                #:get-attachment-start-screen
                #:get-retained-screen
                #:set-attachment-input
                #:attachment-application-owner-p)
  (:import-from #:mtm.utf8
                #:get-utf8)
  (:import-from #:mtm.platform
                #:tty-p
                #:set-raw-terminal)
  (:import-from #:mtm.session
                #:set-current-attachment)
  (:export #:set-tests))
