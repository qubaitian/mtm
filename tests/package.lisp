(defpackage #:mtm.tests
  (:use #:cl)
  (:import-from #:mtm
                #:attachment-attached-p
                #:del-attachment
                #:del-session
                #:del-session-manager
                #:get-session-list
                #:get-session-size
                #:new-attachment
                #:new-session-manager
                #:session-name
                #:session-running-p
                #:get-session-manager)
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
                #:get-session-manager-value
                #:get-session-value
                #:new-session-value
                #:set-attachment-input
                #:attachment-full-screen-owner-p
                #:session-full-screen-p
                #:set-active-attachment)
  (:import-from #:mtm.utf8
                #:get-utf8)
  (:import-from #:mtm.platform
                #:tty-p
                #:set-raw-terminal)
  (:export #:set-tests))
