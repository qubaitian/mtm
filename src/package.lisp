(defpackage #:mtm.platform
  (:use #:cl #:cffi)
  (:export
   #:+pollerr+
   #:+pollhup+
   #:+pollin+
   #:+pollnval+
   #:set-raw-terminal
   #:del-pty
   #:get-poll-events
   #:get-terminal-size
   #:set-terminal-size
   #:set-socket-sigpipe
   #:get-fd
   #:get-system-clipboard
   #:set-close-on-exec
   #:new-pty
   #:del-process
   #:set-system-clipboard
   #:tty-p
   #:get-process-status
   #:set-fd))

(defpackage #:mtm.utf8
  (:use #:cl)
  (:export
   #:get-utf8-chunk
   #:get-utf8))

(defpackage #:mtm.editor
  (:use #:cl)
  (:import-from #:mtm.platform
                #:set-fd)
  (:import-from #:mtm.utf8
                #:get-utf8
                #:get-utf8-chunk)
  (:export
   #:del-editor-selection
   #:del-editor-render
   #:editor-empty-p
   #:get-editor-selection-text
   #:new-editor
   #:new-input-parser
   #:set-editor-mouse
   #:set-editor-byte
   #:set-editor-key
   #:set-editor-paste
   #:set-editor-render
   #:set-input-parser-events))

(defpackage #:mtm.pty
  (:use #:cl)
  (:import-from #:mtm.platform
                #:del-pty
                #:get-fd
                #:set-fd
                #:new-pty
                #:del-process
                #:get-process-status
                #:set-terminal-size)
  (:export
   #:del-shell-session
   #:pty-master
   #:get-shell-output-bytes
   #:session-open-p
   #:shell-session
   #:new-shell-session
   #:set-shell-input
   #:set-shell-size))

(defpackage #:mtm.terminal
  (:use #:cl)
  (:export
   #:get-terminal-copy
   #:new-terminal-emulator
   #:get-terminal-render
   #:get-terminal-screen-events
   #:set-terminal-input
   #:set-terminal-size
   #:screen-cell-character
   #:screen-cell-style
   #:get-terminal-screen-lines
   #:get-terminal-cell
   #:get-terminal-cursor-position
   #:terminal-alternate-screen-p
   #:terminal-emulator
   #:terminal-height
   #:terminal-width))

(defpackage #:mtm.session
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:condition-notify
                #:condition-wait
                #:current-thread
                #:join-thread
                #:make-condition-variable
                #:make-lock
                #:make-thread
                #:with-lock-held)
  (:import-from #:mtm.pty
                #:del-shell-session
                #:get-shell-output-bytes
                #:new-shell-session
                #:set-shell-input
                #:set-shell-size)
  (:import-from #:mtm.terminal
                #:get-terminal-copy
                #:new-terminal-emulator
                #:get-terminal-screen-events
                #:set-terminal-size
                #:set-terminal-input
                #:terminal-alternate-screen-p
                #:terminal-height
                #:terminal-width)
  (:import-from #:mtm.utf8
                #:get-utf8-chunk)
  (:intern #:set-current-attachment)
  (:export
   #:attachment
   #:attachment-session
   #:attachment-attached-p
   #:del-attachment
   #:del-current-session
   #:del-session
   #:del-session-manager
   #:get-attachment-output
   #:get-attachment-start-screen
   #:get-current-session
   #:get-retained-screen
   #:get-session
   #:get-session-list
   #:get-session-size
   #:get-session-manager
   #:new-session
   #:new-attachment
   #:new-session-manager
   #:session-application-p
   #:session-history-box
   #:session-name
   #:session-running-p
   #:set-attachment-input
   #:set-attachment-terminal-size
   #:attachment-application-owner-p))

(defpackage #:mtm.frontend
  (:use #:cl)
  (:import-from #:mtm.platform
                #:+pollerr+
                #:+pollhup+
                #:+pollin+
                #:+pollnval+
                #:get-fd
                #:get-system-clipboard
                #:get-poll-events
                #:get-terminal-size
                #:set-fd
                #:set-system-clipboard
                #:tty-p
                #:set-raw-terminal)
  (:import-from #:mtm.session
                #:del-attachment
                #:attachment-session
                #:attachment-application-owner-p
                #:get-attachment-output
                #:get-attachment-start-screen
                #:get-retained-screen
                #:get-session-list
                #:get-session-size
                #:new-attachment
                #:session-application-p
                #:session-history-box
                #:session-name
                #:set-attachment-input
                #:set-attachment-terminal-size
                #:set-current-attachment)
  (:import-from #:mtm.editor
                #:del-editor-render
                #:del-editor-selection
                #:editor-empty-p
                #:new-editor
                #:new-input-parser
                #:set-editor-mouse
                #:set-editor-byte
                #:set-editor-key
                #:set-editor-paste
                #:set-editor-render
                #:set-input-parser-events)
  (:import-from #:mtm.terminal
                #:get-terminal-cursor-position
                #:get-terminal-render)
  (:import-from #:mtm.utf8
                #:get-utf8)
  (:export
   #:set-socket-frontend
   #:set-frontend-application-mode
   #:session-frontend-name
   #:session-frontend-socket-fd
   #:set-current-session
   #:set-passthrough-frontend))

(defpackage #:mtm
  (:use #:cl)
  (:import-from #:mtm.frontend
                #:set-current-session
                #:set-passthrough-frontend)
  (:import-from #:mtm.session
                #:attachment
                #:attachment-session
                #:attachment-attached-p
                #:del-attachment
                #:del-current-session
                #:del-session
                #:del-session-manager
                #:get-session
                #:get-session-list
                #:get-session-size
                #:get-session-manager
                #:get-current-session
                #:get-attachment-output
                #:get-attachment-start-screen
                #:get-retained-screen
                #:new-attachment
                #:new-session
                #:new-session-manager
                #:session-application-p
                #:session-name
                #:session-running-p
                #:set-attachment-input
                #:set-attachment-terminal-size
                #:attachment-application-owner-p)
  (:export
   #:attachment
   #:attachment-session
   #:attachment-attached-p
   #:del-attachment
   #:del-current-session
   #:del-session
   #:del-session-manager
   #:get-attachment-output
   #:get-attachment-start-screen
   #:get-current-session
   #:get-retained-screen
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
   #:set-attachment-input
   #:set-attachment-terminal-size
   #:attachment-application-owner-p
   #:set-current-session
   #:set-passthrough-frontend))
