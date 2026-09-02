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
   #:new-pty-process
   #:del-process
   #:set-process-signal
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
                #:new-pty-process
                #:del-process
                #:get-process-status
                #:set-process-signal
                #:set-terminal-size)
  (:export
   #:del-shell-session
   #:pty-master
   #:get-shell-output-bytes
   #:session-open-p
   #:shell-session
   #:new-shell-session
   #:del-process-session
   #:get-process-id
   #:get-process-output-bytes
   #:new-process-session
   #:set-shell-input
   #:set-shell-size))

(defpackage #:mtm.service
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
                #:del-process-session
                #:get-process-id
                #:get-process-output-bytes
                #:new-process-session)
  (:export
   #:del-service-runtime
   #:get-service-runtime-output
   #:get-service-runtime-source-path
   #:get-service-runtime-snapshot
   #:new-service-runtime
   #:set-service-runtime-desired-state
   #:set-service-runtime-specification))

(defpackage #:mtm.service-config
  (:use #:cl))

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
                #:terminal-height
                #:terminal-width)
  (:import-from #:mtm.utf8
                #:get-utf8-chunk)
  (:import-from #:mtm.service
                #:del-service-runtime
                #:get-service-runtime-output
                #:get-service-runtime-source-path
                #:get-service-runtime-snapshot
                #:new-service-runtime
                #:set-service-runtime-desired-state
                #:set-service-runtime-specification)
  (:intern #:get-shell
           #:get-session-manager-value
           #:get-session-value
           #:new-session-value
           #:session-full-screen-p
           #:set-active-attachment)
  (:export
   #:attachment
   #:attachment-session
   #:attachment-attached-p
   #:del-attachment
   #:del-service
   #:del-session
   #:del-session-manager
   #:get-attachment-output
   #:get-attachment-start-screen
   #:get-retained-screen
   #:get-session-list
   #:get-session-size
   #:get-session-manager
   #:get-service
   #:get-service-list
   #:get-service-output
   #:new-attachment
   #:new-service
   #:new-service-source
   #:new-session-manager
   #:session-history-box
   #:session-name
   #:session-running-p
   #:set-service
   #:set-attachment-input
   #:set-attachment-terminal-size))

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
                #:get-attachment-output
                #:get-attachment-start-screen
                #:get-shell
                #:get-retained-screen
                #:get-session-value
                #:get-session-list
                #:get-session-size
                #:get-service-list
                #:get-service-output
                #:new-attachment
                #:new-session-value
                #:session-full-screen-p
                #:session-history-box
                #:session-name
                #:set-attachment-input
                #:set-attachment-terminal-size
                #:set-active-attachment)
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
   #:get-session
   #:new-session
   #:set-socket-frontend
   #:session-frontend-name
   #:session-frontend-socket-fd
   #:session-frontend-return-session-name
   #:session-frontend-service-log-p
   #:set-passthrough-frontend))

(defpackage #:mtm
  (:use #:cl)
  (:import-from #:mtm.frontend
                #:get-session
                #:new-session
                #:set-passthrough-frontend)
  (:import-from #:mtm.session
                #:attachment
                #:attachment-session
                #:attachment-attached-p
                #:del-attachment
                #:del-service
                #:del-session
                #:del-session-manager
                #:get-session-list
                #:get-session-size
                #:get-session-manager
                #:get-service
                #:get-service-list
                #:get-service-output
                #:get-attachment-output
                #:get-attachment-start-screen
                #:get-retained-screen
                #:new-attachment
                #:new-service
                #:new-service-source
                #:new-session-manager
                #:session-name
                #:session-running-p
                #:set-service
                #:set-attachment-input
                #:set-attachment-terminal-size)
  (:export
   #:attachment
   #:attachment-session
   #:attachment-attached-p
   #:del-attachment
   #:del-service
   #:del-session
   #:del-session-manager
   #:get-attachment-output
   #:get-attachment-start-screen
   #:get-retained-screen
   #:get-session
   #:get-session-list
   #:get-session-size
   #:get-session-manager
   #:get-service
   #:get-service-list
   #:get-service-output
   #:new-attachment
   #:new-service
   #:new-service-source
   #:new-session
   #:new-session-manager
   #:session-name
   #:session-running-p
   #:set-service
   #:set-attachment-input
   #:set-attachment-terminal-size
   #:set-passthrough-frontend))
