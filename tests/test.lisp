(in-package #:mtm.tests)

(defvar *tests* nil)

(defmacro deftest (name () &body body)
  `(progn
     (defun ,name () ,@body)
     (pushnew ',name *tests*)))

(defun check (condition format-control &rest format-arguments)
  (unless condition
    (error (apply #'format nil format-control format-arguments))))

(defun bytes-to-string (bytes)
  (map 'string #'code-char bytes))

(defun screen-has-text-p (terminal text)
  (some (lambda (line) (search text line))
        (get-terminal-screen-lines terminal)))

(defun wait-until (predicate &key (attempts 100) (delay 0.05))
  (loop repeat attempts
        when (funcall predicate)
          do (return t)
        do (sleep delay)
        finally (return (funcall predicate))))

(defun attachment-output-has-text-p (attachment text)
  (let ((output ""))
    (loop repeat 200
          do (multiple-value-bind (bytes eof-p)
                 (get-attachment-output attachment :wait-p nil)
               (when (and bytes (plusp (length bytes)))
                 (setf output
                       (concatenate 'string output (bytes-to-string bytes)))
                 (when (search text output)
                   (return-from attachment-output-has-text-p t)))
               (when eof-p
                 (return nil))
               (sleep 0.01)))
    nil))

(deftest public-surface-keeps-only-session-functions ()
  (check (null (find-package "MTM.INPUT"))
         "The removed input package still exists.")
  (check (null (find-package "MTM.LOGGING"))
         "The removed logging package still exists.")
  (check (null (find-package "MTM.API"))
         "The removed generic API package still exists.")
  (dolist (name '("SET-COMMAND-FRONTEND"
                  "SET-INTERACTIVE-SHELL"
                  "GET-TERMINAL-RENDER"
                  "SET-MANAGER-DEBUG-VALUE"
                  "GET-SESSION-MANAGER-STATE"
                  "GET-SESSION-BY-NAME"
                  "GET-SESSION-RUNNING-P"
                  "GET-ATTACHMENT-ATTACHED-P"
                  "SESSION-ID"))
    (check (null (find-symbol name "MTM"))
           "The public package still exports ~A."
           name))
  (dolist (name '("NEW-EDITOR"
                  "SET-EDITOR-BYTE"
                  "GET-EDITOR-HISTORY"))
    (check (null (find-symbol name "MTM"))
           "The public package still exports ~A."
           name))
  (multiple-value-bind (symbol status)
      (find-symbol "SET-PASSTHROUGH-FRONTEND" "MTM")
    (check (and symbol (eq status :external))
           "The public package does not export passthrough.")))

(deftest pty-session-forwards-raw-bytes ()
  (let ((session (new-shell-session :shell "/bin/sh" :width 100 :height 40)))
    (unwind-protect
        (progn
          (check (session-open-p session)
                 "The PTY Session does not start open.")
          (check (tty-p (pty-master session))
                 "The PTY master is not a terminal descriptor.")
          (set-shell-input
           session
           (get-utf8
            (format nil "stty size; printf '~A033[31mraw-marker~A033[0m'; exit~%"
                    (string (code-char 92))
                    (string (code-char 92)))))
          (let ((output ""))
            (loop
              (multiple-value-bind (bytes eof-p)
                  (get-shell-output-bytes session)
                (when (and bytes (plusp (length bytes)))
                  (setf output
                        (concatenate 'string output (bytes-to-string bytes))))
                (when eof-p
                  (return))))
            (check (search "40 100" output)
                   "The shell does not observe its fixed PTY size.")
            (check (search (format nil "~C[31mraw-marker~C[0m" #\Escape #\Escape)
                           output)
                   "The PTY output does not preserve control bytes.")))
      (del-shell-session session))
    (check (not (session-open-p session))
           "The PTY Session remains open after close.")))

(deftest pty-session-updates-terminal-size ()
  (let ((session (new-shell-session :shell "/bin/sh" :width 100 :height 40)))
    (unwind-protect
        (progn
          (set-shell-size session 11 42)
          (set-shell-input session (get-utf8 (format nil "stty size; exit~%")))
          (let ((output ""))
            (loop
              (multiple-value-bind (bytes eof-p)
                  (get-shell-output-bytes session)
                (when (and bytes (plusp (length bytes)))
                  (setf output
                        (concatenate 'string output (bytes-to-string bytes))))
                (when eof-p
                  (return))))
            (check (search "11 42" output)
                   "The shell does not observe the updated PTY size.")))
      (del-shell-session session))))

(deftest terminal-projects-ansi-output ()
  (let ((terminal (new-terminal-emulator :width 8 :height 2)))
    (set-terminal-input terminal (format nil "abc~C[2J~C[Hxy" #\Escape #\Escape))
    (check (string= "xy      " (first (get-terminal-screen-lines terminal)))
           "ANSI cursor or erase handling is incorrect.")
    (check (not (search "mtm" (get-terminal-render terminal)))
           "The terminal projection contains unexpected interface text.")
    (multiple-value-bind (row column)
        (get-terminal-cursor-position terminal)
      (check (and (= row 1) (= column 3))
             "The terminal cursor position is incorrect."))))

(deftest terminal-preserves-styles ()
  (let ((terminal (new-terminal-emulator :width 8 :height 2)))
    (set-terminal-input terminal (format nil "~C[31mred~C[0mplain"
                                         #\Escape #\Escape))
    (let ((styled (get-terminal-cell terminal 1 1))
          (plain (get-terminal-cell terminal 1 4)))
      (check (and (char= #\r (screen-cell-character styled))
                  (member 31 (screen-cell-style styled)))
             "The terminal projection loses SGR style.")
      (check (null (screen-cell-style plain))
             "The terminal projection keeps cleared SGR style."))
    (check (equal '("redplain" "        ")
                   (get-terminal-screen-lines terminal))
           "The terminal projection loses screen content.")))

(deftest terminal-restores-primary-screen-after-alternate-screen ()
  (let ((terminal (new-terminal-emulator :width 8 :height 2)))
    (set-terminal-input terminal "main")
    (set-terminal-input terminal (format nil "~C[?1049hvim" #\Escape))
    (check (terminal-alternate-screen-p terminal)
           "The terminal does not enter its alternate screen.")
    (check (not (screen-has-text-p terminal "main"))
           "The alternate screen keeps primary screen text.")
    (set-terminal-input terminal (format nil "~C[?1049l" #\Escape))
    (check (not (terminal-alternate-screen-p terminal))
           "The terminal does not return from its alternate screen.")
    (check (screen-has-text-p terminal "main")
           "The terminal loses primary screen text on return.")
    (check (equal '(:enter :leave)
                  (get-terminal-screen-events terminal))
           "The terminal reports the wrong screen events.")))

(deftest managed-session-full-screen-mode-follows-screen-events ()
  (new-session-manager)
  (unwind-protect
       (let* ((session (new-session-value "full-screen" :shell "/bin/sh"))
              (owner (new-attachment "full-screen")))
         (check (not (session-full-screen-p session))
                "The Session starts in full-screen mode.")
         (mtm.session::set-session-pty-output
          session
          (get-utf8 (format nil "~C[?1049h" #\Escape)))
         (check (session-full-screen-p session)
                "The Session misses automatic full-screen entry.")
         (check (attachment-full-screen-owner-p owner)
                "The first Attachment does not control full-screen size.")
         (mtm.session::set-session-pty-output
          session
          (get-utf8 (format nil "~C[?1049l" #\Escape)))
         (check (not (session-full-screen-p session))
                "The Session stays in full-screen mode after return.")
         (check (not (attachment-full-screen-owner-p owner))
                "The Session keeps its full-screen owner after return.")
         (del-attachment owner))
    (del-session-manager)))

(deftest full-screen-uses-frontend-height ()
  (new-session-manager)
  (unwind-protect
       (let* ((session (new-session-value "sized-full-screen" :shell "/bin/sh"))
              (owner
                (new-attachment (session-name session)))
              (frontend
                (mtm.frontend::new-session-frontend
                 :attachment owner
                 :full-screen-p t
                 :rows 10
                 :columns 80)))
         (mtm.session::set-session-pty-output
          session
          (get-utf8 (format nil "~C[?1049h" #\Escape)))
         (mtm.frontend::set-full-screen-size frontend)
         (multiple-value-bind (rows columns)
             (mtm.platform:get-terminal-size
              (pty-master
               (mtm.session::managed-shell-session
                session)))
           (check (and (= rows 10) (= columns 80))
                  "The full-screen PTY does not use frontend height.")))
    (del-session-manager)))

(deftest terminal-reports-its-size ()
  (let ((session (new-shell-session :shell "/bin/sh" :width 37 :height 9)))
    (unwind-protect
         (multiple-value-bind (rows columns)
             (mtm.platform:get-terminal-size (pty-master session))
           (check (and (= rows 9) (= columns 37))
                  "The terminal reports the wrong size."))
      (del-shell-session session))))

(deftest frontend-parses-sgr-left-click ()
  (let ((bytes (get-utf8 (format nil "~C[<0;7;9M" #\Escape))))
    (multiple-value-bind (kind end button column row press-p)
        (mtm.frontend::get-sgr-mouse-report bytes 0)
      (check (and (eq kind :event)
                  (= end (length bytes))
                  (= button 0)
                  (= column 7)
                  (= row 9)
                  press-p)
             "The frontend does not parse an SGR left click."))))

(deftest frontend-parses-legacy-left-click ()
  (let ((bytes (new-test-octets 27 91 77 32 39 41)))
    (multiple-value-bind (kind end button column row press-p)
        (mtm.frontend::get-legacy-mouse-report bytes 0)
      (check (and (eq kind :event)
                  (= end (length bytes))
                  (= button 0)
                  (= column 7)
                  (= row 9)
                  press-p)
             "The frontend does not parse a legacy left click."))))

(deftest frontend-parses-legacy-release ()
  (let ((bytes (new-test-octets 27 91 77 35 39 41)))
    (multiple-value-bind (kind end button column row press-p)
        (mtm.frontend::get-legacy-mouse-report bytes 0)
      (check (and (eq kind :event)
                  (= end (length bytes))
                  (= button 0)
                  (= column 7)
                  (= row 9)
                  (not press-p))
             "The frontend does not parse a legacy release."))))

(deftest frontend-parses-sgr-extra-fields ()
  (let ((bytes (get-utf8 (format nil "~C[<0;7;9;0M" #\Escape))))
    (multiple-value-bind (kind end button column row press-p)
        (mtm.frontend::get-sgr-mouse-report bytes 0)
      (check (and (eq kind :event)
                  (= end (length bytes))
                  (= button 0)
                  (= column 7)
                  (= row 9)
                  press-p)
             "The frontend rejects an extended SGR mouse report."))))

(deftest frontend-strips-split-full-screen-mode-control ()
  (let* ((frontend
           (mtm.frontend::new-session-frontend :full-screen-p t))
         (marker (get-utf8
                  (format nil "x~C]MTM;mode;editor~Cy"
                          #\Escape
                          (code-char 7))))
         (ordinary ""))
    (labels ((consume (bytes)
               (mtm.frontend::set-mtm-controls
                frontend
                bytes
                (lambda (chunk)
                  (setf ordinary
                        (concatenate 'string ordinary (bytes-to-string chunk))))
                (lambda (payload)
                  (mtm.frontend::set-session-mode-control
                   frontend payload)))))
      (consume (subseq marker 0 4))
      (consume (subseq marker 4)))
    (check (string= "xy" ordinary)
           "The frontend forwards bytes around a mode control incorrectly.")
    (check (not (mtm.frontend::session-frontend-full-screen-p frontend))
           "The frontend does not apply a mode control.")))

(deftest frontend-parses-full-screen-resize-control ()
  (multiple-value-bind (rows columns)
      (mtm.frontend::get-resize-control "resize;21;80")
    (check (and (= rows 21) (= columns 80))
           "The frontend parses the wrong full-screen size.")))

(deftest frontend-forwards-incomplete-control-after-delay ()
  (let* ((frontend (mtm.frontend::new-session-frontend))
         (ordinary (make-array 0 :element-type '(unsigned-byte 8))))
    (mtm.frontend::set-mtm-controls
     frontend
     (new-test-octets 27)
     (lambda (bytes)
       (setf ordinary
             (concatenate '(vector (unsigned-byte 8)) ordinary bytes)))
     (lambda (payload)
       (declare (ignore payload))))
    (setf (mtm.frontend::session-frontend-control-since frontend)
          (- (get-internal-real-time) internal-time-units-per-second))
    (mtm.frontend::set-pending-mtm-control-output
     frontend
     (lambda (bytes)
       (setf ordinary
             (concatenate '(vector (unsigned-byte 8)) ordinary bytes))))
    (check (equalp #(27) ordinary)
           "The frontend delays an incomplete control forever.")))

(defun new-test-octets (&rest values)
  (make-array (length values)
              :element-type '(unsigned-byte 8)
              :initial-contents values))

(defun capture-platform-output (function)
  "Capture bytes written through the platform output boundary."
  (let ((bytes (new-test-octets))
        (original (symbol-function 'mtm.platform:set-fd)))
    (unwind-protect
         (progn
           (setf (symbol-function 'mtm.platform:set-fd)
                 (lambda (fd output)
                   (declare (ignore fd))
                   (setf bytes
                         (concatenate '(vector (unsigned-byte 8))
                                      bytes
                                      output))
                   (length output)))
           (funcall function)
           bytes)
      (setf (symbol-function 'mtm.platform:set-fd) original))))

(deftest frontend-enables-compatible-mouse-tracking ()
  (let ((frontend (mtm.frontend::new-session-frontend :output-fd 1)))
    (check (equalp
            (capture-platform-output
             (lambda ()
               (mtm.frontend::set-terminal-mouse frontend)))
            (get-utf8
             (format nil "~C[?1000h~C[?1002h~C[?1006h"
                     #\Escape #\Escape #\Escape)))
           "The frontend enables compatible mouse tracking.")
    (check (equalp
            (capture-platform-output
             (lambda ()
               (mtm.frontend::del-terminal-mouse frontend)))
            (get-utf8
             (format nil "~C[?1006l~C[?1002l~C[?1000l"
                     #\Escape #\Escape #\Escape)))
           "The frontend disables compatible mouse tracking.")))

(deftest editor-area-uses-full-frontend-height ()
  (let* ((terminal (new-terminal-emulator :width 4 :height 4))
         (session
           (make-instance
            'mtm.session::managed-session
            :name "alpha"
            :manager nil
            :shell-session nil
            :terminal terminal))
         (frontend
           (mtm.frontend::new-session-frontend
            :name "alpha"
            :rows 4
            :columns 4
            :output-fd -1
            :attachment
            (make-instance
             'mtm.session::attachment
             :session session
             :start-screen terminal
             :max-buffer-bytes 1))))
    (set-terminal-input terminal (format nil "~C[3;1H" #\Escape))
    (setf (mtm.frontend::session-frontend-editor frontend)
          (mtm.editor:new-editor)
          (mtm.frontend::session-frontend-input-parser frontend)
          (mtm.editor:new-input-parser))
    (set-terminal-input
     terminal
     (map 'string
          #'code-char
          (capture-platform-output
           (lambda ()
             (mtm.frontend::set-session-frontend-input
              frontend
              (new-test-octets 97 97 97 97 97))))))
    (check (= 4 (mtm.frontend::get-editor-viewport-height frontend))
           "The Editor area does not use full frontend height.")
    (check (screen-has-text-p terminal "aaaa")
           "The Editor area does not render across the full height.")
    (check (not (screen-has-text-p terminal "session-manager"))
           "The Editor area renders removed Session manager UI.")))

(deftest editor-area-submits-and-preserves-spaces ()
  (let ((editor (mtm.editor:new-editor)))
    (mtm.editor::set-editor-buffer-octets editor (new-test-octets 97 32 32))
    (multiple-value-bind (action submission)
        (mtm.editor:set-editor-byte editor 13)
      (check (eq action :submit)
             "End Enter must submit content.")
      (check (equalp submission (new-test-octets 97 32 32))
             "Submission must preserve spaces.")
      (check (mtm.editor:editor-empty-p editor)
             "Submission must clear the editor."))))

(deftest editor-area-interprets-unescaped-newlines ()
  (let* ((history-box (cons nil nil))
         (editor (mtm.editor:new-editor :history-box history-box)))
    (mtm.editor::set-editor-buffer-octets
     editor (new-test-octets 108 115 10 45 97 108))
    (multiple-value-bind (action submission)
        (mtm.editor:set-editor-byte editor 13)
      (check (eq action :submit)
             "Submission with an unescaped newline must submit content.")
      (check (equalp submission (new-test-octets 108 115 32 45 97 108))
             "Unescaped newlines must become spaces in submissions.")
      (check (equalp (first (car history-box))
                     (new-test-octets 108 115 10 45 97 108))
             "History must preserve the original newline."))))

(deftest editor-area-keeps-escaped-newlines ()
  (let ((editor (mtm.editor:new-editor)))
    (mtm.editor::set-editor-buffer-octets
     editor (new-test-octets 108 115 32 92 10 45 97 108))
    (multiple-value-bind (action submission)
        (mtm.editor:set-editor-byte editor 13)
      (check (eq action :submit)
             "Submission with an escaped newline must submit content.")
      (check (equalp submission (new-test-octets 108 115 32 92 10 45 97 108))
             "Escaped newlines must keep normal shell semantics."))))

(deftest editor-area-treats-even-backslashes-as-unescaped ()
  (let ((editor (mtm.editor:new-editor)))
    (mtm.editor::set-editor-buffer-octets
     editor (new-test-octets 97 92 92 10 98))
    (multiple-value-bind (action submission)
        (mtm.editor:set-editor-byte editor 13)
      (check (eq action :submit)
             "Submission with even backslashes must submit content.")
      (check (equalp submission (new-test-octets 97 92 92 32 98))
             "Even backslashes must not escape a newline."))))

(deftest editor-area-inserts-newline-before-buffer-end ()
  (let ((editor (mtm.editor:new-editor)))
    (mtm.editor::set-editor-buffer-octets editor (new-test-octets 97 98 99))
    (mtm.editor::set-editor-cursor-home editor)
    (mtm.editor::set-editor-cursor-right editor)
    (multiple-value-bind (action ignored)
        (mtm.editor:set-editor-byte editor 13)
      (declare (ignore ignored))
      (check (eq action :changed)
             "Middle Enter must insert a newline.")
      (check (equalp (mtm.editor::get-editor-buffer editor)
                     (new-test-octets 97 10 98 99))
             "Middle Enter must keep both text parts."))))

(deftest editor-area-ignores-empty-enter ()
  (let ((editor (mtm.editor:new-editor)))
    (multiple-value-bind (action ignored)
        (mtm.editor:set-editor-byte editor 13)
      (declare (ignore ignored))
      (check (eq action :none)
             "Empty Enter must do nothing."))))

(deftest editor-area-detaches-on-empty-ctrl-d ()
  (let ((editor (mtm.editor:new-editor)))
    (multiple-value-bind (action ignored)
        (mtm.editor:set-editor-byte editor 4)
      (declare (ignore ignored))
      (check (eq action :detach)
             "Empty Ctrl-D must detach."))))

(deftest editor-area-deletes-utf8-on-backspace ()
  (let ((editor (mtm.editor:new-editor)))
    (mtm.editor::set-editor-buffer-octets
     editor (new-test-octets #xe4 #xb8 #xad 120))
    (mtm.editor::del-editor-character-backward editor)
    (mtm.editor::del-editor-character-backward editor)
    (check (equalp (mtm.editor::get-editor-buffer editor) (new-test-octets))
           "Backspace must remove one UTF-8 character.")))

(deftest editor-area-selects-text-with-mouse ()
  (let ((editor (mtm.editor:new-editor)))
    (mtm.editor::set-editor-buffer-octets
     editor (get-utf8 (format nil "hello~%world")))
    (let ((render (mtm.editor:set-editor-render editor nil 32 24 0 7)))
      (check (eq (mtm.editor:set-editor-mouse editor render 0 1 7 t)
                 :changed)
             "Mouse press must start a Text selection.")
      (check (eq (mtm.editor:set-editor-mouse editor render 32 6 7 t)
                 :changed)
             "Mouse motion must extend a Text selection.")
      (mtm.editor:set-editor-mouse editor render 32 1 8 t)
      (mtm.editor:set-editor-mouse editor render 0 1 8 nil)
      (check (string= (format nil "hello~%")
                      (mtm.editor:get-editor-selection-text editor))
             "Mouse selection must use Edit buffer positions."))))

(deftest editor-area-replaces-text-selection-on-paste ()
  (let ((editor (mtm.editor:new-editor)))
    (mtm.editor::set-editor-buffer-octets editor (get-utf8 "hello world"))
    (let ((render (mtm.editor:set-editor-render editor nil 32 24 0)))
      (mtm.editor:set-editor-mouse editor render 0 1 1 t)
      (mtm.editor:set-editor-mouse editor render 32 6 1 t)
      (mtm.editor:set-editor-mouse editor render 0 6 1 nil))
    (mtm.editor:set-editor-paste editor (get-utf8 "goodbye"))
    (check (string= "goodbye world"
                    (bytes-to-string (mtm.editor::get-editor-buffer editor)))
           "Pasted content must replace the Text selection.")))

(deftest editor-area-clears-selection-at-mouse-anchor ()
  (let ((editor (mtm.editor:new-editor)))
    (mtm.editor::set-editor-buffer-octets editor (get-utf8 "hello"))
    (let ((render (mtm.editor:set-editor-render editor nil 32 24 0)))
      (mtm.editor:set-editor-mouse editor render 0 1 1 t)
      (mtm.editor:set-editor-mouse editor render 32 6 1 t)
      (check (eq (mtm.editor:set-editor-mouse editor render 32 1 1 t)
                 :changed)
             "Mouse motion back to the anchor must redraw selection.")
      (mtm.editor:set-editor-mouse editor render 0 1 1 nil)
      (check (null (mtm.editor:get-editor-selection-text editor))
             "Returning to the anchor must clear the Text selection."))))

(deftest editor-area-copies-and-cuts-text-selection ()
  (let ((editor (mtm.editor:new-editor)))
    (mtm.editor::set-editor-buffer-octets editor (get-utf8 "hello"))
    (let ((render (mtm.editor:set-editor-render editor nil 32 24 0)))
      (mtm.editor:set-editor-mouse editor render 0 1 1 t)
      (mtm.editor:set-editor-mouse editor render 32 6 1 t)
      (mtm.editor:set-editor-mouse editor render 0 6 1 nil))
    (multiple-value-bind (action text)
        (mtm.editor:set-editor-key editor :copy)
      (check (and (eq action :copy) (string= text "hello"))
             "Copy must return the selected text."))
    (multiple-value-bind (action text)
        (mtm.editor:set-editor-key editor :cut)
      (check (and (eq action :cut) (string= text "hello"))
             "Cut must return the selected text."))
    (mtm.editor:del-editor-selection editor)
    (check (mtm.editor:editor-empty-p editor)
           "Cut must remove the Text selection after clipboard success.")))

(deftest editor-area-deletes-text-selection ()
  (let ((editor (mtm.editor:new-editor)))
    (mtm.editor::set-editor-buffer-octets editor (get-utf8 "hello world"))
    (let ((render (mtm.editor:set-editor-render editor nil 32 24 0)))
      (mtm.editor:set-editor-mouse editor render 0 1 1 t)
      (mtm.editor:set-editor-mouse editor render 32 6 1 t)
      (mtm.editor:set-editor-mouse editor render 0 6 1 nil))
    (check (eq (mtm.editor:set-editor-key editor :delete) :changed)
           "Delete must remove the Text selection.")
    (check (string= " world"
                    (bytes-to-string (mtm.editor::get-editor-buffer editor)))
           "Delete must preserve text outside the selection.")))

(deftest editor-area-history-selects-latest-submission ()
  (let ((editor (mtm.editor:new-editor)))
    (mtm.editor::set-editor-buffer-octets editor (new-test-octets 97))
    (mtm.editor:set-editor-byte editor 13)
    (mtm.editor::set-editor-buffer-octets editor (new-test-octets 98))
    (mtm.editor:set-editor-byte editor 13)
    (check (eq (mtm.editor:set-editor-key editor :up) :changed)
           "History up must select the latest submission.")
    (check (equalp (mtm.editor::get-editor-buffer editor) (new-test-octets 98))
           "History must preserve submission order.")))

(deftest editor-area-shares-session-history ()
  (let* ((box (cons nil nil))
         (first (mtm.editor:new-editor :history-box box))
         (second (mtm.editor:new-editor :history-box box)))
    (mtm.editor::set-editor-buffer-octets first (new-test-octets 109 97 107 101))
    (mtm.editor:set-editor-byte first 13)
    (check (mtm.editor:editor-empty-p first)
           "The first Editor area must clear after Submission.")
    (check (eq (mtm.editor:set-editor-key second :up) :changed)
           "The second Editor area cannot walk Session History.")
    (check (equalp (mtm.editor::get-editor-buffer second)
                   (new-test-octets 109 97 107 101))
           "Shared History must show the submitted Edit buffer.")))

(deftest editor-area-splits-screen-lines-on-newline ()
  (let ((lines (mtm.editor::get-editor-screen-lines
                (new-test-octets 97 10 98) 80 0)))
    (check (= (length lines) 2)
           "Newlines must create separate screen lines.")))

(deftest editor-area-parses-bracketed-paste ()
  (let ((parser (mtm.editor:new-input-parser))
        (input (new-test-octets 27 91 50 48 48 126 97 10 98
                               27 91 50 48 49 126)))
    (let ((events (mtm.editor:set-input-parser-events parser input)))
      (check (= (length events) 1)
             "Paste must produce one event.")
      (check (eq (first (first events)) :paste)
             "Paste must use the paste event.")
      (check (equalp (second (first events)) (new-test-octets 97 10 98))
             "Paste must preserve newlines."))))

(deftest editor-area-parses-application-arrows ()
  (let ((parser (mtm.editor:new-input-parser)))
    (check (equal (mtm.editor:set-input-parser-events
                   parser (new-test-octets 27 79 67))
                  '((:key :right)))
           "Application right arrow must produce a right key event.")
    (check (equal (mtm.editor:set-input-parser-events
                   parser (new-test-octets 27 79 68))
                  '((:key :left)))
           "Application left arrow must produce a left key event.")))

(deftest editor-area-parses-command-key-events ()
  (let ((parser (mtm.editor:new-input-parser)))
    (check (equal (mtm.editor:set-input-parser-events
                   parser (new-test-octets 27 91 57 57 59 57 117))
                  '((:key :copy)))
           "Command-C must produce a copy event.")
    (check (equal (mtm.editor:set-input-parser-events
                   parser (new-test-octets 27 91 49 50 48 59 57 117))
                  '((:key :cut)))
           "Command-X must produce a cut event.")
    (check (equal (mtm.editor:set-input-parser-events
                   parser (new-test-octets 27 91 49 49 56 59 57 117))
                  '((:key :paste)))
           "Command-V must produce a paste event.")))

(deftest editor-area-wraps-cursor-at-column-zero ()
  (let ((editor (mtm.editor:new-editor)))
    (mtm.editor::set-editor-buffer-octets editor (new-test-octets 97 98 99 100))
    (mtm.editor::set-editor-cursor-home editor)
    (mtm.editor::set-editor-cursor-right editor)
    (mtm.editor::set-editor-cursor-right editor)
    (mtm.editor::set-editor-cursor-right editor)
    (let ((render (mtm.editor:set-editor-render editor nil 3 24 0)))
      (check (= (mtm.editor::get-editor-render-cursor-row render) 1)
             "Wrap-boundary cursor must use the next screen row.")
      (check (= (mtm.editor::get-editor-render-cursor-column render) 0)
             "Wrap-boundary cursor must start at column zero."))))

(deftest editor-area-completes-and-navigates-candidates ()
  (let ((editor
          (mtm.editor:new-editor
           :completion-provider
           (lambda (prefix)
             (check (string= prefix "f")
                    "Completion provider received the wrong prefix.")
             '("foo" "far")))))
    (mtm.editor::set-editor-buffer-octets editor (get-utf8 "f"))
    (check (eq (mtm.editor:set-editor-key editor :tab) :changed)
           "Tab must open the Completion menu.")
    (check (mtm.editor:get-editor-completion-active-p editor)
           "The Completion menu must remain active.")
    (check (eq (mtm.editor:set-editor-key editor :down) :changed)
           "Down must move the Completion selection.")
    (check (= (mtm.editor::get-editor-completion-index editor) 1)
           "Down must select the second candidate.")
    (check (eq (mtm.editor:set-editor-key editor :enter) :changed)
           "Enter must accept the selected candidate.")
    (check (string= "far"
                    (bytes-to-string (mtm.editor::get-editor-buffer editor)))
           "Enter must replace the Completion prefix.")
    (check (not (mtm.editor:get-editor-completion-active-p editor))
           "Accepting a candidate must close the Completion menu.")))

(deftest editor-area-completion-closes-before-normal-input ()
  (let ((editor
          (mtm.editor:new-editor
           :completion-provider
           (lambda (prefix)
             (declare (ignore prefix))
             '("foo" "far")))))
    (mtm.editor::set-editor-buffer-octets editor (get-utf8 "f"))
    (mtm.editor:set-editor-key editor :tab)
    (check (eq (mtm.editor:set-editor-byte editor (char-code #\x)) :changed)
           "Normal input must continue after menu dismissal.")
    (check (string= "fx"
                    (bytes-to-string (mtm.editor::get-editor-buffer editor)))
           "Normal input must append after menu dismissal.")
    (check (not (mtm.editor:get-editor-completion-active-p editor))
           "Normal input must close the Completion menu.")))

(deftest editor-area-completion-extends-common-prefix ()
  (let ((editor
          (mtm.editor:new-editor
           :completion-provider
           (lambda (prefix)
             (declare (ignore prefix))
             '("foobar" "foobaz")))))
    (mtm.editor::set-editor-buffer-octets editor (get-utf8 "f"))
    (check (eq (mtm.editor:set-editor-key editor :tab) :changed)
           "Tab must extend a shared Completion prefix.")
    (check (string= "fooba"
                    (bytes-to-string (mtm.editor::get-editor-buffer editor)))
           "Tab must insert the longest shared Completion prefix.")
    (check (mtm.editor:get-editor-completion-active-p editor)
           "A shared prefix extension must keep the menu active.")))

(deftest editor-area-renders-completion-menu-as-ansi ()
  (let ((editor
          (mtm.editor:new-editor
           :completion-provider
           (lambda (prefix)
             (declare (ignore prefix))
             '("foo" "far")))))
    (mtm.editor::set-editor-buffer-octets editor (get-utf8 "f"))
    (mtm.editor:set-editor-key editor :tab)
    (let ((render (mtm.editor:set-editor-render editor nil 40 8 0)))
      (check (= (mtm.editor::get-editor-render-rows render) 3)
             "The render must include two Completion menu rows."))
    (let ((output
            (bytes-to-string
             (capture-platform-output
              (lambda ()
                (mtm.editor:set-editor-render editor 1 40 8 0))))))
      (check (search "foo" output)
             "The ANSI Completion menu must contain the first candidate.")
      (check (search "far" output)
             "The ANSI Completion menu must contain the second candidate.")
      (check (search (format nil "~C[7m" #\Escape) output)
             "The ANSI Completion menu must highlight its selection."))))

(deftest editor-area-flushes-standalone-escape ()
  (let ((parser (mtm.editor:new-input-parser)))
    (check (null (mtm.editor:set-input-parser-events
                 parser (new-test-octets 27)))
           "The parser must keep a possible Escape sequence pending.")
    (check (equal (mtm.editor:set-input-parser-events
                   parser (new-test-octets) :flush-p t)
                  '((:key :escape)))
           "The parser must flush standalone Escape as a key.")))

(deftest editor-area-keeps-keys-until-submission ()
  (let ((frontend
          (mtm.frontend::new-session-frontend :name "alpha" :rows 6 :columns 32)))
    (setf (mtm.frontend::session-frontend-editor frontend)
          (mtm.editor:new-editor)
          (mtm.frontend::session-frontend-input-parser frontend)
          (mtm.editor:new-input-parser))
    (mtm.frontend::set-session-frontend-input
     frontend (new-test-octets 108 115))
    (check (equalp (mtm.editor::get-editor-buffer
                    (mtm.frontend::session-frontend-editor frontend))
                   (new-test-octets 108 115))
           "The Editor area must keep keys before Submission.")))

(deftest frontend-consumes-editor-mouse-reports-without-status-bar ()
  (let* ((editor (mtm.editor:new-editor))
         (frontend
           (mtm.frontend::new-session-frontend
            :rows 24
            :columns 32))
         (render nil))
    (mtm.editor::set-editor-buffer-octets editor (get-utf8 "hello"))
    (setf render (mtm.editor:set-editor-render editor nil 32 24 0))
    (setf (mtm.frontend::session-frontend-editor frontend) editor
          (mtm.frontend::session-frontend-input-parser frontend)
          (mtm.editor:new-input-parser)
          (mtm.frontend::session-frontend-editor-render frontend) render)
    (mtm.frontend::set-session-frontend-input
     frontend
     (get-utf8
      (format nil "~C[<0;1;1M~C[<32;6;1M~C[<0;6;1m"
              #\Escape #\Escape #\Escape)))
    (check (string= "hello"
                    (mtm.editor:get-editor-selection-text editor))
           "The frontend must consume Editor area mouse reports.")))

(deftest frontend-consumes-legacy-editor-mouse-reports ()
  (let* ((editor (mtm.editor:new-editor))
         (frontend
           (mtm.frontend::new-session-frontend
            :rows 24
            :columns 32))
         (render nil))
    (mtm.editor::set-editor-buffer-octets editor (get-utf8 "hello"))
    (setf render (mtm.editor:set-editor-render editor nil 32 24 0))
    (setf (mtm.frontend::session-frontend-editor frontend) editor
          (mtm.frontend::session-frontend-input-parser frontend)
          (mtm.editor:new-input-parser)
          (mtm.frontend::session-frontend-editor-render frontend) render)
    (mtm.frontend::set-session-frontend-input
     frontend
     (new-test-octets
      27 91 77 32 33 33
      27 91 77 64 38 33
      27 91 77 35 38 33))
    (check (and (equalp (mtm.editor::get-editor-buffer editor)
                        (get-utf8 "hello"))
                (string= "hello"
                         (mtm.editor:get-editor-selection-text editor))
                (not (mtm.editor::get-editor-mouse-selecting-p editor)))
           "The frontend must consume legacy Editor mouse reports.")))

(deftest frontend-forwards-full-screen-mouse-reports ()
  (let* ((frontend
           (mtm.frontend::new-session-frontend
            :socket-fd 9
            :full-screen-p t))
         (editor (mtm.editor:new-editor))
         (bytes (get-utf8 (format nil "~C[<0;1;1M" #\Escape))))
    (setf (mtm.frontend::session-frontend-editor frontend) editor)
    (check (equalp
            (capture-platform-output
             (lambda ()
               (mtm.frontend::set-session-frontend-input frontend bytes)))
            bytes)
           "The full-screen frontend must forward mouse reports.")))


(defun session-value-or-nil (name)
  "Return the named Session, or NIL when it is missing."
  (handler-case
      (get-session-value name)
    (error () nil)))

(deftest managed-session-forwards-raw-input-and-output ()
  (new-session-manager)
  (unwind-protect
       (let* ((session (new-session-value "raw"
                                    :shell "/bin/sh"
                                    :width 30
                                    :height 6))
              (attachment (new-attachment "raw")))
         (check attachment
                "The Session does not accept its first Attachment.")
         (check (set-attachment-input
                 attachment
                 (get-utf8
                  (format nil "printf '~A033[31mraw-marker~A033[0m'; sleep 1~%"
                          (string (code-char 92))
                          (string (code-char 92)))))
                "The Attachment rejects raw input.")
         (check
          (attachment-output-has-text-p
           attachment
           (format nil "~C[31mraw-marker~C[0m" #\Escape #\Escape))
          "The Attachment rewrites raw shell output.")
         (check (session-running-p session)
                "The healthy Session stops unexpectedly.")
         (check (attachment-attached-p attachment)
                "The healthy Attachment disconnects unexpectedly."))
    (del-session-manager)))

(deftest detached-session-restores-retained-display ()
  (new-session-manager)
  (unwind-protect
       (let* ((session (new-session-value "screen"
                                    :shell "/bin/sh"
                                    :width 20
                                    :height 4))
              (first (new-attachment "screen")))
         (check (set-attachment-input
                 first
                 (get-utf8 (format nil "printf screen-marker; sleep 1~%")))
                "The first Attachment rejects input.")
         (check (wait-until
                 (lambda ()
                   (screen-has-text-p (get-retained-screen session)
                                      "screen-marker")))
                "The Session does not update its retained display.")
         (set-active-attachment first)
         (check (del-attachment first)
                "The active Attachment does not detach.")
         (check (session-running-p session)
                "Detaching terminates the Session.")
         (let ((second (new-attachment "screen")))
           (check (screen-has-text-p
                   (get-attachment-start-screen second)
                   "screen-marker")
                  "Reattachment loses the retained display.")
           (check (search "screen-marker"
                          (get-terminal-render
                           (get-attachment-start-screen second)))
                  "The retained display cannot render for reattachment.")
           (del-attachment second)))
    (del-session-manager)))

(deftest managed-session-broadcasts-output ()
  (new-session-manager)
  (unwind-protect
       (let* ((session (new-session-value "broadcast" :shell "/bin/sh"))
              (first (new-attachment (session-name session)))
              (second (new-attachment (session-name session))))
         (check (and first second)
                "The Session does not accept multiple Attachments.")
         (check (set-attachment-input
                 first
                 (get-utf8 (format nil "printf broadcast-marker; sleep 1~%")))
                "The writer Attachment rejects input.")
         (check (and (attachment-output-has-text-p first "broadcast-marker")
                     (attachment-output-has-text-p second "broadcast-marker"))
                "The Session does not broadcast output."))
    (del-session-manager)))

(deftest slow-attachment-disconnects-after-buffer-overflow ()
  (new-session-manager :max-buffer-bytes 16384)
  (unwind-protect
       (let* ((session (new-session-value "overflow" :shell "/bin/sh"))
              (slow (new-attachment (session-name session)))
              (writer (new-attachment (session-name session)))
              (stop-drainer-p nil)
              (drainer
                (make-thread
                 (lambda ()
                   (loop until stop-drainer-p
                         do (get-attachment-output writer :wait-p nil)
                            (sleep 0.001)))
                 :name "mtm test output drainer")))
         (unwind-protect
              (progn
                (check (set-attachment-input
                        writer
                        (get-utf8
                         (format nil
                                 "i=0; while [ $i -lt 2000 ]; do printf 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'; i=$((i+1)); done~%")))
                       "The writer Attachment rejects input.")
                (check (wait-until
                        (lambda () (not (attachment-attached-p slow))))
                       "A slow Attachment survives buffer overflow.")
                (check (attachment-attached-p writer)
                       "A healthy Attachment disconnects with the slow one."))
           (setf stop-drainer-p t)
           (join-thread drainer)))
    (del-session-manager)))

(deftest natural-exit-removes-session ()
  (new-session-manager)
  (unwind-protect
       (let* ((session (new-session-value "exit" :shell "/bin/sh"))
              (attachment (new-attachment "exit")))
         (check (set-attachment-input attachment
                                      (get-utf8 (format nil "exit~%")))
                "The Attachment rejects exit input.")
         (check (wait-until (lambda () (null (session-value-or-nil "exit"))))
                "Natural shell exit keeps the Session record.")
         (check (not (attachment-attached-p attachment))
                "Natural shell exit keeps the Attachment connected.")
         (check (not (session-running-p session))
                "Natural shell exit leaves the Session running."))
    (del-session-manager)))

(deftest explicit-termination-stops-session ()
  (new-session-manager)
  (unwind-protect
       (let* ((session (new-session-value "delete" :shell "/bin/sh"))
              (attachment (new-attachment "delete")))
         (check (eq session (del-session "delete"))
                "Explicit Session deletion fails.")
         (check (not (session-running-p session))
                "Explicit deletion leaves the Session running.")
         (check (not (attachment-attached-p attachment))
                "Explicit deletion leaves the Attachment connected.")
         (check (null (session-value-or-nil "delete"))
                "Explicit deletion keeps the named Session."))
    (del-session-manager)))

(deftest session-manager-stops-all-sessions ()
  (new-session-manager)
  (unwind-protect
       (let* ((manager (get-session-manager-value))
              (session (new-session-value "managed" :shell "/bin/sh")))
         (check manager
                "The Session manager does not start.")
         (check (eq manager (get-session-manager-value))
                "The process does not share its Session manager.")
         (check (eq manager (del-session-manager))
                "The Session manager does not stop.")
         (check (null (get-session-manager-value))
                "The Session manager remains globally stored.")
         (check (not (session-running-p session))
                "Stopping the manager leaves a Session running."))
    (when (get-session-manager-value)
      (del-session-manager))))

(defun terminal-settings (fd)
  "Return terminal settings for FD as text."
  (uiop:run-program (list "stty" "-g")
                    :input (format nil "/dev/fd/~D" fd)
                    :output :string))

(deftest raw-terminal-restores-after-normal-exit-and-error ()
  (let ((session (new-shell-session :shell "/bin/sh" :width 80 :height 24)))
    (unwind-protect
        (let ((fd (pty-master session)))
          (let ((before (terminal-settings fd)))
            (set-raw-terminal (lambda () (values)) :fd fd)
            (check (string= before (terminal-settings fd))
                   "Normal raw-terminal exit does not restore settings.")
            (let ((raised nil))
              (handler-case
                  (set-raw-terminal
                   (lambda () (error "expected raw-terminal error"))
                   :fd fd)
                (error () (setf raised t)))
              (check raised
                     "Error raw-terminal body does not propagate.")
              (check (string= before (terminal-settings fd))
                     "Error raw-terminal exit does not restore settings."))))
      (del-shell-session session))))

(defun set-tests ()
  (let ((passed 0)
        (failed 0))
    (dolist (test (reverse *tests*))
      (handler-case
          (progn
            (funcall test)
            (incf passed)
            (format t "PASS ~A~%" test))
        (error (condition)
          (incf failed)
          (format t "FAIL ~A: ~A~%" test condition))))
    (format t "~A passed, ~A failed.~%" passed failed)
    (when (plusp failed)
      (error "The test suite has failures."))
    t))
