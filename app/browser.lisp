(in-package #:mtm.app)

(defparameter +browser-address+ "127.0.0.1")
(defparameter +browser-port+ 7681)
(defparameter +browser-session-prefix+ "/session/")
(defparameter +browser-close-normal-code+ 1000)
(defparameter +browser-close-policy-code+ 1008)
(defparameter +browser-close-retry-code+ 1013)

(defun get-browser-url ()
  "Return the local Browser frontend URL."
  (format nil "http://~A:~D/" +browser-address+ +browser-port+))

(defun new-browser-response (status content-type body)
  "Return a Clack response with one BODY string."
  (list status
        (list :content-type content-type)
        (list body)))

(defun new-browser-not-found-response ()
  "Return the Browser frontend's not-found response."
  (new-browser-response 404
                    "text/plain; charset=utf-8"
                    (format nil "Not found.~%")))

(defun get-browser-escaped-html (text)
  "Escape TEXT for an HTML text or attribute position."
  (with-output-to-string (output)
    (loop for character across (princ-to-string text)
          do (case character
               (#\& (write-string "&amp;" output))
               (#\< (write-string "&lt;" output))
               (#\> (write-string "&gt;" output))
               (#\" (write-string "&quot;" output))
               (#\' (write-string "&#39;" output))
               (otherwise (write-char character output))))))

(defun get-browser-session-name (path)
  "Return the Session name in PATH, or NIL."
  (when (and (stringp path)
             (uiop:string-prefix-p +browser-session-prefix+ path))
    (let ((name (subseq path (length +browser-session-prefix+))))
      (and (plusp (length name))
           name))))

(defun new-browser-index-page ()
  "Return the Browser frontend's Session list page."
  (with-output-to-string (html)
    (write-string
     "<!doctype html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
  <title>MTM sessions</title>
</head>
<body>
  <h1>MTM sessions</h1>
"
     html)
    (let ((sessions (get-session-list)))
      (if sessions
          (dolist (session sessions)
            (let ((name (get-browser-escaped-html (car session)))
                  (state (get-browser-escaped-html
                          (string-downcase (princ-to-string (cdr session))))))
              (format html
                      "  <p><a href=\"/session/~A\">~A</a> <small>~A</small></p>~%"
                      name
                      name
                      state)))
          (write-string "  <p>No sessions.</p>
"                       html)))
    (write-string
     "</body>
</html>
"
     html)))

(defun new-browser-session-page (name width height)
  "Return the xterm.js page for NAME with fixed terminal dimensions."
  (with-output-to-string (html)
    (format html
            "<!doctype html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
  <title>MTM: ~A</title>
  <link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/@xterm/xterm@5.5.0/css/xterm.css\">
  <style>
    html, body { width: 100%; height: 100%; margin: 0; background: #111; }
    #terminal { width: 100vw; height: 100vh; padding: 8px; box-sizing: border-box; }
  </style>
</head>
<body>
  <div id=\"terminal\"></div>
  <script src=\"https://cdn.jsdelivr.net/npm/@xterm/xterm@5.5.0/lib/xterm.js\"></script>
  <script>
    const columns = ~D;
    const rows = ~D;
    const encoder = new TextEncoder();
    const terminal = new Terminal({ cols: columns, rows: rows, cursorBlink: true });
    terminal.open(document.getElementById(\"terminal\"));
    let socket = null;
    let ended = false;

    function inputFrame(bytes) {
      const frame = new Uint8Array(bytes.length + 1);
      frame[0] = 0;
      frame.set(bytes, 1);
      return frame;
    }

    function connect() {
      const scheme = location.protocol === \"https:\" ? \"wss\" : \"ws\";
      socket = new WebSocket(scheme + \"://\" + location.host + location.pathname, [\"tty\"]);
      socket.binaryType = \"arraybuffer\";
      socket.addEventListener(\"open\", () => {
        socket.send(encoder.encode(JSON.stringify({ columns: columns, rows: rows })));
      });
      socket.addEventListener(\"message\", (event) => {
        const frame = new Uint8Array(event.data);
        if (frame.length > 0 && frame[0] === 0) {
          terminal.write(frame.slice(1));
        }
      });
      socket.addEventListener(\"close\", (event) => {
        if (event.code === 1000) {
          ended = true;
          terminal.write(\"\\r\\n[Session ended]\\r\\n\");
        } else if (event.code === 1008) {
          ended = true;
          terminal.write(\"\\r\\n[Connection rejected]\\r\\n\");
        } else if (!ended) {
          terminal.write(\"\\r\\n[Reconnecting...]\\r\\n\");
          window.setTimeout(connect, 1000);
        }
      });
    }

    terminal.onData((data) => {
      if (socket && socket.readyState === WebSocket.OPEN) {
        socket.send(inputFrame(encoder.encode(data)));
      }
    });
    connect();
  </script>
</body>
</html>
"
            (get-browser-escaped-html name)
            width
            height)))

(defun get-browser-message-string (message)
  "Decode a Browser handshake MESSAGE as UTF-8 text."
  (typecase message
    (string message)
    ((vector (unsigned-byte 8))
     (babel:octets-to-string message :encoding :utf-8))
    (otherwise
     (error "The Browser handshake must be text or bytes."))))

(defun get-browser-handshake-size (message)
  "Return the columns and rows from a Browser handshake."
  (let* ((object (yason:parse (get-browser-message-string message)
                              :object-as :alist))
         (columns (cdr (assoc "columns" object :test #'string=)))
         (rows (cdr (assoc "rows" object :test #'string=))))
    (unless (and (integerp columns)
                 (plusp columns)
                 (integerp rows)
                 (plusp rows))
      (error "The Browser handshake has an invalid terminal size."))
    (values columns rows)))

(defun get-browser-handshake (session message)
  "Check that MESSAGE matches SESSION's fixed terminal size."
  (multiple-value-bind (columns rows)
      (get-browser-handshake-size message)
    (multiple-value-bind (session-width session-height)
        (get-session-size session)
      (unless (and (= columns session-width)
                   (= rows session-height))
        (error "The Browser terminal size does not match the Session."))))
  t)

(defun browser-input-message-p (message)
  "Return true when MESSAGE is a raw input frame."
  (and (typep message '(vector (unsigned-byte 8)))
       (plusp (length message))
       (zerop (aref message 0))))

(defun get-browser-output-frame (bytes)
  "Prefix raw BYTES with the ttyd-compatible output marker."
  (check-type bytes (vector (unsigned-byte 8)))
  (let ((frame (make-array (1+ (length bytes))
                           :element-type '(unsigned-byte 8))))
    (setf (aref frame 0) 0)
    (replace frame bytes :start1 1)
    frame))

(defun get-browser-close-code (session)
  "Return the close code for SESSION's current state."
  (if (session-running-p session)
      +browser-close-retry-code+
      +browser-close-normal-code+))

(defun set-browser-output (websocket attachment)
  "Forward ATTACHMENT output through WEBSOCKET."
  (handler-case
      (progn
        (websocket-driver:send-binary
         websocket
         (get-browser-output-frame
          (get-utf8
           (get-terminal-render
            (get-attachment-start-screen attachment)))))
        (loop
          (multiple-value-bind (bytes end-p)
              (get-attachment-output attachment)
            (when (and bytes (plusp (length bytes)))
              (websocket-driver:send-binary
               websocket
               (get-browser-output-frame bytes)))
            (when end-p
              (websocket-driver:close-connection
               websocket
               ""
               (get-browser-close-code
                (attachment-session attachment)))
              (return)))))
    (error ()
      (ignore-errors
        (websocket-driver:close-connection
         websocket
         ""
         (get-browser-close-code (attachment-session attachment)))))))

(defun set-browser-websocket (env session)
  "Return a streaming response for SESSION's Browser Attachment."
  (let ((websocket (websocket-driver:make-server
                    env
                    :accept-protocols '("tty")))
        (attachment nil)
        (output-thread nil))
    (websocket-driver:on
     :message
     websocket
     (lambda (message)
       (if attachment
           (when (browser-input-message-p message)
             (set-attachment-input attachment (subseq message 1)))
           (handler-case
               (progn
                 (get-browser-handshake session message)
                 (setf attachment (new-attachment (session-name session))
                       output-thread
                       (make-thread
                        (lambda ()
                          (set-browser-output websocket attachment))
                        :name "mtm browser output")))
             (error ()
               (websocket-driver:close-connection
                websocket
                ""
                (if (session-running-p session)
                    +browser-close-policy-code+
                    +browser-close-normal-code+)))))))
    (websocket-driver:on
     :close
     websocket
     (lambda (&key code reason)
       (declare (ignore code reason))
       (when attachment
         (ignore-errors (del-attachment attachment)))))
    (lambda (responder)
      (declare (ignore responder))
      (unwind-protect
           (websocket-driver:start-connection websocket)
        (when attachment
          (ignore-errors (del-attachment attachment)))
        (when (and output-thread
                   (not (eq output-thread (current-thread))))
          (ignore-errors (join-thread output-thread)))))))

(defun browser-websocket-p (env)
  "Return true when ENV requests a WebSocket upgrade."
  (and (hash-table-p (getf env :headers))
       (websocket-driver:websocket-p env)))

(defun set-browser-request (env)
  "Serve the Browser frontend and its WebSocket route."
  (let* ((path (getf env :path-info))
         (websocket-p (browser-websocket-p env))
         (name (get-browser-session-name path)))
    (cond
      ((and (stringp path)
            (not websocket-p)
            (string= path "/"))
       (new-browser-response 200
                         "text/html; charset=utf-8"
                         (new-browser-index-page)))
      ((and name)
       (handler-case
           (let ((session (get-session name)))
             (if websocket-p
                 (set-browser-websocket env session)
                 (multiple-value-bind (width height)
                     (get-session-size session)
                   (new-browser-response 200
                                     "text/html; charset=utf-8"
                                     (new-browser-session-page name width height)))))
         (error ()
           (new-browser-not-found-response))))
      (t
       (new-browser-not-found-response)))))

(defun new-browser-server ()
  "Start the local Browser frontend server."
  (clack:clackup #'set-browser-request
                 :server :hunchentoot
                 :address +browser-address+
                 :port +browser-port+
                 :use-thread t
                 :debug nil
                 :silent t))

(defun del-browser-server (server)
  "Stop the Browser frontend server."
  (when server
    (clack:stop server)))
