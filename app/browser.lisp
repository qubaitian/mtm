(in-package #:mtm.app)

;; Define the loopback address used by the Browser terminal.
(defparameter +browser-address+ "127.0.0.1")

;; Define the local port used by the Browser terminal.
(defparameter +browser-port+ 7681)

;; Define the normal WebSocket close code.
(defparameter +browser-close-normal-code+ 1000)

;; Define the policy close code for an invalid handshake.
(defparameter +browser-close-policy-code+ 1008)

;; Return the local URL for the Browser terminal.
(defun get-browser-url ()
  "Return the local Browser terminal URL."
  (format nil "http://~A:~D/" +browser-address+ +browser-port+))

;; Build a Clack response with one BODY string.
(defun new-browser-response (status content-type body)
  "Return a Clack response with one BODY string."
  (list status
        (list :content-type content-type)
        (list body)))

;; Build the not-found response for unsupported Browser routes.
(defun new-browser-not-found-response ()
  "Return the Browser terminal's not-found response."
  (new-browser-response 404
                        "text/plain; charset=utf-8"
                        (format nil "Not found.~%")))

;; Build the xterm.js page for one ordinary Browser terminal.
(defun new-browser-terminal-page ()
  "Return the xterm.js page for an ordinary Browser terminal."
  (with-output-to-string (html)
    (write-string
     "<!doctype html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
  <title>MTM browser terminal</title>
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
    const columns = 80;
    const rows = 24;
    const encoder = new TextEncoder();
    const terminal = new Terminal({ cols: columns, rows: rows, cursorBlink: true });
    terminal.open(document.getElementById(\"terminal\"));
    let socket = null;

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
        terminal.write(event.code === 1000
          ? \"\\r\\n[Shell exited]\\r\\n\"
          : \"\\r\\n[Terminal disconnected]\\r\\n\");
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
     html)))

;; Decode one Browser handshake message as UTF-8 text.
(defun get-browser-message-string (message)
  "Decode a Browser handshake MESSAGE as UTF-8 text."
  (typecase message
    (string message)
    ((vector (unsigned-byte 8))
     (babel:octets-to-string message :encoding :utf-8))
    (otherwise
     (error "The Browser handshake must be text or bytes."))))

;; Read the terminal size from one Browser handshake.
(defun get-browser-handshake-size (message)
  "Return the columns and rows from a Browser handshake."
  (let* (;; Parse the browser's initial terminal-size object.
         (object (yason:parse (get-browser-message-string message)
                              :object-as :alist))
         ;; Read the requested terminal column count.
         (columns (cdr (assoc "columns" object :test #'string=)))
         ;; Read the requested terminal row count.
         (rows (cdr (assoc "rows" object :test #'string=))))
    (unless (and (integerp columns)
                 (plusp columns)
                 (integerp rows)
                 (plusp rows))
      (error "The Browser handshake has an invalid terminal size."))
    (values columns rows)))

;; Identify a raw input frame from the Browser terminal.
(defun browser-input-message-p (message)
  "Return true when MESSAGE is a raw input frame."
  (and (typep message '(vector (unsigned-byte 8)))
       (plusp (length message))
       (zerop (aref message 0))))

;; Prefix raw BYTES with the ttyd-compatible output marker.
(defun get-browser-output-frame (bytes)
  "Prefix raw BYTES with the ttyd-compatible output marker."
  (check-type bytes (vector (unsigned-byte 8)))
  (let (;; Allocate one marker byte before the raw output.
        (frame (make-array (1+ (length bytes))
                           :element-type '(unsigned-byte 8))))
    (setf (aref frame 0) 0)
    (replace frame bytes :start1 1)
    frame))

;; Forward ordinary Shell output through the Browser WebSocket.
(defun set-browser-output (websocket shell-session)
  "Forward SHELL-SESSION output through WEBSOCKET."
  (handler-case
      (loop
        (multiple-value-bind (bytes end-p)
            (get-shell-output-bytes shell-session)
          (when (and bytes (plusp (length bytes)))
            (websocket-driver:send-binary
             websocket
             (get-browser-output-frame bytes)))
          (when end-p
            (websocket-driver:close-connection
             websocket
             ""
             +browser-close-normal-code+)
            (return))))
    (error ()
      (ignore-errors
        (websocket-driver:close-connection
         websocket
         ""
         +browser-close-normal-code+)))))

;; Run one ordinary Shell behind a Browser WebSocket.
(defun set-browser-websocket (env)
  "Return a streaming response for one ordinary Browser terminal."
  (let (;; Keep the WebSocket for the whole Browser connection.
        (websocket (websocket-driver:make-server
                    env
                    :accept-protocols '("tty")))
        ;; Keep the ordinary Shell created for this Browser connection.
        (shell-session nil)
        ;; Keep the output reader so cleanup can join it.
        (output-thread nil))
    (websocket-driver:on
     :message
     websocket
     (lambda (message)
       (if shell-session
           (when (browser-input-message-p message)
             (set-shell-input shell-session (subseq message 1)))
           (handler-case
               (multiple-value-bind (columns rows)
                   (get-browser-handshake-size message)
                 (setf shell-session
                       (new-shell-session :width columns :height rows)
                       output-thread
                       (make-thread
                        (lambda ()
                          (set-browser-output websocket shell-session))
                        :name "mtm browser shell output")))
             (error ()
               (websocket-driver:close-connection
                websocket
                ""
                +browser-close-policy-code+))))))
    (websocket-driver:on
     :close
     websocket
     (lambda (&key code reason)
       (declare (ignore code reason))
       (when shell-session
         (ignore-errors (del-shell-session shell-session)))))
    (lambda (responder)
      (declare (ignore responder))
      (unwind-protect
           (websocket-driver:start-connection websocket)
        (when shell-session
          (ignore-errors (del-shell-session shell-session)))
        (when (and output-thread
                   (not (eq output-thread (current-thread))))
          (ignore-errors (join-thread output-thread)))))))

;; Identify Browser requests that request a WebSocket upgrade.
(defun browser-websocket-p (env)
  "Return true when ENV requests a WebSocket upgrade."
  (and (hash-table-p (getf env :headers))
       (websocket-driver:websocket-p env)))

;; Serve the single ordinary Browser terminal route.
(defun set-browser-request (env)
  "Serve the Browser terminal and its WebSocket route."
  (let (;; Read the requested URL path.
        (path (getf env :path-info))
        ;; Detect the WebSocket form of the root route.
        (websocket-p (browser-websocket-p env)))
    (if (and (stringp path)
             (string= path "/"))
        (if websocket-p
            (set-browser-websocket env)
            (new-browser-response 200
                                  "text/html; charset=utf-8"
                                  (new-browser-terminal-page)))
        (new-browser-not-found-response))))

;; Start the local Browser terminal server.
(defun new-browser-server ()
  "Start the local Browser terminal server."
  (clack:clackup #'set-browser-request
                 :server :hunchentoot
                 :address +browser-address+
                 :port +browser-port+
                 :use-thread t
                 :debug nil
                 :silent t))

;; Stop the local Browser terminal server.
(defun del-browser-server (server)
  "Stop the Browser terminal server."
  (when server
    (clack:stop server)))
