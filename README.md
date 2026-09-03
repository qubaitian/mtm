# mtm

MTM means Mouse Terminal Multiplexer.
MTM manages named interactive Shell sessions on a macOS PTY.
It keeps Session management, an Editor area, and Passthrough transport.
The MVP targets SBCL and CFFI.

## Requirements

Use a Common Lisp implementation.
Verification uses SBCL.
Use [OCICL](https://github.com/ocicl/ocicl) for dependencies.
Legacy OpenSSL may need `USE_LEGACY_OPENSSL=1`.
Then run `sbcl --load setup.lisp`.

## Test

Run the test suite from the repository root.

```sh
sbcl --noinform --non-interactive --load init --eval '(asdf:test-system "mtm")'
```

## Build

Build the standalone command from the repository root.

```sh
sbcl --noinform --load init --eval '(asdf:make "mtm/app")' --quit
```

The build writes the executable to `bin/mtm`.

The detached Session manager keeps its executable loaded.
Restart it after rebuilding MTM.
Restarting the manager removes every managed Session.

## Run

Session commands use separate operation and path arguments.
The supported Session operations are `new`, `get`, and `del`.

```sh
./bin/mtm new session-manager
open http://127.0.0.1:7681/
./bin/mtm get session-manager
```

The manager starts the Browser terminal at `http://127.0.0.1:7681/`.
The `new session-manager` command does not open a browser.
Open that address manually to use an ordinary Shell terminal.
The command prints manager state, not the Browser terminal URL.
Use the same `mtm` commands as any other terminal.
The Browser terminal loads xterm.js from its pinned CDN URL.
The Browser terminal starts one ordinary Shell per browser connection.
It does not list or select a managed Session automatically.
`new session-manager` ensures that the manager runs.
`get session-manager` prints its state and all named Sessions.
The stopped result is `state stopped`.
The running result includes one `session` line per Session.
It includes one `service` line per Service.

Load one or more named Services from a Lisp source.

```sh
./bin/mtm new service /absolute/path/services.lisp
./bin/mtm get service web
./bin/mtm set service web stopped
./bin/mtm set service web running
./bin/mtm del service web
```

The source file can declare several Services.
Each Service name must be unique across the manager.
Session and Service names share one namespace.
`get service` prints desired state, observed state, and PID.
`set service` changes only the in-memory desired state.
`del service` removes the current Service registration.
Loading the source again can restore a deleted registration.

Use this declaration form.

```lisp
(mtm:new-service
 :name "web"
 :program '("/opt/web/bin/server"
            "--config"
            "/etc/web/config.sexp")
 :working-directory "/srv/web")
```

`:program` is a non-empty list of strings.
`:working-directory` is optional.
The manager inherits its own working directory by default.
The source file runs as trusted Common Lisp code.


Ensure and enter a named Session.

```sh
./bin/mtm new session s1
./bin/mtm get session s1
```

`new session s1` creates the manager when needed.
It creates `s1` when needed.
It then enters `s1` through the Terminal frontend.
Repeating the command reuses the existing Session.
`get session s1` enters an existing Session.
Both commands require a Session name.

The Editor area intercepts keys before Submission.
Enter at the buffer end submits the Edit buffer.
Unescaped newlines become one space on Submission.
Empty Ctrl-D detaches. The Session keeps running.
Up walks Session History after Reattachment.
The Editor area reads Chinese Completion from user Pinyin dictionary files.
Place one or more `.txt` files in `~/.mtm/dictionaries/` before starting MTM.
Use `character<TAB>pinyin<TAB>weight` lines in each file.
The weight must be a non-negative integer.
Rows with missing or invalid weights are ignored.
MTM reads direct files in filename order.
Higher weights rank candidates earlier.
Equal weights keep file and line order.
MTM reads valid single-character entries only.
Type `hao`, press Tab twice, and accept `好`.
The overlay starts at column 0 and covers the prompt.
Live PTY output stays raw octets.
Click and drag in the Editor area to select text.
The Terminal frontend accepts SGR and legacy mouse reports.
Full-screen applications receive their mouse reports unchanged.
The Browser terminal leaves mouse selection to xterm.js.
Use `Command-X` to cut, `Command-C` to copy, and `Delete` to remove selection.
Use `Command-V` to replace selection or insert Pasted content.
Command shortcuts need terminal support for the Kitty keyboard protocol.
The Session continues while its shell remains alive.

Full-screen terminal applications switch transport automatically.
Run `vim` or another full-screen tool inside a Session.
MTM detects the terminal's alternate-screen entry.
It then sends keyboard bytes directly to the PTY.
MTM detects alternate-screen exit and restores the Editor area.
No extra command or option is needed.
The full-screen application uses the complete Terminal frontend height.
The Session keeps running after frontend detachment.

Delete a named Session or stop the manager.

```sh
./bin/mtm del session s1
./bin/mtm del session-manager
```

`del session <name>` removes that named Session.
Repeating the command has no additional effect.
`del session-manager` removes every Session and stops the manager.
Repeating that command has no additional effect.
Bare `get session` and `del session` are invalid.
There is no Session `set` command.
Invalid operations report errors and return a non-zero status.
The manager stays alive after each command exits.

MTM has no evaluator or debug mode.

## Lisp interface

The public data interface uses named functions.
The Session manager is global within one Lisp process.

```lisp
(mtm:new-session-manager)
(mtm:get-session-manager)
(mtm:new-session "s1")
(mtm:get-session "s1")
(mtm:get-session-list)
(mtm:del-session "s1")
(mtm:del-session-manager)
(mtm:new-service
 :name "web"
 :program '("/opt/web/bin/server" "--config" "/etc/web/config.sexp")
 :working-directory "/srv/web")
(mtm:get-service "web")
(mtm:get-service-list)
(mtm:get-service-output "web")
(mtm:set-service "web" :stopped)
(mtm:del-service "web")
(mtm:new-service-source "/absolute/path/services.lisp")
```

`new-session-manager` ensures the process-global manager.
`get-session-manager` returns a state and Session snapshot.
The running snapshot uses `(:state :running :sessions ...)`.
The stopped snapshot uses `(:state :stopped :sessions nil)`.
`new-session` ensures the manager and named Session.
It then enters the Session through the Terminal frontend.
`get-session` requires a name and enters that Session.
`get-session-list` returns Session names and states.
`del-session` requires a name and removes that Session.
`del-session-manager` removes every Session and stops the manager.
Session values expose no user-editing operation.
`new-service` creates one Service in a Lisp process.
`new-service-source` loads several Service declarations atomically.
`get-service` returns one Service state snapshot.
`get-service-list` returns Service names and observed states.
`get-service-output` returns bounded recent output octets.
`set-service` changes the desired state to `:running` or `:stopped`.
`del-service` removes one current Service registration.
