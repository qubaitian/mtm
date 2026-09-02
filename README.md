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

The Session manager status bar stays visible during Terminal use.
The bar shows the number of running Sessions.
Click the bar to expand or collapse Session rows.
Click a Session row to enter that Session.
Press `Esc` to collapse expanded rows.

The Editor area intercepts keys before Submission.
Enter at the buffer end submits the Edit buffer.
Unescaped newlines become one space on Submission.
Empty Ctrl-D detaches. The Session keeps running.
Up walks Session History after Reattachment.
The overlay starts at column 0 and covers the prompt.
Live PTY output stays raw octets.
Click and drag in the Editor area to select text.
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
The status bar remains visible and reserves terminal rows.
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
