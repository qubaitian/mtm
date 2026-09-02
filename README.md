# mtm

MTM means Mouse Terminal Multiplexer.
MTM manages named interactive Shell sessions on a macOS PTY.
It keeps Session management, an Editor area, and Passthrough transport.
The MVP targets SBCL and CFFI.

## Requirements

Use a Common Lisp implementation.
Current verification uses SBCL.
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
sbcl --noinform --load init \
  --eval '(asdf:make "mtm/app")' \
  --quit
```

The build writes the executable to `bin/mtm`.

## Run

The CLI uses four data operations.
The operation, path, and value use separate arguments.

Start the Session manager before other commands.

```sh
./bin/mtm new session-manager
./bin/mtm get session-manager
```

The manager makes the Browser terminal available at `http://127.0.0.1:7681/`.
Open that address to use an ordinary Shell terminal in the browser.
Use the same `mtm` commands as any other terminal.
The Browser terminal loads xterm.js from its pinned CDN URL.

The manager state is `running` or `stopped`.
The `new` operation rejects a running manager.
The `del` operation stops the manager and all Sessions.

Create and list named Sessions.

```sh
./bin/mtm new session s1
./bin/mtm get session
```

Enter a Session through the Terminal frontend.

```sh
./bin/mtm set current-session s1
```

The Editor area intercepts keys before Submission.
Enter at the buffer end submits the Edit buffer.
Unescaped newlines become one space on Submission.
Empty Ctrl-D detaches. The Session keeps running.
Up walks Session History after Reattachment.
The overlay starts at column 0 and covers the prompt.
Live PTY output stays raw octets.
The frontend shows a green Session manager status bar at the bottom.
Click the bar to expand or collapse the Session rows.
Click a Session row to enter that Session.
Press `Esc` to collapse expanded rows.
Click and drag in the Editor area to select text.
Use `Command-X` to cut, `Command-C` to copy, and `Delete` to remove selection.
Use `Command-V` to replace selection or insert Pasted content.
Command shortcuts need terminal support for the Kitty keyboard protocol.
The Session continues while its shell remains alive.

Enter Application passthrough for a full-screen terminal application.

```sh
./bin/mtm set current-session s1 --application
```

Application passthrough sends every keyboard byte directly to the PTY.
This supports Vim and other full-screen terminal applications.
The status bar stays visible and reserves its terminal rows.
MTM updates the PTY size after the frontend resizes.
Vim leaving its alternate screen restores the Editor area.
An application without an alternate screen stays in passthrough.
Close the frontend to detach its Attachment.
The Session continues running after detachment.

Delete a Session or stop the manager.

```sh
./bin/mtm del session s1
./bin/mtm del session-manager
```

`new session <name>` creates a named Session without entering it.
`get session` lists named Sessions.
`del session <name>` terminates and removes one Session.
`set current-session <name>` enters an existing Session.
Invalid operations report errors and return a non-zero status.
The manager stays alive after each command exits.
The manager does not start automatically.

MTM has no evaluator or debug mode.

## Lisp interface

The public data interface uses named functions.
The Session manager is global within one Lisp process.

```lisp
(mtm:new-session-manager)
(mtm:get-session-manager)
(mtm:new-session "s1")
(mtm:get-session-list)
(mtm:set-current-session "s1")
(mtm:set-current-session "s1" :application-p t)
(mtm:get-current-session)
(mtm:del-current-session)
(mtm:del-session "s1")
(mtm:del-session-manager)
```

`new-session` creates the global manager when it is missing.
`get-session-manager` returns the manager or `NIL`.
`get-session-list` returns Session names and states.
`set-current-session` enters the Terminal frontend for one Session name.
Use `:application-p t` for Application passthrough.
`del-current-session` detaches the current Attachment without arguments.
The CLI does not support `get current-session`.
