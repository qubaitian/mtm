# MTM (Mouse Terminal Multiplexer)

This context defines shell Sessions, terminal display, and the Editor area.
It does not define implementation details.

## Process

**Shell session**:
A running interactive shell with a controlling PTY.
_Avoid_: command, terminal process

**Selected shell**:
The shell selected by the user's `$SHELL` setting.
_Avoid_: zsh

**Session manager**:
A process-global registry that owns shell Sessions, Services, and Attachments.
It controls their lifecycles and lists their names and states.
It has a `running` or `stopped` Manager state.
_Avoid_: session service, session daemon

**Manager state**:
The lifecycle state of the Session manager.
`running` accepts requests, while `stopped` accepts none.
_Avoid_: service status, process state

**Session name**:
A required human-readable label unique within one Session manager.
All callers use it to select a Session.
_Avoid_: process ID

**Session list**:
The named Sessions available through the Session manager.
Each listed Session includes its name and Manager state.
_Avoid_: session picker, session menu

**PTY**:
A pseudo-terminal pair that gives a child process terminal behavior.
_Avoid_: pipe

**PTY master**:
The endpoint used for Session input and output.
_Avoid_: shell terminal

**PTY slave**:
The endpoint used as the shell's controlling terminal.
_Avoid_: child pipe

**Session termination**:
The end of a Shell session after natural exit or deletion.
It does not mean Attachment detachment.
_Avoid_: detachment

## Services

**Service**:
A named external program managed by the Session manager.
It is separate from a Shell Session and has no interactive Attachment.
_Avoid_: Shell Session, session service, daemon

**Service name**:
A unique human-readable string within one Session manager.
_Avoid_: process ID, executable name, keyword name

**Service specification**:
A trusted declaration of one Service's program and execution context.
_Avoid_: service command, shell command

**Service source**:
A trusted Lisp file that declares one or more Services.
Each Service source owns the declarations that it contains.
_Avoid_: service list, service registry

**Service output**:
Recent output emitted by a Service.
It can be viewed without sending input to the Service.
_Avoid_: Session output, log file

**Service row**:
A status bar row for one Service.
Selecting it shows recent Service output without interactive input.
_Avoid_: Session row, service status line

## Display

**Terminal frontend**:
A component that connects one Attachment to a terminal.
It can use an Editor area for local input.
It uses full-screen transport for live PTY input.
_Avoid_: shell session, command frontend

**Browser frontend**:
A Terminal frontend that runs in a web browser.
It connects one Attachment to a managed Session.
_Avoid_: web session, browser session, ttyd session

**Browser terminal**:
A browser-hosted ordinary terminal for one Shell session.
It does not select a managed Session by default.
Users use normal MTM operations to enter or create managed Sessions.
_Avoid_: browser session, web session

**Shared output**:
Raw PTY output that every attached frontend can observe.
_Avoid_: frontend-local output

**Attachment**:
A connection between a terminal frontend and a Session.
Each Attachment has its own output buffer.
_Avoid_: terminal session

**Detachment**:
The state where a frontend disconnects while its Session continues.
_Avoid_: session close, termination

**Reattachment**:
A new Attachment to an existing running Session.
It receives the retained display before new output.
_Avoid_: session restart

**Retained display**:
The latest screen projection kept while a Session runs detached.
Reattachment starts with this projection.
It is not retained after Session termination.
_Avoid_: cleared screen

**Passthrough mode**:
Live PTY transport of raw octets.
It does not rewrite live output.
_Avoid_: raw mode

**Terminal emulator**:
An internal display projection for retained Session screens.
It decodes UTF-8 and handles supported ANSI sequences.
It does not change live raw output.
_Avoid_: terminal renderer

**Screen grid**:
A rectangular display made of character cells and styles.
_Avoid_: terminal buffer

**Terminal size**:
The PTY width and height measured in character cells.
Normal Sessions use their configured dimensions.
Full-screen transport uses the attached Terminal frontend's dimensions.
_Avoid_: pixel size

## Transport

**Raw terminal octets**:
The exact bytes sent through a PTY Attachment.
MTM does not decode or rewrite live transport bytes.
_Avoid_: terminal text

**UTF-8 projection text**:
Text decoded only for the retained display projection.
Malformed sequences use the replacement character.
_Avoid_: live output text

**ANSI control sequence**:
A byte sequence that changes terminal display or cursor state.
_Avoid_: escape string

## Mouse input

**Mouse report**:
A terminal input sequence that describes a mouse action and cell coordinates.
A Terminal frontend consumes it locally or forwards it unchanged.
_Avoid_: mouse command

**Mouse tracking**:
A terminal mode that requests Mouse reports from the Terminal frontend.
It applies while the local status bar is active.
_Avoid_: mouse capture

## Data operations

**Data operation**:
One of `new`, `set`, `get`, or `del`.
It manages one value position.
_Avoid_: command

**Operation prefix**:
The leading verb for a project-defined operation function.
It uses only `new`, `set`, `get`, or `del`.
`set` covers state changes, workflows, and output.
`get` covers reads and derived values.
Predicates, required external names, generated names, and test names are exceptions.
_Avoid_: start, attach, lookup, terminate

**Value position**:
An address that contains one non-`nil` value or no value.
_Avoid_: empty object

**Missing position**:
A value position with no value.
`new` creates the missing value.
_Avoid_: empty value

**Existing position**:
A value position with a non-`nil` value.
`new` reuses it, while `get` reads it.
`del` removes it.
_Avoid_: occupied value

**Session manager position**:
A singleton value position for the Session manager.
`new` ensures it, while `get` returns its state and Session list.
`del` stops it and removes every Session.
Creating a Session ensures the manager when needed.
_Avoid_: service position

## Editor

**Editor area**:
The local text area before Submission.
It belongs to one Terminal frontend.
_Avoid_: input box, command box, command frontend

**Edit buffer**:
The complete text now held by one Editor area.
It belongs to one Attachment.
Detachment discards it.
_Avoid_: input value, command string, draft

**Submission**:
The act that sends a non-empty Edit buffer to the Session.
_Avoid_: send, confirm

**Unescaped newline**:
A newline preceded by zero or an even number of backslashes.
Submission turns it into one ASCII space.

**Escaped newline**:
A newline preceded by an odd number of consecutive backslashes.
Submission keeps it for the shell.

**Empty editor**:
The state where the Edit buffer holds no characters.
_Avoid_: blank content

**Buffer end**:
The absolute end of the Edit buffer.
It is not the line end.
_Avoid_: line end, last line

**Multiline editing**:
An Edit buffer that can hold more than one text line.
_Avoid_: multiline input, newline mode

**History**:
The sequence of Edit buffer contents saved by Submission.
It belongs to one Session.
Reattachment can walk it.
_Avoid_: command history, input history, edit history

**Viewport**:
The text range now visible in the Editor area.
It uses the complete Terminal frontend height.
_Avoid_: visible area, scrolling window

**Pasted content**:
Complete text inserted at once, including newlines.
_Avoid_: pasted text

**Insertion point**:
The position in an Edit buffer where new text enters.
_Avoid_: editor cursor, caret

**Text selection**:
A non-empty continuous range of characters in an Edit buffer.
_Avoid_: terminal selection, output selection

**System clipboard**:
The operating system text store used to exchange Pasted content.
_Avoid_: internal clipboard, application clipboard

## Interactive applications

**Full-screen terminal application**:
A program that controls the terminal screen and reads individual key events.
_Avoid_: Vim mode, terminal editor

**Full-screen transport**:
Automatic Session transport for an alternate-screen terminal application.
It sends keyboard bytes directly to the PTY.
It returns to the Editor area after alternate-screen exit.
_Avoid_: raw mode, Vim mode

**Alternate-screen exit**:
The event that returns every Attachment to the Editor area.
The Session continues running.
_Avoid_: mode reset, application detachment
