# Keep named Sessions with local frontend input

## Status

accepted

## Decision

MTM keeps one long-lived Session manager.
It owns named interactive Shell sessions.
Each Session supports multiple Attachments.
Each Attachment has a bounded output buffer.

The Terminal frontend uses an Editor area for normal input.
It sends submitted input to the Session's PTY.
It forwards live PTY output without rewriting.
The manager keeps a Terminal emulator projection for reattachment.
Reattachment receives that projection before new PTY bytes.

The Session manager status bar shows named Sessions locally.
The bar stays outside the Session's retained display.
It reserves terminal rows when full-screen transport runs.

Natural shell exit removes the Session immediately.
Named deletion terminates its shell and Attachments.
Detachment leaves a running Session available.
Normal Sessions keep their configured dimensions.
Full-screen transport may reduce PTY height for the status bar.

The CLI and Lisp API use named Session operations.
Session creation ensures the manager when needed.
Session entry requires a Session name.
The Session value has no user-editing operation.
