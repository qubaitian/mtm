# Keep only Passthrough mode and Session management

The earlier design included a command frontend and a status line.
It also included input editing, history, evaluation, execution, and debug logs.
Those features increase the product surface without serving Session management.

## Status

accepted

## Decision

MTM keeps only these user-facing capabilities:

- A long-lived Session manager.
- Named interactive Shell sessions.
- Multiple Attachments for one running Session.
- One strict Passthrough frontend.

Passthrough input and output use raw PTY octets.
The frontend performs no local key handling.
The frontend performs no live output rewriting.

The Session manager owns each PTY and its background reader.
Each Attachment has a bounded output buffer.
Overflow disconnects only the slow Attachment.
Detachment keeps a running Session alive.
Reattachment remains available while that Session runs.

The manager keeps an internal Terminal emulator projection.
The projection supports retained display and reattachment.
It does not change live raw output.
Reattachment receives the projection before new PTY bytes.

Session size is fixed when the Session starts.
MTM reserves no status line.
Natural shell exit removes the Session immediately.
Explicit Session deletion terminates its shell and Attachments.

The data API keeps `new`, `set`, `get`, and `del`.
It keeps the `session-manager`, `session`, and `current-session` positions.
The CLI exposes manager and Session lifecycle operations.
The CLI enters Passthrough mode with `set current-session`.

## Conflicts with earlier ADRs

This decision supersedes the status line in ADR-0004.
It narrows the retained display behavior from ADR-0005.
It removes the command frontend and execution states from ADR-0006.
It removes debug and closed-session behavior from ADR-0008.
It keeps the lifecycle and four-operation decisions.

## Consequences

MTM has no command editor or input history.
MTM has no language detection or evaluator.
MTM has no execution state or execution worker.
MTM has no diagnostic logger or debug position.
MTM has no status-row reservation.
The internal display projection remains required for reattachment.
