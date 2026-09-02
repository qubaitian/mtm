# Make Session manager lifecycle operations idempotent

## Status

accepted

## Decision

The Session manager is a process-global singleton value.
`new session-manager` ensures that the manager exists.
`get session-manager` reports its state and named Sessions.
`del session-manager` stops it and removes every Session.

The manager starts automatically when `new session <name>` needs it.
Repeated creation returns the existing manager.
Repeated manager deletion reports the stopped state.
Named Session deletion also has no effect after removal.

The manager owns every Session and Attachment.
Stopping the manager terminates every Session and disconnects clients.
Restarting the manager starts with no named Sessions.
