# Manage detachable PTY sessions in process

Earlier scope excludes detachment from the MVP.
This decision supersedes that scope exclusion.
The low-level shell session still stays separate from display behavior.
Managed sessions own shared display state for attached frontends.
An in-process session manager owns each PTY and its background reader.
It stores opaque IDs and retains display projections while Sessions run.
Detachment removes one attachment without terminating the shell session.
Reattachment accepts only running sessions and restores their retained screen.

ADR-0011 removes final-screen retention after Session termination.

## Status

superseded by ADR-0006
ADR-0011 narrows its retention and input behavior.

## Consequences

PTY output broadcasts to every attached frontend.
Each attachment owns a bounded output buffer.
Buffer overflow disconnects only the slow attachment.
Input drafts stay private to their attachments.
The session size stays fixed after creation.
Service restart loses all in-memory sessions.
