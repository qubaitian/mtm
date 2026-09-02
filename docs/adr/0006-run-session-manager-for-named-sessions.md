# Run a session manager for named sessions

This decision is superseded by ADR-0010 for manager lifecycle behavior.

MTM uses a long-lived session manager to own shell sessions across command invocations.
The command surface described here is superseded by ADR-0008.
The manager lifetime, scope, socket, and restart decisions remain.
The bare `mtm` command starts the manager when necessary, displays the session list, and exits without attaching.
The `mtm <session-name>` command starts the manager when necessary, atomically finds or creates the named session, then attaches the command frontend.
All commands use the same manager when it already exists.
The manager scope is the current user on the current machine.
The manager stays alive without sessions until an external stop signal.
The bare command displays session names and execution states.
This change adds no manager stop option.
The manager uses a per-user Unix domain socket for local IPC.
Commands wait for manager readiness before sending requests.
Commands replace stale manager endpoints before restarting the manager.
Frontend communication failure exits the frontend and preserves the session.
Named lookup supports `ready`, `running`, and `error` execution states.
This decision does not define new behavior for `closed` sessions.
Manager restart loses sessions because session state remains in memory.

## Status

superseded by ADR-0010
ADR-0011 removes its command frontend and execution states.
