# Run a user-scoped Session manager for named Sessions

## Status

accepted

## Decision

MTM uses one long-lived Session manager for one user and host.
The manager owns named shell Sessions and their Attachments.
It runs behind a per-user Unix socket.
Commands connect to that socket across process invocations.

`new session-manager` ensures that the manager runs.
`new session <name>` ensures the manager and named Session.
It then enters that Session through the Terminal frontend.
`get session <name>` enters an existing named Session.
`get session-manager` returns the manager state and Session list.
`del session <name>` removes one named Session.
`del session-manager` removes every Session and stops the manager.

The manager starts automatically for `new session <name>`.
Repeated `new` and `del` operations keep the same result.
Commands wait for manager readiness before sending requests.
The manager keeps running after ordinary client commands exit.
The manager stops only through `del session-manager` or process failure.
Restarting the manager loses in-memory Session state.

The manager also hosts the local Browser frontend.
The Browser frontend lists and enters named Sessions.
