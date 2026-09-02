# Use a process-global name-based Lisp API

## Status

accepted

## Decision

MTM exposes named Lisp functions for Session management.
The functions use Session names as their public identity.
The Session manager remains process-global.

`new-session-manager` ensures the global manager.
`get-session-manager` returns its state and Session snapshot.
`new-session` ensures a named Session, then enters it.
`get-session` enters an existing named Session.
`del-session` removes a named Session.
`del-session-manager` removes all Sessions and stops the manager.

Session entry requires a name.
Session values have no public setter.
The generic `mtm.api` package is not part of the interface.

## Consequences

The manager stores Sessions by name only.
Session list values contain names and running states.
Repeated creation reuses the same Session value.
Repeated deletion leaves the same stopped result.
Internal callers use private value functions when needed.
