# Use four MTM data operations

MTM exposes `new`, `set`, `get`, and `del` through `mtm.api`.
`new` creates only a missing position, while `set`, `get`, and `del` require an existing position.
`nil` means missing, so a position cannot contain `nil`.
Session positions retain live managed Session objects rather than replacing them with lists.
The CLI mirrors the interface with Session lifecycle and Passthrough operations.
`current-session` is a control position that `set` initializes or switches.

ADR-0011 removes the debug position and narrows supported positions.

## Status

superseded in part by ADR-0011 and ADR-0012

## Considered Options

MTM does not shadow `CL:SET` or `CL:GET`.
Those functions have different signatures and storage semantics.
The property-list implementation may still use `(setf (cl:get ...))`, `cl:get`, and `remprop` internally.

## Consequences

The named-function interface in ADR-0012 replaces this generic API.
The new interface replaces the named-session and `debug` command forms described by ADR-0006 and ADR-0007.
ADR-0011 removes closed-session retention after natural shell termination.
`del` terminates a live Session and removes its registry position immediately.
