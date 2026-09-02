# Give the Session manager an explicit lifecycle

MTM exposes the Session manager as a singleton data position.
`new session-manager` starts it, `get session-manager` reports its state, and `del session-manager` stops it.
Other operations require a running Session manager because explicit lifecycle control makes ownership clear.

## Status

superseded by ADR-0012

## Considered Options

Lazy manager startup keeps old commands convenient but hides service ownership.
Idempotent `new` and `del` operations conflict with the four data operation rules.

## Consequences

Stopping the Session manager terminates every Session and disconnects clients.
Restarting the Session manager loses all in-memory Sessions.
