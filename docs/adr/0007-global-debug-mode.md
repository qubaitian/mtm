# Add a global debug mode

MTM enables debug mode through the existing session manager.
The manager writes complete diagnostic logs to a per-user file and broadcasts them to active sessions.
SBCL reports conditions and backtraces without entering the debugger.

## Status

superseded by ADR-0008

## Considered Options

Restarting the manager does not work because it loses all in-memory sessions.

## Consequences

Debug mode remains active until the manager exits.
Diagnostic records include submitted input and error backtraces.
