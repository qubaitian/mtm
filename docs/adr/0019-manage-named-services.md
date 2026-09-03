# Manage named Services beside named Sessions

## Status

accepted

## Decision

MTM adds named Services to the Session manager.
Service names share one global namespace with Session names.
The manager rejects every duplicate name.

Trusted Lisp source files declare one or more Services.
Each source owns its declared names.
Reload validates every declaration before changing state.
Reload removes declarations deleted from that source.
Reload updates declarations owned by that source.
The manager reloads sources only after explicit commands.
The manager does not persist source or desired-state data.

Each Service runs one arbitrary argument vector inside one PTY.
The Service keeps bounded recent output in memory.
The Service accepts no input from the frontend.
The default desired state is `:running`.
Natural exit marks failure and restarts after one second.
Stopping sends `SIGTERM`, then `SIGKILL` after two seconds.
The Service controls only its direct child process.

The public Service snapshot exposes desired and observed states.
The observed states are `:running`, `:stopped`, and `:failed`.
The snapshot includes the current child process identifier.

## Consequences

Service definitions can run several independent programs.
Service source files remain trusted local code.
Invalid sources leave the current registry unchanged.
Recent output disappears when the manager restarts.
The PTY layer now supports arbitrary argument vectors.
The manager still uses one in-memory lifecycle owner.

## Conflicts

The status-bar clauses are superseded.
The current frontend renders no status bar.
The Service lifecycle and snapshot decisions remain accepted.
