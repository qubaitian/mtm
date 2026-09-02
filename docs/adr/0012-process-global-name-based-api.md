# Use a process-global name-based Lisp API

MTM exposes named Lisp functions for Session management.
The functions use Session names as their public identity.
The process keeps one shared Session manager and current Attachment.
Session creation creates the manager when it does not exist.
The manager getter returns the manager or no value.
The manager deletion function stops and clears the manager.
The current-session setter enters Passthrough mode.
The current-session deletion function detaches the current Attachment without arguments.
The generic `mtm.api` operation package is removed.

## Status

accepted

## Considered Options

Passing a Manager to every function exposes ownership clearly.
It conflicts with the required process-global public API.

Adding a separate Session ID creates an unused identity.
Session names already identify every Session.

Keeping a generic `new`, `set`, `get`, and `del` dispatcher duplicates named functions.
Named functions make the Lisp API direct and discoverable.

## Consequences

`new-session` accepts a positional Session name.
Session management functions do not accept a Manager argument.
The manager stores Sessions by name only.
Session list values contain Session names and Manager states.
Only creation lazily starts the process-global Manager.
Workflow and output functions use the `set-*` prefix.
Predicate functions end with `-p` as an exception.

## Conflicts with earlier ADRs

This decision supersedes the generic API surface in ADR-0008.
It supersedes explicit Manager ownership in ADR-0010.
It follows the complete project-defined function naming decision in ADR-0009.
It keeps Passthrough and Session behavior from ADR-0011.
