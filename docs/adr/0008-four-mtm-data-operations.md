# Use four MTM data operations

## Status

accepted

## Decision

MTM uses `new`, `set`, `get`, and `del` operation prefixes.
Each operation addresses a named value position.

`new` ensures a value exists.
It creates a missing value and reuses an existing value.
`get` reads a value or enters a named Session.
`set` changes state, applies input, or sends output.
`del` removes, stops, closes, or cleans up a value.

The Session manager position stores one manager value.
`new session-manager` ensures that value exists.
`get session-manager` returns its state and all named Sessions.
`del session-manager` stops the manager and removes every Session.

The Session position uses a required Session name.
`new session <name>` ensures that Session, then enters it.
`get session <name>` enters an existing Session.
`del session <name>` removes that named Session.
Bare `get session` and `del session` forms are invalid.
The Session position has no `set` form.

Every Session operation is safe to repeat.
A repeated `new` does not create a duplicate Session.
Repeated deletion does not restore a removed value.

The CLI and Lisp interface share these meanings.
Lisp names use the corresponding operation prefixes.
