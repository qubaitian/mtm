# Use four operation prefixes for project-defined functions

## Status

accepted

## Decision

MTM names each project-defined function with an operation prefix and noun.
The prefix is one of `new`, `set`, `get`, or `del`.

`new-*` creates a resource or ensures a value.
`set-*` changes state, applies input, runs workflows, or sends output.
`get-*` reads or derives a value.
`del-*` stops, closes, removes, or cleans up a resource.

Predicates end with `-p` as an exception.
Required Common Lisp, library, FFI, macro-generated, and struct names remain exceptions.
Test declarations and test helpers may use descriptive names.

## Consequences

Session entry uses the `get-session` function.
Session creation and entry use the `new-session` function.
Internal value lookup uses `get-session-value`.
MTM provides no aliases for removed names.
