# Use four operation prefixes for project-defined functions

MTM names every project-defined function with an operation prefix and noun.
The prefix is one of `new`, `set`, `get`, or `del`.
`new-*` creates a resource or value.
`set-*` changes state, applies input, runs workflows, or sends output.
`get-*` reads or derives a value.
`del-*` stops, closes, removes, or cleans up a resource.
Predicates end with `-p` as an exception.
Required Common Lisp, library, FFI, macro-generated, and struct-generated names remain exceptions.
Test declarations and test helpers may use descriptive names.
This removes competing verbs for the same data operation.

## Status

accepted

## Considered Options

Semantic verbs such as `start`, `attach`, and `lookup` are clearer alone.
The four prefixes reduce the vocabulary that callers must learn.

## Consequences

The rename changes every MTM caller, test, and public export.
MTM provides no aliases for the old function names.
