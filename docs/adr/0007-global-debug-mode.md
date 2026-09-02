# Keep evaluation and debug controls outside MTM

## Status

accepted

## Decision

MTM exposes no evaluator or debug data position.
The CLI reports invalid operations as errors.
The Lisp API exposes only Session and frontend functions.
Diagnostic output stays on the process error stream.

## Consequences

MTM has no debug command or debug mode option.
The Session manager does not broadcast diagnostic records.
Users inspect failures through the returned error text.
