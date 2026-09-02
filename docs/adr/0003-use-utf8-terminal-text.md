# Use UTF-8 terminal text

Terminal display treats user-visible text as UTF-8.
The PTY transport still preserves control bytes and unknown sequences.
This keeps text APIs direct while preserving terminal control behavior.

## Status

accepted

## Consequences

Malformed UTF-8 needs a defined replacement behavior later.
Unicode cell width remains outside the first emulator subset.
