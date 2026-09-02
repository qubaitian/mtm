# Preserve compatible terminal mouse input

MTM accepts SGR and legacy X10 Mouse reports at the local Terminal frontend.
It consumes local left-button actions and forwards full-screen reports unchanged.
The Browser terminal keeps mouse selection inside xterm.js.

## Status

accepted

## Consequences

The local frontend supports older terminals without changing live PTY bytes.
Right-button, middle-button, and wheel actions have no local meaning yet.
