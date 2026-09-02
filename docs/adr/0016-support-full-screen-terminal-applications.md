# Detect full-screen terminal applications automatically

## Status

accepted

## Decision

MTM detects full-screen terminal applications from alternate-screen events.
Users do not select a frontend mode explicitly.
Users do not pass a CLI or Lisp mode option.

The Session terminal emulator reports alternate-screen entry and exit.
Entry marks the Session as full-screen.
All attached frontends receive the matching transport state.
Full-screen input sends keyboard bytes directly to the PTY.
Exit restores the Editor area for every Attachment.

The Session manager status bar stays visible during full-screen transport.
It reserves rows below the application display.
The full-screen frontend updates the Session PTY size.
Closing a frontend detaches its Attachment.
The Session continues running after detachment.

Applications without an alternate screen keep the Editor area.
The same behavior serves Vim, pagers, and other terminal tools.
