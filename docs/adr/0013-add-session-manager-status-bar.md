# Add a local Session manager status bar

## Status

accepted

## Decision

MTM adds a local status bar to interactive Terminal frontends.
The collapsed bar shows the number of running Sessions.
Clicking the bar toggles the Session rows.
Clicking a row enters that named Session.

The frontend draws the bar with terminal control sequences.
The Session's live PTY bytes remain unchanged.
The bar reserves terminal rows during full-screen transport.
The Terminal frontend handles bar mouse reports locally.

## Consequences

The status bar can reduce the Session display height.
The frontend must refresh its Session list periodically.
The first version assumes the list fits the terminal height.
