# Add a local Session manager status bar

MTM adds a local Session manager status bar to every interactive Terminal frontend.
The collapsed bar shows the number of running Sessions.
Clicking the bar toggles Session rows, and clicking a row enters that Session.
The frontend draws the bar with terminal control sequences while PTY bytes stay unchanged.

## Status

accepted

## Consequences

The frontend handles mouse reports for the status bar.
The bar overlays the bottom display area and keeps the PTY size fixed.
The first version assumes the Session list fits the terminal height.
This decision supersedes the status bar and strict Passthrough limits in ADR-0011.
