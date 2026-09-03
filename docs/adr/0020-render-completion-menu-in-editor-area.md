# Render the Completion menu inside the Editor area

## Status

accepted

## Decision

MTM renders the Completion menu as ANSI output from the Editor area.
The menu uses candidates supplied by an Editor completion provider.
The provider receives the current Completion prefix.
Candidates replace the prefix when the user accepts one.

Tab opens or accepts the Completion menu.
Up and Down move its selected candidate.
Enter accepts the selected candidate.
Escape closes the menu.
Other input closes the menu before normal Editor handling.

The Browser frontend does not add a separate DOM completion component.
Full-screen transport bypasses the Completion menu.

## Consequences

Completion shares the existing Editor redraw and terminal geometry.
The provider stays independent from candidate discovery.
The menu remains available to local ANSI Terminal frontends.
Browser-specific rendering does not become a second completion path.
