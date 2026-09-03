# Render the Completion menu inside the Editor area

## Status

accepted

## Decision

MTM renders the Completion menu as ANSI output from the Editor area.
The menu uses candidates supplied by an Editor completion provider.
The provider receives the current Completion prefix.
MTM queries the provider after typed input or deletion changes the buffer.
It queries immediately when the Completion prefix has at least one character.
It does not query after paste, cursor movement, or Submission.
Candidates replace the prefix only after numeric acceptance.

The menu displays at most nine candidates in provider order.
Visible candidates use numeric labels from 1 through 9.
MTM shows the first nine candidates without pagination.
Number keys accept their corresponding visible candidate.
Invalid numbers close the menu and insert normally.
Automatic completion does not insert or expand text.

Tab opens the menu when it is inactive.
Tab closes the menu and inserts a literal Tab when active.
Enter closes the menu and follows normal Editor handling.
Escape closes the menu.
Up and Down close the menu before normal Editor handling.
History keeps its Up and Down behavior when the menu is inactive.
Other input closes the menu before normal Editor handling.

The Browser frontend does not add a Completion menu.
Full-screen transport bypasses the Completion menu.

## Rationale

Automatic queries reuse the existing provider and ANSI rendering path.
Numeric labels avoid ambiguity with Tab and Enter.
Nine rows provide one-digit selection without pagination.
The Browser terminal remains an ordinary Shell.
This keeps Completion behavior inside the existing Editor area.

## Consequences

Completion shares the existing Editor redraw and terminal geometry.
The provider stays independent from candidate discovery.
The menu remains available to local ANSI Terminal frontends.
Tab, Enter, Up, and Down no longer select candidates.
Browser-specific rendering does not become a second completion path.
