# Run the Browser terminal as an ordinary Shell

The Browser terminal starts one ordinary Shell per browser connection.
It does not select or create a managed Session.
Users use normal MTM commands inside that Shell.
`set current-session` creates the managed Attachment.
The Browser terminal uses the current Shell and default Session size.
The manager starts the browser listener without opening a browser.

## Status

accepted

## Consequences

The Browser terminal does not need Session-specific routes.
The root URL is the only Browser route.
Closing the browser connection closes its ordinary Shell.
Managed Sessions remain independent until explicit entry.

## Conflicts

This supersedes ADR-0016's Session list and direct Session routes.
