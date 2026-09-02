# Manage detachable PTY Sessions in the manager

## Status

accepted

## Decision

The Session manager owns each Session's PTY and reader.
It stores Sessions by name.
Each Session retains a terminal display projection.
Each Session supports multiple Attachments.

Detachment removes one Attachment without terminating its Session.
Reattachment requires a named running Session.
Reattachment shows the retained display before new PTY output.
Natural shell exit removes the Session.
Named deletion terminates its shell and Attachments.

## Consequences

Slow Attachments disconnect after exceeding their output limits.
Other Attachments continue receiving shared output.
The Session manager keeps shell state across client commands.
