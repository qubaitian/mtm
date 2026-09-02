# Add a local Editor area

MTM intercepts keys in the Terminal frontend before Submission.
This restores a local Editor area after ADR-0011 removed it.
The frontend is not a general terminal for vim or pagers.

## Status

accepted

## Decision

The Editor area is a layer on the Terminal frontend.
It is not a second frontend.
The Edit buffer belongs to one Attachment.
History belongs to the Session and is shared by Attachments.
Empty Ctrl-D detaches the Attachment.
It does not send EOF to the shell.
The overlay starts at column 0.
The status bar handles mouse and Esc first.
The Viewport excludes status bar rows.
History and the Editor area are not public data positions.

## Conflicts

This decision supersedes the “no editor” clause in ADR-0011.
ADR-0013 still owns the status bar.
