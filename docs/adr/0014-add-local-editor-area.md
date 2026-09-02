# Add a local Editor area

## Status

accepted

## Decision

MTM intercepts normal Terminal frontend input before Submission.
The Editor area belongs to one Attachment.
Session History belongs to the Session.
History remains available across Attachments.

The Editor area handles ordinary Shell input.
Empty Ctrl-D detaches the Attachment.
The overlay starts at column zero.
The status bar handles mouse input and Escape first.
The Viewport excludes status bar rows.

Full-screen terminal applications use automatic full-screen transport.
The terminal emulator detects alternate-screen entry.
The frontend then sends keyboard bytes directly to the PTY.
Alternate-screen exit restores the Editor area.

Edit buffers and History are not public data positions.
