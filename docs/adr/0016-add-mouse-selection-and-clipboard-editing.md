# Add mouse selection and clipboard editing

The local Editor area supports mouse selection and system clipboard editing.
This keeps terminal output read-only and keeps Session transport separate.

## Status

accepted

## Decision

Left-click moves the Insertion point.
Dragging selects one continuous Text selection, including newlines.
Command-X cuts, Command-C copies, and Delete removes the Text selection.
Command-V replaces the Text selection or inserts at the Insertion point.
The frontend recognizes Command events from the Kitty keyboard protocol.
Drag positions outside the visible Editor area clamp to its nearest edge.

## Consequences

Selection state belongs to one Attachment.
Clipboard content uses plain text from the operating system.
Pasted content keeps newlines until Submission interprets them.
