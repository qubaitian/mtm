# Support full-screen terminal applications

MTM keeps the Editor area as the default Terminal frontend mode.
An explicit CLI or Lisp option enters Application passthrough.
This supports Vim while preserving line editing for normal Shell use.

## Status

accepted

## Decision

Application passthrough belongs to the Session.
All Attachments follow the Session's current mode.
New Attachments inherit the target Session's mode.

The Application owner controls the Session's PTY size.
The Session keeps its last size when the owner leaves.
The next Application Attachment can become the owner.

The Session manager status bar remains visible in Application passthrough.
The status bar reserves its rows instead of covering application output.
The PTY height excludes all reserved status bar rows.
Resizing the Application owner updates the Session's PTY size.
Status bar rows handle their own mouse reports.
All keyboard input, including `Esc`, reaches the application.

Application return occurs when the application leaves its alternate screen.
Application return restores the Editor area for every Attachment.
The Session continues running after Application return.
Applications without an alternate screen remain in passthrough.
Closing the Terminal frontend detaches its Attachment.
Application passthrough has no local keyboard Detachment shortcut.

Edit buffers survive mode changes because they belong to Attachments.

## Conflicts

This decision supersedes the Vim and pager exclusion in ADR-0014.
It changes the status bar layout from ADR-0013 in Application passthrough.
It keeps the four Data operations from ADR-0008.
Application passthrough remains a frontend mode, not a Data position.
