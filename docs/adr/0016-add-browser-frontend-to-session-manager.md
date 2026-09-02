# Add a Browser frontend inside the Session manager

The Session manager starts an HTTP and WebSocket listener on loopback.
The listener uses `127.0.0.1:7681`.
The Browser frontend reuses named Sessions and creates one Attachment.
It uses an MTM-owned xterm.js page and ttyd-compatible messages.
The root page lists Sessions, and `/session/<name>` opens one Session.
Browser disconnection detaches its Attachment and keeps the Session running.

## Status

superseded by ADR-0017

## Consequences

The manager owns the browser listener and its shutdown.
The existing PTY and Attachment behavior stays unchanged.
MTM does not depend on ttyd's page or server.
The browser page loads xterm.js from a CDN.
The repository stores no JavaScript asset.
The page keeps only the initialization script it needs.
Remote access needs a later authentication and TLS decision.
Terminal size stays fixed when the Session starts.
