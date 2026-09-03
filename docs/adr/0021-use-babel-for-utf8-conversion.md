# Use Babel for UTF-8 conversion

## Status

accepted

## Decision

MTM uses Babel for all text and UTF-8 octet conversion.
The core system depends on Babel for terminal projection support.
The Editor keeps raw octets, while Babel supplies character boundaries.
Raw PTY, ANSI, mouse, and MTM control framing remains byte-level.
Incomplete PTY characters remain pending across reads.
Session termination converts pending bytes using Babel replacement behavior.

## Consequences

UTF-8 codec logic has one dependency owner.
Terminal projection keeps characters split across reads intact.
Unicode display width remains deferred under ADR 0003.
