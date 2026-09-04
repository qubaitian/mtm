# Use a default Completion provider

## Status

superseded by ADR-0022

MTM needs a default Completion provider for local Editor areas.
The provider reads candidate entries from user dictionary files.
It preserves source order while loading dictionary entries.
It matches Completion code prefixes.
It replaces the Completion prefix after acceptance.
