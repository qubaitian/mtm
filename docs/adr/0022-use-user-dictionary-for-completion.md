# Use a user dictionary for Completion

## Status

superseded by ADR-0023

MTM must avoid committing a large Completion dictionary.
MTM reads user dictionary files from `~/.mtm/dictionaries/`.
It does not use a bundled fallback when those files are missing.
This keeps user-maintained Completion data outside the source tree.
The provider loads dictionaries once, so restart MTM after edits.
