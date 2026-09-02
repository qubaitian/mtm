# Reserve terminal rows for the Session status bar

## Status

accepted

## Decision

Each interactive Terminal frontend reserves rows for its status bar.
The status bar stays below the Session display.
The Editor Viewport excludes those rows.
Full-screen transport also reserves the same rows.

## Consequences

The frontend adjusts full-screen PTY height when needed.
Session output remains visible above the status bar.
