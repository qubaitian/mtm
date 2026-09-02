# Use a macOS PTY with SBCL

The first release targets SBCL running on macOS.
It uses CFFI to call the operating system PTY and process APIs.
The shell inherits the user's shell, environment, and working directory.

## Status

accepted

## Consequences

The first release does not promise support for other platforms.
The system keeps a clear boundary around foreign operating system calls.
Later Linux support can replace the platform bindings behind that boundary.
