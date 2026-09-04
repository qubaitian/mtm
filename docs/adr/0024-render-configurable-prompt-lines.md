# Render configurable Prompt lines in MTM

MTM owns the Prompt line for named Sessions.
The manager loads trusted Common Lisp configuration at startup.
The Prompt provider receives only the Session name.
The Terminal frontend renders the returned line beside the local Editor area.
The Browser terminal remains an ordinary Shell.

## Consequences

Provider code can query the Session manager directly.
It can also query external system values.
The zsh adapter preserves user startup files and hides the Shell prompt.
Raw PTY output remains unchanged.
