# ICL Browser Completion Research

This note covers browser rendering only.
It excludes Lisp interface discovery.

## Conclusion

ICL does not create a DOM completion dropdown.
The ICL editor draws the menu with ANSI terminal sequences.
The browser displays those sequences through xterm.js.
No completion-candidate JSON message crosses the WebSocket.

The generic output path supports this design.
The browser writes every server output chunk into xterm.js.
[browser.js](</Users/qubaitian/code/icl/assets/browser.js:245>) handles `output` messages.
[completion.lisp](</Users/qubaitian/code/icl/src/completion.lisp:763>) renders the menu.

## Main page

The server returns a small HTML shell.
The shell contains one Dockview mount point.
It loads Dockview, xterm.js, CSS, and application scripts.
[browser-ui.lisp](</Users/qubaitian/code/icl/src/browser-ui.lisp:35>) builds this shell.

The server serves the shell at a tokenized path.
It serves JavaScript and CSS from `/assets/`.
[browser-server.lisp](</Users/qubaitian/code/icl/src/browser-server.lisp:190>) routes these requests.

The browser creates the Dockview layout.
The layout contains a `TerminalPanel` for the REPL.
[browser.js](</Users/qubaitian/code/icl/assets/browser.js:4009>) creates the layout.

## Main REPL input and output

The terminal panel creates an xterm.js terminal.
It uses five hundred columns to prevent wrapping.
[browser.js](</Users/qubaitian/code/icl/assets/browser.js:3833>) defines the panel.
[browser.js](</Users/qubaitian/code/icl/assets/browser.js:3855>) creates the terminal.

xterm.js sends raw key data as `input` messages.
The server queues each received character.
[browser.js](</Users/qubaitian/code/icl/assets/browser.js:3867>) sends the input.
[browser-websocket.lisp](</Users/qubaitian/code/icl/src/browser-websocket.lisp:165>) queues the input.

The server starts the real ICL REPL editor.
The browser mode uses WebSocket input and output streams.
[browser-server.lisp](</Users/qubaitian/code/icl/src/browser-server.lisp:372>) starts that editor.

The server packages editor output as generic `output` messages.
The browser converts line feeds to carriage-return line feeds.
Then it calls `terminal.write`.
[browser-websocket.lisp](</Users/qubaitian/code/icl/src/browser-websocket.lisp:2048>) buffers output.
[browser-websocket.lisp](</Users/qubaitian/code/icl/src/browser-websocket.lisp:2066>) sends output.
[browser.js](</Users/qubaitian/code/icl/assets/browser.js:245>) writes output into xterm.js.

The browser has no completion request handler.
Tab travels through the same raw input path.

## Completion menu rendering

Tab handling opens the menu after completion processing.
[editor.lisp](</Users/qubaitian/code/icl/src/editor.lisp:868>) handles Tab.
[completion.lisp](</Users/qubaitian/code/icl/src/completion.lisp:729>) stores menu state.

The renderer draws the menu below the cursor line.
It moves to the prefix start column.
It clears each row before writing content.
[completion.lisp](</Users/qubaitian/code/icl/src/completion.lisp:763>) implements this rendering.

The selected row uses reverse video and bold text.
Other rows use gray text.
Long lists add a scrollbar character.
Overflow adds a `[selected/total]` count row.
[completion.lisp](</Users/qubaitian/code/icl/src/completion.lisp:781>) writes candidate rows.
[specials.lisp](</Users/qubaitian/code/icl/src/specials.lisp:465>) defines the ANSI styles.

The renderer moves the cursor back afterward.
It restores the original prompt column.
[completion.lisp](</Users/qubaitian/code/icl/src/completion.lisp:806>) restores the cursor.

The visual sequence is therefore:

```text
xterm key data
  -> WebSocket input
  -> ICL editor reads Tab
  -> ANSI menu output
  -> WebSocket output
  -> xterm.js renders the menu
```

## Menu keyboard behavior

Up and Down change the selected index.
Enter and Tab apply the selected item.
Escape closes the menu.
Other keys close the menu before normal processing.
[editor.lisp](</Users/qubaitian/code/icl/src/editor.lisp:905>) defines these cases.

The editor redraws the current line and menu together.
It redraws after navigation and after menu opening.
[editor.lisp](</Users/qubaitian/code/icl/src/editor.lisp:1404>) controls those redraws.

## Layout details that keep the menu visible

The terminal container supports horizontal and vertical scrolling.
The inner wrapper expands to the terminal's natural width.
[browser.css](</Users/qubaitian/code/icl/assets/browser.css:41>) defines this layout.

Browser wrapping calculations use five hundred columns.
This keeps long input on one logical terminal line.
[editor.lisp](</Users/qubaitian/code/icl/src/editor.lisp:198>) defines `safe-term-width`.

The terminal resizes rows with `ResizeObserver`.
It keeps the five hundred-column width fixed.
[browser.js](</Users/qubaitian/code/icl/assets/browser.js:3979>) implements row fitting.

## Notebook code cells

Notebook code cells use the same real ICL editor.
The resting cell uses a highlighted `<pre>` element.
Editing replaces it with an xterm.js terminal.
[notebook.js](</Users/qubaitian/code/icl/assets/notebook.js:1125>) builds the resting cell.
[notebook.js](</Users/qubaitian/code/icl/assets/notebook.js:900>) opens the editor terminal.

Cell keystrokes use `cell-key` messages.
Cell editor output uses `cell-term` messages.
The browser writes cell output into that xterm.js instance.
[browser-websocket.lisp](</Users/qubaitian/code/icl/src/browser-websocket.lisp:445>) accepts cell messages.
[browser-websocket.lisp](</Users/qubaitian/code/icl/src/browser-websocket.lisp:2111>) sends cell output.
[notebook.js](</Users/qubaitian/code/icl/assets/notebook.js:20>) renders cell output.

The server reports the editor's required row count.
The count includes visible completion menu rows.
The browser resizes the cell terminal to that count.
[browser-websocket.lisp](</Users/qubaitian/code/icl/src/browser-websocket.lisp:2160>) reports cell rows.
[notebook.js](</Users/qubaitian/code/icl/assets/notebook.js:27>) resizes the cell terminal.

This row reporting prevents the menu from clipping.
The completion tests verify the extra-line calculation.
[completion-tests.lisp](</Users/qubaitian/code/icl/tests/completion-tests.lisp:154>) covers this behavior.

## Other dropdown-like controls

The CSS `.app-menu` styles the hamburger menu.
It is unrelated to completion.
[browser.css](</Users/qubaitian/code/icl/assets/browser.css:303>) defines that menu.

The source viewer uses a real HTML `<select>` element.
That selector chooses among multiple source definitions.
It is also unrelated to completion.
[browser.js](</Users/qubaitian/code/icl/assets/browser.js:2554>) creates that selector.

## Reproduction summary

To copy ICL's approach, embed xterm.js in the page.
Send raw key data through one WebSocket.
Run the editor outside the browser.
Render the menu with ANSI cursor controls.
Write all output back into xterm.js.
Resize the terminal when the menu needs more rows.
