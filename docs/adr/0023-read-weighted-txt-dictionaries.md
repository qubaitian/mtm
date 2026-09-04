# Read weighted TXT dictionaries for Completion

## Status

accepted

MTM reads every direct `.txt` file in the user's dictionaries directory.
It sorts files by filename and preserves each file's line order.
Each row has one non-empty Completion candidate.
Each row has a matching code and optional non-negative weight.
Missing or empty weights use zero.
Matching candidates sort by descending weight.
Equal weights preserve source order.
Duplicate candidates keep their highest matching weight.
This replaces the former single dictionary source.
