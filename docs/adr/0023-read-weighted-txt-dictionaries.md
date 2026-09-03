# Read weighted TXT dictionaries for Completion

## Status

accepted

MTM reads every direct `.txt` file in the user's dictionaries directory.
It sorts files by filename and preserves each file's line order.
Each row has a Han character, Pinyin code, and non-negative weight.
Matching candidates sort by descending weight.
Equal weights preserve source order.
Duplicate candidates keep their highest matching weight.
This replaces the former single Rime `.dict.yaml` source.
