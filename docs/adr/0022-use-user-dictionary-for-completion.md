# Use a user dictionary for Completion

## Status

accepted

MTM must avoid committing a large Completion dictionary.
MTM reads only the user 8105 dictionary at `~/.mtm/dictionaries/8105.dict.yaml`.
It does not use a bundled fallback when that file is missing.
This follows Rime-ice's separate user-dictionary pattern while keeping `.dict.yaml` format.
The Provider loads the dictionary once, so restart MTM after edits.
