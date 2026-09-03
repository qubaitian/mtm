# Use Rime 8105 for default Completion

## Status

superseded by ADR-0022

MTM needs a default Completion provider for Chinese shell input.
MTM bundles the complete Rime-ice 8105 dictionary for this provider.
The provider reads active single-character entries and preserves source order.
It matches Pinyin code prefixes and replaces the Completion prefix after acceptance.
The smaller character dictionary avoids loading Rime's larger word dictionaries.
