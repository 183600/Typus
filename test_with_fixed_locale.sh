#!/bin/bash

# Fix locale warnings by setting proper locale
export LC_ALL=C
export LANG=C
export LC_CTYPE=C
export LC_MESSAGES=C
export LC_COLLATE=C

# Run the original command
cabal test --flags="-fast production" --test-show-details=direct "$@"