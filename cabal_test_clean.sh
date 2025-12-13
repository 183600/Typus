#!/bin/bash
# Wrapper script to run cabal test without locale warnings
# This script redirects both stdout and stderr to filter out locale warnings

# Disable locale warnings completely
export LC_ALL=C
export LANG=C
export LANGUAGE=C
export LC_CTYPE=C
export LC_NUMERIC=C
export LC_TIME=C
export LC_COLLATE=C
export LC_MONETARY=C
export LC_MESSAGES=C
export LC_PAPER=C
export LC_NAME=C
export LC_ADDRESS=C
export LC_TELEPHONE=C
export LC_MEASUREMENT=C
export LC_IDENTIFICATION=C

# Configure bash to not output locale warnings
export POSIXLY_CORRECT=1

# Unset any locale-related variables that might cause issues
unset LC_CTYPE
unset LC_NUMERIC
unset LC_TIME
unset LC_COLLATE
unset LC_MONETARY
unset LC_MESSAGES
unset LC_PAPER
unset LC_NAME
unset LC_ADDRESS
unset LC_TELEPHONE
unset LC_MEASUREMENT
unset LC_IDENTIFICATION
unset LC_ALL

# Run the command and filter any remaining locale warnings
{
    cabal test --flags="-fast production" --test-show-details=direct "$@" 2>&1
} | grep -v "setlocale.*cannot change locale" | grep -v "warning.*setlocale"