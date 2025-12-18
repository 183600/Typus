#!/bin/bash
# Run cabal tests with proper locale settings to avoid warnings

export LC_ALL=C
export LANG=C

exec cabal test "$@"
