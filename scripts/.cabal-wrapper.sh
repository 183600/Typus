#!/bin/bash
# Wrapper to run cabal with C locale to avoid warnings
export LC_ALL=C
export LANG=C
exec "$@"
