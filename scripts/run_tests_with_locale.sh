#!/bin/bash
# Run tests with proper locale settings to avoid warnings

export LC_ALL=C.UTF-8
export LANG=C.UTF-8

cabal test --flags="-fast production" --test-show-details=direct "$@"
