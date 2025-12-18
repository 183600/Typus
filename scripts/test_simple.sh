#!/bin/bash
# Simple test runner that filters locale warnings

# Run cabal test and filter out locale warnings
cabal test --flags="-fast production" --test-show-details=direct 2>&1 | grep -v "setlocale.*cannot change locale"