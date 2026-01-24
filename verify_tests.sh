#!/bin/bash

# Comprehensive test verification script for Typus project
# This script runs various test configurations to ensure all issues are resolved

echo "========================================"
echo "Typus Project Comprehensive Test Check"
echo "========================================"
echo ""

# Set memory limits
export GHCRTS="-M2G -A16m"

echo "1. Running standard stack test..."
stack test
if [ $? -eq 0 ]; then
    echo "✓ Standard stack test PASSED"
else
    echo "✗ Standard stack test FAILED"
    exit 1
fi
echo ""

echo "2. Running stack test with fast flag..."
stack test --flag "*:fast"
if [ $? -eq 0 ]; then
    echo "✓ Fast flag test PASSED"
else
    echo "✗ Fast flag test FAILED"
    exit 1
fi
echo ""

echo "3. Running stack test with production flag disabled..."
stack test --flag "*:-production"
if [ $? -eq 0 ]; then
    echo "✓ Production disabled test PASSED"
else
    echo "✗ Production disabled test FAILED"
    exit 1
fi
echo ""

echo "4. Running stack test with custom GHC options..."
stack test --ghc-options="-O0 -rtsopts"
if [ $? -eq 0 ]; then
    echo "✓ Custom GHC options test PASSED"
else
    echo "✗ Custom GHC options test FAILED"
    exit 1
fi
echo ""

echo "5. Running stack test with test arguments..."
stack test --test-arguments="+RTS -M1024m -A16m -RTS"
if [ $? -eq 0 ]; then
    echo "✓ Test arguments PASSED"
else
    echo "✗ Test arguments FAILED"
    exit 1
fi
echo ""

echo "6. Running stack test with single job..."
stack test --jobs=1
if [ $? -eq 0 ]; then
    echo "✓ Single job test PASSED"
else
    echo "✗ Single job test FAILED"
    exit 1
fi
echo ""

echo "7. Running the full command from the original request..."
GHCRTS="-M2G -A16m" stack test \
      --flag "*:fast" \
      --flag "*:-production" \
      --ghc-options="-O0 -rtsopts" \
      --test-arguments="+RTS -M1024m -A16m -RTS" \
      --jobs=1

if [ $? -eq 0 ]; then
    echo "✓ Full command test PASSED"
else
    echo "✗ Full command test FAILED"
    exit 1
fi
echo ""

echo "8. Checking for compilation errors..."
stack build > /dev/null 2>&1
if [ $? -eq 0 ]; then
    echo "✓ No compilation errors"
else
    echo "✗ Compilation errors found"
    exit 1
fi
echo ""

echo "9. Checking git status..."
git_status=$(git status --porcelain)
if [ -z "$git_status" ]; then
    echo "✓ No uncommitted changes"
else
    echo "⚠ There are uncommitted changes:"
    echo "$git_status"
fi
echo ""

echo "========================================"
echo "All tests completed successfully!"
echo "The Typus project is in a good state."
echo "========================================"