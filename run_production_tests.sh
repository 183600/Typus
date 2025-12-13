#!/bin/bash
# Script to run tests with production flags and check for warnings

# Set C locale to avoid locale warnings
export LC_ALL=C

echo "Running tests with production flags..."
cabal test --flags="-fast production" --test-show-details=direct

if [ $? -eq 0 ]; then
    echo "All tests passed successfully!"
else
    echo "Some tests failed!"
    exit 1
fi

echo "Building with werror flag to check for warnings..."
cabal build --flags="-fast production werror"

if [ $? -eq 0 ]; then
    echo "Build completed without warnings!"
else
    echo "Build failed with warnings!"
    exit 1
fi

echo "All checks passed!"