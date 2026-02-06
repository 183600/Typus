#!/bin/bash
# Memory-optimized test runner for Typus project

# Set memory limits
export GHCRTS="-M512m -A32m -n2m"

# Set environment variables for reduced memory usage
export TYPUS_SKIP_GO_BUILD=1

# Run tests with memory optimization
echo "Running memory-optimized tests..."

# Run with fast mode and reduced test count
cabal test --flags="fast" --test-options="--quickcheck-tests=50 --quickcheck-max-size=20"

echo "Memory-optimized tests completed."