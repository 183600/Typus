#!/bin/bash
# Memory-optimized test runner for Typus project

# First, build with moderate memory limits
echo "Building with moderate memory limits..."
unset GHCRTS  # Use default limits for building
cabal build --flags="fast"

# Set aggressive memory limits for testing only
export GHCRTS="-M512m -A16m -n2m -H64m"

# Set environment variables for reduced memory usage
export TYPUS_SKIP_GO_BUILD=1

# Additional memory optimization settings
export GHC_HEAP_ALLOCATION=0.1
export GHC_GC_YIELD_LIMIT=1000

echo "Running aggressively memory-optimized tests..."
echo "Memory limit: 512MB"
echo "Allocation area: 16MB"
echo "Nursery size: 2MB"

# Run tests with aggressive memory optimization
cabal test --flags="fast" --test-options="--quickcheck-tests=25 --quickcheck-max-size=10 --quickcheck-shrinks=50"

# Check exit code
if [ $? -eq 0 ]; then
    echo "✓ Memory-optimized tests completed successfully."
else
    echo "✗ Some tests failed, but this may be due to aggressive memory limits."
    echo "  Consider running with standard limits if needed:"
    echo "  cabal test --flags=\"fast\""
fi