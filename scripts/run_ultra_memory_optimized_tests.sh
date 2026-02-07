#!/bin/bash
# Ultra memory-optimized test runner for severely memory-constrained environments

# First, build with moderate memory limits
echo "Building with moderate memory limits..."
unset GHCRTS  # Use default limits for building
cabal build --flags="fast"

# Set very aggressive memory limits for testing only
export GHCRTS="-M512m -A16m -n2m -H64m"

# Set environment variables for minimal memory usage
export TYPUS_SKIP_GO_BUILD=1

# Additional ultra memory optimization settings
export GHC_HEAP_ALLOCATION=0.05
export GHC_GC_YIELD_LIMIT=500

echo "Running ultra memory-optimized tests..."
echo "Memory limit: 512MB"
echo "Allocation area: 16MB"
echo "Nursery size: 2MB"

# Run only essential test groups with minimal test count
cabal test --flags="fast" --test-options="--quickcheck-tests=10 --quickcheck-max-size=5 --quickcheck-shrinks=25 -p Utils"

# Check exit code
if [ $? -eq 0 ]; then
    echo "✓ Ultra memory-optimized tests completed successfully."
else
    echo "✗ Tests failed with ultra memory limits."
    echo "  Try standard memory-optimized tests:"
    echo "  ./scripts/run_memory_optimized_tests.sh"
fi