#!/bin/bash
# Enhanced memory-optimized test runner for Typus project with granular memory control

# Set memory level based on environment variable or default to conservative
MEMORY_LEVEL=${TYPUS_MEMORY_LEVEL:-"conservative"}

# First, build with moderate memory limits
echo "Building with moderate memory limits..."
unset GHCRTS  # Use default limits for building
cabal build --flags="fast"

# Set memory limits based on memory level
case $MEMORY_LEVEL in
  "minimal")
    export GHCRTS="-M256m -A8m -n1m -H32m"
    export GHC_HEAP_ALLOCATION=0.05
    export GHC_GC_YIELD_LIMIT=300
    QUICKCHECK_TESTS=8
    QUICKCHECK_MAX_SIZE=2
    QUICKCHECK_MAX_SHRINKS=8
    echo "Using MINIMAL memory limits (256MB)"
    ;;
  "strict")
    export GHCRTS="-M512m -A12m -n2m -H48m"
    export GHC_HEAP_ALLOCATION=0.08
    export GHC_GC_YIELD_LIMIT=500
    QUICKCHECK_TESTS=15
    QUICKCHECK_MAX_SIZE=3
    QUICKCHECK_MAX_SHRINKS=15
    echo "Using STRICT memory limits (512MB)"
    ;;
  "conservative")
    export GHCRTS="-M768m -A16m -n2m -H64m"
    export GHC_HEAP_ALLOCATION=0.1
    export GHC_GC_YIELD_LIMIT=750
    QUICKCHECK_TESTS=30
    QUICKCHECK_MAX_SIZE=5
    QUICKCHECK_MAX_SHRINKS=25
    echo "Using CONSERVATIVE memory limits (768MB)"
    ;;
  "moderate")
    export GHCRTS="-M1024m -A20m -n4m -H96m"
    export GHC_HEAP_ALLOCATION=0.15
    export GHC_GC_YIELD_LIMIT=1000
    QUICKCHECK_TESTS=50
    QUICKCHECK_MAX_SIZE=8
    QUICKCHECK_MAX_SHRINKS=35
    echo "Using MODERATE memory limits (1GB)"
    ;;
  *)
    echo "Unknown memory level: $MEMORY_LEVEL"
    echo "Valid levels: minimal, strict, conservative, moderate"
    exit 1
    ;;
esac

# Set environment variables for reduced memory usage
export TYPUS_SKIP_GO_BUILD=1

echo "Memory configuration:"
echo "  Runtime memory: $(echo $GHCRTS | grep -o '\-M[0-9]*m' | cut -c3-)"
echo "  Allocation area: $(echo $GHCRTS | grep -o '\-A[0-9]*m' | cut -c3-)"
echo "  Nursery size: $(echo $GHCRTS | grep -o '\-n[0-9]*m' | cut -c3-)"
echo "  QuickCheck tests: $QUICKCHECK_TESTS"
echo "  QuickCheck max size: $QUICKCHECK_MAX_SIZE"
echo "  QuickCheck max shrinks: $QUICKCHECK_MAX_SHRINKS"

# Run tests with enhanced memory optimization
echo "Running enhanced memory-optimized tests..."

# Choose the appropriate test suite based on memory level
case $MEMORY_LEVEL in
  "minimal")
    cabal test --flags="fast" --test-options="--quickcheck-tests=$QUICKCHECK_TESTS --quickcheck-max-size=$QUICKCHECK_MAX_SIZE --quickcheck-shrinks=$QUICKCHECK_MAX_SHRINKS -p Ultra" typus-test
    ;;
  "strict")
    cabal test --flags="fast" --test-options="--quickcheck-tests=$QUICKCHECK_TESTS --quickcheck-max-size=$QUICKCHECK_MAX_SIZE --quickcheck-shrinks=$QUICKCHECK_MAX_SHRINKS -p Strict" typus-test
    ;;
  "conservative"|"moderate")
    cabal test --flags="fast" --test-options="--quickcheck-tests=$QUICKCHECK_TESTS --quickcheck-max-size=$QUICKCHECK_MAX_SIZE --quickcheck-shrinks=$QUICKCHECK_MAX_SHRINKS" typus-test
    ;;
esac

# Check exit code
if [ $? -eq 0 ]; then
    echo "✓ Enhanced memory-optimized tests completed successfully with $MEMORY_LEVEL settings."
else
    echo "✗ Tests failed with $MEMORY_LEVEL memory limits."
    echo "  Try with a higher memory level:"
    echo "  TYPUS_MEMORY_LEVEL=moderate ./scripts/run_enhanced_memory_optimized_tests.sh"
    echo "  Or use standard memory-optimized tests:"
    echo "  ./scripts/run_memory_optimized_tests.sh"
fi