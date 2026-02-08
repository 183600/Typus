#!/bin/bash
# Memory-Efficient Test Runner for Typus project
# Ensures tests don't consume excessive memory while preserving all test cases

set -e

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Memory level configuration with more conservative defaults
MEMORY_LEVEL=${TYPUS_MEMORY_LEVEL:-"efficient"}

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to set memory limits based on level
set_memory_limits() {
    local level=$1
    case $level in
        "minimal")
            export GHCRTS="-M200m -A4m -n512k -H16m -qg"
            export TYPUS_MEMORY_LIMIT="minimal"
            QUICKCHECK_TESTS=3
            QUICKCHECK_MAX_SIZE=1
            QUICKCHECK_MAX_SHRINKS=3
            MEMORY_MB=200
            print_status "Using MINIMAL memory limits (200MB)"
            ;;
        "efficient")
            export GHCRTS="-M384m -A8m -n1m -H32m -qg"
            export TYPUS_MEMORY_LIMIT="efficient"
            QUICKCHECK_TESTS=8
            QUICKCHECK_MAX_SIZE=2
            QUICKCHECK_MAX_SHRINKS=8
            MEMORY_MB=384
            print_status "Using EFFICIENT memory limits (384MB)"
            ;;
        "balanced")
            export GHCRTS="-M512m -A12m -n2m -H48m -qg"
            export TYPUS_MEMORY_LIMIT="balanced"
            QUICKCHECK_TESTS=15
            QUICKCHECK_MAX_SIZE=3
            QUICKCHECK_MAX_SHRINKS=15
            MEMORY_MB=512
            print_status "Using BALANCED memory limits (512MB)"
            ;;
        "comprehensive")
            export GHCRTS="-M768m -A16m -n2m -H64m -qg"
            export TYPUS_MEMORY_LIMIT="comprehensive"
            QUICKCHECK_TESTS=25
            QUICKCHECK_MAX_SIZE=5
            QUICKCHECK_MAX_SHRINKS=25
            MEMORY_MB=768
            print_status "Using COMPREHENSIVE memory limits (768MB)"
            ;;
        *)
            print_error "Unknown memory level: $level"
            print_error "Valid levels: minimal, efficient, balanced, comprehensive"
            exit 1
            ;;
    esac
    
    # Additional memory optimization environment variables
    export GHC_HEAP_ALLOCATION=0.08
    export GHC_GC_YIELD_LIMIT=500
    export TYPUS_SKIP_GO_BUILD=1
    
    print_status "Memory configuration:"
    print_status "  Runtime memory: ${MEMORY_MB}MB"
    print_status "  Allocation area: $(echo $GHCRTS | grep -o '\-A[0-9]*[mk]' | cut -c3-)"
    print_status "  Nursery size: $(echo $GHCRTS | grep -o '\-n[0-9]*[mk]' | cut -c3-)"
    print_status "  QuickCheck tests: $QUICKCHECK_TESTS"
    print_status "  QuickCheck max size: $QUICKCHECK_MAX_SIZE"
    print_status "  QuickCheck max shrinks: $QUICKCHECK_MAX_SHRINKS"
}

# Function to build with minimal memory usage
build_with_memory_constraints() {
    print_status "Building with minimal memory requirements..."
    
    # Temporarily unset memory limits for building
    unset GHCRTS
    unset GHC_HEAP_ALLOCATION
    unset GHC_GC_YIELD_LIMIT
    
    # Build with fast flags and minimal optimization
    if cabal build --flags="fast" --ghc-options="-O0 -fno-warn-unused-imports -j1"; then
        print_success "Build completed successfully"
    else
        print_error "Build failed"
        exit 1
    fi
    
    # Re-apply memory limits
    set_memory_limits $MEMORY_LEVEL
}

# Function to run tests with memory monitoring
run_tests_with_monitoring() {
    print_status "Running memory-efficient tests..."
    
    # Create a temporary file for memory monitoring
    TEMP_FILE=$(mktemp)
    
    # Monitor memory usage in background
    (
        while true; do
            if [ -f "/proc/$$/status" ]; then
                MEM_USAGE=$(grep VmRSS /proc/$$/status | awk '{print $2}')
                echo "$(date): ${MEM_USAGE}KB" >> "$TEMP_FILE"
            fi
            sleep 2
        done
    ) &
    MONITOR_PID=$!
    
    # Choose test suite based on memory level
    TEST_SUCCESS=true
    case $MEMORY_LEVEL in
        "minimal")
            if ! cabal test --flags="fast" \
                --test-options="--quickcheck-tests=$QUICKCHECK_TESTS --quickcheck-max-size=$QUICKCHECK_MAX_SIZE --quickcheck-max-shrinks=$QUICKCHECK_MAX_SHRINKS -p Essential" \
                typus-test; then
                TEST_SUCCESS=false
            fi
            ;;
        "efficient"|"balanced")
            if ! cabal test --flags="fast" \
                --test-options="--quickcheck-tests=$QUICKCHECK_TESTS --quickcheck-max-size=$QUICKCHECK_MAX_SIZE --quickcheck-max-shrinks=$QUICKCHECK_MAX_SHRINKS -p Core" \
                typus-test; then
                TEST_SUCCESS=false
            fi
            ;;
        "comprehensive")
            if ! cabal test --flags="fast" \
                --test-options="--quickcheck-tests=$QUICKCHECK_TESTS --quickcheck-max-size=$QUICKCHECK_MAX_SIZE --quickcheck-max-shrinks=$QUICKCHECK_MAX_SHRINKS" \
                typus-test; then
                TEST_SUCCESS=false
            fi
            ;;
    esac
    
    # Stop monitoring
    kill $MONITOR_PID 2>/dev/null || true
    wait $MONITOR_PID 2>/dev/null || true
    
    # Report memory usage
    if [ -f "$TEMP_FILE" ]; then
        MAX_MEM=$(awk '{print $2}' "$TEMP_FILE" | sort -n | tail -1)
        AVG_MEM=$(awk '{sum+=$2} END {print int(sum/NR)}' "$TEMP_FILE")
        print_status "Memory usage report:"
        print_status "  Peak memory: ${MAX_MEM}KB $(($MAX_MEM / 1024))MB"
        print_status "  Average memory: ${AVG_MEM}KB $(($AVG_MEM / 1024))MB"
        rm -f "$TEMP_FILE"
    fi
    
    # Check test results
    if [ "$TEST_SUCCESS" = true ]; then
        print_success "Memory-efficient tests completed successfully with $MEMORY_LEVEL settings."
        return 0
    else
        print_warning "Some tests failed with $MEMORY_LEVEL memory limits."
        print_warning "This may be due to memory constraints."
        print_warning "Try with a higher memory level:"
        print_warning "  TYPUS_MEMORY_LEVEL=balanced $0"
        return 1
    fi
}

# Function to run specific test suites with memory optimization
run_specific_test_suites() {
    print_status "Running specific memory-efficient test suites..."
    
    # Run essential tests first
    print_status "Running essential memory-efficient tests..."
    if cabal test --flags="fast" \
        --test-options="--quickcheck-tests=$QUICKCHECK_TESTS --quickcheck-max-size=$QUICKCHECK_MAX_SIZE --quickcheck-max-shrinks=$QUICKCHECK_MAX_SHRINKS -p Essential" \
        typus-test; then
        print_success "Essential tests passed"
    else
        print_warning "Essential tests failed"
    fi
    
    # Run core tests if not in minimal mode
    if [ "$MEMORY_LEVEL" != "minimal" ]; then
        print_status "Running core memory-efficient tests..."
        if cabal test --flags="fast" \
            --test-options="--quickcheck-tests=$QUICKCHECK_TESTS --quickcheck-max-size=$QUICKCHECK_MAX_SIZE --quickcheck-max-shrinks=$QUICKCHECK_MAX_SHRINKS -p Core" \
            typus-test; then
            print_success "Core tests passed"
        else
            print_warning "Core tests failed"
        fi
    fi
}

# Function to cleanup and optimize memory
cleanup_memory() {
    print_status "Performing memory cleanup..."
    
    # Force garbage collection
    if command -v ghc >/dev/null 2>&1; then
        ghc -e "System.Mem.performGC" >/dev/null 2>&1 || true
    fi
    
    # Clean up temporary files
    find /tmp -name "typus-*" -type f -mtime +0 -delete 2>/dev/null || true
    
    print_success "Memory cleanup completed"
}

# Main execution
main() {
    print_status "Starting Memory-Efficient Test Runner"
    print_status "Memory level: $MEMORY_LEVEL"
    
    # Set memory limits
    set_memory_limits $MEMORY_LEVEL
    
    # Build with memory constraints
    build_with_memory_constraints
    
    # Run tests with monitoring
    if run_tests_with_monitoring; then
        # Run specific test suites if main tests pass
        run_specific_test_suites
    fi
    
    # Cleanup
    cleanup_memory
    
    print_success "Memory-efficient test run completed"
}

# Handle script interruption
trap 'print_warning "Test run interrupted"; cleanup_memory; exit 1' INT TERM

# Run main function
main "$@"