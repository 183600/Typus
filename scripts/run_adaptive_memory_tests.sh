#!/bin/bash
# Adaptive memory-optimized test runner for Typus project
# Provides multiple memory optimization levels based on available system resources

set -e

# Function to detect available memory
detect_memory() {
    if command -v free >/dev/null 2>&1; then
        # Linux
        free -m | awk 'NR==2{printf "%.0f", $7}'
    elif command -v vm_stat >/dev/null 2>&1; then
        # macOS
        vm_stat | awk '/free/ {gsub(/\./, "", $3); print $3 * 4096 / 1024 / 1024}'
    else
        # Default fallback
        echo "2048"
    fi
}

# Function to set memory limits based on available memory
set_memory_limits() {
    local available_mb=$1
    local level=$2
    
    echo "Available memory: ${available_mb}MB"
    
    case $level in
        "minimal")
            export GHCRTS="-M256m -A8m -n1m -H32m"
            export GHC_HEAP_ALLOCATION=0.05
            export GHC_GC_YIELD_LIMIT=300
            QUICKCHECK_TESTS=10
            QUICKCHECK_MAX_SIZE=3
            QUICKCHECK_SHRINKS=10
            echo "Memory level: MINIMAL (256MB)"
            ;;
        "ultra")
            export GHCRTS="-M512m -A16m -n2m -H64m"
            export GHC_HEAP_ALLOCATION=0.05
            export GHC_GC_YIELD_LIMIT=500
            QUICKCHECK_TESTS=25
            QUICKCHECK_MAX_SIZE=5
            QUICKCHECK_SHRINKS=25
            echo "Memory level: ULTRA (512MB)"
            ;;
        "aggressive")
            export GHCRTS="-M1024m -A32m -n4m -H128m"
            export GHC_HEAP_ALLOCATION=0.1
            export GHC_GC_YIELD_LIMIT=1000
            QUICKCHECK_TESTS=50
            QUICKCHECK_MAX_SIZE=10
            QUICKCHECK_SHRINKS=35
            echo "Memory level: AGGRESSIVE (1GB)"
            ;;
        "moderate")
            export GHCRTS="-M2048m -A64m -n8m -H256m"
            export GHC_HEAP_ALLOCATION=0.15
            export GHC_GC_YIELD_LIMIT=2000
            QUICKCHECK_TESTS=75
            QUICKCHECK_MAX_SIZE=15
            QUICKCHECK_SHRINKS=50
            echo "Memory level: MODERATE (2GB)"
            ;;
        "auto")
            # Auto-select based on available memory
            if [ "$available_mb" -lt 512 ]; then
                set_memory_limits "$available_mb" "minimal"
            elif [ "$available_mb" -lt 1024 ]; then
                set_memory_limits "$available_mb" "ultra"
            elif [ "$available_mb" -lt 2048 ]; then
                set_memory_limits "$available_mb" "aggressive"
            else
                set_memory_limits "$available_mb" "moderate"
            fi
            return
            ;;
        *)
            echo "Unknown memory level: $level"
            echo "Available levels: minimal, ultra, aggressive, moderate, auto"
            exit 1
            ;;
    esac
}

# Function to run tests with memory monitoring
run_memory_monitored_tests() {
    local test_pattern=$1
    
    echo "Starting memory-monitored test run..."
    echo "Test pattern: ${test_pattern:-all}"
    
    # Build with moderate memory limits first
    echo "Building with moderate memory limits..."
    unset GHCRTS
    cabal build --flags="fast"
    
    # Set optimized memory limits for testing
    set_memory_limits "$AVAILABLE_MEMORY" "$MEMORY_LEVEL"
    
    # Set environment variables for reduced memory usage
    export TYPUS_SKIP_GO_BUILD=1
    
    echo "QuickCheck settings: tests=$QUICKCHECK_TESTS, max_size=$QUICKCHECK_MAX_SIZE, shrinks=$QUICKCHECK_SHRINKS"
    
    # Run tests with memory optimization
    if [ -n "$test_pattern" ]; then
        cabal test --flags="fast" --test-options="--quickcheck-tests=$QUICKCHECK_TESTS --quickcheck-max-size=$QUICKCHECK_MAX_SIZE --quickcheck-shrinks=$QUICKCHECK_SHRINKS -p $test_pattern"
    else
        cabal test --flags="fast" --test-options="--quickcheck-tests=$QUICKCHECK_TESTS --quickcheck-max-size=$QUICKCHECK_MAX_SIZE --quickcheck-shrinks=$QUICKCHECK_SHRINKS"
    fi
    
    local exit_code=$?
    
    if [ $exit_code -eq 0 ]; then
        echo "✓ Memory-optimized tests completed successfully."
    else
        echo "✗ Some tests failed with current memory limits."
        echo "  Try running with higher memory limits:"
        echo "  $0 moderate"
        echo "  $0 aggressive"
        echo "  Or run without memory optimization:"
        echo "  cabal test --flags=\"fast\""
    fi
    
    return $exit_code
}

# Function to show usage
show_usage() {
    echo "Usage: $0 [MEMORY_LEVEL] [TEST_PATTERN]"
    echo ""
    echo "MEMORY_LEVEL options:"
    echo "  minimal    - Minimal memory usage (256MB)"
    echo "  ultra      - Ultra low memory usage (512MB)"
    echo "  aggressive - Aggressive memory limits (1GB)"
    echo "  moderate   - Moderate memory limits (2GB)"
    echo "  auto       - Auto-select based on available memory (default)"
    echo ""
    echo "TEST_PATTERN: Optional pattern to filter tests (e.g., 'Utils', 'Parser')"
    echo ""
    echo "Examples:"
    echo "  $0                    # Auto-detect memory level"
    echo "  $0 ultra              # Use ultra memory optimization"
    echo "  $0 aggressive Utils   # Run Utils tests with aggressive limits"
    echo ""
}

# Main script execution
main() {
    # Parse command line arguments
    MEMORY_LEVEL=${1:-auto}
    TEST_PATTERN=${2:-}
    
    # Show help if requested
    if [[ "$MEMORY_LEVEL" == "-h" || "$MEMORY_LEVEL" == "--help" ]]; then
        show_usage
        exit 0
    fi
    
    # Detect available memory
    AVAILABLE_MEMORY=$(detect_memory)
    
    echo "=== Typus Adaptive Memory-Optimized Test Runner ==="
    
    # Set memory limits
    set_memory_limits "$AVAILABLE_MEMORY" "$MEMORY_LEVEL"
    
    # Run tests
    run_memory_monitored_tests "$TEST_PATTERN"
}

# Run main function
main "$@"