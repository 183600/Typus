#!/bin/bash
# Advanced memory-optimized test runner with adaptive memory management and profiling

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

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
    return 0
}

# Function to set advanced memory limits
set_advanced_memory_limits() {
    local level=$1
    local available_mb=$2
    
    print_status "Setting advanced memory limits for level: $level"
    print_status "Available memory: ${available_mb}MB"
    
    case $level in
        "extreme")
            export GHCRTS="-M280m -A4m -n512k -H16m"
            export GHC_HEAP_ALLOCATION=0.03
            export GHC_GC_YIELD_LIMIT=200
            export TYPUS_MEMORY_PROFILE="false"
            QUICKCHECK_TESTS=3
            QUICKCHECK_MAX_SIZE=1
            QUICKCHECK_MAX_SHRINKS=3
            MEMORY_LIMIT_MB=280
            print_status "Memory level: EXTREME (280MB)"
            ;;
        "minimal")
            export GHCRTS="-M256m -A8m -n1m -H32m"
            export GHC_HEAP_ALLOCATION=0.05
            export GHC_GC_YIELD_LIMIT=300
            export TYPUS_MEMORY_PROFILE="false"
            QUICKCHECK_TESTS=10
            QUICKCHECK_MAX_SIZE=3
            QUICKCHECK_MAX_SHRINKS=10
            MEMORY_LIMIT_MB=256
            print_status "Memory level: MINIMAL (256MB)"
            ;;
        "conservative")
            export GHCRTS="-M512m -A16m -n2m -H64m"
            export GHC_HEAP_ALLOCATION=0.08
            export GHC_GC_YIELD_LIMIT=500
            export TYPUS_MEMORY_PROFILE="true"
            QUICKCHECK_TESTS=25
            QUICKCHECK_MAX_SIZE=5
            QUICKCHECK_MAX_SHRINKS=25
            MEMORY_LIMIT_MB=512
            print_status "Memory level: CONSERVATIVE (512MB)"
            ;;
        "adaptive")
            # Auto-select based on available memory
            if [ "$available_mb" -lt 256 ]; then
                set_advanced_memory_limits "extreme" "$available_mb"
            elif [ "$available_mb" -lt 512 ]; then
                set_advanced_memory_limits "minimal" "$available_mb"
            elif [ "$available_mb" -lt 1024 ]; then
                set_advanced_memory_limits "conservative" "$available_mb"
            else
                set_advanced_memory_limits "moderate" "$available_mb"
            fi
            return
            ;;
        "moderate")
            export GHCRTS="-M1024m -A32m -n4m -H128m"
            export GHC_HEAP_ALLOCATION=0.1
            export GHC_GC_YIELD_LIMIT=750
            export TYPUS_MEMORY_PROFILE="true"
            QUICKCHECK_TESTS=50
            QUICKCHECK_MAX_SIZE=8
            QUICKCHECK_MAX_SHRINKS=35
            MEMORY_LIMIT_MB=1024
            print_status "Memory level: MODERATE (1GB)"
            ;;
        "balanced")
            export GHCRTS="-M2048m -A64m -n8m -H256m"
            export GHC_HEAP_ALLOCATION=0.15
            export GHC_GC_YIELD_LIMIT=1000
            export TYPUS_MEMORY_PROFILE="true"
            QUICKCHECK_TESTS=100
            QUICKCHECK_MAX_SIZE=15
            QUICKCHECK_MAX_SHRINKS=50
            MEMORY_LIMIT_MB=2048
            print_status "Memory level: BALANCED (2GB)"
            ;;
        *)
            print_error "Unknown memory level: $level"
            print_status "Available levels: extreme, minimal, conservative, adaptive, moderate, balanced"
            exit 1
            ;;
    esac
    
    # Print memory configuration
    print_status "Memory configuration:"
    print_status "  Runtime memory: ${MEMORY_LIMIT_MB}MB"
    print_status "  Allocation area: $(echo $GHCRTS | grep -o '\-A[0-9]*[mk]' | cut -c3-)"
    print_status "  Nursery size: $(echo $GHCRTS | grep -o '\-n[0-9]*[mk]' | cut -c3-)"
    print_status "  Heap allocation: $GHC_HEAP_ALLOCATION"
    print_status "  GC yield limit: $GHC_GC_YIELD_LIMIT"
    print_status "  QuickCheck tests: $QUICKCHECK_TESTS"
    print_status "  QuickCheck max size: $QUICKCHECK_MAX_SIZE"
    print_status "  QuickCheck max shrinks: $QUICKCHECK_MAX_SHRINKS"
    print_status "  Memory profiling: $TYPUS_MEMORY_PROFILE"
}

# Function to run tests with memory monitoring
run_advanced_memory_tests() {
    local test_pattern=$1
    local enable_profiling=$2
    
    print_status "Building with moderate memory limits..."
    unset GHCRTS
    
    # Build with output capture for better error handling
    local build_output
    build_output=$(cabal build --flags="fast" 2>&1)
    local build_exit_code=$?
    
    if [ $build_exit_code -ne 0 ]; then
        print_error "Build failed with exit code: $build_exit_code"
        print_error "Build output:"
        echo "$build_output" | head -20
        exit 1
    fi
    
    print_success "Build completed successfully"
    
    # Set optimized memory limits for testing
    set_advanced_memory_limits "$MEMORY_LEVEL" "$AVAILABLE_MEMORY"
    
    # Set environment variables for reduced memory usage
    export TYPUS_SKIP_GO_BUILD=1
    export TYPUS_ADVANCED_MEMORY=1
    
    # Construct test options with RTS options
    local test_options="--quickcheck-tests=$QUICKCHECK_TESTS --quickcheck-max-size=$QUICKCHECK_MAX_SIZE --quickcheck-shrinks=$QUICKCHECK_MAX_SHRINKS"
    
    if [ -n "$test_pattern" ]; then
        test_options="$test_options -p $test_pattern"
    fi
    
    # Add RTS options at the beginning for proper parsing
    test_options="+RTS -M${MEMORY_LIMIT_MB}m -A16m -RTS $test_options"
    
    if [ "$enable_profiling" = "true" ]; then
        test_options="$test_options +RTS -s -h"
    fi
    
    print_status "Running advanced memory-optimized tests..."
    print_status "Test options: $test_options"
    print_status "Memory limit: ${MEMORY_LIMIT_MB}MB"
    
    # Run tests with timeout and memory monitoring
    local start_time=$(date +%s)
    
    # Capture test output for better error analysis
    local test_output
    if command -v timeout >/dev/null 2>&1; then
        # Use timeout if available (5 minutes)
        test_output=$(timeout 300 cabal test --flags="fast" --test-options="$test_options" 2>&1)
        local test_exit_code=$?
    else
        test_output=$(cabal test --flags="fast" --test-options="$test_options" 2>&1)
        local test_exit_code=$?
    fi
    
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))
    
    # Check for memory-related errors
    if echo "$test_output" | grep -q "heap exhausted\|out of memory\|Memory allocation failed"; then
        print_error "Memory allocation failed - try increasing memory limits"
        echo "$test_output" | tail -10
        return 1
    fi
    
    if [ $test_exit_code -eq 0 ]; then
        print_success "Advanced memory-optimized tests completed successfully in ${duration}s"
        # Extract test count from output
        local test_count=$(echo "$test_output" | grep -o "All [0-9]* tests passed" | grep -o "[0-9]*" || echo "unknown")
        print_status "Tests executed: $test_count"
    elif [ $test_exit_code -eq 124 ]; then
        print_error "Tests timed out after 5 minutes"
        print_warning "Try with higher memory limits or fewer tests"
        echo "$test_output" | tail -10
    else
        print_error "Tests failed with exit code: $test_exit_code"
        print_warning "Try with higher memory limits:"
        print_warning "  $0 moderate"
        print_warning "  $0 balanced"
        print_warning "Or run without memory optimization:"
        print_warning "  cabal test --flags=\"fast\""
        echo "$test_output" | tail -20
    fi
    
    return $test_exit_code
}

# Function to show usage
show_usage() {
    echo "Usage: $0 [MEMORY_LEVEL] [TEST_PATTERN] [OPTIONS]"
    echo ""
    echo "MEMORY_LEVEL options:"
    echo "  extreme     - Extreme memory optimization (128MB)"
    echo "  minimal     - Minimal memory usage (256MB)"
    echo "  conservative - Conservative memory limits (512MB)"
    echo "  adaptive    - Auto-select based on available memory (default)"
    echo "  moderate    - Moderate memory limits (1GB)"
    echo "  balanced    - Balanced memory usage (2GB)"
    echo ""
    echo "TEST_PATTERN: Optional pattern to filter tests (e.g., 'Utils', 'Parser')"
    echo ""
    echo "OPTIONS:"
    echo "  --profile   Enable memory profiling"
    echo "  --monitor   Enable real-time memory monitoring"
    echo "  --help, -h  Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0                           # Auto-detect memory level"
    echo "  $0 extreme                   # Use extreme memory optimization"
    echo "  $0 conservative Utils        # Run Utils tests with conservative limits"
    echo "  $0 moderate Parser --profile # Run Parser tests with profiling"
    echo ""
}

# Function to validate memory level
validate_memory_level() {
    local level=$1
    local valid_levels=("extreme" "minimal" "conservative" "adaptive" "moderate" "balanced")
    
    for valid_level in "${valid_levels[@]}"; do
        if [ "$level" = "$valid_level" ]; then
            return 0
        fi
    done
    
    return 1
}

# Main script execution
main() {
    # Initialize variables
    MEMORY_LEVEL="adaptive"
    TEST_PATTERN=""
    ENABLE_PROFILING="false"
    ENABLE_MONITORING="false"
    
    # Parse command line arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            extreme|minimal|conservative|adaptive|moderate|balanced)
                MEMORY_LEVEL="$1"
                shift
                ;;
            --profile)
                ENABLE_PROFILING="true"
                shift
                ;;
            --monitor)
                ENABLE_MONITORING="true"
                shift
                ;;
            --help|-h)
                show_usage
                exit 0
                ;;
            -*)
                print_warning "Unknown option: $1"
                show_usage
                exit 1
                ;;
            *)
                # Assume it's a test pattern
                TEST_PATTERN="$1"
                shift
                ;;
        esac
    done
    
    # Validate memory level
    if ! validate_memory_level "$MEMORY_LEVEL"; then
        print_error "Invalid memory level: $MEMORY_LEVEL"
        show_usage
        exit 1
    fi
    
    # Detect available memory
    AVAILABLE_MEMORY=$(detect_memory)
    
    echo "=== Typus Advanced Memory-Optimized Test Runner ==="
    echo "Memory level: $MEMORY_LEVEL"
    echo "Available memory: ${AVAILABLE_MEMORY}MB"
    echo "Profiling: $ENABLE_PROFILING"
    echo "Monitoring: $ENABLE_MONITORING"
    echo ""
    
    # Set memory limits
    set_advanced_memory_limits "$MEMORY_LEVEL" "$AVAILABLE_MEMORY"
    
    # Run tests
    run_advanced_memory_tests "$TEST_PATTERN" "$ENABLE_PROFILING"
    
    local exit_code=$?
    
    # Print summary
    echo ""
    echo "=== Test Summary ==="
    echo "Memory level: $MEMORY_LEVEL (${MEMORY_LIMIT_MB}MB)"
    echo "Test pattern: ${TEST_PATTERN:-all}"
    echo "Profiling: $ENABLE_PROFILING"
    
    if [ $exit_code -eq 0 ]; then
        print_success "All tests completed successfully"
    else
        print_error "Tests failed with exit code: $exit_code"
    fi
    
    exit $exit_code
}

# Run main function with all arguments
main "$@"