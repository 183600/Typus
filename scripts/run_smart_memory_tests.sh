#!/bin/bash
# Smart Memory Test Runner for Typus Project
# This script provides intelligent memory-optimized test execution

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Default configuration
MODE="auto"
VERBOSE=false
MEMORY_LIMIT=""
RUN_COVERAGE=false
DRY_RUN=false

# Help message
show_help() {
    echo "Typus Smart Memory Test Runner"
    echo ""
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --mode MODE         Test mode: auto, extreme, minimal, standard, ci"
    echo "  --memory-limit MB   Custom memory limit in MB"
    echo "  --verbose, -v       Enable verbose output"
    echo "  --coverage          Enable test coverage"
    echo "  --dry-run           Show configuration without running tests"
    echo "  --help, -h          Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0                           # Auto-configure based on system resources"
    echo "  $0 --mode extreme            # Extreme memory constraints (16MB)"
    echo "  $0 --mode minimal            # Minimal memory usage (32MB)"
    echo "  $0 --memory-limit 64         # Custom 64MB limit"
    echo "  $0 --mode ci --coverage      # CI mode with coverage"
    echo "  $0 --verbose --dry-run       # Show configuration details"
    echo ""
    echo "Environment Variables:"
    echo "  MEMORY_LIMIT_MB    Override memory limit"
    echo "  CI=true           Force CI mode"
    echo "  VERBOSE=true       Enable verbose output"
}

# Print colored output
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

print_config() {
    echo -e "${PURPLE}[CONFIG]${NC} $1"
}

print_memory() {
    echo -e "${CYAN}[MEMORY]${NC} $1"
}

# Parse command line arguments
parse_args() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            --mode)
                MODE="$2"
                shift 2
                ;;
            --memory-limit)
                MEMORY_LIMIT="$2"
                shift 2
                ;;
            --verbose|-v)
                VERBOSE=true
                shift
                ;;
            --coverage)
                RUN_COVERAGE=true
                shift
                ;;
            --dry-run)
                DRY_RUN=true
                shift
                ;;
            --help|-h)
                show_help
                exit 0
                ;;
            *)
                print_error "Unknown option: $1"
                show_help
                exit 1
                ;;
        esac
    done
    
    # Override with environment variables if set
    if [[ -n "$MEMORY_LIMIT_MB" ]]; then
        MEMORY_LIMIT="$MEMORY_LIMIT_MB"
    fi
    
    if [[ "$VERBOSE" == "true" ]]; then
        VERBOSE=true
    fi
    
    if [[ "$CI" == "true" && "$MODE" == "auto" ]]; then
        MODE="ci"
    fi
}

# Detect system resources
detect_system_resources() {
    print_status "Detecting system resources..."
    
    # Get total memory
    if [[ -f /proc/meminfo ]]; then
        TOTAL_MEM=$(grep MemTotal /proc/meminfo | awk '{print $2}')
        TOTAL_MEM_MB=$((TOTAL_MEM / 1024))
        AVAILABLE_MEM=$(grep MemAvailable /proc/meminfo | awk '{print $2}')
        AVAILABLE_MEM_MB=$((AVAILABLE_MEM / 1024))
    else
        # Fallback for non-Linux systems
        TOTAL_MEM_MB=4096
        AVAILABLE_MEM_MB=2048
    fi
    
    # Get CPU cores
    CPU_CORES=$(nproc 2>/dev/null || echo "4")
    
    # Check if in CI environment
    IS_CI=false
    if [[ -n "$CI" || -n "$CONTINUOUS_INTEGRATION" ]]; then
        IS_CI=true
    fi
    
    # Check if in container
    IS_CONTAINER=false
    if [[ -f /.dockerenv || -n "$DOCKER_CONTAINER" ]]; then
        IS_CONTAINER=true
    fi
    
    print_memory "Total Memory: ${TOTAL_MEM_MB}MB"
    print_memory "Available Memory: ${AVAILABLE_MEM_MB}MB"
    print_memory "CPU Cores: $CPU_CORES"
    print_memory "CI Environment: $IS_CI"
    print_memory "Container: $IS_CONTAINER"
}

# Configure memory settings based on mode
configure_memory() {
    local mode="$1"
    
    print_config "Configuring memory for mode: $mode"
    
    case $mode in
        auto)
            if [[ "$IS_CI" == "true" ]]; then
                configure_memory "ci"
            elif [[ "$AVAILABLE_MEM_MB" -lt 256 ]]; then
                configure_memory "extreme"
            elif [[ "$AVAILABLE_MEM_MB" -lt 512 ]]; then
                configure_memory "minimal"
            elif [[ "$AVAILABLE_MEM_MB" -lt 1024 ]]; then
                configure_memory "standard"
            else
                configure_memory "standard"
            fi
            ;;
        extreme)
            MEMORY_LIMIT_MB=16
            QUICKCHECK_SIZE=1
            QUICKCHECK_TESTS=2
            QUICKCHECK_SHRINKS=2
            TEST_SELECTION_RATIO=0.05
            ;;
        minimal)
            MEMORY_LIMIT_MB=32
            QUICKCHECK_SIZE=2
            QUICKCHECK_TESTS=5
            QUICKCHECK_SHRINKS=5
            TEST_SELECTION_RATIO=0.1
            ;;
        standard)
            MEMORY_LIMIT_MB=128
            QUICKCHECK_SIZE=5
            QUICKCHECK_TESTS=20
            QUICKCHECK_SHRINKS=20
            TEST_SELECTION_RATIO=0.3
            ;;
        ci)
            MEMORY_LIMIT_MB=64
            QUICKCHECK_SIZE=3
            QUICKCHECK_TESTS=10
            QUICKCHECK_SHRinks=10
            TEST_SELECTION_RATIO=0.15
            ;;
        *)
            print_error "Unknown mode: $mode"
            exit 1
            ;;
    esac
    
    # Override with custom memory limit if provided
    if [[ -n "$MEMORY_LIMIT" ]]; then
        MEMORY_LIMIT_MB="$MEMORY_LIMIT"
        # Adjust other parameters based on custom limit
        if [[ "$MEMORY_LIMIT_MB" -lt 32 ]]; then
            QUICKCHECK_SIZE=1
            QUICKCHECK_TESTS=3
            QUICKCHECK_SHRINKS=3
            TEST_SELECTION_RATIO=0.1
        elif [[ "$MEMORY_LIMIT_MB" -lt 64 ]]; then
            QUICKCHECK_SIZE=2
            QUICKCHECK_TESTS=5
            QUICKCHECK_SHRINKS=5
            TEST_SELECTION_RATIO=0.15
        elif [[ "$MEMORY_LIMIT_MB" -lt 128 ]]; then
            QUICKCHECK_SIZE=3
            QUICKCHECK_TESTS=10
            QUICKCHECK_SHRINKS=10
            TEST_SELECTION_RATIO=0.2
        fi
    fi
    
    print_memory "Memory Limit: ${MEMORY_LIMIT_MB}MB"
    print_memory "QuickCheck Size: $QUICKCHECK_SIZE"
    print_memory "QuickCheck Tests: $QUICKCHECK_TESTS"
    print_memory "QuickCheck Shrinks: $QUICKCHECK_SHRINKS"
    print_memory "Test Selection Ratio: $(echo "$TEST_SELECTION_RATIO * 100" | bc -l | cut -d. -f1)%"
}

# Set up GHC runtime options
setup_ghc_runtime() {
    local memory_mb="$1"
    
    print_config "Setting up GHC runtime options..."
    
    # Calculate GHC RTS options
    local heap_size=$((memory_mb / 4))
    local nursery_size=$((memory_mb / 16))
    local max_heap_size=$memory_mb
    
    # Ensure minimum values
    heap_size=$((heap_size > 1 ? heap_size : 1))
    nursery_size=$((nursery_size > 1 ? nursery_size : 1))
    
    export GHCRTS="-M${max_heap_size}m -A${heap_size}m -n${nursery_size}m -H$((heap_size * 2))m -qg"
    
    print_memory "GHC RTS: $GHCRTS"
}

# Build the project
build_project() {
    print_status "Building project..."
    
    # Use conservative memory settings for building
    unset GHCRTS
    
    if cabal build --flags="fast" test/SmartMemoryTestRunner.hs; then
        print_success "Project built successfully"
    else
        print_error "Build failed"
        exit 1
    fi
}

# Run the smart memory tests
run_tests() {
    local test_args=""
    
    # Build test arguments
    if [[ "$VERBOSE" == "true" ]]; then
        test_args="$test_args --verbose"
    fi
    
    case $MODE in
        auto)
            # Mode will be detected by the runner
            ;;
        extreme|minimal|standard|ci)
            test_args="$test_args --mode $MODE"
            ;;
    esac
    
    if [[ -n "$MEMORY_LIMIT" ]]; then
        test_args="$test_args --memory-limit $MEMORY_LIMIT"
    fi
    
    if [[ "$RUN_COVERAGE" == "true" ]]; then
        test_args="$test_args --coverage"
    fi
    
    if [[ "$DRY_RUN" == "true" ]]; then
        test_args="$test_args --dry-run"
    fi
    
    print_status "Running smart memory tests..."
    print_config "Test args: $test_args"
    
    if [[ "$DRY_RUN" == "true" ]]; then
        print_status "DRY RUN: Would execute tests with above configuration"
        return 0
    fi
    
    # Run the smart memory test runner
    if cabal run test/SmartMemoryTestRunner.hs -- $test_args; then
        print_success "All tests completed successfully"
    else
        print_warning "Some tests failed, but this may be due to memory constraints"
        return 1
    fi
}

# Generate test report
generate_report() {
    local exit_code="$1"
    
    print_status "Generating test report..."
    
    cat > SMART_MEMORY_TEST_REPORT.md << EOF
# Typus Smart Memory Test Report

## Test Configuration
- **Mode**: $MODE
- **Memory Limit**: ${MEMORY_LIMIT_MB}MB
- **QuickCheck Size**: $QUICKCHECK_SIZE
- **QuickCheck Tests**: $QUICKCHECK_TESTS
- **QuickCheck Shrinks**: $QUICKCHECK_SHRINKS
- **Test Selection Ratio**: $(echo "$TEST_SELECTION_RATIO * 100" | bc -l | cut -d. -f1)%

## System Resources
- **Total Memory**: ${TOTAL_MEM_MB}MB
- **Available Memory**: ${AVAILABLE_MEM_MB}MB
- **CPU Cores**: $CPU_CORES
- **CI Environment**: $IS_CI
- **Container**: $IS_CONTAINER

## GHC Runtime Options
- **GHCRTS**: $GHCRTS

## Test Results
- **Exit Code**: $exit_code
- **Status**: $([ $exit_code -eq 0 ] && echo "SUCCESS" || echo "FAILED")

## Recommendations
EOF

    if [[ $exit_code -eq 0 ]]; then
        echo "✅ All tests passed successfully" >> SMART_MEMORY_TEST_REPORT.md
        echo "💡 Current configuration is optimal for your system" >> SMART_MEMORY_TEST_REPORT.md
    else
        echo "⚠️ Some tests failed" >> SMART_MEMORY_TEST_REPORT.md
        echo "💡 Consider:" >> SMART_MEMORY_TEST_REPORT.md
        echo "   - Increasing memory limit with --memory-limit" >> SMART_MEMORY_TEST_REPORT.md
        echo "   - Using less aggressive mode (--mode minimal or --mode standard)" >> SMART_MEMORY_TEST_REPORT.md
        echo "   - Running with --verbose for detailed output" >> SMART_MEMORY_TEST_REPORT.md
    fi
    
    print_success "Test report generated: SMART_MEMORY_TEST_REPORT.md"
}

# Main execution
main() {
    echo "=== Typus Smart Memory Test Runner ==="
    echo ""
    
    parse_args "$@"
    detect_system_resources
    configure_memory "$MODE"
    setup_ghc_runtime "$MEMORY_LIMIT_MB"
    
    echo ""
    print_status "Configuration Summary:"
    print_memory "Memory Limit: ${MEMORY_LIMIT_MB}MB"
    print_memory "Test Selection: $(echo "$TEST_SELECTION_RATIO * 100" | bc -l | cut -d. -f1)% of tests"
    print_memory "GHC Runtime: $GHCRTS"
    echo ""
    
    if [[ "$DRY_RUN" != "true" ]]; then
        build_project
    fi
    
    # Run tests and capture exit code
    if run_tests; then
        TEST_EXIT_CODE=0
    else
        TEST_EXIT_CODE=1
    fi
    
    generate_report "$TEST_EXIT_CODE"
    
    echo ""
    if [[ $TEST_EXIT_CODE -eq 0 ]]; then
        print_success "🎉 Smart memory tests completed successfully!"
    else
        print_warning "⚠️ Some tests failed. Check the report for details."
    fi
    
    exit $TEST_EXIT_CODE
}

# Run main function
main "$@"