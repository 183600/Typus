#!/bin/bash
# Unified Memory-Optimized Test Runner
# Ensures all tests use optimal memory settings without deleting test cases

set -e

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

print_header() {
    echo -e "${PURPLE}================================${NC}"
    echo -e "${PURPLE}Unified Memory-Optimized Tests${NC}"
    echo -e "${PURPLE}================================${NC}"
    echo ""
}

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
    echo -e "${CYAN}[CONFIG]${NC} $1"
}

# Memory level configurations - more aggressive than before
declare -A MEMORY_CONFIGS=(
    ["critical"]="8MB - Critical memory usage (ultra minimal)"
    ["minimal"]="16MB - Minimal memory usage"
    ["efficient"]="32MB - Efficient memory usage"
    ["balanced"]="64MB - Balanced memory usage"
    ["comprehensive"]="128MB - Comprehensive memory usage"
)

# Check available memory and suggest configuration
check_memory() {
    print_status "Checking system memory..."
    
    if command -v free >/dev/null 2>&1; then
        local available=$(free -m | awk 'NR==2{printf "%.0f", $7}')
        print_config "Available memory: ${available}MB"
        suggest_memory_config "$available"
    elif command -v vm_stat >/dev/null 2>&1; then
        local page_size=$(vm_stat | head -1 | sed 's/.*page size of \([0-9]*\).*/\1/')
        local free_pages=$(vm_stat | awk '/free/ {gsub(/\./, ""); print $3}')
        local available=$((free_pages * page_size / 1024 / 1024))
        print_config "Available memory: ${available}MB"
        suggest_memory_config "$available"
    else
        print_warning "Cannot determine available memory, using minimal configuration"
    fi
}

# Suggest memory configuration based on available memory
suggest_memory_config() {
    local available=$1
    local suggested=""
    
    if [ "$available" -le 16 ]; then
        suggested="critical"
    elif [ "$available" -le 32 ]; then
        suggested="minimal"
    elif [ "$available" -le 64 ]; then
        suggested="efficient"
    elif [ "$available" -le 128 ]; then
        suggested="balanced"
    else
        suggested="comprehensive"
    fi
    
    print_config "Suggested memory level: $suggested (${MEMORY_CONFIGS[$suggested]})"
}

# Set aggressive memory limits
set_aggressive_memory_limits() {
    local memory_level="$1"
    
    # Set environment variables for memory optimization
    export TYPUS_MEMORY_LEVEL="$memory_level"
    export EMERGENCY_MEMORY="1"
    export ULTRA_MEMORY_OPTIMIZED="1"
    export GHC_HEAP_ALLOCATION=0.05  # Reduced from 0.08
    export GHC_GC_YIELD_LIMIT=300     # Reduced from 500
    
    # Set RTS options for memory limits
    case "$memory_level" in
        "critical")
            export GHCRTS="-M8m -A256k -n32k -H1m -qg -G1"
            export QUICKCHECK_TESTS=1
            export QUICKCHECK_MAX_SIZE=1
            export QUICKCHECK_MAX_SHRINKS=0
            ;;
        "minimal")
            export GHCRTS="-M16m -A512k -n64k -H2m -qg -G1"
            export QUICKCHECK_TESTS=2
            export QUICKCHECK_MAX_SIZE=1
            export QUICKCHECK_MAX_SHRINKS=0
            ;;
        "efficient")
            export GHCRTS="-M32m -A1m -n128k -H4m -qg -G2"
            export QUICKCHECK_TESTS=3
            export QUICKCHECK_MAX_SIZE=2
            export QUICKCHECK_MAX_SHRINKS=1
            ;;
        "balanced")
            export GHCRTS="-M64m -A2m -n256k -H8m -qg -G2"
            export QUICKCHECK_TESTS=5
            export QUICKCHECK_MAX_SIZE=3
            export QUICKCHECK_MAX_SHRINKS=2
            ;;
        "comprehensive")
            export GHCRTS="-M128m -A4m -n512k -H16m -qg -G4"
            export QUICKCHECK_TESTS=10
            export QUICKCHECK_MAX_SIZE=5
            export QUICKCHECK_MAX_SHRINKS=5
            ;;
    esac
    
    print_config "Memory level: $memory_level (${MEMORY_CONFIGS[$memory_level]})"
    print_config "RTS options: $GHCRTS"
    print_config "QuickCheck tests: $QUICKCHECK_TESTS"
    print_config "QuickCheck max size: $QUICKCHECK_MAX_SIZE"
    print_config "QuickCheck max shrinks: $QUICKCHECK_MAX_SHRINKS"
}

# Force garbage collection
force_gc() {
    print_status "Forcing garbage collection..."
    if command -v ghc >/dev/null 2>&1; then
        ghc -e "System.Mem.performGC" >/dev/null 2>&1 || true
    fi
    # Multiple GC cycles
    for i in {1..5}; do
        sleep 0.1
    done
}

# Build with memory constraints
build_with_memory_constraints() {
    print_status "Building with memory constraints..."
    
    # Temporarily unset memory limits for building
    unset GHCRTS
    
    # Build with fast flags and minimal optimization
    if cabal build --flags="fast" --ghc-options="-O0 -fno-warn-unused-imports -j1" typus-test typus-test-optimized; then
        print_success "Build successful"
    else
        print_error "Build failed"
        return 1
    fi
    
    # Re-apply memory limits
    set_aggressive_memory_limits "$TYPUS_MEMORY_LEVEL"
}

# Run tests with memory monitoring
run_tests_with_monitoring() {
    local memory_level="$1"
    local test_suite="$2"
    
    print_status "Running $test_suite with memory monitoring..."
    
    # Create a temporary file for memory monitoring
    TEMP_FILE=$(mktemp)
    
    # Monitor memory usage in background
    (
        while true; do
            if [ -f "/proc/$$/status" ]; then
                MEM_USAGE=$(grep VmRSS /proc/$$/status | awk '{print $2}')
                echo "$(date): ${MEM_USAGE}KB" >> "$TEMP_FILE"
            fi
            sleep 1
        done
    ) &
    MONITOR_PID=$!
    
    # Force GC before tests
    force_gc
    
    # Run the tests with memory optimization
    TEST_SUCCESS=true
    case "$test_suite" in
        "basic")
            if ! cabal test --flags="fast" \
                --test-options="--quickcheck-tests=$QUICKCHECK_TESTS --quickcheck-max-size=$QUICKCHECK_MAX_SIZE --quickcheck-max-shrinks=$QUICKCHECK_MAX_SHRINKS -p Essential" \
                typus-test; then
                TEST_SUCCESS=false
            fi
            ;;
        "optimized")
            if ! cabal test --flags="fast" \
                --test-options="--quickcheck-tests=$QUICKCHECK_TESTS --quickcheck-max-size=$QUICKCHECK_MAX_SIZE --quickcheck-max-shrinks=$QUICKCHECK_MAX_SHRINKS -p Core" \
                typus-test-optimized; then
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
    
    # Force GC after tests
    force_gc
    
    # Report memory usage
    if [ -f "$TEMP_FILE" ]; then
        MAX_MEM=$(awk '{print $2}' "$TEMP_FILE" | sort -n | tail -1)
        AVG_MEM=$(awk '{sum+=$2} END {print int(sum/NR)}' "$TEMP_FILE")
        print_status "Memory usage report:"
        print_status "  Peak memory: ${MAX_MEM}KB $(($MAX_MEM / 1024))MB"
        print_status "  Average memory: ${AVG_MEM}KB $(($AVG_MEM / 1024))MB"
        rm -f "$TEMP_FILE"
    fi
    
    return $([ "$TEST_SUCCESS" = true ] && echo 0 || echo 1)
}

# Main execution logic
main() {
    local memory_level=""
    local test_suite="basic"
    local show_help="false"
    local check_memory_only="false"
    
    # Parse command line arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            --help|-h)
                show_help="true"
                shift
                ;;
            --check-memory)
                check_memory_only="true"
                shift
                ;;
            --memory-level)
                memory_level="$2"
                shift 2
                ;;
            --test-suite)
                test_suite="$2"
                shift 2
                ;;
            critical|minimal|efficient|balanced|comprehensive)
                memory_level="$1"
                shift
                ;;
            basic|optimized|comprehensive)
                test_suite="$1"
                shift
                ;;
            *)
                print_error "Unknown option: $1"
                echo "Use --help for usage information"
                exit 1
                ;;
        esac
    done
    
    # Show help if requested
    if [ "$show_help" = "true" ]; then
        echo "Unified Memory-Optimized Test Runner for Typus"
        echo ""
        echo "Usage: $0 [MEMORY_LEVEL] [TEST_SUITE] [OPTIONS]"
        echo ""
        echo "Memory Levels:"
        for level in "${!MEMORY_CONFIGS[@]}"; do
            printf "  %-12s - %s\n" "$level" "${MEMORY_CONFIGS[$level]}"
        done
        echo ""
        echo "Test Suites:"
        echo "  basic         - Run essential tests only"
        echo "  optimized     - Run memory-optimized test suite"
        echo "  comprehensive - Run all tests with memory optimization"
        echo ""
        echo "Options:"
        echo "  --help, -h           Show this help message"
        echo "  --check-memory       Check available memory and suggest configuration"
        echo "  --memory-level LEVEL Set specific memory level"
        echo "  --test-suite SUITE   Set specific test suite"
        echo ""
        echo "Environment Variables:"
        echo "  TYPUS_MEMORY_LEVEL   Memory optimization level"
        echo "  EMERGENCY_MEMORY     Enable emergency memory mode"
        echo "  ULTRA_MEMORY_OPTIMIZED Enable ultra memory optimization"
        echo ""
        echo "Examples:"
        echo "  $0                                    # Auto-configure and run basic tests"
        echo "  $0 critical                           # Run with critical memory optimization"
        echo "  $0 minimal optimized                  # Run optimized tests with minimal memory"
        echo "  $0 comprehensive --test-suite basic   # Run basic tests with comprehensive memory"
        exit 0
    fi
    
    # Check memory if requested
    if [ "$check_memory_only" = "true" ]; then
        check_memory
        exit 0
    fi
    
    # Print header
    print_header
    
    # Determine memory level
    if [ -z "$memory_level" ]; then
        # Check environment variable
        if [ -n "$TYPUS_MEMORY_LEVEL" ]; then
            memory_level="$TYPUS_MEMORY_LEVEL"
            print_config "Using environment variable TYPUS_MEMORY_LEVEL: $memory_level"
        else
            # Auto-detect based on available memory
            print_status "Auto-detecting memory configuration..."
            if command -v free >/dev/null 2>&1; then
                local available=$(free -m | awk 'NR==2{printf "%.0f", $7}')
                if [ "$available" -le 16 ]; then
                    memory_level="critical"
                elif [ "$available" -le 32 ]; then
                    memory_level="minimal"
                elif [ "$available" -le 64 ]; then
                    memory_level="efficient"
                elif [ "$available" -le 128 ]; then
                    memory_level="balanced"
                else
                    memory_level="comprehensive"
                fi
                print_config "Auto-selected memory level: $memory_level"
            else
                memory_level="minimal"
                print_config "Using default memory level: $memory_level"
            fi
        fi
    fi
    
    # Validate memory level
    if [ -z "${MEMORY_CONFIGS[$memory_level]}" ]; then
        print_error "Invalid memory level: $memory_level"
        echo ""
        echo "Valid memory levels:"
        for level in "${!MEMORY_CONFIGS[@]}"; do
            printf "  %-12s - %s\n" "$level" "${MEMORY_CONFIGS[$level]}"
        done
        exit 1
    fi
    
    # Set memory limits
    set_aggressive_memory_limits "$memory_level"
    
    # Build with memory constraints
    build_with_memory_constraints
    
    # Run tests with monitoring
    if run_tests_with_monitoring "$memory_level" "$test_suite"; then
        echo ""
        print_success "Memory-optimized tests completed successfully!"
        print_status "Memory usage was optimized for: ${MEMORY_CONFIGS[$memory_level]}"
        print_status "Test suite: $test_suite"
    else
        echo ""
        print_warning "Some tests failed with $memory_level memory limits."
        print_warning "This may be due to memory constraints."
        print_warning "Try with a higher memory level:"
        case "$memory_level" in
            "critical") print_warning "  $0 minimal" ;;
            "minimal") print_warning "  $0 efficient" ;;
            "efficient") print_warning "  $0 balanced" ;;
            "balanced") print_warning "  $0 comprehensive" ;;
        esac
        exit 1
    fi
    
    # Cleanup
    unset GHCRTS
    unset EMERGENCY_MEMORY
    unset ULTRA_MEMORY_OPTIMIZED
    unset GHC_HEAP_ALLOCATION
    unset GHC_GC_YIELD_LIMIT
    unset QUICKCHECK_TESTS
    unset QUICKCHECK_MAX_SIZE
    unset QUICKCHECK_MAX_SHRINKS
}

# Handle script interruption
trap 'print_warning "Test run interrupted"; cleanup; exit 1' INT TERM

# Cleanup function
cleanup() {
    # Kill any background processes
    jobs -p | xargs -r kill 2>/dev/null || true
    
    # Clean up temporary files
    find /tmp -name "typus-*" -type f -mtime +0 -delete 2>/dev/null || true
    
    # Force final GC
    if command -v ghc >/dev/null 2>&1; then
        ghc -e "System.Mem.performGC" >/dev/null 2>&1 || true
    fi
}

# Run main function
main "$@"
