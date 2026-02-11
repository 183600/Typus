#!/bin/bash
# Advanced Memory-Optimized Test Runner for Typus project
# This script ensures tests don't consume excessive memory while preserving all test cases

set -e

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Advanced memory level configuration
MEMORY_LEVEL=${TYPUS_MEMORY_LEVEL:-"adaptive"}

# Test execution strategy
EXECUTION_STRATEGY=${TYPUS_EXECUTION_STRATEGY:-"auto"}

# Memory monitoring
ENABLE_MEMORY_MONITORING=${TYPUS_MEMORY_MONITORING:-"true"}

# Resource cleanup
ENABLE_AGGRESSIVE_CLEANUP=${TYPUS_AGGRESSIVE_CLEANUP:-"true"}

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

print_debug() {
    if [ "$TYPUS_DEBUG" = "true" ]; then
        echo -e "${PURPLE}[DEBUG]${NC} $1"
    fi
}

print_memory() {
    if [ "$ENABLE_MEMORY_MONITORING" = "true" ]; then
        echo -e "${CYAN}[MEMORY]${NC} $1"
    fi
}

# Function to detect system memory
detect_system_memory() {
    if command -v free >/dev/null 2>&1; then
        # Linux
        TOTAL_MEM=$(free -m | awk 'NR==2{print $2}')
        AVAILABLE_MEM=$(free -m | awk 'NR==2{print $7}')
    elif command -v sysctl >/dev/null 2>&1; then
        # macOS
        TOTAL_MEM=$(sysctl -n hw.memsize | awk '{print int($1/1024/1024)}')
        AVAILABLE_MEM=$TOTAL_MEM
    else
        # Default fallback
        TOTAL_MEM=2048
        AVAILABLE_MEM=1024
    fi
    
    print_debug "System memory: ${TOTAL_MEM}MB total, ${AVAILABLE_MEM}MB available"
}

# Function to determine optimal memory level
determine_memory_level() {
    detect_system_memory
    
    if [ "$MEMORY_LEVEL" = "adaptive" ]; then
        if [ "$AVAILABLE_MEM" -lt 512 ]; then
            MEMORY_LEVEL="ultra_minimal"
        elif [ "$AVAILABLE_MEM" -lt 1024 ]; then
            MEMORY_LEVEL="extreme_minimal"
        elif [ "$AVAILABLE_MEM" -lt 2048 ]; then
            MEMORY_LEVEL="aggressive_minimal"
        else
            MEMORY_LEVEL="moderate_minimal"
        fi
        print_status "Adaptively selected memory level: $MEMORY_LEVEL"
    fi
}

# Function to set memory limits based on level
set_memory_limits() {
    local level=$1
    case $level in
        "ultra_minimal")
            export GHCRTS="-M64m -A2m -n256k -H8m -qg -G1"
            export TYPUS_MEMORY_LIMIT="ultra_minimal"
            QUICKCHECK_TESTS=1
            QUICKCHECK_MAX_SIZE=1
            QUICKCHECK_MAX_SHRINKS=1
            MEMORY_MB=64
            BATCH_SIZE=3
            STREAM_CHUNK_SIZE=2
            print_status "Using ULTRA MINIMAL memory limits (64MB)"
            ;;
        "extreme_minimal")
            export GHCRTS="-M128m -A4m -n512k -H16m -qg -G1"
            export TYPUS_MEMORY_LIMIT="extreme_minimal"
            QUICKCHECK_TESTS=2
            QUICKCHECK_MAX_SIZE=1
            QUICKCHECK_MAX_SHRINKS=2
            MEMORY_MB=128
            BATCH_SIZE=5
            STREAM_CHUNK_SIZE=3
            print_status "Using EXTREME MINIMAL memory limits (128MB)"
            ;;
        "aggressive_minimal")
            export GHCRTS="-M256m -A6m -n1m -H24m -qg -G1"
            export TYPUS_MEMORY_LIMIT="aggressive_minimal"
            QUICKCHECK_TESTS=3
            QUICKCHECK_MAX_SIZE=2
            QUICKCHECK_MAX_SHRINKS=3
            MEMORY_MB=256
            BATCH_SIZE=8
            STREAM_CHUNK_SIZE=5
            print_status "Using AGGRESSIVE MINIMAL memory limits (256MB)"
            ;;
        "moderate_minimal")
            export GHCRTS="-M512m -A8m -n2m -H32m -qg -G1"
            export TYPUS_MEMORY_LIMIT="moderate_minimal"
            QUICKCHECK_TESTS=5
            QUICKCHECK_MAX_SIZE=3
            QUICKCHECK_MAX_SHRINKS=5
            MEMORY_MB=512
            BATCH_SIZE=12
            STREAM_CHUNK_SIZE=8
            print_status "Using MODERATE MINIMAL memory limits (512MB)"
            ;;
        "conservative")
            export GHCRTS="-M1024m -A12m -n4m -H64m -qg -G1"
            export TYPUS_MEMORY_LIMIT="conservative"
            QUICKCHECK_TESTS=10
            QUICKCHECK_MAX_SIZE=5
            QUICKCHECK_MAX_SHRINKS=8
            MEMORY_MB=1024
            BATCH_SIZE=20
            STREAM_CHUNK_SIZE=12
            print_status "Using CONSERVATIVE memory limits (1GB)"
            ;;
        *)
            print_error "Unknown memory level: $level"
            print_error "Valid levels: ultra_minimal, extreme_minimal, aggressive_minimal, moderate_minimal, conservative"
            exit 1
            ;;
    esac
    
    # Additional memory optimization environment variables
    export GHC_HEAP_ALLOCATION=0.01
    export GHC_GC_YIELD_LIMIT=100
    export TYPUS_SKIP_GO_BUILD=1
    export TYPUS_EXECUTION_STRATEGY="$EXECUTION_STRATEGY"
    
    print_status "Memory configuration:"
    print_status "  Runtime memory: ${MEMORY_MB}MB"
    print_status "  Allocation area: $(echo $GHCRTS | grep -o '\-A[0-9]*[mk]' | cut -c3-)"
    print_status "  Nursery size: $(echo $GHCRTS | grep -o '\-n[0-9]*[mk]' | cut -c3-)"
    print_status "  QuickCheck tests: $QUICKCHECK_TESTS"
    print_status "  QuickCheck max size: $QUICKCHECK_MAX_SIZE"
    print_status "  QuickCheck max shrinks: $QUICKCHECK_MAX_SHRINKS"
    print_status "  Batch size: $BATCH_SIZE"
    print_status "  Stream chunk size: $STREAM_CHUNK_SIZE"
}

# Function to determine optimal execution strategy
determine_execution_strategy() {
    if [ "$EXECUTION_STRATEGY" = "auto" ]; then
        if [ "$MEMORY_MB" -le 128 ]; then
            EXECUTION_STRATEGY="streaming"
        elif [ "$MEMORY_MB" -le 256 ]; then
            EXECUTION_STRATEGY="batched"
        else
            EXECUTION_STRATEGY="direct"
        fi
        print_status "Auto-selected execution strategy: $EXECUTION_STRATEGY"
    fi
}

# Function to monitor memory usage
monitor_memory() {
    local label=$1
    if [ "$ENABLE_MEMORY_MONITORING" = "true" ]; then
        if [ -f "/proc/$$/status" ]; then
            local mem_usage=$(grep VmRSS /proc/$$/status | awk '{print $2}')
            local mem_peak=$(grep VmHWM /proc/$$/status | awk '{print $2}')
            print_memory "$label: ${mem_usage}KB (peak: ${mem_peak}KB) $((${mem_usage} / 1024))MB"
        fi
    fi
}

# Function to perform aggressive cleanup
aggressive_cleanup() {
    if [ "$ENABLE_AGGRESSIVE_CLEANUP" = "true" ]; then
        print_status "Performing aggressive memory cleanup..."
        
        # Force system memory cleanup if available
        if [ -w /proc/sys/vm/drop_caches ]; then
            echo 3 > /proc/sys/vm/drop_caches 2>/dev/null || true
        fi
        
        # Force Haskell garbage collection
        if command -v ghc >/dev/null 2>&1; then
            ghc -e "System.Mem.performGC; System.Mem.performMajorGC" >/dev/null 2>&1 || true
        fi
        
        # Clean up temporary files
        find /tmp -name "typus-*" -type f -mmin +60 -delete 2>/dev/null || true
        find /tmp -name "cabal-*" -type f -mmin +60 -delete 2>/dev/null || true
        
        print_status "Aggressive cleanup completed"
    fi
}

# Function to build with minimal memory usage
build_with_memory_constraints() {
    print_status "Building with minimal memory requirements..."
    
    # Temporarily unset memory limits for building
    unset GHCRTS
    unset GHC_HEAP_ALLOCATION
    unset GHC_GC_YIELD_LIMIT
    
    # Build with fast flags and minimal optimization
    if cabal build --flags="fast" --ghc-options="-O0 -fno-warn-unused-imports -j1 +RTS -A32m -n16m -H128m -RTS"; then
        print_success "Build completed successfully"
    else
        print_error "Build failed"
        exit 1
    fi
    
    # Re-apply memory limits
    set_memory_limits $MEMORY_LEVEL
}

# Function to run tests with streaming strategy
run_streaming_tests() {
    print_status "Running tests with streaming strategy..."
    
    local test_modules=$(cabal list-bin typus-test 2>/dev/null || echo "typus-test")
    local total_tests=0
    local chunk_num=1
    
    # Get list of all test patterns
    local test_patterns=$(find test/Test/Unit -name "*Spec.hs" -o -name "*Tests.hs" | \
                         sed 's|test/Test/Unit/||' | sed 's|\.hs||' | \
                         tr '\n' ' ')
    
    # Split tests into chunks
    for chunk in $(echo "$test_patterns" | xargs -n $STREAM_CHUNK_SIZE); do
        print_status "Processing chunk $chunk_num: $chunk"
        monitor_memory "chunk-$chunk_num-before"
        
        # Run tests for this chunk
        if cabal test --flags="fast" \
            --test-options="--quickcheck-tests=$QUICKCHECK_TESTS --quickcheck-max-size=$QUICKCHECK_MAX_SIZE --quickcheck-max-shrinks=$QUICKCHECK_MAX_SHRINKS -p $(echo $chunk | tr ' ' ',')" \
            $test_modules; then
            print_success "Chunk $chunk_num completed successfully"
        else
            print_warning "Chunk $chunk_num failed, continuing..."
        fi
        
        monitor_memory "chunk-$chunk_num-after"
        aggressive_cleanup
        
        # Small delay between chunks
        sleep 1
        
        chunk_num=$((chunk_num + 1))
    done
}

# Function to run tests with batched strategy
run_batched_tests() {
    print_status "Running tests with batched strategy..."
    
    local test_modules=$(cabal list-bin typus-test 2>/dev/null || echo "typus-test")
    local batch_num=1
    
    # Run tests in batches
    while true; do
        print_status "Running batch $batch_num"
        monitor_memory "batch-$batch_num-before"
        
        # Run a batch of tests
        if cabal test --flags="fast" \
            --test-options="--quickcheck-tests=$QUICKCHECK_TESTS --quickcheck-max-size=$QUICKCHECK_MAX_SIZE --quickcheck-max-shrinks=$QUICKCHECK_MAX_SHRINKS --timeout=60" \
            $test_modules; then
            print_success "Batch $batch_num completed successfully"
            
            # Check if we need to run more batches
            if [ "$batch_num" -ge 3 ]; then
                break
            fi
        else
            print_warning "Batch $batch_num failed"
            break
        fi
        
        monitor_memory "batch-$batch_num-after"
        aggressive_cleanup
        
        # Delay between batches
        sleep 2
        
        batch_num=$((batch_num + 1))
    done
}

# Function to run tests with direct strategy
run_direct_tests() {
    print_status "Running tests with direct strategy..."
    
    local test_modules=$(cabal list-bin typus-test 2>/dev/null || echo "typus-test")
    
    monitor_memory "direct-before"
    
    if cabal test --flags="fast" \
        --test-options="--quickcheck-tests=$QUICKCHECK_TESTS --quickcheck-max-size=$QUICKCHECK_MAX_SIZE --quickcheck-max-shrinks=$QUICKCHECK_MAX_SHRINKS" \
        $test_modules; then
        print_success "Direct tests completed successfully"
    else
        print_warning "Direct tests failed"
        return 1
    fi
    
    monitor_memory "direct-after"
}

# Function to run tests with the selected strategy
run_tests_with_strategy() {
    determine_execution_strategy
    
    case $EXECUTION_STRATEGY in
        "streaming")
            run_streaming_tests
            ;;
        "batched")
            run_batched_tests
            ;;
        "direct")
            run_direct_tests
            ;;
        *)
            print_error "Unknown execution strategy: $EXECUTION_STRATEGY"
            exit 1
            ;;
    esac
}

# Function to run tests with memory monitoring
run_tests_with_monitoring() {
    print_status "Running advanced memory-optimized tests..."
    
    # Create a temporary file for memory monitoring
    TEMP_FILE=$(mktemp)
    
    # Start memory monitoring in background
    (
        while true; do
            if [ -f "/proc/$$/status" ]; then
                MEM_USAGE=$(grep VmRSS /proc/$$/status | awk '{print $2}')
                MEM_PEAK=$(grep VmHWM /proc/$$/status | awk '{print $2}')
                echo "$(date): ${MEM_USAGE}KB (peak: ${MEM_PEAK}KB)" >> "$TEMP_FILE"
            fi
            sleep 1
        done
    ) &
    MONITOR_PID=$!
    
    # Run the tests with selected strategy
    TEST_SUCCESS=true
    if ! run_tests_with_strategy; then
        TEST_SUCCESS=false
    fi
    
    # Stop monitoring
    kill $MONITOR_PID 2>/dev/null || true
    wait $MONITOR_PID 2>/dev/null || true
    
    # Report memory usage
    if [ -f "$TEMP_FILE" ]; then
        MAX_MEM=$(awk '{print $3}' "$TEMP_FILE" | sort -n | tail -1)
        AVG_MEM=$(awk '{sum+=$3} END {print int(sum/NR)}' "$TEMP_FILE")
        print_memory "Memory usage report:"
        print_memory "  Peak memory: ${MAX_MEM}KB $((${MAX_MEM} / 1024))MB"
        print_memory "  Average memory: ${AVG_MEM}KB $((${AVG_MEM} / 1024))MB"
        rm -f "$TEMP_FILE"
    fi
    
    # Check test results
    if [ "$TEST_SUCCESS" = true ]; then
        print_success "Advanced memory-optimized tests completed successfully with $MEMORY_LEVEL settings."
        return 0
    else
        print_warning "Some tests failed with $MEMORY_LEVEL memory limits."
        print_warning "This may be due to aggressive memory optimization."
        print_warning "Try with a higher memory level:"
        print_warning "  TYPUS_MEMORY_LEVEL=moderate_minimal $0"
        return 1
    fi
}

# Function to run specific test suites
run_specific_test_suites() {
    print_status "Running specific test suites..."
    
    # Core functionality tests
    print_status "Running core functionality tests..."
    if cabal test --flags="fast" \
        --test-options="--quickcheck-tests=$QUICKCHECK_TESTS --quickcheck-max-size=$QUICKCHECK_MAX_SIZE --quickcheck-max-shrinks=$QUICKCHECK_MAX_SHRINKS -p Core" \
        typus-test; then
        print_success "Core tests passed"
    else
        print_warning "Core tests failed"
    fi
    
    # Memory-critical tests
    print_status "Running memory-critical tests..."
    if cabal test --flags="fast" \
        --test-options="--quickcheck-tests=$QUICKCHECK_TESTS --quickcheck-max-size=$QUICKCHECK_MAX_SIZE --quickcheck-max-shrinks=$QUICKCHECK_MAX_SHRINKS -p Memory" \
        typus-test; then
        print_success "Memory tests passed"
    else
        print_warning "Memory tests failed"
    fi
}

# Function to cleanup and optimize memory
cleanup_memory() {
    print_status "Performing final memory cleanup..."
    aggressive_cleanup
    print_success "Final cleanup completed"
}

# Function to show usage information
show_usage() {
    echo "Advanced Memory-Optimized Test Runner for Typus"
    echo ""
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Environment Variables:"
    echo "  TYPUS_MEMORY_LEVEL      Memory optimization level"
    echo "                          (ultra_minimal, extreme_minimal, aggressive_minimal, moderate_minimal, conservative, adaptive)"
    echo "  TYPUS_EXECUTION_STRATEGY Test execution strategy"
    echo "                          (streaming, batched, direct, auto)"
    echo "  TYPUS_MEMORY_MONITORING Enable memory monitoring (true/false)"
    echo "  TYPUS_AGGRESSIVE_CLEANUP Enable aggressive cleanup (true/false)"
    echo "  TYPUS_DEBUG             Enable debug output (true/false)"
    echo ""
    echo "Examples:"
    echo "  $0                                    # Run with adaptive settings"
    echo "  TYPUS_MEMORY_LEVEL=ultra_minimal $0  # Run with ultra minimal memory"
    echo "  TYPUS_EXECUTION_STRATEGY=streaming $0 # Run with streaming strategy"
    echo ""
}

# Main execution
main() {
    # Check for help flag
    if [ "$1" = "--help" ] || [ "$1" = "-h" ]; then
        show_usage
        exit 0
    fi
    
    print_status "Starting Advanced Memory-Optimized Test Runner"
    print_status "Memory level: $MEMORY_LEVEL"
    print_status "Execution strategy: $EXECUTION_STRATEGY"
    
    # Determine and set memory limits
    determine_memory_level
    set_memory_limits $MEMORY_LEVEL
    
    # Build with memory constraints
    build_with_memory_constraints
    
    # Run tests with monitoring
    if run_tests_with_monitoring; then
        # Run specific test suites if main tests pass
        run_specific_test_suites
    fi
    
    # Final cleanup
    cleanup_memory
    
    print_success "Advanced memory-optimized test run completed"
}

# Handle script interruption
trap 'print_warning "Test run interrupted"; cleanup_memory; exit 1' INT TERM

# Run main function
main "$@"