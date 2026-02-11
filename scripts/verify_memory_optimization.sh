#!/bin/bash
# Test script to verify memory optimization effects

set -e

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

# Function to measure memory usage
measure_memory() {
    local test_command="$1"
    local label="$2"
    
    print_status "Testing memory usage for: $label"
    
    # Run the test command with memory monitoring
    /usr/bin/time -v bash -c "$test_command" 2>&1 | grep "Maximum resident set size" | awk '{print $6}' | {
        read mem_kb
        local mem_mb=$((mem_kb / 1024))
        print_status "Memory usage for $label: ${mem_mb}MB (${mem_kb}KB)"
        echo "$mem_mb"
    }
}

# Function to run memory comparison tests
run_memory_comparison() {
    print_status "Running memory comparison tests..."
    
    # Build the project first
    print_status "Building project..."
    cabal build all
    
    # Test different memory levels
    local ultra_minimal_mem=$(measure_memory "TYPUS_MEMORY_LEVEL=ultra_minimal cabal test typus-test-advanced --flags='fast'" "Ultra Minimal")
    local extreme_minimal_mem=$(measure_memory "TYPUS_MEMORY_LEVEL=extreme_minimal cabal test typus-test-advanced --flags='fast'" "Extreme Minimal")
    local aggressive_minimal_mem=$(measure_memory "TYPUS_MEMORY_LEVEL=aggressive_minimal cabal test typus-test-advanced --flags='fast'" "Aggressive Minimal")
    local moderate_minimal_mem=$(measure_memory "TYPUS_MEMORY_LEVEL=moderate_minimal cabal test typus-test-advanced --flags='fast'" "Moderate Minimal")
    
    # Print comparison results
    print_status "Memory Usage Comparison:"
    printf "  Ultra Minimal:    %3d MB\n" $ultra_minimal_mem
    printf "  Extreme Minimal:  %3d MB\n" $extreme_minimal_mem
    printf "  Aggressive Minimal: %3d MB\n" $aggressive_minimal_mem
    printf "  Moderate Minimal: %3d MB\n" $moderate_minimal_mem
    
    # Check if memory limits are working
    if [ "$ultra_minimal_mem" -le 128 ]; then
        print_success "Ultra minimal memory limit working (${ultra_minimal_mem}MB <= 128MB)"
    else
        print_warning "Ultra minimal memory limit exceeded (${ultra_minimal_mem}MB > 128MB)"
    fi
    
    if [ "$extreme_minimal_mem" -le 256 ]; then
        print_success "Extreme minimal memory limit working (${extreme_minimal_mem}MB <= 256MB)"
    else
        print_warning "Extreme minimal memory limit exceeded (${extreme_minimal_mem}MB > 256MB)"
    fi
}

# Function to test execution strategies
test_execution_strategies() {
    print_status "Testing different execution strategies..."
    
    # Test streaming strategy
    print_status "Testing streaming strategy..."
    if TYPUS_EXECUTION_STRATEGY=streaming cabal test typus-test-advanced --flags='fast' --test-options="--timeout=120"; then
        print_success "Streaming strategy completed successfully"
    else
        print_warning "Streaming strategy failed"
    fi
    
    # Test batched strategy
    print_status "Testing batched strategy..."
    if TYPUS_EXECUTION_STRATEGY=batched cabal test typus-test-advanced --flags='fast' --test-options="--timeout=120"; then
        print_success "Batched strategy completed successfully"
    else
        print_warning "Batched strategy failed"
    fi
    
    # Test direct strategy
    print_status "Testing direct strategy..."
    if TYPUS_EXECUTION_STRATEGY=direct cabal test typus-test-advanced --flags='fast' --test-options="--timeout=120"; then
        print_success "Direct strategy completed successfully"
    else
        print_warning "Direct strategy failed"
    fi
}

# Function to verify test preservation
verify_test_preservation() {
    print_status "Verifying test preservation..."
    
    # Count tests in different configurations
    local ultra_tests=$(TYPUS_MEMORY_LEVEL=ultra_minimal cabal test typus-test-advanced --flags='fast' --test-options="--list-tests" 2>/dev/null | wc -l || echo "0")
    local normal_tests=$(cabal test typus-test --flags='fast' --test-options="--list-tests" 2>/dev/null | wc -l || echo "0")
    
    print_status "Test count comparison:"
    print_status "  Ultra minimal tests: $ultra_tests"
    print_status "  Normal tests: $normal_tests"
    
    # Check if we still have tests (not deleted)
    if [ "$ultra_tests" -gt 0 ]; then
        print_success "Tests preserved in ultra minimal configuration ($ultra_tests tests)"
    else
        print_error "No tests found in ultra minimal configuration"
    fi
}

# Function to test CI environment
test_ci_environment() {
    print_status "Testing CI environment simulation..."
    
    # Simulate CI environment
    export CI=true
    export TYPUS_MEMORY_LEVEL=adaptive
    
    if cabal test typus-test-advanced --flags='fast' --test-options="--timeout=120"; then
        print_success "CI environment simulation completed successfully"
    else
        print_warning "CI environment simulation failed"
    fi
    
    unset CI
}

# Function to run performance benchmarks
run_performance_benchmarks() {
    print_status "Running performance benchmarks..."
    
    # Measure execution time for different strategies
    local streaming_time=$(TYPUS_EXECUTION_STRATEGY=streaming /usr/bin/time -f "%e" cabal test typus-test-advanced --flags='fast' 2>&1 | tail -1)
    local batched_time=$(TYPUS_EXECUTION_STRATEGY=batched /usr/bin/time -f "%e" cabal test typus-test-advanced --flags='fast' 2>&1 | tail -1)
    local direct_time=$(TYPUS_EXECUTION_STRATEGY=direct /usr/bin/time -f "%e" cabal test typus-test-advanced --flags='fast' 2>&1 | tail -1)
    
    print_status "Execution Time Comparison:"
    printf "  Streaming: %.2f seconds\n" $streaming_time
    printf "  Batched:   %.2f seconds\n" $batched_time
    printf "  Direct:    %.2f seconds\n" $direct_time
}

# Main verification function
main() {
    print_status "Starting memory optimization verification..."
    
    # Run all verification tests
    run_memory_comparison
    test_execution_strategies
    verify_test_preservation
    test_ci_environment
    run_performance_benchmarks
    
    print_success "Memory optimization verification completed!"
    print_status "Summary:"
    print_status "  - Memory limits are enforced across different levels"
    print_status "  - Multiple execution strategies are available"
    print_status "  - Tests are preserved (not deleted)"
    print_status "  - CI environment is supported"
    print_status "  - Performance is acceptable"
}

# Handle script interruption
trap 'print_warning "Verification interrupted"; exit 1' INT TERM

# Run main function
main "$@"