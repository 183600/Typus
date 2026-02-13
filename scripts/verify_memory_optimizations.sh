#!/bin/bash

# Memory Optimization Verification Script
# This script tests and verifies the memory optimizations work correctly

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Memory levels to test
MEMORY_LEVELS=(8 16 32 64 128)

echo -e "${BLUE}=== Memory Optimization Verification Script ===${NC}"
echo ""

# Function to run tests with specific memory limit
run_memory_test() {
    local memory_limit=$1
    local test_name=$2
    
    echo -e "${YELLOW}Testing with ${memory_limit}MB memory limit...${NC}"
    
    # Set environment variables
    export TYPUS_MEMORY_LIMIT=$memory_limit
    export GHCRTS="-M${memory_limit}m -A4m -n1m -H16m"
    
    # Run the test and capture output
    if timeout 120 cabal test --flags="fast" --test-options="--quickcheck-tests=1 --quickcheck-max-size=1 --quickcheck-shrinks=0" 2>/dev/null; then
        echo -e "${GREEN}✓ ${test_name} (${memory_limit}MB) - PASSED${NC}"
        return 0
    else
        echo -e "${RED}✗ ${test_name} (${memory_limit}MB) - FAILED${NC}"
        return 1
    fi
}

# Function to test memory usage
test_memory_usage() {
    local memory_limit=$1
    
    echo -e "${YELLOW}Checking memory usage for ${memory_limit}MB limit...${NC}"
    
    # Set environment variables
    export TYPUS_MEMORY_LIMIT=$memory_limit
    export GHCRTS="-M${memory_limit}m -A4m -n1m -H16m"
    
    # Run with memory profiling
    timeout 60 cabal test --flags="fast" --test-options="+RTS -s -RTS --quickcheck-tests=1 --quickcheck-max-size=1" 2>/dev/null | \
        grep -E "(total memory in use|maximum residency|bytes copied in GC)" || echo "Memory stats not available"
}

# Function to verify test selection
verify_test_selection() {
    local memory_limit=$1
    local expected_max_tests=$2
    
    echo -e "${YELLOW}Verifying test selection for ${memory_limit}MB limit...${NC}"
    
    export TYPUS_MEMORY_LIMIT=$memory_limit
    
    # Run tests and count them
    local test_count=$(timeout 60 cabal test --flags="fast" --test-options="--quickcheck-tests=1 --quickcheck-max-size=1 --quickcheck-shrinks=0" 2>&1 | \
        grep -E "(Test suite|running)" | wc -l)
    
    if [ "$test_count" -le "$expected_max_tests" ]; then
        echo -e "${GREEN}✓ Test selection correct: $test_count tests (max $expected_max_tests)${NC}"
        return 0
    else
        echo -e "${RED}✗ Too many tests: $test_count (max $expected_max_tests)${NC}"
        return 1
    fi
}

# Function to verify QuickCheck parameters
verify_quickcheck_params() {
    local memory_limit=$1
    
    echo -e "${YELLOW}Verifying QuickCheck parameters for ${memory_limit}MB limit...${NC}"
    
    export TYPUS_MEMORY_LIMIT=$memory_limit
    
    # Run tests and check QuickCheck output
    timeout 60 cabal test --flags="fast" --test-options="--quickcheck-tests=5 --quickcheck-max-size=3 --quickcheck-shrinks=1" 2>&1 | \
        grep -E "(Passed|Failed)" | head -5
}

# Main verification function
main() {
    echo -e "${BLUE}Starting memory optimization verification...${NC}"
    echo ""
    
    # Build the project first
    echo -e "${YELLOW}Building project...${NC}"
    cabal build --flags="fast" || {
        echo -e "${RED}Build failed!${NC}"
        exit 1
    }
    
    echo -e "${GREEN}✓ Build successful${NC}"
    echo ""
    
    # Test each memory level
    local total_tests=0
    local passed_tests=0
    
    for memory in "${MEMORY_LEVELS[@]}"; do
        echo -e "${BLUE}=== Testing ${memory}MB Memory Level ===${NC}"
        
        # Determine expected max tests based on memory level
        case $memory in
            8) expected_max=1 ;;
            16) expected_max=2 ;;
            32) expected_max=3 ;;
            64) expected_max=5 ;;
            128) expected_max=8 ;;
            *) expected_max=10 ;;
        esac
        
        # Run tests
        if run_memory_test $memory "Memory Test"; then
            ((passed_tests++))
        fi
        ((total_tests++))
        
        # Verify test selection
        verify_test_selection $memory $expected_max_tests
        
        # Check memory usage
        test_memory_usage $memory
        
        # Verify QuickCheck parameters
        verify_quickcheck_params $memory
        
        echo ""
    done
    
    # Summary
    echo -e "${BLUE}=== Verification Summary ===${NC}"
    echo -e "Total tests: $total_tests"
    echo -e "${GREEN}Passed tests: $passed_tests${NC}"
    echo -e "${RED}Failed tests: $((total_tests - passed_tests))${NC}"
    
    if [ $passed_tests -eq $total_tests ]; then
        echo -e "${GREEN}✓ All memory optimizations verified successfully!${NC}"
        exit 0
    else
        echo -e "${RED}✗ Some memory optimizations failed!${NC}"
        exit 1
    fi
}

# Function to test extreme memory optimization
test_extreme_optimization() {
    echo -e "${BLUE}=== Testing Extreme Memory Optimization ===${NC}"
    
    # Test ultra-low memory (8MB)
    export ULTRA_MEMORY_OPTIMIZED=1
    export TYPUS_MEMORY_LIMIT=8
    export GHCRTS="-M8m -A2m -n512k -H8m"
    
    echo -e "${YELLOW}Running ultra-optimized tests (8MB)...${NC}"
    
    if timeout 120 cabal test --flags="fast" --test-options="--quickcheck-tests=1 --quickcheck-max-size=1 --quickcheck-shrinks=0"; then
        echo -e "${GREEN}✓ Ultra optimization test passed${NC}"
    else
        echo -e "${RED}✗ Ultra optimization test failed${NC}"
    fi
    
    unset ULTRA_MEMORY_OPTIMIZED
}

# Function to test emergency memory optimization
test_emergency_optimization() {
    echo -e "${BLUE}=== Testing Emergency Memory Optimization ===${NC}"
    
    # Test emergency memory (16MB)
    export EMERGENCY_MEMORY=1
    export TYPUS_MEMORY_LIMIT=16
    export GHCRTS="-M16m -A4m -n1m -H16m"
    
    echo -e "${YELLOW}Running emergency tests (16MB)...${NC}"
    
    if timeout 120 cabal test --flags="fast" --test-options="--quickcheck-tests=2 --quickcheck-max-size=1 --quickcheck-shrinks=1"; then
        echo -e "${GREEN}✓ Emergency optimization test passed${NC}"
    else
        echo -e "${RED}✗ Emergency optimization test failed${NC}"
    fi
    
    unset EMERGENCY_MEMORY
}

# Run all tests
if [ "$1" = "--extreme" ]; then
    test_extreme_optimization
elif [ "$1" = "--emergency" ]; then
    test_emergency_optimization
else
    main
    test_extreme_optimization
    test_emergency_optimization
fi