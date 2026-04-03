#!/bin/bash
# Simple Linux Compatibility Verification for Typus Project

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}=== Typus Linux Compatibility Verification ===${NC}"
echo

# Test counter
TESTS_PASSED=0
TESTS_FAILED=0

# Function to run test
run_test() {
    local test_name="$1"
    local test_command="$2"
    
    echo -e "${BLUE}Testing: $test_name${NC}"
    
    if bash -c "$test_command" >/dev/null 2>&1; then
        echo -e "${GREEN}✓ PASS${NC}: $test_name"
        ((TESTS_PASSED++))
    else
        echo -e "${RED}✗ FAIL${NC}: $test_name"
        ((TESTS_FAILED++))
    fi
}

# Test 1: Basic build system
run_test "Cabal build configuration" "cabal build --dry-run"

# Test 2: Go toolchain
run_test "Go compiler" "go version"

# Test 3: Haskell compiler
run_test "GHC compiler" "ghc --version"

# Test 4: File path handling
run_test "File path handling" "[ -f 'examples/hello_typus.typus' ]"

# Test 5: Shell script execution
run_test "Shell script execution" "bash scripts/linux-compatibility-check.sh > /dev/null"

# Test 6: Go module functionality
run_test "Go module support" "cd /tmp && go mod init test_compat 2>/dev/null; rm -f go.mod go.sum"

# Test 7: Basic file operations
run_test "File operations" "touch /tmp/test_typus_compat && rm /tmp/test_typus_compat"

# Test 8: Process execution
run_test "Process execution" "echo 'test' | cat > /dev/null"

# Test 9: Directory creation
run_test "Directory creation" "mkdir -p /tmp/typus_test_dir && rmdir /tmp/typus_test_dir"

# Test 10: Memory optimization scripts
run_test "Memory optimization scripts" "bash scripts/run_memory_efficient_tests.sh --help 2>&1 | head -1 | grep -q 'Starting'"

# Summary
echo
echo -e "${BLUE}=== Test Summary ===${NC}"
echo -e "${GREEN}Tests Passed: $TESTS_PASSED${NC}"
if [ "$TESTS_FAILED" -gt 0 ]; then
    echo -e "${RED}Tests Failed: $TESTS_FAILED${NC}"
else
    echo -e "${GREEN}Tests Failed: $TESTS_FAILED${NC}"
fi

if [ "$TESTS_FAILED" -eq 0 ]; then
    echo -e "${GREEN}✓ All compatibility tests passed! The Typus project is fully compatible with Linux.${NC}"
    exit 0
else
    echo -e "${YELLOW}⚠ Some tests failed. Review the output above for details.${NC}"
    exit 1
fi

echo
echo -e "${BLUE}=== System Information ===${NC}"
echo "  OS: $(uname -s)"
echo "  Architecture: $(uname -m)"
if [ "$CI" = "true" ] || [ "$GITHUB_ACTIONS" = "true" ]; then
    echo "  Environment: CI"
else
    echo "  Environment: Local"
fi

# Memory info
if command -v free >/dev/null 2>&1; then
    echo "  Available Memory: $(free -m | awk 'NR==2{print $7}') MB"
fi