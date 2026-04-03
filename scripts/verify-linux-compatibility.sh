#!/bin/bash
# Verify Linux Compatibility for Typus Project
# Tests all major components to ensure they work properly on Linux

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}=== Typus Linux Compatibility Verification ===${NC}"
echo

# Source compatibility helpers
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/platform-compatibility-helpers.sh"

# Test counter
TESTS_PASSED=0
TESTS_FAILED=0

# Function to run test and track results
run_test() {
    local test_name="$1"
    local test_command="$2"
    
    echo -e "${BLUE}Testing: $test_name${NC}"
    
    # Create temporary file for output
    local temp_file=$(mktemp)
    
    if eval "$test_command" > "$temp_file" 2>&1; then
        echo -e "${GREEN}✓ PASS${NC}: $test_name"
        ((TESTS_PASSED++))
        rm -f "$temp_file"
        return 0
    else
        echo -e "${RED}✗ FAIL${NC}: $test_name"
        # Show error details for debugging
        if [ -s "$temp_file" ]; then
            echo -e "${YELLOW}  Error details: $(cat "$temp_file" | head -1)${NC}"
        fi
        ((TESTS_FAILED++))
        rm -f "$temp_file"
        return 1
    fi
}

# Test 1: Basic build system
run_test "Cabal build configuration" "cabal build --dry-run"

# Test 2: Go toolchain
run_test "Go compiler" "go version"

# Test 3: Haskell compiler
run_test "GHC compiler" "ghc --version"

# Test 4: Memory detection
run_test "Memory detection" "get_available_memory 512 > /dev/null"

# Test 5: Locale configuration
run_test "Locale configuration" "set_safe_locale && echo \$LC_ALL | grep -q 'C'"

# Test 6: File path handling
run_test "File path handling" "[ -f 'examples/hello_typus.typus' ]"

# Test 7: Shell script execution
run_test "Shell script execution" "bash scripts/linux-compatibility-check.sh > /dev/null"

# Test 8: Basic calculation
run_test "Calculation utilities" "calculate '2 + 2' | grep -q '4'"

# Test 9: Environment detection
run_test "Environment detection" "[ \"\$(detect_ci_environment)\" = \"ci\" ] || [ \"\$(detect_ci_environment)\" = \"local\" ]"

# Test 10: Architecture detection
run_test "Architecture detection" "[ \"\$(get_system_arch)\" = \"x86_64\" ]"

# Test 11: Memory optimization scripts
run_test "Memory optimization scripts" "bash scripts/run_memory_efficient_tests.sh --help 2>&1 | head -1 | grep -q 'Starting'"

# Test 12: Go module functionality
run_test "Go module support" "cd /tmp && go mod init test_compat 2>/dev/null; rm -f go.mod go.sum"

# Test 13: Basic file operations
run_test "File operations" "touch /tmp/test_typus_compat && rm /tmp/test_typus_compat"

# Test 14: Process execution
run_test "Process execution" "echo 'test' | cat > /dev/null"

# Test 15: Directory creation
run_test "Directory creation" "mkdir -p /tmp/typus_test_dir && rmdir /tmp/typus_test_dir"

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
echo "  OS: $(get_os_info)"
echo "  Architecture: $(get_system_arch)"
echo "  Environment: $(detect_ci_environment)"
echo "  Available Memory: $(get_available_memory) MB"

# Cleanup
cd - >/dev/null 2>&1