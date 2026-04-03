#!/bin/bash
# Linux Compatibility Check for Typus Project
# This script verifies that all required commands are available on Linux systems

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}=== Typus Linux Compatibility Check ===${NC}"
echo

# Function to check if command exists
check_command() {
    local cmd=$1
    local description=$2
    local required=$3
    
    if command -v "$cmd" >/dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} $description ($cmd)"
        return 0
    else
        if [ "$required" = "required" ]; then
            echo -e "${RED}✗${NC} $description ($cmd) - REQUIRED but not found"
            return 1
        else
            echo -e "${YELLOW}⚠${NC} $description ($cmd) - Optional but not found"
            return 0
        fi
    fi
}

# Function to check locale availability
check_locale() {
    local locale=$1
    if locale -a | grep -qi "^$locale$"; then
        echo -e "${GREEN}✓${NC} Locale $locale available"
        return 0
    else
        echo -e "${YELLOW}⚠${NC} Locale $locale not available"
        return 0  # Don't fail the script for missing locales
    fi
}

# Required build tools
check_command "cabal" "Haskell Cabal build tool" "required"
check_command "ghc" "Glasgow Haskell Compiler" "required"
check_command "go" "Go compiler" "required"
check_command "bash" "Bash shell" "required"

# Optional tools used in scripts
check_command "free" "Memory information tool" "optional"
check_command "/usr/bin/time" "Detailed time command" "optional"
check_command "bc" "Arbitrary precision calculator" "optional"
check_command "awk" "Text processing tool" "optional"
check_command "grep" "Pattern matching tool" "optional"
check_command "sed" "Stream editor" "optional"
check_command "env" "Environment utility" "optional"
check_command "uname" "System information" "optional"

# Check locale availability
echo
echo -e "${BLUE}Checking locale availability...${NC}"
check_locale "C.utf8"
check_locale "C.UTF-8"
check_locale "en_US.UTF-8"
check_locale "C"

# Check system information
echo
echo -e "${BLUE}System Information:${NC}"
if command -v uname >/dev/null 2>&1; then
    echo "  OS: $(uname -s)"
    echo "  Architecture: $(uname -m)"
    echo "  Kernel: $(uname -r)"
fi

if command -v lsb_release >/dev/null 2>&1; then
    echo "  Distribution: $(lsb_release -d | cut -f2)"
fi

# Check memory information
echo
echo -e "${BLUE}Memory Information:${NC}"
if command -v free >/dev/null 2>&1; then
    echo "  Available memory:"
    free -h | head -2
else
    echo "  Memory information not available (free command not found)"
fi

# Check disk space
echo
echo -e "${BLUE}Disk Space:${NC}"
df -h . | head -2

# Check Haskell toolchain version
echo
echo -e "${BLUE}Haskell Toolchain:${NC}"
if command -v ghc >/dev/null 2>&1; then
    echo "  GHC version: $(ghc --version | head -1)"
fi

if command -v cabal >/dev/null 2>&1; then
    echo "  Cabal version: $(cabal --version | head -1)"
fi

# Check Go toolchain version
echo
echo -e "${BLUE}Go Toolchain:${NC}"
if command -v go >/dev/null 2>&1; then
    echo "  Go version: $(go version)"
fi

# Test basic build functionality
echo
echo -e "${BLUE}Testing basic build functionality...${NC}"
if command -v cabal >/dev/null 2>&1; then
    if cabal build --dry-run >/dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} Cabal build configuration is valid"
    else
        echo -e "${RED}✗${NC} Cabal build configuration has issues"
    fi
else
    echo -e "${YELLOW}⚠${NC} Cabal not available, skipping build test"
fi

# Check for alternative commands
echo
echo -e "${BLUE}Checking for alternative commands...${NC}"

# Alternative to /usr/bin/time
if ! command -v /usr/bin/time >/dev/null 2>&1; then
    if command -v time >/dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} Found alternative: 'time' (instead of /usr/bin/time)"
    else
        echo -e "${YELLOW}⚠${NC} No time command available"
    fi
fi

# Alternative to bc for calculations
if ! command -v bc >/dev/null 2>&1; then
    if command -v awk >/dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} Found alternative: 'awk' (can handle basic calculations)"
    elif command -v python3 >/dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} Found alternative: 'python3' (can handle calculations)"
    else
        echo -e "${YELLOW}⚠${NC} No calculation tools available"
    fi
fi

# Alternative to free for memory detection
if ! command -v free >/dev/null 2>&1; then
    if [ -f "/proc/meminfo" ]; then
        echo -e "${GREEN}✓${NC} Found alternative: '/proc/meminfo' (for memory information)"
    else
        echo -e "${YELLOW}⚠${NC} No memory detection tools available"
    fi
fi

echo
echo -e "${BLUE}=== Compatibility Check Complete ===${NC}"
echo -e "${GREEN}All required tools are available. The Typus project should work on this Linux system.${NC}"

# Provide recommendations if needed
if ! command -v /usr/bin/time >/dev/null 2>&1 || ! command -v bc >/dev/null 2>&1 || ! command -v free >/dev/null 2>&1; then
    echo
echo -e "${YELLOW}Recommendations:${NC}"
    if ! command -v /usr/bin/time >/dev/null 2>&1; then
        echo "  Install 'time' package for detailed timing information"
    fi
    if ! command -v bc >/dev/null 2>&1; then
        echo "  Install 'bc' package for calculations in scripts"
    fi
    if ! command -v free >/dev/null 2>&1; then
        echo "  Install 'procps' package for memory information"
    fi
fi