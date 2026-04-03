#!/bin/bash
# Enhanced Memory-Optimized Test Runner
# This script provides an easy way to run tests with enhanced memory optimization

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
    echo -e "${PURPLE}Enhanced Memory-Optimized Tests${NC}"
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

# Memory level configurations
declare -A MEMORY_CONFIGS=(
    ["micro"]="16MB - Ultra minimal memory usage"
    ["ultra_light"]="24MB - Ultra light memory usage"
    ["enhanced"]="32MB - Enhanced memory usage"
    ["standard"]="48MB - Standard memory usage"
)

# Help function
show_help() {
    echo "Enhanced Memory-Optimized Test Runner for Typus"
    echo ""
    echo "Usage: $0 [MEMORY_LEVEL] [OPTIONS]"
    echo ""
    echo "Memory Levels:"
    for level in "${!MEMORY_CONFIGS[@]}"; do
        printf "  %-12s - %s\n" "$level" "${MEMORY_CONFIGS[$level]}"
    done
    echo ""
    echo "Options:"
    echo "  --verbose, -v     Enable verbose output"
    echo "  --help, -h        Show this help message"
    echo "  --list-tests      List available test suites"
    echo "  --check-memory    Check available memory and suggest configuration"
    echo ""
    echo "Environment Variables:"
    echo "  TYPUS_MEMORY_LEVEL      Memory optimization level"
    echo "  TYPUS_MEMORY_LIMIT_MB   Explicit memory limit in MB"
    echo "  TYPUS_VERBOSE           Enable verbose output"
    echo ""
    echo "Examples:"
    echo "  $0                                    # Auto-configure based on environment"
    echo "  $0 micro                              # Run with micro memory optimization"
    echo "  $0 enhanced --verbose                 # Run with enhanced optimization and verbose output"
    echo "  TYPUS_MEMORY_LEVEL=ultra_light $0    # Run with ultra light optimization"
}

# Check available memory and suggest configuration
check_memory() {
    print_status "Checking system memory..."
    
    # Try different methods to get available memory
    if [ -f /proc/meminfo ]; then
        # Linux with /proc/meminfo (most reliable)
        local available=$(grep MemAvailable /proc/meminfo | awk '{printf "%.0f", $2/1024}')
        print_config "Available memory: ${available}MB"
        suggest_memory_config "$available"
    elif command -v free >/dev/null 2>&1; then
        # Linux with free command
        local available=$(free -m | awk 'NR==2{printf "%.0f", $7}')
        print_config "Available memory: ${available}MB"
        suggest_memory_config "$available"
    elif command -v vm_stat >/dev/null 2>&1; then
        # macOS with vm_stat
        local page_size=$(vm_stat | head -1 | sed 's/.*page size of \([0-9]*\).*/\1/')
        local free_pages=$(vm_stat | awk '/free/ {gsub(/\./, ""); print $3}')
        local available=$((free_pages * page_size / 1024 / 1024))
        print_config "Available memory: ${available}MB"
        suggest_memory_config "$available"
    else
        print_warning "Cannot determine available memory, using default configuration"
    fi
}

# Suggest memory configuration based on available memory
suggest_memory_config() {
    local available=$1
    local suggested=""
    
    if [ "$available" -le 32 ]; then
        suggested="micro"
    elif [ "$available" -le 64 ]; then
        suggested="ultra_light"
    elif [ "$available" -le 128 ]; then
        suggested="enhanced"
    else
        suggested="standard"
    fi
    
    print_config "Suggested memory level: $suggested (${MEMORY_CONFIGS[$suggested]})"
}

# List available test suites
list_tests() {
    print_status "Available test suites:"
    echo ""
    echo "Enhanced Memory-Optimized Tests:"
    echo "  - Enhanced Memory-Efficient Tests (core memory optimization tests)"
    echo "  - Optimized Test Suite (existing tests with memory optimization)"
    echo ""
    echo "Other Test Suites:"
    echo "  - typus-test (standard test suite)"
    echo "  - typus-test-optimized (memory-optimized test suite)"
    echo "  - typus-test-advanced (advanced memory-optimized test suite)"
}

# Run enhanced memory tests
run_enhanced_tests() {
    local memory_level="$1"
    local verbose="$2"
    
    print_status "Running enhanced memory-optimized tests..."
    print_config "Memory level: $memory_level (${MEMORY_CONFIGS[$memory_level]})"
    
    # Set environment variables
    export TYPUS_MEMORY_LEVEL="$memory_level"
    if [ "$verbose" = "true" ]; then
        export TYPUS_VERBOSE="true"
        print_config "Verbose mode: enabled"
    fi
    
    # Set RTS options for memory limits
    case "$memory_level" in
        "micro")
            export GHCRTS="-M16m -A512k -n64k -H2m"
            ;;
        "ultra_light")
            export GHCRTS="-M24m -A1m -n128k -H3m"
            ;;
        "enhanced")
            export GHCRTS="-M32m -A2m -n256k -H4m"
            ;;
        "standard")
            export GHCRTS="-M48m -A4m -n512k -H6m"
            ;;
    esac
    
    print_config "RTS options: $GHCRTS"
    
    # Build and run tests
    print_status "Building enhanced test suite..."
    if cabal build typus-test-enhanced; then
        print_success "Build successful"
    else
        print_error "Build failed"
        return 1
    fi
    
    print_status "Running tests..."
    echo ""
    
    # Run the tests
    local test_args=""
    if [ "$verbose" = "true" ]; then
        test_args="--verbose"
    fi
    
    if cabal run typus-test-enhanced -- $memory_level $test_args; then
        print_success "All tests completed successfully!"
    else
        print_error "Tests failed"
        return 1
    fi
    
    # Clean up environment
    unset GHCRTS
}

# Main execution logic
main() {
    local memory_level=""
    local verbose="false"
    local show_help="false"
    local list_tests="false"
    local check_memory="false"
    
    # Parse command line arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            --help|-h)
                show_help="true"
                shift
                ;;
            --verbose|-v)
                verbose="true"
                shift
                ;;
            --list-tests)
                list_tests="true"
                shift
                ;;
            --check-memory)
                check_memory="true"
                shift
                ;;
            micro|ultra_light|enhanced|standard)
                memory_level="$1"
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
        show_help
        exit 0
    fi
    
    # List tests if requested
    if [ "$list_tests" = "true" ]; then
        list_tests
        exit 0
    fi
    
    # Check memory if requested
    if [ "$check_memory" = "true" ]; then
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
                if [ "$available" -le 32 ]; then
                    memory_level="micro"
                elif [ "$available" -le 64 ]; then
                    memory_level="ultra_light"
                elif [ "$available" -le 128 ]; then
                    memory_level="enhanced"
                else
                    memory_level="standard"
                fi
                print_config "Auto-selected memory level: $memory_level"
            else
                memory_level="enhanced"
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
    
    # Check verbose flag from environment
    if [ "$TYPUS_VERBOSE" = "true" ]; then
        verbose="true"
    fi
    
    # Run tests
    if run_enhanced_tests "$memory_level" "$verbose"; then
        echo ""
        print_success "Enhanced memory-optimized tests completed successfully!"
        print_status "Memory usage was optimized for: ${MEMORY_CONFIGS[$memory_level]}"
    else
        echo ""
        print_error "Test execution failed"
        exit 1
    fi
}

# Run main function
main "$@"