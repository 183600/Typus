#!/bin/bash

# Enhanced Precise Type Tests Runner for Typus
# This script runs the comprehensive precise type test suite

set -e  # Exit on any error

echo "=================================="
echo "Enhanced Precise Type Tests Runner"
echo "=================================="
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    local color=$1
    local message=$2
    echo -e "${color}${message}${NC}"
}

# Check if we're in the right directory
if [ ! -f "typus.cabal" ]; then
    print_status $RED "ERROR: Please run this script from the project root directory"
    exit 1
fi

# Function to run tests with different methods
run_tests() {
    local method=$1
    
    case $method in
        "stack")
            print_status $BLUE "Running tests with Stack..."
            if command -v stack &> /dev/null; then
                stack test --test-arguments="--pattern 'Precise Type Tests'" || true
                stack exec run-enhanced-precise-type-tests || true
            else
                print_status $YELLOW "Stack not found, trying cabal..."
                run_tests "cabal"
            fi
            ;;
        "cabal")
            print_status $BLUE "Running tests with Cabal..."
            if command -v cabal &> /dev/null; then
                cabal test --test-options="--pattern 'Precise Type Tests'" || true
                cabal run run-enhanced-precise-type-tests || true
            else
                print_status $YELLOW "Cabal not found, trying direct execution..."
                run_tests "direct"
            fi
            ;;
        "direct")
            print_status $BLUE "Running tests directly..."
            # Try to run the test executable directly
            if [ -f "dist-newstyle/build/*/ghc-*/typus-*/x/run-enhanced-precise-type-tests/build/run-enhanced-precise-type-tests/run-enhanced-precise-type-tests" ]; then
                ./dist-newstyle/build/*/ghc-*/typus-*/x/run-enhanced-precise-type-tests/build/run-enhanced-precise-type-tests/run-enhanced-precise-type-tests
            elif [ -f ".stack-work/install/*/ghc-*/typus-*/bin/run-enhanced-precise-type-tests" ]; then
                .stack-work/install/*/ghc-*/typus-*/bin/run-enhanced-precise-type-tests
            else
                print_status $YELLOW "Test executable not found, building first..."
                build_project
                run_tests "direct"
            fi
            ;;
        *)
            print_status $RED "Unknown method: $method"
            exit 1
            ;;
    esac
}

# Function to build the project
build_project() {
    print_status $BLUE "Building project..."
    
    if command -v stack &> /dev/null; then
        stack build --test --no-run-tests
    elif command -v cabal &> /dev/null; then
        cabal build
    else
        print_status $RED "ERROR: Neither Stack nor Cabal found. Please install Haskell build tools."
        exit 1
    fi
}

# Function to run specific test categories
run_specific_category() {
    local category=$1
    print_status $BLUE "Running specific test category: $category"
    
    case $category in
        "dependent"|"refinement"|"linear"|"ownership"|"computation"|"optimization"|"polymorphic"|"error"|"performance"|"integration")
            if command -v stack &> /dev/null; then
                stack exec run-enhanced-precise-type-tests -- --category $category
            elif command -v cabal &> /dev/null; then
                cabal run run-enhanced-precise-type-tests -- --category $category
            else
                print_status $RED "Cannot run specific category without build tools"
                exit 1
            fi
            ;;
        *)
            print_status $RED "Unknown category: $category"
            print_status $YELLOW "Available categories: dependent, refinement, linear, ownership, computation, optimization, polymorphic, error, performance, integration"
            exit 1
            ;;
    esac
}

# Function to show help
show_help() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --help, -h              Show this help message"
    echo "  --build, -b             Build the project first"
    echo "  --stack                 Use Stack to run tests"
    echo "  --cabal                 Use Cabal to run tests"
    echo "  --category CATEGORY     Run specific test category"
    echo "  --list-categories       List available test categories"
    echo "  --fast                  Run only fast tests"
    echo "  --full                  Run full test suite"
    echo "  --production            Run production-grade tests"
    echo "  --coverage              Enable test coverage"
    echo ""
    echo "Examples:"
    echo "  $0                      Run all tests with auto-detection"
    echo "  $0 --build              Build and run all tests"
    echo "  $0 --stack              Run tests with Stack"
    echo "  $0 --category dependent Run only dependent type tests"
    echo "  $0 --fast               Run only fast tests"
}

# Function to list available categories
list_categories() {
    echo "Available test categories:"
    echo "  dependent    - Advanced dependent type tests"
    echo "  refinement   - Complex refinement type tests"
    echo "  linear       - Sophisticated linear type tests"
    echo "  ownership    - Advanced ownership type tests"
    echo "  computation  - Type-level computation tests"
    echo "  optimization - Constraint optimization tests"
    echo "  polymorphic  - Polymorphic constraint tests"
    echo "  error        - Error boundary tests"
    echo "  performance  - Performance type tests"
    echo "  integration  - Integration type tests"
}

# Main execution
main() {
    local build_first=false
    local test_method="auto"
    local category=""
    local fast_mode=false
    local full_mode=false
    local production_mode=false
    local coverage_mode=false
    
    # Parse command line arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            --help|-h)
                show_help
                exit 0
                ;;
            --build|-b)
                build_first=true
                shift
                ;;
            --stack)
                test_method="stack"
                shift
                ;;
            --cabal)
                test_method="cabal"
                shift
                ;;
            --category)
                category="$2"
                shift 2
                ;;
            --list-categories)
                list_categories
                exit 0
                ;;
            --fast)
                fast_mode=true
                shift
                ;;
            --full)
                full_mode=true
                shift
                ;;
            --production)
                production_mode=true
                shift
                ;;
            --coverage)
                coverage_mode=true
                shift
                ;;
            *)
                print_status $RED "Unknown option: $1"
                show_help
                exit 1
                ;;
        esac
    done
    
    # Build if requested
    if [ "$build_first" = true ]; then
        build_project
    fi
    
    # Set build flags based on options
    local build_flags=""
    if [ "$fast_mode" = true ]; then
        build_flags="$build_flags --flag typus:fast"
    fi
    if [ "$full_mode" = true ]; then
        build_flags="$build_flags --flag typus:full"
    fi
    if [ "$production_mode" = true ]; then
        build_flags="$build_flags --flag typus:production"
    fi
    if [ "$coverage_mode" = true ]; then
        build_flags="$build_flags --flag typus:coverage"
    fi
    
    # Run specific category or all tests
    if [ -n "$category" ]; then
        run_specific_category "$category"
    else
        # Determine test method
        if [ "$test_method" = "auto" ]; then
            if command -v stack &> /dev/null; then
                test_method="stack"
            elif command -v cabal &> /dev/null; then
                test_method="cabal"
            else
                test_method="direct"
            fi
        fi
        
        print_status $BLUE "Running enhanced precise type tests with $test_method..."
        run_tests "$test_method"
    fi
}

# Run main function with all arguments
main "$@"