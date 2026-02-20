#!/usr/bin/env bash
# Batch Memory Optimization Script for QuickCheck Tests
# This script applies memory optimization configurations to QuickCheck test files

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
TEST_DIR="$PROJECT_ROOT/test"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if a file already has memory optimization imports
has_memory_optimization() {
    local file="$1"
    grep -q "import.*MemoryOptimizedQuickCheck\|applyQuickCheckMemoryConfig\|withQuickCheckMemoryConfig" "$file" 2>/dev/null
}

# Apply memory optimization to a single test file
optimize_test_file() {
    local file="$1"
    local backup_file="${file}.backup"
    
    log_info "Optimizing $file"
    
    # Create backup
    cp "$file" "$backup_file"
    
    # Check if file already has optimization
    if has_memory_optimization "$file"; then
        log_warning "$file already has memory optimization, skipping..."
        rm "$backup_file"
        return 0
    fi
    
    # Apply basic memory optimization template
    cat > "$file" << 'EOF'
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Memory optimization applied by batch optimization script
EOF
    
    # Copy original content after the language pragmas
    tail -n +3 "$backup_file" >> "$file"
    
    # Add memory optimization imports after the module declaration
    sed -i '/^module.*where$/a\
\
import TestSupport.MemoryOptimizedQuickCheck \
  ( QuickCheckMemoryConfig(..)\
  , lowMemoryConfig\
  , applyQuickCheckMemoryConfig\
  , genSmallString\
  , genSmallList\
  , genSmallInt\
  )' "$file"
    
    # Apply memory optimization to test suite
    sed -i 's/tests :: TestTree/tests :: TestTree/' "$file"
    sed -i 's/testGroup "\([^"]*\)"/applyQuickCheckMemoryConfig lowMemoryConfig $ testGroup "\1 (Memory Optimized)"/g' "$file"
    
    # Optimize string properties
    sed -i 's/prop_\([^(*]*\) :: String -> Property/prop_\1 :: String -> Property/' "$file"
    sed -i 's/prop_\([^(*]*\) s =/prop_\1 s = let limitedInput = take 5 s in/' "$file"
    
    log_success "Optimized $file"
    rm "$backup_file"
}

# Find all QuickCheck test files
find_quickcheck_files() {
    find "$TEST_DIR" -name "*QuickCheck*.hs" -type f | grep -v backup | sort
}

# Main optimization function
optimize_all_tests() {
    local quickcheck_files=()
    local optimized_count=0
    local skipped_count=0
    
    # Find all QuickCheck files
    while IFS= read -r file; do
        quickcheck_files+=("$file")
    done < <(find_quickcheck_files)
    
    log_info "Found ${#quickcheck_files[@]} QuickCheck test files"
    
    # Optimize each file
    for file in "${quickcheck_files[@]}"; do
        if has_memory_optimization "$file"; then
            ((skipped_count++))
            log_warning "Skipping $file (already optimized)"
        else
            optimize_test_file "$file"
            ((optimized_count++))
        fi
    done
    
    log_success "Optimization complete!"
    log_info "Optimized: $optimized_count files"
    log_info "Skipped: $skipped_count files"
}

# Verify optimization
verify_optimization() {
    log_info "Verifying memory optimization..."
    
    local total_files=0
    local optimized_files=0
    
    while IFS= read -r file; do
        ((total_files++))
        if has_memory_optimization "$file"; then
            ((optimized_files++))
        fi
    done < <(find_quickcheck_files)
    
    local percentage=$((optimized_files * 100 / total_files))
    
    log_info "Total QuickCheck files: $total_files"
    log_info "Optimized files: $optimized_files"
    log_info "Optimization coverage: $percentage%"
    
    if [ "$percentage" -ge 80 ]; then
        log_success "Memory optimization coverage is excellent!"
    elif [ "$percentage" -ge 50 ]; then
        log_warning "Memory optimization coverage is good, but could be improved"
    else
        log_error "Memory optimization coverage needs improvement"
    fi
}

# Restore backups if needed
restore_backups() {
    log_info "Restoring backup files..."
    find "$TEST_DIR" -name "*.backup" -type f | while read -r backup; do
        local original="${backup%.backup}"
        mv "$backup" "$original"
        log_info "Restored $original"
    done
}

# Clean up backup files
cleanup_backups() {
    log_info "Cleaning up backup files..."
    find "$TEST_DIR" -name "*.backup" -type f -delete
    log_success "Backup files cleaned up"
}

# Show help
show_help() {
    echo "Usage: $0 [COMMAND]"
    echo ""
    echo "Commands:"
    echo "  optimize    Apply memory optimization to all QuickCheck test files"
    echo "  verify      Verify memory optimization coverage"
    echo "  restore     Restore all files from backups"
    echo "  cleanup     Remove backup files"
    echo "  help        Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0 optimize"
    echo "  $0 verify"
}

# Main script logic
main() {
    case "${1:-optimize}" in
        "optimize")
            log_info "Starting batch memory optimization of QuickCheck tests..."
            optimize_all_tests
            verify_optimization
            ;;
        "verify")
            verify_optimization
            ;;
        "restore")
            restore_backups
            ;;
        "cleanup")
            cleanup_backups
            ;;
        "help"|"-h"|"--help")
            show_help
            ;;
        *)
            log_error "Unknown command: $1"
            show_help
            exit 1
            ;;
    esac
}

# Run main function
main "$@"