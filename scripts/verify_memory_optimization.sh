#!/usr/bin/env bash
# Simple Memory Optimization Verification Script

set -e

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

# Count optimized files
count_optimized_files() {
    local total=$(find test -name "*QuickCheck*.hs" -type f | wc -l)
    local optimized=$(find test -name "*QuickCheck*.hs" -type f -exec grep -l "UniversalMemoryOptimization\|MemoryOptimizedQuickCheck" {} \; | wc -l)
    local percentage=$((optimized * 100 / total))
    
    log_info "QuickCheck files: $total"
    log_info "Optimized files: $optimized"
    log_info "Coverage: $percentage%"
    
    if [ "$percentage" -ge 10 ]; then
        log_success "Memory optimization coverage is improving!"
    else
        log_warning "Memory optimization coverage needs more work"
    fi
}

# Test a few optimized files
test_optimized_files() {
    log_info "Testing recently optimized files..."
    
    local files_to_test=(
        "test/Test/Unit/UtilsQuickCheckSpec.hs"
        "test/Test/Unit/TypeSystemQuickCheckSpec.hs"
        "test/Test/Unit/SimpleParserQuickCheckSpec.hs"
    )
    
    for file in "${files_to_test[@]}"; do
        if [ -f "$file" ]; then
            if grep -q "UniversalMemoryOptimization\|MemoryOptimizedQuickCheck" "$file"; then
                log_success "$file is optimized"
            else
                log_warning "$file is not optimized"
            fi
        else
            log_warning "$file does not exist"
        fi
    done
}

# Main function
main() {
    log_info "Memory Optimization Verification"
    log_info "================================="
    
    count_optimized_files
    test_optimized_files
    
    log_info "Verification complete!"
}

main "$@"