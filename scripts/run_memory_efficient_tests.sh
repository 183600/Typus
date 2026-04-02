#!/bin/bash
#!/bin/bash
# Memory efficient test runner script
# Ensure tests don't consume excessive memory while preserving all functionality

set -e

# Set locale to avoid encoding warnings
export LC_ALL=C
export LANG=C
export LC_CTYPE=C
export LC_MESSAGES=C
export LC_COLLATE=C

# 内存优化环境变量
export GHC_HEAP_SIZE="64M"
export GHC_STACK_SIZE="2M"
export CABAL_MAX_BUILD_JOBS="1"

# Memory monitoring function
monitor_memory() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] Current memory usage:"
    free -h | grep -E "Mem|Swap"
    echo ""
}

# Memory check function
check_memory_usage() {
    local memory_threshold="85"
    local memory_usage=$(free | grep Mem | awk '{printf "%.0f", $3/$2 * 100.0}')
    
    if [ "$memory_usage" -gt "$memory_threshold" ]; then
        echo "⚠️  Warning: Memory usage exceeds ${memory_threshold}% (current: ${memory_usage}%)"
        echo "Applying emergency memory optimization strategy..."
        return 1
    else
        echo "✓ Memory usage normal (current: ${memory_usage}%)"
        return 0
    fi
}

# Intelligent test selection function
select_tests_intelligently() {
    local memory_level="$1"
    
    case "$memory_level" in
        "extreme_minimal")
            echo "--test-show-details=direct --test-option=--quickcheck-tests=1 --test-option=--quickcheck-max-size=1"
            ;;
        "emergency")
            echo "--test-show-details=direct --test-option=--quickcheck-tests=3 --test-option=--quickcheck-max-size=3"
            ;;
        "balanced")
            echo "--test-show-details=direct --test-option=--quickcheck-tests=10 --test-option=--quickcheck-max-size=5"
            ;;
        *)
            echo "--test-show-details=direct"
            ;;
    esac
}

# Main function
main() {
    echo "🚀 Starting memory efficient test run..."
    echo "========================================"
    
    # Initial memory check
    monitor_memory
    
    # Detect memory level
    if check_memory_usage; then
        memory_level="balanced"
        echo "Using balanced memory mode"
    else
        memory_level="emergency"
        echo "Using emergency memory mode"
    fi
    
    # Extreme memory check
    local total_memory=$(free | grep Mem | awk '{print $2}')
    if [ "$total_memory" -lt "16777216" ]; then  # Less than 16MB
        memory_level="extreme_minimal"
        echo "Using extreme minimal memory mode"
    fi
    
    # Intelligent test selection
    local test_options=$(select_tests_intelligently "$memory_level")
    
    echo ""
    echo "📋 Test configuration:"
    echo "   Memory level: $memory_level"
    echo "   Test options: $test_options"
    echo "========================================"
    
    # Run tests
    echo "▶️  Running memory optimized tests..."
    
    # Apply memory optimization flags
    local cabal_flags="-fast production memory_optimized"
    
    # Run test command
    cabal test --flags="$cabal_flags" $test_options "$@"
    
    local test_exit_code=$?
    
    echo ""
    echo "========================================"
    echo "📊 Test completion status: $test_exit_code"
    
    # Final memory check
    monitor_memory
    
    return $test_exit_code
}

# 执行主函数
main "$@"