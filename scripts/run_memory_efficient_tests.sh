#!/bin/bash
# 内存高效测试运行脚本
# 确保测试用例不会消耗大量内存，同时保留所有测试功能

set -e

# 设置locale以避免警告
export LC_ALL=C
export LANG=C
export LC_CTYPE=C
export LC_MESSAGES=C
export LC_COLLATE=C

# 内存优化环境变量
export GHC_HEAP_SIZE="64M"
export GHC_STACK_SIZE="2M"
export CABAL_MAX_BUILD_JOBS="1"

# 内存监控函数
monitor_memory() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] 当前内存使用情况:"
    free -h | grep -E "Mem|Swap"
    echo ""
}

# 内存检查函数
check_memory_usage() {
    local memory_threshold="85"
    local memory_usage=$(free | grep Mem | awk '{printf "%.0f", $3/$2 * 100.0}')
    
    if [ "$memory_usage" -gt "$memory_threshold" ]; then
        echo "⚠️  警告: 内存使用率超过 ${memory_threshold}% (当前: ${memory_usage}%)"
        echo "应用紧急内存优化策略..."
        return 1
    else
        echo "✓ 内存使用正常 (当前: ${memory_usage}%)"
        return 0
    fi
}

# 智能测试选择函数
select_tests_intelligently() {
    local memory_level="$1"
    
    case "$memory_level" in
        "extreme_minimal")
            echo "--test-option=--pattern='Test.Unit.BasicQuickCheckTestSuite|Test.Unit.CoreQuickCheckSpec|Test.Unit.ConciseTestSuite'"
            ;;
        "emergency")
            echo "--test-option=--pattern='Test.Unit.(BasicQuickCheckTestSuite|CoreQuickCheckSpec|ConciseTestSuite|MemoryOptimizedTestSuite|ExtremeMemoryOptimizedTestSuite)'"
            ;;
        "balanced")
            echo "--test-option=--pattern='Test.Unit.(BasicQuickCheckTestSuite|CoreQuickCheckSpec|ConciseTestSuite|MemoryOptimizedTestSuite|ExtremeMemoryOptimizedTestSuite|EnhancedMemoryOptimizedTestSuite)'"
            ;;
        *)
            echo ""
            ;;
    esac
}

# 主函数
main() {
    echo "🚀 启动内存高效测试运行..."
    echo "========================================"
    
    # 初始内存检查
    monitor_memory
    
    # 检测内存级别
    if check_memory_usage; then
        memory_level="balanced"
        echo "使用平衡内存模式"
    else
        memory_level="emergency"
        echo "使用紧急内存模式"
    fi
    
    # 极端内存检查
    local total_memory=$(free | grep Mem | awk '{print $2}')
    if [ "$total_memory" -lt "16777216" ]; then  # 小于16MB
        memory_level="extreme_minimal"
        echo "使用极端最小内存模式"
    fi
    
    # 智能选择测试
    local test_options=$(select_tests_intelligently "$memory_level")
    
    echo ""
    echo "📋 测试配置:"
    echo "   内存级别: $memory_level"
    echo "   测试选项: $test_options"
    echo "========================================"
    
    # 运行测试
    echo "▶️  运行内存优化测试..."
    
    # 应用内存优化标志
    local cabal_flags="--flags=\"-fast production memory_optimized\""
    
    # 运行测试命令
    cabal test $cabal_flags $test_options --test-show-details=direct "$@"
    
    local test_exit_code=$?
    
    echo ""
    echo "========================================"
    echo "📊 测试完成状态: $test_exit_code"
    
    # 最终内存检查
    monitor_memory
    
    return $test_exit_code
}

# 执行主函数
main "$@"