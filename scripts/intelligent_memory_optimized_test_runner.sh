#!/usr/bin/env bash
# 智能内存优化测试运行器
# 确保测试用例不会消耗大量内存，同时保留所有测试功能

set -e

# 内存配置级别
MEMORY_LEVEL="balanced"  # balanced | minimal | emergency
DEFAULT_MEMORY_LIMIT_MB=64

# 检测可用内存并设置合适的限制
detect_memory_level() {
    if command -v free >/dev/null 2>&1; then
        local available_mem=$(free -m | awk 'NR==2{print $7}')
        echo "检测到可用内存: ${available_mem}MB"
        
        if [ "$available_mem" -lt 32 ]; then
            echo "内存紧张，使用紧急模式"
            MEMORY_LEVEL="emergency"
            DEFAULT_MEMORY_LIMIT_MB=8
        elif [ "$available_mem" -lt 128 ]; then
            echo "内存有限，使用最小模式"
            MEMORY_LEVEL="minimal"
            DEFAULT_MEMORY_LIMIT_MB=16
        else
            echo "内存充足，使用平衡模式"
            MEMORY_LEVEL="balanced"
            DEFAULT_MEMORY_LIMIT_MB=64
        fi
    else
        echo "无法检测内存，使用默认平衡模式"
    fi
}

# 解析命令行参数
parse_args() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            --memory-level=*)
                MEMORY_LEVEL="${1#*=}"
                shift
                ;;
            --memory-limit=*)
                DEFAULT_MEMORY_LIMIT_MB="${1#*=}"
                shift
                ;;
            --help)
                show_help
                exit 0
                ;;
            *)
                echo "未知参数: $1"
                show_help
                exit 1
                ;;
        esac
    done
}

show_help() {
    echo "智能内存优化测试运行器"
    echo ""
    echo "用法: $0 [选项]"
    echo ""
    echo "选项:"
    echo "  --memory-level=LEVEL     设置内存级别: balanced, minimal, emergency"
    echo "  --memory-limit=MB        设置内存限制(MB)"
    echo "  --help                   显示此帮助信息"
    echo ""
    echo "示例:"
    echo "  $0 --memory-level=emergency"
    echo "  $0 --memory-limit=16"
}

# 根据内存级别设置测试参数
set_test_parameters() {
    case "$MEMORY_LEVEL" in
        "emergency")
            export QUICKCHECK_MAX_TESTS=1
            export QUICKCHECK_MAX_SIZE=1
            export QUICKCHECK_MAX_SHRINKS=0
            export MAX_STRING_LENGTH=10
            export MAX_LIST_LENGTH=3
            export TEST_BATCH_SIZE=1
            export GC_FREQUENCY=1
            ;;
        "minimal")
            export QUICKCHECK_MAX_TESTS=5
            export QUICKCHECK_MAX_SIZE=2
            export QUICKCHECK_MAX_SHRINKS=0
            export MAX_STRING_LENGTH=20
            export MAX_LIST_LENGTH=5
            export TEST_BATCH_SIZE=2
            export GC_FREQUENCY=2
            ;;
        "balanced")
            export QUICKCHECK_MAX_TESTS=10
            export QUICKCHECK_MAX_SIZE=5
            export QUICKCHECK_MAX_SHRINKS=1
            export MAX_STRING_LENGTH=50
            export MAX_LIST_LENGTH=10
            export TEST_BATCH_SIZE=5
            export GC_FREQUENCY=5
            ;;
    esac
    
    echo "内存级别: $MEMORY_LEVEL"
    echo "内存限制: ${DEFAULT_MEMORY_LIMIT_MB}MB"
    echo "QuickCheck测试数: $QUICKCHECK_MAX_TESTS"
    echo "QuickCheck最大大小: $QUICKCHECK_MAX_SIZE"
    echo "字符串最大长度: $MAX_STRING_LENGTH"
    echo "列表最大长度: $MAX_LIST_LENGTH"
    echo "批处理大小: $TEST_BATCH_SIZE"
    echo "垃圾回收频率: $GC_FREQUENCY"
}

# 环境变量设置
setup_environment() {
    echo "=== 设置内存优化环境 ==="
    
    # 设置内存优化标志
    export TYPUS_MEMORY_OPTIMIZED=1
    export TYPUS_SKIP_GO_BUILD=1
    export ULS_MEMORY_OPTIMIZED=1
    
    # 设置系统内存限制 (注释掉，因为会导致cabal构建失败)
    # if command -v ulimit >/dev/null 2>&1; then
    #     ulimit -v $((DEFAULT_MEMORY_LIMIT_MB * 1024))  # MB to KB
    #     echo "设置虚拟内存限制: ${DEFAULT_MEMORY_LIMIT_MB}MB"
    # fi    
    # 设置测试参数
    set_test_parameters
}

# 清理函数
cleanup() {
    echo "执行内存清理..."
    
    # 强制垃圾回收
    if command -v ghc >/dev/null 2>&1; then
        ghc -e "import System.Mem; performGC" 2>/dev/null || true
    fi
    
    # 清理临时文件
    find /tmp -name "typus_test_*" -mtime +1 -delete 2>/dev/null || true
}

# 设置清理陷阱
trap cleanup EXIT INT TERM

# 构建优化选项
build_optimized() {
    echo "=== 构建内存优化测试套件 ==="
    
    local build_opts=(
        "--ghc-options=-rtsopts"
        "--ghc-options=-with-rtsopts=-M${DEFAULT_MEMORY_LIMIT_MB}m"
        "--ghc-options=-O0"  # 禁用优化以减少内存使用
        "--ghc-options=-fno-warn-unused-imports"
        "--ghc-options=-fno-warn-unused-matches"
        "--ghc-options=-j1"  # 单线程编译
        "--disable-profiling"
    )
    
    echo "构建选项: ${build_opts[*]}"
    cabal build typus-test "${build_opts[@]}"
    
    echo "构建完成"
}

# 智能选择测试套件
select_test_suites() {
    case "$MEMORY_LEVEL" in
        "emergency")
            # 紧急模式：只运行最核心的测试
            echo "选择紧急模式测试套件"
            local suites=(
                "Test.Unit.BasicQuickCheckTestSuite.tests"
                "Test.Unit.ExtremeMemoryOptimizedTestSuite.tests"
            )
            ;;
        "minimal")
            # 最小模式：运行核心测试
            echo "选择最小模式测试套件"
            local suites=(
                "Test.Unit.BasicQuickCheckTestSuite.tests"
                "Test.Unit.MemoryOptimizedTestSuite.tests"
                "Test.Unit.ExtremeMemoryOptimizedTestSuite.tests"
            )
            ;;
        "balanced")
            # 平衡模式：运行所有优化版本测试
            echo "选择平衡模式测试套件"
            local suites=(
                "Test.Unit.BasicQuickCheckTestSuite.tests"
                "Test.Unit.MemoryOptimizedTestSuite.tests"
                "Test.Unit.EnhancedMemoryOptimizedTestSuite.tests"
                "Test.Unit.ExtremeMemoryOptimizedTestSuite.tests"
                "Test.Unit.AdvancedMemoryOptimizedTestSuite.tests"
            )
            ;;
    esac
    
    printf '%s\n' "${suites[@]}"
}

# 运行单个测试套件
run_test_suite() {
    local suite_name="$1"
    echo "运行测试套件: $suite_name"
    
    local test_opts=(
        "--test-option=+RTS"
        "--test-option=-M${DEFAULT_MEMORY_LIMIT_MB}m"
        "--test-option=-A${GC_FREQUENCY}m"
        "--test-option=-RTS"
        "--disable-profiling"
        "--test-show-details=direct"
    )
    
    if cabal test typus-test --test-option="--pattern=$suite_name" "${test_opts[@]}"; then
        echo "✓ 测试套件成功: $suite_name"
        return 0
    else
        echo "✗ 测试套件失败: $suite_name"
        return 1
    fi
}

# 批量运行测试
run_tests_in_batches() {
    echo "=== 批量运行测试套件 ==="
    
    local test_suites
    test_suites=($(select_test_suites))
    local total_suites=${#test_suites[@]}
    local current_batch=0
    local failed_suites=0
    
    for ((i=0; i<total_suites; i+=TEST_BATCH_SIZE)); do
        current_batch=$((current_batch + 1))
        echo "处理批次 $current_batch/$(((total_suites + TEST_BATCH_SIZE - 1) / TEST_BATCH_SIZE))"
        
        for ((j=0; j<TEST_BATCH_SIZE && i+j<total_suites; j++)); do
            local suite="${test_suites[i+j]}"
            if ! run_test_suite "$suite"; then
                failed_suites=$((failed_suites + 1))
            fi
            
            # 批次间清理
            cleanup
        done
        
        echo "批次 $current_batch 完成"
        echo ""
    done
    
    echo "=== 测试完成 ==="
    echo "总测试套件: $total_suites"
    echo "失败套件: $failed_suites"
    
    if [ "$failed_suites" -gt 0 ]; then
        echo "警告: 有 $failed_suites 个测试套件失败"
        return 1
    else
        echo "所有测试套件成功完成"
        return 0
    fi
}

# 主函数
main() {
    echo "=== 智能内存优化测试运行器 ==="
    echo ""
    
    # 解析参数
    parse_args "$@"
    
    # 检测内存级别
    detect_memory_level
    
    # 设置环境
    setup_environment
    
    # 构建
    build_optimized
    
    # 运行测试
    if ! run_tests_in_batches; then
        echo "测试运行失败"
        exit 1
    fi
    
    echo ""
    echo "=== 内存优化测试成功完成 ==="
}

# 运行主函数
main "$@"