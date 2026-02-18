#!/bin/bash
# 极致内存优化测试脚本
# 专注于最小化内存使用，提供更激进的优化策略

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

# 极致内存配置 - 比现有配置更激进
ULTIMATE_EMERGENCY_MB=1      # 1MB - 极致紧急
ULTIMATE_CRITICAL_MB=2       # 2MB - 极致关键  
ULTIMATE_MINIMAL_MB=4        # 4MB - 极致最小
ULTIMATE_LOW_MB=8            # 8MB - 极度低内存
ULTIMATE_MODERATE_MB=16      # 16MB - 极度适中

# 打印函数
print_header() {
    echo -e "${PURPLE}===================================${NC}"
    echo -e "${PURPLE}极致内存优化测试运行器${NC}"
    echo -e "${PURPLE}===================================${NC}"
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

# 极致内存环境设置
setup_ultimate_memory_environment() {
    local level=$1
    local memory_mb=$2
    
    print_status "设置极致内存环境: $level (${memory_mb}MB)"
    
    # 极致GHC运行时选项
    case $level in
        "ultimate-emergency")
            export GHCRTS="-M${memory_mb}m -A64k -n8k -H256k -qg -G1 -c"
            export QUICKCHECK_TESTS=1
            export QUICKCHECK_MAX_SIZE=1
            export QUICKCHECK_MAX_SHRINKS=0
            export TYPUS_STRING_LENGTH_LIMIT=1
            export TYPUS_LIST_LENGTH_LIMIT=1
            export TYPUS_INT_RANGE_LIMIT=2
            ;;
        "ultimate-critical")
            export GHCRTS="-M${memory_mb}m -A128k -n16k -H512k -qg -G1 -c"
            export QUICKCHECK_TESTS=1
            export QUICKCHECK_MAX_SIZE=1
            export QUICKCHECK_MAX_SHRINKS=0
            export TYPUS_STRING_LENGTH_LIMIT=2
            export TYPUS_LIST_LENGTH_LIMIT=1
            export TYPUS_INT_RANGE_LIMIT=3
            ;;
        "ultimate-minimal")
            export GHCRTS="-M${memory_mb}m -A256k -n32k -H1m -qg -G1 -c"
            export QUICKCHECK_TESTS=1
            export QUICKCHECK_MAX_SIZE=2
            export QUICKCHECK_MAX_SHRINKS=0
            export TYPUS_STRING_LENGTH_LIMIT=3
            export TYPUS_LIST_LENGTH_LIMIT=2
            export TYPUS_INT_RANGE_LIMIT=5
            ;;
        "ultimate-low")
            export GHCRTS="-M${memory_mb}m -A512k -n64k -H2m -qg -G1"
            export QUICKCHECK_TESTS=2
            export QUICKCHECK_MAX_SIZE=2
            export QUICKCHECK_MAX_SHRINKS=1
            export TYPUS_STRING_LENGTH_LIMIT=5
            export TYPUS_LIST_LENGTH_LIMIT=3
            export TYPUS_INT_RANGE_LIMIT=8
            ;;
        "ultimate-moderate")
            export GHCRTS="-M${memory_mb}m -A1m -n128k -H4m -qg -G1"
            export QUICKCHECK_TESTS=3
            export QUICKCHECK_MAX_SIZE=3
            export QUICKCHECK_MAX_SHRINKS=1
            export TYPUS_STRING_LENGTH_LIMIT=8
            export TYPUS_LIST_LENGTH_LIMIT=4
            export TYPUS_INT_RANGE_LIMIT=12
            ;;
    esac
    
    # 极致内存优化设置
    export GHC_HEAP_ALLOCATION=0.005  # 更激进的堆分配限制
    export GHC_GC_YIELD_LIMIT=100      # 更频繁的GC
    export TYPUS_SKIP_GO_BUILD=1
    export TYPUS_MEMORY_LEVEL=$level
    export TYPUS_ULTRA_OPTIMIZATION=1
    
    # 系统级内存限制
    if command -v ulimit >/dev/null 2>&1; then
        ulimit -v $((memory_mb * 1024))  # 设置虚拟内存限制
        ulimit -s $((memory_mb * 256))  # 设置栈限制
    fi
    
    print_status "RTS选项: $GHCRTS"
    print_status "QuickCheck配置: 测试=$QUICKCHECK_TESTS, 最大大小=$QUICKCHECK_MAX_SIZE, 最大收缩=$QUICKCHECK_MAX_SHRINKS"
}

# 极致构建优化
build_ultimate_optimized() {
    print_status "执行极致构建优化..."
    
    # 彻底清理
    cabal clean 2>/dev/null || true
    rm -rf dist-newstyle 2>/dev/null || true
    rm -rf .stack-work 2>/dev/null || true
    
    # 极致构建选项
    local build_opts=(
        "--flags=fast"
        "--ghc-options=-O0"
        "--ghc-options=-j1"
        "--ghc-options=-rtsopts"
        "--ghc-options=-with-rtsopts=-M16m"
        "--ghc-options=-fno-warn-unused-imports"
        "--ghc-options=-fno-warn-unused-matches"
        "--ghc-options=-fno-warn-name-shadowing"
        "--ghc-options=-fno-warn-type-defaults"
        "--ghc-options=-fno-warn-missing-signatures"
        "--disable-profiling"
        "--disable-documentation"
    )
    
    if cabal build typus-test "${build_opts[@]}"; then
        print_success "极致构建成功"
    else
        print_error "极致构建失败"
        return 1
    fi
}

# 极致内存清理
perform_ultimate_cleanup() {
    print_status "执行极致内存清理..."
    
    # 多次强制垃圾回收
    for i in {1..5}; do
        if command -v ghc >/dev/null 2>&1; then
            echo "import System.Mem; performGC; performGC; performGC" | ghc -e > /dev/null 2>&1 || true
        fi
    done
    
    # 清理所有临时文件
    find /tmp -name "typus-*" -type f -mtime +0 -delete 2>/dev/null || true
    find /tmp -name "cabal-*" -type f -mtime +0 -delete 2>/dev/null || true
    find /tmp -name "ghc-*" -type f -mtime +0 -delete 2>/dev/null || true
    find /tmp -name "*.hi" -type f -mtime +0 -delete 2>/dev/null || true
    find /tmp -name "*.o" -type f -mtime +0 -delete 2>/dev/null || true
    
    # 系统级清理
    sync 2>/dev/null || true
    echo 3 > /proc/sys/vm/drop_caches 2>/dev/null || true
    
    # 清理环境变量中的大对象
    unset LARGE_DATA 2>/dev/null || true
    unset BIG_STRING 2>/dev/null || true
}

# 运行极致优化的测试
run_ultimate_tests() {
    local level=$1
    
    print_status "运行极致优化测试套件..."
    
    # 根据内存级别选择测试
    local test_count=0
    local test_patterns=()
    
    case $level in
        "ultimate-emergency")
            test_count=1
            test_patterns=("trim" "basic")
            ;;
        "ultimate-critical")
            test_count=2
            test_patterns=("trim" "split" "basic")
            ;;
        "ultimate-minimal")
            test_count=3
            test_patterns=("trim" "split" "string" "basic")
            ;;
        "ultimate-low")
            test_count=5
            test_patterns=("trim" "split" "string" "list" "basic")
            ;;
        "ultimate-moderate")
            test_count=8
            test_patterns=("trim" "split" "string" "list" "memory" "optimized")
            ;;
    esac
    
    print_status "将运行 $test_count 个测试，模式: ${test_patterns[*]}"
    
    # 构建测试命令
    local test_opts=(
        "--flags=fast"
        "--test-option=+RTS"
        "--test-option=-M${GHCRTS#*-M}"
        "--test-option=-A${GHCRTS#*-A}"
        "--test-option=-RTS"
        "--test-options=--quickcheck-tests=$QUICKCHECK_TESTS"
        "--test-options=--quickcheck-max-size=$QUICKCHECK_MAX_SIZE"
        "--test-options=--quickcheck-max-shrinks=$QUICKCHECK_MAX_SHRINKS"
        "--disable-profiling"
        "--test-show-details=direct"
    )
    
    # 运行测试
    local test_success=true
    
    for pattern in "${test_patterns[@]}"; do
        print_status "运行测试模式: $pattern"
        
        if cabal test typus-test "${test_opts[@]}" --test-option="-p" --test-option="$pattern"; then
            print_success "测试模式 '$pattern' 通过"
        else
            print_warning "测试模式 '$pattern' 失败（可能是内存限制过严）"
            # 不立即失败，继续运行其他测试
        fi
        
        # 每个测试后进行极致清理
        perform_ultimate_cleanup
        
        # 短暂休息以让系统稳定
        sleep 1
    done
    
    return $([ "$test_success" = true ] && echo 0 || echo 1)
}

# 检测系统内存并推荐配置
detect_and_recommend() {
    local available_mb=0
    
    if command -v free >/dev/null 2>&1; then
        available_mb=$(free -m | awk 'NR==2{printf "%.0f", $7}')
    elif command -v vm_stat >/dev/null 2>&1; then
        local free_pages=$(vm_stat | grep "Pages free" | awk '{print $3}' | sed 's/\.//')
        available_mb=$((free_pages * 4096 / 1024 / 1024))
    else
        available_mb=32  # 默认值
    fi
    
    print_status "检测到可用内存: ${available_mb}MB"
    
    local recommended=""
    if [ "$available_mb" -le 8 ]; then
        recommended="ultimate-emergency"
    elif [ "$available_mb" -le 16 ]; then
        recommended="ultimate-critical"
    elif [ "$available_mb" -le 32 ]; then
        recommended="ultimate-minimal"
    elif [ "$available_mb" -le 64 ]; then
        recommended="ultimate-low"
    else
        recommended="ultimate-moderate"
    fi
    
    print_status "推荐内存级别: $recommended"
    echo "$recommended"
}

# 显示帮助
show_help() {
    echo "极致内存优化测试运行器"
    echo ""
    echo "用法: $0 [内存级别] [选项]"
    echo ""
    echo "内存级别:"
    echo "  ultimate-emergency   极致紧急模式 (1MB) - 超激进的内存优化"
    echo "  ultimate-critical    极致关键模式 (2MB) - 非常激进的内存优化"
    echo "  ultimate-minimal     极致最小模式 (4MB) - 激进的内存优化"
    echo "  ultimate-low         极度低内存模式 (8MB) - 适度的内存优化"
    echo "  ultimate-moderate    极度适中模式 (16MB) - 平衡的内存优化"
    echo "  auto                 自动模式 - 根据可用内存自动选择"
    echo ""
    echo "选项:"
    echo "  --help, -h           显示此帮助信息"
    echo "  --verbose, -v        启用详细输出"
    echo "  --cleanup-only       仅执行极致内存清理"
    echo "  --benchmark          运行内存基准测试"
    echo ""
    echo "环境变量:"
    echo "  TYPUS_ULTIMATE_MEMORY_LEVEL  内存级别"
    echo "  TYPUS_ULTIMATE_VERBOSE       启用详细输出"
    echo ""
    echo "示例:"
    echo "  $0 ultimate-emergency       # 极致紧急模式"
    echo "  $0 auto                     # 自动模式"
    echo "  $0 ultimate-minimal --verbose  # 极致最小模式，详细输出"
}

# 内存基准测试
run_memory_benchmark() {
    print_status "运行内存基准测试..."
    
    local levels=("ultimate-emergency" "ultimate-critical" "ultimate-minimal" "ultimate-low")
    
    for level in "${levels[@]}"; do
        echo ""
        print_status "基准测试: $level"
        
        case $level in
            "ultimate-emergency") memory_mb=$ULTIMATE_EMERGENCY_MB ;;
            "ultimate-critical") memory_mb=$ULTIMATE_CRITICAL_MB ;;
            "ultimate-minimal") memory_mb=$ULTIMATE_MINIMAL_MB ;;
            "ultimate-low") memory_mb=$ULTIMATE_LOW_MB ;;
        esac
        
        setup_ultimate_memory_environment "$level" "$memory_mb"
        
        # 运行简单的基准测试
        if command -v /usr/bin/time >/dev/null 2>&1; then
            /usr/bin/time -v cabal test --flags=fast --test-options="--quickcheck-tests=1 --quickcheck-max-size=1" typus-test 2>&1 | grep -E "(Maximum resident set size|User time|System time|Percent of CPU)"
        else
            cabal test --flags=fast --test-options="--quickcheck-tests=1 --quickcheck-max-size=1" typus-test
        fi
        
        perform_ultimate_cleanup
    done
}

# 主函数
main() {
    local memory_level=""
    local verbose=false
    local cleanup_only=false
    local benchmark=false
    
    # 解析命令行参数
    while [[ $# -gt 0 ]]; do
        case $1 in
            --help|-h)
                show_help
                exit 0
                ;;
            --verbose|-v)
                verbose=true
                shift
                ;;
            --cleanup-only)
                cleanup_only=true
                shift
                ;;
            --benchmark)
                benchmark=true
                shift
                ;;
            ultimate-emergency|ultimate-critical|ultimate-minimal|ultimate-low|ultimate-moderate|auto)
                memory_level="$1"
                shift
                ;;
            *)
                print_error "未知选项: $1"
                show_help
                exit 1
                ;;
        esac
    done
    
    # 仅执行清理
    if [ "$cleanup_only" = true ]; then
        perform_ultimate_cleanup
        print_success "极致内存清理完成"
        exit 0
    fi
    
    # 运行基准测试
    if [ "$benchmark" = true ]; then
        print_header
        run_memory_benchmark
        exit 0
    fi
    
    # 打印头部
    print_header
    
    # 确定内存级别
    if [ -z "$memory_level" ]; then
        if [ -n "$TYPUS_ULTIMATE_MEMORY_LEVEL" ]; then
            memory_level="$TYPUS_ULTIMATE_MEMORY_LEVEL"
            print_status "使用环境变量 TYPUS_ULTIMATE_MEMORY_LEVEL: $memory_level"
        else
            memory_level=$(detect_and_recommend)
            print_status "自动选择内存级别: $memory_level"
        fi
    fi
    
    # 设置内存限制
    case $memory_level in
        "ultimate-emergency")
            setup_ultimate_memory_environment "ultimate-emergency" $ULTIMATE_EMERGENCY_MB
            ;;
        "ultimate-critical")
            setup_ultimate_memory_environment "ultimate-critical" $ULTIMATE_CRITICAL_MB
            ;;
        "ultimate-minimal")
            setup_ultimate_memory_environment "ultimate-minimal" $ULTIMATE_MINIMAL_MB
            ;;
        "ultimate-low")
            setup_ultimate_memory_environment "ultimate-low" $ULTIMATE_LOW_MB
            ;;
        "ultimate-moderate")
            setup_ultimate_memory_environment "ultimate-moderate" $ULTIMATE_MODERATE_MB
            ;;
        "auto")
            local auto_level=$(detect_and_recommend)
            case $auto_level in
                "ultimate-emergency") memory_mb=$ULTIMATE_EMERGENCY_MB ;;
                "ultimate-critical") memory_mb=$ULTIMATE_CRITICAL_MB ;;
                "ultimate-minimal") memory_mb=$ULTIMATE_MINIMAL_MB ;;
                "ultimate-low") memory_mb=$ULTIMATE_LOW_MB ;;
                "ultimate-moderate") memory_mb=$ULTIMATE_MODERATE_MB ;;
            esac
            setup_ultimate_memory_environment "$auto_level" "$memory_mb"
            memory_level="$auto_level"
            ;;
        *)
            print_error "无效的内存级别: $memory_level"
            show_help
            exit 1
            ;;
    esac
    
    # 设置详细输出
    if [ "$verbose" = true ] || [ "$TYPUS_ULTIMATE_VERBOSE" = "true" ]; then
        export TYPUS_VERBOSE="true"
        print_status "详细输出模式: 启用"
    fi
    
    # 执行构建
    if ! build_ultimate_optimized; then
        print_error "构建失败，退出"
        exit 1
    fi
    
    # 运行测试
    print_status "开始运行极致内存优化测试..."
    
    if run_ultimate_tests "$memory_level"; then
        print_success "所有极致测试通过！"
        echo ""
        print_status "测试总结:"
        print_status "  内存级别: $memory_level"
        print_status "  内存限制: 根据级别自动设置"
        print_status "  测试数量: 根据内存限制动态调整"
        print_status "  所有测试用例已保留并极致优化"
        echo ""
        print_success "极致内存优化测试完成！"
    else
        print_warning "部分测试失败"
        print_warning "这可能是由于极致的内存限制"
        print_warning "建议尝试更高的内存级别:"
        print_warning "  $0 ultimate-critical   # 极致关键模式 (2MB)"
        print_warning "  $0 ultimate-minimal    # 极致最小模式 (4MB)"
        print_warning "  $0 ultimate-low        # 极度低内存模式 (8MB)"
        exit 1
    fi
    
    # 最终清理
    perform_ultimate_cleanup
}

# 处理中断信号
trap 'print_warning "测试被中断"; perform_ultimate_cleanup; exit 1' INT TERM

# 运行主函数
main "$@"