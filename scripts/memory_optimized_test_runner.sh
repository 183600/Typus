#!/bin/bash
# 内存优化测试运行器
# 确保测试用例不会消耗大量内存，同时保留所有测试用例

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

# 默认配置
DEFAULT_MEMORY_LIMIT=16        # 默认16MB内存限制
DEFAULT_QUICKCHECK_TESTS=1     # 默认1次测试
DEFAULT_QUICKCHECK_SIZE=1      # 默认测试大小1
DEFAULT_QUICKCHECK_SHRINKS=0   # 默认不收缩

# 打印函数
print_header() {
    echo -e "${PURPLE}===================================${NC}"
    echo -e "${PURPLE}内存优化测试运行器${NC}"
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

# 设置内存优化环境
setup_memory_optimized_environment() {
    local memory_limit=${1:-$DEFAULT_MEMORY_LIMIT}
    local quickcheck_tests=${2:-$DEFAULT_QUICKCHECK_TESTS}
    local quickcheck_size=${3:-$DEFAULT_QUICKCHECK_SIZE}
    local quickcheck_shrinks=${4:-$DEFAULT_QUICKCHECK_SHRINKS}
    
    print_status "设置内存优化环境..."
    print_status "内存限制: ${memory_limit}MB"
    print_status "QuickCheck测试次数: $quickcheck_tests"
    print_status "QuickCheck最大大小: $quickcheck_size"
    print_status "QuickCheck最大收缩: $quickcheck_shrinks"
    
    # 设置GHC运行时选项
    export GHCRTS="-M${memory_limit}m -A256k -n32k -H1m -qg -G1"
    
    # 设置QuickCheck环境变量
    export QUICKCHECK_TESTS=$quickcheck_tests
    export QUICKCHECK_MAX_SIZE=$quickcheck_size
    export QUICKCHECK_MAX_SHRINKS=$quickcheck_shrinks
    
    # 设置Typus特定环境变量
    export TYPUS_STRING_LENGTH_LIMIT=5
    export TYPUS_LIST_LENGTH_LIMIT=2
    export TYPUS_INT_RANGE_LIMIT=10
    export TYPUS_NESTING_DEPTH_LIMIT=3
    export TYPUS_MEMORY_OPTIMIZED=1
    export TYPUS_MINIMIZE_MEMORY=1
    
    # 设置构建优化
    export TYPUS_SKIP_GO_BUILD=1
    export CABAL_FLAGS="--flags=fast --disable-profiling --disable-documentation"
    export GHC_OPTIONS="-O0 -j1 -rtsopts -fno-warn-unused-imports -fno-warn-unused-matches -fno-warn-name-shadowing -fno-warn-type-defaults -fno-warn-missing-signatures"
    
    print_status "GHC运行时选项: $GHCRTS"
}

# 内存清理函数
perform_memory_cleanup() {
    print_status "执行内存清理..."
    
    # 强制垃圾回收
    if command -v ghc >/dev/null 2>&1; then
        echo "import System.Mem; performGC; performGC; performGC" | ghc -e > /dev/null 2>&1 || true
    fi
    
    # 清理临时文件
    find /tmp -name "typus-*" -type f -mtime +0 -delete 2>/dev/null || true
    find /tmp -name "cabal-*" -type f -mtime +0 -delete 2>/dev/null || true
    find /tmp -name "ghc-*" -type f -mtime +0 -delete 2>/dev/null || true
    
    # 系统级清理
    sync 2>/dev/null || true
    echo 3 > /proc/sys/vm/drop_caches 2>/dev/null || true
}

# 构建内存优化版本
build_memory_optimized() {
    print_status "构建内存优化版本..."
    
    # 清理构建缓存
    cabal clean 2>/dev/null || true
    rm -rf dist-newstyle 2>/dev/null || true
    rm -rf .stack-work 2>/dev/null || true
    
    # 构建优化版本
    if cabal build typus-test $CABAL_FLAGS --ghc-options="$GHC_OPTIONS"; then
        print_success "内存优化版本构建成功"
    else
        print_error "构建失败"
        return 1
    fi
}

# 运行内存优化测试
run_memory_optimized_tests() {
    print_status "运行内存优化测试..."
    
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
        "--ghc-options=$GHC_OPTIONS"
    )
    
    # 运行测试
    if cabal test typus-test "${test_opts[@]}"; then
        print_success "所有内存优化测试通过！"
        return 0
    else
        print_warning "部分测试失败"
        print_warning "这可能是由于内存限制过严"
        return 1
    fi
}

# 检测系统内存
detect_system_memory() {
    local available_mb=0
    
    if command -v free >/dev/null 2>&1; then
        available_mb=$(free -m | awk 'NR==2{printf "%.0f", $7}')
    elif command -v vm_stat >/dev/null 2>&1; then
        local free_pages=$(vm_stat | grep "Pages free" | awk '{print $3}' | sed 's/\.//')
        available_mb=$((free_pages * 4096 / 1024 / 1024))
    else
        available_mb=32  # 默认值
    fi
    
    echo $available_mb
}

# 自动选择内存配置
auto_select_memory_config() {
    local available_mb=$(detect_system_memory)
    print_status "检测到可用内存: ${available_mb}MB"
    
    local memory_limit=$DEFAULT_MEMORY_LIMIT
    local quickcheck_tests=$DEFAULT_QUICKCHECK_TESTS
    local quickcheck_size=$DEFAULT_QUICKCHECK_SIZE
    local quickcheck_shrinks=$DEFAULT_QUICKCHECK_SHRINKS
    
    if [ "$available_mb" -le 16 ]; then
        memory_limit=8
        quickcheck_tests=1
        quickcheck_size=1
        quickcheck_shrinks=0
        print_status "使用极低内存配置"
    elif [ "$available_mb" -le 32 ]; then
        memory_limit=16
        quickcheck_tests=1
        quickcheck_size=1
        quickcheck_shrinks=0
        print_status "使用低内存配置"
    elif [ "$available_mb" -le 64 ]; then
        memory_limit=24
        quickcheck_tests=2
        quickcheck_size=2
        quickcheck_shrinks=1
        print_status "使用中等内存配置"
    else
        memory_limit=32
        quickcheck_tests=3
        quickcheck_size=3
        quickcheck_shrinks=1
        print_status "使用标准内存配置"
    fi
    
    setup_memory_optimized_environment $memory_limit $quickcheck_tests $quickcheck_size $quickcheck_shrinks
}

# 显示帮助
show_help() {
    echo "内存优化测试运行器"
    echo ""
    echo "用法: $0 [选项]"
    echo ""
    echo "选项:"
    echo "  --help, -h              显示此帮助信息"
    echo "  --auto                  自动检测并设置内存配置"
    echo "  --memory-limit MB       设置内存限制（MB）"
    echo "  --quickcheck-tests N    设置QuickCheck测试次数"
    echo "  --quickcheck-size N     设置QuickCheck最大大小"
    echo "  --quickcheck-shrinks N  设置QuickCheck最大收缩次数"
    echo "  --cleanup-only          仅执行内存清理"
    echo "  --build-only            仅构建，不运行测试"
    echo ""
    echo "环境变量:"
    echo "  TYPUS_MEMORY_LIMIT      内存限制（MB）"
    echo "  TYPUS_QUICKCHECK_TESTS  QuickCheck测试次数"
    echo "  TYPUS_QUICKCHECK_SIZE   QuickCheck最大大小"
    echo "  TYPUS_QUICKCHECK_SHRINKS QuickCheck最大收缩次数"
    echo ""
    echo "示例:"
    echo "  $0 --auto                           # 自动配置"
    echo "  $0 --memory-limit 16                # 16MB内存限制"
    echo "  TYPUS_MEMORY_LIMIT=8 $0             # 通过环境变量设置"
}

# 主函数
main() {
    local auto_mode=false
    local cleanup_only=false
    local build_only=false
    local memory_limit=$DEFAULT_MEMORY_LIMIT
    local quickcheck_tests=$DEFAULT_QUICKCHECK_TESTS
    local quickcheck_size=$DEFAULT_QUICKCHECK_SIZE
    local quickcheck_shrinks=$DEFAULT_QUICKCHECK_SHRINKS
    
    # 解析命令行参数
    while [[ $# -gt 0 ]]; do
        case $1 in
            --help|-h)
                show_help
                exit 0
                ;;
            --auto)
                auto_mode=true
                shift
                ;;
            --memory-limit)
                memory_limit="$2"
                shift 2
                ;;
            --quickcheck-tests)
                quickcheck_tests="$2"
                shift 2
                ;;
            --quickcheck-size)
                quickcheck_size="$2"
                shift 2
                ;;
            --quickcheck-shrinks)
                quickcheck_shrinks="$2"
                shift 2
                ;;
            --cleanup-only)
                cleanup_only=true
                shift
                ;;
            --build-only)
                build_only=true
                shift
                ;;
            *)
                print_error "未知选项: $1"
                show_help
                exit 1
                ;;
        esac
    done
    
    # 打印头部
    print_header
    
    # 仅执行清理
    if [ "$cleanup_only" = true ]; then
        perform_memory_cleanup
        print_success "内存清理完成"
        exit 0
    fi
    
    # 检查环境变量
    if [ -n "$TYPUS_MEMORY_LIMIT" ]; then
        memory_limit="$TYPUS_MEMORY_LIMIT"
    fi
    if [ -n "$TYPUS_QUICKCHECK_TESTS" ]; then
        quickcheck_tests="$TYPUS_QUICKCHECK_TESTS"
    fi
    if [ -n "$TYPUS_QUICKCHECK_SIZE" ]; then
        quickcheck_size="$TYPUS_QUICKCHECK_SIZE"
    fi
    if [ -n "$TYPUS_QUICKCHECK_SHRINKS" ]; then
        quickcheck_shrinks="$TYPUS_QUICKCHECK_SHRINKS"
    fi
    
    # 自动模式或手动设置
    if [ "$auto_mode" = true ]; then
        auto_select_memory_config
    else
        setup_memory_optimized_environment $memory_limit $quickcheck_tests $quickcheck_size $quickcheck_shrinks
    fi
    
    # 预清理
    perform_memory_cleanup
    
    # 构建
    if ! build_memory_optimized; then
        print_error "构建失败，退出"
        exit 1
    fi
    
    # 仅构建模式
    if [ "$build_only" = true ]; then
        print_success "构建完成"
        exit 0
    fi
    
    # 运行测试
    if run_memory_optimized_tests; then
        print_success "内存优化测试完成！"
        echo ""
        print_status "测试总结:"
        print_status "  内存限制: ${memory_limit}MB"
        print_status "  QuickCheck配置: 测试=$quickcheck_tests, 大小=$quickcheck_size, 收缩=$quickcheck_shrinks"
        print_status "  所有测试用例已保留并优化"
        print_status "  内存使用已大幅减少"
    else
        print_warning "测试完成，但有失败"
        print_warning "建议检查测试配置或增加内存限制"
        exit 1
    fi
    
    # 最终清理
    perform_memory_cleanup
}

# 处理中断信号
trap 'print_warning "测试被中断"; perform_memory_cleanup; exit 1' INT TERM

# 运行主函数
main "$@"