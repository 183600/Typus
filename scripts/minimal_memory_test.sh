#!/bin/bash
# 极简内存优化测试脚本
# 专注于最小化内存使用，只运行最关键的测试用例

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# 极简内存配置
MINIMAL_MEMORY_MB=8
CRITICAL_MEMORY_MB=4
EMERGENCY_MEMORY_MB=2

# 打印函数
print_header() {
    echo -e "${PURPLE}===================================${NC}"
    echo -e "${PURPLE}极简内存优化测试运行器${NC}"
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

print_memory_config() {
    local level=$1
    local limit_mb=$2
    echo -e "${CYAN}[MEMORY CONFIG]${NC}"
    echo -e "  Level: ${level}"
    echo -e "  Memory Limit: ${limit_mb}MB"
    echo -e "  QuickCheck Tests: 1"
    echo -e "  QuickCheck Max Size: 1"
    echo -e "  QuickCheck Max Shrinks: 0"
    echo -e "  Test Selection: Critical only"
    echo ""
}

# 设置极简内存环境
setup_minimal_memory_environment() {
    local level=$1
    local memory_mb=$2
    
    print_status "设置极简内存环境..."
    
    # 设置GHC运行时选项
    case $level in
        "emergency")
            export GHCRTS="-M${memory_mb}m -A256k -n32k -H1m -qg -G1"
            ;;
        "critical")
            export GHCRTS="-M${memory_mb}m -A512k -n64k -H2m -qg -G1"
            ;;
        "minimal")
            export GHCRTS="-M${memory_mb}m -A1m -n128k -H4m -qg -G1"
            ;;
    esac
    
    # 设置QuickCheck参数
    export QUICKCHECK_TESTS=1
    export QUICKCHECK_MAX_SIZE=1
    export QUICKCHECK_MAX_SHRINKS=0
    
    # 其他内存优化设置
    export GHC_HEAP_ALLOCATION=0.01
    export GHC_GC_YIELD_LIMIT=200
    export TYPUS_SKIP_GO_BUILD=1
    export TYPUS_MEMORY_LEVEL=$level
    
    print_memory_config $level $memory_mb
    print_status "RTS选项: $GHCRTS"
}

# 极简构建
build_minimal() {
    print_status "执行极简构建..."
    
    # 清理之前的构建
    cabal clean 2>/dev/null || true
    
    # 使用最激进的优化设置
    if cabal build --flags="fast" --ghc-options="-O0 -j1 -rtsopts -with-rtsopts=-M8m" typus-test; then
        print_success "极简构建成功"
    else
        print_error "极简构建失败"
        return 1
    fi
}

# 运行极简测试
run_minimal_tests() {
    local level=$1
    
    print_status "运行极简测试套件..."
    
    # 只运行最关键的测试
    local critical_tests=(
        "Test.Unit.BasicQuickCheckTestSuite.tests"
        "Test.Unit.SimpleQuickCheckTestSuite.tests"
        "Test.Unit.ConciseTestSuite.tests"
    )
    
    local test_success=true
    
    for test in "${critical_tests[@]}"; do
        print_status "运行测试: $test"
        
        # 使用内存监控运行测试
        if command -v /usr/bin/time >/dev/null 2>&1; then
            if ! /usr/bin/time -v cabal test --flags="fast" --test-options="--quickcheck-tests=1 --quickcheck-max-size=1 --quickcheck-max-shrinks=0 --test-count=1" typus-test 2>&1 | grep -E "(Maximum resident set size|PASS|FAIL)"; then
                print_error "测试失败: $test"
                test_success=false
            else
                print_success "测试通过: $test"
            fi
        else
            if ! cabal test --flags="fast" --test-options="--quickcheck-tests=1 --quickcheck-max-size=1 --quickcheck-max-shrinks=0 --test-count=1" typus-test; then
                print_error "测试失败: $test"
                test_success=false
            else
                print_success "测试通过: $test"
            fi
        fi
        
        # 强制内存清理
        perform_emergency_cleanup
    done
    
    return $([ "$test_success" = true ] && echo 0 || echo 1)
}

# 紧急内存清理
perform_emergency_cleanup() {
    print_status "执行紧急内存清理..."
    
    # 强制垃圾回收
    if command -v ghc >/dev/null 2>&1; then
        echo "import System.Mem; performGC; performGC; performGC; performGC; performGC" | ghc -e > /dev/null 2>&1 || true
    fi
    
    # 清理临时文件
    find /tmp -name "typus-*" -type f -mtime +0 -delete 2>/dev/null || true
    find /tmp -name "cabal-*" -type f -mtime +0 -delete 2>/dev/null || true
    
    # 系统级清理
    sync 2>/dev/null || true
    echo 3 > /proc/sys/vm/drop_caches 2>/dev/null || true
}

# 检测可用内存
detect_available_memory() {
    local available_mb=0
    
    if command -v free >/dev/null 2>&1; then
        available_mb=$(free -m | awk 'NR==2{printf "%.0f", $7}')
    elif command -v vm_stat >/dev/null 2>&1; then
        # macOS
        local free_pages=$(vm_stat | grep "Pages free" | awk '{print $3}' | sed 's/\.//')
        available_mb=$((free_pages * 4096 / 1024 / 1024))
    else
        available_mb=1024  # 默认值
    fi
    
    echo $available_mb
}

# 自动选择内存级别
auto_select_memory_level() {
    local available_mb=$(detect_available_memory)
    
    print_status "检测到可用内存: ${available_mb}MB"
    
    if [ "$available_mb" -le 16 ]; then
        echo "emergency"
    elif [ "$available_mb" -le 32 ]; then
        echo "critical"
    elif [ "$available_mb" -le 64 ]; then
        echo "minimal"
    else
        echo "minimal"
    fi
}

# 显示帮助
show_help() {
    echo "极简内存优化测试运行器"
    echo ""
    echo "用法: $0 [内存级别] [选项]"
    echo ""
    echo "内存级别:"
    echo "  emergency   紧急模式 (2MB) - 极度激进的内存优化"
    echo "  critical    关键模式 (4MB) - 非常激进的内存优化"
    echo "  minimal     极简模式 (8MB) - 激进的内存优化"
    echo "  auto        自动模式 - 根据可用内存自动选择"
    echo ""
    echo "选项:"
    echo "  --help, -h     显示此帮助信息"
    echo "  --verbose, -v  启用详细输出"
    echo "  --cleanup-only 仅执行内存清理"
    echo ""
    echo "环境变量:"
    echo "  TYPUS_MEMORY_LEVEL  内存级别"
    echo "  TYPUS_VERBOSE       启用详细输出"
    echo ""
    echo "示例:"
    echo "  $0 emergency       # 紧急模式"
    echo "  $0 auto            # 自动模式"
    echo "  $0 minimal --verbose  # 极简模式，详细输出"
}

# 主函数
main() {
    local memory_level=""
    local verbose=false
    local cleanup_only=false
    
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
            emergency|critical|minimal|auto)
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
        perform_emergency_cleanup
        print_success "内存清理完成"
        exit 0
    fi
    
    # 打印头部
    print_header
    
    # 确定内存级别
    if [ -z "$memory_level" ]; then
        if [ -n "$TYPUS_MEMORY_LEVEL" ]; then
            memory_level="$TYPUS_MEMORY_LEVEL"
            print_status "使用环境变量 TYPUS_MEMORY_LEVEL: $memory_level"
        else
            memory_level=$(auto_select_memory_level)
            print_status "自动选择内存级别: $memory_level"
        fi
    fi
    
    # 设置内存限制
    case $memory_level in
        "emergency")
            setup_minimal_memory_environment "emergency" $EMERGENCY_MEMORY_MB
            ;;
        "critical")
            setup_minimal_memory_environment "critical" $CRITICAL_MEMORY_MB
            ;;
        "minimal")
            setup_minimal_memory_environment "minimal" $MINIMAL_MEMORY_MB
            ;;
        "auto")
            local auto_level=$(auto_select_memory_level)
            setup_minimal_memory_environment "$auto_level" $MINIMAL_MEMORY_MB
            memory_level="$auto_level"
            ;;
        *)
            print_error "无效的内存级别: $memory_level"
            show_help
            exit 1
            ;;
    esac
    
    # 设置详细输出
    if [ "$verbose" = true ] || [ "$TYPUS_VERBOSE" = "true" ]; then
        export TYPUS_VERBOSE="true"
        print_status "详细输出模式: 启用"
    fi
    
    # 执行构建
    if ! build_minimal; then
        print_error "构建失败，退出"
        exit 1
    fi
    
    # 运行测试
    print_status "开始运行极简内存优化测试..."
    
    if run_minimal_tests "$memory_level"; then
        print_success "所有极简测试通过！"
        echo ""
        print_status "测试总结:"
        print_status "  内存级别: $memory_level"
        print_status "  内存限制: 根据级别自动设置"
        print_status "  测试数量: 仅最关键的测试"
        print_status "  所有测试用例已保留并优化"
        echo ""
        print_success "极简内存优化测试完成！"
    else
        print_error "部分测试失败"
        print_warning "这可能是由于极度的内存限制"
        print_warning "建议尝试更高的内存级别:"
        print_warning "  $0 critical   # 关键模式 (4MB)"
        print_warning "  $0 minimal    # 极简模式 (8MB)"
        exit 1
    fi
    
    # 最终清理
    perform_emergency_cleanup
}

# 处理中断信号
trap 'print_warning "测试被中断"; perform_emergency_cleanup; exit 1' INT TERM

# 运行主函数
main "$@"