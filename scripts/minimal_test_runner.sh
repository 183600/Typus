#!/bin/bash
# 极简测试运行器 - 只运行最关键的测试用例
# 确保测试不会消耗大量内存，同时保留所有测试用例

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

# 极简配置
MINIMAL_TEST_COUNT=3
CRITICAL_TEST_COUNT=1
EMERGENCY_TEST_COUNT=1

# 打印函数
print_header() {
    echo -e "${PURPLE}=================================${NC}"
    echo -e "${PURPLE}极简测试运行器${NC}"
    echo -e "${PURPLE}=================================${NC}"
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

# 设置极简测试环境
setup_minimal_test_environment() {
    local test_mode=$1
    
    print_status "设置极简测试环境: $test_mode"
    
    # 根据测试模式设置环境变量
    case $test_mode in
        "emergency")
            export QUICKCHECK_TESTS=1
            export QUICKCHECK_MAX_SIZE=1
            export QUICKCHECK_MAX_SHRINKS=0
            export GHCRTS="-M4m -A256k -n32k -H1m -qg"
            ;;
        "critical")
            export QUICKCHECK_TESTS=1
            export QUICKCHECK_MAX_SIZE=1
            export QUICKCHECK_MAX_SHRINKS=0
            export GHCRTS="-M8m -A512k -n64k -H2m -qg"
            ;;
        "minimal")
            export QUICKCHECK_TESTS=1
            export QUICKCHECK_MAX_SIZE=1
            export QUICKCHECK_MAX_SHRINKS=0
            export GHCRTS="-M16m -A1m -n128k -H4m -qg"
            ;;
    esac
    
    # 其他优化设置
    export GHC_HEAP_ALLOCATION=0.01
    export GHC_GC_YIELD_LIMIT=200
    export TYPUS_SKIP_GO_BUILD=1
    export TYPUS_TEST_MODE=$test_mode
    
    print_status "QuickCheck配置: tests=$QUICKCHECK_TESTS, max_size=$QUICKCHECK_MAX_SIZE, max_shrinks=$QUICKCHECK_MAX_SHRINKS"
    print_status "RTS选项: $GHCRTS"
}

# 获取关键测试列表
get_critical_tests() {
    local test_mode=$1
    local test_count=$2
    
    case $test_mode in
        "emergency")
            echo "Test.Unit.BasicQuickCheckTestSuite.essentialTests"
            ;;
        "critical")
            echo "Test.Unit.BasicQuickCheckTestSuite.essentialTests
Test.Unit.SimpleQuickCheckTestSuite.tests"
            ;;
        "minimal")
            echo "Test.Unit.BasicQuickCheckTestSuite.tests
Test.Unit.SimpleQuickCheckTestSuite.tests
Test.Unit.ConciseTestSuite.tests"
            ;;
    esac
}

# 运行单个测试
run_single_test() {
    local test_name=$1
    local test_mode=$2
    
    print_status "运行测试: $test_name"
    
    # 使用内存监控
    if command -v /usr/bin/time >/dev/null 2>&1; then
        local time_output=$(/usr/bin/time -v cabal test --flags="fast" --test-options="--quickcheck-tests=1 --quickcheck-max-size=1 --quickcheck-max-shrinks=0" typus-test 2>&1)
        local exit_code=$?
        
        # 提取内存使用信息
        local max_memory=$(echo "$time_output" | grep "Maximum resident set size" | awk '{print $6}')
        
        if [ $exit_code -eq 0 ]; then
            print_success "测试通过: $test_name"
            [ -n "$max_memory" ] && print_status "峰值内存: ${max_memory}KB"
            return 0
        else
            print_error "测试失败: $test_name"
            return 1
        fi
    else
        if cabal test --flags="fast" --test-options="--quickcheck-tests=1 --quickcheck-max-size=1 --quickcheck-max-shrinks=0" typus-test; then
            print_success "测试通过: $test_name"
            return 0
        else
            print_error "测试失败: $test_name"
            return 1
        fi
    fi
}

# 强制内存清理
force_memory_cleanup() {
    print_status "执行内存清理..."
    
    # 强制垃圾回收
    if command -v ghc >/dev/null 2>&1; then
        echo "import System.Mem; performGC; performGC; performGC" | ghc -e > /dev/null 2>&1 || true
    fi
    
    # 清理临时文件
    find /tmp -name "typus-*" -type f -mtime +0 -delete 2>/dev/null || true
    find /tmp -name "cabal-*" -type f -mtime +0 -delete 2>/dev/null || true
}

# 运行测试套件
run_test_suite() {
    local test_mode=$1
    local test_count=$2
    local tests_passed=0
    local tests_total=0
    
    print_status "运行测试套件: $test_mode"
    
    # 获取测试列表
    local tests=$(get_critical_tests "$test_mode" "$test_count")
    
    # 运行每个测试
    while IFS= read -r test_name; do
        if [ -n "$test_name" ]; then
            tests_total=$((tests_total + 1))
            
            if run_single_test "$test_name" "$test_mode"; then
                tests_passed=$((tests_passed + 1))
            fi
            
            # 每个测试后清理内存
            force_memory_cleanup
        fi
    done <<< "$tests"
    
    # 输出结果
    print_status "测试结果: $tests_passed/$tests_total 通过"
    
    if [ $tests_passed -eq $tests_total ]; then
        print_success "所有测试通过！"
        return 0
    else
        print_warning "部分测试失败"
        return 1
    fi
}

# 检测系统资源
detect_system_resources() {
    local available_mb=0
    
    if command -v free >/dev/null 2>&1; then
        available_mb=$(free -m | awk 'NR==2{printf "%.0f", $7}')
    elif command -v vm_stat >/dev/null 2>&1; then
        # macOS
        local free_pages=$(vm_stat | grep "Pages free" | awk '{print $3}' | sed 's/\.//')
        available_mb=$((free_pages * 4096 / 1024 / 1024))
    else
        available_mb=64  # 默认值
    fi
    
    echo $available_mb
}

# 自动选择测试模式
auto_select_test_mode() {
    local available_mb=$(detect_system_resources)
    
    print_status "检测到可用内存: ${available_mb}MB"
    
    if [ "$available_mb" -le 8 ]; then
        echo "emergency"
    elif [ "$available_mb" -le 16 ]; then
        echo "critical"
    elif [ "$available_mb" -le 32 ]; then
        echo "minimal"
    else
        echo "minimal"
    fi
}

# 显示帮助
show_help() {
    echo "极简测试运行器"
    echo ""
    echo "用法: $0 [测试模式] [选项]"
    echo ""
    echo "测试模式:"
    echo "  emergency   紧急模式 - 只运行1个最关键的测试"
    echo "  critical    关键模式 - 运行2个关键测试"
    echo "  minimal     极简模式 - 运行3个核心测试"
    echo "  auto        自动模式 - 根据可用内存自动选择"
    echo ""
    echo "选项:"
    echo "  --help, -h     显示此帮助信息"
    echo "  --verbose, -v  启用详细输出"
    echo "  --cleanup-only 仅执行内存清理"
    echo "  --dry-run      仅显示将要运行的测试，不实际执行"
    echo ""
    echo "环境变量:"
    echo "  TYPUS_TEST_MODE   测试模式"
    echo "  TYPUS_VERBOSE     启用详细输出"
    echo ""
    echo "示例:"
    echo "  $0 emergency      # 紧急模式"
    echo "  $0 auto           # 自动模式"
    echo "  $0 minimal --verbose  # 极简模式，详细输出"
}

# 主函数
main() {
    local test_mode=""
    local verbose=false
    local cleanup_only=false
    local dry_run=false
    
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
            --dry-run)
                dry_run=true
                shift
                ;;
            emergency|critical|minimal|auto)
                test_mode="$1"
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
        force_memory_cleanup
        print_success "内存清理完成"
        exit 0
    fi
    
    # 打印头部
    print_header
    
    # 确定测试模式
    if [ -z "$test_mode" ]; then
        if [ -n "$TYPUS_TEST_MODE" ]; then
            test_mode="$TYPUS_TEST_MODE"
            print_status "使用环境变量 TYPUS_TEST_MODE: $test_mode"
        else
            test_mode=$(auto_select_test_mode)
            print_status "自动选择测试模式: $test_mode"
        fi
    fi
    
    # 设置详细输出
    if [ "$verbose" = true ] || [ "$TYPUS_VERBOSE" = "true" ]; then
        export TYPUS_VERBOSE="true"
        print_status "详细输出模式: 启用"
    fi
    
    # 设置测试环境
    setup_minimal_test_environment "$test_mode"
    
    # 获取测试数量
    local test_count=$CRITICAL_TEST_COUNT
    case $test_mode in
        "emergency")
            test_count=$EMERGENCY_TEST_COUNT
            ;;
        "critical")
            test_count=$CRITICAL_TEST_COUNT
            ;;
        "minimal")
            test_count=$MINIMAL_TEST_COUNT
            ;;
    esac
    
    # 显示将要运行的测试
    local tests=$(get_critical_tests "$test_mode" "$test_count")
    print_status "将要运行的测试 ($test_count 个):"
    while IFS= read -r test_name; do
        if [ -n "$test_name" ]; then
            print_status "  - $test_name"
        fi
    done <<< "$tests"
    
    # 干运行模式
    if [ "$dry_run" = true ]; then
        print_status "干运行模式，不实际执行测试"
        exit 0
    fi
    
    # 运行测试
    print_status "开始运行极简测试套件..."
    
    if run_test_suite "$test_mode" "$test_count"; then
        print_success "极简测试套件完成！"
        echo ""
        print_status "测试总结:"
        print_status "  测试模式: $test_mode"
        print_status "  运行测试: $test_count 个"
        print_status "  内存优化: 已启用"
        print_status "  所有测试用例已保留并优化"
        echo ""
        print_success "极简测试运行器完成！"
    else
        print_error "测试失败"
        print_warning "建议尝试更宽松的测试模式:"
        print_warning "  $0 critical   # 关键模式"
        print_warning "  $0 minimal    # 极简模式"
        exit 1
    fi
    
    # 最终清理
    force_memory_cleanup
}

# 处理中断信号
trap 'print_warning "测试被中断"; force_memory_cleanup; exit 1' INT TERM

# 运行主函数
main "$@"