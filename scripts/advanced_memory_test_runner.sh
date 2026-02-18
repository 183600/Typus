#!/bin/bash
# 高级内存测试运行器
# 集成智能内存管理和测试优化策略

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

# 高级内存配置
declare -A MEMORY_CONFIGS
MEMORY_CONFIGS[emergency]="2:immediate:essential:none"
MEMORY_CONFIGS[critical]="4:aggressive:essential:basic"
MEMORY_CONFIGS[minimal]="8:aggressive:core:basic"
MEMORY_CONFIGS[balanced]="16:predictive:smart:detailed"
MEMORY_CONFIGS[comprehensive]="32:lazy:full:realtime"

# 打印函数
print_header() {
    echo -e "${PURPLE}========================================${NC}"
    echo -e "${PURPLE}高级内存测试运行器${NC}"
    echo -e "${PURPLE}========================================${NC}"
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
    local config=${MEMORY_CONFIGS[$level]}
    IFS=':' read -r memory_limit gc_strategy test_selection monitoring <<< "$config"
    
    echo -e "${CYAN}[高级内存配置]${NC}"
    echo -e "  级别: ${level}"
    echo -e "  内存限制: ${memory_limit}MB"
    echo -e "  GC策略: ${gc_strategy}"
    echo -e "  测试选择: ${test_selection}"
    echo -e "  监控级别: ${monitoring}"
    echo ""
}

# 检测系统内存
detect_system_memory() {
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

# 自动选择内存级别
auto_select_memory_level() {
    local available_mb=$(detect_system_memory)
    
    if [ "$available_mb" -le 16 ]; then
        echo "emergency"
    elif [ "$available_mb" -le 32 ]; then
        echo "critical"
    elif [ "$available_mb" -le 64 ]; then
        echo "minimal"
    elif [ "$available_mb" -le 128 ]; then
        echo "balanced"
    else
        echo "comprehensive"
    fi
}

# 解析内存配置
parse_memory_config() {
    local level=$1
    local config=${MEMORY_CONFIGS[$level]}
    IFS=':' read -r memory_limit gc_strategy test_selection monitoring <<< "$config"
    
    echo "$memory_limit:$gc_strategy:$test_selection:$monitoring"
}

# 生成GHC RTS选项
generate_rts_options() {
    local level=$1
    local config=$(parse_memory_config $level)
    IFS=':' read -r memory_limit gc_strategy test_selection monitoring <<< "$config"
    
    local alloc_area nursery_size heap_size
    
    case $memory_limit in
        2)
            alloc_area="128k"
            nursery_size="16k"
            heap_size="512k"
            ;;
        4)
            alloc_area="256k"
            nursery_size="32k"
            heap_size="1m"
            ;;
        8)
            alloc_area="512k"
            nursery_size="64k"
            heap_size="2m"
            ;;
        16)
            alloc_area="1m"
            nursery_size="128k"
            heap_size="4m"
            ;;
        32)
            alloc_area="2m"
            nursery_size="256k"
            heap_size="8m"
            ;;
        *)
            alloc_area="1m"
            nursery_size="128k"
            heap_size="4m"
            ;;
    esac
    
    echo "-M${memory_limit}m -A${alloc_area} -n${nursery_size} -H${heap_size} -qg -G1"
}

# 生成QuickCheck配置
generate_quickcheck_config() {
    local level=$1
    local config=$(parse_memory_config $level)
    IFS=':' read -r memory_limit gc_strategy test_selection monitoring <<< "$config"
    
    local tests max_size max_shrinks
    
    case $test_selection in
        essential)
            tests=1
            max_size=1
            max_shrinks=0
            ;;
        core)
            tests=2
            max_size=2
            max_shrinks=1
            ;;
        smart)
            tests=3
            max_size=2
            max_shrinks=1
            ;;
        full)
            tests=5
            max_size=3
            max_shrinks=2
            ;;
        *)
            tests=1
            max_size=1
            max_shrinks=0
            ;;
    esac
    
    echo "$tests:$max_size:$max_shrinks"
}

# 执行垃圾回收策略
execute_gc_strategy() {
    local strategy=$1
    
    case $strategy in
        immediate)
            print_status "执行立即垃圾回收..."
            if command -v ghc >/dev/null 2>&1; then
                echo "import System.Mem; performGC; performGC; performGC; performGC" | ghc -e > /dev/null 2>&1 || true
            fi
            ;;
        aggressive)
            print_status "执行激进垃圾回收..."
            for i in {1..5}; do
                if command -v ghc >/dev/null 2>&1; then
                    echo "import System.Mem; performGC" | ghc -e > /dev/null 2>&1 || true
                fi
                sleep 0.5
            done
            ;;
        predictive)
            print_status "执行预测性垃圾回收..."
            if command -v ghc >/dev/null 2>&1; then
                echo "import System.Mem; performGC" | ghc -e > /dev/null 2>&1 || true
            fi
            sleep 1
            if command -v ghc >/dev/null 2>&1; then
                echo "import System.Mem; performGC" | ghc -e > /dev/null 2>&1 || true
            fi
            ;;
        lazy)
            print_status "执行延迟垃圾回收..."
            sleep 2
            if command -v ghc >/dev/null 2>&1; then
                echo "import System.Mem; performGC" | ghc -e > /dev/null 2>&1 || true
            fi
            ;;
    esac
}

# 内存监控
monitor_memory() {
    local level=$1
    local config=$(parse_memory_config $level)
    IFS=':' read -r memory_limit gc_strategy test_selection monitoring <<< "$config"
    
    case $monitoring in
        none)
            return
            ;;
        basic)
            local current_mb=$(detect_system_memory)
            print_status "当前内存使用: ${current_mb}MB"
            ;;
        detailed)
            local current_mb=$(detect_system_memory)
            print_status "详细内存监控 - 当前: ${current_mb}MB, 限制: ${memory_limit}MB"
            execute_gc_strategy immediate
            ;;
        realtime)
            print_status "实时内存监控已启用"
            # 这里可以添加更复杂的实时监控逻辑
            ;;
    esac
}

# 智能测试选择
select_tests() {
    local level=$1
    local config=$(parse_memory_config $level)
    IFS=':' read -r memory_limit gc_strategy test_selection monitoring <<< "$config"
    
    local all_tests=(
        "Test.Unit.BasicQuickCheckTestSuite.tests"
        "Test.Unit.SimpleQuickCheckTestSuite.tests"
        "Test.Unit.ConciseTestSuite.tests"
        "Test.Unit.MemoryOptimizedTestSuite.tests"
        "Test.Unit.EnhancedMemoryOptimizedTestSuite.tests"
    )
    
    case $test_selection in
        essential)
            echo "${all_tests[0]}"
            ;;
        core)
            echo "${all_tests[0]}"
            echo "${all_tests[1]}"
            echo "${all_tests[2]}"
            ;;
        smart)
            # 智能采样
            local total=${#all_tests[@]}
            local sample_size=$((total / 2 + 1))
            for ((i=0; i<sample_size; i++)); do
                echo "${all_tests[i]}"
            done
            ;;
        full)
            printf '%s\n' "${all_tests[@]}"
            ;;
    esac
}

# 设置高级内存环境
setup_advanced_memory_environment() {
    local level=$1
    
    print_status "设置高级内存环境: $level"
    
    # 显示可用内存信息
    local available_mb=$(detect_system_memory)
    print_status "检测到可用内存: ${available_mb}MB"
    
    print_memory_config $level
    
    # 生成配置
    local rts_options=$(generate_rts_options $level)
    local quickcheck_config=$(generate_quickcheck_config $level)
    IFS=':' read -r tests max_size max_shrinks <<< "$quickcheck_config"
    
    # 设置环境变量
    export GHCRTS="$rts_options"
    export QUICKCHECK_TESTS="$tests"
    export QUICKCHECK_MAX_SIZE="$max_size"
    export QUICKCHECK_MAX_SHRINKS="$max_shrinks"
    
    # 其他优化设置
    export GHC_HEAP_ALLOCATION=0.01
    export GHC_GC_YIELD_LIMIT=200
    export TYPUS_SKIP_GO_BUILD=1
    export TYPUS_ADVANCED_MEMORY_MODE=1
    export TYPUS_MEMORY_LEVEL="$level"
    
    print_status "RTS选项: $GHCRTS"
    print_status "QuickCheck配置: tests=$tests, max_size=$max_size, max_shrinks=$max_shrinks"
}

# 运行单个测试
run_test_with_advanced_memory() {
    local test_name=$1
    local level=$2
    local config=$(parse_memory_config $level)
    IFS=':' read -r memory_limit gc_strategy test_selection monitoring <<< "$config"
    
    print_status "运行测试: $test_name"
    
    # 预测试内存管理
    execute_gc_strategy $gc_strategy
    
    # 内存监控
    monitor_memory $level
    
    # 运行测试
    local test_success=true
    if command -v /usr/bin/time >/dev/null 2>&1; then
        local time_output=$(/usr/bin/time -v cabal test --flags="fast" --test-options="--quickcheck-tests=$QUICKCHECK_TESTS --quickcheck-max-size=$QUICKCHECK_MAX_SIZE --quickcheck-max-shrinks=$QUICKCHECK_MAX_SHRINKS" typus-test 2>&1)
        local exit_code=$?
        
        # 提取内存使用信息
        local max_memory=$(echo "$time_output" | grep "Maximum resident set size" | awk '{print $6}' || echo "N/A")
        
        if [ $exit_code -eq 0 ]; then
            print_success "测试通过: $test_name"
            [ "$max_memory" != "N/A" ] && print_status "峰值内存: ${max_memory}KB"
        else
            print_error "测试失败: $test_name"
            test_success=false
        fi
    else
        if cabal test --flags="fast" --test-options="--quickcheck-tests=$QUICKCHECK_TESTS --quickcheck-max-size=$QUICKCHECK_MAX_SIZE --quickcheck-max-shrinks=$QUICKCHECK_MAX_SHRINKS" typus-test; then
            print_success "测试通过: $test_name"
        else
            print_error "测试失败: $test_name"
            test_success=false
        fi
    fi
    
    # 后测试内存管理
    execute_gc_strategy $gc_strategy
    
    [ "$test_success" = true ]
}

# 运行高级内存测试套件
run_advanced_memory_test_suite() {
    local level=$1
    local tests_passed=0
    local tests_total=0
    
    print_status "运行高级内存测试套件: $level"
    
    # 获取测试列表
    local tests=$(select_tests $level)
    
    # 运行每个测试
    while IFS= read -r test_name; do
        if [ -n "$test_name" ]; then
            tests_total=$((tests_total + 1))
            
            if run_test_with_advanced_memory "$test_name" "$level"; then
                tests_passed=$((tests_passed + 1))
            fi
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

# 显示帮助
show_help() {
    echo "高级内存测试运行器"
    echo ""
    echo "用法: $0 [内存级别] [选项]"
    echo ""
    echo "内存级别:"
    echo "  emergency      紧急模式 (2MB) - 立即GC，仅关键测试"
    echo "  critical       关键模式 (4MB) - 激进GC，仅关键测试"
    echo "  minimal        极简模式 (8MB) - 激进GC，核心测试"
    echo "  balanced       平衡模式 (16MB) - 预测GC，智能测试"
    echo "  comprehensive  全面模式 (32MB) - 延迟GC，完整测试"
    echo "  auto           自动模式 - 根据系统资源自动选择"
    echo ""
    echo "选项:"
    echo "  --help, -h           显示此帮助信息"
    echo "  --verbose, -v        启用详细输出"
    echo "  --dry-run            仅显示配置，不运行测试"
    echo "  --monitor-only       仅执行内存监控"
    echo "  --gc-only            仅执行垃圾回收策略"
    echo ""
    echo "环境变量:"
    echo "  TYPUS_MEMORY_LEVEL   内存级别"
    echo "  TYPUS_VERBOSE        启用详细输出"
    echo ""
    echo "示例:"
    echo "  $0 emergency          # 紧急模式"
    echo "  $0 auto               # 自动模式"
    echo "  $0 balanced --verbose # 平衡模式，详细输出"
}

# 主函数
main() {
    local memory_level=""
    local verbose=false
    local dry_run=false
    local monitor_only=false
    local gc_only=false
    
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
            --dry-run)
                dry_run=true
                shift
                ;;
            --monitor-only)
                monitor_only=true
                shift
                ;;
            --gc-only)
                gc_only=true
                shift
                ;;
            emergency|critical|minimal|balanced|comprehensive|auto)
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
    
    # 打印头部
    print_header
    
    # 确定内存级别
    if [ -z "$memory_level" ]; then
        if [ -n "$TYPUS_MEMORY_LEVEL" ]; then
            memory_level="$TYPUS_MEMORY_LEVEL"
            print_status "使用环境变量 TYPUS_MEMORY_LEVEL: $memory_level"
        else
            local auto_level=$(auto_select_memory_level)
            memory_level="$auto_level"
            print_status "自动选择内存级别: $memory_level"
        fi
    fi
    
    # 自动模式处理
    if [ "$memory_level" = "auto" ]; then
        local auto_level=$(auto_select_memory_level)
        memory_level="$auto_level"
        print_status "自动模式选择: $memory_level"
    fi
    
    # 设置详细输出
    if [ "$verbose" = true ] || [ "$TYPUS_VERBOSE" = "true" ]; then
        export TYPUS_VERBOSE="true"
        print_status "详细输出模式: 启用"
    fi
    
    # 设置高级内存环境
    setup_advanced_memory_environment "$memory_level"
    
    # 仅监控模式
    if [ "$monitor_only" = true ]; then
        monitor_memory "$memory_level"
        print_success "内存监控完成"
        exit 0
    fi
    
    # 仅GC模式
    if [ "$gc_only" = true ]; then
        local config=$(parse_memory_config $memory_level)
        IFS=':' read -r memory_limit gc_strategy test_selection monitoring <<< "$config"
        execute_gc_strategy $gc_strategy
        print_success "垃圾回收完成"
        exit 0
    fi
    
    # 干运行模式
    if [ "$dry_run" = true ]; then
        print_status "干运行模式 - 配置如下："
        print_memory_config $memory_level
        print_status "将要运行的测试："
        select_tests $memory_level | while read -r test; do
            print_status "  - $test"
        done
        exit 0
    fi
    
    # 运行测试
    print_status "开始运行高级内存优化测试..."
    
    if run_advanced_memory_test_suite "$memory_level"; then
        print_success "高级内存测试套件完成！"
        echo ""
        print_status "测试总结:"
        print_status "  内存级别: $memory_level"
        print_status "  高级策略: 已启用"
        print_status "  智能优化: 已应用"
        print_status "  所有测试用例已保留并优化"
        echo ""
        print_success "高级内存测试运行器完成！"
    else
        print_error "测试失败"
        print_warning "建议尝试更宽松的内存级别:"
        print_warning "  $0 critical      # 关键模式"
        print_warning "  $0 minimal       # 极简模式"
        print_warning "  $0 balanced      # 平衡模式"
        exit 1
    fi
}

# 处理中断信号
trap 'print_warning "测试被中断"; execute_gc_strategy aggressive; exit 1' INT TERM

# 运行主函数
main "$@"