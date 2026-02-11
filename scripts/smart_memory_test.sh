#!/bin/bash
# 智能内存优化测试选择器
# 这个脚本根据可用内存和测试需求智能选择测试用例

set -e

# 颜色代码
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

# 配置参数
MEMORY_LIMIT=${1:-"auto"}
TEST_TYPE=${2:-"essential"}
VERBOSITY=${3:-"normal"}

# 打印函数
print_header() {
    echo -e "${CYAN}=== Typus 智能内存优化测试选择器 ===${NC}"
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

print_memory_info() {
    echo -e "${CYAN}[MEMORY]${NC} $1"
}

# 检测系统内存
detect_system_memory() {
    if [ -f /proc/meminfo ]; then
        local total_mem=$(grep MemTotal /proc/meminfo | awk '{print $2}')
        local available_mem=$(grep MemAvailable /proc/meminfo | awk '{print $2}')
        local total_mb=$((total_mem / 1024))
        local available_mb=$((available_mem / 1024))
        
        print_memory_info "系统总内存: ${total_mb}MB"
        print_memory_info "可用内存: ${available_mb}MB"
        
        echo $available_mb
    else
        print_warning "无法检测系统内存，使用默认值"
        echo 2048  # 默认2GB
    fi
}

# 推荐内存配置
recommend_memory_config() {
    local available_mem=$1
    local test_type=$2
    
    print_status "根据可用内存(${available_mem}MB)和测试类型(${test_type})推荐配置:"
    
    if [ "$available_mem" -lt 128 ]; then
        print_memory_info "推荐: 极端配置 (32MB)"
        echo "extreme"
    elif [ "$available_mem" -lt 256 ]; then
        print_memory_info "推荐: 最小配置 (64MB)"
        echo "minimal"
    elif [ "$available_mem" -lt 512 ]; then
        print_memory_info "推荐: 标准配置 (128MB)"
        echo "standard"
    elif [ "$available_mem" -lt 1024 ]; then
        print_memory_info "推荐: CI配置 (96MB)"
        echo "ci"
    else
        case $test_type in
            "essential")
                print_memory_info "推荐: 标准配置 (128MB)"
                echo "standard"
                ;;
            "comprehensive")
                print_memory_info "推荐: CI配置 (96MB)"
                echo "ci"
                ;;
            "full")
                print_memory_info "推荐: 平衡配置 (256MB)"
                echo "balanced"
                ;;
            *)
                print_memory_info "推荐: 标准配置 (128MB)"
                echo "standard"
                ;;
        esac
    fi
}

# 设置内存环境
setup_memory_environment() {
    local config=$1
    
    case $config in
        "extreme")
            export GHCRTS="-M32m -A1m -n128k -H4m -qg"
            export TYPUS_MEMORY_LIMIT="extreme"
            export QUICKCHECK_TESTS=2
            export QUICKCHECK_MAX_SIZE=1
            export QUICKCHECK_MAX_SHRINKS=1
            export TYPUS_TEST_SELECTION_RATIO=0.05
            ;;
        "minimal")
            export GHCRTS="-M64m -A2m -n256k -H6m -qg"
            export TYPUS_MEMORY_LIMIT="minimal"
            export QUICKCHECK_TESTS=3
            export QUICKCHECK_MAX_SIZE=1
            export QUICKCHECK_MAX_SHRINKS=2
            export TYPUS_TEST_SELECTION_RATIO=0.1
            ;;
        "standard")
            export GHCRTS="-M128m -A4m -n512k -H12m -qg"
            export TYPUS_MEMORY_LIMIT="standard"
            export QUICKCHECK_TESTS=8
            export QUICKCHECK_MAX_SIZE=2
            export QUICKCHECK_MAX_SHRINKS=5
            export TYPUS_TEST_SELECTION_RATIO=0.2
            ;;
        "ci")
            export GHCRTS="-M96m -A3m -n384k -H8m -qg"
            export TYPUS_MEMORY_LIMIT="ci"
            export QUICKCHECK_TESTS=5
            export QUICKCHECK_MAX_SIZE=1
            export QUICKCHECK_MAX_SHRINKS=3
            export TYPUS_TEST_SELECTION_RATIO=0.15
            ;;
        "balanced")
            export GHCRTS="-M256m -A8m -n1m -H24m -qg"
            export TYPUS_MEMORY_LIMIT="balanced"
            export QUICKCHECK_TESTS=15
            export QUICKCHECK_MAX_SIZE=3
            export QUICKCHECK_MAX_SHRINKS=8
            export TYPUS_TEST_SELECTION_RATIO=0.3
            ;;
        *)
            print_error "未知的内存配置: $config"
            exit 1
            ;;
    esac
    
    # 基本优化
    export GHC_HEAP_ALLOCATION=0.06
    export GHC_GC_YIELD_LIMIT=400
    export TYPUS_SKIP_GO_BUILD=1
    
    print_success "内存环境配置完成"
}

# 选择测试套件
select_test_suite() {
    local test_type=$1
    local config=$2
    
    print_status "选择测试套件: $test_type"
    
    case $test_type in
        "essential")
            TEST_SUITE="MainOptimized"
            TEST_FLAGS="--flags=\"fast\""
            ENV_VARS="MINIMAL_TESTS=true"
            ;;
        "comprehensive")
            TEST_SUITE="MainOptimized"
            TEST_FLAGS="--flags=\"fast\""
            ENV_VARS="CI=true"
            ;;
        "full")
            TEST_SUITE="Main"
            TEST_FLAGS="--flags=\"fast\""
            ENV_VARS=""
            ;;
        "ultra")
            TEST_SUITE="MainOptimized"
            TEST_FLAGS="--flags=\"fast\""
            ENV_VARS="MINIMAL_TESTS=true CI=true"
            ;;
        *)
            print_error "未知的测试类型: $test_type"
            print_status "可用类型: essential, comprehensive, full, ultra"
            exit 1
            ;;
    esac
    
    print_success "测试套件选择完成: $TEST_SUITE"
}

# 构建项目
build_project() {
    local config=$1
    
    print_status "构建项目..."
    
    # 临时取消内存限制进行构建
    unset GHCRTS
    
    # 使用优化构建选项
    if cabal build --flags="fast" --ghc-options="-O0 -j1 -fno-warn-unused-imports" $TEST_SUITE; then
        print_success "构建完成"
    else
        print_error "构建失败"
        exit 1
    fi
    
    # 重新应用内存设置
    setup_memory_environment $config
}

# 运行测试
run_tests() {
    local config=$1
    local test_type=$2
    
    print_status "运行内存优化测试..."
    
    # 设置环境变量
    if [ -n "$ENV_VARS" ]; then
        eval "export $ENV_VARS"
    fi
    
    # 创建内存监控
    TEMP_FILE=$(mktemp)
    
    # 监控内存使用
    (
        while true; do
            if [ -f "/proc/$$/status" ]; then
                MEM_USAGE=$(grep VmRSS /proc/$$/status | awk '{print $2}')
                echo "$(date): ${MEM_USAGE}KB" >> "$TEMP_FILE"
            fi
            sleep 2
        done
    ) &
    MONITOR_PID=$!
    
    # 运行测试
    TEST_SUCCESS=true
    START_TIME=$(date +%s)
    
    if ! cabal test $TEST_FLAGS \
        --test-options="--quickcheck-tests=$QUICKCHECK_TESTS --quickcheck-max-size=$QUICKCHECK_MAX_SIZE --quickcheck-max-shrinks=$QUICKCHECK_MAX_SHRINKS" \
        $TEST_SUITE; then
        TEST_SUCCESS=false
    fi
    
    END_TIME=$(date +%s)
    DURATION=$((END_TIME - START_TIME))
    
    # 停止监控
    kill $MONITOR_PID 2>/dev/null || true
    wait $MONITOR_PID 2>/dev/null || true
    
    # 报告结果
    echo ""
    print_status "=== 测试执行报告 ==="
    print_status "执行时间: ${DURATION}秒"
    
    if [ -f "$TEMP_FILE" ]; then
        MAX_MEM=$(awk '{print $2}' "$TEMP_FILE" | sort -n | tail -1)
        AVG_MEM=$(awk '{sum+=$2} END {print int(sum/NR)}' "$TEMP_FILE")
        print_memory_info "峰值内存: ${MAX_MEM}KB $(($MAX_MEM / 1024))MB"
        print_memory_info "平均内存: ${AVG_MEM}KB $(($AVG_MEM / 1024))MB"
        rm -f "$TEMP_FILE"
    fi
    
    if [ "$TEST_SUCCESS" = true ]; then
        print_success "所有测试通过！"
        return 0
    else
        print_warning "部分测试失败"
        print_warning "建议尝试更高的内存配置:"
        print_warning "  $0 standard $test_type"
        return 1
    fi
}

# 清理
cleanup() {
    print_status "清理资源..."
    
    # 强制垃圾回收
    if command -v ghc >/dev/null 2>&1; then
        echo "import System.Mem; replicateM_ 5 performGC" | ghc -e > /dev/null 2>&1 || true
    fi
    
    # 清理临时文件
    find /tmp -name "typus-*" -type f -mtime +0 -delete 2>/dev/null || true
    
    print_success "清理完成"
}

# 显示帮助
show_help() {
    echo "用法: $0 [内存限制] [测试类型] [详细程度]"
    echo ""
    echo "内存限制选项:"
    echo "  auto        - 自动检测并推荐 (默认)"
    echo "  extreme     - 32MB (极受限环境)"
    echo "  minimal     - 64MB (轻度受限环境)"
    echo "  standard    - 128MB (开发环境)"
    echo "  ci          - 96MB (CI环境)"
    echo "  balanced    - 256MB (平衡环境)"
    echo ""
    echo "测试类型选项:"
    echo "  essential    - 核心测试 (默认)"
    echo "  comprehensive - 综合测试"
    echo "  full        - 完整测试"
    echo "  ultra       - 超级精简测试"
    echo ""
    echo "示例:"
    echo "  $0                    # 自动配置，核心测试"
    echo "  $0 minimal essential  # 最小内存，核心测试"
    echo "  $0 standard full      # 标准内存，完整测试"
}

# 主函数
main() {
    print_header
    
    # 处理帮助参数
    if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
        show_help
        exit 0
    fi
    
    # 检测系统内存
    if [ "$MEMORY_LIMIT" = "auto" ]; then
        AVAILABLE_MEM=$(detect_system_memory)
        MEMORY_LIMIT=$(recommend_memory_config $AVAILABLE_MEM $TEST_TYPE)
    fi
    
    print_status "使用内存配置: $MEMORY_LIMIT"
    print_status "测试类型: $TEST_TYPE"
    
    # 设置内存环境
    setup_memory_environment $MEMORY_LIMIT
    
    # 选择测试套件
    select_test_suite $TEST_TYPE $MEMORY_LIMIT
    
    # 构建项目
    build_project $MEMORY_LIMIT
    
    # 运行测试
    if run_tests $MEMORY_LIMIT $TEST_TYPE; then
        EXIT_CODE=0
    else
        EXIT_CODE=1
    fi
    
    # 清理
    cleanup
    
    print_status "测试运行完成"
    exit $EXIT_CODE
}

# 处理中断
trap 'print_warning "测试运行被中断"; cleanup; exit 1' INT TERM

# 运行主函数
main "$@"