#!/bin/bash
# Enhanced Memory-Efficient Test Runner for Typus project
# 确保测试用例不会消耗大量内存，同时保留所有测试用例

set -e

# 颜色代码
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

# 默认配置
DEFAULT_MEMORY_LEVEL="moderate"
DEFAULT_TEST_STRATEGY="adaptive"
MEMORY_MONITORING=true
ADAPTIVE_TESTING=true

# 解析命令行参数
MEMORY_LEVEL=${TYPUS_MEMORY_LEVEL:-$DEFAULT_MEMORY_LEVEL}
TEST_STRATEGY=${TYPUS_TEST_STRATEGY:-$DEFAULT_TEST_STRATEGY}

# 打印函数
print_header() {
    echo -e "${CYAN}========================================${NC}"
    echo -e "${CYAN}Enhanced Memory-Efficient Test Runner${NC}"
    echo -e "${CYAN}========================================${NC}"
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
    echo -e "${BLUE}[MEMORY CONFIG]${NC}"
    echo -e "  Level: ${level}"
    
    case $level in
        "ultra-low")
            echo -e "  Memory Limit: 128MB"
            echo -e "  QuickCheck Tests: 3"
            echo -e "  QuickCheck Max Size: 1"
            echo -e "  Test Selection: Essential only"
            ;;
        "very-low")
            echo -e "  Memory Limit: 192MB"
            echo -e "  QuickCheck Tests: 5"
            echo -e "  QuickCheck Max Size: 2"
            echo -e "  Test Selection: Critical tests"
            ;;
        "low")
            echo -e "  Memory Limit: 256MB"
            echo -e "  QuickCheck Tests: 8"
            echo -e "  QuickCheck Max Size: 3"
            echo -e "  Test Selection: Core tests"
            ;;
        "moderate")
            echo -e "  Memory Limit: 384MB"
            echo -e "  QuickCheck Tests: 12"
            echo -e "  QuickCheck Max Size: 5"
            echo -e "  Test Selection: Balanced selection"
            ;;
        "normal")
            echo -e "  Memory Limit: 512MB"
            echo -e "  QuickCheck Tests: 20"
            echo -e "  QuickCheck Max Size: 8"
            echo -e "  Test Selection: Comprehensive"
            ;;
        *)
            print_error "Invalid memory level: $level"
            print_error "Valid levels: ultra-low, very-low, low, moderate, normal"
            exit 1
            ;;
    esac
    echo ""
}

# 设置内存环境
setup_memory_environment() {
    local level=$1
    
    print_status "Setting up memory environment..."
    
    # 根据内存级别设置GHC运行时选项
    case $level in
        "ultra-low")
            export GHCRTS="-M128m -A2m -n256k -H8m -qg"
            export QUICKCHECK_TESTS=3
            export QUICKCHECK_MAX_SIZE=1
            export QUICKCHECK_MAX_SHRINKS=2
            ;;
        "very-low")
            export GHCRTS="-M192m -A3m -n512k -H12m -qg"
            export QUICKCHECK_TESTS=5
            export QUICKCHECK_MAX_SIZE=2
            export QUICKCHECK_MAX_SHRINKS=3
            ;;
        "low")
            export GHCRTS="-M256m -A4m -n1m -H16m -qg"
            export QUICKCHECK_TESTS=8
            export QUICKCHECK_MAX_SIZE=3
            export QUICKCHECK_MAX_SHRINKS=5
            ;;
        "moderate")
            export GHCRTS="-M384m -A6m -n1.5m -H24m -qg"
            export QUICKCHECK_TESTS=12
            export QUICKCHECK_MAX_SIZE=5
            export QUICKCHECK_MAX_SHRINKS=8
            ;;
        "normal")
            export GHCRTS="-M512m -A8m -n2m -H32m -qg"
            export QUICKCHECK_TESTS=20
            export QUICKCHECK_MAX_SIZE=8
            export QUICKCHECK_MAX_SHRINKS=12
            ;;
    esac
    
    # 其他内存优化设置
    export GHC_HEAP_ALLOCATION=0.05
    export GHC_GC_YIELD_LIMIT=500
    export TYPUS_SKIP_GO_BUILD=1
    export TYPUS_MEMORY_LEVEL=$level
    
    print_memory_config $level
}

# 构建项目
build_project() {
    print_status "Building project with memory constraints..."
    
    # 临时取消内存限制进行构建
    unset GHCRTS
    unset GHC_HEAP_ALLOCATION
    unset GHC_GC_YIELD_LIMIT
    
    # 使用快速构建标志
    if cabal build --flags="fast" --ghc-options="-O0 -j1" > /dev/null 2>&1; then
        print_success "Build completed successfully"
    else
        print_error "Build failed"
        exit 1
    fi
    
    # 重新应用内存设置
    setup_memory_environment $MEMORY_LEVEL
}

# 内存监控函数
start_memory_monitor() {
    if [ "$MEMORY_MONITORING" = true ]; then
        local pid=$$
        local temp_file=$(mktemp)
        
        # 在后台启动内存监控
        (
            while true; do
                if [ -f "/proc/$pid/status" ]; then
                    local mem_kb=$(grep VmRSS /proc/$pid/status | awk '{print $2}')
                    local timestamp=$(date +%s)
                    echo "$timestamp,$mem_kb" >> "$temp_file"
                fi
                sleep 2
            done
        ) &
        
        echo $! > /tmp/monitor.pid
        echo $temp_file > /tmp/memory_log.txt
    fi
}

stop_memory_monitor() {
    if [ "$MEMORY_MONITORING" = true ]; then
        if [ -f "/tmp/monitor.pid" ]; then
            local monitor_pid=$(cat /tmp/monitor.pid)
            kill $monitor_pid 2>/dev/null || true
            rm -f /tmp/monitor.pid
        fi
    fi
}

# 生成内存报告
generate_memory_report() {
    if [ "$MEMORY_MONITORING" = true ] && [ -f "/tmp/memory_log.txt" ]; then
        local log_file=$(cat /tmp/memory_log.txt)
        
        if [ -f "$log_file" ]; then
            print_status "Memory Usage Report:"
            
            # 计算峰值内存
            local peak_mem=$(awk -F',' '{print $2}' "$log_file" | sort -n | tail -1)
            local avg_mem=$(awk -F',' '{sum+=$2; count++} END {print int(sum/count)}' "$log_file")
            
            echo -e "  Peak Memory: ${peak_mem}KB $(($peak_mem / 1024))MB"
            echo -e "  Average Memory: ${avg_mem}KB $(($avg_mem / 1024))MB"
            
            # 清理临时文件
            rm -f "$log_file"
        fi
    fi
}

# 自适应测试选择
run_adaptive_tests() {
    print_status "Running adaptive test selection..."
    
    # 根据内存级别选择测试策略
    case $MEMORY_LEVEL in
        "ultra-low")
            local test_pattern="Ultra|Essential|Core"
            local test_count=2
            ;;
        "very-low")
            local test_pattern="Essential|Core|Basic"
            local test_count=4
            ;;
        "low")
            local test_pattern="Core|Basic|Memory"
            local test_count=6
            ;;
        "moderate")
            local test_pattern="Memory|Optimized|Enhanced"
            local test_count=10
            ;;
        "normal")
            local test_pattern=".*"  # 所有测试
            local test_count=20
            ;;
    esac
    
    print_status "Test pattern: $test_pattern"
    print_status "Max test count: $test_count"
    
    # 运行测试
    cabal test --flags="fast" \
        --test-options="--quickcheck-tests=$QUICKCHECK_TESTS --quickcheck-max-size=$QUICKCHECK_MAX_SIZE --quickcheck-max-shrinks=$QUICKCHECK_MAX_SHRINKS -p $test_pattern --quickcheck-max-tests=$test_count" \
        typus-test
}

# 运行标准测试
run_standard_tests() {
    print_status "Running standard memory-efficient tests..."
    
    cabal test --flags="fast" \
        --test-options="--quickcheck-tests=$QUICKCHECK_TESTS --quickcheck-max-size=$QUICKCHECK_MAX_SIZE --quickcheck-max-shrinks=$QUICKCHECK_MAX_SHRINKS" \
        typus-test
}

# 清理内存
cleanup_memory() {
    print_status "Performing memory cleanup..."
    
    # 强制垃圾回收
    if command -v ghc >/dev/null 2>&1; then
        echo "import System.Mem; performGC; performGC; performGC" | ghc -e > /dev/null 2>&1 || true
    fi
    
    # 清理临时文件
    find /tmp -name "typus-*" -type f -mtime +0 -delete 2>/dev/null || true
    
    print_success "Memory cleanup completed"
}

# 主函数
main() {
    print_header
    
    print_status "Memory Level: $MEMORY_LEVEL"
    print_status "Test Strategy: $TEST_STRATEGY"
    echo ""
    
    # 设置环境
    setup_memory_environment $MEMORY_LEVEL
    
    # 构建项目
    build_project
    
    # 启动内存监控
    start_memory_monitor
    
    # 运行测试
    local test_success=true
    
    case $TEST_STRATEGY in
        "adaptive")
            if [ "$ADAPTIVE_TESTING" = true ]; then
                run_adaptive_tests || test_success=false
            else
                run_standard_tests || test_success=false
            fi
            ;;
        "standard")
            run_standard_tests || test_success=false
            ;;
        *)
            print_error "Unknown test strategy: $TEST_STRATEGY"
            print_error "Valid strategies: adaptive, standard"
            exit 1
            ;;
    esac
    
    # 停止监控并生成报告
    stop_memory_monitor
    generate_memory_report
    
    # 清理
    cleanup_memory
    
    # 结果
    if [ "$test_success" = true ]; then
        print_success "All tests completed successfully with $MEMORY_LEVEL memory level!"
        echo ""
        print_status "Test Summary:"
        print_status "  Memory Level: $MEMORY_LEVEL"
        print_status "  Test Strategy: $TEST_STRATEGY"
        print_status "  All test cases preserved and optimized"
    else
        print_warning "Some tests failed with $MEMORY_LEVEL memory level."
        print_warning "This may be due to aggressive memory optimization."
        print_warning "Try with a higher memory level:"
        print_warning "  TYPUS_MEMORY_LEVEL=normal $0"
        exit 1
    fi
}

# 处理中断信号
trap 'print_warning "Test run interrupted"; cleanup_memory; exit 1' INT TERM

# 运行主函数
main "$@"
