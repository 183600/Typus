#!/bin/bash
# 自适应内存测试优化器
# 在不删除测试用例的情况下，动态调整内存使用策略

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

# 内存监控配置
MEMORY_CHECK_INTERVAL=5  # 检查间隔（秒）
HIGH_MEMORY_THRESHOLD=0.8  # 高内存使用阈值（80%）
CRITICAL_MEMORY_THRESHOLD=0.9  # 关键内存使用阈值（90%）

# 显示帮助信息
show_help() {
    echo -e "${PURPLE}自适应内存测试优化器${NC}"
    echo ""
    echo "用法: $0 [测试命令] [选项]"
    echo ""
    echo "选项:"
    echo "  --monitor-interval=N   内存监控间隔（秒，默认：5）"
    echo "  --high-threshold=N     高内存阈值（0-1，默认：0.8）"
    echo "  --critical-threshold=N 关键内存阈值（0-1，默认：0.9）"
    echo "  --verbose, -v         详细输出"
    echo "  --help, -h            显示帮助信息"
    echo ""
    echo "示例:"
    echo "  $0 \"cabal test\" --monitor-interval=10"
    echo "  $0 \"stack test\" --verbose"
}

# 获取内存使用率（百分比）
get_memory_usage() {
    if command -v free >/dev/null 2>&1; then
        local total_mem=$(free -m | awk 'NR==2{print $2}')
        local used_mem=$(free -m | awk 'NR==2{print $3}')
        echo "scale=2; $used_mem / $total_mem" | bc
    else
        echo "0.0"
    fi
}

# 执行垃圾回收
perform_gc() {
    echo -e "${YELLOW}[GC] 执行垃圾回收...${NC}"
    if command -v ghc >/dev/null 2>&1; then
        ghc -e "import System.Mem; performGC" 2>/dev/null || true
    fi
    sleep 1
}

# 调整测试参数
adjust_test_params() {
    local memory_usage=$1
    local verbose=$2
    
    if (( $(echo "$memory_usage > $CRITICAL_MEMORY_THRESHOLD" | bc -l) )); then
        echo -e "${RED}[CRITICAL] 内存使用率过高 (${memory_usage}%)，采取紧急措施${NC}"
        
        # 紧急措施
        export QUICKCHECK_TESTS=1
        export QUICKCHECK_MAX_SIZE=1
        export QUICKCHECK_MAX_SHRINKS=0
        
        # 强制GC
        perform_gc
        
        if [ "$verbose" = "true" ]; then
            echo -e "${RED}启用了紧急测试参数：${NC}"
            echo "  QUICKCHECK_TESTS=1"
            echo "  QUICKCHECK_MAX_SIZE=1"
            echo "  QUICKCHECK_MAX_SHRINKS=0"
        fi
        
    elif (( $(echo "$memory_usage > $HIGH_MEMORY_THRESHOLD" | bc -l) )); then
        echo -e "${YELLOW}[WARNING] 内存使用率较高 (${memory_usage}%)，调整测试参数${NC}"
        
        # 高内存使用调整
        export QUICKCHECK_TESTS=2
        export QUICKCHECK_MAX_SIZE=2
        export QUICKCHECK_MAX_SHRINKS=1
        
        # 执行GC
        perform_gc
        
        if [ "$verbose" = "true" ]; then
            echo -e "${YELLOW}启用了保守测试参数：${NC}"
            echo "  QUICKCHECK_TESTS=2"
            echo "  QUICKCHECK_MAX_SIZE=2"
            echo "  QUICKCHECK_MAX_SHRINKS=1"
        fi
        
    else
        # 正常内存使用，使用默认参数
        export QUICKCHECK_TESTS=10
        export QUICKCHECK_MAX_SIZE=10
        export QUICKCHECK_MAX_SHRINKS=5
        
        if [ "$verbose" = "true" ]; then
            echo -e "${GREEN}[INFO] 内存使用正常 (${memory_usage}%)，使用标准测试参数${NC}"
        fi
    fi
}

# 内存监控进程
monitor_memory() {
    local test_pid=$1
    local interval=$2
    local verbose=$3
    
    echo -e "${BLUE}[MONITOR] 启动内存监控 (PID: $test_pid, 间隔: ${interval}s)${NC}"
    
    while kill -0 "$test_pid" 2>/dev/null; do
        local memory_usage=$(get_memory_usage)
        
        if [ "$verbose" = "true" ]; then
            echo -e "${CYAN}[MONITOR] 当前内存使用率: ${memory_usage}%${NC}"
        fi
        
        # 调整测试参数
        adjust_test_params "$memory_usage" "$verbose"
        
        sleep "$interval"
    done
    
    echo -e "${GREEN}[MONITOR] 测试进程已结束，停止内存监控${NC}"
}

# 主函数
main() {
    local test_command=""
    local monitor_interval=$MEMORY_CHECK_INTERVAL
    local verbose=false
    
    # 解析参数
    while [[ $# -gt 0 ]]; do
        case $1 in
            --monitor-interval=*)
                monitor_interval="${1#*=}"
                shift
                ;;
            --high-threshold=*)
                HIGH_MEMORY_THRESHOLD="${1#*=}"
                shift
                ;;
            --critical-threshold=*)
                CRITICAL_MEMORY_THRESHOLD="${1#*=}"
                shift
                ;;
            --verbose|-v)
                verbose=true
                shift
                ;;
            --help|-h)
                show_help
                exit 0
                ;;
            -*)
                echo -e "${RED}错误: 未知选项 $1${NC}"
                show_help
                exit 1
                ;;
            *)
                if [ -z "$test_command" ]; then
                    test_command="$1"
                else
                    test_command="$test_command $1"
                fi
                shift
                ;;
        esac
    done
    
    if [ -z "$test_command" ]; then
        echo -e "${RED}错误: 必须提供测试命令${NC}"
        show_help
        exit 1
    fi
    
    echo -e "${PURPLE}=== 自适应内存测试优化器 ===${NC}"
    echo "测试命令: $test_command"
    echo "监控间隔: ${monitor_interval}秒"
    echo "高内存阈值: ${HIGH_MEMORY_THRESHOLD}"
    echo "关键内存阈值: ${CRITICAL_MEMORY_THRESHOLD}"
    echo ""
    
    # 设置初始测试参数
    export QUICKCHECK_TESTS=10
    export QUICKCHECK_MAX_SIZE=10
    export QUICKCHECK_MAX_SHRINKS=5
    
    # 启动测试命令
    eval "$test_command" &
    local test_pid=$!
    
    # 启动内存监控
    monitor_memory "$test_pid" "$monitor_interval" "$verbose" &
    local monitor_pid=$!
    
    # 等待测试完成
    wait "$test_pid"
    local test_exit_code=$?
    
    # 停止监控
    kill "$monitor_pid" 2>/dev/null || true
    
    # 最终垃圾回收
    perform_gc
    
    echo -e "${PURPLE}=== 测试完成 (退出码: $test_exit_code) ===${NC}"
    
    exit "$test_exit_code"
}

# 运行主函数
main "$@"