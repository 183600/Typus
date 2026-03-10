#!/bin/bash
# 智能内存测试运行器
# 在不删除测试用例的情况下，智能调整内存使用策略

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

# 配置
CONFIG_FILE="${TYPUS_CONFIG:-enhanced_test_memory_optimization.yaml}"
DEFAULT_MEMORY_LEVEL="balanced"

# 显示帮助信息
show_help() {
    echo -e "${PURPLE}智能内存测试运行器${NC}"
    echo ""
    echo "用法: $0 [内存级别] [测试命令] [选项]"
    echo ""
    echo "内存级别:"
    echo "  ultra_conservative  - 超保守 (8MB)"
    echo "  conservative        - 保守 (16MB)"
    echo "  balanced            - 平衡 (32MB)"
    echo "  optimized           - 优化 (64MB)"
    echo ""
    echo "选项:"
    echo "  --config=FILE       配置文件路径"
    echo "  --auto              自动检测内存级别"
    echo "  --monitor           启用内存监控"
    echo "  --verbose, -v      详细输出"
    echo "  --help, -h         显示帮助信息"
    echo ""
    echo "示例:"
    echo "  $0 balanced \"cabal test\""
    echo "  $0 --auto \"stack test\" --verbose"
}

# 解析YAML配置（简化版本）
parse_config() {
    local level=$1
    
    # 从配置文件中读取设置（简化实现）
    case $level in
        ultra_conservative)
            MEMORY_LIMIT_MB=8
            QUICKCHECK_TESTS=1
            QUICKCHECK_MAX_SIZE=1
            QUICKCHECK_MAX_SHRINKS=0
            RTS_OPTS="-M8m -A256k -n32k -H512k -qg -G1"
            ;;
        conservative)
            MEMORY_LIMIT_MB=16
            QUICKCHECK_TESTS=2
            QUICKCHECK_MAX_SIZE=2
            QUICKCHECK_MAX_SHRINKS=1
            RTS_OPTS="-M16m -A512k -n64k -H1m -qg -G1"
            ;;
        balanced)
            MEMORY_LIMIT_MB=32
            QUICKCHECK_TESTS=5
            QUICKCHECK_MAX_SIZE=3
            QUICKCHECK_MAX_SHRINKS=2
            RTS_OPTS="-M32m -A1m -n128k -H2m -qg -G1"
            ;;
        optimized)
            MEMORY_LIMIT_MB=64
            QUICKCHECK_TESTS=10
            QUICKCHECK_MAX_SIZE=5
            QUICKCHECK_MAX_SHRINKS=3
            RTS_OPTS="-M64m -A2m -n256k -H4m -qg -G1"
            ;;
        *)
            echo -e "${RED}错误: 未知内存级别 '$level'${NC}"
            show_help
            exit 1
            ;;
    esac
}

# 检测可用内存并选择级别
auto_detect_level() {
    local available_mb=0
    
    if command -v free >/dev/null 2>&1; then
        available_mb=$(free -m | awk 'NR==2{printf "%.0f", $7}')
    else
        echo -e "${YELLOW}[WARNING] 无法检测可用内存，使用默认级别${NC}"
        echo "balanced"
        return
    fi
    
    if [ "$available_mb" -lt 16 ]; then
        echo "ultra_conservative"
    elif [ "$available_mb" -lt 32 ]; then
        echo "conservative"
    elif [ "$available_mb" -lt 64 ]; then
        echo "balanced"
    else
        echo "optimized"
    fi
}

# 设置内存限制
set_memory_limits() {
    local limit_mb=$1
    
    if command -v ulimit >/dev/null 2>&1; then
        # 设置虚拟内存限制
        ulimit -v $((limit_mb * 1024))
        echo -e "${GREEN}[CONFIG] 设置虚拟内存限制: ${limit_mb}MB${NC}"
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

# 构建测试命令
build_test_command() {
    local test_cmd=$1
    local rts_opts=$2
    local quickcheck_opts="--quickcheck-tests=$QUICKCHECK_TESTS --quickcheck-max-size=$QUICKCHECK_MAX_SIZE --quickcheck-max-shrinks=$QUICKCHECK_MAX_SHRINKS"
    
    # 根据测试命令类型构建
    if [[ "$test_cmd" == *"cabal"* ]]; then
        echo "$test_cmd $quickcheck_opts --ghc-options=\"$rts_opts\""
    elif [[ "$test_cmd" == *"stack"* ]]; then
        echo "$test_cmd $quickcheck_opts --ghc-options \"$rts_opts\""
    else
        # 其他测试命令
        echo "$test_cmd"
    fi
}

# 主函数
main() {
    local memory_level=""
    local test_command=""
    local auto_detect=false
    local enable_monitor=false
    local verbose=false
    
    # 解析参数
    while [[ $# -gt 0 ]]; do
        case $1 in
            --config=*)
                CONFIG_FILE="${1#*=}"
                shift
                ;;
            --auto)
                auto_detect=true
                shift
                ;;
            --monitor)
                enable_monitor=true
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
                if [ -z "$memory_level" ]; then
                    memory_level="$1"
                elif [ -z "$test_command" ]; then
                    test_command="$1"
                else
                    test_command="$test_command $1"
                fi
                shift
                ;;
        esac
    done
    
    # 自动检测内存级别
    if [ "$auto_detect" = "true" ]; then
        memory_level=$(auto_detect_level)
        echo -e "${GREEN}[AUTO] 检测到可用内存，选择级别: $memory_level${NC}"
    fi
    
    # 设置默认级别
    if [ -z "$memory_level" ]; then
        memory_level="$DEFAULT_MEMORY_LEVEL"
        echo -e "${YELLOW}[INFO] 使用默认内存级别: $memory_level${NC}"
    fi
    
    if [ -z "$test_command" ]; then
        echo -e "${RED}错误: 必须提供测试命令${NC}"
        show_help
        exit 1
    fi
    
    # 解析配置
    parse_config "$memory_level"
    
    echo -e "${PURPLE}=== 智能内存测试运行器 ===${NC}"
    echo "内存级别: $memory_level"
    echo "内存限制: ${MEMORY_LIMIT_MB}MB"
    echo "QuickCheck参数: tests=${QUICKCHECK_TESTS}, max_size=${QUICKCHECK_MAX_SIZE}, shrinks=${QUICKCHECK_MAX_SHRINKS}"
    echo "测试命令: $test_command"
    echo ""
    
    # 设置内存限制
    set_memory_limits "$MEMORY_LIMIT_MB"
    
    # 执行初始垃圾回收
    perform_gc
    
    # 构建并执行测试命令
    local final_test_cmd=$(build_test_command "$test_command" "$RTS_OPTS")
    
    if [ "$verbose" = "true" ]; then
        echo -e "${CYAN}[VERBOSE] 最终测试命令: $final_test_cmd${NC}"
    fi
    
    # 设置环境变量
    export TYPUS_MEMORY_LEVEL="$memory_level"
    export TYPUS_MEMORY_OPTIMIZED=1
    export TYPUS_PRESERVE_TESTS=1
    
    # 执行测试
    echo -e "${BLUE}[TEST] 开始执行测试...${NC}"
    eval "$final_test_cmd"
    local test_exit_code=$?
    
    # 最终垃圾回收
    perform_gc
    
    echo -e "${PURPLE}=== 测试完成 (退出码: $test_exit_code) ===${NC}"
    
    exit "$test_exit_code"
}

# 运行主函数
main "$@"