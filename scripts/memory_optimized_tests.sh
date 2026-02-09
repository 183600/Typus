#!/bin/bash

# Typus 内存优化测试脚本
# 根据可用内存运行不同级别的内存优化测试

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# 内存级别定义
CRITICAL_MEMORY=64   # 64MB - 极端内存限制
EXTREME_MEMORY=128   # 128MB - 严重内存限制
MINIMAL_MEMORY=256   # 256MB - 最小内存限制
ULTRA_MEMORY=512     # 512MB - 超低内存限制

# 获取可用内存（MB）
get_available_memory() {
    local available_mem
    if command -v free >/dev/null 2>&1; then
        available_mem=$(free -m | awk 'NR==2{printf "%.0f", $7}')
    elif command -v vm_stat >/dev/null 2>&1; then
        # macOS
        available_mem=$(vm_stat | grep "Pages free" | awk '{print $3}' | sed 's/\.//' | awk '{printf "%.0f", $1 * 4096 / 1024 / 1024}')
    else
        available_mem=1024  # 默认值
    fi
    echo $available_mem
}

# 运行测试的函数
run_test() {
    local test_type=$1
    local memory_limit=$2
    local test_command=$3
    
    echo -e "${YELLOW}运行 $test_type 测试 (内存限制: ${memory_limit}MB)${NC}"
    
    # 设置内存限制（如果系统支持）
    if command -v ulimit >/dev/null 2>&1; then
        ulimit -v $((memory_limit * 1024)) 2>/dev/null || true
    fi
    
    # 运行测试
    if eval "$test_command"; then
        echo -e "${GREEN}$test_type 测试通过${NC}"
        return 0
    else
        echo -e "${RED}$test_type 测试失败${NC}"
        return 1
    fi
}

# 主函数
main() {
    local available_mem
    available_mem=$(get_available_memory)
    
    echo -e "${GREEN}可用内存: ${available_mem}MB${NC}"
    
    # 根据可用内存选择测试级别
    if [ $available_mem -lt $CRITICAL_MEMORY ]; then
        echo -e "${RED}内存严重不足，运行关键测试${NC}"
        run_test "关键" $CRITICAL_MEMORY "stack test typus-test-optimized --test-arguments=\"--pattern=Critical\""
    elif [ $available_mem -lt $EXTREME_MEMORY ]; then
        echo -e "${YELLOW}内存受限，运行极端测试${NC}"
        run_test "极端" $EXTREME_MEMORY "stack test typus-test-optimized --test-arguments=\"--pattern=Extreme\""
    elif [ $available_mem -lt $MINIMAL_MEMORY ]; then
        echo -e "${YELLOW}内存有限，运行最小测试${NC}"
        run_test "最小" $MINIMAL_MEMORY "stack test typus-test-optimized --test-arguments=\"--pattern=Minimal\""
    elif [ $available_mem -lt $ULTRA_MEMORY ]; then
        echo -e "${YELLOW}内存较少，运行超低测试${NC}"
        run_test "超低" $ULTRA_MEMORY "stack test typus-test-optimized --test-arguments=\"--pattern=Ultra\""
    else
        echo -e "${GREEN}内存充足，运行优化测试${NC}"
        run_test "优化" 1024 "stack test typus-test-optimized"
    fi
}

# 显示帮助信息
show_help() {
    echo "Typus 内存优化测试脚本"
    echo ""
    echo "用法: $0 [选项]"
    echo ""
    echo "选项:"
    echo "  -c, --critical    强制运行关键测试 (64MB)"
    echo "  -e, --extreme     强制运行极端测试 (128MB)"
    echo "  -m, --minimal     强制运行最小测试 (256MB)"
    echo "  -u, --ultra       强制运行超低测试 (512MB)"
    echo "  -o, --optimized   强制运行优化测试 (无限制)"
    echo "  -h, --help        显示此帮助信息"
    echo ""
    echo "如果不指定选项，脚本将根据可用内存自动选择测试级别。"
}

# 解析命令行参数
case "${1:-}" in
    -c|--critical)
        run_test "关键" $CRITICAL_MEMORY "stack test typus-test-optimized --test-arguments=\"--pattern=Critical\""
        ;;
    -e|--extreme)
        run_test "极端" $EXTREME_MEMORY "stack test typus-test-optimized --test-arguments=\"--pattern=Extreme\""
        ;;
    -m|--minimal)
        run_test "最小" $MINIMAL_MEMORY "stack test typus-test-optimized --test-arguments=\"--pattern=Minimal\""
        ;;
    -u|--ultra)
        run_test "超低" $ULTRA_MEMORY "stack test typus-test-optimized --test-arguments=\"--pattern=Ultra\""
        ;;
    -o|--optimized)
        run_test "优化" 1024 "stack test typus-test-optimized"
        ;;
    -h|--help)
        show_help
        ;;
    "")
        main
        ;;
    *)
        echo "未知选项: $1"
        show_help
        exit 1
        ;;
esac