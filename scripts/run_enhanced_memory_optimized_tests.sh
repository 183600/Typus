#!/bin/bash

# 增强内存优化测试脚本
# 集成所有内存优化功能，提供全面的内存优化测试解决方案

set -e

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# 内存级别定义
ULTRA_EXTREME_MEMORY=16   # 16MB - 超极端内存限制
CRITICAL_MEMORY=24        # 24MB - 关键内存限制
EMERGENCY_MEMORY=32       # 32MB - 紧急内存限制
MINIMAL_MEMORY=48         # 48MB - 最小内存限制
CI_MEMORY=64              # 64MB - CI/CD内存限制
DEVELOPMENT_MEMORY=128    # 128MB - 开发环境内存限制
COMPREHENSIVE_MEMORY=256  # 256MB - 综合环境内存限制

# 获取可用内存（MB）
get_available_memory() {
    local available_mem
    if command -v free >/dev/null 2>&1; then
        available_mem=$(free -m | awk 'NR==2{printf "%.0f", $7}')
    elif command -v vm_stat >/dev/null 2>&1; then
        # macOS
        available_mem=$(vm_stat | grep "Pages free" | awk '{print $3}' | sed 's/\.//' | awk '{printf "%.0f", $1 * 4096 / 1024 / 1024}')
    else
        available_mem=512  # 默认值
    fi
    echo $available_mem
}

# 获取系统总内存（MB）
get_total_memory() {
    local total_mem
    if command -v free >/dev/null 2>&1; then
        total_mem=$(free -m | awk 'NR==2{printf "%.0f", $2}')
    elif command -v vm_stat >/dev/null 2>&1; then
        # macOS
        total_mem=$(sysctl -n hw.memsize | awk '{printf "%.0f", $1 / 1024 / 1024}')
    else
        total_mem=1024  # 默认值
    fi
    echo $total_mem
}

# 检测内存压力
detect_memory_pressure() {
    local available_mem=$(get_available_memory)
    local total_mem=$(get_total_memory)
    local pressure=$(echo "scale=2; 1 - ($available_mem / $total_mem)" | bc -l)
    echo $pressure
}

# 运行测试的函数
run_enhanced_test() {
    local test_type=$1
    local memory_limit=$2
    local test_command=$3
    local cleanup_strategy=$4
    
    echo -e "${CYAN}运行 $test_type 测试${NC}"
    echo -e "${BLUE}内存限制: ${memory_limit}MB${NC}"
    echo -e "${BLUE}清理策略: $cleanup_strategy${NC}"
    echo -e "${BLUE}命令: $test_command${NC}"
    
    # 设置内存限制（如果系统支持）
    if command -v ulimit >/dev/null 2>&1; then
        ulimit -v $((memory_limit * 1024)) 2>/dev/null || true
    fi
    
    # 设置环境变量
    export MEMORY_LIMIT_MB=$memory_limit
    export CLEANUP_STRATEGY=$cleanup_strategy
    
    # 运行测试
    local start_time=$(date +%s)
    if eval "$test_command"; then
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))
        echo -e "${GREEN}$test_type 测试通过 (耗时: ${duration}秒)${NC}"
        return 0
    else
        echo -e "${RED}$test_type 测试失败${NC}"
        return 1
    fi
}

# 运行增强内存优化测试
run_enhanced_memory_tests() {
    local available_mem=$(get_available_memory)
    local total_mem=$(get_total_memory)
    local pressure=$(detect_memory_pressure)
    
    echo -e "${GREEN}=== 增强内存优化测试系统 ===${NC}"
    echo -e "${GREEN}可用内存: ${available_mem}MB${NC}"
    echo -e "${GREEN}总内存: ${total_mem}MB${NC}"
    echo -e "${GREEN}内存压力: ${pressure}${NC}"
    echo ""
    
    # 根据可用内存和压力选择测试级别
    if [ $available_mem -lt $ULTRA_EXTREME_MEMORY ]; then
        echo -e "${RED}内存严重不足，运行超极端测试${NC}"
        run_enhanced_test "超极端" $ULTRA_EXTREME_MEMORY \
            "stack test EnhancedMemoryTestRunner --test-arguments=\"ultra\"" \
            "emergency"
    elif [ $available_mem -lt $CRITICAL_MEMORY ]; then
        echo -e "${RED}内存极度受限，运行关键测试${NC}"
        run_enhanced_test "关键" $CRITICAL_MEMORY \
            "stack test EnhancedMemoryTestRunner --test-arguments=\"critical\"" \
            "aggressive"
    elif [ $available_mem -lt $EMERGENCY_MEMORY ]; then
        echo -e "${YELLOW}内存受限，运行紧急测试${NC}"
        run_enhanced_test "紧急" $EMERGENCY_MEMORY \
            "stack test EnhancedMemoryTestRunner --test-arguments=\"emergency\"" \
            "aggressive"
    elif [ $available_mem -lt $MINIMAL_MEMORY ]; then
        echo -e "${YELLOW}内存有限，运行最小测试${NC}"
        run_enhanced_test "最小" $MINIMAL_MEMORY \
            "stack test EnhancedMemoryTestRunner --test-arguments=\"minimal\"" \
            "standard"
    elif [ $available_mem -lt $CI_MEMORY ]; then
        echo -e "${YELLOW}内存较少，运行CI测试${NC}"
        run_enhanced_test "CI" $CI_MEMORY \
            "stack test EnhancedMemoryTestRunner --test-arguments=\"ci\"" \
            "standard"
    elif [ $available_mem -lt $DEVELOPMENT_MEMORY ]; then
        echo -e "${GREEN}内存适中，运行开发测试${NC}"
        run_enhanced_test "开发" $DEVELOPMENT_MEMORY \
            "stack test EnhancedMemoryTestRunner --test-arguments=\"development\"" \
            "light"
    else
        echo -e "${GREEN}内存充足，运行综合测试${NC}"
        run_enhanced_test "综合" $COMPREHENSIVE_MEMORY \
            "stack test EnhancedMemoryTestRunner --test-arguments=\"comprehensive\"" \
            "light"
    fi
}

# 运行特定类型的测试
run_specific_test() {
    local test_type=$1
    local memory_limit
    local cleanup_strategy
    local test_command
    
    case $test_type in
        "ultra")
            memory_limit=$ULTRA_EXTREME_MEMORY
            cleanup_strategy="emergency"
            test_command="stack test EnhancedMemoryTestRunner --test-arguments=\"ultra\""
            ;;
        "critical")
            memory_limit=$CRITICAL_MEMORY
            cleanup_strategy="aggressive"
            test_command="stack test EnhancedMemoryTestRunner --test-arguments=\"critical\""
            ;;
        "emergency")
            memory_limit=$EMERGENCY_MEMORY
            cleanup_strategy="aggressive"
            test_command="stack test EnhancedMemoryTestRunner --test-arguments=\"emergency\""
            ;;
        "minimal")
            memory_limit=$MINIMAL_MEMORY
            cleanup_strategy="standard"
            test_command="stack test EnhancedMemoryTestRunner --test-arguments=\"minimal\""
            ;;
        "ci")
            memory_limit=$CI_MEMORY
            cleanup_strategy="standard"
            test_command="stack test EnhancedMemoryTestRunner --test-arguments=\"ci\""
            ;;
        "development")
            memory_limit=$DEVELOPMENT_MEMORY
            cleanup_strategy="light"
            test_command="stack test EnhancedMemoryTestRunner --test-arguments=\"development\""
            ;;
        "comprehensive")
            memory_limit=$COMPREHENSIVE_MEMORY
            cleanup_strategy="light"
            test_command="stack test EnhancedMemoryTestRunner --test-arguments=\"comprehensive\""
            ;;
        *)
            echo -e "${RED}错误: 未知的测试类型 '$test_type'${NC}"
            return 1
            ;;
    esac
    
    run_enhanced_test "$test_type" $memory_limit "$test_command" "$cleanup_strategy"
}

# 运行内存基准测试
run_memory_benchmark() {
    echo -e "${PURPLE}=== 内存基准测试 ===${NC}"
    
    local test_types=("ultra" "critical" "emergency" "minimal" "ci")
    local results=()
    
    for test_type in "${test_types[@]}"; do
        echo -e "${CYAN}运行 $test_type 基准测试...${NC}"
        
        local start_time=$(date +%s)
        local start_memory=$(get_available_memory)
        
        if run_specific_test "$test_type"; then
            local end_time=$(date +%s)
            local end_memory=$(get_available_memory)
            local duration=$((end_time - start_time))
            local memory_diff=$((start_memory - end_memory))
            
            results+=("$test_type: ${duration}s, ${memory_diff}MB")
            echo -e "${GREEN}$test_type 基准测试完成${NC}"
        else
            results+=("$test_type: FAILED")
            echo -e "${RED}$test_type 基准测试失败${NC}"
        fi
        
        # 等待内存稳定
        sleep 2
    done
    
    echo -e "${PURPLE}=== 基准测试结果 ===${NC}"
    for result in "${results[@]}"; do
        echo -e "${BLUE}$result${NC}"
    done
}

# 生成内存优化报告
generate_memory_report() {
    echo -e "${PURPLE}=== 内存优化报告 ===${NC}"
    
    local available_mem=$(get_available_memory)
    local total_mem=$(get_total_memory)
    local pressure=$(detect_memory_pressure)
    
    echo -e "${GREEN}系统信息:${NC}"
    echo -e "${BLUE}  总内存: ${total_mem}MB${NC}"
    echo -e "${BLUE}  可用内存: ${available_mem}MB${NC}"
    echo -e "${BLUE}  内存压力: ${pressure}${NC}"
    echo ""
    
    echo -e "${GREEN}内存优化级别:${NC}"
    echo -e "${BLUE}  超极端 (16MB): 用于极度受限环境${NC}"
    echo -e "${BLUE}  关键 (24MB): 用于关键任务环境${NC}"
    echo -e "${BLUE}  紧急 (32MB): 用于紧急情况${NC}"
    echo -e "${BLUE}  最小 (48MB): 用于资源受限环境${NC}"
    echo -e "${BLUE}  CI (64MB): 用于持续集成环境${NC}"
    echo -e "${BLUE}  开发 (128MB): 用于开发环境${NC}"
    echo -e "${BLUE}  综合 (256MB): 用于完整测试${NC}"
    echo ""
    
    echo -e "${GREEN}优化功能:${NC}"
    echo -e "${BLUE}  - 统一内存管理和限制${NC}"
    echo -e "${BLUE}  - 极端内存优化策略${NC}"
    echo -e "${BLUE}  - 智能测试选择${NC}"
    echo -e "${BLUE}  - 增强内存监控${NC}"
    echo -e "${BLUE}  - 自适应清理策略${NC}"
    echo -e "${BLUE}  - 持续内存监控${NC}"
    echo ""
    
    echo -e "${GREEN}建议:${NC}"
    if [ $available_mem -lt $EMERGENCY_MEMORY ]; then
        echo -e "${YELLOW}  - 内存严重不足，建议使用 ultra 或 critical 模式${NC}"
    elif [ $available_mem -lt $CI_MEMORY ]; then
        echo -e "${YELLOW}  - 内存有限，建议使用 emergency 或 minimal 模式${NC}"
    else
        echo -e "${GREEN}  - 内存充足，可以使用 development 或 comprehensive 模式${NC}"
    fi
}

# 显示帮助信息
show_help() {
    echo "增强内存优化测试脚本"
    echo ""
    echo "用法: $0 [选项]"
    echo ""
    echo "选项:"
    echo "  -u, --ultra        运行超极端测试 (16MB)"
    echo "  -c, --critical     运行关键测试 (24MB)"
    echo "  -e, --emergency    运行紧急测试 (32MB)"
    echo "  -m, --minimal      运行最小测试 (48MB)"
    echo "  -i, --ci           运行CI测试 (64MB)"
    echo "  -d, --development  运行开发测试 (128MB)"
    echo "  -o, --comprehensive 运行综合测试 (256MB)"
    echo "  -b, --benchmark    运行内存基准测试"
    echo "  -r, --report       生成内存优化报告"
    echo "  -h, --help         显示此帮助信息"
    echo ""
    echo "如果不指定选项，脚本将根据可用内存自动选择测试级别。"
    echo ""
    echo "环境变量:"
    echo "  MEMORY_LIMIT_MB    手动设置内存限制"
    echo "  CLEANUP_STRATEGY   设置清理策略 (light/standard/aggressive/emergency)"
    echo "  ULTRA_TESTS=true   强制启用超极端模式"
    echo "  CRITICAL_TESTS=true 强制启用关键模式"
    echo "  EMERGENCY_TESTS=true 强制启用紧急模式"
    echo "  MINIMAL_TESTS=true 强制启用最小模式"
    echo "  CI=true            强制启用CI模式"
}

# 主函数
main() {
    # 检查Stack是否可用
    if ! command -v stack >/dev/null 2>&1; then
        echo -e "${RED}错误: 未找到Stack命令，请确保已安装Haskell Stack${NC}"
        exit 1
    fi
    
    # 检查项目结构
    if [ ! -f "EnhancedMemoryTestRunner.hs" ]; then
        echo -e "${RED}错误: 未找到EnhancedMemoryTestRunner.hs，请确保在正确的目录中运行${NC}"
        exit 1
    fi
    
    # 解析命令行参数
    case "${1:-}" in
        -u|--ultra)
            run_specific_test "ultra"
            ;;
        -c|--critical)
            run_specific_test "critical"
            ;;
        -e|--emergency)
            run_specific_test "emergency"
            ;;
        -m|--minimal)
            run_specific_test "minimal"
            ;;
        -i|--ci)
            run_specific_test "ci"
            ;;
        -d|--development)
            run_specific_test "development"
            ;;
        -o|--comprehensive)
            run_specific_test "comprehensive"
            ;;
        -b|--benchmark)
            run_memory_benchmark
            ;;
        -r|--report)
            generate_memory_report
            ;;
        -h|--help)
            show_help
            ;;
        "")
            run_enhanced_memory_tests
            ;;
        *)
            echo -e "${RED}错误: 未知选项 '$1'${NC}"
            echo ""
            show_help
            exit 1
            ;;
    esac
}

# 运行主函数
main "$@"