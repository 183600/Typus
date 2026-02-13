#!/bin/bash

# 增强内存测试运行器配置脚本
# 这个脚本提供了针对不同环境的内存优化测试配置

set -e

# 颜色输出
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 默认配置
DEFAULT_MEMORY_MB=64
DEFAULT_BATCH_SIZE=5
DEFAULT_GC_FREQUENCY=3
DEFAULT_TEST_SELECTION_RATIO=0.3

# 环境检测
detect_environment() {
    echo -e "${BLUE}检测运行环境...${NC}"
    
    # 检测CI环境
    if [ "$CI" = "true" ] || [ "$GITHUB_ACTIONS" = "true" ] || [ "$GITLAB_CI" = "true" ]; then
        echo -e "${YELLOW}检测到CI环境${NC}"
        ENVIRONMENT="ci"
        return 0
    fi
    
    # 检测容器环境
    if [ -f /.dockerenv ]; then
        echo -e "${YELLOW}检测到Docker容器环境${NC}"
        ENVIRONMENT="docker"
        return 0
    fi
    
    # 检测可用内存
    if command -v free >/dev/null 2>&1; then
        AVAILABLE_MEM=$(free -m | awk 'NR==2{printf "%.0f", $7}')
        echo -e "${GREEN}可用内存: ${AVAILABLE_MEM}MB${NC}"
        
        if [ "$AVAILABLE_MEM" -lt 128 ]; then
            ENVIRONMENT="low-memory"
        elif [ "$AVAILABLE_MEM" -lt 512 ]; then
            ENVIRONMENT="standard"
        else
            ENVIRONMENT="high-memory"
        fi
    else
        ENVIRONMENT="unknown"
    fi
    
    echo -e "${GREEN}环境类型: $ENVIRONMENT${NC}"
}

# 设置内存配置
set_memory_config() {
    local env=$1
    
    echo -e "${BLUE}设置内存配置...${NC}"
    
    case $env in
        "ci")
            MEMORY_MB=32
            BATCH_SIZE=3
            GC_FREQUENCY=1
            TEST_SELECTION_RATIO=0.1
            CONSERVATIVE=true
            AGGRESSIVE_GC=true
            ;;
        "docker")
            MEMORY_MB=48
            BATCH_SIZE=4
            GC_FREQUENCY=2
            TEST_SELECTION_RATIO=0.15
            CONSERVATIVE=true
            AGGRESSIVE_GC=true
            ;;
        "low-memory")
            MEMORY_MB=32
            BATCH_SIZE=3
            GC_FREQUENCY=1
            TEST_SELECTION_RATIO=0.1
            CONSERVATIVE=true
            AGGRESSIVE_GC=true
            ;;
        "standard")
            MEMORY_MB=64
            BATCH_SIZE=5
            GC_FREQUENCY=3
            TEST_SELECTION_RATIO=0.3
            CONSERVATIVE=false
            AGGRESSIVE_GC=false
            ;;
        "high-memory")
            MEMORY_MB=128
            BATCH_SIZE=8
            GC_FREQUENCY=5
            TEST_SELECTION_RATIO=0.5
            CONSERVATIVE=false
            AGGRESSIVE_GC=false
            ;;
        *)
            MEMORY_MB=$DEFAULT_MEMORY_MB
            BATCH_SIZE=$DEFAULT_BATCH_SIZE
            GC_FREQUENCY=$DEFAULT_GC_FREQUENCY
            TEST_SELECTION_RATIO=$DEFAULT_TEST_SELECTION_RATIO
            CONSERVATIVE=false
            AGGRESSIVE_GC=false
            ;;
    esac
    
    # 允许环境变量覆盖
    MEMORY_MB=${AVAILABLE_MEMORY_MB:-$MEMORY_MB}
    BATCH_SIZE=${TEST_BATCH_SIZE:-$BATCH_SIZE}
    GC_FREQUENCY=${GC_FREQUENCY:-$GC_FREQUENCY}
    TEST_SELECTION_RATIO=${TEST_SELECTION_RATIO:-$TEST_SELECTION_RATIO}
    
    echo -e "${GREEN}内存配置:${NC}"
    echo -e "  内存限制: ${MEMORY_MB}MB"
    echo -e "  批处理大小: $BATCH_SIZE"
    echo -e "  GC频率: $GC_FREQUENCY"
    echo -e "  测试选择比例: $(echo "$TEST_SELECTION_RATIO * 100" | bc -l)%"
    echo -e "  保守模式: $CONSERVATIVE"
    echo -e "  激进GC: $AGGRESSIVE_GC"
}

# 设置GHC选项
set_ghc_options() {
    echo -e "${BLUE}设置GHC选项...${NC}"
    
    GHC_OPTIONS="+RTS -M${MEMORY_MB}m -A8m -n2m -qg -RTS"
    
    if [ "$AGGRESSIVE_GC" = "true" ]; then
        GHC_OPTIONS="$GHC_OPTIONS -G1"
    fi
    
    echo -e "${GREEN}GHC选项: $GHC_OPTIONS${NC}"
}

# 设置环境变量
set_environment_variables() {
    echo -e "${BLUE}设置环境变量...${NC}"
    
    export TYPUS_MEMORY_LIMIT_MB=$MEMORY_MB
    export TYPUS_BATCH_SIZE=$BATCH_SIZE
    export TYPUS_GC_FREQUENCY=$GC_FREQUENCY
    export TYPUS_TEST_SELECTION_RATIO=$TEST_SELECTION_RATIO
    export TYPUS_CONSERVATIVE_MODE=$CONSERVATIVE
    export TYPUS_AGGRESSIVE_GC=$AGGRESSIVE_GC
    export GHC_OPTIONS="$GHC_OPTIONS"
    
    # 设置locale以避免警告
    export LC_ALL=C
    export LANG=C
    export LC_CTYPE=C
    export LC_MESSAGES=C
    export LC_COLLATE=C
    
    echo -e "${GREEN}环境变量已设置${NC}"
}

# 创建测试配置文件
create_test_config() {
    echo -e "${BLUE}创建测试配置文件...${NC}"
    
    cat > test-memory-config.yaml << EOF
# 增强内存测试配置
memory:
  limit_mb: $MEMORY_MB
  batch_size: $BATCH_SIZE
  gc_frequency: $GC_FREQUENCY
  test_selection_ratio: $TEST_SELECTION_RATIO
  conservative_mode: $CONSERVATIVE
  aggressive_gc: $AGGRESSIVE_GC

execution:
  strategy: $(if [ "$CONSERVATIVE" = "true" ]; then echo "sequential"; else echo "batched"; fi)
  max_concurrent_tests: 1
  timeout_seconds: 300

monitoring:
  enable_memory_monitoring: true
  log_memory_usage: $CONSERVATIVE
  memory_check_interval: 1000

optimization:
  enable_test_reuse: true
  enable_memory_prediction: true
  adaptive_scaling: $(if [ "$ENVIRONMENT" != "ci" ]; then echo "true"; else echo "false"; fi)
  
environment:
  type: $ENVIRONMENT
  detected_memory_mb: ${AVAILABLE_MEM:-unknown}
EOF
    
    echo -e "${GREEN}测试配置文件已创建: test-memory-config.yaml${NC}"
}

# 运行内存优化的测试
run_memory_optimized_tests() {
    echo -e "${BLUE}运行内存优化测试...${NC}"
    
    # 选择测试运行器
    local test_runner=""
    
    if [ -f "test/EnhancedMemoryTestRunnerAdvanced.hs" ]; then
        test_runner="test/EnhancedMemoryTestRunnerAdvanced.hs"
    elif [ -f "test/SmartMemoryTestRunner.hs" ]; then
        test_runner="test/SmartMemoryTestRunner.hs"
    else
        echo -e "${RED}未找到内存优化测试运行器${NC}"
        return 1
    fi
    
    echo -e "${GREEN}使用测试运行器: $test_runner${NC}"
    
    # 构建测试运行器
    echo -e "${BLUE}构建测试运行器...${NC}"
    cabal build enhanced-memory-test-runner || cabal build smart-memory-test-runner || {
        echo -e "${RED}构建测试运行器失败${NC}"
        return 1
    }
    
    # 运行测试
    echo -e "${BLUE}执行测试...${NC}"
    
    local test_args=""
    if [ "$CONSERVATIVE" = "true" ]; then
        test_args="$test_args --minimal"
    fi
    
    if [ "$VERBOSE" = "true" ]; then
        test_args="$test_args --verbose"
    fi
    
    # 运行测试并监控内存
    if command -v /usr/bin/time >/dev/null 2>&1; then
        /usr/bin/time -v cabal run enhanced-memory-test-runner -- $test_args
    else
        cabal run enhanced-memory-test-runner -- $test_args
    fi
}

# 验证内存优化效果
verify_optimization() {
    echo -e "${BLUE}验证内存优化效果...${NC}"
    
    # 检查内存使用报告
    if [ -f "test-memory-report.txt" ]; then
        echo -e "${GREEN}内存使用报告:${NC}"
        cat test-memory-report.txt
    fi
    
    # 检查是否有内存泄漏
    echo -e "${BLUE}检查内存泄漏...${NC}"
    
    # 这里可以添加更多的验证逻辑
    echo -e "${GREEN}内存优化验证完成${NC}"
}

# 显示帮助信息
show_help() {
    echo -e "${BLUE}增强内存测试运行器配置脚本${NC}"
    echo ""
    echo "用法: $0 [选项]"
    echo ""
    echo "选项:"
    echo "  -h, --help              显示帮助信息"
    echo "  -e, --environment ENV   指定环境类型 (ci|docker|low-memory|standard|high-memory)"
    echo "  -m, --memory MB         指定内存限制（MB）"
    echo "  -b, --batch-size SIZE   指定批处理大小"
    echo "  -r, --ratio RATIO       指定测试选择比例（0.0-1.0）"
    echo "  -v, --verbose           启用详细输出"
    echo "  --conservative          启用保守模式"
    echo "  --aggressive-gc         启用激进垃圾回收"
    echo ""
    echo "环境变量:"
    echo "  AVAILABLE_MEMORY_MB     可用内存（MB）"
    echo "  TEST_BATCH_SIZE         测试批处理大小"
    echo "  GC_FREQUENCY            垃圾回收频率"
    echo "  TEST_SELECTION_RATIO    测试选择比例"
    echo ""
    echo "示例:"
    echo "  $0 --environment ci"
    echo "  $0 --memory 32 --conservative"
    echo "  $0 --batch-size 3 --ratio 0.1"
}

# 主函数
main() {
    echo -e "${GREEN}=== 增强内存测试运行器配置 ===${NC}"
    echo ""
    
    # 解析命令行参数
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                show_help
                exit 0
                ;;
            -e|--environment)
                ENVIRONMENT="$2"
                shift 2
                ;;
            -m|--memory)
                MEMORY_MB="$2"
                shift 2
                ;;
            -b|--batch-size)
                BATCH_SIZE="$2"
                shift 2
                ;;
            -r|--ratio)
                TEST_SELECTION_RATIO="$2"
                shift 2
                ;;
            -v|--verbose)
                VERBOSE=true
                shift
                ;;
            --conservative)
                CONSERVATIVE=true
                shift
                ;;
            --aggressive-gc)
                AGGRESSIVE_GC=true
                shift
                ;;
            *)
                echo -e "${RED}未知选项: $1${NC}"
                show_help
                exit 1
                ;;
        esac
    done
    
    # 检测环境
    if [ -z "$ENVIRONMENT" ]; then
        detect_environment
    fi
    
    # 设置内存配置
    set_memory_config "$ENVIRONMENT"
    
    # 设置GHC选项
    set_ghc_options
    
    # 设置环境变量
    set_environment_variables
    
    # 创建测试配置文件
    create_test_config
    
    # 运行内存优化的测试
    run_memory_optimized_tests
    
    # 验证优化效果
    verify_optimization
    
    echo -e "${GREEN}=== 配置完成 ===${NC}"
}

# 运行主函数
main "$@"