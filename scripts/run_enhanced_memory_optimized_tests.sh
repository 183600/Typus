#!/usr/bin/env bash
# 增强内存优化测试运行脚本
# 基于现有内存优化基础设施，提供更智能的内存管理

set -e

echo "=== 增强内存优化测试运行 ==="
echo ""

# 检测可用内存并设置优化级别
AVAILABLE_MEMORY=$(free -m | awk 'NR==2{print $7}' || echo "512")
echo "检测到可用内存: ${AVAILABLE_MEMORY}MB"

# 基于可用内存设置优化级别
if [ "$AVAILABLE_MEMORY" -lt 16 ]; then
    MEMORY_LEVEL="EMERGENCY"
    MEMORY_LIMIT_MB=1
    TEST_BATCH_SIZE=1
    echo "内存级别: 紧急 (${MEMORY_LIMIT_MB}MB)"
elif [ "$AVAILABLE_MEMORY" -lt 32 ]; then
    MEMORY_LEVEL="CRITICAL"
    MEMORY_LIMIT_MB=2
    TEST_BATCH_SIZE=1
    echo "内存级别: 关键 (${MEMORY_LIMIT_MB}MB)"
elif [ "$AVAILABLE_MEMORY" -lt 64 ]; then
    MEMORY_LEVEL="MINIMAL"
    MEMORY_LIMIT_MB=4
    TEST_BATCH_SIZE=2
    echo "内存级别: 最小 (${MEMORY_LIMIT_MB}MB)"
elif [ "$AVAILABLE_MEMORY" -lt 128 ]; then
    MEMORY_LEVEL="LOW"
    MEMORY_LIMIT_MB=8
    TEST_BATCH_SIZE=3
    echo "内存级别: 低 (${MEMORY_LIMIT_MB}MB)"
else
    MEMORY_LEVEL="MODERATE"
    MEMORY_LIMIT_MB=16
    TEST_BATCH_SIZE=5
    echo "内存级别: 中等 (${MEMORY_LIMIT_MB}MB)"
fi

# 环境变量设置
export TYPUS_MEMORY_LEVEL="$MEMORY_LEVEL"
export TYPUS_SKIP_GO_BUILD=1
export TYPUS_MINIMAL_MODE=1

# 系统内存限制
if command -v ulimit >/dev/null 2>&1; then
    ulimit -v $((MEMORY_LIMIT_MB * 1024))
    echo "设置虚拟内存限制: ${MEMORY_LIMIT_MB}MB"
fi

# 清理函数
cleanup() {
    echo "执行内存清理..."
    # 强制垃圾回收
    if command -v ghc >/dev/null 2>&1; then
        ghc -e "import System.Mem; performGC" 2>/dev/null || true
    fi
    # 清理临时文件
    find /tmp -name "typus*" -mtime +1 -delete 2>/dev/null || true
}

# 设置清理陷阱
trap cleanup EXIT INT TERM

# 构建优化选项
BUILD_OPTS=(
    "--ghc-options=-rtsopts"
    "--ghc-options=-with-rtsopts=-M${MEMORY_LIMIT_MB}m"
    "--ghc-options=-O0"
    "--ghc-options=-fno-warn-unused-imports"
    "--ghc-options=-fno-warn-unused-matches"
    "--disable-profiling"
)

echo ""
echo "构建增强内存优化测试套件..."
if ! cabal build typus-test "${BUILD_OPTS[@]}"; then
    echo "构建失败，尝试简化构建..."
    cabal build typus-test --ghc-options="-O0 -rtsopts" --disable-profiling
fi

echo ""
echo "运行增强内存优化测试..."

# 测试选择函数 - 基于内存级别选择适当的测试套件
select_test_suite() {
    local memory_level="$1"
    case "$memory_level" in
        EMERGENCY)
            echo "--test-option=--pattern=*Essential* --test-option=--pattern=*Core*"
            ;;
        CRITICAL)
            echo "--test-option=--pattern=*Essential* --test-option=--pattern=*Core* --test-option=--pattern=*Basic*"
            ;;
        MINIMAL)
            echo "--test-option=--pattern=*Essential* --test-option=--pattern=*Core* --test-option=--pattern=*Basic* --test-option=--pattern=*Standard*"
            ;;
        LOW)
            echo "--test-option=--pattern=*Essential* --test-option=--pattern=*Core* --test-option=--pattern=*Basic* --test-option=--pattern=*Standard* --test-option=--pattern=*Optimized*"
            ;;
        MODERATE)
            echo ""  # 运行所有测试
            ;;
        *)
            echo ""  # 默认运行所有测试
            ;;
    esac
}

# 运行内存优化测试的函数
run_enhanced_memory_test() {
    local test_pattern="$1"
    local test_name="$2"
    
    echo "运行测试: ${test_name}"
    
    # 基于内存级别选择测试选项
    local test_options=$(select_test_suite "$MEMORY_LEVEL")
    
    # 运行测试并监控内存
    if cabal test typus-test \
        --test-option="+RTS" \
        --test-option="-M${MEMORY_LIMIT_MB}m" \
        --test-option="-A1m" \
        --test-option="-RTS" \
        --disable-profiling \
        --test-show-details=direct \
        $test_options; then
        echo "✓ 测试完成: ${test_name}"
    else
        echo "✗ 测试失败: ${test_name}"
        return 1
    fi
    
    # 测试后清理
    cleanup
    echo ""
}

# 运行核心测试套件
echo "=== 运行核心测试套件 ==="
run_enhanced_memory_test "*Essential*" "核心功能测试"

# 根据内存级别运行扩展测试
if [ "$MEMORY_LEVEL" != "EMERGENCY" ] && [ "$MEMORY_LEVEL" != "CRITICAL" ]; then
    echo "=== 运行基础测试套件 ==="
    run_enhanced_memory_test "*Basic*" "基础功能测试"
fi

if [ "$MEMORY_LEVEL" = "MODERATE" ] || [ "$MEMORY_LEVEL" = "LOW" ]; then
    echo "=== 运行标准测试套件 ==="
    run_enhanced_memory_test "*Standard*" "标准功能测试"
fi

if [ "$MEMORY_LEVEL" = "MODERATE" ]; then
    echo "=== 运行全面测试套件 ==="
    run_enhanced_memory_test "*" "全面功能测试"
fi

echo ""
echo "=== 增强内存优化测试完成 ==="
echo "内存级别: ${MEMORY_LEVEL}"
echo "内存限制: ${MEMORY_LIMIT_MB}MB"
echo "测试批次大小: ${TEST_BATCH_SIZE}"
echo "测试执行状态: 成功完成"

# 生成内存使用报告
echo ""
echo "=== 内存使用报告 ==="
if command -v free >/dev/null 2>&1; then
    echo "最终内存状态:"
    free -h | head -2
fi