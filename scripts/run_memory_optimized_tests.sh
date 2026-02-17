#!/usr/bin/env bash
# 内存优化测试运行脚本
# 用于在低内存环境下运行测试，确保不会消耗大量内存

set -e

# 内存限制配置
MEMORY_LIMIT_MB=32  # 总内存限制32MB
TEST_BATCH_SIZE=3   # 批处理大小
GC_FREQUENCY=2      # 垃圾回收频率

# 环境变量设置
export ULS_MEMORY_OPTIMIZED=1
export EMERGENCY_MEMORY=1
export TYPUS_SKIP_GO_BUILD=1  # 跳过Go构建以节省内存

# 系统内存限制
if command -v ulimit >/dev/null 2>&1; then
    # 设置虚拟内存限制
    ulimit -v $((MEMORY_LIMIT_MB * 1024))  # MB to KB
    echo "设置虚拟内存限制: ${MEMORY_LIMIT_MB}MB"
fi

# 清理函数
cleanup() {
    echo "清理临时文件和进程..."
    # 强制垃圾回收
    if command -v ghc >/dev/null 2>&1; then
        ghc -e "import System.Mem; performGC" 2>/dev/null || true
    fi
}

# 设置清理陷阱
trap cleanup EXIT INT TERM

echo "=== 内存优化测试运行 ==="
echo "内存限制: ${MEMORY_LIMIT_MB}MB"
echo "批处理大小: ${TEST_BATCH_SIZE}"
echo "垃圾回收频率: ${GC_FREQUENCY}"
echo ""

# 构建优化选项
BUILD_OPTS=(
    "--ghc-options=-rtsopts"
    "--ghc-options=-with-rtsopts=-M${MEMORY_LIMIT_MB}m"
    "--ghc-options=-O0"  # 禁用优化以减少内存使用
    "--ghc-options=-fno-warn-unused-imports"
    "--ghc-options=-fno-warn-unused-matches"
)

echo "构建测试套件..."
cabal build typus-test "${BUILD_OPTS[@]}" --disable-profiling

echo ""
echo "运行内存优化测试..."

# 运行测试的函数
run_memory_optimized_test() {
    local test_name="$1"
    echo "运行测试: ${test_name}"
    
    # 运行测试并监控内存
    cabal test typus-test \
        --test-option="+RTS" \
        --test-option="-M${MEMORY_LIMIT_MB}m" \
        --test-option="-A${GC_FREQUENCY}m" \
        --test-option="-RTS" \
        --disable-profiling \
        --test-show-details=direct \
    || {
        echo "测试失败: ${test_name}"
        return 1
    }
    
    # 测试后清理
    cleanup
    
    echo "测试完成: ${test_name}"
    echo ""
}

# 运行核心测试套件
echo "=== 运行核心测试套件 ==="
run_memory_optimized_test "核心QuickCheck测试"

# 如果有足够的内存，运行扩展测试
AVAILABLE_MEMORY=$(free -m | awk 'NR==2{print $7}' || echo "0")
if [ "$AVAILABLE_MEMORY" -gt 64 ]; then
    echo "检测到足够内存，运行扩展测试..."
    run_memory_optimized_test "扩展功能测试"
fi

echo ""
echo "=== 所有测试完成 ==="
echo "内存优化测试运行成功完成"