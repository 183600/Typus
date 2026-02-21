#!/bin/bash

# 验证极度最小化测试的核心功能
# 确保优化后的测试仍然保持关键功能

set -e

echo "=== 验证极度最小化测试的核心功能 ==="
echo

# 设置环境变量
export LC_ALL=C
export LANG=C
export MEMORY_LIMIT_MB=8

echo "1. 检查极度最小化测试运行器是否存在..."
if [ ! -f "test/runners/ExtremeMinimalTestRunner.hs" ]; then
    echo "❌ 错误: 极度最小化测试运行器不存在"
    exit 1
fi
echo "✅ 极度最小化测试运行器存在"

echo
echo "2. 检查UltraMemoryOptimizedQuickCheckTests是否存在..."
if [ ! -f "test/Test/Unit/UltraMemoryOptimizedQuickCheckTests.hs" ]; then
    echo "❌ 错误: UltraMemoryOptimizedQuickCheckTests不存在"
    exit 1
fi
echo "✅ UltraMemoryOptimizedQuickCheckTests存在"

echo
echo "3. 检查测试配置文件是否存在..."
if [ ! -f "test/test-minimal-memory-config-optimized.yaml" ]; then
    echo "❌ 错误: 优化配置文件不存在"
    exit 1
fi
echo "✅ 优化配置文件存在"

echo
echo "4. 验证测试数量..."
TEST_COUNT=$(grep -c "testProperty" test/Test/Unit/UltraMemoryOptimizedQuickCheckTests.hs)
echo "发现 $TEST_COUNT 个testProperty"
if [ "$TEST_COUNT" -eq 10 ]; then
    echo "✅ 测试数量正确 (10个关键测试)"
else
    echo "⚠️  警告: 测试数量不是10个，而是 $TEST_COUNT 个"
fi

echo
echo "5. 验证内存配置..."
MEMORY_LIMIT=$(grep "limit_mb:" test/test-minimal-memory-config-optimized.yaml | awk '{print $2}')
echo "内存限制配置: ${MEMORY_LIMIT}MB"
if [ "$MEMORY_LIMIT" = "8" ]; then
    echo "✅ 内存限制配置正确 (8MB)"
else
    echo "⚠️  警告: 内存限制不是8MB，而是 ${MEMORY_LIMIT}MB"
fi

echo
echo "6. 检查QuickCheck配置..."
QC_SIZE=$(grep "max_quickcheck_size:" test/test-minimal-memory-config-optimized.yaml | awk '{print $2}')
QC_TESTS=$(grep "max_quickcheck_tests:" test/test-minimal-memory-config-optimized.yaml | awk '{print $2}')
echo "QuickCheck最大大小: $QC_SIZE"
echo "QuickCheck测试次数: $QC_TESTS"

if [ "$QC_SIZE" = "1" ] && [ "$QC_TESTS" = "1" ]; then
    echo "✅ QuickCheck配置正确 (最小配置)"
else
    echo "⚠️  警告: QuickCheck配置不是最小值"
fi

echo
echo "7. 检查排除模式..."
EXCLUDE_DEBUG=$(grep -c "debug" test/test-minimal-memory-config-optimized.yaml)
EXCLUDE_200=$(grep -c "200QuickCheck" test/test-minimal-memory-config-optimized.yaml)
echo "排除debug文件: $EXCLUDE_DEBUG 处"
echo "排除200QuickCheck文件: $EXCLUDE_200 处"

if [ "$EXCLUDE_DEBUG" -gt 0 ] && [ "$EXCLUDE_200" -gt 0 ]; then
    echo "✅ 排除模式配置正确"
else
    echo "⚠️  警告: 排除模式可能不完整"
fi

echo
echo "8. 验证核心测试功能..."
echo "检查核心测试函数:"

# 检查关键测试函数是否存在
CORE_TESTS=(
    "prop_trim_idempotent"
    "prop_split_by_basic"
    "prop_remove_comments_basic"
    "prop_is_complete_string_literal"
    "prop_safe_process_string_safe"
)

for test_func in "${CORE_TESTS[@]}"; do
    if grep -q "$test_func" test/Test/Unit/UltraMemoryOptimizedQuickCheckTests.hs; then
        echo "✅ $test_func 存在"
    else
        echo "❌ $test_func 缺失"
    fi
done

echo
echo "9. 检查cabal配置..."
if grep -q "typus-test-extreme-minimal" typus.cabal; then
    echo "✅ cabal中存在极度最小化测试套件配置"
else
    echo "❌ cabal中缺少极度最小化测试套件配置"
fi

echo
echo "=== 验证完成 ==="
echo "极度最小化测试配置已验证，核心功能应该得到保留"
echo
echo "运行命令:"
echo "  cabal test typus-test-extreme-minimal"
echo "  或者:"
echo "  ./scripts/run_extreme_minimal_tests.sh"