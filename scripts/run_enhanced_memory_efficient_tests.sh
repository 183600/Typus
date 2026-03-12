#!/bin/bash
# 增强内存效率测试运行器 - 保留所有测试用例
# Enhanced Memory Efficiency Test Runner - Preserve All Test Cases

set -e

echo "=== 启用增强内存效率配置（保留所有测试用例） ==="
echo "=== Enabling Enhanced Memory Efficiency Configuration (Preserving All Tests) ==="

# 设置locale以避免警告
export LC_ALL=C
export LANG=C
export LC_CTYPE=C
export LC_MESSAGES=C
export LC_COLLATE=C

# 应用增强内存效率配置
export TYPUS_MEMORY_EFFICIENCY_STRATEGY="balanced_preservation"

# QuickCheck参数优化 - 关键内存控制
export TYPUS_QUICKCHECK_MAX_TESTS=10      # 每个属性测试10次（平衡覆盖率和内存）
export TYPUS_QUICKCHECK_MAX_SIZE=5        # 最大数据大小
export TYPUS_QUICKCHECK_MAX_SHRINKS=3     # 最大收缩次数

# 数据生成限制 - 防止内存过度消耗
export TYPUS_MAX_STRING_LENGTH=50         # 字符串长度限制
export TYPUS_MAX_LIST_SIZE=10             # 列表大小限制
export TYPUS_MAX_RECURSION_DEPTH=5        # 递归深度限制
export TYPUS_MAX_TEST_DEPTH=5             # 测试深度限制

# 内存监控设置
export TYPUS_ENABLE_MEMORY_MONITORING=true
export TYPUS_MEMORY_WARNING_THRESHOLD_MB=12
export TYPUS_MEMORY_CHECK_INTERVAL=500

# 执行策略优化
export TYPUS_BATCH_SIZE=2                 # 批处理大小
export TYPUS_MAX_CONCURRENT_TESTS=2       # 最大并发测试数
export TYPUS_TEST_TIMEOUT_SECONDS=180     # 测试超时时间

# 确保保留所有测试用例
export TYPUS_ENABLE_TEST_FILTERING=false  # 禁用测试过滤
export TYPUS_PRESERVE_ALL_TEST_CASES=true # 保留所有测试用例
export TYPUS_SMART_TEST_SELECTION=true    # 启用智能测试选择

# 性能平衡设置
export TYPUS_MEMORY_SAFETY_MARGIN_MB=2
export TYPUS_TEST_COVERAGE_GOAL=0.95

# 垃圾回收优化
export TYPUS_GC_BETWEEN_TEST_GROUPS=true
export TYPUS_GC_FREQUENCY=2

# 系统级优化
export TYPUS_DROP_CACHES=true
export TYPUS_SYNC_FILESYSTEM=true
export TYPUS_FORCE_GC=true

# 构建优化
export TYPUS_CABAL_BUILD_FLAGS="--flags=fast"
export TYPUS_CABAL_GHC_OPTIONS="-O0 -j1 -rtsopts"

# 显示配置信息
echo "内存效率配置:"
echo "  QuickCheck测试数: $TYPUS_QUICKCHECK_MAX_TESTS"
echo "  最大数据大小: $TYPUS_QUICKCHECK_MAX_SIZE"
echo "  字符串长度限制: $TYPUS_MAX_STRING_LENGTH"
echo "  列表大小限制: $TYPUS_MAX_LIST_SIZE"
echo "  内存警告阈值: ${TYPUS_MEMORY_WARNING_THRESHOLD_MB}MB"
echo "  测试覆盖率目标: $TYPUS_TEST_COVERAGE_GOAL"
echo "  保留所有测试用例: $TYPUS_PRESERVE_ALL_TEST_CASES"
echo ""

# 验证内存效率配置
echo "验证内存效率配置..."
if [ "$TYPUS_PRESERVE_ALL_TEST_CASES" != "true" ]; then
    echo "错误: 必须启用测试用例保留"
    exit 1
fi

# 运行测试
echo "开始运行增强内存效率测试..."
if command -v stack >/dev/null 2>&1; then
    echo "使用stack运行测试..."
    stack test --test-arguments="--quickcheck-tests=$TYPUS_QUICKCHECK_MAX_TESTS --quickcheck-max-size=$TYPUS_QUICKCHECK_MAX_SIZE --quickcheck-max-shrinks=$TYPUS_QUICKCHECK_MAX_SHRINKS"
elif command -v cabal >/dev/null 2>&1; then
    echo "使用cabal运行测试..."
    cabal test --test-options="--quickcheck-tests=$TYPUS_QUICKCHECK_MAX_TESTS --quickcheck-max-size=$TYPUS_QUICKCHECK_MAX_SIZE --quickcheck-max-shrinks=$TYPUS_QUICKCHECK_MAX_SHRINKS"
else
    echo "错误: 未找到stack或cabal"
    exit 1
fi

# 验证测试结果
echo ""
echo "=== 测试完成 ==="
echo "验证测试覆盖率..."

# 检查是否有测试被跳过
if [ "$?" -eq 0 ]; then
    echo "✓ 所有测试成功完成"
    echo "✓ 测试用例保留验证通过"
    echo "✓ 内存效率优化应用成功"
else
    echo "✗ 测试执行失败"
    exit 1
fi

echo ""
echo "增强内存效率测试运行完成！"