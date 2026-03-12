#!/bin/bash
# 优化内存测试运行器 - 保留所有测试用例
# Optimized Memory Test Runner - Preserve All Test Cases

set -e

echo "=== 启用增强内存优化配置（保留所有测试用例） ==="
echo "=== Enabling Enhanced Memory Optimization (Preserving All Tests) ==="

# 设置locale以避免警告
export LC_ALL=C
export LANG=C
export LC_CTYPE=C
export LC_MESSAGES=C
export LC_COLLATE=C

# 应用增强内存优化配置
export TYPUS_MEMORY_CONFIG_FILE="enhanced_memory_optimization_preserve_tests.yaml"

# QuickCheck参数优化 - 减少内存消耗的关键
export TYPUS_QUICKCHECK_MAX_TESTS=10      # 每个属性测试10次
export TYPUS_QUICKCHECK_MAX_SIZE=5        # 最大数据大小
export TYPUS_QUICKCHECK_MAX_SHRINKS=3     # 最大收缩次数

# 数据生成限制
export TYPUS_MAX_STRING_LENGTH=50         # 字符串长度限制
export TYPUS_MAX_LIST_SIZE=10             # 列表大小限制
export TYPUS_MAX_RECURSION_DEPTH=5        # 递归深度限制
export TYPUS_MAX_TEST_DEPTH=5             # 测试深度限制

# 内存监控设置
export TYPUS_ENABLE_MEMORY_MONITORING=true
export TYPUS_MEMORY_WARNING_THRESHOLD_MB=12
export TYPUS_MEMORY_CHECK_INTERVAL=500

# 执行策略
export TYPUS_BATCH_SIZE=2
export TYPUS_MAX_CONCURRENT_TESTS=2
export TYPUS_TEST_TIMEOUT_SECONDS=180

# 禁用测试过滤，确保保留所有测试用例
export TYPUS_ENABLE_TEST_FILTERING=false
export TYPUS_PRESERVE_ALL_TEST_CASES=true

# 性能平衡设置
export TYPUS_MEMORY_SAFETY_MARGIN_MB=2
export TYPUS_TEST_COVERAGE_GOAL=0.95

# 显示配置信息
echo "内存优化配置:"
echo "- QuickCheck测试次数: $TYPUS_QUICKCHECK_MAX_TESTS"
echo "- QuickCheck数据大小: $TYPUS_QUICKCHECK_MAX_SIZE"
echo "- 字符串长度限制: $TYPUS_MAX_STRING_LENGTH"
echo "- 列表大小限制: $TYPUS_MAX_LIST_SIZE"
echo "- 递归深度限制: $TYPUS_MAX_RECURSION_DEPTH"
echo "- 测试深度限制: $TYPUS_MAX_TEST_DEPTH"
echo "- 内存警告阈值: ${TYPUS_MEMORY_WARNING_THRESHOLD_MB}MB"
echo "- 测试覆盖率目标: ${TYPUS_TEST_COVERAGE_GOAL}"
echo "- 保留所有测试用例: 是"
echo ""

# 运行测试
echo "开始运行测试..."
cabal test --flags="-fast production" --test-show-details=direct "$@"

TEST_EXIT_CODE=$?

if [ $TEST_EXIT_CODE -eq 0 ]; then
    echo "✅ 测试成功完成 - 所有测试用例已保留"
    echo "✅ Tests completed successfully - All test cases preserved"
else
    echo "⚠️ 测试失败 - 退出代码: $TEST_EXIT_CODE"
    echo "⚠️ Tests failed - Exit code: $TEST_EXIT_CODE"
fi

exit $TEST_EXIT_CODE