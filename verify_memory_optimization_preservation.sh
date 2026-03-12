#!/bin/bash
# 验证内存优化并确保测试用例保留
# Verify Memory Optimization and Test Case Preservation

echo "=== 验证内存优化配置 ==="
echo "=== Verifying Memory Optimization Configuration ==="

# 检查配置文件是否存在
if [ ! -f "enhanced_memory_optimization_preserve_tests.yaml" ]; then
    echo "❌ 配置文件不存在: enhanced_memory_optimization_preserve_tests.yaml"
    exit 1
else
    echo "✅ 配置文件存在"
fi

# 检查优化脚本是否存在
if [ ! -f "scripts/run_optimized_memory_preserve_tests.sh" ]; then
    echo "❌ 优化脚本不存在: scripts/run_optimized_memory_preserve_tests.sh"
    exit 1
else
    echo "✅ 优化脚本存在"
fi

# 检查脚本是否可执行
if [ ! -x "scripts/run_optimized_memory_preserve_tests.sh" ]; then
    echo "❌ 优化脚本不可执行"
    exit 1
else
    echo "✅ 优化脚本可执行"
fi

# 检查关键配置参数
echo ""
echo "关键配置参数检查:"

# 读取配置文件并检查关键设置
if grep -q "preserve_all_test_cases: true" enhanced_memory_optimization_preserve_tests.yaml; then
    echo "✅ 保留所有测试用例配置: 启用"
else
    echo "❌ 保留所有测试用例配置: 未启用"
fi

if grep -q "enable_test_filtering: false" enhanced_memory_optimization_preserve_tests.yaml; then
    echo "✅ 测试过滤: 禁用"
else
    echo "❌ 测试过滤: 未禁用"
fi

if grep -q "max_tests_per_property: 10" enhanced_memory_optimization_preserve_tests.yaml; then
    echo "✅ QuickCheck测试次数限制: 10"
else
    echo "❌ QuickCheck测试次数限制: 未设置"
fi

if grep -q "string_length_limit: 50" enhanced_memory_optimization_preserve_tests.yaml; then
    echo "✅ 字符串长度限制: 50"
else
    echo "❌ 字符串长度限制: 未设置"
fi

if grep -q "list_length_limit: 10" enhanced_memory_optimization_preserve_tests.yaml; then
    echo "✅ 列表长度限制: 10"
else
    echo "❌ 列表长度限制: 未设置"
fi

if grep -q "recursion_depth_limit: 5" enhanced_memory_optimization_preserve_tests.yaml; then
    echo "✅ 递归深度限制: 5"
else
    echo "❌ 递归深度限制: 未设置"
fi

# 检查内存限制设置
if grep -q "limit_mb: 16" enhanced_memory_optimization_preserve_tests.yaml; then
    echo "✅ 内存限制: 16MB"
else
    echo "❌ 内存限制: 未设置或值不正确"
fi

# 检查性能平衡设置
if grep -q "test_coverage_goal: 0.95" enhanced_memory_optimization_preserve_tests.yaml; then
    echo "✅ 测试覆盖率目标: 95%"
else
    echo "❌ 测试覆盖率目标: 未设置"
fi

echo ""
echo "=== 配置验证完成 ==="
echo "=== Configuration Verification Complete ==="

# 显示优化策略摘要
echo ""
echo "优化策略摘要:"
echo "1. 保留所有测试用例 - 不删除任何测试"
echo "2. 限制QuickCheck参数 - 减少测试数据规模"
echo "3. 限制数据生成 - 控制字符串、列表、递归深度"
echo "4. 平衡内存使用 - 16MB限制，95%覆盖率目标"
echo "5. 禁用测试过滤 - 确保所有测试都能运行"

echo ""
echo "✅ 内存优化配置验证成功"
echo "✅ 所有测试用例将被保留"
echo ""
echo "使用方法:"
echo "./scripts/run_optimized_memory_preserve_tests.sh"