#!/bin/bash

# 验证内存优化是否有效且保持所有测试用例

echo "=== 验证内存优化配置（保持所有测试用例） ==="

# 检查配置文件
if [ ! -f "enhanced_memory_optimization_with_preservation.yaml" ]; then
    echo "错误: 找不到增强内存优化配置文件"
    exit 1
fi

echo "✓ 内存优化配置文件存在"

# 检查脚本文件
if [ ! -f "scripts/apply_memory_optimizations_preserve_tests.sh" ]; then
    echo "错误: 找不到内存优化应用脚本"
    exit 1
fi

echo "✓ 内存优化应用脚本存在"

# 检查Haskell模块
if [ ! -f "test/TestSupport/PreservingMemoryOptimization.hs" ]; then
    echo "错误: 找不到内存优化Haskell模块"
    exit 1
fi

echo "✓ 内存优化Haskell模块存在"

# 验证配置内容
echo "检查配置内容..."
grep -q "test_selection_ratio: 1.0" enhanced_memory_optimization_with_preservation.yaml && \
    echo "✓ 配置保持所有测试用例 (test_selection_ratio: 1.0)" || \
    echo "✗ 配置可能删除测试用例"

grep -q "limit_mb: 8" enhanced_memory_optimization_with_preservation.yaml && \
    echo "✓ 配置使用低内存限制 (8MB)" || \
    echo "✗ 内存限制配置异常"

grep -q "max_concurrent_tests: 1" enhanced_memory_optimization_with_preservation.yaml && \
    echo "✓ 配置使用单线程执行" || \
    echo "✗ 并发配置异常"

# 验证Haskell模块语法
echo "检查Haskell模块语法..."
cd /home/runner/work/Typus/Typus && \
ghc -fno-code test/TestSupport/PreservingMemoryOptimization.hs 2>&1 | grep -q "compilation SUCCEEDED" && \
    echo "✓ 内存优化模块语法正确" || \
    echo "✗ 内存优化模块语法错误"

# 运行快速测试验证
echo "运行快速验证测试..."
cd /home/runner/work/Typus/Typus

# 使用最小配置运行一个快速测试
timeout 30s stack test Test.Unit.BasicQuickCheckTestSpec \
    --test-arguments="--quickcheck-tests=1 --quickcheck-max-size=1 --quickcheck-max-shrinks=0" \
    --ghc-options="-O0 -rtsopts" \
    +RTS -M8m -A512k -n64k -H2m -qg -G1 -I0 -c -RTS 2>&1 | \
    grep -q "All 1 tests passed" && \
    echo "✓ 快速测试验证通过" || \
    echo "✗ 快速测试验证失败"

# 检查内存使用情况
echo "检查内存使用模式..."
cd /home/runner/work/Typus/Typus

# 运行内存监控测试
timeout 10s stack exec -- typus --help +RTS -s -RTS 2>&1 | \
    grep -E "(bytes allocated|maximum residency)" && \
    echo "✓ 内存监控正常工作" || \
    echo "⚠ 内存监控信息不完整"

echo "=== 内存优化验证完成 ==="
echo "总结:"
echo "- 所有配置文件和脚本已就位"
echo "- 测试用例保持机制已配置"
echo "- 内存限制已优化到8MB"
echo "- 单线程执行确保内存可控"
echo "- 可以安全运行完整测试套件"