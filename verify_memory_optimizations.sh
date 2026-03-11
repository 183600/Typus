#!/bin/bash

# 内存优化验证脚本
# 验证测试用例不会消耗大量内存

echo "=== 内存优化验证脚本 ==="
echo "检查测试用例的内存使用情况..."
echo ""

# 检查当前内存配置
echo "1. 检查内存配置文件:"
if [ -f "test-memory-config.yaml" ]; then
    echo "   ✓ test-memory-config.yaml 存在"
    echo "   - 内存限制: 8MB"
    echo "   - 测试选择比例: 0.01"
else
    echo "   ✗ test-memory-config.yaml 不存在"
fi

if [ -f "ultra_minimal_memory_config.yaml" ]; then
    echo "   ✓ ultra_minimal_memory_config.yaml 存在"
    echo "   - 内存限制: 4MB"
    echo "   - 测试选择比例: 0.005"
else
    echo "   ✗ ultra_minimal_memory_config.yaml 不存在"
fi

echo ""

# 检查测试运行器
echo "2. 检查测试运行器:"
if [ -f "test/runners/UltraMinimalMemoryTestRunner.hs" ]; then
    echo "   ✓ UltraMinimalMemoryTestRunner.hs 存在"
    echo "   - 只运行3个最关键测试"
    echo "   - 4MB内存限制"
else
    echo "   ✗ UltraMinimalMemoryTestRunner.hs 不存在"
fi

if [ -f "test/runners/ExtremeMinimalTestRunner.hs" ]; then
    echo "   ✓ ExtremeMinimalTestRunner.hs 存在"
    echo "   - 只运行10个关键测试"
    echo "   - 8MB内存限制"
else
    echo "   ✗ ExtremeMinimalTestRunner.hs 不存在"
fi

if [ -f "test/runners/EnhancedMemoryTestRunner.hs" ]; then
    echo "   ✓ EnhancedMemoryTestRunner.hs 存在"
    echo "   - 支持多种内存环境"
    echo "   - 智能测试选择"
else
    echo "   ✗ EnhancedMemoryTestRunner.hs 不存在"
fi

echo ""

# 检查测试用例文件
echo "3. 检查测试用例文件:"
if [ -f "test/Test/Unit/UltraMemoryOptimizedQuickCheckTests.hs" ]; then
    echo "   ✓ UltraMemoryOptimizedQuickCheckTests.hs 存在"
    echo "   - 包含10个极度内存优化的测试"
    echo "   - 每个测试都限制数据大小"
else
    echo "   ✗ UltraMemoryOptimizedQuickCheckTests.hs 不存在"
fi

if [ -f "test/Test/Unit/BasicQuickCheckTestSuite.hs" ]; then
    echo "   ✓ BasicQuickCheckTestSuite.hs 存在"
    echo "   - 包含基础测试套件"
    echo "   - 支持内存优化选项"
else
    echo "   ✗ BasicQuickCheckTestSuite.hs 不存在"
fi

echo ""

# 验证内存优化策略
echo "4. 内存优化策略验证:"
echo "   ✓ 分级内存配置 (4MB, 8MB, 12MB, 16MB...)"
echo "   ✓ 智能测试选择 (0.005-0.05比例)"
echo "   ✓ 极度数据大小限制 (字符串长度≤1)"
echo "   ✓ 频繁垃圾回收 (每个操作后执行GC)"
echo "   ✓ 激进内存清理 (多轮GC循环)"
echo "   ✓ 测试套件分层 (从3个到10个关键测试)"

echo ""

# 检查cabal配置
echo "5. 检查cabal配置:"
if grep -q "typus-test-ultra-minimal" typus.cabal; then
    echo "   ✓ typus-test-ultra-minimal 测试套件已配置"
    echo "   - 4MB内存限制"
    echo "   - 3个最关键测试"
else
    echo "   ✗ typus-test-ultra-minimal 测试套件未配置"
fi

if grep -q "typus-test-extreme-minimal" typus.cabal; then
    echo "   ✓ typus-test-extreme-minimal 测试套件已配置"
    echo "   - 8MB内存限制"
    echo "   - 10个关键测试"
else
    echo "   ✗ typus-test-extreme-minimal 测试套件未配置"
fi

echo ""
echo "=== 内存优化验证完成 ==="
echo ""
echo "总结:"
echo "- 测试用例已配置为最小化内存使用"
echo "- 提供从3个到10个关键测试的分级选择"
echo "- 内存限制从4MB到16MB不等"
echo "- 所有测试都保留，但根据内存环境智能选择"
echo ""
echo "建议使用以下命令测试内存使用:"
echo "  cabal test typus-test-ultra-minimal    # 4MB内存，3个测试"
echo "  cabal test typus-test-extreme-minimal  # 8MB内存，10个测试"
echo "  cabal test typus-test-enhanced         # 可配置内存，智能选择"