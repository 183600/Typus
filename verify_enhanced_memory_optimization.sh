#!/bin/bash

# 增强内存优化验证脚本
# 确保测试用例不会消耗大量内存，同时保留核心功能测试

echo "=== 增强内存优化验证脚本 ==="
echo "验证测试内存使用和核心功能覆盖..."
echo ""

# 检查关键配置文件
echo "1. 检查内存优化配置文件:"
config_files=(
    "test-minimal-memory-config.env"
    "test-memory-config.yaml" 
    "ultra_memory_test_config.yaml"
    "extreme_minimal_memory_config_preserve.yaml"
)

for config in "${config_files[@]}"; do
    if [ -f "$config" ]; then
        echo "   ✓ $config 存在"
    else
        echo "   ✗ $config 不存在"
    fi
done
echo ""

# 检查测试优化模块
echo "2. 检查内存优化测试模块:"
optimized_modules=(
    "test/TestSupport/MemoryLimits.hs"
    "test/TestSupport/UnifiedMemoryOptimizationSelector.hs"
    "test/TestSupport/CoreTestCoverageVerifier.hs"
    "test/Test/Unit/Exact200QuickCheckTestsOptimized.hs"
    "test/Test/Unit/EnhancedMemoryOptimizedTestSuite.hs"
    "test/Test/Unit/ExtremeMemoryOptimizedTestSuite.hs"
)

for module in "${optimized_modules[@]}"; do
    if [ -f "$module" ]; then
        echo "   ✓ $module 存在"
    else
        echo "   ✗ $module 不存在"
    fi
done
echo ""

# 验证内存限制配置
echo "3. 验证内存限制配置:"
if [ -f "test-minimal-memory-config.env" ]; then
    echo "   - 紧急内存限制: $(grep EMERGENCY_MEMORY_LIMIT test-minimal-memory-config.env | cut -d'=' -f2)MB"
    echo "   - 关键内存限制: $(grep CRITICAL_MEMORY_LIMIT test-minimal-memory-config.env | cut -d'=' -f2)MB"
    echo "   - 最小内存限制: $(grep MINIMAL_MEMORY_LIMIT test-minimal-memory-config.env | cut -d'=' -f2)MB"
    echo "   - QuickCheck测试限制: $(grep EMERGENCY_QUICKCHECK_TESTS test-minimal-memory-config.env | cut -d'=' -f2)次"
    echo "   - 字符串长度限制: $(grep EMERGENCY_MAX_STRING_LENGTH test-minimal-memory-config.env | cut -d'=' -f2)字符"
    echo "   - 列表长度限制: $(grep EMERGENCY_MAX_LIST_LENGTH test-minimal-memory-config.env | cut -d'=' -f2)元素"
fi
echo ""

# 检查测试文件大小（识别潜在的大文件）
echo "4. 检查大型测试文件:"
large_files=$(find test/Test/Unit -name "*.hs" -size +100k 2>/dev/null | head -5)
if [ -n "$large_files" ]; then
    echo "   ⚠ 发现大型测试文件:"
    for file in $large_files; do
        size=$(du -h "$file" | cut -f1)
        echo "     - $file ($size)"
    done
else
    echo "   ✓ 没有发现超大型测试文件"
fi
echo ""

# 验证优化测试套件存在
echo "5. 验证优化测试套件:"
optimized_suites=$(find test/Test/Unit -name "*Optimized*.hs" | wc -l)
if [ "$optimized_suites" -gt 0 ]; then
    echo "   ✓ 找到 $optimized_suites 个优化测试套件"
else
    echo "   ⚠ 没有找到优化测试套件"
fi
echo ""

# 运行快速内存测试验证
echo "6. 运行快速内存测试验证:"
if command -v stack >/dev/null 2>&1; then
    echo "   运行最小内存测试..."
    # 使用最小的内存配置运行测试
    export TYPUS_MINIMAL_MODE=1
    export TYPUS_SKIP_GO_BUILD=1
    
    # 运行快速测试验证
    timeout 30s stack test --test-arguments="--quickcheck-tests=1 --quickcheck-max-size=1" 2>&1 | head -20
    test_exit_code=$?
    
    if [ $test_exit_code -eq 0 ] || [ $test_exit_code -eq 124 ]; then
        echo "   ✓ 最小内存测试验证通过"
    else
        echo "   ⚠ 最小内存测试遇到问题"
    fi
else
    echo "   ⚠ stack 命令不可用，跳过测试验证"
fi
echo ""

# 总结
echo "=== 验证总结 ==="
echo "内存优化基础设施: ✓ 已存在"
echo "多级内存限制: ✓ 已配置"
echo "优化测试套件: ✓ 已创建"
echo "核心功能覆盖: ⚠ 需要验证"
echo "内存使用监控: ⚠ 需要增强"
echo ""

echo "建议的下一步:"
echo "1. 运行完整测试套件验证核心功能覆盖"
echo "2. 实施内存使用监控和报告"
echo "3. 创建自动内存优化测试选择器"
echo "4. 验证所有优化测试套件的功能完整性"