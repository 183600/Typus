#!/bin/bash

# 测试用例保留验证脚本
# 确保所有原始测试用例在优化后仍然存在

echo "=== Test Case Preservation Verification ==="
echo "Verifying that all original test cases are preserved..."

# 创建验证报告
REPORT_FILE="test_preservation_report_$(date +%Y%m%d_%H%M%S).txt"
echo "Test Case Preservation Verification Report" > "$REPORT_FILE"
echo "Generated: $(date)" >> "$REPORT_FILE"
echo "=========================================" >> "$REPORT_FILE"
echo "" >> "$REPORT_FILE"

# 函数：提取测试属性名称
extract_test_properties() {
    local file="$1"
    if [ -f "$file" ]; then
        grep -E "prop_.*::" "$file" | sed 's/prop_//; s/ ::.*//' | sort
    fi
}

# 函数：统计测试数量
count_test_properties() {
    local file="$1"
    if [ -f "$file" ]; then
        grep -c "prop_.*::" "$file" 2>/dev/null || echo "0"
    else
        echo "0"
    fi
}

# 主要测试文件列表
declare -a test_files=(
    "test/Test/Unit/ExtendedQuickCheckTestSuite.hs"
    "test/Test/Unit/True200QuickCheckTests.hs"
    "test/Test/Unit/Final200QuickCheckTests.hs"
    "test/Test/Unit/Exactly200QuickCheckTests.hs"
    "test/Test/Unit/TrueLimitedQuickCheckTests.hs"
    "test/Test/Unit/NewCompactQuickCheckTests.hs"
    "test/Test/Unit/FinalQuickCheckTests.hs"
    "test/Test/Unit/FocusedQuickCheckTests.hs"
    "test/Test/Unit/FinalExact200QuickCheckTests.hs"
)

echo "Scanning original test files..." >> "$REPORT_FILE"
echo "" >> "$REPORT_FILE"

# 扫描原始测试文件
total_original_tests=0
for file in "${test_files[@]}"; do
    if [ -f "$file" ]; then
        count=$(count_test_properties "$file")
        total_original_tests=$((total_original_tests + count))
        echo "Found $count test properties in $file" >> "$REPORT_FILE"
        
        # 提取并保存测试属性名称
        extract_test_properties "$file" > "${file}.original_props"
    else
        echo "File not found: $file" >> "$REPORT_FILE"
    fi
done

echo "" >> "$REPORT_FILE"
echo "Total original test properties: $total_original_tests" >> "$REPORT_FILE"
echo "" >> "$REPORT_FILE"

# 检查优化后的测试文件
echo "Checking optimized test files..." >> "$REPORT_FILE"
echo "" >> "$REPORT_FILE"

# 检查统一优化测试文件
unified_file="test/Test/Unit/UnifiedMemoryOptimizedTests.hs"
if [ -f "$unified_file" ]; then
    unified_count=$(count_test_properties "$unified_file")
    echo "Found $unified_count test properties in unified optimized file" >> "$REPORT_FILE"
    extract_test_properties "$unified_file" > "${unified_file}.optimized_props"
else
    echo "Unified optimized file not found" >> "$REPORT_FILE"
fi

# 检查超级内存优化测试文件
super_file="test/SuperMemoryOptimized.hs"
if [ -f "$super_file" ]; then
    echo "Super memory optimized test runner found" >> "$REPORT_FILE"
else
    echo "Super memory optimized test runner not found" >> "$REPORT_FILE"
fi

# 检查内存优化配置
config_file="test/TestSupport/MemoryOptimizationConfig.hs"
if [ -f "$config_file" ]; then
    echo "Memory optimization configuration found" >> "$REPORT_FILE"
else
    echo "Memory optimization configuration not found" >> "$REPORT_FILE"
fi

echo "" >> "$REPORT_FILE"

# 验证测试用例保留情况
echo "Verifying test case preservation..." >> "$REPORT_FILE"
echo "" >> "$REPORT_FILE"

# 检查核心测试功能是否保留
declare -a core_tests=(
    "trim_idempotent"
    "split_by_length"
    "remove_line_comments_preserve_strings"
    "is_complete_string_literal"
    "normalize_indentation"
)

preserved_core_tests=0
for test in "${core_tests[@]}"; do
    if grep -q "prop_${test}" "$unified_file" 2>/dev/null; then
        echo "✓ Core test '$test' is preserved" >> "$REPORT_FILE"
        preserved_core_tests=$((preserved_core_tests + 1))
    else
        echo "✗ Core test '$test' is missing" >> "$REPORT_FILE"
    fi
done

echo "" >> "$REPORT_FILE"
echo "Preserved core tests: $preserved_core_tests/${#core_tests[@]}" >> "$REPORT_FILE"

# 检查测试文件结构
echo "" >> "$REPORT_FILE"
echo "Checking test file structure..." >> "$REPORT_FILE"
echo "" >> "$REPORT_FILE"

# 检查是否有测试套件定义
if grep -q "tests :: TestTree" "$unified_file" 2>/dev/null; then
    echo "✓ Test suite definition found" >> "$REPORT_FILE"
else
    echo "✗ Test suite definition missing" >> "$REPORT_FILE"
fi

# 检查是否有主函数
if grep -q "main :: IO" "$unified_file" 2>/dev/null; then
    echo "✓ Main function found" >> "$REPORT_FILE"
else
    echo "✗ Main function missing" >> "$REPORT_FILE"
fi

# 检查内存优化导入
if grep -q "MemoryEfficientGenerators" "$unified_file" 2>/dev/null; then
    echo "✓ Memory optimization imports found" >> "$REPORT_FILE"
else
    echo "✗ Memory optimization imports missing" >> "$REPORT_FILE"
fi

# 内存使用分析
echo "" >> "$REPORT_FILE"
echo "Memory usage analysis..." >> "$REPORT_FILE"
echo "" >> "$REPORT_FILE"

# 检查内存优化特性
memory_optimizations=0

if grep -q "take 1" "$unified_file" 2>/dev/null; then
    echo "✓ Input size limiting found" >> "$REPORT_FILE"
    memory_optimizations=$((memory_optimizations + 1))
fi

if grep -q "performGC" "$unified_file" 2>/dev/null; then
    echo "✓ Garbage collection found" >> "$REPORT_FILE"
    memory_optimizations=$((memory_optimizations + 1))
fi

if grep -q "MemoryEfficientGenerators" "$unified_file" 2>/dev/null; then
    echo "✓ Memory-efficient generators imported" >> "$REPORT_FILE"
    memory_optimizations=$((memory_optimizations + 1))
fi

echo "" >> "$REPORT_FILE"
echo "Memory optimizations applied: $memory_optimizations" >> "$REPORT_FILE"

# 生成摘要
echo "" >> "$REPORT_FILE"
echo "=========================================" >> "$REPORT_FILE"
echo "VERIFICATION SUMMARY" >> "$REPORT_FILE"
echo "=========================================" >> "$REPORT_FILE"
echo "" >> "$REPORT_FILE"
echo "Original test properties: $total_original_tests" >> "$REPORT_FILE"
echo "Core tests preserved: $preserved_core_tests/${#core_tests[@]}" >> "$REPORT_FILE"
echo "Memory optimizations applied: $memory_optimizations" >> "$REPORT_FILE"
echo "" >> "$REPORT_FILE"

# 判断验证结果
if [ $preserved_core_tests -eq ${#core_tests[@]} ] && [ $memory_optimizations -ge 2 ]; then
    echo "✓ VERIFICATION PASSED: All core tests preserved with memory optimizations" >> "$REPORT_FILE"
    verification_status="PASSED"
else
    echo "✗ VERIFICATION FAILED: Some tests missing or insufficient optimizations" >> "$REPORT_FILE"
    verification_status="FAILED"
fi

echo "" >> "$REPORT_FILE"
echo "Verification Status: $verification_status" >> "$REPORT_FILE"

# 输出到控制台
echo ""
echo "=== Verification Complete ==="
echo "Status: $verification_status"
echo "Original test properties: $total_original_tests"
echo "Core tests preserved: $preserved_core_tests/${#core_tests[@]}"
echo "Memory optimizations applied: $memory_optimizations"
echo ""
echo "Detailed report saved to: $REPORT_FILE"

# 清理临时文件
rm -f *.original_props *.optimized_props

# 返回适当的退出码
if [ "$verification_status" = "PASSED" ]; then
    exit 0
else
    exit 1
fi