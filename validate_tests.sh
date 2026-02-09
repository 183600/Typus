#!/bin/bash

# 简单的测试验证脚本
echo "验证新添加的测试文件..."

# 检查测试文件是否存在
test_files=(
    "test/Test/Unit/CoreUtilsQuickCheckTests.hs"
    "test/Test/Unit/ParserQuickCheckTests.hs"
    "test/Test/Unit/CompilerCoreQuickCheckTests.hs"
    "test/Test/Unit/DependencyAnalysisQuickCheckTests.hs"
    "test/Test/Unit/OwnershipAnalysisQuickCheckTests.hs"
    "test/Test/Unit/ErrorHandlingQuickCheckTests.hs"
    "test/Test/Unit/ComprehensiveQuickCheckTestSuite.hs"
)

missing_files=()
for file in "${test_files[@]}"; do
    if [ -f "$file" ]; then
        echo "✓ $file 存在"
        # 统计测试属性数量
        properties=$(grep -c "testProperty\|prop_" "$file" 2>/dev/null || echo "0")
        echo "  包含 $properties 个测试属性"
    else
        echo "✗ $file 不存在"
        missing_files+=("$file")
    fi
done

# 检查typus.cabal是否包含新模块
echo ""
echo "检查typus.cabal中的模块引用..."
if grep -q "Test.Unit.CoreUtilsQuickCheckTests" typus.cabal; then
    echo "✓ CoreUtilsQuickCheckTests 已添加到cabal文件"
else
    echo "✗ CoreUtilsQuickCheckTests 未添加到cabal文件"
fi

if grep -q "Test.Unit.ParserQuickCheckTests" typus.cabal; then
    echo "✓ ParserQuickCheckTests 已添加到cabal文件"
else
    echo "✗ ParserQuickCheckTests 未添加到cabal文件"
fi

if grep -q "Test.Unit.CompilerCoreQuickCheckTests" typus.cabal; then
    echo "✓ CompilerCoreQuickCheckTests 已添加到cabal文件"
else
    echo "✗ CompilerCoreQuickCheckTests 未添加到cabal文件"
fi

if grep -q "Test.Unit.DependencyAnalysisQuickCheckTests" typus.cabal; then
    echo "✓ DependencyAnalysisQuickCheckTests 已添加到cabal文件"
else
    echo "✗ DependencyAnalysisQuickCheckTests 未添加到cabal文件"
fi

if grep -q "Test.Unit.OwnershipAnalysisQuickCheckTests" typus.cabal; then
    echo "✓ OwnershipAnalysisQuickCheckTests 已添加到cabal文件"
else
    echo "✗ OwnershipAnalysisQuickCheckTests 未添加到cabal文件"
fi

if grep -q "Test.Unit.ErrorHandlingQuickCheckTests" typus.cabal; then
    echo "✓ ErrorHandlingQuickCheckTests 已添加到cabal文件"
else
    echo "✗ ErrorHandlingQuickCheckTests 未添加到cabal文件"
fi

if grep -q "Test.Unit.ComprehensiveQuickCheckTestSuite" typus.cabal; then
    echo "✓ ComprehensiveQuickCheckTestSuite 已添加到cabal文件"
else
    echo "✗ ComprehensiveQuickCheckTestSuite 未添加到cabal文件"
fi

# 统计总测试数量
echo ""
echo "测试统计："
total_properties=0
for file in "${test_files[@]}"; do
    if [ -f "$file" ]; then
        properties=$(grep -c "testProperty\|prop_" "$file" 2>/dev/null || echo "0")
        total_properties=$((total_properties + properties))
    fi
done

echo "总共添加了约 $total_properties 个QuickCheck测试属性"

# 检查是否在200个测试的限制内
if [ $total_properties -le 200 ]; then
    echo "✓ 测试数量在要求的200个限制内"
else
    echo "✗ 测试数量超过200个限制"
fi

echo ""
echo "测试验证完成！"