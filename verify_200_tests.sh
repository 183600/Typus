#!/bin/bash

echo "验证新添加的200个测试用例..."
echo "========================================"

# 检查测试文件是否存在
if [ -f "test/Test/Unit/FinalExact200QuickCheckTests.hs" ]; then
    echo "✓ FinalExact200QuickCheckTests.hs 文件存在"
else
    echo "✗ FinalExact200QuickCheckTests.hs 文件不存在"
    exit 1
fi

# 统计测试数量
test_count=$(grep -c "testProperty" test/Test/Unit/FinalExact200QuickCheckTests.hs)
echo "✓ 测试数量: $test_count"

if [ $test_count -eq 200 ]; then
    echo "✓ 测试数量符合要求 (200个)"
else
    echo "✗ 测试数量不符合要求 (需要200个，实际有 $test_count 个)"
fi

# 检查typus.cabal中的模块引用
if grep -q "Test.Unit.FinalExact200QuickCheckTests" typus.cabal; then
    echo "✓ FinalExact200QuickCheckTests 已添加到typus.cabal"
else
    echo "✗ FinalExact200QuickCheckTests 未添加到typus.cabal"
fi

# 检查Tests.hs中的导入
if grep -q "import qualified Test.Unit.FinalExact200QuickCheckTests" test/Test/Unit/Tests.hs; then
    echo "✓ FinalExact200QuickCheckTests 已在Tests.hs中导入"
else
    echo "✗ FinalExact200QuickCheckTests 未在Tests.hs中导入"
fi

# 检查测试组合
if grep -q "FinalExact200QuickCheckTests.exact200QuickCheckTests" test/Test/Unit/Tests.hs; then
    echo "✓ FinalExact200QuickCheckTests 已添加到测试组合"
else
    echo "✗ FinalExact200QuickCheckTests 未添加到测试组合"
fi

# 统计各模块的测试数量
core_utils_count=$(grep -c "prop_trim\|prop_split\|prop_remove\|prop_is\|prop_break\|prop_safe\|prop_normalize" test/Test/Unit/FinalExact200QuickCheckTests.hs)
parser_count=$(grep -c "prop_parse" test/Test/Unit/FinalExact200QuickCheckTests.hs)
compiler_count=$(grep -c "prop_compile\|prop_ir\|prop_type\|prop_ownership\|prop_go\|prop_lexer\|prop_error" test/Test/Unit/FinalExact200QuickCheckTests.hs)
dependency_count=$(grep -c "prop_dependency" test/Test/Unit/FinalExact200QuickCheckTests.hs)
ownership_count=$(grep -c "prop_ownership" test/Test/Unit/FinalExact200QuickCheckTests.hs)
error_count=$(grep -c "prop_error" test/Test/Unit/FinalExact200QuickCheckTests.hs)

echo ""
echo "各模块测试分布:"
echo "- 核心工具函数测试: $core_utils_count 个"
echo "- 解析器测试: $parser_count 个"
echo "- 编译器核心测试: $compiler_count 个"
echo "- 依赖分析测试: $dependency_count 个"
echo "- 所有权分析测试: $ownership_count 个"
echo "- 错误处理测试: $error_count 个"

echo ""
echo "========================================"
echo "验证完成！"