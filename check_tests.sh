#!/bin/bash

# 运行测试并保存输出
echo "Running super memory optimized tests..."

# 检查是否有超级内存优化测试运行器
if [ -f "test/SuperMemoryOptimized.hs" ]; then
    echo "Using Super Memory Optimized Test Runner..."
    
    # 使用极简环境运行测试
    cd test
    GHCRTS="-M64m -A8m" stack runghc SuperMemoryOptimized.hs minimal \
          --flag "*:fast" \
          --flag "*:-production" \
          --ghc-options="-O0 -rtsopts" \
          2>&1 | tee ../detailed_test_output.log
    cd ..
    
    # 检查测试结果
    if [ $? -eq 0 ]; then
        echo "Super memory optimized tests passed!"
    else
        echo "Super memory optimized tests failed, falling back to standard tests..."
        # 回退到标准测试
        GHCRTS="-M512m -A8m" stack test \
              --flag "*:fast" \
              --flag "*:-production" \
              --ghc-options="-O0 -rtsopts" \
              --test-arguments="+RTS -M256m -A8m -RTS" \
              --jobs=1 2>&1 | tee detailed_test_output.log
    fi
else
    echo "Super Memory Optimized Test Runner not found, using standard tests..."
    # 使用保守的内存设置运行标准测试
    GHCRTS="-M512m -A8m" stack test \
          --flag "*:fast" \
          --flag "*:-production" \
          --ghc-options="-O0 -rtsopts" \
          --test-arguments="+RTS -M256m -A8m -RTS" \
          --jobs=1 2>&1 | tee detailed_test_output.log
fi

# 分析测试结果
echo ""
echo "=== Test Analysis ==="
echo "Checking for failed tests..."
if grep -q "failed\|FAIL\|Error" detailed_test_output.log | grep -v "properties:" | grep -v "OK"; then
    echo "Found some failed tests or errors!"
else
    echo "No failed tests found."
fi

echo ""
echo "Checking for warnings..."
if grep -q "Warning:" detailed_test_output.log; then
    echo "Found warnings:"
    grep "Warning:" detailed_test_output.log
else
    echo "No warnings found."
fi

echo ""
echo "Checking test summary..."
if grep -q "All.*tests passed" detailed_test_output.log; then
    echo "All tests passed successfully!"
    grep "All.*tests passed" detailed_test_output.log
else
    echo "Could not confirm all tests passed."
fi

echo ""
echo "Checking for test suite completion..."
if grep -q "Test suite.*passed" detailed_test_output.log; then
    echo "Test suites completed successfully:"
    grep "Test suite.*passed" detailed_test_output.log
else
    echo "Could not confirm test suite completion."
fi