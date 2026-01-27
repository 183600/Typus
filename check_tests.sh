#!/bin/bash

# 运行测试并保存输出
echo "Running tests..."
GHCRTS="-M2G -A16m" stack test \
      --flag "*:fast" \
      --flag "*:-production" \
      --ghc-options="-O0 -rtsopts" \
      --test-arguments="+RTS -M1024m -A16m -RTS" \
      --jobs=1 2>&1 | tee detailed_test_output.log

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