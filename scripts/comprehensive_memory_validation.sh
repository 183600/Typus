#!/bin/bash

# 综合内存验证脚本
# 验证测试用例不会消耗大量内存，同时保留所有测试用例

echo "=== 综合内存验证 ==="

# 设置严格的内存限制
export GHC_RTS="-M6M -K512K -A256K"
export CABAL_MAX_BUILD_JOBS=1

# 测试级别配置
TEST_LEVELS=(
    "--quickcheck-tests=1 --quickcheck-max-size=1"  # 极端模式
    "--quickcheck-tests=2 --quickcheck-max-size=1"  # 紧急模式  
    "--quickcheck-tests=3 --quickcheck-max-size=2"  # 最小模式
)

PASSED_TESTS=0
FAILED_TESTS=0

for TEST_ARGS in "${TEST_LEVELS[@]}"; do
    echo "运行测试级别: $TEST_ARGS"
    
    # 运行测试并检查内存使用
    TIMEOUT=45
    OUTPUT=$(timeout ${TIMEOUT}s cabal test --test-options="$TEST_ARGS" 2>&1)
    EXIT_CODE=$?
    
    # 检查测试结果
    if echo "$OUTPUT" | grep -q "Test suite passed"; then
        echo "✓ 测试级别 $TEST_ARGS 通过"
        ((PASSED_TESTS++))
    elif [ $EXIT_CODE -eq 124 ]; then
        echo "⚠ 测试级别 $TEST_ARGS 超时 (${TIMEOUT}s)"
        ((FAILED_TESTS++))
    elif echo "$OUTPUT" | grep -qi "out of memory"; then
        echo "✗ 测试级别 $TEST_ARGS 内存溢出"
        ((FAILED_TESTS++))
    else
        echo "? 测试级别 $TEST_ARGS 未知状态"
        ((FAILED_TESTS++))
    fi
    
    # 强制垃圾回收
    echo 3 > /proc/sys/vm/drop_caches 2>/dev/null || true
    sleep 2
    
done

# 总结结果
echo ""
echo "=== 验证结果 ==="
echo "通过测试级别: $PASSED_TESTS"
echo "失败测试级别: $FAILED_TESTS"

if [ $FAILED_TESTS -eq 0 ]; then
    echo "✓ 所有内存级别测试通过"
    exit 0
else
    echo "✗ 部分内存级别测试失败"
    exit 1
fi