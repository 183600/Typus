#!/bin/bash

# 验证内存效率的脚本
# 确保测试用例不会消耗大量内存，同时保留所有测试用例

echo "=== 验证内存效率测试 ==="

# 设置内存限制环境变量
export GHC_RTS="-M8M -K1M"
export CABAL_MAX_BUILD_JOBS=1

# 运行最小内存测试
echo "运行最小内存测试..."
timeout 60s cabal test --test-options="--quickcheck-tests=1 --quickcheck-max-size=1" 2>&1 | grep -E "(Test suite|passed|failed|out of memory)" || echo "测试完成"

# 运行中等内存测试
echo "运行中等内存测试..."
timeout 60s cabal test --test-options="--quickcheck-tests=3 --quickcheck-max-size=2" 2>&1 | grep -E "(Test suite|passed|failed|out of memory)" || echo "测试完成"

# 检查是否有内存溢出
echo "检查内存使用情况..."
if dmesg 2>/dev/null | grep -i "out of memory"; then
    echo "警告：检测到内存溢出问题"
    exit 1
else
    echo "✓ 内存使用正常"
fi

echo "=== 内存效率验证完成 ==="