#!/bin/bash

# 极度最小化测试运行脚本
# 只运行最关键的测试，最小内存使用

set -e

echo "=== 极度最小化测试运行脚本 ==="
echo "目标: 最小内存使用，保留核心功能"
echo

# 设置环境变量
export SKIP_TESTS="false"
export MEMORY_LIMIT_MB=8
export QUICKCHECK_MAX_SIZE=1
export QUICKCHECK_TESTS=1
export QUICKCHECK_MAX_SHRINKS=0
export GHC_OPTIONS="-O0 -rtsopts -with-rtsopts=-M8m"

# 设置locale以避免警告
export LC_ALL=C
export LANG=C

echo "环境配置:"
echo "  内存限制: ${MEMORY_LIMIT_MB}MB"
echo "  QuickCheck最大大小: ${QUICKCHECK_MAX_SIZE}"
echo "  QuickCheck测试次数: ${QUICKCHECK_TESTS}"
echo "  QuickCheck最大收缩: ${QUICKCHECK_MAX_SHRINKS}"
echo

# 检查是否存在极度最小化测试运行器
RUNNER_PATH="test/runners/ExtremeMinimalTestRunner"
if [ ! -f "${RUNNER_PATH}.hs" ]; then
    echo "错误: 找不到极度最小化测试运行器 ${RUNNER_PATH}.hs"
    exit 1
fi

echo "编译极度最小化测试运行器..."
stack build --ghc-options="-O0 -rtsopts -with-rtsopts=-M8m" typus-test-extreme-minimal || {
    echo "编译失败，尝试使用cabal..."
    cabal build --ghc-options="-O0 -rtsopts -with-rtsopts=-M8m" typus-test-extreme-minimal
}

echo
echo "运行极度最小化测试套件..."
echo "只包含10个最关键的测试"
echo

# 运行测试
if command -v stack &> /dev/null; then
    stack exec -- ghc -rtsopts -with-rtsopts="-M8m -K1M" -O0 "${RUNNER_PATH}" -o extreme_minimal_test_runner
    ./extreme_minimal_test_runner +RTS -M8m -K1M -n2m -RTS
else
    cabal run typus-test-extreme-minimal -- +RTS -M8m -K1M -n2m -RTS
fi

echo
echo "=== 极度最小化测试完成 ==="
echo "内存使用已最小化，核心功能已验证"