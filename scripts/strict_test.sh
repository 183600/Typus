#!/bin/bash
# 脚本用于使用最严格的警告标志运行测试

echo "=== 使用最严格的警告标志运行测试 ==="
echo "时间: $(date)"
echo "目录: $(pwd)"
echo

# 清理之前的构建
echo "清理构建缓存..."
cabal clean

# 配置项目以启用所有警告并将警告视为错误
echo "配置项目以启用所有警告并将警告视为错误..."
cabal configure --flags="-fast production" --enable-tests --ghc-options="-Wall -Wextra -Werror -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wmissing-exported-signatures -Wmissing-home-modules -Wpartial-fields -Wredundant-constraints -Wmonomorphism-restriction -Wmissing-signatures -Wname-shadowing -Worphans -Wpartial-type-signatures -Wtabs -Wtype-defaults -Wunrecognised-pragmas -Wunused-do-bind -Wunused-foralls -Wunused-imports -Wunused-matches -Wunused-top-binds -Wunused-type-patterns -Wunused-local-binds -Wmissing-methods"

# 尝试构建项目
echo
echo "=== 尝试构建（如果有警告会失败） ==="
if cabal build 2>&1 | tee strict_build_output.txt; then
    echo "构建成功，没有警告"
else
    echo "构建失败，检测到警告或错误"
    echo "=== 检测到的问题 ==="
    grep -i "warning\|error" strict_build_output.txt | grep -v "Compiling" | grep -v "Linking" | grep -v "Configuring" | grep -v "Preprocessing" | grep -v "Building" | grep -v "Resolving" | grep -v "In order" | grep -v "Build profile" || echo "无法解析错误输出"
fi

# 尝试运行测试
echo
echo "=== 尝试运行测试（如果有警告会失败） ==="
if cabal test --flags="-fast production" --test-show-details=direct 2>&1 | tee strict_test_output.txt; then
    echo "测试成功，没有警告"
else
    echo "测试失败，检测到警告或错误"
    echo "=== 检测到的问题 ==="
    grep -i "warning\|error" strict_test_output.txt | grep -v "OK" | grep -v "RUNNING" | grep -v "PASS" | grep -v "Test" | grep -v "Build profile" | grep -v "In order" | grep -v "will be built" | grep -v "Preprocessing" | grep -v "Building" | grep -v "Linking" | grep -v "Running" || echo "无法解析错误输出"
fi