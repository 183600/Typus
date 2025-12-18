#!/bin/bash
# 脚本用于捕获所有测试输出，包括警告和错误

echo "=== 运行测试并捕获所有输出 ==="
echo "时间: $(date)"
echo "目录: $(pwd)"
echo

# 清理之前的构建
echo "清理构建缓存..."
cabal clean

# 配置项目以启用所有警告
echo "配置项目以启用所有警告..."
cabal configure --flags="-fast production" --enable-tests --ghc-options="-Wall -Wextra -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wmissing-exported-signatures -Wmissing-home-modules -Wpartial-fields -Wredundant-constraints -Wmissing-export-lists -Wmonomorphism-restriction -Wmissing-signatures -Wname-shadowing -Worphans -Wpartial-type-signatures -Wtabs -Wtype-defaults -Wunrecognised-pragmas -Wunused-do-bind -Wunused-foralls -Wunused-imports -Wunused-matches -Wunused-top-binds -Wunused-type-patterns"

# 构建项目并捕获所有输出
echo
echo "=== 构建输出 ==="
cabal build 2>&1 | tee build_output.txt

# 运行测试并捕获所有输出
echo
echo "=== 测试输出 ==="
cabal test --flags="-fast production" --test-show-details=direct 2>&1 | tee test_output.txt

# 提取警告和错误
echo
echo "=== 提取的警告和错误 ==="
grep -i "warning\|error" build_output.txt test_output.txt | grep -v "OK" | grep -v "RUNNING" | grep -v "PASS" || echo "未发现警告或错误"

# 显示最终状态
echo
echo "=== 最终状态 ==="
echo "构建退出码: ${PIPESTATUS[0]}"
echo "测试退出码: ${PIPESTATUS[1]}"