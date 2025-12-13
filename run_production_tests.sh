#!/bin/bash
# 运行生产模式测试的脚本，自动设置locale以避免警告

export LC_ALL=C
export LANG=C

echo "运行测试命令: cabal test --flags=\"-fast production\" --test-show-details=direct"
cabal test --flags="-fast production" --test-show-details=direct

exit $?