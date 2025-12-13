#!/bin/bash
# 设置环境变量以避免 locale 警告
export LC_ALL=C
export LANG=C

# 运行测试
cabal test --flags="-fast production" --test-show-details=direct "$@"