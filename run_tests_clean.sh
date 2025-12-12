#!/bin/bash
# 设置locale环境以避免警告
export LC_ALL=C
export LANG=C

# 运行原始命令
cabal test --flags="-fast production" --test-show-details=direct "$@"