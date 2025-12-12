#!/bin/bash
# 设置正确的locale以避免警告
export LANG=C
export LC_ALL=C
# 运行原始命令
cabal test --flags="-fast production" --test-show-details=direct "$@"