#!/bin/bash
# 设置locale以避免locale警告
export LC_ALL=C
export LANG=C

# 运行测试，过滤掉bash本身的locale警告
cabal test --flags="-fast production" --test-show-details=direct "$@" 2>&1 | grep -v "^/bin/bash: warning: setlocale"