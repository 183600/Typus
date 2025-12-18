#!/bin/bash
# 设置locale以避免警告
unset LC_ALL
export LANG=C
export LC_CTYPE=C
export LC_MESSAGES=C

# 运行原始命令
exec 2> >(grep -v "setlocale.*cannot change locale" >&2)
cabal test --flags="-fast production" --test-show-details=direct "$@"