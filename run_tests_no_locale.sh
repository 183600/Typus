#!/bin/bash
# 完全避免 locale 警告的测试脚本
# 设置所有locale相关的环境变量为C
unset LC_ALL
unset LANG
unset LANGUAGE
export LC_CTYPE=C
export LC_NUMERIC=C
export LC_TIME=C
export LC_COLLATE=C
export LC_MONETARY=C
export LC_MESSAGES=C
export LC_PAPER=C
export LC_NAME=C
export LC_ADDRESS=C
export LC_TELEPHONE=C
export LC_MEASUREMENT=C
export LC_IDENTIFICATION=C

# 运行测试
exec cabal test --flags="-fast production" --test-show-details=direct "$@"