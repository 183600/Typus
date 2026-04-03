#!/bin/bash
# 完全避免 locale 警告的测试脚本
# 设置所有locale相关的环境变量为C.utf8以支持UTF-8字符编码
unset LC_ALL
unset LANG
unset LANGUAGE
export LC_ALL=C.utf8
export LANG=C.utf8
export LC_CTYPE=C.utf8
export LC_NUMERIC=C.utf8
export LC_TIME=C.utf8
export LC_COLLATE=C.utf8
export LC_MONETARY=C.utf8
export LC_MESSAGES=C.utf8
export LC_PAPER=C.utf8
export LC_NAME=C.utf8
export LC_ADDRESS=C.utf8
export LC_TELEPHONE=C.utf8
export LC_MEASUREMENT=C.utf8
export LC_IDENTIFICATION=C.utf8

# 运行测试
exec cabal test --flags="-fast production" --test-show-details=direct "$@"