#!/bin/bash
# 设置正确的 locale 以避免警告
export LC_ALL=C.UTF-8
export LANG=C.UTF-8

# 运行测试
cabal test --flags="-fast production" "$@"