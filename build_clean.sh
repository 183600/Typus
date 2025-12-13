#!/bin/bash
# 构建项目的脚本，确保使用正确的 locale 设置

# 设置 locale 以避免警告
export LC_ALL=C.UTF-8
export LANG=C

# 构建项目
cabal build --flags="-fast production" --ghc-options="-Wall" "$@"