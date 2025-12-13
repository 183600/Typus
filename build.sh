#!/bin/bash

# 设置locale以避免警告
export LC_ALL=C
export LANG=C
export LC_CTYPE=C
export LC_MESSAGES=C
export LC_COLLATE=C

# 运行构建
cabal build --flags="-fast production"