#!/bin/bash
# 完全避免 locale 警告
export LC_ALL=C
export LANG=C
export LANGUAGE=C
# 运行测试
cabal test --flags="-fast production" --test-show-details=direct "$@" 2>&1