#!/bin/bash
# 使用项目 locale 设置以避免编码错误和减少警告
source "$(dirname "$0")/../.locale-env"
# 运行测试
cabal test --flags="-fast production" --test-show-details=direct "$@" 2>&1