#!/bin/bash
# 设置locale以避免警告 - 使用项目标准的 .locale-env 配置
source "$(dirname "$0")/../.locale-env"

# 运行测试
cabal test --flags="-fast production" --test-show-details=direct "$@"