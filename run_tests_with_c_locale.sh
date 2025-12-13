#!/bin/bash

# 设置正确的locale环境变量以避免警告
export LC_ALL=C
export LANG=C
export LANGUAGE=C

# 运行测试
cabal test --flags="-fast production" --test-show-details=direct