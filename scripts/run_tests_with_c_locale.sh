#!/bin/bash

# 设置正确的locale环境变量以避免警告并支持UTF-8字符编码
export LC_ALL=C.utf8
export LANG=C.utf8
export LANGUAGE=C.utf8

# 运行测试
cabal test --flags="-fast production" --test-show-details=direct