#!/bin/bash
# 完全修复locale问题的测试脚本
# 使用env命令确保环境变量在子进程中正确设置
env -u LC_ALL -u LANG -u LC_CTYPE -u LC_MESSAGES -u LC_COLLATE -u LC_MONETARY -u LC_NUMERIC -u LC_TIME LC_ALL=C LANG=C LC_CTYPE=C LC_MESSAGES=C LC_COLLATE=C LC_MONETARY=C LC_NUMERIC=C LC_TIME=C cabal test --flags="-fast production" --test-show-details=direct "$@"