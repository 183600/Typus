#!/bin/bash
# 使用env命令设置locale以避免bash启动时的警告
env -u LC_ALL LC_ALL=C LANG=C LC_CTYPE=C LC_MESSAGES=C LC_COLLATE=C cabal test --flags="-fast production" --test-show-details=direct "$@"