#!/bin/bash
# 设置正确的locale以避免警告
unset LANG
unset LC_ALL
unset LANGUAGE
export LC_CTYPE=C
export LC_NUMERIC=C
export LC_TIME=C
export LC_COLLATE=C
export LC_MONETARY=C
export LC_MESSAGES=C
export LC_PAPER=C
export LC_NAME=C
export LC_ADDRESS=C
export LC_TELEPHONE=C
export LC_MEASUREMENT=C
export LC_IDENTIFICATION=C
# 确保必要的环境变量存在
export HOME="/home/runner"
export XDG_CACHE_HOME="/home/runner/.cache"
export GOCACHE="/home/runner/.cache/go-build"
export PATH="/home/runner/.ghcup/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
# 运行原始命令
exec cabal test --flags="-fast production" --test-show-details=direct "$@"