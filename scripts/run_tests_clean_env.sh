#!/bin/bash
# 使用env命令完全隔离locale环境
env -i LC_CTYPE=C LC_NUMERIC=C LC_TIME=C LC_COLLATE=C LC_MONETARY=C LC_MESSAGES=C LC_PAPER=C LC_NAME=C LC_ADDRESS=C LC_TELEPHONE=C LC_MEASUREMENT=C LC_IDENTIFICATION=C PATH="$PATH" HOME="$HOME" USER="$USER" SHELL="$SHELL" cabal test --flags="-fast production" --test-show-details=direct "$@"