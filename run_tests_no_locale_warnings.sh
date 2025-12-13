#!/bin/bash
# 完全避免 locale 警告的测试运行脚本

# 使用 exec 来替换当前 shell 进程，这样可以避免 bash 的警告
exec env -i LC_ALL=C.UTF-8 LANG=C PATH="$PATH" bash -c '
    cabal test --flags="-fast production" --test-show-details=direct "$@" 2>&1 | grep -v "setlocale.*cannot change locale"
' bash "$@"