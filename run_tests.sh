#!/bin/bash
# 运行Typus测试的正确方式，避免locale警告
exec ./run_with_correct_locale.sh cabal test --flags="-fast production" --test-show-details=direct "$@"