#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -eq 0 ]; then
  exit 0
fi

formatter=""
declare -a formatter_args

if command -v fourmolu >/dev/null 2>&1; then
  formatter="fourmolu"
  formatter_args=(--mode inplace)
elif command -v ormolu >/dev/null 2>&1; then
  formatter="ormolu"
  formatter_args=(--mode inplace)
elif command -v stylish-haskell >/dev/null 2>&1; then
  formatter="stylish-haskell"
  formatter_args=(-i)
else
  echo "format-haskell.sh: no formatter found (fourmolu, ormolu, stylish-haskell); skipping formatting" >&2
  exit 0
fi

for file in "$@"; do
  "$formatter" "${formatter_args[@]}" "$file"
done
