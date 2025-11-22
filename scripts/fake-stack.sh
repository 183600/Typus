#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 || "$1" != "exec" ]]; then
  echo "fake-stack: only 'stack exec typus -- …' is supported" >&2
  exit 2
fi
shift

if [[ $# -lt 1 ]]; then
  echo "fake-stack: missing command" >&2
  exit 3
fi

command_name="$1"
shift

if [[ "$command_name" != "typus" ]]; then
  echo "fake-stack: only the 'typus' executable is supported" >&2
  exit 4
fi

if [[ $# -gt 0 && "$1" == "--" ]]; then
  shift
fi

export TYPUS_SKIP_GO_BUILD="${TYPUS_SKIP_GO_BUILD:-1}"

if command -v cabal >/dev/null 2>&1; then
  cabal run typus -- "$@"
elif command -v runghc >/dev/null 2>&1; then
  runghc -isrc -iapp app/Main.hs "$@"
else
  echo "fake-stack: neither 'cabal' nor 'runghc' is available to run typus" >&2
  exit 5
fi
