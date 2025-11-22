#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "fake-go: expected command" >&2
  exit 2
fi

command_name="$1"
shift

if [[ "$command_name" != "build" ]]; then
  echo "fake-go: only 'build' is supported" >&2
  exit 3
fi

output_path=""
declare -a targets=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    -o)
      shift
      if [[ $# -eq 0 ]]; then
        echo "fake-go: -o requires a path" >&2
        exit 4
      fi
      output_path="$1"
      shift
      ;;
    *)
      targets+=("$1")
      shift
      ;;
  esac
done

if [[ ${#targets[@]} -eq 0 ]]; then
  echo "fake-go: missing Go source file" >&2
  exit 5
fi

target_file="${targets[-1]}"

if [[ ! -f "$target_file" ]]; then
  echo "fake-go: $target_file does not exist" >&2
  exit 6
fi

if ! grep -q "package " "$target_file"; then
  echo "fake-go: $target_file is missing a package declaration" >&2
  exit 7
fi

if [[ -n "$output_path" ]]; then
  mkdir -p "$(dirname "$output_path")"
  : > "$output_path"
fi

exit 0
