#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "fake-go: expected command" >&2
  exit 2
fi

command_name="$1"
shift

ensure_go_source() {
  local file="$1"

  if [[ ! -f "$file" ]]; then
    echo "fake-go: $file does not exist" >&2
    exit 6
  fi

  if ! grep -q "package " "$file"; then
    echo "fake-go: $file is missing a package declaration" >&2
    exit 7
  fi
}

handle_build() {
  local output_path=""
  local -a targets=()

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

  local target_file="${targets[-1]}"
  ensure_go_source "$target_file"

  if [[ -n "$output_path" ]]; then
    mkdir -p "$(dirname "$output_path")"
    : > "$output_path"
  fi
}

handle_run() {
  local -a go_sources=()

  while [[ $# -gt 0 ]]; do
    local token="$1"
    shift

    if [[ "$token" == "--" ]]; then
      break
    fi

    if [[ "$token" == -* && ${#go_sources[@]} -eq 0 ]]; then
      continue
    fi

    if [[ "$token" == -* && ${#go_sources[@]} -gt 0 ]]; then
      break
    fi

    if [[ "$token" == *.go ]]; then
      go_sources+=("$token")
      continue
    fi

    if [[ ${#go_sources[@]} -eq 0 ]]; then
      go_sources+=("$token")
    else
      break
    fi
  done

  if [[ ${#go_sources[@]} -eq 0 ]]; then
    echo "fake-go: missing Go source file" >&2
    exit 5
  fi

  for source in "${go_sources[@]}"; do
    ensure_go_source "$source"
  done
}

case "$command_name" in
  build)
    handle_build "$@"
    ;;
  run)
    handle_run "$@"
    ;;
  *)
    echo "fake-go: only 'build' and 'run' are supported" >&2
    exit 3
    ;;
esac

exit 0
