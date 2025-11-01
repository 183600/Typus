#!/usr/bin/env bash
set -euo pipefail

# Regenerate heavy-weight example artefacts that were previously committed to the
# repository. This keeps the Git history slim while still allowing contributors
# to reproduce the assets locally when needed.

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EXAMPLES_DIR="${ROOT_DIR}/examples"
OUTPUT_DIR="${EXAMPLES_DIR}/generated"

mkdir -p "${OUTPUT_DIR}"

if ! command -v stack >/dev/null 2>&1; then
  echo "[regenerate-example-artifacts] stack is required to invoke the typus CLI" >&2
  exit 1
fi

declare -A FILES_TO_CONVERT=(
  ["simple_type_system.typus"]="simple_type_system.go"
  ["comprehensive_go_syntax_test.typus"]="comprehensive_go_syntax_test.go"
  ["250913.typus"]="250913.go"
)

for source in "${!FILES_TO_CONVERT[@]}"; do
  input="${EXAMPLES_DIR}/${source}"
  output="${OUTPUT_DIR}/${FILES_TO_CONVERT[$source]}"

  if [[ ! -f "${input}" ]]; then
    echo "[regenerate-example-artifacts] skipping missing input ${input}" >&2
    continue
  fi

  echo "[regenerate-example-artifacts] typus convert ${source} -> ${output}"
  stack exec -- typus convert "${input}" -o "${output}"
done

echo "[regenerate-example-artifacts] artefacts written to ${OUTPUT_DIR}"
