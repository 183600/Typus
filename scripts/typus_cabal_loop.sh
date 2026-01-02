#!/usr/bin/env bash
set -u
set -o pipefail

RELEASE_WINDOW_SECONDS=604800
CABAL_LOG="/tmp/typus_cabal_last.log"

WORK_BRANCH="${WORK_BRANCH:-master}"

AUTO_PUSH_PROGRESS="${AUTO_PUSH_PROGRESS:-1}"
PUSH_MIN_INTERVAL_SECONDS="${PUSH_MIN_INTERVAL_SECONDS:-300}"
LAST_PUSH_TS_FILE="/tmp/typus_last_push_ts"

GIT_DIR_REAL="$(git rev-parse --git-dir 2>/dev/null || echo ".git")"
RELEASE_MARKER_FILE="${RELEASE_MARKER_FILE:-${GIT_DIR_REAL%/}/typus_release_tag}"

extract_cabal_version() {
  local f ver
  f="$(find . -name '*.cabal' \
        -not -path './dist-newstyle/*' \
        -not -path './.git/*' \
        -print -quit 2>/dev/null || true)"
  [[ -n "${f:-}" ]] || return 1

  ver="$(sed -nE 's/^[[:space:]]*[Vv]ersion[[:space:]]*:[[:space:]]*([0-9]+(\.[0-9]+)*)[[:space:]]*.*$/\1/p' "$f" | head -n1 || true)"
  [[ -n "${ver:-}" ]] || return 1
  printf '%s\n' "$ver"
}

has_error_in_log() {
  local log="$1"
  [[ -f "$log" ]] || return 1
  grep -Eiq '(^|[^[:alpha:]])(error:|fatal:|panic:|exception:|segmentation fault)([^[:alpha:]]|$)' "$log"
}

head_has_skip_ci() {
  local msg
  msg="$(git log -1 --pretty=%B 2>/dev/null || true)"
  echo "${msg}" | grep -Eiq '\[(skip ci|ci skip)\]'
}

maybe_push_progress() {
  [[ "${AUTO_PUSH_PROGRESS}" = "1" ]] || return 0
  head_has_skip_ci || return 0

  local now last=0 delta
  now="$(date +%s)"
  if [[ -f "${LAST_PUSH_TS_FILE}" ]]; then
    last="$(cat "${LAST_PUSH_TS_FILE}" 2>/dev/null || echo 0)"
  fi
  delta=$(( now - last ))
  if (( delta < PUSH_MIN_INTERVAL_SECONDS )); then
    return 0
  fi

  # 只 push 分支，不 push tag（避免触发 release-on-tag）
  git push --quiet origin "HEAD:${WORK_BRANCH}" 2>/dev/null || true
  printf '%s\n' "${now}" > "${LAST_PUSH_TS_FILE}" || true
}

latest_release_age_ok() {
  command -v gh >/dev/null 2>&1 || return 1
  [[ -n "${GITHUB_REPOSITORY:-}" ]] || return 1
  if [[ -z "${GH_TOKEN:-}" && -z "${GITHUB_TOKEN:-}" ]]; then
    return 1
  fi

  local published_at pub_ts now_ts delta
  published_at="$(gh api "/repos/${GITHUB_REPOSITORY}/releases/latest" --jq '.published_at' 2>/dev/null || true)"
  if [[ -z "${published_at:-}" || "${published_at}" == "null" ]]; then
    return 0
  fi

  pub_ts="$(date -d "$published_at" +%s 2>/dev/null || echo 0)"
  now_ts="$(date +%s)"
  [[ "$pub_ts" -gt 0 ]] || return 1

  delta=$(( now_ts - pub_ts ))
  (( delta >= RELEASE_WINDOW_SECONDS ))
}

attempt_bump_and_tag() {
  if [[ "${GITHUB_ACTIONS:-}" != "true" ]]; then
    return 0
  fi

  if [[ -f "$RELEASE_MARKER_FILE" ]]; then
    return 0
  fi

  if ! latest_release_age_ok; then
    return 0
  fi

  git fetch --tags --force >/dev/null 2>&1 || true

  local old_ver new_ver tag
  old_ver="$(extract_cabal_version || true)"

  iflow '增加版本号(例如0.9.1变成0.9.2) think:high' --yolo || return 0

  git add -A

  new_ver="$(extract_cabal_version || true)"
  [[ -n "${new_ver:-}" ]] || return 0
  if [[ -n "${old_ver:-}" && "${new_ver}" == "${old_ver}" ]]; then
    return 0
  fi
  if git diff --cached --quiet; then
    return 0
  fi

  # 发布 commit 不带 [skip ci]，让 tag push 能触发 release-on-tag
  git commit -m "chore(release): v${new_ver}" || return 0

  tag="v${new_ver}"
  if ! git rev-parse -q --verify "refs/tags/${tag}" >/dev/null; then
    git tag -a "${tag}" -m "${tag}" || return 0
  fi

  mkdir -p "$(dirname -- "$RELEASE_MARKER_FILE")"
  printf '%s\n' "${tag}" > "$RELEASE_MARKER_FILE"
}

trap 'exit 0' INT TERM

while true; do
  : > "$CABAL_LOG"

  cabal test --flags="-fast production" --test-show-details=direct 2>&1 | tee "$CABAL_LOG"
  ps=("${PIPESTATUS[@]}")
  CABAL_STATUS="${ps[0]:-255}"

  HAS_ERROR=0
  if has_error_in_log "$CABAL_LOG"; then
    HAS_ERROR=1
  fi

  if [[ "$CABAL_STATUS" -eq 0 ]]; then
    iflow "给这个项目增加一些cabal test测试用例，不要超过10个，如果需要使用QuickCheck就使用QuickCheck think:high" --yolo || true

    git add -A
    if ! git diff --cached --quiet; then
      git commit -m "测试通过 [skip ci]" || true
      maybe_push_progress || true
    fi

    if [[ "$HAS_ERROR" -eq 0 ]]; then
      attempt_bump_and_tag || true
    fi
  else
    iflow '解决cabal test --flags="-fast production" --test-show-details=direct显示的所有问题（除了warning），除非测试用例本身有编译错误，否则只修改测试用例以外的代码，debug时可通过加日志和打断点，一定不要消耗大量CPU/内存资源 think:high' --yolo || true
  fi

  sleep 1
done
