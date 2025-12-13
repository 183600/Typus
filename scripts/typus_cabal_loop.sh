#!/usr/bin/env bash
# scripts/typus_cabal_loop.sh
#
# 设计目标（满足你的要求）：
# 1) 循环：cabal test -> (失败则 iflow 修) / (通过则 iflow 小改进可选) -> 再测试验证
# 2) iflow 的修改永远发生在 commit 之前
# 3) 本轮时间结束时，如果当前状态测试通过：commit + push，然后立刻退出（提交后不再修改）
# 4) 本脚本自己负责 push，避免“脚本 commit 了但 workflow 末尾判断无 staged changes 导致不 push”的断循环
#
# 依赖：git, cabal, iflow (iflow-cli)
#
set -Eeuo pipefail

ts() { date -u +"%Y-%m-%dT%H:%M:%SZ"; }
log() { printf '[%s] %s\n' "$(ts)" "$*" >&2; }
die() { log "FATAL: $*"; exit 1; }

# ---------------- Config ----------------
RUN_HOURS="${RUN_HOURS:-${1:-5}}"
TARGET_BRANCH="${TARGET_BRANCH:-master}"

CABAL_TEST_CMD="${CABAL_TEST_CMD:-cabal test all}"
CABAL_IDLE_TIMEOUT_SEC="${CABAL_IDLE_TIMEOUT_SEC:-1800}"   # cabal 无输出超时(秒)，默认30min

IFLOW_IDLE_TIMEOUT_SEC="${IFLOW_IDLE_TIMEOUT_SEC:-1800}"   # iflow 无输出超时(秒)，默认30min
IFLOW_TOTAL_TIMEOUT_SEC="${IFLOW_TOTAL_TIMEOUT_SEC:-3600}" # iflow 单次硬超时(秒)，默认1h
IFLOW_EXTRA_ARGS="${IFLOW_EXTRA_ARGS:---all-files}"
IFLOW_MAX_CONSEC_FAIL="${IFLOW_MAX_CONSEC_FAIL:-3}"

# 放这个文件就停机（你可提交这个文件到仓库来“刹车”）
LOOP_STOP_FILE="${LOOP_STOP_FILE:-.iflow/STOP}"

# 日志目录：优先放到 RUNNER_TEMP，避免被 git add -A 误加入仓库
DEFAULT_LOOP_DIR="${RUNNER_TEMP:-/tmp}/iflow_loop"
LOOP_DIR="${LOOP_DIR:-$DEFAULT_LOOP_DIR}"

# 测试通过时是否让 iflow 做一次小改进（默认开启）
DO_IMPROVEMENT_ON_PASS="${DO_IMPROVEMENT_ON_PASS:-1}"

# 没有任何变更时是否允许空提交（一般不建议）
ALLOW_EMPTY_COMMIT="${ALLOW_EMPTY_COMMIT:-0}"

# ---------------- Helpers ----------------
is_int() { [[ "${1:-}" =~ ^[0-9]+$ ]]; }

mtime_epoch() {
  if stat -c %Y "$1" >/dev/null 2>&1; then
    stat -c %Y "$1"
  else
    stat -f %m "$1"
  fi
}

need_cmd() { command -v "$1" >/dev/null 2>&1 || die "Missing required command: $1"; }

ensure_repo_root() {
  local root
  root="$(git rev-parse --show-toplevel 2>/dev/null || true)"
  [[ -n "$root" ]] || die "Not inside a git repository."
  cd "$root"
}

ensure_git_identity() {
  local name email
  name="$(git config user.name || true)"
  email="$(git config user.email || true)"
  [[ -n "$name"  ]] || git config user.name  "github-actions[bot]"
  [[ -n "$email" ]] || git config user.email "41898282+github-actions[bot]@users.noreply.github.com"
}

git_has_changes() {
  [[ -n "$(git status --porcelain)" ]]
}

# run_with_idle_watchdog <idle_timeout_sec> <logfile> <cmd...>
# - 将 cmd 的 stdout/stderr 合并输出到控制台和 logfile
# - 若 idle_timeout_sec 内完全无新输出，则杀掉 cmd 并返回 124
run_with_idle_watchdog() {
  local idle_timeout="$1"; shift
  local logfile="$1"; shift
  local -a cmd=( "$@" )

  mkdir -p "$(dirname "$logfile")"
  : >"$logfile"

  local fifo hb_file killed_file
  fifo="$(mktemp -u)"
  hb_file="$(mktemp)"
  killed_file="$(mktemp)"
  echo "0" >"$killed_file"
  touch "$hb_file"

  mkfifo "$fifo"

  log "RUN: ${cmd[*]}"
  log "LOG: $logfile"
  log "Idle-timeout: ${idle_timeout}s"

  # reader：从 fifo 读行，更新心跳，并 tee 到控制台+log
  (
    set +e
    while IFS= read -r line; do
      printf '%s\n' "$line"
      touch "$hb_file" >/dev/null 2>&1 || true
    done <"$fifo"
  ) | tee -a "$logfile" &
  local reader_pid=$!

  # 启动命令：把输出写入 fifo
  set +e
  "${cmd[@]}" >"$fifo" 2>&1 &
  local cmd_pid=$!
  set -e

  # watchdog：检测 hb_file 的 mtime，超时则 kill
  (
    while kill -0 "$cmd_pid" >/dev/null 2>&1; do
      sleep 5
      local now last
      now="$(date +%s)"
      last="$(mtime_epoch "$hb_file" 2>/dev/null || echo 0)"
      if (( now - last > idle_timeout )); then
        log "WATCHDOG: No output for ${idle_timeout}s. Terminating PID=$cmd_pid ..."
        echo "1" >"$killed_file"
        kill -TERM "$cmd_pid" >/dev/null 2>&1 || true
        sleep 10
        kill -KILL "$cmd_pid" >/dev/null 2>&1 || true
        exit 0
      fi
    done
    exit 0
  ) &
  local watchdog_pid=$!

  # 等待命令结束拿 rc
  set +e
  wait "$cmd_pid"
  local rc=$?
  set -e

  # 关闭 fifo：命令结束后 writer 已关闭，reader 会自然 EOF；但保险起见清理掉 fifo
  # 等 watchdog/reader 退出
  wait "$watchdog_pid" >/dev/null 2>&1 || true
  wait "$reader_pid"   >/dev/null 2>&1 || true

  local killed
  killed="$(cat "$killed_file" 2>/dev/null || echo 0)"

  rm -f "$fifo" "$hb_file" "$killed_file" >/dev/null 2>&1 || true

  if [[ "$killed" == "1" ]]; then
    return 124
  fi
  return "$rc"
}

build_prompt_on_fail() {
  local test_log="$1"
  cat <<EOF
你在一个 Haskell cabal 项目里工作（Typus）。

当前 \`cabal test\` 失败了。请你：

1) 读取失败日志：@${test_log}
2) 定位失败原因（编译错误、测试失败、依赖/配置问题等）
3) 在仓库里做最小修改以修复问题
4) 必要时补充/修复测试，保证问题不会回归
5) 最后运行：!${CABAL_TEST_CMD}
6) 确保 \`cabal test\` 通过后结束

约束：
- 不要大规模重构；优先小步、可验证、可回滚
- 不要改动无关文件
- 只在本仓库内修改
EOF
}

build_prompt_on_pass_improve() {
  cat <<EOF
你在一个 Haskell cabal 项目里工作（Typus）。

当前测试已通过。请你做一次“小步改进”，并确保仍然通过测试：

1) 选择一个最小、明确、短时间能完成的改进点（例如：修一个小 bug、增强一处边界处理、补一两个关键测试、清理一处明显的错误处理）
2) 实现该改进（尽量少改动）
3) 为改进补充对应测试（必须能在 CI 上运行）
4) 运行并修到通过：!${CABAL_TEST_CMD}
5) 最终保持仓库处于“测试通过”的状态后结束

约束：
- 不要引入大范围架构调整
- 不要添加需要外部服务/网络才能稳定运行的测试
EOF
}

run_iflow_once() {
  local prompt_text="$1"
  local iflow_log="$2"

  local -a cmd=( iflow --yolo --prompt "$prompt_text" )
  if [[ -n "${IFLOW_EXTRA_ARGS:-}" ]]; then
    # shellcheck disable=SC2206
    cmd+=( ${IFLOW_EXTRA_ARGS} )
  fi

  # 给 iflow 一个硬超时（如果 timeout 不存在就直接跑）
  if command -v timeout >/dev/null 2>&1; then
    cmd=( timeout --foreground "${IFLOW_TOTAL_TIMEOUT_SEC}" "${cmd[@]}" )
  fi

  run_with_idle_watchdog "$IFLOW_IDLE_TIMEOUT_SEC" "$iflow_log" "${cmd[@]}"
}

commit_and_push_then_exit() {
  local msg="$1"

  ensure_git_identity

  # 只在 commit 前 add（保证“修改在提交之前”）
  git add -A

  if git diff --cached --quiet; then
    if [[ "${ALLOW_EMPTY_COMMIT}" == "1" ]]; then
      log "No changes staged; creating an empty commit because ALLOW_EMPTY_COMMIT=1."
      git commit --allow-empty -m "$msg"
    else
      log "No changes to commit; exiting without push."
      exit 0
    fi
  else
    git commit -m "$msg"
  fi

  # 为了降低因远端前进导致的 push 失败概率：commit 后 rebase 到远端（必要时）
  # 注意：rebase 会改写 commit hash，但这是 CI 自动提交，一般可接受
  log "Fetching origin/${TARGET_BRANCH}..."
  git fetch origin "${TARGET_BRANCH}" || true

  if git show-ref --verify --quiet "refs/remotes/origin/${TARGET_BRANCH}"; then
    log "Rebasing onto origin/${TARGET_BRANCH}..."
    git rebase "origin/${TARGET_BRANCH}"
  fi

  log "Pushing to origin ${TARGET_BRANCH}..."
  git push origin "HEAD:${TARGET_BRANCH}"

  log "Commit+push done. Exiting now (no further modifications after commit)."
  exit 0
}

# ---------------- Validate ----------------
ensure_repo_root
need_cmd git
need_cmd cabal
need_cmd iflow
need_cmd mkfifo

is_int "$RUN_HOURS" || die "RUN_HOURS must be integer hours. Got: $RUN_HOURS"
is_int "$CABAL_IDLE_TIMEOUT_SEC" || die "CABAL_IDLE_TIMEOUT_SEC must be integer. Got: $CABAL_IDLE_TIMEOUT_SEC"
is_int "$IFLOW_IDLE_TIMEOUT_SEC" || die "IFLOW_IDLE_TIMEOUT_SEC must be integer. Got: $IFLOW_IDLE_TIMEOUT_SEC"
is_int "$IFLOW_TOTAL_TIMEOUT_SEC" || die "IFLOW_TOTAL_TIMEOUT_SEC must be integer. Got: $IFLOW_TOTAL_TIMEOUT_SEC"
is_int "$IFLOW_MAX_CONSEC_FAIL" || die "IFLOW_MAX_CONSEC_FAIL must be integer. Got: $IFLOW_MAX_CONSEC_FAIL"

mkdir -p "$LOOP_DIR"

start_ts="$(date +%s)"
end_ts=$(( start_ts + RUN_HOURS * 3600 ))

log "Loop start. RUN_HOURS=${RUN_HOURS}h; end_epoch=${end_ts}"
log "Branch:    ${TARGET_BRANCH}"
log "STOP file: ${LOOP_STOP_FILE}"
log "Log dir:   ${LOOP_DIR}"
log "Test cmd:  ${CABAL_TEST_CMD}"

consec_iflow_fail=0
iter=0
ever_changed=0

# ---------------- Main loop ----------------
while :; do
  iter=$((iter + 1))

  if [[ -f "$LOOP_STOP_FILE" ]]; then
    log "STOP flag found at '${LOOP_STOP_FILE}'. Exiting without commit/push."
    exit 0
  fi

  now="$(date +%s)"
  if (( now >= end_ts )); then
    log "Time budget reached. Final test before commit..."
    final_log="${LOOP_DIR}/cabal_test_final_${iter}.log"

    set +e
    run_with_idle_watchdog "$CABAL_IDLE_TIMEOUT_SEC" "$final_log" bash -lc "$CABAL_TEST_CMD"
    final_rc=$?
    set -e

    if (( final_rc == 0 )); then
      commit_and_push_then_exit "ci: loop update (run ${GITHUB_RUN_ID:-local}, iters ${iter})"
    else
      log "Final tests failing; exit 1 without commit/push to avoid breaking ${TARGET_BRANCH}."
      exit 1
    fi
  fi

  log "============================================================"
  log "ITERATION #${iter} (remaining=$((end_ts - now))s)"
  git status --porcelain || true

  test_log="${LOOP_DIR}/cabal_test_${iter}.log"
  iflow_log="${LOOP_DIR}/iflow_${iter}.log"
  verify_log="${LOOP_DIR}/cabal_test_verify_${iter}.log"

  # 1) 先跑测试
  set +e
  run_with_idle_watchdog "$CABAL_IDLE_TIMEOUT_SEC" "$test_log" bash -lc "$CABAL_TEST_CMD"
  test_rc=$?
  set -e

  # 2) 根据结果决定 prompt
  if (( test_rc != 0 )); then
    log "cabal test: FAIL (rc=$test_rc) -> ask iFlow to fix"
    prompt="$(build_prompt_on_fail "$test_log")"
  else
    log "cabal test: PASS"
    if [[ "${DO_IMPROVEMENT_ON_PASS}" == "1" ]]; then
      log "DO_IMPROVEMENT_ON_PASS=1 -> ask iFlow for a small improvement"
      prompt="$(build_prompt_on_pass_improve)"
    else
      log "No improvement requested; continue until time budget ends."
      sleep 2
      continue
    fi
  fi

  # 3) iflow 修改（一定发生在 commit 之前）
  pre_status="$(git status --porcelain || true)"

  set +e
  run_iflow_once "$prompt" "$iflow_log"
  iflow_rc=$?
  set -e

  if (( iflow_rc != 0 )); then
    consec_iflow_fail=$((consec_iflow_fail + 1))
    log "iflow: FAIL (rc=$iflow_rc), consecutive_fail=${consec_iflow_fail}"
    if (( consec_iflow_fail >= IFLOW_MAX_CONSEC_FAIL )); then
      die "iflow failed ${consec_iflow_fail} times consecutively (possible API key expired / prompt error)."
    fi
    sleep $(( 10 * consec_iflow_fail ))
    continue
  fi

  consec_iflow_fail=0

  post_status="$(git status --porcelain || true)"
  if [[ "$pre_status" != "$post_status" ]]; then
    ever_changed=1
    log "Repo changed by iFlow. Diff stat:"
    git diff --stat || true
  else
    log "iFlow produced no diff this iteration."
  fi

  # 4) 验证测试：确保循环过程中始终往“可提交状态”收敛
  set +e
  run_with_idle_watchdog "$CABAL_IDLE_TIMEOUT_SEC" "$verify_log" bash -lc "$CABAL_TEST_CMD"
  verify_rc=$?
  set -e

  if (( verify_rc == 0 )); then
    log "Verification tests: PASS (continue looping until time ends, then commit once)."
  else
    log "Verification tests: FAIL (rc=$verify_rc) -> continue loop to fix."
  fi
done