#!/usr/bin/env bash
# scripts/typus_cabal_loop.sh
#
# 目标：
# - 在 GitHub Actions 一轮运行内反复：
#   1) cabal test
#   2) 失败 -> 用 iflow 修到通过
#   3) 通过 -> 可选做一次小改进（仍需测试通过）
# - 最关键：一旦准备提交（commit）=> 先确保 iflow 已经完成全部修改与验证
# - commit+push 成功后立刻退出（提交后不再修改代码）
#
# 依赖：
# - git, cabal, iflow
#
# 常用环境变量（可选）：
#   RUN_HOURS=5                      单轮最多跑多久（小时）
#   CABAL_TEST_CMD="cabal test all"  测试命令
#   CABAL_IDLE_TIMEOUT_SEC=1800      cabal 无输出超时（秒）
#   IFLOW_IDLE_TIMEOUT_SEC=1800      iflow 无输出超时（秒）
#   IFLOW_TOTAL_TIMEOUT_SEC=3600     iflow 单次总超时（秒）
#   IFLOW_EXTRA_ARGS="--all-files"   传给 iflow 的额外参数
#   IFLOW_MAX_CONSEC_FAIL=3          iflow 连续失败上限
#   LOOP_STOP_FILE=".iflow/STOP"     放这个文件就停机
#   LOOP_DIR=".iflow_loop"           日志目录
#   DO_IMPROVEMENT_ON_PASS=1         测试通过后是否做一次“小步改进”
#   ALLOW_EMPTY_COMMIT=0             没有任何变更时是否允许空提交（一般不建议）
#
set -Eeuo pipefail

ts() { date -u +"%Y-%m-%dT%H:%M:%SZ"; }
log() { printf '[%s] %s\n' "$(ts)" "$*" >&2; }
die() { log "FATAL: $*"; exit 1; }

# ---------------- Config ----------------
RUN_HOURS="${RUN_HOURS:-${1:-5}}"

CABAL_TEST_CMD="${CABAL_TEST_CMD:-cabal test all}"
CABAL_IDLE_TIMEOUT_SEC="${CABAL_IDLE_TIMEOUT_SEC:-1800}"   # 30min

IFLOW_IDLE_TIMEOUT_SEC="${IFLOW_IDLE_TIMEOUT_SEC:-1800}"   # 30min
IFLOW_TOTAL_TIMEOUT_SEC="${IFLOW_TOTAL_TIMEOUT_SEC:-3600}" # 1h
IFLOW_EXTRA_ARGS="${IFLOW_EXTRA_ARGS:---all-files}"
IFLOW_MAX_CONSEC_FAIL="${IFLOW_MAX_CONSEC_FAIL:-3}"

LOOP_STOP_FILE="${LOOP_STOP_FILE:-.iflow/STOP}"
LOOP_DIR="${LOOP_DIR:-.iflow_loop}"

DO_IMPROVEMENT_ON_PASS="${DO_IMPROVEMENT_ON_PASS:-1}"
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

ensure_repo_root() {
  local root
  root="$(git rev-parse --show-toplevel 2>/dev/null || true)"
  [[ -n "$root" ]] || die "Not inside a git repository."
  cd "$root"
}

need_cmd() { command -v "$1" >/dev/null 2>&1 || die "Missing required command: $1"; }

run_with_idle_watchdog() {
  # run_with_idle_watchdog <idle_timeout_sec> <logfile> <cmd...>
  local idle_timeout="$1"; shift
  local logfile="$1"; shift
  local -a cmd=( "$@" )

  : >"$logfile"

  local hb_file
  hb_file="$(mktemp)"
  touch "$hb_file"

  log "RUN: ${cmd[*]}"
  log "LOG: $logfile"
  log "Idle-timeout: ${idle_timeout}s"

  # 用 coproc 读输出，逐行更新心跳
  coproc RUNPROC { "${cmd[@]}" 2>&1; }
  local cmd_pid="$RUNPROC_PID"

  (
    while kill -0 "$cmd_pid" >/dev/null 2>&1; do
      sleep 5
      local now last
      now="$(date +%s)"
      last="$(mtime_epoch "$hb_file" 2>/dev/null || echo 0)"
      if (( now - last > idle_timeout )); then
        log "WATCHDOG: No output for ${idle_timeout}s. Terminating PID=$cmd_pid ..."
        kill -TERM "$cmd_pid" >/dev/null 2>&1 || true
        sleep 10
        kill -KILL "$cmd_pid" >/dev/null 2>&1 || true
        exit 124
      fi
    done
    exit 0
  ) &
  local watchdog_pid="$!"

  set +e
  while IFS= read -r -u "${RUNPROC[0]}" line; do
    printf '%s\n' "$line" | tee -a "$logfile"
    touch "$hb_file" >/dev/null 2>&1 || true
  done

  wait "$cmd_pid"
  local rc=$?

  wait "$watchdog_pid"
  local wd_rc=$?

  rm -f "$hb_file" >/dev/null 2>&1 || true
  set -e

  if [[ "$wd_rc" == "124" ]]; then
    log "WATCHDOG: Command killed due to idle timeout."
    return 124
  fi
  return "$rc"
}

git_has_changes() {
  [[ -n "$(git status --porcelain)" ]]
}

ensure_git_identity() {
  # 如果 workflow 已经配了，这里不会覆盖；否则给默认值
  local name email
  name="$(git config user.name || true)"
  email="$(git config user.email || true)"

  if [[ -z "$name" ]]; then
    git config user.name "github-actions[bot]"
  fi
  if [[ -z "$email" ]]; then
    git config user.email "41898282+github-actions[bot]@users.noreply.github.com"
  fi
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

  # 硬超时，避免单次 iflow 吃满整轮
  if command -v timeout >/dev/null 2>&1; then
    cmd=( timeout --foreground "${IFLOW_TOTAL_TIMEOUT_SEC}" "${cmd[@]}" )
  fi

  run_with_idle_watchdog "$IFLOW_IDLE_TIMEOUT_SEC" "$iflow_log" "${cmd[@]}"
}

commit_and_push_then_exit() {
  local msg="$1"

  ensure_git_identity

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

  # 关键：脚本内 push，避免“脚本 commit 后 workflow 末尾不 push”的断循环
  log "Pushing to origin master..."
  git push origin HEAD:master

  log "Commit+push done. Exiting now (no further modifications after commit)."
  exit 0
}

# ---------------- Validate ----------------
ensure_repo_root
need_cmd git
need_cmd cabal
need_cmd iflow

is_int "$RUN_HOURS" || die "RUN_HOURS must be an integer (hours). Got: $RUN_HOURS"
is_int "$CABAL_IDLE_TIMEOUT_SEC" || die "CABAL_IDLE_TIMEOUT_SEC must be integer. Got: $CABAL_IDLE_TIMEOUT_SEC"
is_int "$IFLOW_IDLE_TIMEOUT_SEC" || die "IFLOW_IDLE_TIMEOUT_SEC must be integer. Got: $IFLOW_IDLE_TIMEOUT_SEC"
is_int "$IFLOW_TOTAL_TIMEOUT_SEC" || die "IFLOW_TOTAL_TIMEOUT_SEC must be integer. Got: $IFLOW_TOTAL_TIMEOUT_SEC"
is_int "$IFLOW_MAX_CONSEC_FAIL" || die "IFLOW_MAX_CONSEC_FAIL must be integer. Got: $IFLOW_MAX_CONSEC_FAIL"

mkdir -p "$LOOP_DIR"

start_ts="$(date +%s)"
end_ts=$(( start_ts + RUN_HOURS * 3600 ))

log "Loop start. RUN_HOURS=${RUN_HOURS}h; end_epoch=${end_ts}"
log "STOP file: ${LOOP_STOP_FILE}"
log "Work dir:  ${LOOP_DIR}"
log "Test cmd:  ${CABAL_TEST_CMD}"

consec_iflow_fail=0
iter=0

# ---------------- Main loop ----------------
while :; do
  iter=$((iter + 1))

  if [[ -f "$LOOP_STOP_FILE" ]]; then
    log "STOP flag found at '${LOOP_STOP_FILE}'. Exiting."
    exit 0
  fi

  now="$(date +%s)"
  if (( now >= end_ts )); then
    # 到点了：如果此时测试通过，则提交；否则不提交（避免把坏状态推上去）
    log "Time budget reached. Running final tests before deciding to commit..."
    final_log="${LOOP_DIR}/cabal_test_final_${iter}.log"
    set +e
    run_with_idle_watchdog "$CABAL_IDLE_TIMEOUT_SEC" "$final_log" bash -lc "$CABAL_TEST_CMD"
    final_rc=$?
    set -e

    if (( final_rc == 0 )); then
      commit_and_push_then_exit "ci: batch update (run ${GITHUB_RUN_ID:-local}, iter ${iter})"
    else
      log "Final tests still failing; exit without commit/push to avoid breaking master."
      exit 1
    fi
  fi

  log "============================================================"
  log "ITERATION #${iter} (remaining=$((end_ts - now))s)"
  git status --porcelain || true

  test_log="${LOOP_DIR}/cabal_test_${iter}.log"
  iflow_log="${LOOP_DIR}/iflow_${iter}.log"

  # 1) 先跑测试，决定走 fix 还是 improve
  set +e
  run_with_idle_watchdog "$CABAL_IDLE_TIMEOUT_SEC" "$test_log" bash -lc "$CABAL_TEST_CMD"
  test_rc=$?
  set -e

  if (( test_rc != 0 )); then
    log "cabal test: FAIL (rc=$test_rc) -> ask iFlow to fix"
    prompt="$(build_prompt_on_fail "$test_log")"
  else
    log "cabal test: PASS"
    if [[ "${DO_IMPROVEMENT_ON_PASS}" == "1" ]]; then
      log "DO_IMPROVEMENT_ON_PASS=1 -> ask iFlow for a small improvement"
      prompt="$(build_prompt_on_pass_improve)"
    else
      # 已经通过且不做改进：直接提交并退出（保证提交后不再修改）
      commit_and_push_then_exit "ci: tests pass (run ${GITHUB_RUN_ID:-local}, iter ${iter})"
    fi
  fi

  # 2) 用 iflow 修改（一定发生在 commit 前）
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

  # 3) iflow 之后立刻再跑测试验证
  verify_log="${LOOP_DIR}/cabal_test_verify_${iter}.log"
  set +e
  run_with_idle_watchdog "$CABAL_IDLE_TIMEOUT_SEC" "$verify_log" bash -lc "$CABAL_TEST_CMD"
  verify_rc=$?
  set -e

  if (( verify_rc == 0 )); then
    log "Verification tests: PASS"
    # 关键：一旦准备提交 -> 立刻 commit+push 并退出（提交后不再修改）
    commit_and_push_then_exit "ci: loop update (run ${GITHUB_RUN_ID:-local}, iter ${iter})"
  else
    log "Verification tests: FAIL (rc=$verify_rc) -> continue loop to fix"
    continue
  fi
done