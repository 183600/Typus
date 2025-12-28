#!/usr/bin/env bash
set -u

# ==================== 配置参数 ====================
WATCHDOG_TIMEOUT=900  # 15分钟（秒）
CHECK_INTERVAL=30     # 检查间隔（秒）

# release 窗口：7天
RELEASE_WINDOW_SECONDS=604800

# cabal 测试日志（用于判断是否出现 error）
CABAL_LOG="/tmp/typus_cabal_last.log"

# 心跳文件（使用脚本PID避免冲突）
HEARTBEAT_FILE="/tmp/typus_heartbeat_$$"

# ==================== 清理函数 ====================
cleanup() {
  rm -f "$HEARTBEAT_FILE"
}
trap cleanup EXIT INT TERM

# ==================== 工具：获取文件修改时间（兼容 Linux/macOS） ====================
get_mtime() {
  # Linux: stat -c%Y; macOS/BSD: stat -f %m
  if stat -c%Y "$1" >/dev/null 2>&1; then
    stat -c%Y "$1" 2>/dev/null || echo 0
  else
    stat -f %m "$1" 2>/dev/null || echo 0
  fi
}

# ==================== 监控函数 ====================
monitor_watchdog() {
  master_pid="$1"; timeout="$2"; hb_file="$3"; shift 3

  while [[ ! -f "$hb_file" ]]; do sleep 1; done
  last_heartbeat=$(date +%s)

  while true; do
    sleep $CHECK_INTERVAL

    if [[ -f "$hb_file" ]]; then
      current_time=$(get_mtime "$hb_file")
      (( current_time > last_heartbeat )) && last_heartbeat=$current_time
    fi

    now=$(date +%s)
    elapsed=$((now - last_heartbeat))

    if (( elapsed > timeout )); then
      echo "⚠️ [$(date '+%F %T')] 检测到${timeout}秒内无输出，正在重启..."
      kill -- -"$master_pid" 2>/dev/null || :
      sleep 1
      exec "$0" "$@"
    fi
  done
}

# ==================== 启动监控 ====================
if [[ ! -f "$HEARTBEAT_FILE" ]]; then
  monitor_watchdog "$$" "$WATCHDOG_TIMEOUT" "$HEARTBEAT_FILE" "$@" &
  touch "$HEARTBEAT_FILE"
  sleep 1
fi

# ==================== 工具：带心跳的命令执行（逐行刷新心跳） ====================
run_with_heartbeat() {
  if command -v stdbuf >/dev/null 2>&1; then
    stdbuf -oL -eL "$@" 2>&1 | awk -v hb="$HEARTBEAT_FILE" '{ print; fflush(); system("touch " hb) }'
  else
    "$@" 2>&1 | awk -v hb="$HEARTBEAT_FILE" '{ print; fflush(); system("touch " hb) }'
  fi

  set +u
  local status=${PIPESTATUS[0]:-127}
  set -u
  return "$status"
}

# ==================== Release 相关工具 ====================
extract_cabal_version() {
  # 取第一个包含 version: 的 .cabal 文件版本号
  local f
  f="$(grep -RIl --include='*.cabal' -E '^[[:space:]]*version[[:space:]]*:' . | head -n1 || true)"
  [[ -n "$f" ]] || return 1
  awk 'BEGIN{IGNORECASE=1}
    match($0, /^[[:space:]]*version[[:space:]]*:[[:space:]]*([0-9]+(\.[0-9]+)*)/, m){ print m[1]; exit }
  ' "$f"
}

has_error_in_log() {
  local log="$1"
  [[ -f "$log" ]] || return 1
  # 相对保守：匹配常见致命错误关键词；避免把普通英文句子里的 error 误判
  grep -Eiq '(^|[^[:alpha:]])(error:|fatal:|panic:|exception:|segmentation fault)([^[:alpha:]]|$)' "$log"
}

latest_release_age_ok() {
  # 0 表示 OK（允许发布），1 表示不允许/无法判断（保守跳过）
  # 依赖：gh + GH_TOKEN + GITHUB_REPOSITORY
  command -v gh >/dev/null 2>&1 || return 1
  [[ -n "${GITHUB_REPOSITORY:-}" ]] || return 1

  # gh 会读 GH_TOKEN / GITHUB_TOKEN；这里要求至少一个存在
  if [[ -z "${GH_TOKEN:-}" && -z "${GITHUB_TOKEN:-}" ]]; then
    return 1
  fi

  local published_at pub_ts now_ts delta
  published_at="$(gh api "/repos/${GITHUB_REPOSITORY}/releases/latest" --jq '.published_at' 2>/dev/null || true)"

  # 没有 release（404/空/null）=> 允许发布
  if [[ -z "$published_at" || "$published_at" == "null" ]]; then
    return 0
  fi

  pub_ts="$(date -d "$published_at" +%s 2>/dev/null || echo 0)"
  now_ts="$(date +%s)"
  if [[ "$pub_ts" -le 0 ]]; then
    return 1
  fi

  delta=$(( now_ts - pub_ts ))
  if (( delta >= RELEASE_WINDOW_SECONDS )); then
    return 0
  else
    return 1
  fi
}

attempt_bump_and_release() {
  # 条件：
  # - GitHub Actions 环境（可选，但建议）
  # - 最近 7 天无新 release
  # - bump 版本号（用 iflow）
  # - push master
  # - 创建 GitHub Release

  if [[ "${GITHUB_ACTIONS:-}" != "true" ]]; then
    echo "ℹ️ 非 GitHub Actions 环境，跳过自动发布。"
    return 0
  fi

  if ! latest_release_age_ok; then
    echo "ℹ️ 最近 7 天内已有 release（或无法判断），跳过自动发布。"
    return 0
  fi

  local old_ver new_ver tag

  old_ver="$(extract_cabal_version || true)"
  echo "ℹ️ 当前版本：${old_ver:-<unknown>}"

  echo "🚀 满足发布条件：开始 bump 版本号（iFlow）..."
  run_with_heartbeat iflow '增加版本号(例如0.9.1变成0.9.2) think:high' --yolo || {
    echo "⚠️ bump 版本号失败，跳过本次发布。"
    return 0
  }

  git add -A

  new_ver="$(extract_cabal_version || true)"
  echo "ℹ️ bump 后版本：${new_ver:-<unknown>}"

  if [[ -z "${new_ver}" ]]; then
    echo "⚠️ 无法从 .cabal 提取版本号，跳过本次发布。"
    return 0
  fi

  if [[ -n "${old_ver}" && "${new_ver}" == "${old_ver}" ]]; then
    echo "⚠️ 版本号未变化（${old_ver} -> ${new_ver}），跳过本次发布。"
    return 0
  fi

  if git diff --cached --quiet; then
    echo "⚠️ bump 后没有任何文件变更被 staged，跳过本次发布。"
    return 0
  fi

  git commit -m "chore(release): v${new_ver}" || {
    echo "⚠️ 提交 bump commit 失败，跳过本次发布。"
    return 0
  }

  echo "⬆️ 推送到 master..."
  # 尽量推；如果远端有新提交则保守跳过发布（避免自动 rebase 引发冲突）
  if ! git push origin HEAD:master; then
    echo "⚠️ git push 失败（远端可能领先或网络问题），跳过创建 release。"
    return 0
  fi

  tag="v${new_ver}"

  if ! command -v gh >/dev/null 2>&1; then
    echo "⚠️ 找不到 gh CLI，无法创建 GitHub Release。"
    return 0
  fi

  if gh release view "${tag}" >/dev/null 2>&1; then
    echo "ℹ️ Release ${tag} 已存在，跳过创建。"
    return 0
  fi

  echo "🏷️ 创建 GitHub Release: ${tag}"
  gh release create "${tag}" --target master --generate-notes || {
    echo "⚠️ 创建 GitHub Release 失败。"
    return 0
  }

  echo "✅ 已发布新版本：${tag}"
  return 0
}

# ==================== 主循环 ====================
trap 'echo; echo "已终止."; exit 0' INT TERM

while true; do
  touch "$HEARTBEAT_FILE"

  echo "===================="
  echo "$(date '+%F %T') 运行测试：cabal test --flags=\"-fast production\" --test-show-details=direct"
  echo "===================="

  # 运行测试：保留实时输出 + 写入日志；由 awk 刷新心跳并检测 warning（沿用你原逻辑）
  : > "$CABAL_LOG"

  if command -v stdbuf >/dev/null 2>&1; then
    stdbuf -oL -eL cabal test --flags="-fast production" --test-show-details=direct 2>&1 | \
      stdbuf -oL -eL tee "$CABAL_LOG" | \
      awk -v hb="$HEARTBEAT_FILE" '
        BEGIN { found=0 }
        {
          print
          fflush()
          system("touch " hb)
          l=tolower($0)
          if (l ~ /(warn(ing)?|警告)/) found=1
        }
        END {
          # 0=发现warning，1=未发现（用退出码传递给外层）
          exit found ? 0 : 1
        }
      '
  else
    cabal test --flags="-fast production" --test-show-details=direct 2>&1 | \
      tee "$CABAL_LOG" | \
      awk -v hb="$HEARTBEAT_FILE" '
        BEGIN { found=0 }
        {
          print
          fflush()
          system("touch " hb)
          l=tolower($0)
          if (l ~ /(warn(ing)?|警告)/) found=1
        }
        END {
          exit found ? 0 : 1
        }
      '
  fi

  set +u
  ps0=${PIPESTATUS[0]:-255}  # cabal
  ps1=${PIPESTATUS[1]:-255}  # tee
  ps2=${PIPESTATUS[2]:-255}  # awk（warning 检测）
  set -u

  CABAL_STATUS=$ps0
  TEE_STATUS=$ps1
  AWK_STATUS=$ps2

  # 计算 HAS_WARNINGS（保守处理：拿不到 awk 退出码则认为有 warning）
  if [[ $AWK_STATUS -eq 0 ]]; then
    HAS_WARNINGS=1
  elif [[ $AWK_STATUS -eq 1 ]]; then
    HAS_WARNINGS=0
  else
    echo "⚠️ 未能获取 awk 退出码（AWK_STATUS=$AWK_STATUS），保守起见认为存在 warning"
    HAS_WARNINGS=1
  fi

  # 计算 HAS_ERROR（基于日志关键词；即使 cabal 退出码 0，也要求日志里不要出现明显 error）
  HAS_ERROR=0
  if has_error_in_log "$CABAL_LOG"; then
    HAS_ERROR=1
  fi

  touch "$HEARTBEAT_FILE"

  if [[ $CABAL_STATUS -eq 0 ]]; then
    # 测试通过：让 iflow 增加测试用例（你原逻辑）
    run_with_heartbeat iflow "给这个项目增加一些cabal test测试用例，不要超过10个，如果需要使用QuickCheck就使用QuickCheck think:high" --yolo || :

    git add .
    if git diff --cached --quiet; then
      echo "ℹ️ 没有文件变化可提交"
    else
      git commit -m "测试通过" || :
    fi

    # ==================== 新增：自动 bump + 发布 ====================
    # 条件：测试通过 + 日志无 error + 7天内无新 release
    if [[ $HAS_ERROR -eq 0 ]]; then
      attempt_bump_and_release || :
    else
      echo "ℹ️ 虽然 cabal 退出码为 0，但日志检测到 error 关键词，跳过自动发布。"
    fi

  else
    echo "调用 iflow 修复..."
    run_with_heartbeat iflow '解决cabal test --flags="-fast production" --test-show-details=direct显示的所有问题（除了warning），除非测试用例本身有编译错误，否则只修改测试用例以外的代码，debug时可通过加日志和打断点，一定不要消耗大量CPU/内存资源 think:high' --yolo || :
  fi

  echo "🔁 回到第 1 步..."
  touch "$HEARTBEAT_FILE"
  sleep 1
done