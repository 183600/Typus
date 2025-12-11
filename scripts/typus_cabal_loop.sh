#!/usr/bin/env bash
set -u

# ==================== 配置参数 ====================
LOG_FILE="cabal_test.log"
WATCHDOG_TIMEOUT=900  # 15分钟（秒）
CHECK_INTERVAL=30     # 检查间隔（秒）

# 心跳文件（使用脚本PID避免冲突）
HEARTBEAT_FILE="/tmp/typus_heartbeat_$$"

# ==================== 清理函数 ====================
cleanup() {
  rm -f "$HEARTBEAT_FILE"
}
trap cleanup EXIT INT TERM

# ==================== 监控函数（修复 local 用法） ====================
monitor_watchdog() {
  master_pid="$1"; timeout="$2"; hb_file="$3"; shift 3

  while [[ ! -f "$hb_file" ]]; do sleep 1; done
  last_heartbeat=$(date +%s)

  while true; do
    sleep $CHECK_INTERVAL

    if [[ -f "$hb_file" ]]; then
      current_time=$(stat -c%Y "$hb_file" 2>/dev/null || echo 0)
      (( current_time > last_heartbeat )) && last_heartbeat=$current_time
    fi

    now=$(date +%s)
    elapsed=$((now - last_heartbeat))

    if (( elapsed > timeout )); then
      echo "⚠️ [$(date '+%F %T')] 检测到${timeout}秒内无输出，正在重启..."
      kill -- -"$master_pid" 2>/dev/null || true
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

# ==================== 主循环 ====================
trap 'echo; echo "已终止."; exit 0' INT TERM

while true; do
  touch "$HEARTBEAT_FILE"

  > "$LOG_FILE"
  {
    echo "===================="
    echo "$(date '+%F %T') 运行测试：cabal test --flags=\"-fast production\" --test-show-details=direct"
    echo "===================="
  } > "$LOG_FILE"

  cabal test --flags="-fast production" --test-show-details=direct 2>&1 | tee -a "$LOG_FILE"
  CABAL_STATUS=${PIPESTATUS[0]}

  touch "$HEARTBEAT_FILE"

  if grep -Eiq '\b(warn(ing)?|警告)\b' "$LOG_FILE"; then
    HAS_WARNINGS=1
  else
    HAS_WARNINGS=0
  fi

  if [[ $CABAL_STATUS -eq 0 && $HAS_WARNINGS -eq 0 ]]; then
    {
      echo "✅ 未发现任何问题（包括 warning）——进行提交并增加测试用例"
      git add .
      if git diff --cached --quiet; then
        echo "ℹ️ 没有文件变化可提交"
      else
        git commit -m "测试通过" || true
      fi
      iflow "给这个项目增加大量的测试用例" --yolo
    } >> "$LOG_FILE" 2>&1
    cat "$LOG_FILE"
  else
    {
      echo "⚠️ 发现问题或 warning（退出码=$CABAL_STATUS），调用 iflow 修复..."
      iflow '解决cabal test --flags="-fast production" --test-show-details=direct显示的所有问题（包括warning），除非测试用例本身有编译错误，否则只修改测试用例以外的代码，debug时可通过加日志和打断点' --yolo
    } >> "$LOG_FILE" 2>&1
    cat "$LOG_FILE"
  fi

  echo "🔁 回到第 1 步..." >> "$LOG_FILE"
  echo "🔁 回到第 1 步..."

  touch "$HEARTBEAT_FILE"
  sleep 1
done