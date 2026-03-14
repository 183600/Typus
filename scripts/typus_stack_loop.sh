#!/usr/bin/env bash
set -u
set -o pipefail

trap 'echo; echo "已终止."; exit 0' INT TERM

while true; do
  echo "===================="
  echo "$(date '+%F %T') 开始执行 iflow 任务"
  echo "===================="

  iflow "检查项目配置和功能完整性 think:high" --yolo || true
  iflow "确保这个项目的各个功能都可以在Linux上正常使用，不要删除这个项目的功能 think:high" --yolo || true

  echo "🔁 重新开始循环..."
  sleep 1
done