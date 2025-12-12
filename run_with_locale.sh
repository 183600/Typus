#!/bin/bash
# 设置locale为C以避免locale警告
export LC_ALL=C
export LANG=C

# 运行原始命令
exec "$@"