#!/bin/bash

echo "验证修复..."

# 检查是否还有 withMemoryCleanupBetweenTests 导入
echo "1. 检查 withMemoryCleanupBetweenTests 导入..."
if grep -r "withMemoryCleanupBetweenTests" test/Test/Unit/CoreUtilsQuickCheckTests.hs > /dev/null 2>&1; then
    echo "   错误：仍然存在 withMemoryCleanupBetweenTests 导入"
    exit 1
else
    echo "   ✓ withMemoryCleanupBetweenTests 导入已移除"
fi

# 检查 const True 是否已修复
echo "2. 检查 const True 类型错误..."
if grep -n "criticalMemoryProperty.*const True" test/Test/Unit/ParserQuickCheckTests.hs | grep -v "property . const True" > /dev/null 2>&1; then
    echo "   错误：仍然存在 const True 类型错误"
    exit 1
else
    echo "   ✓ const True 类型错误已修复"
fi

echo "所有错误已修复！"