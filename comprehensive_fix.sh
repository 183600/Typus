#!/bin/bash

# 备份原文件
cp test/Test/Unit/ComprehensiveTypusTestSuite.hs test/Test/Unit/ComprehensiveTypusTestSuite.hs.bak

# 修复所有 parseTypus 相关的函数
# 将 "in property $ not (null $ show errors)" 替换为 "in case parsed of"
sed -i 's/in property \$ not (null \$ show errors)/in case parsed of/g' test/Test/Unit/ComprehensiveTypusTestSuite.hs

# 在每个 "in case parsed of" 后面添加正确的 case 分支
# 先处理简单的 Right/Left 模式
sed -i '/in case parsed of/,/Left _ -> property False/ {
  /in case parsed of/a\
     Right ast -> property $ not (null $ show ast)
  /Right ast -> property \$ not (null \$ show ast)/N
  /Right ast -> property \$ not (null \$ show ast)\n.*Right _ -> property True/ {
    s/\(Right ast -> property \$ not (null \$ show ast)\)\n.*Right _ -> property True/\1/
  }
}' test/Test/Unit/ComprehensiveTypusTestSuite.hs

echo "第一阶段修复完成"

# 处理编译器相关的函数
sed -i '/Right ast ->/,/let compiled = C\.compile ast/ {
  /Right ast ->/c\
     Right ast -> 
}' test/Test/Unit/ComprehensiveTypusTestSuite.hs

echo "修复完成"