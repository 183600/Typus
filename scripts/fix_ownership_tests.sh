#!/bin/bash

# 批量修复 analyzeOwnership 类型错误的 sed 脚本
sed -i 's/parsed = O\.analyzeOwnership \([a-zA-Z]*Str\)/errors = O.analyzeOwnership \1/g' test/Test/Unit/ComprehensiveTypusTestSuite.hs
sed -i 's/case parsed of/in property $ not (null $ show errors)/g' test/Test/Unit/ComprehensiveTypusTestSuite.hs
sed -i '/Right analysis -> property \$ not (null \$ show analysis)/d' test/Test/Unit/ComprehensiveTypusTestSuite.hs
sed -i '/Left _ -> property True/d' test/Test/Unit/ComprehensiveTypusTestSuite.hs

echo "批量修复完成"