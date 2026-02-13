#!/bin/bash

# 修复重复的 "in" 问题
sed -i 's/in in property/in property/g' test/Test/Unit/ComprehensiveTypusTestSuite.hs

echo "修复重复的 in 完成"