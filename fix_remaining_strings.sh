#!/bin/bash

# 修复所有多行字符串问题
sed -i '1480,1484c\  then let closureStr = "{//! ownership: on\ns := NewMyString(\"" ++ s ++ "\")\nf := func() { /* 使用 s */ }\n// s 被闭包捕获}"' test/Test/Unit/ComprehensiveTypusTestSuite.hs

sed -i '1492,1494c\  then let recursiveStr = "{//! ownership: on\ntype " ++ s ++ "Node struct { data int; next *" ++ s ++ "Node }"' test/Test/Unit/ComprehensiveTypusTestSuite.hs

sed -i '1578,1580c\  then let debugStr = "{//! ownership: on\n//! debug: ownership\ns := NewMyString(\"" ++ s ++ "\")\n// 调试所有权信息}"' test/Test/Unit/ComprehensiveTypusTestSuite.hs

echo "修复完成"