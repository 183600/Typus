#!/usr/bin/env python3
"""
验证 analyzeOwnership 函数的使用是否已修复
"""

# 读取文件内容
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ComprehensiveTypusTestSuite.hs', 'r') as f:
    content = f.read()

# 检查是否还有使用 analyzeOwnership 的地方期望 Either 类型
import re

# 查找所有使用 analyzeOwnership 的地方
pattern = r'(\w+)\s*=\s*O\.analyzeOwnership\s+(\w+)'
matches = re.findall(pattern, content)

print(f"找到 {len(matches)} 处使用 analyzeOwnership 的地方")

# 检查每个使用的地方
for var_name, arg_name in matches:
    # 查找这个变量后面的使用
    pattern2 = rf'case\s+{var_name}\s+of'
    if re.search(pattern2, content):
        print(f"错误: {var_name} 仍然在 case 表达式中使用，期望 Either 类型")
    else:
        print(f"正确: {var_name} 没有在 case 表达式中使用")

print("验证完成")