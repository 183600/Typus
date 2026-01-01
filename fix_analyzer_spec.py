#!/usr/bin/env python3
import re

# 读取文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/AnalyzerCrossAnalysisSpec.hs', 'r') as f:
    content = f.read()

# 替换 symbolLocation 为 symbolScope
content = re.sub(r'symbolLocation = \(\d+, \d+\)', 'symbolScope = 1', content)
content = re.sub(r'symbolLocation = \(2, 1\)', 'symbolScope = 2', content)

# 添加 constraints = [] 到 SymbolInfo 构造中
# 查找缺少 constraints 的 SymbolInfo 构造
pattern = r'(\{[^}]+isBorrowed = [^}]+)\}'
def add_constraints(match):
    return match.group(1) + ',\n        , constraints = []\n    }'
    
content = re.sub(pattern, add_constraints, content)

# 替换 Warning 为 ErrorSeverity.Warning
content = re.sub(r'Warning', 'ErrorSeverity.Warning', content)

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/AnalyzerCrossAnalysisSpec.hs', 'w') as f:
    f.write(content)

print("Fixed symbolLocation and Warning references")