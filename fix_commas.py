#!/usr/bin/env python3
import re

# 读取文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/AnalyzerCrossAnalysisSpec.hs', 'r') as f:
    content = f.read()

# 修复多余的逗号和重复的 constraints 行
# 模式: ,\n\s*,\s*, constraints = []
content = re.sub(r',\s*,\s*, constraints = \[\]', ', constraints = []', content)

# 模式: ,\n\s*,\s*, constraints = []
content = re.sub(r',\s*,\s*, constraints = \[\]', ', constraints = []', content)

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/AnalyzerCrossAnalysisSpec.hs', 'w') as f:
    f.write(content)

print("Fixed extra commas and duplicate constraints lines")