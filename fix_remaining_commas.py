#!/usr/bin/env python3
import re

# 读取文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/AnalyzerCrossAnalysisSpec.hs', 'r') as f:
    content = f.read()

# 修复多余的逗号
# 模式: ,\n        ,\s*,
content = re.sub(r',\s*,\s*,', ', ', content)

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/AnalyzerCrossAnalysisSpec.hs', 'w') as f:
    f.write(content)

print("Fixed remaining comma issues")