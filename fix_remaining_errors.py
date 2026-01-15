#!/usr/bin/env python3
import re

# 修复 AnalyzerSymbolTableQuickCheckTestSpec.hs
with open('/home/runner/work/Typus/Typus/test/Test/Unit/AnalyzerSymbolTableQuickCheckTestSpec.hs', 'r') as f:
    content = f.read()

# 修复剩余的 Property 类型错误
content = re.sub(r'Right table -> Map\.size table >= (\d+)', r'Right table -> property (Map.size table >= \1)', content)

with open('/home/runner/work/Typus/Typus/test/Test/Unit/AnalyzerSymbolTableQuickCheckTestSpec.hs', 'w') as f:
    f.write(content)

# 修复 BasicQuickCheckTestSuite.hs
with open('/home/runner/work/Typus/Typus/test/Test/Unit/BasicQuickCheckTestSuite.hs', 'r') as f:
    content = f.read()

# 修复 prop_trim_basic 函数
content = re.sub(
    r'property \$ length trimmed <= length s &&\s+\(null s ==> null trimmed\) &&\s+\(all isSpace s ==> null trimmed\)',
    '''property $ length trimmed <= length s && 
     (null s ==> null trimmed) &&
     (all isSpace s ==> null trimmed)''',
    content,
    flags=re.MULTILINE
)

# 修复 prop_trim_regular 函数
content = re.sub(
    r'property \$ not \(null trimmed\) && head trimmed === c && length trimmed >= 1',
    'property $ not (null trimmed) && head trimmed === c && length trimmed >= 1',
    content
)

with open('/home/runner/work/Typus/Typus/test/Test/Unit/BasicQuickCheckTestSuite.hs', 'w') as f:
    f.write(content)

# 修复 ErrorHandlerConsistencyQuickCheckSpec.hs
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ErrorHandlerConsistencyQuickCheckSpec.hs', 'r') as f:
    content = f.read()

# 修复 Property 类型错误
content = re.sub(r'in pos1 === pos1 && pos2 === pos2', 'in property (pos1 === pos1 && pos2 === pos2)', content)

# 修复 Text 类型错误
content = re.sub(r'message = "Error " \+\+ T\.pack \(show i\)', 'message = T.pack $ "Error " ++ show i', content)

# 修复其他 Property 类型错误
content = re.sub(r'in length formatted >= 0', 'in property (length formatted >= 0)', content)
content = re.sub(r'in all \\\\e -> length e >= 0\) formatted', 'in property (all (\\\\e -> length e >= 0) formatted)', content)

with open('/home/runner/work/Typus/Typus/test/Test/Unit/ErrorHandlerConsistencyQuickCheckSpec.hs', 'w') as f:
    f.write(content)

print("Fixed all remaining errors")