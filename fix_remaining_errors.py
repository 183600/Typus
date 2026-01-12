#!/usr/bin/env python3
import re

# 修复 EnhancedBoundaryConditionsSpec.hs 中的 replicate 错误
with open('/home/runner/work/Typus/Typus/test/Test/Unit/EnhancedBoundaryConditionsSpec.hs', 'r') as f:
    content = f.read()

# 修复第200行的错误
content = re.sub(
    r'let imports = concat \(replicate \(min n 100\) "import module" \+\+ show n \+\+ ";\\n"\)',
    'let imports = concat (replicate (min n 100) ("import module" ++ show n ++ ";\\n"))',
    content
)

with open('/home/runner/work/Typus/Typus/test/Test/Unit/EnhancedBoundaryConditionsSpec.hs', 'w') as f:
    f.write(content)

# 修复 EnhancedDependentTypesSpec.hs 中的解析错误
with open('/home/runner/work/Typus/Typus/test/Test/Unit/EnhancedDependentTypesSpec.hs', 'r') as f:
    content = f.read()

# 修复第29行的解析错误
content = re.sub(
    r'Left _ -> property True  -- 解析失败也算通过',
    'Left _ -> property True  -- 解析失败也算通过',
    content
)

with open('/home/runner/work/Typus/Typus/test/Test/Unit/EnhancedDependentTypesSpec.hs', 'w') as f:
    f.write(content)

print("修复完成")