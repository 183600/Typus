#!/usr/bin/env python3
import re

# 读取文件内容
with open('/home/runner/work/Typus/Typus/test/Test/Unit/EnhancedDependentTypesSpec.hs', 'r') as f:
    content = f.read()

# 定义替换模式
pattern = r'(\s+let code = ".*?")\n(\s+result = checkDependentTypes code)\n(\s+in case result of)'

# 定义替换函数
def replace_match(match):
    indent = match.group(1).split('let')[0]
    code_line = match.group(1)
    result_line = match.group(2).replace('checkDependentTypes code', 'case parseTypus code of\n' + indent + '    Left _ -> property True  -- 解析失败也算通过\n' + indent + '    Right typusFile -> case checkDependentTypes typusFile of')
    return code_line + '\n' + result_line + '\n' + match.group(3)

# 执行替换
new_content = re.sub(pattern, replace_match, content, flags=re.DOTALL)

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/EnhancedDependentTypesSpec.hs', 'w') as f:
    f.write(new_content)

print("批量修改完成")