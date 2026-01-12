#!/usr/bin/env python3
import re

# 修复 EnhancedBoundaryConditionsSpec.hs 中的字符串字面量错误
with open('/home/runner/work/Typus/Typus/test/Test/Unit/EnhancedBoundaryConditionsSpec.hs', 'r') as f:
    content = f.read()

# 修复第200行的字符串字面量错误
content = re.sub(
    r'let imports = concat \(replicate \(min n 100\) \("import module" \+\+ show n \+\+;',
    r'let imports = concat (replicate (min n 100) ("import module" ++ show n ++ ";',
    content
)

with open('/home/runner/work/Typus/Typus/test/Test/Unit/EnhancedBoundaryConditionsSpec.hs', 'w') as f:
    f.write(content)

# 修复 EnhancedCompilerBasicSpec.hs 中的多个问题
with open('/home/runner/work/Typus/Typus/test/Test/Unit/EnhancedCompilerBasicSpec.hs', 'r') as f:
    content = f.read()

# 修复 compile 调用 - 需要 TypusFile 而不是 String
content = re.sub(
    r'result = compile ""',
    '''result = case parseTypus "" of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile''',
    content
)

content = re.sub(
    r'result = compile code',
    '''result = case parseTypus code of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile''',
    content
)

# 修复 malformedSyntaxError 调用
content = re.sub(
    r'let error = malformedSyntaxError msg',
    'let error = malformedSyntaxError',
    content
)

# 修复 SyntaxError 模式匹配
content = re.sub(
    r'SyntaxError _ _ _ -> property True',
    'SyntaxError _ _ _ _ _ -> property True',
    content
)

# 修复 renderCompilationError 调用
content = re.sub(
    r'rendered = renderCompilationError error',
    'rendered = renderCompilationError [error]',
    content
)

# 修复 Property 类型问题
content = re.sub(
    r'in not \(T\.null rendered\)',
    'in property (not (T.null (T.pack rendered)))',
    content
)

with open('/home/runner/work/Typus/Typus/test/Test/Unit/EnhancedCompilerBasicSpec.hs', 'w') as f:
    f.write(content)

print("修复完成")