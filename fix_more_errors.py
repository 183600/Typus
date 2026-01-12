#!/usr/bin/env python3
import re

# 修复 EnhancedCompilerBasicSpec.hs 中的 SyntaxError 和 TypeError 导入问题
with open('/home/runner/work/Typus/Typus/test/Test/Unit/EnhancedCompilerBasicSpec.hs', 'r') as f:
    content = f.read()

# 更新导入列表，添加 SyntaxError 和 TypeError
content = re.sub(
    r'import Compiler\s*\((.*?)\)',
    lambda m: 'import Compiler (\n  ' + m.group(1).replace('\n', '\n  ') + '\n  , SyntaxError(..)\n  , TypeError(..)\n)',
    content,
    flags=re.DOTALL
)

with open('/home/runner/work/Typus/Typus/test/Test/Unit/EnhancedCompilerBasicSpec.hs', 'w') as f:
    f.write(content)

# 修复 EnhancedErrorHandlingSpec.hs 中的 Located 导入问题
with open('/home/runner/work/Typus/Typus/test/Test/Unit/EnhancedErrorHandlingSpec.hs', 'r') as f:
    content = f.read()

# 添加 Located 导入
if 'import SourceLocation' in content:
    content = re.sub(
        r'import SourceLocation\s*\((.*?)\)',
        lambda m: 'import SourceLocation (\n  ' + m.group(1).replace('\n', '\n  ') + '\n  , Located(..)\n)',
        content,
        flags=re.DOTALL
    )
else:
    content = re.sub(
        r'(import ErrorHandler)',
        r'\1\nimport SourceLocation (Located(..))',
        content
    )

# 修复 generateErrorReport 的歧义
content = re.sub(
    r'let report = generateErrorReport errors',
    'let report = ErrorHandler.generateErrorReport errors',
    content
)

with open('/home/runner/work/Typus/Typus/test/Test/Unit/EnhancedErrorHandlingSpec.hs', 'w') as f:
    f.write(content)

# 修复 EnhancedIntegrationSpec.hs 中的类型问题
with open('/home/runner/work/Typus/Typus/test/Test/Unit/EnhancedIntegrationSpec.hs', 'r') as f:
    content = f.read()

# 修复 compile 函数调用 - 需要 TypusFile 而不是 String
content = re.sub(
    r'compileResult = compile input',
    '''compileResult = case parseTypus input of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile''',
    content
)

# 修复 formatCompilerErrors 的类型问题
content = re.sub(
    r'let parseMsg = T\.pack \$ formatCompilerErrors parseErr',
    'let parseMsg = T.pack $ formatCompilerErrors [parseErr]',
    content
)

content = re.sub(
    r'let compileMsg = T\.pack \$ formatCompilerErrors compileErr',
    'let compileMsg = T.pack $ formatCompilerErrors [compileErr]',
    content
)

# 修复 Property 类型问题
content = re.sub(
    r'in not \(T\.null parseMsg\) && not \(T\.null compileMsg\)',
    'in property (not (T.null parseMsg) && not (T.null compileMsg))',
    content
)

with open('/home/runner/work/Typus/Typus/test/Test/Unit/EnhancedIntegrationSpec.hs', 'w') as f:
    f.write(content)

print("修复完成")