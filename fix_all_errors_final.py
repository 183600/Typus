#!/usr/bin/env python3
import re

# 修复 AnalyzerSymbolTableQuickCheckTestSpec.hs
with open('/home/runner/work/Typus/Typus/test/Test/Unit/AnalyzerSymbolTableQuickCheckTestSpec.hs', 'r') as f:
    content = f.read()

# 修复剩余的 Property 类型错误
content = re.sub(r'Right table -> Map\.size table >= n', 'Right table -> property (Map.size table >= n)', content)

with open('/home/runner/work/Typus/Typus/test/Test/Unit/AnalyzerSymbolTableQuickCheckTestSpec.hs', 'w') as f:
    f.write(content)

# 修复 BasicQuickCheckTestSuite.hs
with open('/home/runner/work/Typus/Typus/test/Test/Unit/BasicQuickCheckTestSuite.hs', 'r') as f:
    content = f.read()

# 修复 prop_trim_basic 函数 - 完全重写
content = re.sub(
    r'''prop_trim_basic :: String -> Property
prop_trim_basic s =
  let trimmed = trim s
  in property \$ length trimmed <= length s && 
     \(null s ==> null trimmed\) &&
     \(all isSpace s ==> null trimmed\)''',
    '''prop_trim_basic :: String -> Property
prop_trim_basic s =
  let trimmed = trim s
  in property $ 
    (length trimmed <= length s) && 
    (if null s then null trimmed else True) &&
    (if all isSpace s then null trimmed else True)''',
    content,
    flags=re.MULTILINE | re.DOTALL
)

# 修复 prop_trim_regular 函数
content = re.sub(
    r'in property \$ not \(null trimmed\) && head trimmed === c && length trimmed >= 1',
    'in property $ not (null trimmed) && head trimmed === c && length trimmed >= 1',
    content
)

with open('/home/runner/work/Typus/Typus/test/Test/Unit/BasicQuickCheckTestSuite.hs', 'w') as f:
    f.write(content)

# 修复 ErrorHandlerConsistencyQuickCheckSpec.hs
with open('/home/runner/work/Typus/Typus/test/Test/Unit/ErrorHandlerConsistencyQuickCheckSpec.hs', 'r') as f:
    content = f.read()

# 修复 Property 类型错误 - 使用 conjoin 而不是 &&
content = re.sub(r'in property \(pos1 === pos1 && pos2 === pos2\)', 'in conjoin [pos1 === pos1, pos2 === pos2]', content)

# 修复其他 Property 类型错误
content = re.sub(r'in property \(length formatted >= 0\)', 'in property (length formatted >= 0)', content)
content = re.sub(r'in property \(all \\\\e -> length e >= 0\) formatted\)', 'in property (all (\\\\e -> length e >= 0) formatted)', content)

with open('/home/runner/work/Typus/Typus/test/Test/Unit/ErrorHandlerConsistencyQuickCheckSpec.hs', 'w') as f:
    f.write(content)

# 修复 FinalQuickCheckTestSuite.hs
with open('/home/runner/work/Typus/Typus/test/Test/Unit/FinalQuickCheckTestSuite.hs', 'r') as f:
    content = f.read()

# 修复字符串字面量错误
content = re.sub(r'"//" `notElem` withoutComments', '"//" `notElem` withoutComments', content)

with open('/home/runner/work/Typus/Typus/test/Test/Unit/FinalQuickCheckTestSuite.hs', 'w') as f:
    f.write(content)

# 修复 SimpleQuickCheckTestSuite.hs
with open('/home/runner/work/Typus/Typus/test/Test/Unit/SimpleQuickCheckTestSuite.hs', 'r') as f:
    content = f.read()

# 修复 isRight 歧义 - 使用完全限定名称
content = re.sub(r'isRight \(Right x\)', 'Data.Either.isRight (Right x)', content)
content = re.sub(r'not \$ isRight \(Left msg\)', 'not $ Data.Either.isRight (Left msg)', content)
content = re.sub(r'isRight e === \(case e of Right _ -> True; Left _ -> False\)', 'Data.Either.isRight e === (case e of Right _ -> True; Left _ -> False)', content)
content = re.sub(r'isRight \(Right 42\)', 'Data.Either.isRight (Right 42)', content)
content = re.sub(r'not \$ isRight \(Left "error"\)', 'not $ Data.Either.isRight (Left "error")', content)

with open('/home/runner/work/Typus/Typus/test/Test/Unit/SimpleQuickCheckTestSuite.hs', 'w') as f:
    f.write(content)

print("Fixed all remaining errors comprehensively")