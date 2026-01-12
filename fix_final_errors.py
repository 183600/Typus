#!/usr/bin/env python3
import re

# 修复 EnhancedIntegrationSpec.hs 中的剩余问题
with open('/home/runner/work/Typus/Typus/test/Test/Unit/EnhancedIntegrationSpec.hs', 'r') as f:
    content = f.read()

# 修复所有使用 compile 的地方
content = re.sub(
    r'compileResults = map compile inputs',
    r'''compileResults = map (\\input -> case parseTypus input of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile) inputs''',
    content
)

# 修复 checkDependentTypes 调用
content = re.sub(
    r'dependentTypesResult = checkDependentTypes input',
    '''dependentTypesResult = case parseTypus input of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> checkDependentTypes typusFile''',
    content
)

# 修复 checkOwnership 调用
content = re.sub(
    r'ownershipResult = checkOwnership input',
    '''ownershipResult = case parseTypus input of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> checkOwnership typusFile''',
    content
)

# 修复其他 compile 调用
content = re.sub(
    r'compileResult = compile input',
    '''compileResult = case parseTypus input of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile''',
    content
)

# 修复 compile original 和 compile modified
content = re.sub(
    r'originalResult = compile original',
    '''originalResult = case parseTypus original of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile''',
    content
)

content = re.sub(
    r'modifiedResult = compile modified',
    '''modifiedResult = case parseTypus modified of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile''',
    content
)

# 修复 Property 类型问题
content = re.sub(
    r'Left errors -> length errors > 0',
    'Left errors -> property (length errors > 0)',
    content
)

# 修复 compile ("-O " ++ input)
content = re.sub(
    r'optimizedResult = compile \("-O " \+\+ input\)',
    '''optimizedResult = case parseTypus ("-O " ++ input) of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile''',
    content
)

# 修复 Right result -> not (T.null result)
content = re.sub(
    r'Right result -> not \(T\.null result\)',
    'Right result -> property (not (T.null (T.pack result)))',
    content
)

# 修复 map compile modules
content = re.sub(
    r'compileResults = map compile modules',
    r'''compileResults = map (\\module -> case parseTypus module of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile) modules''',
    content
)

# 修复 compile module1 和 compile module2
content = re.sub(
    r'compileResult1 = compile module1',
    '''compileResult1 = case parseTypus module1 of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile''',
    content
)

content = re.sub(
    r'compileResult2 = compile module2',
    '''compileResult2 = case parseTypus module2 of
        Left _ -> Left []  -- 解析失败
        Right typusFile -> compile typusFile''',
    content
)

with open('/home/runner/work/Typus/Typus/test/Test/Unit/EnhancedIntegrationSpec.hs', 'w') as f:
    f.write(content)

print("修复完成")