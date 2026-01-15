#!/usr/bin/env python3
import re

# 读取文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/AnalyzerSymbolTableQuickCheckTestSpec.hs', 'r') as f:
    content = f.read()

# 添加 System.IO.Unsafe 导入
if 'import System.IO.Unsafe' not in content:
    content = re.sub(
        r'import Control\.Monad\.State\s+import Control\.Monad\.Except',
        'import Control.Monad.State\nimport Control.Monad.Except\nimport System.IO.Unsafe (unsafePerformIO)',
        content
    )

# 替换所有 runExcept (evalStateT (collectSymbolsAndTypes code) newAnalyzerState)
# 为 unsafePerformIO (runExceptT (evalStateT (collectSymbolsAndTypes code) newAnalyzerState))
pattern = r'runExcept \(evalStateT \(collectSymbolsAndTypes (\w+)\) newAnalyzerState\)'
def replacer(match):
    var_name = match.group(1)
    return f'unsafePerformIO (runExceptT (evalStateT (collectSymbolsAndTypes {var_name}) newAnalyzerState))'

content = re.sub(pattern, replacer, content)

# 替换所有 runExcept (evalStateT (collectSymbolsAndTypes "") newAnalyzerState)
content = re.sub(
    r'runExcept \(evalStateT \(collectSymbolsAndTypes ""\) newAnalyzerState\)',
    'unsafePerformIO (runExceptT (evalStateT (collectSymbolsAndTypes "") newAnalyzerState))',
    content
)

# 替换所有 runExcept (evalStateT (collectSymbolsAndTypes multiLineCode) newAnalyzerState)
content = re.sub(
    r'runExcept \(evalStateT \(collectSymbolsAndTypes multiLineCode\) newAnalyzerState\)',
    'unsafePerformIO (runExceptT (evalStateT (collectSymbolsAndTypes multiLineCode) newAnalyzerState))',
    content
)

# 替换所有 runExcept (evalStateT (collectSymbolsAndTypes typeDef) newAnalyzerState)
content = re.sub(
    r'runExcept \(evalStateT \(collectSymbolsAndTypes typeDef\) newAnalyzerState\)',
    'unsafePerformIO (runExceptT (evalStateT (collectSymbolsAndTypes typeDef) newAnalyzerState))',
    content
)

# 替换所有 runExcept (evalStateT (collectSymbolsAndTypes varDecl) newAnalyzerState)
content = re.sub(
    r'runExcept \(evalStateT \(collectSymbolsAndTypes varDecl\) newAnalyzerState\)',
    'unsafePerformIO (runExceptT (evalStateT (collectSymbolsAndTypes varDecl) newAnalyzerState))',
    content
)

# 替换所有 runExcept (evalStateT (collectSymbolsAndTypes funcDecl) newAnalyzerState)
content = re.sub(
    r'runExcept \(evalStateT \(collectSymbolsAndTypes funcDecl\) newAnalyzerState\)',
    'unsafePerformIO (runExceptT (evalStateT (collectSymbolsAndTypes funcDecl) newAnalyzerState))',
    content
)

# 替换所有 runExcept (evalStateT (collectSymbolsAndTypes complexType) newAnalyzerState)
content = re.sub(
    r'runExcept \(evalStateT \(collectSymbolsAndTypes complexType\) newAnalyzerState\)',
    'unsafePerformIO (runExceptT (evalStateT (collectSymbolsAndTypes complexType) newAnalyzerState))',
    content
)

# 替换所有 runExcept (evalStateT (collectSymbolsAndTypes recursiveType) newAnalyzerState)
content = re.sub(
    r'runExcept \(evalStateT \(collectSymbolsAndTypes recursiveType\) newAnalyzerState\)',
    'unsafePerformIO (runExceptT (evalStateT (collectSymbolsAndTypes recursiveType) newAnalyzerState))',
    content
)

# 替换所有 runExcept (evalStateT (collectSymbolsAndTypes genericType) newAnalyzerState)
content = re.sub(
    r'runExcept \(evalStateT \(collectSymbolsAndTypes genericType\) newAnalyzerState\)',
    'unsafePerformIO (runExceptT (evalStateT (collectSymbolsAndTypes genericType) newAnalyzerState))',
    content
)

# 替换所有 runExcept (evalStateT (collectSymbolsAndTypes complexProgram) newAnalyzerState)
content = re.sub(
    r'runExcept \(evalStateT \(collectSymbolsAndTypes complexProgram\) newAnalyzerState\)',
    'unsafePerformIO (runExceptT (evalStateT (collectSymbolsAndTypes complexProgram) newAnalyzerState))',
    content
)

# 替换所有 runExcept (evalStateT (collectSymbolsAndTypes invalidCode) newAnalyzerState)
content = re.sub(
    r'runExcept \(evalStateT \(collectSymbolsAndTypes invalidCode\) newAnalyzerState\)',
    'unsafePerformIO (runExceptT (evalStateT (collectSymbolsAndTypes invalidCode) newAnalyzerState))',
    content
)

# 替换所有 runExcept (evalStateT (collectSymbolsAndTypes duplicateDefs) newAnalyzerState)
content = re.sub(
    r'runExcept \(evalStateT \(collectSymbolsAndTypes duplicateDefs\) newAnalyzerState\)',
    'unsafePerformIO (runExceptT (evalStateT (collectSymbolsAndTypes duplicateDefs) newAnalyzerState))',
    content
)

# 替换所有 runExcept (evalStateT (collectSymbolsAndTypes mutualDeps) newAnalyzerState)
content = re.sub(
    r'runExcept \(evalStateT \(collectSymbolsAndTypes mutualDeps\) newAnalyzerState\)',
    'unsafePerformIO (runExceptT (evalStateT (collectSymbolsAndTypes mutualDeps) newAnalyzerState))',
    content
)

# 替换所有 runExcept (evalStateT (collectSymbolsAndTypes nestedType) newAnalyzerState)
content = re.sub(
    r'runExcept \(evalStateT \(collectSymbolsAndTypes nestedType\) newAnalyzerState\)',
    'unsafePerformIO (runExceptT (evalStateT (collectSymbolsAndTypes nestedType) newAnalyzerState))',
    content
)

# 替换所有 runExcept (evalStateT (collectSymbolsAndTypes manySymbols) newAnalyzerState)
content = re.sub(
    r'runExcept \(evalStateT \(collectSymbolsAndTypes manySymbols\) newAnalyzerState\)',
    'unsafePerformIO (runExceptT (evalStateT (collectSymbolsAndTypes manySymbols) newAnalyzerState))',
    content
)

# 替换所有 runExcept (evalStateT (collectSymbolsAndTypes typeDef) newAnalyzerState)
content = re.sub(
    r'runExcept \(evalStateT \(collectSymbolsAndTypes typeDef\) newAnalyzerState\)',
    'unsafePerformIO (runExceptT (evalStateT (collectSymbolsAndTypes typeDef) newAnalyzerState))',
    content
)

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/AnalyzerSymbolTableQuickCheckTestSpec.hs', 'w') as f:
    f.write(content)

print("Fixed AnalyzerSymbolTableQuickCheckTestSpec.hs with IO handling")