#!/usr/bin/env python3
import re

# 读取文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/AnalyzerSymbolTableQuickCheckTestSpec.hs', 'r') as f:
    content = f.read()

# 替换所有 runExcept (evalStateT (collectSymbolsAndTypes code) newAnalyzerState) 
# 为 runExcept (evalStateT (collectSymbolsAndTypes code) newAnalyzerState)
pattern = r'runExcept \(evalStateT \(collectSymbolsAndTypes code\) newAnalyzerState\)'
content = re.sub(pattern, 'runIdentity (runExceptT (evalStateT (collectSymbolsAndTypes code) newAnalyzerState))', content)

# 替换所有 Right (_, table) 为 Right table
content = re.sub(r'Right \(_, table\)', 'Right table', content)

# 替换所有 Map.size table >= 0 为 property (Map.size table >= 0)
content = re.sub(r'Map\.size table >= 0', 'property (Map.size table >= 0)', content)

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/AnalyzerSymbolTableQuickCheckTestSpec.hs', 'w') as f:
    f.write(content)

print("Fixed AnalyzerSymbolTableQuickCheckTestSpec.hs")