#!/usr/bin/env python3
import re

# 读取文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/AnalyzerSymbolTableQuickCheckTestSpec.hs', 'r') as f:
    content = f.read()

# 替换所有 Right (_, table) 为 Right table
content = re.sub(r'Right \(_, table\)', 'Right table', content)

# 替换所有 Right (_, table1), Right (_, table2) 为 Right table1, Right table2
content = re.sub(r'Right \(_, table1\), Right \(_, table2\)', 'Right table1, Right table2', content)

# 替换所有 Map.size table >= 0 为 property (Map.size table >= 0)
content = re.sub(r'Map\.size table >= 0', 'property (Map.size table >= 0)', content)

# 替换所有 Map.size table === 0 为 property (Map.size table === 0)
content = re.sub(r'Map\.size table === 0', 'property (Map.size table === 0)', content)

# 替换所有 Map.size table1 === Map.size table2 为 property (Map.size table1 === Map.size table2)
content = re.sub(r'Map\.size table1 === Map\.size table2', 'property (Map.size table1 === Map.size table2)', content)

# 替换所有 Map.size table >= n 为 property (Map.size table >= n)
content = re.sub(r'Map\.size table >= (\d+)', r'property (Map.size table >= \1)', content)

# 替换所有 Map.size table > 0 为 property (Map.size table > 0)
content = re.sub(r'Map\.size table > 0', 'property (Map.size table > 0)', content)

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/AnalyzerSymbolTableQuickCheckTestSpec.hs', 'w') as f:
    f.write(content)

print("Fixed AnalyzerSymbolTableQuickCheckTestSpec.hs")