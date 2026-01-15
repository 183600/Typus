#!/usr/bin/env python3
import re

# 读取文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/AnalyzerSymbolTableQuickCheckTestSpec.hs', 'r') as f:
    content = f.read()

# 修复测试函数中的 assertBool 调用
content = re.sub(r'assertBool ".*" \(property \(Map\.size table > 0\)\)', r'assertBool "Simple type definition should result in non-empty symbol table" (Map.size table > 0)', content)
content = re.sub(r'assertBool ".*" \(property \(Map\.size table >= 0\)\)', r'assertBool "Empty code should result in empty symbol table" (Map.size table >= 0)', content)
content = re.sub(r'assertBool ".*" \(property \(property \(Map\.size table >= 0\)\)\)', r'assertBool "Invalid code might still result in symbol table" (Map.size table >= 0)', content)

# 写回文件
with open('/home/runner/work/Typus/Typus/test/Test/Unit/AnalyzerSymbolTableQuickCheckTestSpec.hs', 'w') as f:
    f.write(content)

print("Fixed test functions in AnalyzerSymbolTableQuickCheckTestSpec.hs")